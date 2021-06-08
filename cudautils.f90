module cudautils
   use ln_precision
   use openacc
   use cublas_v2
   use cusolverDn
   use mattype
   implicit none
   private
   public :: createGPU
   public :: copyToGPU
   public :: deleteGPU

   public :: sum_gpu
   public :: copy_trid_toGPU 
   public :: delete_trid_fromGPU 
   public :: check_sum_trid 

 contains   

  subroutine createGPU(A)
    type(z_DNS), intent(in) :: A     
    integer :: nrow, ncol
    
    nrow = A%nrow
    ncol = A%ncol    
    !$acc enter data create(A)
    !$acc enter data create(A%val(:nrow,:ncol))
  end subroutine createGPU
  
  subroutine deleteGPU(A)
    type(z_DNS), intent(inout) :: A
    integer :: nrow, ncol
    
    nrow = A%nrow
    ncol = A%ncol    
    !$acc exit data delete(A%val(0:nrow,0:ncol))
    !$acc exit data delete(A)
  end subroutine deleteGPU
 
  subroutine copyToGPU(A)
    type(z_DNS), intent(in) :: A     
    integer :: nrow, ncol
    
    nrow = A%nrow
    ncol = A%ncol    
    !$acc enter data copyin(A)
    !$acc enter data copyin(A%val(0:nrow,0:ncol))
 end subroutine copyToGPU

 !~- Routine to perform the absolute sum of each element inside a matrix on the GPU ~-
 ! Arguments:  
 !           hh: the CublasHandle 
 !           H: the matrix for which we want the basolute sum
 !           summ: real variable that is the desired sum 
 !~-
  subroutine sum_gpu(hcublas, Mat, summ)
    type(cublasHandle), intent(in) :: hcublas
    complex(dp), intent(in) :: Mat(:,:)
    real(dp), intent(out) :: summ
    !
    integer :: istat, n

    n=size(Mat,1)
    !$acc data present(Mat)
    !$acc host_data use_device(Mat)
    summ = 0.0_dp
    istat=cublasDzasum(hcublas, n*n, Mat, 1, summ)
    !$acc end host_data
    !$acc end data
  end subroutine sum_gpu
  
  subroutine copy_trid_toGPU(M)
    type(z_DNS), dimension(:,:), intent(in) :: M
    integer :: ii, nbl

    nbl = size(M,1)
    call copyToGPU(M(1,1))
    do ii=2,nbl
       call copyToGPU(M(ii,ii))
       call copyToGPU(M(ii-1,ii))
       call copyToGPU(M(ii,ii-1))
    end do
  end subroutine copy_trid_toGPU

  subroutine delete_trid_fromGPU(M)
    type(z_DNS), dimension(:,:), intent(in) :: M
    integer :: ii, nbl

    nbl = size(M,1)
    call deleteGPU(M(1,1))
    do ii=2,nbl
       call deleteGPU(M(ii,ii))
       call deleteGPU(M(ii-1,ii))
       call deleteGPU(M(ii,ii-1))
    end do
  end subroutine delete_trid_fromGPU
 
 !~- Routine to perform the absolute sum of each element inside each block of H ~-
 ! Arguments:  
 !           hh: the CublasHandle to call the sum_gpu function
 !           H: the tridiagonal matrix of matrices
 !           name_: a string to print the name of the tridiagonal matrix
 !           gpu: logical variable. If .true. , the sum is performed on the GPU. If .false.  it is performed on the CPU
 !~-
  subroutine check_sum_trid(hh,T,name_,gpu)
    type(z_DNS), dimension(:,:), intent(in) :: T
    character(3), intent(in) :: name_
    logical, intent(in) :: gpu
    type(CublasHandle), intent(in) :: hh   

    integer :: nbl, i
    real(dp) :: summ 

    nbl = size(T,1)
    if (gpu .eq. .true.) then
       write(*,*) '~-~-~-~-',name_,' check sum GPU: ~-~-~-~-'     
       call sum_gpu(hh, T(1,1)%val, summ)
       write(*,*) '       ',name_,'(',1,1,')=', summ

       do i= 2,nbl
          call sum_gpu(hh, T(i,i)%val, summ)
          write(*,*) '       ',name_,'(',i,i,')=', summ
          call sum_gpu(hh, T(i-1,i)%val, summ)
          write(*,*) '       ',name_,'(',i-1,i,')=', summ
          call sum_gpu(hh, T(i,i-1)%val, summ)
          write(*,*) '       ',name_,'(',i,i-1,')=', summ
       end do
    else
       write(*,*) '~-~-~-~-',name_,' check sum CPU: ~-~-~-~-'    
       summ = sum(ABS(T(1,1)%val)) 
       write(*,*) 'CPU:   ',name_,'(',1,1,')=', summ

       do i= 2,nbl
          summ = sum(ABS(T(i,i)%val)) 
         write(*,*) 'CPU:   ',name_,'(',i,i,')=', summ
          summ = sum(ABS(T(i-1,i)%val)) 
        write(*,*) 'CPU:   ',name_,'(',i-1,i,')=', summ
          summ = sum(ABS(T(i,i-1)%val)) 
         write(*,*) 'CPU:   ',name_,'(',i,i-1,')=', summ
       end do
       write(*,*) '~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-'     
    endif     
  end subroutine check_sum_trid 

end module cudautils 
