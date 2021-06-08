program test
  use ln_precision
  use mattype
  use cudautils
  use cublas_v2
  implicit none
  
  integer, parameter :: npl = 5 
  type(z_DNS), allocatable, target :: H(:,:)
  type(cublasHandle) ::  hh
  integer :: istat, i, j, fu, nrow, ncol 
  character(1) :: ci, cim1

  allocate(H(npl,npl))
 
  !Ready to assemble the H(5x5) matrix of matrices reading from files. Each element of H is a z_DNS-type matrix. A z_DNS-type matrix is essentially a
  !derived type that contains the matrix, the number of rows and the number of columns (definition is in mattype.f90). H is in
  !tri-diagonal form, so only tri-diagonal elements are occupied by a z_DNS-type matrix.

  open(unit=37,file='H/H_11_n.dat',access='stream', form='unformatted')
  read(37) nrow,ncol
  close(37)

  !The create subroutine just allocates the single blocks of H as H(i,i)(nrow,ncol) (routine in mattype.f90)
  call create(H(1,1),nrow,ncol)

  open(unit=37,file='H/H_11.bin',access='stream')
  read(37) H(1,1)%val !Assembling the H matrix reading from binary file
  close(37)

  !Repeating the same procedure for each block of H
  do i = 2, npl
     write(ci,'(i1)') i
     write(cim1,'(i1)') i-1

     close(37)
     open(unit=37,file='H/H_'//ci//cim1//'_n.dat',access='stream',form='unformatted')
     read(37) nrow, ncol
     close(37)

     call create(H(i,i-1),nrow,ncol)
     open(unit=37,file='H/H_'//ci//cim1//'.bin',access='stream')
     read(37) H(i,i-1)%val
     close(37)
  
     open(unit=37,file='H/H_'//cim1//ci//'_n.dat',access='stream', form='unformatted')
     read(37)  nrow, ncol
     close(37)
     
     call create(H(i-1,i),nrow,ncol)
     open(unit=37,file='H/H_'//cim1//ci//'.bin',access='stream')
     read(37)  H(i-1,i)%val
     close(37)

     open(unit=37,file='H/H_'//ci//ci//'_n.dat',access='stream', form ='unformatted')
     read(37) nrow, ncol
     close(37)
     
     call create(H(i,i),nrow,ncol)
     open(unit=37,file='H/H_'//ci//ci//'.bin',access='stream')
     read(37) H(i,i)%val
     close(37)
  end do
  print*,'H all created'

  !Printing the sum of modules of each matrix belonging to H, from the CPU memory (routine in cudautils.f90)
  call check_sum_trid(hh,H,'H  ',.false.)
  
  
  istat = cublasCreate(hh)
  !$acc data
  
  !Routine to copy all blocks of H to the GPU (routine in cudautils.f90)
  call copy_trid_toGPU(H)

  !Printing the sum of modules of each matrix belonging to H, from the GPU memory (here I get the error on the last block)
  call check_sum_trid(hh,H,'H  ',.true.)

  !Routines that deletes from GPU each block of H (routine in cudautils.f90)
  call delete_trid_fromGPU(H)

  print*, 'H deleted from GPU'
  
  !The destroy subroutine just deallocates the single blocks of H (routine in mattype.f90)
  call destroy(H(1,1))

  do i = 2, npl
    call destroy(H(i,i))
    call destroy(H(i-1,i))
    call destroy(H(i,i-1))
  end do
  
  !$acc end data
  istat = cublasDestroy(hh)
  deallocate(H)

end program test

    
    
