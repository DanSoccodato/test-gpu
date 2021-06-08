module mattype
  use ln_precision    
  private
  public :: z_DNS, create, destroy

  type :: z_DNS
    complex(dp), dimension(:,:), allocatable :: val
    integer :: nrow
    integer :: ncol
  end type z_DNS
 
  contains
  subroutine create(A,m,n)
    type(z_DNS) :: A   
    integer, intent(in) :: m,n 
    allocate(A%val(m,n))
    A%nrow = m
    A%ncol = n
  end subroutine create  

  subroutine destroy(A)
    type(z_DNS) :: A 
    deallocate(A%val)
  end subroutine destroy

end module mattype
