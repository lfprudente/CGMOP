module globals

implicit none

integer :: n,m,problem
logical :: checkder,scaleF
real(kind=8) :: seed,HZparam
real(kind=8),allocatable :: x(:),d(:),sF(:),JF(:,:)
logical,allocatable :: quadratic(:)

end module globals
