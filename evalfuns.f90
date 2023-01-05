subroutine sevalf(n,x,f,ind,inform)

	use globals, only: sF
	use myproblem

	implicit none

	! SCALAR ARGUMENTS
	integer, intent(in) :: n,ind
	integer, intent(out) :: inform
	real(kind=8), intent(in) :: x(n)
	real(kind=8), intent(out) :: f
	
	! FUNCTIONS
	logical :: IsANumber
	
	call evalf(n,x,f,ind)
	
	if ( .not. IsANumber(f) ) then
		inform = -1
		write(*,1000) ind
	end if
	
	f = sF(ind) * f

 1000 format(/,1X,'WARNING: The objective function value ',   &
			'computed by the user-supplied',/,1X,'subroutine ', &
			'evalf may be +Inf, -Inf or NaN. Function number:',1X,I4)
	
end subroutine sevalf

!***********************************************************************
!***********************************************************************

subroutine sevalg(n,x,g,ind,inform)

	use globals, only: sF
	use myproblem
	
	implicit none
	
	! SCALAR ARGUMENTS
	integer, intent(in) :: n,ind
	integer, intent(out) :: inform
	
	! ARRAY ARGUMENTS
	real(kind=8), intent(in)  :: x(n)
	real(kind=8), intent(out) :: g(n)
	
	! LOCAL SCALARS
	integer :: i
	logical :: IsANumber
  
	inform = 0 
	
	call evalg(n,x,g,ind)
	
	do i = 1,n
		if ( .not. IsANumber(g(i)) ) then
			inform = -1
			write(*,1000) ind
		end if
	end do
	
	g = sF(ind) * g

 1000	format(/,1X,'WARNING: There is an element whose value ', &
                'may be +Inf, -Inf or NaN in the',/,1X,'gradient of ', &
                'the objective computed by the user-supplied ',   &
                'subroutine evalg. Function number:',1X,I4)	
end subroutine sevalg


!***********************************************************************
!***********************************************************************

subroutine evalphi(stp,phi,ind)

	use globals, only: n,x,d

	implicit none

	! SCALAR ARGUMENTS
	integer, intent(in) :: ind
	real(kind=8), intent(in) :: stp
	real(kind=8), intent(out) :: phi
	
	! LOCAL SCALAR
	integer :: informfg

	call sevalf(n,x + stp * d,phi,ind,informfg)

end subroutine evalphi

!***********************************************************************
!***********************************************************************

subroutine evalgphi(stp,gphi,ind)

	use globals, only: n,x,d

	implicit none

	! SCALAR ARGUMENTS
	integer, intent(in) :: ind
	real(kind=8), intent(in) :: stp
	real(kind=8), intent(out) :: gphi
	
	! LOCAL SCALAR
	integer :: informfg

	! LOCAL ARRAYS
	real(kind=8) :: g(n)

	call sevalg(n,x + stp * d,g,ind,informfg)

	gphi = dot_product(g,d)

end subroutine evalgphi

!***********************************************************************
!***********************************************************************

logical function IsANumber(x)

  implicit none

  ! SCALAR ARGUMENTS
  real(kind=8), intent(in) :: x
  
  ! LOCAL SCALAR
  real(kind=8) :: bignum
  
  bignum = 1.0d+99

  IsANumber = .true.
  if ( .not. abs( x ) .le. bignum ) IsANumber = .false.

end function IsANumber
