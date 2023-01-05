module myproblem

use globals, only: problem,seed

implicit none

  ! SUBROUTINES
  public :: inip, evalf, evalg, quadstep

contains

	subroutine inip(n,m,x,quadratic,scaleF,checkder)

	implicit none

	! SCALAR ARGUMENTS
	integer, intent(out) :: n,m
	logical, intent(out) :: scaleF,checkder
	
	! ARRAY ARGUMENTS
	real(kind=8), allocatable, intent(out) :: x(:)
	logical, allocatable, intent(out) :: quadratic(:)
	
	! LOCAL SCALARS
	integer :: i,allocerr
	real, parameter :: pi = 3.1415927
	real(kind=8), parameter :: zero = 0.0d0, one = 1.0d0
	real(kind=8) :: a,b
	
	! FUNCTIONS
	real(kind=8) :: drand
	
	! JOS1
	! Dynamic Weighted Aggregation for Evolutionary Multi-Objetive Optimization: Why Does It Work and How?
	
	if ( problem == 1 ) then 
	
			! Number of variables

			n = 1000
			
			! Number of objectives
			
			m = 2
			
			allocate(x(n),quadratic(m),stat=allocerr)
			!allocate(x(n),l(n),u(n),stat=allocerr)
			if ( allocerr .ne. 0 ) then
				 write(*,*) 'Allocation error in main program'
				 stop
			end if
			
			! Initial point
			
			a = - 1.0d4
			b =   1.0d4
			
			do i = 1,n
					x(i) = a + ( b - a ) * drand(seed)
			end do
			
			! Quadratic functions?
			
			do i = 1,m
					quadratic(i) = .true.
			end do
			
			! Scale the problem? 
			
			scaleF   = .false.
			
			! Check derivatives?
			
			checkder = .false.
	end if
	
! ----------------------------------------------------------------------	

	! SLC2
	! The Directed Search Method for Unconstrained Multi-Objective Optimization Problems	

	if ( problem == 2 ) then 
	
			! Number of variables

			n = 100
			
			! Number of objectives
			
			m = 2
			
			allocate(x(n),quadratic(m),stat=allocerr)
			!allocate(x(n),l(n),u(n),stat=allocerr)
			if ( allocerr .ne. 0 ) then
				 write(*,*) 'Allocation error in main program'
				 stop
			end if
			
			! Initial point
			
			a = - 1.0d2
			b =   1.0d2
			
			do i = 1,n
					x(i) = a + ( b - a ) * drand(seed)
			end do
			
			! Quadratic functions?
			
			do i = 1,m
					quadratic(i) = .false.
			end do
			
			! Scale the problem?
			
			scaleF   = .false.
			
			! Check derivatives?
			
			checkder = .false.
	end if
	
! ----------------------------------------------------------------------	

	! SLCDT1
	! Convergence of stochastic search algorithms to finite size pareto set approximations
 
	
	if ( problem == 3 ) then 
	
			! Number of variables
			
			n = 2
			
			! Number of objectives
			
			m = 2
			
			allocate(x(n),quadratic(m),stat=allocerr)
			!allocate(x(n),l(n),u(n),stat=allocerr)
			if ( allocerr .ne. 0 ) then
				 write(*,*) 'Allocation error in main program'
				 stop
			end if
			
			! Initial point
			
			a = - 5.0d0
			b =   5.0d0
			
			do i = 1,n
					x(i) = a + ( b - a ) * drand(seed)
			end do 
			
			! Quadratic functions?
			
			do i = 1,m
					quadratic(i) = .false.
			end do
			
			! Scale the problem?
			
			scaleF   = .false.
			
			! Check derivatives?
			
			checkder = .false.
	end if


	
! ----------------------------------------------------------------------

! AP1
! Example 1 of "A modified Quasi-Newton method for vector optimization problem"

	if ( problem == 4 ) then 
	
			! Number of variables
			
			n = 2
			
			! Number of objectives
			
			m = 3
			
			allocate(x(n),quadratic(m),stat=allocerr)
			!allocate(x(n),l(n),u(n),stat=allocerr)
			if ( allocerr .ne. 0 ) then
				 write(*,*) 'Allocation error in main program'
				 stop
			end if
			
			! Initial point
			
			a = - 1.0d2
			b =   1.0d2
			
			do i = 1,n
					x(i) = a + ( b - a ) * drand(seed)
			end do 
			
			! Quadratic functions?
			
			do i = 1,m
					quadratic(i) = .false.
			end do
			
			! Scale the problem?
			
			scaleF   = .false.
			
			! Check derivatives?
			
			checkder = .false.
	end if
	
! ----------------------------------------------------------------------

	! AP2
	! Example 2 of "A modified Quasi-Newton method for vector optimization problem"

	if ( problem == 5 ) then 
	
			! Number of variables
			
			n = 1
			
			! Number of objectives
			
			m = 2
			
			allocate(x(n),quadratic(m),stat=allocerr)
			!allocate(x(n),l(n),u(n),stat=allocerr)
			if ( allocerr .ne. 0 ) then
				 write(*,*) 'Allocation error in main program'
				 stop
			end if
			
			! Initial point
			
			a = - 1.0d2
			b =   1.0d2
			
			do i = 1,n
					x(i) = a + ( b - a ) * drand(seed)
			end do 
			
			! Quadratic functions?
			
			do i = 1,m
					quadratic(i) = .true.
			end do
			
			! Scale the problem?
			
			scaleF   = .false.
			
			! Check derivatives?
			
			checkder = .false.
	end if	
	
! ----------------------------------------------------------------------

	! AP3
	! Example 3 of "A modified Quasi-Newton method for vector optimization problem"

	if ( problem == 6 ) then 
	
			! Number of variables
			
			n = 2
			
			! Number of objectives
			
			m = 2
			
			allocate(x(n),quadratic(m),stat=allocerr)
			!allocate(x(n),l(n),u(n),stat=allocerr)
			if ( allocerr .ne. 0 ) then
				 write(*,*) 'Allocation error in main program'
				 stop
			end if
			
			! Initial point
			
			a = - 1.0d2
			b =   1.0d2
			
			do i = 1,n
					x(i) = a + ( b - a ) * drand(seed)
			end do 
			
			! Quadratic functions?
			
			do i = 1,m
					quadratic(i) = .false.
			end do
			
			! Scale the problem?
			
			scaleF   = .false.
			
			! Check derivatives?
			
			checkder = .false.
	end if
	
! ----------------------------------------------------------------------

	! AP4
	! Exemple 4 of "A modified Quasi-Newton method for vector optimization problem"
	
	if ( problem == 7 ) then 
	
			! Number of variables
			
			n = 3
			
			! Number of objectives
			
			m = 3
			
			allocate(x(n),quadratic(m),stat=allocerr)
			!allocate(x(n),l(n),u(n),stat=allocerr)
			if ( allocerr .ne. 0 ) then
				 write(*,*) 'Allocation error in main program'
				 stop
			end if
			
			! Initial point
			
			a = - 1.0d2
			b =   1.0d2
			
			do i = 1,n
					x(i) = a + ( b - a ) * drand(seed)
			end do 
			
			! Quadratic functions?
			
			do i = 1,m
					quadratic(i) = .false.
			end do
			
			! Scale the problem?
			
			scaleF   = .false.
			
			! Check derivatives?
			
			checkder = .false.
	end if
	
	! ----------------------------------------------------------------------
	
	! Lov1
	! "A SYNTHETIC APPROACH TO MULTIOBJECTIVE OPTIMIZATION"
	
	if ( problem == 8 ) then 
	
			! Number of variables
			
			n = 2
			
			! Number of objectives
			
			m = 2
			
			allocate(x(n),quadratic(m),stat=allocerr)
			!allocate(x(n),l(n),u(n),stat=allocerr)
			if ( allocerr .ne. 0 ) then
				 write(*,*) 'Allocation error in main program'
				 stop
			end if
			
			! Initial point
			
			a =  -1.0d2
			b =   1.0d2
			
			do i = 1,n
					x(i) = a + ( b - a ) * drand(seed)
			end do 
			
			! Quadratic functions?
			
			do i = 1,m
					quadratic(i) = .true.
			end do
			
			! Scale the problem?
			
			scaleF   = .false.
			
			! Check derivatives?
			
			checkder = .false.
	end if
	
! ----------------------------------------------------------------------
	
	! Lov3
	! "A SYNTHETIC APPROACH TO MULTIOBJECTIVE OPTIMIZATION"
	
	if ( problem == 9 ) then 
	
			! Number of variables
			
			n = 2
			
			! Number of objectives
			
			m = 2
			
			allocate(x(n),quadratic(m),stat=allocerr)
			!allocate(x(n),l(n),u(n),stat=allocerr)
			if ( allocerr .ne. 0 ) then
				 write(*,*) 'Allocation error in main program'
				 stop
			end if
			
			! Initial point
			
			a = - 1.0d2
			b =   1.0d2
			
			do i = 1,n
					x(i) = a + ( b - a ) * drand(seed)
			end do 
			
			! Quadratic functions?
			
			quadratic(1) = .true.
			quadratic(2) = .false.
			
			! Scale the problem?
			
			scaleF   = .false.
			
			! Check derivatives?
			
			checkder = .false.
	end if
	
! ----------------------------------------------------------------------
	
	! Lov4
	! "A SYNTHETIC APPROACH TO MULTIOBJECTIVE OPTIMIZATION"
	
	if ( problem == 10 ) then 
	
			! Number of variables
			
			n = 2
			
			! Number of objectives
			
			m = 2
			
			allocate(x(n),quadratic(m),stat=allocerr)
			!allocate(x(n),l(n),u(n),stat=allocerr)
			if ( allocerr .ne. 0 ) then
				 write(*,*) 'Allocation error in main program'
				 stop
			end if
			
			! Initial point
			
			a = - 1.0d2
			b =   1.0d2
			
			do i = 1,n
					x(i) = a + ( b - a ) * drand(seed)
			end do 
			
			! Quadratic functions?
			
			quadratic(1) = .false.
			quadratic(2) = .true.
			
			! Scale the problem?
			
			scaleF   = .false.
			
			! Check derivatives?
			
			checkder = .false.
	end if
	
	! ----------------------------------------------------------------------
	
	! FF1
	! C. M. Fonseca and P. J. Fleming, “An overview of evolutionary algorithms in multiobjective optimization
	
	if ( problem == 11 ) then 
	
			! Number of variables
			
			n = 2
			
			! Number of objectives
			
			m = 2
			
			allocate(x(n),quadratic(m),stat=allocerr)
			!allocate(x(n),l(n),u(n),stat=allocerr)
			if ( allocerr .ne. 0 ) then
				 write(*,*) 'Allocation error in main program'
				 stop
			end if
			
			! Initial point
			
			a = - 1.0d0
			b =   1.0d0
			
			do i = 1,n
					x(i) = a + ( b - a ) * drand(seed)
			end do 
			
			! Quadratic functions?
			
			do i = 1,m
					quadratic(i) = .false.
			end do
			
			! Scale the problem?
			
			scaleF   = .false.
			
			! Check derivatives?
			
			checkder = .false.
	end if	
	
	! ----------------------------------------------------------------------
	
	! FDS
	! NEWTON’S METHOD FOR MULTIOBJECTIVE OPTIMIZATION
	
	if ( problem == 12 ) then 
	
			! Number of variables
			
			n = 5000
			
			! Number of objectives
			
			m = 3
			
			allocate(x(n),quadratic(m),stat=allocerr)
			!allocate(x(n),l(n),u(n),stat=allocerr)
			if ( allocerr .ne. 0 ) then
				 write(*,*) 'Allocation error in main program'
				 stop
			end if
			
			! Initial point
			
			a = - 2.0d0
			b =   2.0d0
			
			do i = 1,n
					x(i) = a + ( b - a ) * drand(seed)
			end do 
			
			! Quadratic functions?
			
			do i = 1,m
					quadratic(i) = .false.
			end do
			
			! Scale the problem?
			
			scaleF   = .true.
			
			! Check derivatives?
			
			checkder = .false.
	end if		
	
! ----------------------------------------------------------------------
	
	!  MMR1 (modified)		
	!  Box-constrained multi-objective optimization: A gradient-like method without ‘‘a priori’’ scalarization	
	
	if ( problem == 13 ) then 
	
			! Number of variables
	
			n = 2
			
			! Number of objectives
			
			m = 2
			
			allocate(x(n),quadratic(m),stat=allocerr)
			!allocate(x(n),l(n),u(n),stat=allocerr)
			if ( allocerr .ne. 0 ) then
				 write(*,*) 'Allocation error in main program'
				 stop
			end if
			
			! Initial point

			a =   0.0d0
			b =   1.0d0
			
			do i = 1,n
					x(i) = a + ( b - a ) * drand(seed)
			end do 
			
			! Quadratic functions?
			
			do i = 1,m
					quadratic(i) = .false.
			end do
			
			! Scale the problem?
			
			scaleF   = .false.
			
			! Check derivatives?
			
			checkder = .false.
	end if
	
! ----------------------------------------------------------------------

	!  MMR5
	!  Box-constrained multi-objective optimization: A gradient-like method without ‘‘a priori’’ scalarization	
	
	if ( problem == 14 ) then 
	
			! Number of variables
		
			n = 5000
			
			! Number of objectives
			
			m = 2
			
			allocate(x(n),quadratic(m),stat=allocerr)
			!allocate(x(n),l(n),u(n),stat=allocerr)
			if ( allocerr .ne. 0 ) then
				 write(*,*) 'Allocation error in main program'
				 stop
			end if
			
			! Initial point

			a = - 5.0d0
			b =   5.0d0
			
			do i = 1,n
					x(i) = a + ( b - a ) * drand(seed)
			end do 
			
			! Quadratic functions?
			
			do i = 1,m
					quadratic(i) = .false.
			end do
			
			! Scale the problem?
			
			scaleF   = .false.
			
			! Check derivatives?
			
			checkder = .false.
	end if	
	
! ----------------------------------------------------------------------

	!  MOP1 ( Particular case of JOS1 )		
	!  A Review of Multiobjective Test Problems and a Scalable Test Problem Toolkit

	
	if ( problem == 15 ) then 
	
			! Number of variables
	
			n = 1
			
			! Number of objectives
			
			m = 2
			
			allocate(x(n),quadratic(m),stat=allocerr)
			!allocate(x(n),l(n),u(n),stat=allocerr)
			if ( allocerr .ne. 0 ) then
				 write(*,*) 'Allocation error in main program'
				 stop
			end if

			! Initial point
			
			a = - 1.0d5
			b =   1.0d5
			
			do i = 1,n
					x(i) = a + ( b - a ) * drand(seed)
			end do 
			
			! Quadratic functions?
			
			do i = 1,m
					quadratic(i) = .true.
			end do
			
			! Scale the problem?
			
			scaleF   = .false.
			
			! Check derivatives?
			
			checkder = .false.
	end if		
	
! ----------------------------------------------------------------------

	!  MOP2
	!  A Review of Multiobjective Test Problems and a Scalable Test Problem Toolkit		
	
	if ( problem == 16 ) then 
	
			! Number of variables
	
			n = 2
			
			! Number of objectives
			
			m = 2
			
			allocate(x(n),quadratic(m),stat=allocerr)
			!allocate(x(n),l(n),u(n),stat=allocerr)
			if ( allocerr .ne. 0 ) then
				 write(*,*) 'Allocation error in main program'
				 stop
			end if
			
			! Initial point

			a = - 1.0d0
			b =   1.0d0
			
			do i = 1,n
					x(i) = a + ( b - a ) * drand(seed)
			end do 
			
			! Quadratic functions?
			
			do i = 1,m
					quadratic(i) = .false.
			end do
			
			! Scale the problem?
			
			scaleF   = .false.
			
			! Check derivatives?
			
			checkder = .false.
	end if		
	
! ----------------------------------------------------------------------

	!  MOP3	
	!  A Review of Multiobjective Test Problems and a Scalable Test Problem Toolkit	
	
	if ( problem == 17 ) then 
	
			! Number of variables
		
			n = 2
			
			! Number of objectives
			
			m = 2
			
			allocate(x(n),quadratic(m),stat=allocerr)
			!allocate(x(n),l(n),u(n),stat=allocerr)
			if ( allocerr .ne. 0 ) then
				 write(*,*) 'Allocation error in main program'
				 stop
			end if
			
			! Initial point
			
			a = - pi
			b =   pi
			
			do i = 1,n
					x(i) = a + ( b - a ) * drand(seed)
			end do 
			
			! Quadratic functions?
			
			quadratic(1) = .false.
			quadratic(2) = .true.
			
			! Scale the problem?
			
			scaleF   = .false.
			
			! Check derivatives?
			
			checkder = .false.
	end if	
	
! ----------------------------------------------------------------------

	!  MOP5
	!  A Review of Multiobjective Test Problems and a Scalable Test Problem Toolkit
	
	if ( problem == 18 ) then 
	
			! Number of variables
	
			n = 2
			
			! Number of objectives
			
			m = 3
			
			allocate(x(n),quadratic(m),stat=allocerr)
			!allocate(x(n),l(n),u(n),stat=allocerr)
			if ( allocerr .ne. 0 ) then
				 write(*,*) 'Allocation error in main program'
				 stop
			end if
			
			! Initial point
			
			a = - 1.0d0
			b =   1.0d0
			
			do i = 1,n
					x(i) = a + ( b - a ) * drand(seed)
			end do 
			
			! Quadratic functions?
			
			quadratic(1) = .false.
			quadratic(2) = .true.
			quadratic(3) = .false.
			
			! Scale the problem?
			
			scaleF   = .false.
			
			! Check derivatives?
			
			checkder = .false.
	end if		
	
! ----------------------------------------------------------------------

	!  MOP7
	!  A Review of Multiobjective Test Problems and a Scalable Test Problem Toolkit
	
	if ( problem == 19 ) then 
	
			! Number of variables
	 		
			n = 2
			
			! Number of objectives
			
			m = 3
			
			allocate(x(n),quadratic(m),stat=allocerr)
			!allocate(x(n),l(n),u(n),stat=allocerr)
			if ( allocerr .ne. 0 ) then
				 write(*,*) 'Allocation error in main program'
				 stop
			end if
			
			! Initial point

			a = - 4.0d2
			b =   4.0d2
			
			do i = 1,n
					x(i) = a + ( b - a ) * drand(seed)
			end do 
			
			! Quadratic functions?
			
			do i = 1,m
					quadratic(i) = .true.
			end do
			
			! Scale the problem?
			
			scaleF   = .false.
			
			! Check derivatives?
			
			checkder = .false.
	end if				
	
! ----------------------------------------------------------------------

	!   DGO1
	!   A Review of Multiobjective Test Problems and a Scalable Test Problem Toolkit
	
	if ( problem == 20 ) then 
	
			! Number of variables
	
			n = 1
			
			! Number of objectives
			
			m = 2
			
			allocate(x(n),quadratic(m),stat=allocerr)
			!allocate(x(n),l(n),u(n),stat=allocerr)
			if ( allocerr .ne. 0 ) then
				 write(*,*) 'Allocation error in main program'
				 stop
			end if
			
			! Initial point
			
			a = - 1.0d1
			b =   1.3d1
			
			do i = 1,n
					x(i) = a + ( b - a ) * drand(seed)
			end do 
			
			! Quadratic functions?
			
			do i = 1,m
					quadratic(i) = .false.
			end do
			
			! Scale the problem?
			
			scaleF   = .false.
			
			! Check derivatives?
			
			checkder = .false.
	end if	
	
! ----------------------------------------------------------------------

	!   Far1
	!   A Review of Multiobjective Test Problems and a Scalable Test Problem Toolkit
	
	if ( problem == 21 ) then 
	
			! Number of variables

			n = 2
			
			! Number of objectives
			
			m = 2
			
			allocate(x(n),quadratic(m),stat=allocerr)
			!allocate(x(n),l(n),u(n),stat=allocerr)
			if ( allocerr .ne. 0 ) then
				 write(*,*) 'Allocation error in main program'
				 stop
			end if
			
			! Initial point

			a = - 1.0d0
			b =   1.0d0
			
			do i = 1,n
					x(i) = a + ( b - a ) * drand(seed)
			end do 
			
			! Quadratic functions?
			
			do i = 1,m
					quadratic(i) = .false.
			end do
			
			! Scale the problem?
			
			scaleF   = .false.
			
			! Check derivatives?
			
			checkder = .false.
	end if			
		
	
! ----------------------------------------------------------------------

	!   MLF1
	!   A Review of Multiobjective Test Problems and a Scalable Test Problem Toolkit
	
	if ( problem == 22 ) then 
	
			! Number of variables

			n = 1
			
			! Number of objectives
			
			m = 2
			
			allocate(x(n),quadratic(m),stat=allocerr)
			!allocate(x(n),l(n),u(n),stat=allocerr)
			if ( allocerr .ne. 0 ) then
				 write(*,*) 'Allocation error in main program'
				 stop
			end if
			
			! Initial point

			a =  0.0d0
			b =  2.0d1
			
			do i = 1,n
					x(i) = a + ( b - a ) * drand(seed)
			end do 
			
			! Quadratic functions?
			
			do i = 1,m
					quadratic(i) = .false.
			end do
			
			! Scale the problem?
			
			scaleF   = .false.
			
			! Check derivatives?
			
			checkder = .false.
	end if	
	
! ----------------------------------------------------------------------

	!   MLF2
	!   A Review of Multiobjective Test Problems and a Scalable Test Problem Toolkit
	
	if ( problem == 23 ) then 
	
			! Number of variables
			
			n = 2
			
			! Number of objectives
			
			m = 2
			
			allocate(x(n),quadratic(m),stat=allocerr)
			!allocate(x(n),l(n),u(n),stat=allocerr)
			if ( allocerr .ne. 0 ) then
				 write(*,*) 'Allocation error in main program'
				 stop
			end if
			
			! Initial point

			a = - 1.0d2
			b =   1.0d2
			
			do i = 1,n
					x(i) = a + ( b - a ) * drand(seed)
			end do 
			
			! Quadratic functions?
			
			do i = 1,m
					quadratic(i) = .false.
			end do
			
			! Scale the problem?
			
			scaleF   = .false.
			
			! Check derivatives?
			
			checkder = .false.
	end if	
	
! ----------------------------------------------------------------------

	!   SP1
	!  	A Review of Multiobjective Test Problems and a Scalable Test Problem Toolkit
	
	if ( problem == 24 ) then 
	
			! Number of variables

			n = 2
			
			! Number of objectives
			
			m = 2
			
			allocate(x(n),quadratic(m),stat=allocerr)
			!allocate(x(n),l(n),u(n),stat=allocerr)
			if ( allocerr .ne. 0 ) then
				 write(*,*) 'Allocation error in main program'
				 stop
			end if
			
			! Initial point

			a = - 1.0d2
			b =   1.0d2
			
			do i = 1,n
					x(i) = a + ( b - a ) * drand(seed)
			end do 
			
			! Quadratic functions?
			
			do i = 1,m
					quadratic(i) = .true.
			end do
			
			! Scale the problem?
			
			scaleF   = .false.
			
			! Check derivatives?
			
			checkder = .false.
	end if		
	
! ----------------------------------------------------------------------

	!   SSFYY2
	!   A Review of Multiobjective Test Problems and a Scalable Test Problem Toolkit
	
	if ( problem == 25 ) then 
	
			! Number of variables
		
			n = 1
			
			! Number of objectives
			
			m = 2
			
			allocate(x(n),quadratic(m),stat=allocerr)
			!allocate(x(n),l(n),u(n),stat=allocerr)
			if ( allocerr .ne. 0 ) then
				 write(*,*) 'Allocation error in main program'
				 stop
			end if
			
			! Initial point

			a = - 1.0d2
			b =   1.0d2
			
			do i = 1,n
					x(i) = a + ( b - a ) * drand(seed)
			end do 
			
			! Quadratic functions?
			
			quadratic(1) = .false.
			quadratic(2) = .false.
			
			! Scale the problem?
			
			scaleF   = .false.
			
			! Check derivatives?
			
			checkder = .false.
	end if		
	
! ----------------------------------------------------------------------

	!   SK1
	!   A Review of Multiobjective Test Problems and a Scalable Test Problem Toolkit
	
	if ( problem == 26 ) then 
	
			! Number of variables
  		
			n = 1
			
			! Number of objectives
			
			m = 2
			
			allocate(x(n),quadratic(m),stat=allocerr)
			!allocate(x(n),l(n),u(n),stat=allocerr)
			if ( allocerr .ne. 0 ) then
				 write(*,*) 'Allocation error in main program'
				 stop
			end if
			
			! Initial point
			
			a = - 1.0d2
			b =   1.0d2
			
			do i = 1,n
					x(i) = a + ( b - a ) * drand(seed)
			end do 
			
			! Quadratic functions?
			
			do i = 1,m
					quadratic(i) = .false.
			end do
			
			! Scale the problem?
			
			scaleF   = .false.
			
			! Check derivatives?
			
			checkder = .false.
	end if	
	
! ----------------------------------------------------------------------

	!   SK2
	!  	A Review of Multiobjective Test Problems and a Scalable Test Problem Toolkit
	
	if ( problem == 27 ) then 
	
			! Number of variables

			n = 4
			
			! Number of objectives
			
			m = 2
			
			allocate(x(n),quadratic(m),stat=allocerr)
			!allocate(x(n),l(n),u(n),stat=allocerr)
			if ( allocerr .ne. 0 ) then
				 write(*,*) 'Allocation error in main program'
				 stop
			end if
			
			! Initial point

			a = - 1.0d1
			b =   1.0d1
			
			do i = 1,n
					x(i) = a + ( b - a ) * drand(seed)
			end do 
			
			! Quadratic functions?
			
			quadratic(1) = .true.
			quadratic(2) = .false.
			
			! Scale the problem?
			
			scaleF   = .false.
			
			! Check derivatives?
			
			checkder = .false.
	end if		
	
! ----------------------------------------------------------------------

	!   VU1
	!   A Review of Multiobjective Test Problems and a Scalable Test Problem Toolkit
	
	if ( problem == 28 ) then 
	
			! Number of variables

			n = 2
			
			! Number of objectives
			
			m = 2
			
			allocate(x(n),quadratic(m),stat=allocerr)
			!allocate(x(n),l(n),u(n),stat=allocerr)
			if ( allocerr .ne. 0 ) then
				 write(*,*) 'Allocation error in main program'
				 stop
			end if
			
			! Initial point

			a = - 3.0d0
			b =   3.0d0
			
			do i = 1,n
					x(i) = a + ( b - a ) * drand(seed)
			end do 
			
			! Quadratic functions?
			
			quadratic(1) = .false.
			quadratic(2) = .true.
			
			! Scale the problem?
			
			scaleF   = .false.
			
			! Check derivatives?
			
			checkder = .false.
	end if
			
! ----------------------------------------------------------------------

	!   Hil1
	!   Generalized Homotopy Approach to Multiobjective Optimization
	
	if ( problem == 29 ) then 
	
			! Number of variables
	
			n = 2
			
			! Number of objectives
			
			m = 2
			
			allocate(x(n),quadratic(m),stat=allocerr)
			!allocate(x(n),l(n),u(n),stat=allocerr)
			if ( allocerr .ne. 0 ) then
				 write(*,*) 'Allocation error in main program'
				 stop
			end if
			
			! Initial point

			a =  0.0d0
			b =  1.0d0
			
			do i = 1,n
					x(i) = a + ( b - a ) * drand(seed)
			end do 
			
			! Quadratic functions?
			
			do i = 1,m
					quadratic(i) = .false.
			end do
			
			! Scale the problem?
			
			scaleF   = .false.
			
			! Check derivatives?
			
			checkder = .false.
	end if			
	
! ----------------------------------------------------------------------

	!   DD1
	!   I. Das and J. E. Dennis. Normal-boundary intersection: A new method for generating
	!		the Pareto surface in nonlinear multicriteria optimization problems. SIAM J. Optim.,
	!		8(3):631–657, 1998.
	
	if ( problem == 30 ) then 
	
			! Number of variables
	
			n = 5
			
			! Number of objectives
			
			m = 2
			
			allocate(x(n),quadratic(m),stat=allocerr)
			!allocate(x(n),l(n),u(n),stat=allocerr)
			if ( allocerr .ne. 0 ) then
				 write(*,*) 'Allocation error in main program'
				 stop
			end if
			
			! Initial point

			a =  -20.0d0
			b =   20.0d0
			
			do i = 1,n
					x(i) = a + ( b - a ) * drand(seed)
			end do 
			
			! Quadratic functions?
			
			quadratic(1) = .true.
			quadratic(2) = .false.
			
			! Scale the problem?
			
			scaleF   = .false.
			
			! Check derivatives?
			
			checkder = .false.
	end if				
	
! ----------------------------------------------------------------------

	! 	KW2
	!   I.Y. Kim, O.L. de Weck, Adaptive weighted-sum method for bi-objective optimization: Pareto front generation
	
	if ( problem == 31 ) then 
	
			! Number of variables
	
			n = 2
			
			! Number of objectives
			
			m = 2
			
			allocate(x(n),quadratic(m),stat=allocerr)
			!allocate(x(n),l(n),u(n),stat=allocerr)
			if ( allocerr .ne. 0 ) then
				 write(*,*) 'Allocation error in main program'
				 stop
			end if
			
			! Initial point

			a = -3.0d0
			b =  3.0d0
			
			do i = 1,n
					x(i) = a + ( b - a ) * drand(seed)
			end do 
			
			! Quadratic functions?
			
			do i = 1,m
					quadratic(i) = .false.
			end do
			
			! Scale the problem?
			
			scaleF   = .false.
			
			! Check derivatives?
			
			checkder = .false.
	end if		
	
! ----------------------------------------------------------------------

	! 	Toi4
	
	if ( problem == 32 ) then 
	
			! Number of variables
	
			n = 4
			
			! Number of objectives
			
			m = 2
			
			allocate(x(n),quadratic(m),stat=allocerr)
			!allocate(x(n),l(n),u(n),stat=allocerr)
			if ( allocerr .ne. 0 ) then
				 write(*,*) 'Allocation error in main program'
				 stop
			end if
			
			! Initial point

			a = -1.0d2
			b =  1.0d2
			
			do i = 1,n
					x(i) = a + ( b - a ) * drand(seed)
			end do 
			
			! Quadratic functions?
			
			quadratic(1) = .false.
			quadratic(2) = .false.
			
			! Scale the problem?
			
			scaleF   = .false.
			
			! Check derivatives?
			
			checkder = .false.
	end if					
	
! ----------------------------------------------------------------------

	! 	Toi8
	
	if ( problem == 33 ) then 
	
			! Number of variables
	
			n = 2
			
			! Number of objectives
			
			m = n
			
			allocate(x(n),quadratic(m),stat=allocerr)
			!allocate(x(n),l(n),u(n),stat=allocerr)
			if ( allocerr .ne. 0 ) then
				 write(*,*) 'Allocation error in main program'
				 stop
			end if
			
			! Initial point

			a = -1.0d0
			b =  1.0d0
			
			do i = 1,n
					x(i) = a + ( b - a ) * drand(seed)
			end do 
			
			! Quadratic functions?
			
			do i = 1,m
					quadratic(i) = .false.
			end do
			
			! Scale the problem?
			
			scaleF   = .false.
			
			! Check derivatives?
			
			checkder = .false.
	end if			
	
! ----------------------------------------------------------------------

	! 	Toi9
	
	if ( problem == 34 ) then 
	
			! Number of variables
	
			n = 300
			
			! Number of objectives
			
			m = n
			
			allocate(x(n),quadratic(m),stat=allocerr)
			!allocate(x(n),l(n),u(n),stat=allocerr)
			if ( allocerr .ne. 0 ) then
				 write(*,*) 'Allocation error in main program'
				 stop
			end if
			
			! Initial point

			a = -1.0d2
			b =  1.0d2
			
			do i = 1,n
					x(i) = a + ( b - a ) * drand(seed)
			end do 
			
			! Quadratic functions?
			
			do i = 1,m
					quadratic(i) = .false.
			end do
			
			! Scale the problem?
			
			scaleF   = .false.
			
			! Check derivatives?
			
			checkder = .false.
	end if		
	
! ----------------------------------------------------------------------

	! 	Toi10
	
	if ( problem == 35 ) then 
	
			! Number of variables
	
			n = 50
			
			! Number of objectives
			
			m = n - 1
			
			allocate(x(n),quadratic(m),stat=allocerr)
			!allocate(x(n),l(n),u(n),stat=allocerr)
			if ( allocerr .ne. 0 ) then
				 write(*,*) 'Allocation error in main program'
				 stop
			end if
			
			! Initial point

			a = -2.0d0
			b =  2.0d0
			
			do i = 1,n
					x(i) = a + ( b - a ) * drand(seed)
			end do 
			
			! Quadratic functions?
			
			do i = 1,m
					quadratic(i) = .false.
			end do
			
			! Scale the problem?
			
			scaleF   = .false.
			
			! Check derivatives?
			
			checkder = .false.
	end if			
	
! ----------------------------------------------------------------------

	! 	MGH9 (Gaussian)
	
	if ( problem == 36 ) then 
	
			! Number of variables
	
			n = 3
			
			! Number of objectives
			
			m = 15
			
			allocate(x(n),quadratic(m),stat=allocerr)
			!allocate(x(n),l(n),u(n),stat=allocerr)
			if ( allocerr .ne. 0 ) then
				 write(*,*) 'Allocation error in main program'
				 stop
			end if
			
			! Initial point

			a = -1.0d0
			b =  1.0d0
			
			do i = 1,n
					x(i) = a + ( b - a ) * drand(seed)
			end do 
			
			! Quadratic functions?
			
			do i = 1,m
					quadratic(i) = .false.
			end do
			
			! Scale the problem?
			
			scaleF   = .false.
			
			! Check derivatives?
			
			checkder = .false.
	end if		
	
	
! ----------------------------------------------------------------------

	! 	MGH16 (Brown and Dennis)
	
	if ( problem == 37 ) then 
	
			! Number of variables
	
			n = 4
			
			! Number of objectives
			
			m = 100
			
			allocate(x(n),quadratic(m),stat=allocerr)
			!allocate(x(n),l(n),u(n),stat=allocerr)
			if ( allocerr .ne. 0 ) then
				 write(*,*) 'Allocation error in main program'
				 stop
			end if
			
			! Initial point

			a = -1.0d2
			b =  1.0d2
			
			do i = 1,n
					x(i) = a + ( b - a ) * drand(seed)
			end do 
			
			! Quadratic functions?
			
			do i = 1,m
					quadratic(i) = .false.
			end do
			
			! Scale the problem?
			
			scaleF   = .false.
			
			! Check derivatives?
			
			checkder = .false.
	end if		
	
! ----------------------------------------------------------------------

	! 	MGH26 (Trigonometric)
	
	if ( problem == 38 ) then 
	
			! Number of variables
	
			n = 4
			
			! Number of objectives
			
			m = n
			
			allocate(x(n),quadratic(m),stat=allocerr)
			!allocate(x(n),l(n),u(n),stat=allocerr)
			if ( allocerr .ne. 0 ) then
				 write(*,*) 'Allocation error in main program'
				 stop
			end if
			
			! Initial point

			a = -1.0d0
			b =  1.0d0
			
			do i = 1,n
					x(i) = a + ( b - a ) * drand(seed)
			end do 
			
			! Quadratic functions?
			
			do i = 1,m
					quadratic(i) = .false.
			end do
			
			! Scale the problem?
			
			scaleF   = .false.
			
			! Check derivatives?
			
			checkder = .false.
	end if
	
! ----------------------------------------------------------------------

	! 	MGH33 (Linear function - rank 1)
	
	if ( problem == 39 ) then 
	
			! Number of variables
	
			n = 10
			
			! Number of objectives
			
			m = 10
			
			allocate(x(n),quadratic(m),stat=allocerr)
			!allocate(x(n),l(n),u(n),stat=allocerr)
			if ( allocerr .ne. 0 ) then
				 write(*,*) 'Allocation error in main program'
				 stop
			end if
			
			! Initial point

			a = -1.0d0
			b =  1.0d0
			
			do i = 1,n
					x(i) = a + ( b - a ) * drand(seed)
			end do 
			
			! Quadratic functions?
			
			do i = 1,m
					quadratic(i) = .false.
			end do
			
			! Scale the problem?
			
			scaleF   = .false.
			
			! Check derivatives?
			
			checkder = .false.
	end if				
	
! ----------------------------------------------------------------------

	! 	DTLZ2.2 
	!	Modelling the Population Distribution in Multi-objective
	! Optimization by Generative Topographic Mapping
	
	if ( problem == 40 ) then 
	
			! Number of variables
	
			n = 3
			
			! Number of objectives
			
			m = 3
			
			allocate(x(n),quadratic(m),stat=allocerr)
			!allocate(x(n),l(n),u(n),stat=allocerr)
			if ( allocerr .ne. 0 ) then
				 write(*,*) 'Allocation error in main program'
				 stop
			end if
			
			! Initial point

			a =  0.0d0
			b =  1.0d0
			
			do i = 1,n
					x(i) = a + ( b - a ) * drand(seed)
			end do 
			
			! Quadratic functions?
			
			do i = 1,m
					quadratic(i) = .false.
			end do
			
			! Scale the problem?
			
			scaleF   = .false.
			
			! Check derivatives?
			
			checkder = .false.
	end if			
	
! ----------------------------------------------------------------------

	!  PNR
	!	 M. Preuss, B. Naujoks, and G. Rudolph, Pareto set and EMOA behaviour for simple
	!  multimodal multiobjective functions, In: Parallel Problem O Solving from Nature-PPSN IX.
	!  Springer. Berlin, 2006, pp. 513–522.
	
	if ( problem == 41 ) then 
	
			! Number of variables
	
			n = 2
			
			! Number of objectives
			
			m = 2
			
			allocate(x(n),quadratic(m),stat=allocerr)
			!allocate(x(n),l(n),u(n),stat=allocerr)
			if ( allocerr .ne. 0 ) then
				 write(*,*) 'Allocation error in main program'
				 stop
			end if
			
			! Initial point

			a = - 1.0d0
			b =   1.0d0
			
			do i = 1,n
					x(i) = a + ( b - a ) * drand(seed)
			end do 
			
			! Quadratic functions?
			
			quadratic(1) = .false.
			quadratic(2) = .true.
			
			! Scale the problem?
			
			scaleF   = .false.
			
			! Check derivatives?
			
			checkder = .false.
	end if	
	
	! ----------------------------------------------------------------------

	!  LTDZ
	!	 Combining convergence and diversity in evolutionary multiobjective optimization
	
	if ( problem == 42 ) then 
	
			! Number of variables
	
			n = 3
			
			! Number of objectives
			
			m = 3
			
			allocate(x(n),quadratic(m),stat=allocerr)
			!allocate(x(n),l(n),u(n),stat=allocerr)
			if ( allocerr .ne. 0 ) then
				 write(*,*) 'Allocation error in main program'
				 stop
			end if
			
			! Initial point

			a = 0.0d0
			b = 1.0d0
			
			do i = 1,n
					x(i) = a + ( b - a ) * drand(seed)
			end do 
			
			! Quadratic functions?
			
			do i = 1,m
					quadratic(i) = .false.
			end do
			
			! Scale the problem?
			
			scaleF   = .false.
			
			! Check derivatives?
			
			checkder = .false.
	end if	
	
	! ----------------------------------------------------------------------

	!  SLCDT2
	!	 Convergence of stochastic search algorithms to finite size pareto set approximations

	
	if ( problem == 43 ) then 
	
			! Number of variables
	
			n = 10
			
			! Number of objectives
			
			m = 3
			
			allocate(x(n),quadratic(m),stat=allocerr)
			!allocate(x(n),l(n),u(n),stat=allocerr)
			if ( allocerr .ne. 0 ) then
				 write(*,*) 'Allocation error in main program'
				 stop
			end if
			
			! Initial point

			a = -1.0d2
			b =  1.0d2
			
			do i = 1,n
					x(i) = a + ( b - a ) * drand(seed)
			end do 
			
			! Quadratic functions?
			
			do i = 1,m
					quadratic(i) = .false.
			end do
			
			! Scale the problem?
			
			scaleF   = .false.
			
			! Check derivatives?
			
			checkder = .false.
	end if		
	
		
		
	end subroutine inip

	!***********************************************************************
	!***********************************************************************

	subroutine evalf(n,x,f,ind)
	
	implicit none

	! SCALAR ARGUMENTS
	integer, intent(in) :: n,ind
	real(kind=8), intent(out) :: f
	
	! ARRAY ARGUMENTS
	real(kind=8), intent(in) :: x(n)
	
	! LOCAL SCALARS
	integer :: i
	real, parameter :: pi = 3.1415927
	real(kind=8) :: faux,A1,A2,B1,B2,a,b,t,y
	
	! JOS1 
	! Dynamic Weighted Aggregation for Evolutionary Multi-Objetive Optimization: Why Does It Work and How?
	
	if ( problem == 1 ) then 

			if ( ind == 1 ) then
					f = 0.0d0
					do i = 1,n
							f = f + x(i) ** 2
					end do
					f = f / n
					return
			end if
			
			if ( ind == 2 ) then
					f = 0.0d0
					do i = 1,n
							f = f + ( x(i) - 2.0d0 ) ** 2  
					end do
					f = f / n
					return
			end if
	
	end if

! ----------------------------------------------------------------------

	! SLC2
	! The Directed Search Method for Unconstrained Multi-Objective Optimization Problems
	
	
	if ( problem == 2 ) then 

			if ( ind == 1 ) then
					f = ( x(1) - 1.0d0 ) ** 4
					do i = 2,n
							f = f + ( x(i) - 1.0d0 ) ** 2
					end do
					return
			end if
			
			if ( ind == 2 ) then
					f = ( x(2) + 1.0d0 ) ** 4
					do i = 1,n
							if ( i /= 2 ) f = f + ( x(i) + 1.0d0 ) ** 2  
					end do
					return
			end if
	
	end if

! ----------------------------------------------------------------------

	! SLCDT1

	if ( problem == 3 ) then 
			
			if ( ind == 1 ) then
					f = 0.5d0 * ( sqrt( 1.0d0 + ( x(1) + x(2) ) ** 2 ) + &
							sqrt( 1.0d0 + ( x(1) - x(2) ) ** 2 ) + x(1) - x(2) ) +  &
							0.85d0 * exp( - ( x(1) + x(2) ) ** 2 )		
					return
			end if
			
			if ( ind == 2 ) then
					f = 0.5d0 * ( sqrt( 1.0d0 + ( x(1) + x(2) ) ** 2 ) + &
							sqrt( 1.0d0 + ( x(1) - x(2) ) ** 2 ) - x(1) + x(2) ) +  &
							0.85d0 * exp( - ( x(1) + x(2) ) ** 2 )	
					return		
			end if	
			
	end if
	

	
! ----------------------------------------------------------------------

  ! AP1: Exemple 1 of "A modified Quasi-Newton method for vector optimization problem"

	if ( problem == 4 ) then 
			
			if ( ind == 1 ) then
					f = 0.25d0 * ( ( x(1) - 1.0d0 ) ** 4 + 2.0d0 * ( x(2) - 2.0d0 ) ** 4 )
					return
			end if
			
			if ( ind == 2 ) then
					f = exp( ( x(1) + x(2) ) / 2.0d0 ) + x(1) ** 2 + x(2) ** 2
					return
			end if	
			
			if ( ind == 3 ) then
					f = 1.0d0/6.0d0 * ( exp( - x(1) ) + 2.0d0 * exp( - x(2) ) )
					return
			end if	
			
	end if		
	
! ----------------------------------------------------------------------

  ! AP2: Exemple 2 of "A modified Quasi-Newton method for vector optimization problem"

	if ( problem == 5 ) then 
			
			if ( ind == 1 ) then
					f = x(1) ** 2 - 4.0d0
					return
			end if
			
			if ( ind == 2 ) then
					f = ( x(1) - 1.0d0 ) ** 2
					return
			end if	
			
	end if		
	
! ----------------------------------------------------------------------

  ! AP3: Exemple 3 of "A modified Quasi-Newton method for vector optimization problem"

	if ( problem == 6 ) then 
			
			if ( ind == 1 ) then
					f = 0.25d0 * ( ( x(1) - 1.0d0 ) ** 4 + 2.0d0 * ( x(2) - 2.0d0 ) ** 4 )
					return
			end if
			
			if ( ind == 2 ) then
					f = ( x(2) - x(1) ** 2 ) ** 2 + ( 1.0d0 - x(1) ) ** 2
					return
			end if	
			
	end if	
	
! ----------------------------------------------------------------------

 ! AP4: Exemple 4 of "A modified Quasi-Newton method for vector optimization problem"

	if ( problem == 7 ) then 
			
			if ( ind == 1 ) then
					f = 1.0d0/9.0d0 * ( ( x(1) - 1.0d0 ) ** 4 + 2.0d0 * ( x(2) - 2.0d0 ) ** 4 &
					+ 3.0d0 * ( x(3) - 3.0d0 ) ** 4 )
					return
			end if
			
			if ( ind == 2 ) then
					f = exp( ( x(1) + x(2) + x(3) ) / 3.0d0 ) + x(1) ** 2 + x(2) ** 2 + x(3) ** 2
					return
			end if	
			
			if ( ind == 3 ) then
					f = 1.0d0/1.2d1 * ( 3.0d0 * exp( -x(1) ) + 4.0d0 * exp( - x(2) ) + 3.0d0 * exp( -x(3) ) )
					return
			end if	
			
	end if	
	
! ----------------------------------------------------------------------

	if ( problem == 8 ) then 
	
			! L1  "A SYNTHETIC APPROACH TO MULTIOBJECTIVE OPTIMIZATION"

			
			if ( ind == 1 ) then
					f = - ( -1.05d0 * x(1) ** 2 - 0.98d0 * x(2) ** 2 )
					return
			end if
			
			if ( ind == 2 ) then
					f = - ( -0.99d0 * ( x(1) - 3.0d0 ) ** 2 - 1.03d0 * ( x(2) - 2.5d0 ) ** 2 )
					return
			end if	
			
	end if		
	
! ----------------------------------------------------------------------

	if ( problem == 9 ) then 
	
			! L3  "A SYNTHETIC APPROACH TO MULTIOBJECTIVE OPTIMIZATION"

			
			if ( ind == 1 ) then
					f = - ( - x(1) ** 2 - x(2) ** 2 )
					return
			end if
			
			if ( ind == 2 ) then
					f = - ( - ( x(1) - 6.0d0 ) ** 2 + ( x(2) + 0.3d0 ) ** 2 )
					return
			end if	
			
	end if		

! ----------------------------------------------------------------------

	if ( problem == 10 ) then 
	
			! L4  "A SYNTHETIC APPROACH TO MULTIOBJECTIVE OPTIMIZATION"

			
			if ( ind == 1 ) then
					f = - x(1) ** 2 - x(2) ** 2 - 4.0d0 * ( exp( - ( x(1) + 2.0d0 ) ** 2 - x(2) ** 2 ) + &
					    exp( - ( x(1) - 2.0d0 ) ** 2 - x(2) ** 2 ) )
					f = - f
					return
			end if
			
			if ( ind == 2 ) then
					f = - ( x(1) - 6.0d0 ) ** 2 - ( x(2) + 0.5d0 ) ** 2
					f = - f
					return
			end if	
			
	end if		
	

! ----------------------------------------------------------------------

	if ( problem == 11 ) then 
	
			! FF1 
			! C. M. Fonseca and P. J. Fleming, “An overview of evolutionary algorithms in multiobjective optimization

			
			if ( ind == 1 ) then
					f = 1.0d0 - exp( - ( x(1) - 1.0d0 ) ** 2 - ( x(2) + 1.0d0 ) ** 2 )
					return
			end if
			
			if ( ind == 2 ) then
					f = 1.0d0 - exp( - ( x(1) + 1.0d0 ) ** 2 - ( x(2) - 1.0d0 ) ** 2 )
					return
			end if	
			
	end if	
	
! ----------------------------------------------------------------------

	! FDS
	! NEWTON’S METHOD FOR MULTIOBJECTIVE OPTIMIZATION
			
	if ( problem == 12 ) then 

			
			if ( ind == 1 ) then
					f = 0.0d0
					do i = 1,n
						f = f + i * ( x(i) - i ) ** 4
					end do	
					f = f / ( n ** 2 )
					return
			end if
			
			if ( ind == 2 ) then
					f = exp( sum(x)/n ) + norm2(x) ** 2
					return
			end if	
			
			if ( ind == 3 ) then
					f = 0.0d0
					do i = 1,n
						f = f + i * ( n - i + 1.0d0 ) * exp( - x(i) )
					end do	
					f = f / ( n * ( n + 1.0d0 ) ) 
					return
			end if	
			
	end if	
	
! ----------------------------------------------------------------------

	!  MMR1 (modified) 			
	!  Box-constrained multi-objective optimization: A gradient-like method without ‘‘a priori’’ scalarization	

	if ( problem == 13 ) then 
			
			if ( ind == 1 ) then
					f = 1.0d0 + x(1) ** 2	
					return
			end if
			
			if ( ind == 2 ) then
					f = 2.0d0 - 0.8d0 * exp( - ( ( x(2) - 0.6d0 ) / 0.4d0 ) ** 2 ) -&
					 exp( - ( ( x(2) - 0.2d0 ) / 0.04d0 ) ** 2 )
					f = f / ( 1.0d0 + x(1) ** 2 )
					return	
			end if	
			
	end if			
	
! ----------------------------------------------------------------------

	!  MMR5 			
	!  Box-constrained multi-objective optimization: A gradient-like method without ‘‘a priori’’ scalarization	

	if ( problem == 14 ) then 
			
			if ( ind == 1 ) then
					f = 0.0d0
					do i = 1,n
						f = f + x(i) ** 2 - 1.0d1 * cos( 2.0d0 * pi * x(i) ) + 1.0d1 
					end do
					f = ( f / n ) ** ( 0.25d0 )
					return
			end if
			
			if ( ind == 2 ) then
					f = 0.0d0
					do i = 1,n
						f = f + ( x(i) - 1.5d0 ) ** 2 &
						- 1.0d1 * cos( 2.0d0 * pi * ( x(i) - 1.5d0 ) ) + 1.0d1 
					end do
					f = ( f / n ) ** ( 0.25d0 )
					return	
			end if	
			
	end if
	
! ----------------------------------------------------------------------

	!  MOP 1	

	if ( problem == 15 ) then 
			
			if ( ind == 1 ) then
					f = x(1) ** 2
					return
			end if
			
			if ( ind == 2 ) then
					f = ( x(1) - 2.0d0 ) ** 2
					return	
			end if	
			
	end if				

! ----------------------------------------------------------------------

	!  MOP 2	

	if ( problem == 16 ) then 
			
			if ( ind == 1 ) then
					f = 0.0d0
					do i = 1,n
						f = f + ( x(i) - 1.0d0 / ( sqrt( real(n) ) ) ) ** 2
					end do
					
					f = 1.0d0 - exp ( - f )	
					return
			end if
			
			if ( ind == 2 ) then
					f = 0.0d0
					do i = 1,n
						f = f + ( x(i) + 1.0d0 / ( sqrt( real (n) ) ) ) ** 2
					end do
					
					f = 1.0d0 - exp ( - f )	
					return	
			end if	
			
	end if	
	
! ----------------------------------------------------------------------

	!  MOP 3	

	if ( problem == 17 ) then 
			
			if ( ind == 1 ) then
					A1 = 0.5d0 * sin(1.0d0) - 2.0d0 * cos(1.0d0) + sin(2.0d0) - 1.5d0 * cos(2.0d0) 
					A2 = 1.5d0 * sin(1.0d0) - cos(1.0d0) + 2.0d0 * sin(2.0d0) - 0.5d0 * cos(2.0d0)
					B1 = 0.5d0 * sin(x(1)) - 2.0d0 * cos(x(1)) + sin(x(2)) - 1.5d0 * cos(x(2)) 
					B2 = 1.5d0 * sin(x(1)) - cos(x(1)) + 2.0d0 * sin(x(2)) - 0.5d0 * cos(x(2))	
					f = - 1.0d0 - ( A1 - B1 ) ** 2 - ( A2 - B2 ) ** 2
					f = - f
					return
			end if
			
			if ( ind == 2 ) then
					f = - ( x(1) + 3.0d0 ) ** 2 - ( x(2) + 1.0d0 ) ** 2
					f = - f	
					return	
			end if	
			
	end if			
	
! ----------------------------------------------------------------------

	!  MOP 5	

	if ( problem == 18 ) then 
			
			if ( ind == 1 ) then
					f = 0.5d0 * ( x(1) ** 2 + x(2) ** 2 ) + sin( x(1) ** 2 + x(2) ** 2 )
					return
			end if
			
			if ( ind == 2 ) then
					f = ( 3.0d0 * x(1) - 2.0d0 * x(2) + 4.0d0 ) ** 2 / 8.0d0 + &
						( x(1) - x(2) + 1.0d0 ) ** 2 / 2.7d1 + 1.5d1
					return	
			end if
			
			if ( ind == 3 ) then
					f = 1.0d0 / ( x(1) ** 2 + x(2) ** 2 + 1.0d0 ) - 1.1d0 * exp( - x(1) ** 2 - x(2) ** 2 )
					return
			end if	
			
			
	end if	

! ----------------------------------------------------------------------

	!  MOP 7

	if ( problem == 19 ) then 
			
			if ( ind == 1 ) then
					f =  ( x(1) - 2.0d0 ) ** 2 / 2.0d0 &
					+ ( x(2) + 1.0d0 ) ** 2 / 1.3d1 + 3.0d0
					return
			end if
			
			if ( ind == 2 ) then
					f =  ( x(1) + x(2) - 3.0d0 ) ** 2 / 3.6d1 &
					+ ( - x(1) + x(2) + 2.0d0 ) ** 2 / 8.0d0 - 1.7d1
					return	
			end if
			
			if ( ind == 3 ) then
					f =  ( x(1) + 2.0d0 * x(2) - 1.0d0 ) ** 2 / 1.75d2 &
					+ ( - x(1) + 2.0d0 * x(2) ) ** 2 / 1.7d1 - 1.3d1
					return
			end if	
			
			
	end if		
	
! ----------------------------------------------------------------------

	!  DGO1

	if ( problem == 20 ) then 
			
			if ( ind == 1 ) then
					f =  sin( x(1) )
					return
			end if
			
			if ( ind == 2 ) then
					f =  sin( x(1) + 0.7d0 )
					return	
			end if
			
			
	end if		
	
! ----------------------------------------------------------------------

	!  Far1

	if ( problem == 21 ) then 
			
			if ( ind == 1 ) then
					f =  - 2.0d0 * exp( 1.5d1 * ( - ( x(1) - 0.1d0 ) ** 2 - x(2) ** 2 ) ) &
					     - exp( 2.0d1 * ( - ( x(1) - 0.6d0 ) ** 2 - ( x(2) - 0.6d0 ) ** 2 ) ) &
					     + exp( 2.0d1 * ( - ( x(1) + 0.6d0 ) ** 2 - ( x(2) - 0.6d0 ) ** 2 ) ) &
					     + exp( 2.0d1 * ( - ( x(1) - 0.6d0 ) ** 2 - ( x(2) + 0.6d0 ) ** 2 ) ) &
					     + exp( 2.0d1 * ( - ( x(1) + 0.6d0 ) ** 2 - ( x(2) + 0.6d0 ) ** 2 ) ) 
					return
			end if
			
			if ( ind == 2 ) then
					f =  2.0d0 * exp( 2.0d1 * ( - x(1) ** 2 - x(2) ** 2 ) ) &
					     + exp( 2.0d1 * ( - ( x(1) - 0.4d0 ) ** 2 - ( x(2) - 0.6d0 ) ** 2 ) ) &
					     - exp( 2.0d1 * ( - ( x(1) + 0.5d0 ) ** 2 - ( x(2) - 0.7d0 ) ** 2 ) ) &
					     - exp( 2.0d1 * ( - ( x(1) - 0.5d0 ) ** 2 - ( x(2) + 0.7d0 ) ** 2 ) ) &
					     + exp( 2.0d1 * ( - ( x(1) + 0.4d0 ) ** 2 - ( x(2) + 0.8d0 ) ** 2 ) ) 
					return	
			end if
			
	end if	
			

! ----------------------------------------------------------------------

	!  MLF1

	if ( problem == 22 ) then 
			
			if ( ind == 1 ) then
					f = ( 1.0d0 + x(1) / 2.0d1 ) * sin( x(1) )
					return
			end if
			
			if ( ind == 2 ) then
					f = ( 1.0d0 + x(1) / 2.0d1 ) * cos( x(1) )
					return	
			end if
			
	end if	
	
! ----------------------------------------------------------------------

	!  MLF2

	if ( problem == 23 ) then 
			
			if ( ind == 1 ) then
					f = 5.0d0 - ( ( x(1) ** 2 + x(2) - 1.1d1 ) ** 2 &
					    + ( x(1) + x(2) ** 2 - 7.0d0 ) ** 2 ) / 2.0d2
					f = - f
					return
			end if
			
			if ( ind == 2 ) then
					f = 5.0d0 - ( ( 4.0d0 * x(1) ** 2 + 2.0d0 * x(2) - 1.1d1 ) ** 2 &
					    + ( 2.0d0 * x(1) + 4.0d0 *  x(2) ** 2 - 7.0d0 ) ** 2 ) / 2.0d2
					f = - f
					return	
			end if
			
	end if		
	
! ----------------------------------------------------------------------

	!  SP1

	if ( problem == 24 ) then 
			
			if ( ind == 1 ) then
					f = ( x(1) - 1.0d0 ) ** 2 + ( x(1) - x(2) ) ** 2
					return
			end if
			
			if ( ind == 2 ) then
					f = ( x(2) - 3.0d0 ) ** 2 + ( x(1) - x(2) ) ** 2
					return	
			end if
			
	end if				
		
! ----------------------------------------------------------------------

	!  SSFYY2

	if ( problem == 25 ) then 
			
			if ( ind == 1 ) then
					f = 1.0d1 + x(1) ** 2 - 1.0d1 * cos( x(1) * pi / 2.0d0 )
					return
			end if
			
			if ( ind == 2 ) then
					f = ( x(1) - 4.0d0 ) ** 2
					return	
			end if
			
	end if			
				
! ----------------------------------------------------------------------

	!  SK1

	if ( problem == 26 ) then 
			
			if ( ind == 1 ) then
					f = - x(1) ** 4 - 3.0d0 * x(1) ** 3 + 1.0d1 * x(1) ** 2 + 1.0d1 * x(1) + 1.0d1
					f = - f
					return
			end if
			
			if ( ind == 2 ) then
					f = - 0.5d0 * x(1) ** 4 + 2.0d0 * x(1) ** 3 + 1.0d1 * x(1) ** 2 - 1.0d1 * x(1) + 5.0d0
					f = - f
					return	
			end if
			
	end if	
	
! ----------------------------------------------------------------------

	!  SK2

	if ( problem == 27 ) then 
			
			if ( ind == 1 ) then
					f = - ( x(1) - 2.0d0 ) ** 2 - ( x(2) + 3.0d0 ) ** 2 &
					- ( x(3) - 5.0d0 ) ** 2 - ( x(4) - 4.0d0 ) ** 2 + 5.0d0
					f = - f
					return
			end if
			
			if ( ind == 2 ) then
					f = ( sin( x(1) ) + sin( x(2) ) + sin( x(3) ) + sin( x(4) ) ) / &
					( 1.0d0 + ( x(1) ** 2 + x(2) ** 2 + x(3) ** 2 + x(4) ** 2 ) / 1.0d2 )
					f = - f
					return	
			end if
			
	end if	
	
! ----------------------------------------------------------------------

	!  VU1

	if ( problem == 28 ) then 
			
			if ( ind == 1 ) then
					f = 1.0d0 / ( x(1) ** 2 + x(2) ** 2 + 1.0d0 )
					return
			end if
			
			if ( ind == 2 ) then
					f = x(1) ** 2 + 3.0d0 * x(2) ** 2 + 1.0d0
					return	
			end if
			
	end if		
	
! ----------------------------------------------------------------------

	!  Hil1

	if ( problem == 29 ) then 
			a = 2.0d0 * pi / 3.6d2 * ( 4.5d1 + 4.0d1 * sin( 2.0d0 * pi * x(1) ) &
			    + 2.5d1 * sin( 2.0d0 * pi * x(2) ) )
			b = 1.0d0 + 0.5d0 * cos( 2.0d0 * pi * x(1) )
			
			if ( ind == 1 ) then
					f = cos( a ) * b
					return
			end if
			
			if ( ind == 2 ) then
					f = sin( a ) * b
					return	
			end if
			
	end if			
	
! ----------------------------------------------------------------------

	!  DD1

	if ( problem == 30 ) then 
			
			if ( ind == 1 ) then
					f = x(1) ** 2 + x(2) ** 2 + x(3) ** 2 + x(4) ** 2 + x(5) ** 2 
					return
			end if
			
			if ( ind == 2 ) then
					f = 3.0d0 * x(1) + 2.0d0 * x(2) - x(3) / 3.0d0 + 1.0d-2 * ( x(4) - x(5) ) ** 3
					return	
			end if
			
	end if				
	
! ----------------------------------------------------------------------

	! 	KW2
	
	if ( problem == 31 ) then 
			
			if ( ind == 1 ) then
					f = - 3.0d0 * ( 1.0d0 - x(1) )**2 * exp( -x(1)**2 - ( x(2) + 1.0d0 ) ** 2 ) &
					    + 1.0d1 * ( x(1) / 5.0d0 - x(1)**3 - x(2)**5 ) * exp( - x(1)**2 - x(2)**2 ) &
					    + 3.0d0 * exp( -( x(1) + 2.0d0 )**2 - x(2)**2 ) - 0.5d0 * ( 2.0d0 * x(1) + x(2) )
					return
			end if
			
			if ( ind == 2 ) then
					f = - 3.0d0 * ( 1.0d0 + x(2) )**2 * exp( -x(2)**2 - ( 1.0d0 - x(1) ) ** 2 ) &
					    + 1.0d1 * ( - x(2) / 5.0d0 + x(2)**3 + x(1)**5 ) * exp( - x(1)**2 - x(2)**2 ) &
					    + 3.0d0 * exp( -( 2.0d0 - x(2) )**2 - x(1)**2 ) 
					return	
			end if
			
	end if				
	
! ----------------------------------------------------------------------

	! 	Toi4
	
	if ( problem == 32 ) then 
			
			if ( ind == 1 ) then
					f = x(1) ** 2 + x(2) ** 2 + 1.0d0
					return
			end if
			
			if ( ind == 2 ) then
					f = 0.5d0 * ( ( x(1) - x(2) ) ** 2 + ( x(3) - x(4) ) ** 2  ) + 1.0d0
					return	
			end if
			
	end if			
	
! ----------------------------------------------------------------------

	! 	Toi8
	
	if ( problem == 33 ) then 
			
			if ( ind == 1 ) then
					f = ( 2.0d0 * x(1) - 1.0d0 ) ** 2 
					return
			end if
			
			if ( ind /= 1 ) then
					f = ind * ( 2.0d0 * x(ind-1) - x(ind) ) ** 2
					return	
			end if
			
	end if		
	
! ----------------------------------------------------------------------

	! 	Toi9
	
	if ( problem == 34 ) then 
			
			if ( ind == 1 ) then
					f = ( 2.0d0 * x(1) - 1.0d0 ) ** 2 + x(2) ** 2
					return
			end if
			
			if ( ind > 1 .and. ind < n ) then
					f = ind * ( 2.0d0 * x(ind-1) - x(ind) ) ** 2  &
					   - ( ind - 1.0d0 ) * x(ind-1)**2 + ind * x(ind) ** 2
					return	
			end if
			
			if ( ind == n ) then
					f = n * ( 2.0d0 * x(n-1) - x(n) ) ** 2 - ( n - 1.0d0 ) * x(n-1)**2
					return
			end if
			
	end if		
	
! ----------------------------------------------------------------------

	! 	Toi10 (Rosenbrock)
	
	if ( problem == 35 ) then 
			
			f = 1.0d2 * ( x(ind+1) - x(ind) ** 2 ) ** 2 + ( x(ind+1) - 1.0d0 ) ** 2
			return
			
	end if			
	
! ----------------------------------------------------------------------

	! 	MGH9 
	
	if ( problem == 36 ) then 
	
			if ( ind == 1 .or. ind == 15 ) then
					y = 9.0d-4
			elseif ( ind == 2 .or. ind == 14 ) then 		
					y = 4.4d-3
			elseif ( ind == 3 .or. ind == 13 ) then 		
					y = 1.75d-2
			elseif ( ind == 4 .or. ind == 12 ) then 		
					y = 5.4d-2		
			elseif ( ind == 5 .or. ind == 11 ) then 		
					y = 1.295d-1		
			elseif ( ind == 6 .or. ind == 10 ) then 		
					y = 2.42d-1
			elseif ( ind == 7 .or. ind == 9  ) then 		
					y = 3.521d-1
			elseif ( ind == 8 ) then 		
					y = 3.989d-1
			end if
			
			t = ( 8.0d0 - ind ) / 2.0d0
					
			f = x(1) * exp( - x(2) * ( t - x(3) ) ** 2 / 2.0d0 ) - y			
			
			return
			
	end if	
	
! ----------------------------------------------------------------------

	! 	MGH16 
	
	if ( problem == 37 ) then 
			
			t = ind / 5.0d0
					
			f = ( x(1) + t * x(2) - exp(t) ) ** 2 + ( x(3) + x(4) * sin(t) - cos(t) ) ** 2
			
			return
			
	end if				
	
! ----------------------------------------------------------------------

	! 	MGH26 
	
	if ( problem == 38 ) then 
								
			t = 0.0d0
			do i = 1,n
					t = t + cos(x(i))
			end do
			
			f = ( n - t + ind * ( 1.0d0 - cos(x(ind)) ) - sin(x(ind)) ) ** 2
			
			return
			
	end if				
	
! ----------------------------------------------------------------------

	! 	MGH33
	
	if ( problem == 39 ) then 
								
			faux = 0.0d0					
			do i = 1,n
					faux = faux + i * x(i)
			end do
			
			f = ( ind * faux - 1.0d0 ) ** 2			
			
			return
			
	end if				
	
! ----------------------------------------------------------------------

	! 	DTLZ2.2
	
	if ( problem == 40 ) then 
								
			faux = 0.0d0					
			do i = 3,n
					faux = faux + ( x(i) ** 2 - x(1) ) ** 2
			end do
			
			if ( ind == 1 ) then
					f = cos( pi / 2.0d0 * x(1) ) * cos( pi / 2.0d0 * x(2) ) * ( 1.0d0 + faux )
					return
			end if
			
			if ( ind == 2 ) then
					f = cos( pi / 2.0d0 * x(1) ) * sin( pi / 2.0d0 * x(2) ) * ( 1.0d0 + faux )
					return
			end if
			
			if ( ind == 3 ) then
					f = sin( pi / 2.0d0 * x(1) ) * ( 1.0d0 + faux )
					return
			end if
			
	end if			
					
! ----------------------------------------------------------------------

	! 	PNR
	
	if ( problem == 41 ) then 
											
			if ( ind == 1 ) then
					f = x(1) ** 4 + x(2) ** 4 - x(1) ** 2 + x(2) ** 2 - 1.0d1 * x(1) * x(2) + 2.0d1
					return
			end if
			
			if ( ind == 2 ) then
					f = x(1) ** 2 + x(2) ** 2
					return
			end if
			
	end if			
	
! ----------------------------------------------------------------------

	! 	LTDZ
	!	Combining convergence and diversity in evolutionary multiobjective optimization
	
	if ( problem == 42 ) then 
											
			if ( ind == 1 ) then
					f = 3.0d0 - ( 1.0d0 + x(3) ) * cos( x(1) * pi / 2.0d0 ) * cos( x(2) * pi / 2.0d0 )
					f = - f
					return
			end if
			
			if ( ind == 2 ) then
					f = 3.0d0 - ( 1.0d0 + x(3) ) * cos( x(1) * pi / 2.0d0 ) * sin( x(2) * pi / 2.0d0 )
					f = - f
					return
			end if
			
			if ( ind == 3 ) then
					f = 3.0d0 - ( 1.0d0 + x(3) ) * cos( x(1) * pi / 2.0d0 ) * sin( x(1) * pi / 2.0d0 )
					f = - f
					return
			end if
			
	end if						
	
! ----------------------------------------------------------------------

	! 	SLCDT2
	!	Convergence of stochastic search algorithms to finite size pareto set approximations 
	
	if ( problem == 43 ) then 
											
			if ( ind == 1 ) then
					f = ( x(1) - 1.0d0 ) ** 4
					do i = 2,n
							f = f + ( x(i) - 1.0d0 ) ** 2
					end do
					return
			end if
			
			if ( ind == 2 ) then
					f = ( x(2) + 1.0d0 ) ** 4
					do i = 1,n
							if ( i /= 2 ) f = f + ( x(i) + 1.0d0 ) ** 2  
					end do
					return
			end if
			
			if ( ind == 3 ) then
					f = ( x(3) - 1.0d0 ) ** 4
					do i = 1,n
							if ( i /= 3 ) f = f + ( x(i) - ( - 1.0d0 ) ** (i+1) ) ** 2  
					end do
					return
			end if
			
	end if						
						
					
	end subroutine evalf

	!***********************************************************************
	!***********************************************************************

	subroutine evalg(n,x,g,ind)
	
	implicit none
	
	! SCALAR ARGUMENTS
	integer, intent(in) :: n,ind
	
	! ARRAY ARGUMENTS
	real(kind=8), intent(in)  :: x(n)
	real(kind=8), intent(out) :: g(n)
	
	! LOCAL SCALARS
	
	integer :: i
	real, parameter :: pi = 3.1415927
	real(kind=8) :: faux,gaux1,gaux2,A1,A2,B1,B2,a,b,t
	
	! JOS1 
	! Dynamic Weighted Aggregation for Evolutionary Multi-Objetive Optimization: Why Does It Work and How?
	
	if ( problem == 1 ) then
	
			if ( ind == 1 ) then					
					do i = 1,n
							g(i) = 2.0d0 * x(i) / n
					end do
					return
			end if
			
			if ( ind == 2 ) then
					do i = 1,n
							g(i) = 2.0d0 * ( x(i) - 2.0d0 ) / n
					end do	
					return				
			end if
			
	end if

! ----------------------------------------------------------------------
	
	! SLC2
	! The Directed Search Method for Unconstrained Multi-Objective Optimization Problems
	
	if ( problem == 2 ) then 

			if ( ind == 1 ) then
					g(1) = 4.0d0 * ( x(1) - 1.0d0 ) ** 3
					
					do i = 2,n
							g(i) = 2.0d0 * ( x(i) - 1.0d0 )
					end do
					return
			end if
			
			if ( ind == 2 ) then
					g(2) = 4.0d0 * ( x(2) + 1.0d0 ) ** 3
					
					do i = 1,n
							if ( i /= 2 ) g(i) = 2.0d0 * ( x(i) + 1.0d0 )
					end do				
					return	
			end if
			
	end if

! ----------------------------------------------------------------------

	! SLCDT1

	if ( problem == 3 ) then 
	
			if ( ind == 1 ) then			
					g(1) = 0.5d0 * ( ( x(1) + x(2) )/sqrt( 1.0d0 + ( x(1) + x(2) ) ** 2 ) + &
								( x(1) - x(2) )/sqrt( 1.0d0 + ( x(1) - x(2) ) ** 2 ) + 1.0d0 )  &
								 - 2.0d0 * 0.85d0 *  ( x(1) + x(2) ) * exp( - ( x(1) + x(2) ) ** 2 )
					g(2) = 0.5d0 * ( ( x(1) + x(2) )/sqrt( 1.0d0 + ( x(1) + x(2) ) ** 2 ) - &
								( x(1) - x(2) )/sqrt( 1.0d0 + ( x(1) - x(2) ) ** 2 ) - 1.0d0 )  &
								 - 2.0d0 * 0.85d0 * ( x(1) + x(2) ) * exp( - ( x(1) + x(2) ) ** 2 )				 
					return			 
			end if
			
			if ( ind == 2 ) then
					g(1) = 0.5d0 * ( ( x(1) + x(2) )/sqrt( 1.0d0 + ( x(1) + x(2) ) ** 2 ) + &
								( x(1) - x(2) )/sqrt( 1.0d0 + ( x(1) - x(2) ) ** 2 ) - 1.0d0 )  &
								 - 2.0d0 * 0.85d0 * exp( - ( x(1) + x(2) ) ** 2 ) * ( x(1) + x(2) )
					g(2) = 0.5d0 * ( ( x(1) + x(2) )/sqrt( 1.0d0 + ( x(1) + x(2) ) ** 2 ) - &
								( x(1) - x(2) )/sqrt( 1.0d0 + ( x(1) - x(2) ) ** 2 ) + 1.0d0 )  &
								 - 2.0d0 * 0.85d0 * ( x(1) + x(2) ) * exp( - ( x(1) + x(2) ) ** 2 )
					return			 
			end if 	
			
	end if


	
! ----------------------------------------------------------------------

! AP1: Exemple 1 of "A modified Quasi-Newton method for vector optimization problem"

	if ( problem == 4 ) then 
			
			if ( ind == 1 ) then
					g(1) = ( x(1) - 1.0d0 ) ** 3 
					g(2) = 2.0d0 * ( x(2) - 2.0d0 ) ** 3
					return
			end if
			
			if ( ind == 2 ) then
					g(1) = 0.5d0 * exp( ( x(1) + x(2) ) / 2.0d0 ) + 2.0d0 * x(1)
					g(2) = 0.5d0 * exp( ( x(1) + x(2) ) / 2.0d0 ) + 2.0d0 * x(2)
					return
			end if	
			
			if ( ind == 3 ) then
					g(1) = - 1.0d0/6.0d0 * exp( - x(1) ) 
					g(2) = - 1.0d0/3.0d0 * exp( - x(2) ) 
					return
			end if	
			
	end if	
	
! ----------------------------------------------------------------------

! AP2: Exemple 2 of "A modified Quasi-Newton method for vector optimization problem"

	if ( problem == 5 ) then 
			
			if ( ind == 1 ) then
					g(1) = 2.0d0 * x(1)
					return
			end if
			
			if ( ind == 2 ) then
					g(1) = 2.0d0 * ( x(1) - 1.0d0 )
					return
			end if	
			
	end if		
	
	
! ----------------------------------------------------------------------

! AP3: Exemple 3 of "A modified Quasi-Newton method for vector optimization problem"

	if ( problem == 6 ) then 
			
			if ( ind == 1 ) then
					g(1) = ( x(1) - 1.0d0 ) ** 3 
					g(2) = 2.0d0 * ( x(2) - 2.0d0 ) ** 3
					return
			end if
			
			if ( ind == 2 ) then
					g(1) = - 4.0d0 * x(1) * ( x(2) - x(1) ** 2 ) - 2.0d0 * ( 1.0d0 - x(1) )
					g(2) = 2.0d0 * ( x(2) - x(1) ** 2 )
					return
			end if	
			
	end if	
	
! ----------------------------------------------------------------------

! AP4: Exemple 4 of "A modified Quasi-Newton method for vector optimization problem"

	if ( problem == 7 ) then 
			
			if ( ind == 1 ) then
					g(1) = 4.0d0/9.0d0 * ( x(1) - 1.0d0 ) ** 3 
					g(2) = 8.0d0/9.0d0 * ( x(2) - 2.0d0 ) ** 3 
					g(3) = 1.2d1/9.0d0 * ( x(3) - 3.0d0 ) ** 3 
					return
			end if
			
			if ( ind == 2 ) then
					g(1) = 1.0d0/3.0d0 * exp( ( x(1) + x(2) + x(3) ) / 3.0d0 ) + 2.0d0 * x(1) 
					g(2) = 1.0d0/3.0d0 * exp( ( x(1) + x(2) + x(3) ) / 3.0d0 ) + 2.0d0 * x(2) 
					g(3) = 1.0d0/3.0d0 * exp( ( x(1) + x(2) + x(3) ) / 3.0d0 ) + 2.0d0 * x(3) 
					return
			end if	
			
			if ( ind == 3 ) then
					g(1) = - 1.0d0/4.0d0 * exp( -x(1) ) 
					g(2) = - 1.0d0/3.0d0 * exp( -x(2) ) 
					g(3) = - 1.0d0/4.0d0 * exp( -x(3) ) 
					return
			end if	
			
	end if	
	
! ----------------------------------------------------------------------
	
	! L1  "A SYNTHETIC APPROACH TO MULTIOBJECTIVE OPTIMIZATION"
	
	if ( problem == 8 ) then 
			
			if ( ind == 1 ) then
					g(1) = 2.1d0 * x(1)
					g(2) = 2.0d0 * 0.98d0 * x(2)
					return
			end if
			
			if ( ind == 2 ) then
					g(1) = 2.0d0 * 0.99d0 * ( x(1) - 3.0d0 )
					g(2) = 2.0d0 * 1.03d0 * ( x(2) - 2.5d0 )
					return
			end if	
			
	end if	
	
! ----------------------------------------------------------------------
	
	! L3  "A SYNTHETIC APPROACH TO MULTIOBJECTIVE OPTIMIZATION"
	
	if ( problem == 9 ) then 
			
			if ( ind == 1 ) then
					g(1) =  2.0d0 * x(1)
					g(2) =  2.0d0 * x(2)
					return
			end if
			
			if ( ind == 2 ) then
					g(1) =  2.0d0 * ( x(1) - 6.0d0 )
					g(2) = - 2.0d0 * ( x(2) + 0.3d0 )
					return
			end if	
			
	end if	
	
! ----------------------------------------------------------------------
	
	! L4  "A SYNTHETIC APPROACH TO MULTIOBJECTIVE OPTIMIZATION"
	
	if ( problem == 10 ) then 
			
			if ( ind == 1 ) then
					g(1) =  2.0d0 * x(1) - 8.0d0 * ( ( x(1) + 2.0d0 ) * exp( - ( x(1) + 2.0d0 ) ** 2 - x(2) ** 2 ) &
					+ ( x(1) - 2.0d0 ) * exp( - ( x(1) - 2.0d0 ) ** 2 - x(2) ** 2 ) )
					g(2) =  2.0d0 * x(2) - 8.0d0 * ( x(2) * exp( - ( x(1) + 2.0d0 ) ** 2 - x(2) ** 2 ) &
					+ x(2) * exp( - ( x(1) - 2.0d0 ) ** 2 - x(2) ** 2 ) )
					return
			end if
			
			if ( ind == 2 ) then
					g(1) =  2.0d0 * ( x(1) - 6.0d0 )
					g(2) =  2.0d0 * ( x(2) + 0.5d0 )
					return
			end if	
			
	end if			
	
! ----------------------------------------------------------------------

	if ( problem == 11 ) then 
	
			! FF1 
			! C. M. Fonseca and P. J. Fleming, “An overview of evolutionary algorithms in multiobjective optimization
			
			if ( ind == 1 ) then
					g(1) = 2.0d0 * ( x(1) - 1.0d0 ) * exp( - ( x(1) - 1.0d0 ) ** 2 - ( x(2) + 1.0d0 ) ** 2 )
					g(2) = 2.0d0 * ( x(2) + 1.0d0 ) * exp( - ( x(1) - 1.0d0 ) ** 2 - ( x(2) + 1.0d0 ) ** 2 )
					return
			end if
			
			if ( ind == 2 ) then
					g(1) = 2.0d0 * ( x(1) + 1.0d0 ) * exp( - ( x(1) + 1.0d0 ) ** 2 - ( x(2) - 1.0d0 ) ** 2 )
					g(2) = 2.0d0 * ( x(2) - 1.0d0 ) * exp( - ( x(1) + 1.0d0 ) ** 2 - ( x(2) - 1.0d0 ) ** 2 )
					return
			end if	
			
	end if		
	
! ----------------------------------------------------------------------

	! FDS
	! NEWTON’S METHOD FOR MULTIOBJECTIVE OPTIMIZATION

	if ( problem == 12 ) then 

			
			if ( ind == 1 ) then
					do i = 1,n
						g(i) = 4.0d0 * i * ( x(i) - i ) ** 3 / n ** 2
					end do
					return
			end if
			
			if ( ind == 2 ) then
					do i = 1,n
						g(i) = exp( sum(x)/n )/n + 2.0d0 * x(i)
					end do
					return
			end if	
			
			if ( ind == 3 ) then
				do i = 1,n
					g(i) = - i * ( n - i + 1.0d0 ) * exp( - x(i) ) / ( n * ( n + 1.0d0 ) ) 
				end do
				return
			end if	
			
	end if			
	
! ----------------------------------------------------------------------

	!  MMR1 (modified) 			
	!  Box-constrained multi-objective optimization: A gradient-like method without ‘‘a priori’’ scalarization	

	if ( problem == 13 ) then 
			
			if ( ind == 1 ) then
					g(1) = 2.0d0 * x(1) 
					g(2) = 0.0d0	
					return
			end if
			
			if ( ind == 2 ) then
					g(1) = 2.0d0 - 0.8d0 * exp( - ( ( x(2) - 0.6d0 ) / 0.4d0 ) ** 2 ) &
					       - exp( - ( ( x(2) - 0.2d0 ) / 0.04d0 ) ** 2 )
					g(1) = - 2.0d0 * x(1) * g(1) / ( 1.0d0 + x(1) ** 2 ) ** 2
					
					g(2) = 1.0d1 * exp( - ( ( x(2) - 0.6d0 ) / 0.4d0 ) ** 2 ) * ( x(2) - 0.6d0 ) &
					   + 1.25d3 * exp( - ( ( x(2) - 0.2d0 ) / 0.04d0 ) ** 2 ) * ( x(2) - 0.2d0 ) 
					g(2) = g(2) / ( 1.0d0 + x(1) ** 2 )   
					return	
			end if	
			
	end if		
	
! ----------------------------------------------------------------------

	!  MMR5 			
	!  Box-constrained multi-objective optimization: A gradient-like method without ‘‘a priori’’ scalarization	

	if ( problem == 14 ) then 
			
			if ( ind == 1 ) then
					faux = 0.0d0
					do i = 1,n
						faux = faux + x(i) ** 2 - 1.0d1 * cos( 2.0d0 * pi * x(i) ) + 1.0d1 
					end do
					faux = 0.25d0 * ( faux / n ) ** ( - 0.75d0 ) 
					
					do i = 1,n
						g(i) = faux * ( 2.0d0 * x(i) + 2.0d1 * pi * &
						sin( 2.0d0 * pi * x(i) ) ) / n
					end do	
					return
			end if
			
			if ( ind == 2 ) then
					faux = 0.0d0
					do i = 1,n
						faux = faux + ( x(i) - 1.5d0 ) ** 2 - 1.0d1 * &
						cos( 2.0d0 * pi * ( x(i) - 1.5d0 ) ) + 1.0d1 
					end do
					faux = 0.25d0 * ( faux / n ) ** ( - 0.75d0 )
					
					do i = 1,n
						g(i) = faux * ( 2.0d0 * ( x(i) - 1.5d0 ) + &
						2.0d1 * pi * sin( 2.0d0 * pi * ( x(i) - 1.5d0 ) ) ) / n
					end do
					return
					
					return	
			end if	
			
	end if	
	
! ----------------------------------------------------------------------

	!  MOP 1
	
	if ( problem == 15 ) then 

		if ( ind == 1 ) then
				g(1) = 2.0d0 * x(1)
				return
		end if
		
		if ( ind == 2 ) then
				g(1) = 2.0d0 * ( x(1) - 2.0d0 )	
				return
		end if
		
	end if	
	
! ----------------------------------------------------------------------

	!  MOP 2	

	if ( problem == 16 ) then 
			
			if ( ind == 1 ) then
					faux = 0.0d0
					do i = 1,n
						faux = faux + ( x(i) - 1.0d0 / ( sqrt(real(n)) ) ) ** 2
					end do
					
					do i = 1,n
						g(i) = 2.0d0 * ( x(i) - 1.0d0 / sqrt(real(n)) ) * exp( - faux )
					end do
					return
			end if
			
			if ( ind == 2 ) then
					faux = 0.0d0
					do i = 1,n
						faux = faux + ( x(i) + 1.0d0 / ( sqrt(real(n)) ) ) ** 2
					end do
					
					do i = 1,n
						g(i) = 2.0d0 * ( x(i) + 1.0d0 / sqrt(real(n)) ) * exp( - faux )
					end do
					return	
			end if	
			
	end if			

! ----------------------------------------------------------------------

	!  MOP 3	

	if ( problem == 17 ) then 
			
			if ( ind == 1 ) then
					A1 = 0.5d0 * sin(1.0d0) - 2.0d0 * cos(1.0d0) + sin(2.0d0) - 1.5d0 * cos(2.0d0) 
					A2 = 1.5d0 * sin(1.0d0) - cos(1.0d0) + 2.0d0 * sin(2.0d0) - 0.5d0 * cos(2.0d0)
					B1 = 0.5d0 * sin(x(1)) - 2.0d0 * cos(x(1)) + sin(x(2)) - 1.5d0 * cos(x(2)) 
					B2 = 1.5d0 * sin(x(1)) - cos(x(1)) + 2.0d0 * sin(x(2)) - 0.5d0 * cos(x(2))	
					g(1) = 2.0d0 * ( A1 - B1 ) *  ( - 0.5d0 * cos(x(1)) - 2.0d0 * sin(x(1)) ) +&
						   2.0d0 * ( A2 - B2 ) *  ( - 1.5d0 * cos(x(1)) - sin(x(1)) )
					
					g(2) = 2.0d0 * ( A1 - B1 ) *  ( - cos(x(2)) - 1.5d0 * sin(x(2)) ) +&
						   2.0d0 * ( A2 - B2 ) *  ( - 2.0d0 * cos(x(2)) - 0.5d0 * sin(x(2)) )
					return
			end if
			
			if ( ind == 2 ) then
					g(1) =  2.0d0 * ( x(1) + 3.0d0 )
					g(2) =  2.0d0 * ( x(2) + 1.0d0 )
					return	
			end if	
			
	end if
	
! ----------------------------------------------------------------------

	!  MOP 5	

	if ( problem == 18 ) then 
			
			if ( ind == 1 ) then
					g(1) = x(1) + 2.0d0 * x(1) * cos( x(1) ** 2 + x(2) ** 2 )
					g(2) = x(2) + 2.0d0 * x(2) * cos( x(1) ** 2 + x(2) ** 2 )
					return
			end if
			
			if ( ind == 2 ) then
					g(1) = 3.0d0 * ( 3.0d0 * x(1) - 2.0d0 * x(2) + 4.0d0 ) / 4.0d0 &
						 + 2.0d0 * ( x(1) - x(2) + 1.0d0 ) / 2.7d1
					g(2) = - 2.0d0 * ( 3.0d0 * x(1) - 2.0d0 * x(2) + 4.0d0 ) / 4.0d0 &
						 - 2.0d0 * ( x(1) - x(2) + 1.0d0 ) / 2.7d1	 	 
					return	
			end if
			
			if ( ind == 3 ) then
					g(1) = - 2.0d0 * x(1) / ( x(1) ** 2 + x(2) ** 2 + 1.0d0 ) ** 2 &
					       + 2.2d0 * x(1) * exp( - x(1) ** 2 - x(2) ** 2 )
					g(2) = - 2.0d0 * x(2) / ( x(1) ** 2 + x(2) ** 2 + 1.0d0 ) ** 2 &
					       + 2.2d0 * x(2) * exp( - x(1) ** 2 - x(2) ** 2 )
					return
			end if	
			
			
	end if		

! ----------------------------------------------------------------------

	!  MOP 7

	if ( problem == 19 ) then 
			
			if ( ind == 1 ) then
					g(1) =  x(1) - 2.0d0
					g(2) =  2.0d0 * ( x(2) + 1.0d0 ) / 1.3d1
					return
			end if
			
			if ( ind == 2 ) then
					g(1) =  ( x(1) + x(2) - 3.0d0 ) / 1.8d1 - ( - x(1) + x(2) + 2.0d0 ) / 4.0d0
					g(2) =  ( x(1) + x(2) - 3.0d0 ) / 1.8d1 + ( - x(1) + x(2) + 2.0d0 ) / 4.0d0
					return	
			end if
			
			if ( ind == 3 ) then
					g(1) =  2.0d0 * ( x(1) + 2.0d0 * x(2) - 1.0d0 ) / 1.75d2 &
					- 2.0d0 * ( - x(1) + 2.0d0 * x(2) ) / 1.7d1
					g(2) =  4.0d0 * ( x(1) + 2.0d0 * x(2) - 1.0d0 ) / 1.75d2 &
					+ 4.0d0 * ( - x(1) + 2.0d0 * x(2) ) / 1.7d1
					return
			end if	
			
			
	end if	
	
! ----------------------------------------------------------------------

	!  DGO1

	if ( problem == 20 ) then 
			
			if ( ind == 1 ) then
					g(1) =  cos( x(1) )
					return
			end if
			
			if ( ind == 2 ) then
					g(1) =  cos( x(1) + 0.7d0 )
					return	
			end if
			
			
	end if	
	
! ----------------------------------------------------------------------

	!  Far1

	if ( problem == 21 ) then 
			
			if ( ind == 1 ) then
					g(1) = 6.0d1 * ( x(1) - 0.1d0 ) * exp( 1.5d1 * ( - ( x(1) - 0.1d0 ) ** 2 - x(2) ** 2 ) ) &
							 + 4.0d1 * ( x(1) - 0.6d0 ) * exp( 2.0d1 * ( - ( x(1) - 0.6d0 ) ** 2 - ( x(2) - 0.6d0 ) ** 2 ) ) &
							 - 4.0d1 * ( x(1) + 0.6d0 ) * exp( 2.0d1 * ( - ( x(1) + 0.6d0 ) ** 2 - ( x(2) - 0.6d0 ) ** 2 ) ) &
							 - 4.0d1 * ( x(1) - 0.6d0 ) * exp( 2.0d1 * ( - ( x(1) - 0.6d0 ) ** 2 - ( x(2) + 0.6d0 ) ** 2 ) ) &
							 - 4.0d1 * ( x(1) + 0.6d0 ) * exp( 2.0d1 * ( - ( x(1) + 0.6d0 ) ** 2 - ( x(2) + 0.6d0 ) ** 2 ) ) 
					
					g(2) = 6.0d1 * x(2) * exp( 1.5d1 * ( - ( x(1) - 0.1d0 ) ** 2 - x(2) ** 2 ) ) &
							 + 4.0d1 * ( x(2) - 0.6d0 ) * exp( 2.0d1 * ( - ( x(1) - 0.6d0 ) ** 2 - ( x(2) - 0.6d0 ) ** 2 ) ) &
							 - 4.0d1 * ( x(2) - 0.6d0 ) * exp( 2.0d1 * ( - ( x(1) + 0.6d0 ) ** 2 - ( x(2) - 0.6d0 ) ** 2 ) ) &
							 - 4.0d1 * ( x(2) + 0.6d0 ) * exp( 2.0d1 * ( - ( x(1) - 0.6d0 ) ** 2 - ( x(2) + 0.6d0 ) ** 2 ) ) &
							 - 4.0d1 * ( x(2) + 0.6d0 ) * exp( 2.0d1 * ( - ( x(1) + 0.6d0 ) ** 2 - ( x(2) + 0.6d0 ) ** 2 ) ) 		 
							 
					return
			end if
			
			if ( ind == 2 ) then
					g(1) = - 8.0d1 * x(1) * exp( 2.0d1 * ( - x(1) ** 2 - x(2) ** 2 ) ) &
								 - 4.0d1 * ( x(1) - 0.4d0 ) * exp( 2.0d1 * ( - ( x(1) - 0.4d0 ) ** 2 - ( x(2) - 0.6d0 ) ** 2 ) ) &
								 + 4.0d1 * ( x(1) + 0.5d0 ) * exp( 2.0d1 * ( - ( x(1) + 0.5d0 ) ** 2 - ( x(2) - 0.7d0 ) ** 2 ) ) &
								 + 4.0d1 * ( x(1) - 0.5d0 ) * exp( 2.0d1 * ( - ( x(1) - 0.5d0 ) ** 2 - ( x(2) + 0.7d0 ) ** 2 ) ) &
								 - 4.0d1 * ( x(1) + 0.4d0 ) * exp( 2.0d1 * ( - ( x(1) + 0.4d0 ) ** 2 - ( x(2) + 0.8d0 ) ** 2 ) ) 
								 
					g(2) = - 8.0d1 * x(2) * exp( 2.0d1 * ( - x(1) ** 2 - x(2) ** 2 ) ) &
							   - 4.0d1 * ( x(2) - 0.6d0 ) * exp( 2.0d1 * ( - ( x(1) - 0.4d0 ) ** 2 - ( x(2) - 0.6d0 ) ** 2 ) ) &
							   + 4.0d1 * ( x(2) - 0.7d0 ) * exp( 2.0d1 * ( - ( x(1) + 0.5d0 ) ** 2 - ( x(2) - 0.7d0 ) ** 2 ) ) &
							   + 4.0d1 * ( x(2) + 0.7d0 ) * exp( 2.0d1 * ( - ( x(1) - 0.5d0 ) ** 2 - ( x(2) + 0.7d0 ) ** 2 ) ) &
							   - 4.0d1 * ( x(2) + 0.8d0 ) * exp( 2.0d1 * ( - ( x(1) + 0.4d0 ) ** 2 - ( x(2) + 0.8d0 ) ** 2 ) ) 
								 
					return	
			end if
			
	end if			
	
! ----------------------------------------------------------------------

	!  MLF1

	if ( problem == 22 ) then 
			
			if ( ind == 1 ) then
					g(1) = sin( x(1) ) / 2.0d1 + ( 1.0d0 + x(1) / 2.0d1 ) * cos( x(1) )
					return
			end if
			
			if ( ind == 2 ) then
					g(1) = cos( x(1) ) / 2.0d1 - ( 1.0d0 + x(1) / 2.0d1 ) * sin( x(1) )
					return	
			end if
			
	end if		
	
! ----------------------------------------------------------------------

	!  MLF2

	if ( problem == 23 ) then 
			
			if ( ind == 1 ) then
					g(1) = ( 2.0d0 * x(1) * ( x(1) ** 2 + x(2) - 1.1d1 ) + ( x(1) + x(2) ** 2 - 7.0d0 )  ) / 1.0d2
					g(2) = ( ( x(1) ** 2 + x(2) - 1.1d1 ) + 2.0d0 * x(2) * ( x(1) + x(2) ** 2 - 7.0d0 )  ) / 1.0d2
					return
			end if
			
			if ( ind == 2 ) then
					g(1) = ( 8.0d0 * x(1) * ( 4.0d0 * x(1) ** 2 + 2.0d0 * x(2) - 1.1d1 ) &
					     + 2.0d0 * ( 2.0d0 * x(1) + 4.0d0 *  x(2) ** 2 - 7.0d0 ) ) / 1.0d2
					g(2) = ( 2.0d0 * ( 4.0d0 * x(1) ** 2 + 2.0d0 * x(2) - 1.1d1 ) &
					     + 8.0d0 * x(2) * ( 2.0d0 * x(1) + 4.0d0 *  x(2) ** 2 - 7.0d0 ) ) / 1.0d2
					return	
			end if
			
	end if		
	
! ----------------------------------------------------------------------

	!  SP1

	if ( problem == 24 ) then 
			
			if ( ind == 1 ) then
					g(1) = 2.0d0 * ( x(1) - 1.0d0 ) + 2.0d0 * ( x(1) - x(2) )
					g(2) = - 2.0d0 * ( x(1) - x(2) )
					return
			end if
			
			if ( ind == 2 ) then
					g(1) = 2.0d0 * ( x(1) - x(2) )
					g(2) = 2.0d0 * ( x(2) - 3.0d0 ) - 2.0d0 * ( x(1) - x(2) )
					return	
			end if
			
	end if			
	
! ----------------------------------------------------------------------

	!  SSFYY2

	if ( problem == 25 ) then 
			
			if ( ind == 1 ) then
					g(1) = 2.0d0 * x(1) + 5.0d0 * pi * sin( x(1) * pi / 2.0d0 )
					return
			end if
			
			if ( ind == 2 ) then
					g(1) = 2.0d0 * ( x(1) - 4.0d0 )
					return	
			end if
			
	end if	
	
! ----------------------------------------------------------------------

	!  SK1

	if ( problem == 26 ) then 
			
			if ( ind == 1 ) then
					g(1) = 4.0d0 * x(1) ** 3 + 9.0d0 * x(1) ** 2 - 2.0d1 * x(1) - 1.0d1
					return
			end if
			
			if ( ind == 2 ) then
					g(1) = 2.0d0 * x(1) ** 3 - 6.0d0 * x(1) ** 2 - 2.0d1 * x(1) + 1.0d1
					return	
			end if
			
	end if	
	
! ----------------------------------------------------------------------

	!  SK2

	if ( problem == 27 ) then 
			
			if ( ind == 1 ) then
					g(1) = 2.0d0 * ( x(1) - 2.0d0 )
					g(2) = 2.0d0 * ( x(2) + 3.0d0 )
					g(3) = 2.0d0 * ( x(3) - 5.0d0 )
					g(4) = 2.0d0 * ( x(4) - 4.0d0 )
					return
			end if
			
			if ( ind == 2 ) then
					faux = 1.0d0 + ( x(1) ** 2 + x(2) ** 2 + x(3) ** 2 + x(4) ** 2 ) / 1.0d2
					
					g(1) = ( - cos( x(1) ) * faux + ( sin( x(1) ) + sin( x(2) ) &
					+ sin( x(3) ) + sin( x(4) ) ) * x(1) / 5.0d1 ) / faux ** 2
					g(2) = ( - cos( x(2) ) * faux + ( sin( x(1) ) + sin( x(2) ) &
					+ sin( x(3) ) + sin( x(4) ) ) * x(2) / 5.0d1 ) / faux ** 2
					g(3) = ( - cos( x(3) ) * faux + ( sin( x(1) ) + sin( x(2) ) &
					+ sin( x(3) ) + sin( x(4) ) ) * x(3) / 5.0d1 ) / faux ** 2
					g(4) = ( - cos( x(4) ) * faux + ( sin( x(1) ) + sin( x(2) ) &
					+ sin( x(3) ) + sin( x(4) ) ) * x(4) / 5.0d1 ) / faux ** 2
					return	
			end if
			
	end if	
	
! ----------------------------------------------------------------------

	!  VU1

	if ( problem == 28 ) then 
			
			if ( ind == 1 ) then
					g(1) = - 2.0d0 * x(1) / ( x(1) ** 2 + x(2) ** 2 + 1.0d0 ) ** 2
					g(2) = - 2.0d0 * x(2) / ( x(1) ** 2 + x(2) ** 2 + 1.0d0 ) ** 2
					return
			end if
			
			if ( ind == 2 ) then
					g(1) = 2.0d0 * x(1)
					g(2) = 6.0d0 * x(2)
					return	
			end if
			
	end if	
	
! ----------------------------------------------------------------------

	!  Hil1

	if ( problem == 29 ) then 
			a = 2.0d0 * pi / 3.6d2 * ( 4.5d1 + 4.0d1 * sin( 2.0d0 * pi * x(1) ) &
			    + 2.5d1 * sin( 2.0d0 * pi * x(2) ) )
			b = 1.0d0 + 0.5d0 * cos( 2.0d0 * pi * x(1) )
			
			if ( ind == 1 ) then
					g(1) = - 1.6d2 * pi ** 2 / 3.6d2 * cos( 2.0d0 * pi * x(1) ) * sin( a ) *  b &
					- pi * sin( 2.0d0 * pi * x(1) ) * cos( a ) 
					g(2) = - 1.0d2 * pi ** 2 / 3.6d2 * cos( 2.0d0 * pi * x(2) ) * sin( a ) *  b 
					return
			end if
			
			if ( ind == 2 ) then
					g(1) = 1.6d2 * pi ** 2 / 3.6d2 * cos( 2.0d0 * pi * x(1) ) * cos( a ) *  b &
					- pi * sin( 2.0d0 * pi * x(1) ) * sin( a ) 
					g(2) = 1.0d2 * pi ** 2 / 3.6d2 * cos( 2.0d0 * pi * x(2) ) * cos( a ) *  b 
					return	
			end if
			
	end if
	
! ----------------------------------------------------------------------

	!  DD1

	if ( problem == 30 ) then 
			
			if ( ind == 1 ) then
					do i = 1,n
							g(i) = 2.0d0 * x(i)
					end do
					return
			end if
			
			if ( ind == 2 ) then
					g(1) = 3.0d0
					g(2) = 2.0d0
					g(3) = - 1.0d0 / 3.0d0
					g(4) = 3.0d-2 * ( x(4) - x(5) ) ** 2
					g(5) = - 3.0d-2 * ( x(4) - x(5) ) ** 2
					return	
			end if
			
	end if	
	
! ----------------------------------------------------------------------

	! 	KW2
	
	if ( problem == 31 ) then 
			
			if ( ind == 1 ) then
					g(1) = 6.0d0 * ( 1.0d0 - x(1) ) * exp( -x(1)**2 - ( x(2) + 1.0d0 ) ** 2 ) &
					      + 6.0d0 * ( 1.0d0 - x(1) )**2 * exp( -x(1)**2 - ( x(2) + 1.0d0 ) ** 2 ) * x(1) &
					      + 1.0d1 * ( 1.0d0 / 5.0d0 - 3.0d0 * x(1)**2 ) * exp( - x(1)**2 - x(2)**2 ) &
					      - 2.0d1 * ( x(1) / 5.0d0 - x(1)**3 - x(2)**5 ) * exp( - x(1)**2 - x(2)**2 ) * x(1) &
					      - 6.0d0 * exp( -( x(1) + 2.0d0 )**2 - x(2)**2 ) * ( x(1) + 2.0d0 ) - 1.0d0
					g(2) = 6.0d0 * ( 1.0d0 - x(1) )**2 * exp( -x(1)**2 - ( x(2) + 1.0d0 ) ** 2 ) * ( x(2) + 1.0d0 ) &
								- 5.0d1 * x(2)**4 * exp( - x(1)**2 - x(2)**2 ) &
								- 1.0d1 * ( x(1) / 5.0d0 - x(1)**3 - x(2)**5 ) * exp( - x(1)**2 - x(2)**2 ) * 2.0d0 * x(2)&
								- 6.0d0 * exp( -( x(1) + 2.0d0 )**2 - x(2)**2 ) * x(2) - 0.5d0
					
!					f = - 3.0d0 * ( 1.0d0 - x(1) )**2 * exp( -x(1)**2 - ( x(2) + 1.0d0 ) ** 2 ) &
!					    + 1.0d1 * ( x(1) / 5.0d0 - x(1)**3 - x(2)**5 ) * exp( - x(1)**2 - x(2)**2 ) &
!					    + 3.0d0 * exp( -( x(1) + 2.0d0 )**2 - x(2)**2 ) - 0.5d0 * ( 2.0d0 * x(1) + x(2) )
					return
			end if
			
			if ( ind == 2 ) then
					g(1) = - 6.0d0 * ( 1.0d0 + x(2) )**2 * exp( -x(2)**2 - ( 1.0d0 - x(1) ) ** 2 ) * ( 1.0d0 - x(1) ) &
								+ 5.0d1 * x(1)**4 * exp( - x(1)**2 - x(2)**2 ) &
								- 2.0d1 * ( - x(2) / 5.0d0 + x(2)**3 + x(1)**5 ) * exp( - x(1)**2 - x(2)**2 ) * x(1) &
								- 6.0d0 * exp( -( 2.0d0 - x(2) )**2 - x(1)**2 ) * x(1)
					
					g(2) = - 6.0d0 * ( 1.0d0 + x(2) ) * exp( -x(2)**2 - ( 1.0d0 - x(1) ) ** 2 ) &
								 + 6.0d0 * ( 1.0d0 + x(2) )**2 * exp( -x(2)**2 - ( 1.0d0 - x(1) ) ** 2 ) * x(2) &
								 + 1.0d1 * ( - 1.0d0 / 5.0d0 + 3.0d0 * x(2)**2 ) * exp( - x(1)**2 - x(2)**2 ) &
								 - 2.0d1 * ( - x(2) / 5.0d0 + x(2)**3 + x(1)**5 ) * exp( - x(1)**2 - x(2)**2 ) * x(2)&
								 + 6.0d0 * exp( -( 2.0d0 - x(2) )**2 - x(1)**2 ) * ( 2.0d0 - x(2) )
					
!					f = - 3.0d0 * ( 1.0d0 + x(2) )**2 * exp( -x(2)**2 - ( 1.0d0 - x(1) ) ** 2 ) &
!					    + 1.0d1 * ( - x(2) / 5.0d0 + x(2)**3 + x(1)**5 ) * exp( - x(1)**2 - x(2)**2 ) &
!					    + 3.0d0 * exp( -( 2.0d0 - x(2) )**2 - x(1)**2 ) 
					return	
			end if
			
	end if
	
! ----------------------------------------------------------------------

	! 	Toi4
	
	if ( problem == 32 ) then 
			
			if ( ind == 1 ) then
					g(1) = 2.0d0 * x(1)
					g(2) = 2.0d0 * x(2)
					g(3) = 0.0d0
					g(4) = 0.0d0
					return
			end if
			
			if ( ind == 2 ) then
					g(1) = x(1) - x(2)
					g(2) = - ( x(1) - x(2) )
					g(3) = x(3) - x(4)
					g(4) = - ( x(3) - x(4) )
					return	
			end if
			
	end if						
	
! ----------------------------------------------------------------------

	! 	Toi8
	
	if ( problem == 33 ) then 
	
			g(:) = 0.0d0
			
			if ( ind == 1 ) then
					g(1) = 4.0d0 * ( 2.0d0 * x(1) - 1.0d0 )
					return
			end if
			
			if ( ind /= 1 ) then
					g(ind-1) = 4.0d0 * ind * ( 2.0d0 * x(ind-1) - x(ind) )
					g(ind)   = - 2.0d0 * ind * ( 2.0d0 * x(ind-1) - x(ind) )
					return	
			end if
			
	end if	
	
! ----------------------------------------------------------------------

	! 	Toi9
	
	if ( problem == 34 ) then 
	
			g(:) = 0.0d0
			
			if ( ind == 1 ) then
					g(1) = 4.0d0 * ( 2.0d0 * x(1) - 1.0d0 )
					g(2) = 2.0d0 * x(2)
					return
			end if
			
			if ( ind > 1 .and. ind < n ) then
					g(ind-1) =  4.0d0 * ind * ( 2.0d0 * x(ind-1) - x(ind) ) &
					          - 2.0d0 * ( ind - 1.0d0 ) * x(ind-1)
					g(ind)   = - 2.0d0 * ind * ( 2.0d0 * x(ind-1) - x(ind) ) + 2.0d0 * ind * x(ind)
					return	
			end if
			
			if ( ind == n ) then
					g(n-1) = 4.0d0 * n * ( 2.0d0 * x(n-1) - x(n) ) - 2.0d0 * ( n - 1.0d0 ) * x(n-1)
					g(n)   = - 2.0d0 * n * ( 2.0d0 * x(n-1) - x(n) )
					return
			end if
			
	end if
	
! ----------------------------------------------------------------------

	! 	Toi10 (Rosenbrock)
	
	if ( problem == 35 ) then 
			
			g(:) = 0.0d0
			
			g(ind)   = - 4.0d2 * ( x(ind+1) - x(ind) ** 2 ) * x(ind)
			g(ind+1) = 2.0d2 * ( x(ind+1) - x(ind) ** 2 ) + 2.0d0 * ( x(ind+1) - 1.0d0 )
			return
						
	end if		
	
! ----------------------------------------------------------------------

	! 	MGH9 
	
	if ( problem == 36 ) then 
				
			t = ( 8.0d0 - ind ) / 2.0d0
			
			g(1) = exp( - x(2) * ( t - x(3) ) ** 2 / 2.0d0 )
			g(2) = - x(1) * exp( - x(2) * ( t - x(3) ) ** 2 / 2.0d0 ) * ( t - x(3) ) ** 2 / 2.0d0
			g(3) = x(1) * exp( - x(2) * ( t - x(3) ) ** 2 / 2.0d0 ) * x(2) * ( t - x(3) ) 
					
!			f = x(1) * exp( - x(2) * ( t - x(3) ) ** 2 / 2.0d0 ) - y			
			
			return
			
	end if		
	
! ----------------------------------------------------------------------

	! 	MGH16 
	
	if ( problem == 37 ) then 
			
			t = ind / 5.0d0
			
			g(1) = 2.0d0 * ( x(1) + t * x(2) - exp(t) )
			g(2) = 2.0d0 * t * ( x(1) + t * x(2) - exp(t) )
			g(3) = 2.0d0 * ( x(3) + x(4) * sin(t) - cos(t) )
			g(4) = 2.0d0 * sin(t) * ( x(3) + x(4) * sin(t) - cos(t) )
					
!			f = ( x(1) + t * x(2) - exp(t) ) ** 2 + ( x(3) + x(4) * sin(t) - cos(t) ) ** 2
			
			return
			
	end if		
	
! ----------------------------------------------------------------------

	! 	MGH26 
	
	if ( problem == 38 ) then 
								
			t = 0.0d0
			do i = 1,n
					t = t + cos(x(i))
			end do
			
			gaux1 = 2.0d0 * ( n - t + ind * ( 1.0d0 - cos(x(ind)) ) - sin(x(ind)) )
			
			g(1) = gaux1 * sin(x(1))
			g(2) = gaux1 * sin(x(2))
			g(3) = gaux1 * sin(x(3))
			g(4) = gaux1 * sin(x(4))
			
			g(ind) = g(ind)  + gaux1 * ( ind * sin(x(ind)) - cos(x(ind)) )
			
!			f = ( n - t + ind * ( 1.0d0 - cos(x(ind)) ) - sin(x(ind)) ) ** 2
			
			return
			
	end if				
	
! ----------------------------------------------------------------------

	! 	MGH33
	
	if ( problem == 39 ) then 
	
			faux = 0.0d0					
			do i = 1,n
					faux = faux + i * x(i)
			end do
			
			faux = 2.0d0 * ( ind * faux - 1.0d0 ) 
		
			do i = 1,n
					g(i) = faux * real(i) * ind
			end do
									
			return
			
	end if		
	
! ----------------------------------------------------------------------

	! 	DTLZ2.2
	
	if ( problem == 40 ) then 
								
			faux  = 0.0d0		
			gaux1 = 0.0d0			
			do i = 3,n
					faux  = faux + ( x(i) ** 2 - x(1) ) ** 2
					gaux1 = gaux1 - 2.0d0 * ( x(i) ** 2 - x(1) )
			end do
			
!			if ( ind == 1 ) then
!					f = cos( pi / 2.0d0 * x(1) ) * cos( pi / 2.0d0 * x(2) ) * ( 1.0d0 + faux )
!					return
!			end if
			
!			if ( ind == 2 ) then
!					f = cos( pi / 2.0d0 * x(1) ) * sin( pi / 2.0d0 * x(2) ) * ( 1.0d0 + faux )
!					return
!			end if
			
!			if ( ind == 3 ) then
!					f = sin( pi / 2.0d0 * x(1) ) * ( 1.0d0 + faux )
!					return
!			end if
			
			if ( ind == 1 ) then
					g(1) = - pi / 2.0d0 * sin( pi / 2.0d0 * x(1) ) * cos( pi / 2.0d0 * x(2) ) * ( 1.0d0 + faux ) &
					       + cos( pi / 2.0d0 * x(1) ) * cos( pi / 2.0d0 * x(2) ) * gaux1
					g(2) = - pi / 2.0d0 * cos( pi / 2.0d0 * x(1) ) * sin( pi / 2.0d0 * x(2) ) * ( 1.0d0 + faux )
					
					do i = 3,n
							g(i) = 4.0d0 * x(i) * ( x(i) ** 2 - x(1) ) * cos( pi / 2.0d0 * x(1) ) * cos( pi / 2.0d0 * x(2) )
					end do      
					
					return
			end if
			
			if ( ind == 2 ) then
					g(1) = - pi / 2.0d0 * sin( pi / 2.0d0 * x(1) ) * sin( pi / 2.0d0 * x(2) ) * ( 1.0d0 + faux ) &
					       + cos( pi / 2.0d0 * x(1) ) * sin( pi / 2.0d0 * x(2) ) * gaux1
					g(2) = pi / 2.0d0 * cos( pi / 2.0d0 * x(1) ) * cos( pi / 2.0d0 * x(2) ) * ( 1.0d0 + faux )
					
					do i = 3,n
							g(i) = 4.0d0 * x(i) * ( x(i) ** 2 - x(1) ) * cos( pi / 2.0d0 * x(1) ) * sin( pi / 2.0d0 * x(2) )
					end do  
			
					return
			end if
			
			if ( ind == 3 ) then
					g(1) = pi / 2.0d0 * cos( pi / 2.0d0 * x(1) ) * ( 1.0d0 + faux ) + sin( pi / 2.0d0 * x(1) ) * gaux1
					g(2) = 0.0d0
					
					do i = 3,n
							g(i) = 4.0d0 * x(i) * ( x(i) ** 2 - x(1) ) * sin( pi / 2.0d0 * x(1) )
					end do  
					
					return
			end if
			
	end if	
	
! ----------------------------------------------------------------------

	! 	PNR
	
	if ( problem == 41 ) then 
											
			if ( ind == 1 ) then
					g(1) = 4.0d0 * x(1) ** 3 - 2.0d0 * x(1) - 1.0d1 * x(2)
					g(2) = 4.0d0 * x(2) ** 3 + 2.0d0 * x(2) - 1.0d1 * x(1)
					return
			end if
			
			if ( ind == 2 ) then
					g(1) = 2.0d0 * x(1)
					g(2) = 2.0d0 * x(2)
					return
			end if
			
	end if		
	
! ----------------------------------------------------------------------

	! 	LTDZ
	!	Combining convergence and diversity in evolutionary multiobjective optimization
	
	if ( problem == 42 ) then 
											
			if ( ind == 1 ) then
					g(1) = pi / 2.0d0 * ( 1.0d0 + x(3) ) * sin( x(1) * pi / 2.0d0 ) * cos( x(2) * pi / 2.0d0 )
					g(2) = pi / 2.0d0 * ( 1.0d0 + x(3) ) * cos( x(1) * pi / 2.0d0 ) * sin( x(2) * pi / 2.0d0 )
					g(3) = - cos( x(1) * pi / 2.0d0 ) * cos( x(2) * pi / 2.0d0 )
					g = - g
					return
			end if
			
			if ( ind == 2 ) then
					g(1) = pi / 2.0d0 * ( 1.0d0 + x(3) ) * sin( x(1) * pi / 2.0d0 ) * sin( x(2) * pi / 2.0d0 )
					g(2) = - pi / 2.0d0 * ( 1.0d0 + x(3) ) * cos( x(1) * pi / 2.0d0 ) * cos( x(2) * pi / 2.0d0 )
					g(3) = - cos( x(1) * pi / 2.0d0 ) * sin( x(2) * pi / 2.0d0 )
					g = - g
					return
			end if
			
			if ( ind == 3 ) then
					g(1) = pi / 2.0d0 * ( 1.0d0 + x(3) ) * sin( x(1) * pi / 2.0d0 ) * sin( x(1) * pi / 2.0d0 ) &
					     - pi / 2.0d0 * ( 1.0d0 + x(3) ) * cos( x(1) * pi / 2.0d0 ) * cos( x(1) * pi / 2.0d0 )
					g(2) = 0.0d0
					g(3) = - cos( x(1) * pi / 2.0d0 ) * sin( x(1) * pi / 2.0d0 )
					g = - g
					return
			end if
			
	end if	
	
! ----------------------------------------------------------------------

	! 	SLCDT2
	!	Convergence of stochastic search algorithms to finite size pareto set approximations 
	
	if ( problem == 43 ) then 
											
			if ( ind == 1 ) then
					g(1) = 4.0d0 * ( x(1) - 1.0d0 ) ** 3
					do i = 2,n
							g(i) = 2.0d0 * ( x(i) - 1.0d0 )
					end do
					return
			end if
			
			if ( ind == 2 ) then
					g(2) = 4.0d0 * ( x(2) + 1.0d0 ) ** 3
					
					do i = 1,n
							if ( i /= 2 ) g(i) = 2.0d0 * ( x(i) + 1.0d0 )
					end do				
					return	
			end if
			
			if ( ind == 3 ) then
					g(3) = 4.0d0 * ( x(3) - 1.0d0 ) ** 3
					do i = 1,n
							if ( i /= 3 ) g(i) = 2.0d0 * ( x(i) - ( - 1.0d0 ) ** (i+1) ) 
					end do
					return
			end if
			
	end if								
														
				

	end subroutine evalg
	
	!***********************************************************************
	!***********************************************************************

	subroutine quadstep(n,x,d,stp,ind,flag)
	
	implicit none

	! SCALAR ARGUMENTS
	integer, intent(in) :: n,ind
	real(kind=8), intent(out) :: stp
	integer, intent(out) :: flag
	
	! ARRAY ARGUMENTS
	real(kind=8), intent(in) :: x(n),d(n)
		
	! LOCAL SCALARS
	integer :: i,allocerr
	
	! LOCAL ARRAYS
	real(kind=8),allocatable :: A(:,:),b(:)
		
	!     If quadratic(ind) is set equal to true in function inip, then
	!     YOU MUST to provide the minimizer x of the associated function.
	
	flag = -1
	
	! JOS1
	! Dynamic Weighted Aggregation for Evolutionary Multi-Objetive Optimization: Why Does It Work and How?
	
	if ( problem == 1 ) then
			if ( ind == 1 ) then
					stp = - dot_product(x,d) / dot_product(d,d)
					flag = 0
					return
			end if
			
			if ( ind == 2 ) then
					stp = ( - dot_product(x,d) + 2.0d0 * sum(d) ) / dot_product(d,d)
					flag = 0
					return
			end if
	end if
	
	! ----------------------------------------------------------------------

	! AP2
	! Example 2 of "A modified Quasi-Newton method for vector optimization problem"
	
	if ( problem == 5 ) then
			if ( ind == 1 ) then
					stp = - dot_product(x,d) / dot_product(d,d)
					flag = 0
					return
			end if
			
			if ( ind == 2 ) then
					stp = ( - dot_product(x,d) + sum(d) ) / dot_product(d,d)
					flag = 0
					return
			end if
	end if
	
	! ----------------------------------------------------------------------
	
	! Lov1
	! "A SYNTHETIC APPROACH TO MULTIOBJECTIVE OPTIMIZATION"
	
	if ( problem == 8 ) then 
		if ( ind == 1 ) then
					stp = - ( 1.05d0 * x(1) * d(1) + 0.98d0 * x(2) * d(2) ) / &
					        ( 1.05d0 * d(1)**2 + 0.98d0 * d(2)**2 )
					flag = 0
					return
			end if
			
			if ( ind == 2 ) then
			
					allocate(A(n,n),b(n),stat=allocerr)
					if ( allocerr .ne. 0 ) then
						 write(*,*) 'Allocation error in main program'
						 stop
					end if
					A(:,:) = 0.0d0
					A(1,1) = 2.0d0 * 0.99d0
					A(2,2) = 2.0d0 * 1.03d0
					b(1)   = - 6.0d0 * 0.99d0
					b(2)   = - 5.0d0 * 1.03d0
					
					stp = - dot_product( matmul(A,x)+b,d ) / dot_product(d,matmul(A,d))
					flag = 0
					
					deallocate(A,b)
					return
			end if
	end if
	
! ----------------------------------------------------------------------
	
	! Lov3
	! "A SYNTHETIC APPROACH TO MULTIOBJECTIVE OPTIMIZATION"
	
	if ( problem == 9 ) then 	
			if ( ind == 1 ) then
					stp = - dot_product(x,d) / dot_product(d,d)
					flag = 0
					return
			end if
	end if
	
! ----------------------------------------------------------------------
	
	! Lov4
	! "A SYNTHETIC APPROACH TO MULTIOBJECTIVE OPTIMIZATION"
	
	if ( problem == 10 ) then 
			if ( ind == 2 ) then
			
					allocate(A(n,n),b(n),stat=allocerr)
					if ( allocerr .ne. 0 ) then
						 write(*,*) 'Allocation error in main program'
						 stop
					end if
					
					A(:,:) = 0.0d0
					A(1,1) = 2.0d0
					A(2,2) = 2.0d0
					b(1)   = - 1.2d1
					b(2)   =   1.0d0
					
					stp = - dot_product( matmul(A,x)+b,d ) / dot_product(d,matmul(A,d))
					flag = 0
					
					deallocate(A,b)
					return
			end if
	end if
	
! ----------------------------------------------------------------------

	!  MOP1 ( Particular case of JOS1 )		
	!  A Review of Multiobjective Test Problems and a Scalable Test Problem Toolkit

	if ( problem == 15 ) then 	
	
		if ( ind == 1 ) then
				stp = - dot_product(x,d) / dot_product(d,d)
				flag = 0
				return
		end if
		
		if ( ind == 2 ) then
				stp = ( - dot_product(x,d) + 2.0d0 * sum(d) ) / dot_product(d,d)
				flag = 0
				return
		end if
	end if
	
	! ----------------------------------------------------------------------

	!  MOP3	
	!  A Review of Multiobjective Test Problems and a Scalable Test Problem Toolkit	
	
	if ( problem == 17 ) then 
			if ( ind == 2 ) then
			
					allocate(A(n,n),b(n),stat=allocerr)
					if ( allocerr .ne. 0 ) then
						 write(*,*) 'Allocation error in main program'
						 stop
					end if
					
					A(:,:) = 0.0d0
					A(1,1) = 2.0d0
					A(2,2) = 2.0d0
					b(1)   = 6.0d0
					b(2)   = 2.0d0
					
					stp = - dot_product( matmul(A,x)+b,d ) / dot_product(d,matmul(A,d))
					flag = 0
					
					deallocate(A,b)
					return
			end if
	end if
	
	
! ----------------------------------------------------------------------

	!  MOP5
	!  A Review of Multiobjective Test Problems and a Scalable Test Problem Toolkit
	
	if ( problem == 18 ) then 
			if ( ind == 2 ) then
			
					allocate(A(n,n),b(n),stat=allocerr)
					if ( allocerr .ne. 0 ) then
						 write(*,*) 'Allocation error in main program'
						 stop
					end if
					
					A(1,1) = 9.0d0/4.0d0 + 2.0d0/2.7d1
					A(1,2) = -6.0d0/4.0d0 - 2.0d0/2.7d1
					A(2,1) = A(1,2)
					A(2,2) = 1.0d0 + 2.0d0/2.7d1
					b(1)   = 3.0d0 + 2.0d0/2.7d1
					b(2)   = - 2.0d0 - 2.0d0/2.7d1
					
					stp = - dot_product( matmul(A,x)+b,d ) / dot_product(d,matmul(A,d))
					flag = 0
					
					deallocate(A,b)
					return
			end if
	end if
	
! ----------------------------------------------------------------------

	!  MOP7
	!  A Review of Multiobjective Test Problems and a Scalable Test Problem Toolkit
	
	if ( problem == 19 ) then 	
	
			allocate(A(n,n),b(n),stat=allocerr)
			if ( allocerr .ne. 0 ) then
				 write(*,*) 'Allocation error in main program'
				 stop
			end if
			
			if ( ind == 1 ) then
			
					A(1,1) = 1.0d0
					A(1,2) = 0.0d0
					A(2,1) = A(1,2)
					A(2,2) = 2.0d0 / 1.3d1
					b(1)   = -2.0d0
					b(2)   = 2.0d0 / 1.3d1
			
					stp = - dot_product( matmul(A,x)+b,d ) / dot_product(d,matmul(A,d))
					flag = 0
					
					deallocate(A,b)
					return
			end if
			
			if ( ind == 2 ) then
			
					A(1,1) = 1.0d0 / 1.8d1 + 1.0d0 / 4.0d0
					A(1,2) = 1.0d0 / 1.8d1 - 1.0d0 / 4.0d0
					A(2,1) = A(1,2)
					A(2,2) = A(1,1)
					b(1)   = -6.0d0 / 3.6d1 - 4.0d0 / 8.0d0
					b(2)   = -6.0d0 / 3.6d1 + 4.0d0 / 8.0d0
			
					stp = - dot_product( matmul(A,x)+b,d ) / dot_product(d,matmul(A,d))
					flag = 0
					
					deallocate(A,b)
					return
			end if
			
			if ( ind == 3 ) then
			
					A(1,1) = 2.0d0 / 1.75d2 + 2.0d0 / 1.7d1
					A(1,2) = 4.0d0 / 1.75d2 - 4.0d0 / 1.7d1
					A(2,1) = A(1,2)
					A(2,2) = 8.0d0 / 1.75d2 + 8.0d0 / 1.7d1
					b(1)   = - 2.0d0 / 1.75d2
					b(2)   = - 4.0d0 / 1.75d2
			
					stp = - dot_product( matmul(A,x)+b,d ) / dot_product(d,matmul(A,d))
					flag = 0
					
					deallocate(A,b)
					return
			end if
	end if
	
! ----------------------------------------------------------------------

	!   SP1
	!  	A Review of Multiobjective Test Problems and a Scalable Test Problem Toolkit
	
	if ( problem == 24 ) then 
	
			allocate(A(n,n),b(n),stat=allocerr)
			if ( allocerr .ne. 0 ) then
				 write(*,*) 'Allocation error in main program'
				 stop
			end if
			
			if ( ind == 1 ) then
			
					A(1,1) = 4.0d0
					A(1,2) = -2.0d0
					A(2,1) = A(1,2)
					A(2,2) = 2.0d0 
					b(1)   = -2.0d0
					b(2)   = 0.0d0
			
					stp = - dot_product( matmul(A,x)+b,d ) / dot_product(d,matmul(A,d))
					flag = 0
					
					deallocate(A,b)
					return
			end if
			
			if ( ind == 2 ) then
			
					A(1,1) = 2.0d0
					A(1,2) = -2.0d0
					A(2,1) = A(1,2)
					A(2,2) = 4.0d0
					b(1)   = 0.0d0
					b(2)   = -6.0d0
			
					stp = - dot_product( matmul(A,x)+b,d ) / dot_product(d,matmul(A,d))
					flag = 0
					
					deallocate(A,b)
					return
			end if	
			
	end if
	
! ----------------------------------------------------------------------

	!   SSFYY2
	!   A Review of Multiobjective Test Problems and a Scalable Test Problem Toolkit
	
	if ( problem == 25 ) then 	
	
			if ( ind == 2 ) then
					stp = ( - dot_product(x,d) + 4.0d0 * sum(d) ) / dot_product(d,d)
					flag = 0
					return
			end if
			
	end if
	
! ----------------------------------------------------------------------

	!   SK2
	!  	A Review of Multiobjective Test Problems and a Scalable Test Problem Toolkit
	
	if ( problem == 27 ) then 	
	
			if ( ind == 1 ) then
			
					allocate(A(n,n),b(n),stat=allocerr)
					if ( allocerr .ne. 0 ) then
						 write(*,*) 'Allocation error in main program'
						 stop
					end if
			
					A(:,:) = 0.0d0
					A(1,1) = 2.0d0
					A(2,2) = 2.0d0
					A(3,3) = 2.0d0
					A(4,4) = 2.0d0
					b(1)   = -4.0d0
					b(2)   = 6.0d0
					b(3)   = -1.0d1
					b(4)   = -8.0d0
					
					stp = - dot_product( matmul(A,x)+b,d ) / dot_product(d,matmul(A,d))
					flag = 0
					
					deallocate(A,b)
					return
			end if
			
	end if
	
! ----------------------------------------------------------------------

	!   VU1
	!   A Review of Multiobjective Test Problems and a Scalable Test Problem Toolkit
	
	if ( problem == 28 ) then 	
	
			if ( ind == 2 ) then
			
					stp = - ( 2.0d0 * x(1) * d(1) + 6.0d0 * x(2) * d(2) ) / &
					        ( 2.0d0 * d(1)**2 + 6.0d0 * d(2)**2 )
					flag = 0
					return
			end if
	end if
	
! ----------------------------------------------------------------------

	!   DD1
	
	if ( problem == 30 ) then 	
	
			if ( ind == 1 ) then
					stp = - dot_product(x,d) / dot_product(d,d)
					flag = 0
					return
			end if
	end if	
	
! ----------------------------------------------------------------------

	!   PNR
	
	if ( problem == 41 ) then 	
	
			if ( ind == 2 ) then
					stp = - dot_product(x,d) / dot_product(d,d)
					flag = 0
					return
			end if
	end if		
	
	end subroutine quadstep

end module myproblem
