program main

	use globals
	use myproblem, only: inip

	implicit none
	
	! LOCAL SCALARS
	integer :: CGparam,outiter,nfev,ngev,nvev,inform
	real(kind=4) :: time
	
	! Define the CG method:
	
  !	CGparam = 1: Fletcher-Reeves method
  !	CGparam = 2: Conjugate Descent method
  !	CGparam = 3: Dai-Yuan method
  !	CGparam = 4: modified Dai-Yuan method
  !	CGparam = 5: Polak-Ribière-Polyak method
  !	CGparam = 6: Hestenes-Stiefel method
  !	CGparam = 7: Hager-Zhang method
  !	CGparam = 8: non-negative Hager-Zhang method
	
	CGparam = 7
	
	! Set the HZparam for the HZ method
	
	if ( CGparam == 7 .or. CGparam == 8 ) HZparam = 1.0d0
	
	! Modify myproblem routine to solve your own problem
	! Alternatively, choose a test problem - see myproblem.f90
	
	problem = 12
	
	seed = 123456.0d0
	
	! Read the initial data of the problem
	
	call inip(n,m,x,quadratic,scaleF,checkder)
	
	! Call NLCG

	call NLCGVO(CGparam,outiter,time,nfev,ngev,nvev,inform)

end program main
