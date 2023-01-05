subroutine NLCGVO(CGparam,outiter,time,nfev,ngev,nvev,inform)

	use globals  
	use myproblem

	implicit none
	
	! SCALARS ARGUMENTS
	integer,intent(in) :: CGparam
	integer,intent(out) :: outiter,nfev,ngev,nvev,inform
	real(kind=4),intent(out) :: time
	
	! LOCAL SCALARS
	integer :: ind,informLS,informALG,outiterLS,informfg,totiniterLS,mquad,mnquad,&
						 nfevLS,ngevLS,allocerr,maxoutiter,LStype
	real(kind=8), parameter :: zero = 0.0d0, one = 1.0d0, two = 2.0d0,   &
	                           bignum = 1.0d+99
	real(kind=8) :: stp,stpmin,stpmax,epsopt,ftol,gtol,ctol,beta,fxv,fxd,&
	                fxprevvprev,fxprevv,fxdprev,fxprevdprev,fxvprev,     &
	                fnegd,eta,norm2v,Fsupn,f,g,theta,tolLS
	real(kind=4) :: timeini,timefin
	logical :: iprint,iprintLS,dsdc,restart

	! LOCAL ARRAYS
	real(kind=8),allocatable :: phi0(:),gphi0(:),v(:),xprev(:),vprev(:),dprev(:),JFprev(:,:)
	integer,allocatable :: quadind(:),nquadind(:)
	character(len=4) :: CGname
	character(len=1) :: restinf,restinfprev

	! EXTERNAL SUBROUTINES
	external :: evalphi,evalgphi,evalqstep
	
  ! This subroutine solves the multiobjective optimization problem
  !
  ! Minimize F(x)
  !
  ! where F:R^n->R^m, using the non-linear conjugate gradient method
  ! described in
	!
  ! L. R. Lucambio Pérez and L. F. Prudente, Non-linear conjugate 
  ! gradient methods for vector optimization, SIAM Journal on 
  ! Optimization 28(3), pp. 2690-2720, 2018.
  !
  ! and
  !
  ! M. L. N. Gonçalves and L. F. Prudente, On the extension of the 
  ! Hager-Zhang conjugate gradient method for vector optimization, 2018.
  !
  ! The method used is chosen according to CGparam:
  !	CGparam = 0: Steepest descent method
  !	CGparam = 1: Fletcher-Reeves method
  !	CGparam = 2: Conjugate Descent method
  !	CGparam = 3: Dai-Yuan method
  !	CGparam = 4: modified Dai-Yuan method
  !	CGparam = 5: Polak-Ribière-Polyak method
  !	CGparam = 6: Hestenes-Stiefel method
  !	CGparam = 7: Hager-Zhang method
  !	CGparam = 8: non-negative Hager-Zhang method
  ! 
  ! The user is required to code the function values and their gradients
  ! in subroutine myproblem.
  !
  ! The steepest descent direction is calculated in innersolver routine.
  ! This routine uses the software Algencan contained in libalgencan.a.
  ! A step-size satifying the Wolfe conditions is determined at each 
	! iteration by means of the line search routine lsvecopt. This routine
	! uses the codes dcsrch and dcstep of Moré and Thuente.  
  !
  ! inform:
  !
  !  0: Optimality satisfied
  !  1: Maximum number of iterations reached
  ! -1: An error occurred during the execution of NLCG algorithm
  !
  ! August of 2019.
  !
  
  interface
		subroutine quadvar(quadratic,m,mquad,mnquad,quadind,nquadind)
		! SCALAR ARGUMENTS
		integer, intent(in)  :: m
		integer, intent(out) :: mquad,mnquad
		! ARRAY ARGUMENTS
		integer,allocatable,intent(out) :: quadind(:),nquadind(:)
		logical,intent(in) :: quadratic(m)
		end subroutine quadvar
	end interface

	!---------------------------------------------------------------------   
	!     Initialization
	!---------------------------------------------------------------------
	
	! Start timing
	
	call cpu_time(timeini)	
	
	! Set default parameters 

	epsopt 	= 5.0d0 * sqrt( 2.0d0 ** (-52) )
	ftol 	= 1.d-04
	gtol	= 1.d-01
	ctol 	= 4.d-01
		
	maxoutiter = 5000

	stpmin = 1.0d-15
	stpmax = 1.0d+10
	
	call quadvar(quadratic,m,mquad,mnquad,quadind,nquadind)
	
	! Allocating the arrays
	
	allocate(d(n),v(n),xprev(n),dprev(n),vprev(n),phi0(m),gphi0(m),sF(m),&
	         JF(m,n),JFprev(m,n),stat=allocerr)
	if ( allocerr .ne. 0 ) then
     write(*,*) 'Allocation error in main program'
     stop
	end if
	
	! Identify the type of line search required
	
	!	LStype = 1: standard Wolfe conditions
	!	LStype = 2: strong Wolfe conditions
	!	LStype = 3: restricted Wolfe conditions
	
	if ( CGparam == 0 ) then
		LStype = 1
	else if ( CGparam == 1 ) then
		LStype = 3
	else if ( CGparam == 2 ) then
		LStype = 2
	else if ( CGparam == 3 ) then
		LStype = 2
	else if ( CGparam == 4 ) then
		LStype = 2
	else if ( CGparam == 5 ) then
		LStype = 3
	else if ( CGparam == 6 ) then
		LStype = 3
	else if ( CGparam == 7 ) then
		LStype = 3
	else if ( CGparam == 8 ) then
		LStype = 3
	end if 
	
	! Print problem information	
	
	iprint   = .true.	
	iprintLS = .false.
	
	if ( iprint ) then
	
		if ( CGparam == 0 ) then
			CGname = 'SD'
		else if ( CGparam == 1 ) then
			CGname = 'FR'
		else if ( CGparam == 2 ) then
			CGname = 'CD'
		else if ( CGparam == 3 ) then
			CGname = 'DY'
		else if ( CGparam == 4 ) then
			CGname = 'mDY'
		else if ( CGparam == 5 ) then
			CGname = 'PRP+'
		else if ( CGparam == 6 ) then
			CGname = 'HS+'
		else if ( CGparam == 7 ) then
			CGname = 'HZ'
		else if ( CGparam == 8 ) then
			CGname = 'HZ+'		
		end if 

		write(*,1000)
		write(*,1010) n, m
		write(*,1020) CGname
	end if
	
	! Check derivatives
	
	if ( checkder ) call checkd(n,m,x)
	
	! Scale problem
	
	call scalefactor(n,m,x,scaleF)	

	! Counters
	
	outiter = 0	
	nfev    = 0
	ngev    = 0
	nvev    = 0
	
	! Compute v(x)
	
	do ind = 1,m
		call sevalg(n,x,JF(ind,:),ind,informfg)
		ngev = ngev + 1
	end do
	
	call evalv(n,m,v,informALG)
	nvev = nvev + 1
	
	! Compute sup-norm of v(x)
	
	norm2v = norm2(v)
	
	! Compute f(x,v)
	
	fxv = maxval( matmul( JF,v ) )
	
	! Compute theta
	
	theta = fxv + norm2v ** 2 / two
	
	if ( iprint ) then
		write(*,1030) epsopt
		write(*,1040)
		write(*,1050) outiter,abs(theta),norm2v,informALG,nfev,ngev,nvev
	end if

	
	!---------------------------------------------------------------------  
	!     Main loop
	!---------------------------------------------------------------------     

	Main_loop: do
	
		!-------------------------------------------------------------------
		!     Test stopping criteria
		!-------------------------------------------------------------------
		
		! Test optimality		
		
		if ( abs( theta ) <= epsopt  ) then
			inform = 0
			
			! Stop timing
			
			call cpu_time(timefin)
			
			time = timefin - timeini
  
			if ( iprint ) then
				if ( outiter > 0 ) then
						if ( mod(outiter,10) .eq. 0 ) write(*,1040)
						write(*,1060) outiter,abs(theta),norm2v,informLS,informALG,restinf,nfev,ngev,nvev
				end if
		
				write(*,1070) nfev, ngev, nvev, time
			end if
			
			deallocate(d,v,xprev,dprev,vprev,phi0,gphi0,sF,JF,JFprev)
			return
		end if
		
		! Test whether the number of iterations is exhausted
		
		if ( outiter >= maxoutiter ) then
			inform = 1
			
			! Stop timing
		
			call cpu_time(timefin)
		
			time = timefin - timeini
				
			if ( iprint ) then
				if ( outiter > 0 ) then
					if ( mod(outiter,10) .eq. 0 ) write(*,1040)
					write(*,1060) outiter,abs(theta),norm2v,informLS,informALG,restinf,nfev,ngev,nvev
				end if
				write(*,1080) nfev, ngev, nvev, time
			end if
			
			deallocate(d,v,xprev,dprev,vprev,phi0,gphi0,sF,JF,JFprev)
			return
		end if
		
		!-------------------------------------------------------------------
		!			Prepare the iteration
		!-------------------------------------------------------------------
		
		! Check for restart
		
		restart = .false.

		if ( outiter > 0 .and. ( informLS /= 0 .or. informALG /= 0 ) ) restart = .true.
		
		! Update parameter beta
		
		if ( outiter > 0 .and. .not. restart ) then	
										
			if ( CGparam == 0 ) then 
				! Steepest descent method:
				
				beta = zero
				
			elseif ( CGparam == 1 ) then
				! FR method:
				
				! Compute f(x,v)			
				fxv = maxval( matmul( JF,v ) ) 
				
				! Compute f(xprev,vprev)
				fxprevvprev = maxval( matmul( JFprev,vprev ) ) 
				
				beta = fxv / fxprevvprev
				beta = 0.98d0 * beta
				
			elseif ( CGparam == 2 ) then
				! CD method:
				
				! Compute f(x,v)
				fxv = maxval( matmul( JF,v ) ) 

				! Compute f(xprev,dprev)
				fxprevdprev = maxval( matmul( JFprev,dprev ) ) 
									
				beta = fxv / fxprevdprev 					
				beta = 0.99d0 * ( 1.0d0 - gtol ) * beta
				
			elseif ( CGparam == 3 ) then
				! DY method:
				
				! Compute f(x,v)
				fxv = maxval( matmul( JF,v ) )
				
				! Compute f(x,dprev)	
				fxdprev =  maxval( matmul( JF,dprev ) ) 
				
				! Compute f(xprev,dprev)
				fxprevdprev =  maxval( matmul( JFprev,dprev ) ) 
									
				beta = - fxv / ( fxdprev - fxprevdprev )
				beta = 0.99d0 * ( ( one - gtol) / ( one + gtol ) ) * beta
				
			elseif ( CGparam == 4 ) then
				! modified DY method:
				
				! Compute f(x,v)
				fxv = maxval( matmul( JF,v ) )
				
				! Compute f(x,dprev)	
				fxdprev =  maxval( matmul( JF,dprev ) ) 
				
				! Compute f(xprev,dprev)
				fxprevdprev =  maxval( matmul( JFprev,dprev ) ) 
									
				beta = - fxv / ( fxdprev - 1.02d0 * fxprevdprev )
				
			elseif ( CGparam == 5 ) then
				! PRP+ method:
				
				! Compute f(xprev,vprev)
				fxprevvprev = maxval( matmul( JFprev,vprev ) ) 
				
				! Compute f(x,v)
				fxv = maxval( matmul( JF,v ) )
				
				! Compute f(xprev,v)
				fxprevv = maxval( matmul( JFprev,v ) )
				
				beta = ( - fxv + fxprevv ) / ( - fxprevvprev )
				beta = max( zero, beta )
				
			elseif ( CGparam == 6 ) then
				! HS+ method:	
				
				! Compute f(x,v)
				fxv = maxval( matmul( JF,v ) )
				
				! Compute f(xprev,v)
				fxprevv = maxval( matmul( JFprev,v ) )
				
				! Compute f(x,dprev)
				fxdprev =  maxval( matmul( JF,dprev ) ) 
				
				! Compute f(xprev,dprev)
				fxprevdprev = maxval( matmul( JFprev,dprev ) ) 

				beta = ( - fxv + fxprevv ) / ( fxdprev - fxprevdprev )
				beta = max( zero, beta )
				
			elseif ( CGparam == 7 .or. CGparam == 8 ) then
				! HZ method:
				
				! Define the HZparam
				!HZparam = two
				
				! Compute f(x,v)
				fxv = maxval( matmul( JF,v ) )
				
				! Compute f(xprev,v)
				fxprevv = maxval( matmul( JFprev,v ) )
				
				! Compute f(x,dprev)
				fxdprev =  maxval( matmul( JF,dprev ) ) 
				
				! Compute f(xprev,dprev)
				fxprevdprev = maxval( matmul( JFprev,dprev ) ) 
				
				! Compute f(xprev,vprev)
				fxprevvprev = maxval( matmul( JFprev,vprev ) ) 
				
				! Compute f(x,vprev)
				fxvprev = maxval( matmul( JF,vprev ) ) 
				
				beta = ( - fxv + fxprevv ) / ( fxdprev - fxprevdprev )       &
				 - HZparam * fxdprev * ( - fxv + fxprevv + fxvprev - fxprevvprev )/&
												  ( fxdprev - fxprevdprev )**2                         
												  
				if ( CGparam == 7 ) then
				
					! Compute eta
			
					eta = - one / ( norm2(dprev) * min( 1.0d-2,norm2(vprev) ) )
					
					beta = max( beta, eta )		
															
				elseif ( CGparam == 8 ) then     
					beta = max( zero, beta )	
				end if
				
			end if
				
		end if
		
		! Define the search direction
		
		if ( outiter == 0 .or. CGparam == 0 .or. restart ) then
			d = v
		else
			d = v + beta * dprev
		end if
		
		! Test if direction d satisfies the sufficient descent condition
		
		dsdc = .true.
		
		if ( outiter > 0 .and. .not. restart .and. ( CGparam == 1 .or.     &
		     CGparam == 5 .or. CGparam == 6 .or. CGparam == 7 .or.         &
		     CGparam == 8 ) ) then
		
				fxd = maxval( matmul( JF,d ) )
				fxv = maxval( matmul( JF,v ) )
				
				if ( fxd > 0.99d0 * ctol * fxv ) then
					dsdc = .false.
				end if
		end if
		
		! For the HZ method, correct the direction if it does not
		! satisfy the sufficient descent condition and beta is negative
		
		if ( CGparam == 7 .and. .not. dsdc .and. beta < zero ) then
							
			! Compute f(x,-dprev)

			fnegd = maxval( matmul( JF,-dprev ) )
			
			! Redefine beta and the search direction
			
			beta = ( one - ctol ) * fxv / fnegd
			
			d = v + beta * dprev
			
			dsdc = .true.
		end if

		!-------------------------------------------------------------------
		!     Iteration
		!-------------------------------------------------------------------
		
		if ( dsdc ) then
		
			! Identify a steepest descent iteration
			
			if ( outiter > 0 ) restinfprev = restinf
			
			if ( outiter == 0 .or. beta == 0.0d0 .or. restart ) then 
				restinf = 'Y'
			else 
				restinf = 'N'
			end if

			! Print information
	
			if ( iprint .and. outiter > 0 ) then
				if ( mod(outiter,10) .eq. 0 ) write(*,1040)
				write(*,1060) outiter, abs(theta), norm2v, informLS,       &
											informALG, restinfprev, nfev, ngev,nvev
			end if
			
			! Increment outiter
			
			outiter = outiter + 1
					
			! Compute initial step
			
			if ( outiter == 1 ) then
				stp = one/norm2v
				stp = max( stp , one    )
				stp = min( stp , stpmax )
			else
			
				fxd = maxval( matmul( JF,d ) )
				fxprevdprev = maxval( matmul( JFprev,dprev ) )
				
				stp = stp * fxprevdprev/fxd
				stp = max( stp , 1.0d-2 )
				stp = min( stp , stpmax, 1.0d2 )
			end if
			
			! Define the tolerances for the line search algorithm
			
			tolLS  = bignum
	
			! Compute phi0 = [phi_1(x),...,phi_m(x)] 
			
			do ind = 1, m
				call sevalf(n,x,phi0(ind),ind,informfg)
				nfev = nfev + 1   
			end do
			
			! Compute gphi0 = [gphi_1(x),...,gphi_m(x)]
			
			gphi0 = matmul( JF, d )		
					
		else
			! Compute f(x,dprev)

			fxdprev =  maxval( matmul( JF,dprev ) ) 
					
			! Define the tolerances for the line search algorithm
			
			tolLS = min( - 0.75d0 * ( one - ctol ) * fxv / beta, 0.5d0 * fxdprev )
			
			if ( tolLS < 1.0d-16 ) then
			
				! Force to restart
				
				informLS = 6
				cycle Main_loop
			end if
			
			! Retrieve previous data
	
			x  = xprev
			v  = vprev
			d  = dprev
			JF = JFprev		
				
		end if		
		
		! Compute the stepsize satisfying the Wolfe conditions
		
		call lsvecopt(evalphi,evalgphi,evalqstep,stp,stpmin,stpmax,m,mquad,&
					mnquad,quadratic,quadind,nquadind,phi0,gphi0,ftol, &
					gtol,iprintLS,nfevLS,ngevLS,outiterLS,totiniterLS,informLS,LStype,tolLS)  
					
		! Check for errors in lsvecopt
		
		if ( informLS == - 1 ) then
			inform = - 1
			
			deallocate(d,v,xprev,dprev,vprev,phi0,gphi0,sF,JF,JFprev)
			return
		end if
				
							  
		nfev = nfev + nfevLS
		ngev = ngev + ngevLS
		
		! Save previous data				
		
		xprev  = x
		vprev  = v
		dprev  = d
		JFprev = JF
		
		! Update x	
		
		x = x + stp * d
				
		! Compute v(x)
		
		do ind = 1,m
			call sevalg(n,x,JF(ind,:),ind,informfg)
			ngev = ngev + 1
		end do
		
		call evalv(n,m,v,informALG)
		nvev = nvev + 1
		
		! Compute sup-norm of v(x)
	
		norm2v = norm2(v)
		
		! Compute f(x,v)
	
		fxv = maxval( matmul( JF,v ) )
		
		! Compute theta
		
		theta = fxv + norm2v ** 2 / two
		
	!---------------------------------------------------------------------
	!     Iterate
	!---------------------------------------------------------------------    

	end do Main_loop

	!--------------------------------------------------------------------- 
	!     End of main loop
	!---------------------------------------------------------------------
	
	! Non-executable statements
	
	1000 format(/,1X,'------------------------------------------------------------',/,1X,& 
					 'NLCG: Non-linear conjugate grandient for vector optimization',/,1X,& 
					 '------------------------------------------------------------')
									 
	1010 format(/,1X,'Number of variables:',1X,I5,/,1X,'Number of functions:',1X,I5)	

	1020 format(/,1X,'CG parameter:',1X,4A)	
	
	1030 format(/,1X,'Optimality tolerance:',1X,1P,D7.1)
	
	1040 format(/,4X,'out',2X,'|theta|',4X,'norm(v)',3X,'LS',1X,'ALG',1X,'SD',1X,'#evalf',2X,'#evalg',2X,'#evalv')
	
	1050 format(1X,I5,3X,1P,D8.2,3X,1P,D8.2,3X,'-',2X,I1,3X,'-',1X,I6,2X,I6,2X,I6)
	
	1060 format(1X,I5,3X,1P,D8.2,3X,1P,D8.2,3X,I1,2X,I1,3X,1A,1X,I6,2X,I6,2X,I6)		
	
	1070 format(/,1X,'Flag of NLCG: solution was found',/,/,1X, &
							'Number of functions evaluations:               ',I6,/,1X, &
							'Number of derivatives evaluations:             ',I6/,/,1X, &
							'Number of vector steepest descent evaluations: ',I6/,/,1X, &
							'Total CPU time in seconds: ',0P,F8.2)						
	
	1080 format(/,1X,'Flag of NLCG: maximum of iterations reached',/,/,1X,&
							'Number of functions evaluations:               ',I6,/,1X, &
							'Number of derivatives evaluations:             ',I6/,/,1X, &
							'Number of vector steepest descent evaluations: ',I6/,/,1X, &
							'Total CPU time in seconds: ',0P,F8.2)

end subroutine NLCGVO
