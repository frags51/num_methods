subroutine gSiedel(a, n, x, tol)
	implicit none

    integer, intent(in) :: n
    real, dimension(n,n+1), intent(inout) :: a
    real, dimension(n), intent(inout) :: x
    real, intent(in) :: tol

    real, dimension(n) :: xP ! Stores previous values of x.

    integer :: ic = 0, jc = 0, k=0, i=0,j=0, flag=1, lCtr = 0 
    real :: sum1 = 0

    !initialize xP
    do i=1, n
    	xP(i) = x(i)
    end do


    do lCtr = 1, 10000
    	do i=1, n
    	sum1 = 0
    	do j=1,n
    		if (.not. j .eq. i) then
    		sum1 = sum1 + x(j)*a(i, j) ! Use new x's value here only
    		end if
    	end do
    	x(i) = (a(i, n+1) -sum1)/a(i,i) 
    	end do
    	
    	! Now check for tolerance
    	flag = 1
   		do i = 1, n
   			if(( .not. (xP(i) .eq. 0) ) .and. (abs((xP(i)-x(i))/xP(i)) .gt. tol)) then 
   				flag = 0
   			end if
   		end do

   		if(flag .eq. 1) then
   			exit
   		end if
   		!else, continue this procedure.
   		! rest xP's to x's

   		do i=1, n
    		xP(i) = x(i)
    	end do
    end do !lCtr loop
    print *, "Loop Counter: ", lCtr

end subroutine