real function lagr(n, X, Y, pt)
implicit none

	integer , intent(in):: n
	real , intent(in):: pt
	real, dimension(n) :: X, Y

	real :: val, ans=0

	integer:: i, j


	do i=1, n
		val=1
		do j=1,n
			if(.not. i .eq. j) then
				val=val*(pt-X(j))
				val = val/(X(i)-X(j))
			end if
		end do
		val = val*Y(i)
		ans = ans+val
	end do

	lagr = ans
end function lagr