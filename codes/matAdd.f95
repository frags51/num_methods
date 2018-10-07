subroutine addMat(N, A, B, RES)
	implicit none

	integer, intent(in) :: N
	real, dimension(N), intent(in) :: A, B
	real, dimension(N), intent(out) :: RES

	integer i, j
	do i=1, N
		do j=1, N
			RES(i,j) = A(i,j)+B(i,j)
		end do
	end do

end subroutine addMat
