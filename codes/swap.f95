subroutine swap(A, B)
implicit none
	
	real, intent(inout) :: A, B

	real:: temp

	A=temp
	A=B
	B=temp

end subroutine swap