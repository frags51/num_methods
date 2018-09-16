!With Pivoting: make pivot element non zero.
subroutine tho(a, n, x)
    implicit none
    
        integer, intent(in) :: n
        real, dimension(n,n+1), intent(inout) :: a
        real, dimension(n), intent(inout) :: x
    
        real :: pivot, factor
        integer :: pivRow, pivCol, stepCount
        integer :: rowCount, colCount

        integer :: ic = 0, jc = 0, pRowCpy, pColCpy, temp2=0, i,j,k
        real :: max1 = 0, max2 = 0, temp=0 
        ! Need this to print them in correct order

        do k=2, n+1
            factor = a(1,1) ! Dont change a(1,1) --> factor will change
            a(1,k) = a(1,k)/factor ! divide by pivot
        end do
        a(1,1) = 1

        do j=2, n ! till second last row
            factor = a(j, j-1)
            do k=1, n+1
                if(.not. (k .eq. j-1)) then
                a(j,k) = a(j,k) - factor*a(j-1,k) ! divide by pivot
                end if
                a(j, j-1) = 0
            end do

            factor = a(j,j)
            do k=j, n+1
                if(.not. (k .eq. j)) then
                a(j,k) = a(j,k)/factor  ! divide by pivot
                end if
                a(j,j)=1
            end do

        end do !j
        
        ! Now Getting X
        x(n) = a(n,n+1)
        do i=n-1, 0, -1
            x(i) = a(i, n+1) - a(i,i+1)*x(i+1)
        end do

end subroutine tho
    
