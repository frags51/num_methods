subroutine gE(a, n, x)
implicit none

    integer, intent(in) :: n
    real, dimension(n,n+1), intent(inout) :: a
    real, dimension(n), intent(inout) :: x

    real :: pivot, factor
    integer :: pivRow, pivCol, stepCount
    integer :: rowCount, colCount

    do stepCount = 1, n-1, 1
        pivRow = stepCount
        pivCol = stepCount
        pivot = a(pivRow, pivCol)

        do rowCount = pivRow+1, n 
            factor = a(rowCount, pivCol)/pivot

            do colCount = pivCol, n+1
                a(rowCount, colCount) = a(rowCount, colCount) - factor*a(pivRow, colCount)
            end do ! c

        end do

    end do

    call backSub(a, n, x)

end subroutine gE

subroutine backSub(a, n, x)
implicit none

    integer, intent(in) :: n
    real, dimension(n,n+1), intent(inout) :: a
    real, dimension(n), intent(inout) :: x

    integer :: rowCount, colCount
    real::summation

    x(n) = a(n,n+1)/a(n,n)

    do rowCount = n-1, 1, -1
        do colCount = n+1,1,-1
            summation = summation + a(rowCount, colCount)*x(colCount)
        end do
        x(rowCount) = (a(rowCount, n+1) - summation)/a(rowCount, rowCount)
        summation = 0
    end do 

    !print
    do rowCount=1, n
        print *, x(rowCount)
    end do
end subroutine backSub
