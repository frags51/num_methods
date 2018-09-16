!With Pivoting: make pivot element non zero.
subroutine gE(a, n, x)
    implicit none
    
        integer, intent(in) :: n
        real, dimension(n,n+1), intent(inout) :: a
        real, dimension(n), intent(inout) :: x
    
        real :: pivot, factor
        integer :: pivRow, pivCol, stepCount
        integer :: rowCount, colCount

        integer :: ic = 0, jc = 0, pRowCpy, pColCpy, temp2=0
        real :: max1 = 0, max2 = 0, temp=0 
        integer, dimension(n) :: mapper ! If cols are changed to make pivot non-zero, Xi's will also be shuffled.
        ! Need this to print them in correct order
 
        do ic = 1, n
            mapper(ic) = ic
        end do

        do stepCount = 1, n-1, 1
            pivRow = stepCount
            pivCol = stepCount
            pivot = a(pivRow, pivCol)
            
            ! pivot is zero
            if(pivot .eq. 0) then
                ic = pivRow
                jc = pivCol
                pRowCpy = pivRow
                pColCpy = pivCol
                do ic = pivRow+1, n ! Iterate over Rows
                    if(ABS(a(ic, pivCol)) > ABS(a(pRowCpy, pivCol))) then
                        pRowCpy = ic !Iterating over rows only.
                    end if ! ABS Compare
                    max1 = a(pRowCpy, pivCol)
                end do
                
                do jc = pivCol+1, n ! Iterate over Cols
                    if(ABS(a(pivRow, jc)) > ABS(a(pivRow, pColCpy))) then
                        pColCpy = jc !Iterating over rows only.
                    end if ! ABS Compare
                    max2 = a(pivRow, pColCpy)
                end do

                if(abs(max1)>abs(max2)) then                
                    ! Swap max1's row and pivot Row
                    ic = 1
                    jc = 1
                    do jc = 1, n+1 ! Swap each element
                        temp = a(pRowCpy, jc)
                        a(pRowCpy, jc) = a(pivRow, jc)
                        a(pivRow, jc) = temp
                    end do  !Swap
                else  ! need to swap cols
                    ic = 1
                    jc = 1

                    do ic = 1, n !Swap Each ealemt
                        temp = a(ic, pColCpy)
                        a(ic, pColCpy) = a(ic, pivCol)
                        a(ic, pivCol) = temp
                        
                    end do
                    
                    temp = mapper(pColCpy)                    
                    mapper(pColCpy) = mapper(pivCol)
                    mapper(pivCol) = pColCpy
                    
                end if ! CHange row
                
            end if ! pivot equals zero
            pivot = a(pivRow, pivCol)
            do rowCount = pivRow+1, n 
                factor = a(rowCount, pivCol)/pivot
    
                do colCount = pivCol, n+1
                    a(rowCount, colCount) = a(rowCount, colCount) - factor*a(pivRow, colCount)
                end do ! c
    
            end do
    
        end do
    
        call backSub(a, n, x)
    
        ! Now change back x's : Their order was changed When Changing Columns..
        do ic=1, n
            if(.not. (mapper(ic) .eq. ic)) then
            
                temp = x(mapper(ic))
                temp2 = mapper(ic)
                x(temp2) = x(ic)
                x(ic) = temp
                mapper(ic) = ic
                mapper(temp2) = temp2
            end if
        end do

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

