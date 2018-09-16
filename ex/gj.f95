subroutine gJordan(a, n, x)
	implicit none

    integer, intent(in) :: n
    real, dimension(n,n+1), intent(inout) :: a
    real, dimension(n), intent(inout) :: x

    real :: pivot, factor
    integer :: pivRow, pivCol, stepCount
    integer :: rowCount, colCount
    integer :: ic = 0, jc = 0, pRowCpy, pColCpy, temp2=0, k=0, i=0,j=0
    real :: max1 = 0, max2 = 0, temp=0 
    integer, dimension(n) :: mapper ! If cols are changed to make pivot non-zero, Xi's will also be shuffled.
    ! Need this to print them in correct order
    do ic = 1, n
        mapper(ic) = ic
    end do
    	 ! row LOop
        do stepCount = 1, n, 1
            pivRow = stepCount
            pivCol = stepCount
            pivot = a(pivRow, pivCol)
            
            ! pivot is zero -> Swapping etc
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

            !actual row ops
            pivot = a(pivRow, pivCol)
            temp = 0.0
            ! First do col1, then col2, and so on....

            	do colCount = 1, n
            		if (.not. (stepCount .eq. colCount)) then
            			temp = a(colCount, stepCount)/a(stepCount, stepCount)
            			do k = 1, n+1 ! Sweep
            				a(colCount, k) = a(colCount, k) - temp*a(stepCount, k)
            			end do
            		endif
            	end do
            

            ! Make Diagonal elements as unity
            !do rowCount=1, n
            !	a(rowCount, n+1) = a(rowCount, n+1)/a(rowCount, rowCount)
            !end do


            ! Swap back any X's that were changed due to pivoting

    
        end do ! Step Count    

            ! Put values of X
            do rowCount=1, n
            	x(rowCount) = a(rowCount, n+1)/a(rowCount, rowCount)
            end do

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
end subroutine