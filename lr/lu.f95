subroutine lu(a, n, x)
      implicit none

      integer, intent(in) :: n
      real, dimension(n,n+1), intent(inout) :: a
      real, dimension(n), intent(inout) :: x


      real, dimension(n,n) :: c
    real :: pivot, factor
    integer :: pivRow, pivCol, stepCount
    integer :: rowCount, colCount
    integer :: ic = 0, jc = 0, pRowCpy, pColCpy, temp2=0
    real :: max1 = 0, max2 = 0, temp=0, sum=0 
    integer, dimension(n) :: mapper ! If cols are changed to make pivot non-zero, Xi's will also be shuffled.

      real, dimension(n,n) :: u, l
      real, dimension(n) ::  y

      integer::i, j, k! counters

      do i=1, n
            y(i) = 0.0
      end do
      do i=1, n
            do j=1, n
                  l(i,j) = 0
                  u(i,j) = 0
            end do
      end do

   !!!!!!!!!!!1 FIX PIVOT
      do ic = 1, n
            mapper(ic) = ic
      end do
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

      end do ! step Count

      ! Fix a col and get U's and L's
      do i=1, n
            do k=i, n

                  sum = 0
                  do j=1, i
                        sum = sum + l(i,j)*u(j,k)
                  end do
                  u(i,k) = a(i,k) - sum
            end do

            do k=i, n
                  if(i .eq. k) then
                        l(i,i) = 1
                  else 
                        sum = 0
                        do j=1, i
                              sum = sum + l(k,j)*u(j,i)
                        end do
                        l(k,i) = (a(k,i)-sum)/u(i,i)
                  end if
            end do
           
      end do

      ! Now forward sweep for Ly = B



      do i = 1, n, 1
            sum = 0
            do j = 1,i-1,1
                sum = sum + l(i, j)*y(j)
            end do
            y(i) = (a(i, n+1) - sum)/l(i,i)
            sum = 0
        end do 

      ! Now backward sweep for Ux = y
      do i = n, 1, -1
            sum = 0
            do j = i+1, n, 1
                  sum = sum + u(i,j)*x(j)
            end do
            x(i) = (y(i)-sum)/u(i,i)
      end do 

      ! Replace elements exchanged due to pivoting
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