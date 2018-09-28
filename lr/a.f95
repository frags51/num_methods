subroutine sumX(x, n, res)
implicit none

integer, intent(in) :: n
real, dimension(n), intent(in) :: x 
real, intent(out) :: res


integer:: i
res = 0
do i=1, n
res = res+x(i)
end do

end subroutine sumX

subroutine sumX2(x, n, res)
    implicit none
    
    integer, intent(in) :: n
    real, dimension(n), intent(in) :: x 
    real, intent(out) :: res
    
    integer:: i
    res = 0
    do i=1, n
    res = res+ x(i)*x(i)
    end do
    
end subroutine sumX2

subroutine sumXY(x,y, n, res)
    implicit none
    
    integer, intent(in) :: n
    real, dimension(n), intent(in) :: x, y 
    real, intent(out) :: res
    
    integer:: i
    res = 0
    do i=1, n
    res = res+x(i)*y(i)
    end do
    
end subroutine sumXY
    