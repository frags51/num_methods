real function f1(x)
    implicit none
    real, dimension(2), intent(inout) :: x

    f1 = 4 - 8*x(1) +4*x(2) -2*(x(1)**3)
end function f1

real function f2(x)
    implicit none
    real, dimension(2), intent(inout) :: x

    f2 = 1 - 4*x(1) + 3*x(2) + (x(2)**2)
end function f2

real function df2x1(x)
    implicit none
    real, dimension(2), intent(inout) :: x

    df2x1 = -4
end function df2x1

real function df2x2(x)
    implicit none
    real, dimension(2), intent(inout) :: x

    df2x2 = 3 + (2*x(2))
end function df2x2

real function df1x1(x)
    implicit none
    real, dimension(2), intent(inout) :: x

    df1x1 = -8 - 6*(x(1)**2)
    
end function df1x1

real function df1x2(x)
    implicit none
    real, dimension(2), intent(inout) :: x

    df1x2 = 4
    
end function df1x2


