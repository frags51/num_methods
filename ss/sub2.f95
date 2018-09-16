subroutine fun(x,f)

    implicit none
    real, dimension(2), intent(inout):: x, f
    f(1) = (-2*x(1)**3 -8*x(1)+4*x(2)+4)/20.0 + x(1)
    f(2) = (-4*x(1)+x(2)**2 + 3*x(2)+1)*(-1/24.0) + x(2)

    

end subroutine fun