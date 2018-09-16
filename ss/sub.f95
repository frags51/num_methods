subroutine fun(x,f)

    implicit none
    real, intent(inout):: x, f
    f = exp(x)/3.0

end subroutine fun