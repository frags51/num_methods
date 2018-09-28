real function funVal(x)
    implicit none
    real, intent(in)::x
    funVal = x - (1.0/3.0)*exp(x)
end function funVal

real function funDVal(x)
    implicit none
    real, intent(in) ::x 
    funDVal = 1 - (1.0/3.0)*exp(x)
end function funDVal