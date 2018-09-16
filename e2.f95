real function blah(a, b)
    implicit none
    real, intent(in) :: a, b
    logical, external :: eqq

    real(kind=8) :: PI=4.D0*DATAN(1.D0)
    if(eqq(a, b) .eqv. (.true.)) then
        blah = COS((pi/180)*A) * EXP(b)
    else 
        blah = SIN((pi/180)*A) * (b**2)
    end if

end function blah