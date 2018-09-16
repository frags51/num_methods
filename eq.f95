logical function eqq(a, b)
    implicit none
    real, intent(in) :: a, b
    if((a>0 .and. b>0) .or. (a<0 .and. b<0)) then 
        eqq = .true.
    else 
        eqq = .false.
    end if
end function