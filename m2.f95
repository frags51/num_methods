program main
implicit none

    real :: a, b
    real, external :: blah

    read (*,*) a, b

    print *, blah(a,b)

end program main