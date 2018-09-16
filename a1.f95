program main
implicit none

    real :: a, b , c
    real, external :: sumOf
    logical,external :: eqq
    read (*, *) b, c

    a = sumOf(c, b)

    write(*, "(f5.2 )") a
    print *, eqq(c, b)
end program main