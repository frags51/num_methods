! Integer to character conversion, and vice-versa.
! ichar(char) -> char to int.
! char(int) -> int to char
program e
implicit none

    character :: c
    integer(kind = 1) :: a

    !a = 50
    c = 'J'
    a = ichar(c)
    print *, a
    print *, c

end program e