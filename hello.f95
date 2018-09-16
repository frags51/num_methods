program blah
implicit none

    CHARACTER(len=10) :: name
    LOGICAL :: blagg
    blagg = .true.
    name = "Supreet"
    print *, "Here is Mr. ", name(2:2), blagg
end program blah