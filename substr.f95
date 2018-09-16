! program to find if a given string contains another substring
! use len_trim

program substr
implicit none

    character(len=10) :: str, sub
    integer :: i = 1
    character :: c
    LOGICAL :: found = .false.
    read(*,*) str, sub

    print *, LEN_TRIM(sub)
    do while(i<=(LEN_TRIM(str)-LEN_TRIM(sub)))
        c = str(i:i)
        if((c .eq. sub(1:1)) .and. (sub .eq. str(i:LEN_TRIM(sub)))) then 
            found = .true. 
        end if
        i = i+1
    end do 

    if(found .eqv. .true.) then 
        print *, "Is substr"
    else 
        print *, "Not substr"
    end if 
end program substr