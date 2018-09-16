
program add
implicit none

    integer:: res = 1
    integer:: i = 1
    
    do while(i<6)
        res = res*i
        print "(i3)", res
        i = i+1
    end do
end program add