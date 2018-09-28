program main
implicit none

    real :: x, tol, xP
    integer:: lCtr = 0
    real, external :: funVal, funDval
    print *, "Enter X (guess) and tol"
    read(*,*) xP
    read(*,*) tol
    

    do while(lCtr < 10000)
        if(funDVal(xP) .eq. 0) then 
            exit
        end if
        
        x = xP - funVal(xP)/funDVal(xP)
        if(abs(x-xP) < tol) then
            
            exit
        end if
        
        xP = x
        lCtr = lCtr + 1
    end do
    
    print *, "Iteration Number: ", lCtr
    print  *, "x1: ", x
    

end program main