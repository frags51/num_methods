!Successive Substitution -> Single Variable
program main
    implicit none
    
    real, dimension(2) :: x, f
    real:: tol
    integer :: lCtr = 0
    
    print *, "Enter X1 X3 (initial Guesses) and Tolerance (in new lines)"
    read(*,*) x(1)
    read(*,*) x(2)
    read(*,*) tol
    
    do while(lCtr < 10000)
        call fun(x, f)

        if((abs(x(1)-f(1))<tol .and. abs(x(2)-f(2)) < tol)) then
            exit
        end if
        x(1) = f(1)
        x(2) = f(2)
        lCtr = lCtr+1
    end do
    
    print *, "Iteration Number: ", lCtr
    print  *, "x1: ", x(1)
    print *, "x2: ", x(2)
    
    end program main