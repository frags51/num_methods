!Successive Substitution -> Single Variable
program main
implicit none

real :: x, tol, f
integer :: lCtr = 0

print *, "Enter X (initial Guess) and Tolerance"
read(*,*) x
read(*,*) tol

do while(lCtr < 10000 .or. (abs(exp(x)/3 - x)>tol))
    call fun(x, f)
    x = f
    lCtr = lCtr+1
end do

print *, x

end program main