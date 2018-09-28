program mvrn
implicit none

real, external :: f1, f2, df1x1, df1x2, df2x1, df2x2

real, dimension(2,2) :: jacob, jInv
real, dimension(2) :: x, xN, f, c
real :: tol
integer:: lCtr = 0

print *, "Enter Tolerance"
read(*,*) tol

print *, "Enter x(1) "
read(*,*) x(1)
print *, "Enter x(2) "
read(*,*) x(2)

jInv(1,1) = 0
jInv(1,2) = 0
jInv(2,1) = 0
jInv(2,2) = 0

do while(lCtr < 10000)
    jacob(1,1) = df1x1(x)
    jacob(1,2) = df1x2(x)
    jacob(2,1) = df2x1(x)
    jacob(2,2) = df2x2(x)
    
    call inverse(jacob, jInv, 2)
    
    f(1) = f1(x)
    f(2) = f2(x)

    c = matmul(jInv, f)

    xN(1) = x(1) - c(1)
    xN(2) = x(2) - c(2)
    
    if((abs( xN(1)-x(1)) .lt. tol) .and. abs( xN(2)-x(2) ).lt. tol) exit
    
    x(1) = xN(1)
    x(2) = xN(2)

    lCtr = lCtr + 1 
end do 

print *, "Iteration Number: ", lCtr
print  *, "x1: ", x(1), "x(2): ", x(2)
    
end program mvrn
