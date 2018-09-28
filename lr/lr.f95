program main
implicit none

real :: a1, a0, r1, r2, r3, r4
integer :: n, j
real, dimension(:), allocatable :: x, y
n = 7
allocate(x(n))
allocate(y(n))

open(3, file="data.txt")

do j=1, n
    read(3, *) x(j), y(j)
end do

close(3)

call sumXY(x, y, n, r1)
call sumX(x, n, r2)
call sumX(y, n, r3)
call sumX2(x, n, r4)

a1 = (n*r1 - r2*r3)/(n*r4 - r2*r2)
a0 = -1 * (a1*r2 - r3) * (1.0/n)

print *, "y = ", a0, " + ", a1, "*x"


end program main