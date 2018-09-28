program main
implicit none


integer :: n = 4, i, j
real, allocatable, dimension(:,:) ::  A, AAug
real, allocatable, dimension(:) :: x, y, xMat,B
real,allocatable,  dimension(:) :: coeff
real :: fact, nth, alpha, fun, r1, r2, mf

allocate(xMat(n))
allocate(x(n))
allocate(y(n))
allocate(coeff(n))

print *, "Read 4 data pts from file d2.txt\n Enter alpha: "
read (*,*) alpha

open(3, file="d2.txt")

do j=1, n
    read(3, *) x(j), y(j)
    xMat(j) = y(j)
end do

do i=2, n
    do j=n, i, -1
        xMat(j) = xMat(j)-xMat(j-1)
    end do

    coeff(i) = xMat(i)
end do

! Compute f(alpha now)
!alpha=2
fact=2
fun = y(1)+coeff(2)*alpha
mf = 1
do i=3, n
    mf = mf* (alpha-i+2)
    !print *,"mf ", mf

    r1 = alpha*mf/fact
    r2 = coeff(i)
    !print *, i
    !print *, r1
    !print *, r2
    fun = fun + r1*r2
    mf = mf/fact
    fact = i
end do

print *, "Interpolated value: ", fun

end program main