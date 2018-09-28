program main
implicit none

integer :: n = 7, i, j
real, allocatable, dimension(:,:) :: xMat, xMatT, A, AAug
real, allocatable, dimension(:) :: xDash, y, B
real,allocatable,  dimension(:) :: coeff

allocate(xMat(n, 2))
allocate(xMatT(2, n))
allocate(A(2, 2))
allocate(AAug(2, 3))

allocate(B(2))
allocate(coeff(2))

allocate(xDash(n))
allocate(y(n))


open(3, file="data.txt")

do j=1, n
    read(3, *) xDash(j), y(j)
end do

do i=1,n
    xMat(i, 1) = 1
    xMat(i, 2) = xDash(i)
end do

xMatT = TRANSPOSE(xMat)

A = MATMUL(xMatT, xMat)

B = MATMUL(xMatT, y)

! Use Gauss Elimintation now

do i = 1, 2
    do j=1, 2
        AAug(i,j) = A(i,j)
    end do
end do

do i =1, 2
    AAug(i,3)= B(i)
end do

call lu(AAug, 2, coeff)

call printMatrix(coeff, 2, 1)

end program main