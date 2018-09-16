!Gauss Elimination

program main
implicit none

    real, dimension(:,:), allocatable :: a !a is augmented matrix (A+b column in Ax=b)
    real, dimension(:), allocatable :: x

    integer :: i=0 ! some loop conters

    integer :: n
    n = 3
    allocate(a(n, n+1))
    allocate(x(n))

    a(1,1) = 2.0
    a(1,2) = 1.0
    a(1,3) = 0.0
    a(1,4) = 1.0
    
    a(2,1) = 1.0
    a(2,2) = 2.0
    a(2,3) = 1.0
    a(2,4) = 2.0
    
    a(3,1) = 0.0
    a(3,2) = 1.0
    a(3,3) = 1.0
    a(3,4) = 4.0  
    
    call gE(a, n, x)

    !call printMatrix(a, n, n+1)
end program main

