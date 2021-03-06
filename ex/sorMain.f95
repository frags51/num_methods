!Gauss Elimination

program main
    implicit none
    
        real, dimension(:,:), allocatable :: a !a is augmented matrix (A+b column in Ax=b)
        real, dimension(:), allocatable :: x
        real :: tol
    
        integer :: i=0 ! some loop conters
    
        integer :: n
        n = 5
        allocate(a(n, n+1))
        allocate(x(n))
    
        a(1,1) = 15
        a(1,2) = -1.0
        a(1,3) = 2.0
        a(1,4) = -3
        a(1,5) = 4
        a(1,6) = 8
        
        a(2,1) = 2.0
        a(2,2) = 23.0
        a(2,3) = -1.0
        a(2,4) = 5
        a(2,5) = -2
        a(2,6) = 82.4
        
        a(3,1) = -1.0
        a(3,2) = 3.0
        a(3,3) = 92.0
        a(3,4) = -5.0
        a(3,5) = 1.0  
        a(3,6) = -764.9  

        a(4,1) = 1.0
        a(4,2) = 2
        a(4,3) = 1.0
        a(4,4) = 27
        a(4,5) = 3  
        a(4,6) = -8.9

        a(5,1) = -4
        a(5,2) = -6
        a(5,3) = -2
        a(5,4) = 8
        a(5,5) = 41  
        a(5,6) = -201.9

        do i=1, n
        print *, "Enter X(",i,")th Guess: "
        read(*,*) x(i) 
        end do

        print *, "Enter Tolerance: "
        read (*,*) tol

        call sor(a, n, x, tol)
    	
        call printMatrix(x, n, 1)

    end program main
    
