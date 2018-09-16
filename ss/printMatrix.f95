
subroutine printMatrix(a, r, c)
    implicit none
    
    integer, intent(in) :: r, c
    real, dimension(r,c) :: a
    integer :: rc, cc
    
    10 format(f7.2)
    
    do rc = 1, r
        do cc = 1, c
            write(*,10,advance='no') a(rc,cc)
        end do
        write(*, *)
    end do
    
    end subroutine printMatrix