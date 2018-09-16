program gg
implicit none

    real, dimension(:,:), allocatable :: a

    integer a1, a2, i, j
    read (*,*) a1, a2
    allocate(a(a1, a2))
    do i = 1, a1
        do j = 1, a2
            a(i,j) = i*j
        end do
    end do
    print "(3f10.2)",  a    

    deallocate(a)
end program gg