subroutine lu(a, n, x)
      implicit none

      integer, intent(in) :: n
      real, dimension(n,n+1), intent(inout) :: a
      real, dimension(n), intent(inout) :: x

      real, dimension(n,n) :: u, l
      real, dimension(n) ::  y

      integer :: i,j,k,l ! counters

      do i=1, n
            y(i) = 0.0
      end do
      do i=1, n
            do j=1, n
                  l(i,j) = 0
                  u(i,j) = 0
            end do
      end do

      ! Fix a col and get U's and L's
      do j=1, n
            do i =1 , n
                  if(i <= j) then
                        u(i,j) = a(i,j)
                        do k=1, i-1
                              u(i,j) = u(i,j) - l(i,k)*u(k,j)
                        end do
                        if(i .eq. j) then
                              l(i,j) = 1
                        end if
                  else 
                        l(i,j) = a(i,j)
                        do k=1 , j ! NOTE THE INDEX HERE AND ABOvE
                              
                        end do
                  end if

end subroutine