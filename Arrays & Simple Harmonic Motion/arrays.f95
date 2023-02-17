program array

implicit none 

integer, parameter :: dp = kind(1.0d0) ! defining double precision
integer(4)::i,j,n
real(dp):: y(3), z(10) !number represents the amount of values put into 
real(dp), dimension(3,3):: a, b !one number: one dimensional array, two number: two-dimensional array (3 by 3)
real(dp), allocatable, dimension(:):: x ! you can tell it to be a one-dimensional array, telling the computer to allocate rray for later

y = (/1.0, 2.0, 3.0 /)
write(*,*) y

z = (/ (i, i=1,10)/)
write(*,*) z

n = 100

allocate(x(n)) !n, n if its multidimensional

do i = 1, n
	x(i) = 1.0*i
	write(*,*) x(i)
end do

do i=1,3
	do j = 1,3
	a(i,j) = 1.0*i*j
	b(i,j) = 1.0*(i-j)
	end do
  end do

do i = 1, 3 
    write(*,*) a(i,1), a(i, 2), a (i,3)
    end do

end program array
  
