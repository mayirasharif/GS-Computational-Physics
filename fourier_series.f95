program fourier_series

implicit none
! define variables 
real(8):: x, y, term, pi
integer(4):: i, nmax
pi = 4.0*atan(1.0)

write(*,*) "Enter the number of terms"
read(*,*) nmax

open (unit=100,file='sawtooth.dat') 

!exact solution

do while (x <= 2.0*pi)
	if (x < pi) then 
	y = x
	else if (x >= pi) then
	y = x - 2.0*pi
	end if
	write (100,*) x, y
	x = x + 0.1
end do
close(100)

x = 0.0

! Fourier Series approx
open (unit=200,file='approx.dat')
do while (x <= 2.0*pi)
	y = 0.0
	do i = 1, nmax
	term = ((-1.0)**(i + 1.0)) * (2.0/i)*(sin(i*x))
	y = y + term
	!write(*,*) x,y
	end do
	write(200,*) x, y
	x = x + 0.1

end do
close(200)

end program fourier_series


	
