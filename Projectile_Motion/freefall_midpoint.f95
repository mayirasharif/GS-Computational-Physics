program freefall 

implicit none

real(8):: v, y, t, Tou, a, v0
integer(4):: i, nmax

a = -9.8d0

write(*,*) "Type below your initial velocity"
read(*,*) v
write(*,*) "Type below your initial position"
read(*,*) y
write(*,*) "Type in your Tou"
read(*,*) Tou
! write(*,*) "How many terms would you like?"
! read(*,*) nmax

t = 0.0d0

open (unit=100, file="pos_midpoint")
open (unit=200, file="v_midpoint")
open(unit=300, file="a_midpoint")
open(unit=400, file="v_old")
write(300,*) a

do while (y > 0)
	t = t + Tou
	v0 = v
	v = v0 + Tou*a
	y = y + Tou*(v0 + v/2.0)
	write (100, *) t, y
	write (200, *) t, v
	write(400, *) t, v
	end do
close(100)
close(200)
close(300)
end program freefall
