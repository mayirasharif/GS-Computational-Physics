program freefall 

implicit none

real(8):: v, y, t, Tou, a
integer(4):: i, nmax

a = -9.8d0

write(*,*) "Type below your initial velocity"
read(*,*) v
write(*,*) "Type in your initial position"
read(*,*) y
write(*,*) "Type in your Timestep(Tou)"
read(*,*) Tou
! write(*,*) "How many terms would you like?"
! read(*,*) nmax

t = 0.0d0

open (unit=100, file="position")
open (unit=200, file="velocity")
open(unit=300, file="acceleration")
write(300,*) a

do while (y > 0)
	t = t + Tou
	y = y + Tou*v
	v = v + Tou*a
	write (100, *) t, y
	write (200, *) t, v
	end do
write(*,*) "The object hit the ground in about: ", t, " seconds"
write(*,*) "The object's final velocity was: ", v, " m/s"
close(100)
close(200)
close(300)
end program freefall
