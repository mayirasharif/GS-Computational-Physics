program freefall_airdrag

implicit none

real(8):: v, y, t, Tou, a, v0

a = (-1.0/2.0*(0.45)*(.00426)*1.23*abs(v)*v)/0.145 - 9.81

write(*,*) "Type below your initial velocity"
read(*,*) v
write(*,*) "Type below your initial position"
read(*,*) y
write(*,*) "Type in your Tou"
read(*,*) Tou
! write(*,*) "How many terms would you like?"
! read(*,*) nmax

t = 0.0d0

open (unit=100, file="pos_airdrag.dat")
open (unit=200, file="v_airdrag.dat")
open(unit=300, file="a_airdrag.dat")

do while (y > 0)
	a = (-1.0/2.0*(0.45)*(.00426)*1.23*abs(v)*v)/0.145 - 9.81
	t = t + Tou
	v0 = v
	v = v0 + Tou*a
	y = y + Tou*(v0 + v/2.0)
	write (100, *) t, y
	write (200, *) t, v
	write(300, *) t, a
	end do
close(100)
close(200)
close(300)
end program freefall_airdrag
