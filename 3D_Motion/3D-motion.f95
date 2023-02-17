program motion_in_3d

implicit none
! initializing all the variables and arrays needed
integer, parameter :: dp = kind(1.0d0)
real(dp):: x, y, z, t, Tau, g, pi, degree, theta, voldone, voldthree, max_height, old_height, mass
integer(4):: i, nmax, n
real(dp), allocatable, dimension(:):: r, v, a, adrag, agrav


! making pi
pi =4.0*atan(1.0)

write(*,*) "Write in Tau"
read(*,*) Tau

n = 3
! allocating the stuff
allocate (r(n), v(n), a(n), adrag(n), agrav(n))

write(*,*) "put in a degree"
read(*,*) degree

write(*,*) "put in a velocity"
read(*,*) v

agrav(1) = 0.0
agrav(2) = 0.0
agrav(3) = 9.81
! adrag(1) = -(1.0/2.0)*(0.45)*(.00426)*1.23*abs(v(1))*v(1)/0.145
! adrag(2) = -(1.0/2.0)*(0.45)*(.00426)*1.23*abs(v(2))*v(2)/0.145
! adrag(3) = -(1.0/2.0)*(0.45)*(.00426)*1.23*abs(v(3))*v(3)/0.145

! air drag 
adrag = -(1.0/2.0)*(0.45)*(.00426)*1.23*abs(v(3))*v(3)/0.145

a(1) = adrag(1) + agrav(1)
a(2) = adrag(2) + agrav(2)
a(3) = adrag(3) + agrav(3)

t = 0.0

!theta = degree*(pi/180.0)
!x(1) = 0.0
!y(1) = 0.0
!z(1) = 1.0
!vx(1) = 20.0*cos(theta)
!vy(1) = 0.0
!vz(1) = 20.0*sin(theta)
!v(3) = z(1)
!theta(1) = degree*(pi/180.0)

! calculating the degree in radians
theta = degree*(pi/180.0)

! position stuff
r(1) = 0.0
r(2) = 0.0
r(3) = 1.0


! velocity stuff
v(1) = 20.0*cos(theta)
v(2) = 0.0
v(3) = 20.0*sin(theta)



agrav(1) = 0.0
agrav(2) = 0.0
agrav(3) = 9.81
adrag(1) = -(1.0/2.0)*(0.45)*(.00426)*1.23*abs(v(1))*v(1))/0.145
adrag(2) = -(1.0/2.0)*(0.45)*(.00426)*1.23*abs(v(2))*v(2)/0.145
adrag(3) = -(1.0/2.0)*(0.45)*(.00426)*1.23*abs(v(3))*v(3)/0.145
adrag = (-.5*.45*.00426*1.23*abs(v)*v)/mass

! s = sqrt(v(1)**2 + v(2)**2 + v(3)**2)


! acceleration stuff
!a(1) = -(1.0/2.0)*(0.45)*(.00426)*1.23*abs(v(1))*v(1)/0.145
!a(2) = 0.0
!a(3) = -(g) + -(1.0/2.0)*(0.45)*(.00426)*1.23*abs(v(3))*v(3)/0.145


voldone = 0.0
voldthree = 0.0

open (unit=100, file="x-pos.dat")
open (unit=200, file="z-pos.dat")
open(unit=300, file="x-v.dat")
open(unit=400, file="z-v.dat")
open(unit=500, file="a.dat")
write(500,*) a(3)

do while (r(3) > 0.0)
	t = t + Tau
	voldone = v(1)
	voldthree = v(3)
	old_height = r(3)
	v(3) = voldthree + Tau*a(3)
	r(1) = r(1) + Tau*(voldone + v(1)/2.0)
	r(3) = r(3) + Tau*(voldthree + v(3)/2.0)
	v(1) = voldone + Tau*a(1)
	adrag = -(1.0/2.0)*(0.45)*(.00426)*1.23*abs(v(3))*v(3)/0.145
	a(1) = adrag(1) - agrav(1)
	a(2) = adrag(2) - agrav(2)
	a(3) = adrag(3) - agrav(3)
	if (old_height < r(3)) then
	max_height = r(3)
	end if
	write (100, *) r(1), r(2), r(3)
	write(300, *) v(1), v(2), v(3)
	write(400, *) a(1), a(2), a(3)
	end do
	
	
	
write(*,*) "The ball stopped at: ", t, "seconds"
write(*,*) "The ball's final velocity was", v(1)
write(*,*) "range:", r(1)
write(*,*) "Max height: ", max_height

	
	
	
	
	
	
	
	
		!r(1) = r(1) + v(1)*Tau
	!r(3) = r(3) + v(3)*Tau
	
	
!	x0 = x(i - 1)
!	x(i) = x0 + Tou*a
!	z(i) = z(i - 1) + Tou*(v0 + v(i)/2.0)
!	r(i) = r(i - 1) + Tau*v(1)
!	vx(i) = vx(i - 1)*cos(theta)
!	write (100, *) t, r(1)
!	write (200, *) t, r(3)
!	write(400, *) t, v
!	end do
close(100)
close(200)
close(300)
close(400)
end program motion_in_3d
