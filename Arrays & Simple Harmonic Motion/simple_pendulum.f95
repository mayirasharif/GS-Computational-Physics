program shm_arrays_verlet

implicit none

! this program solves simple harmonic motion problems using arrays.
integer, parameter :: dp = kind(1.0d0)
integer, parameter :: sp = kind(1)
integer(4) :: i, j, n
real(dp):: m, Tau, k, g, length, pi, degree, Inertia
real(dp), allocatable, dimension(:):: x, w, ke, E, U, t, theta, alpha, h ! allocate them later

write(*,*) "Type in your Tau"
read(*,*) Tau

! this allows the user to input the value for the spring stiffness constant.

pi = 4.0*atan(1.0)


! Inertia == mass x length squared 

write(*,*) "Put in n"
read(*,*) n

write(*,*) "Put in your degree"
read(*,*) degree
allocate(theta(n), w(n), alpha(n), ke(n), E(n), U(n), t(n), h(n))

! initializing the stuff
	length = 1.0
	g = 9.8
	theta(1) = degree*(pi/180.0)
	alpha(1) = -1.0*(g/length)*sin(theta(1))
	h(1) = length*(1.0 - cos(theta(1)))
	m = 1.0
	Inertia = m*length**2.0
	w(1) = 0.0
	theta(2) = theta(1) + Tau*w(1) + (1.0/2.0)*(Tau**2.0)*alpha(1)
	t(1) = 0.0
	ke(1) = 0.5*Inertia*w(1)**2.0
	U(1) = m*g*length*(1.0-cos(theta(1)))
	E(1) = ke(1) + U(1)


do i=2, n-1
	t(i) = t(i-1)+Tau 
	alpha(i) = -1.0*(g/length)*sin(theta(i))
	theta(i + 1) = 2.0*theta(i) - theta(i-1) + (Tau**2.0)*alpha(i)
	w(i) = (theta(i+1) - theta(i-1))/(2.0*Tau)
	ke(i) = 0.5*Inertia*(w(i)**2.0) 
	U(i) = m*g*length*(1.0-cos(theta(i)))
	E(i) = ke(i) + U(i) !change these 
	end do

open(100, file="ke-pendulum.dat")

open(101, file="pe-pendulum.dat")

open(102, file="E-pendulum.dat")

open(103, file="theta-pendulum.dat")

open(104, file="w-pendulum.dat")


open(106, file="a-pendulum.dat")

do i=2,n-1
	write(103,*) t(i), theta(i)
	write(100,*) t(i), ke(i)
	write(101,*) t(i), U(i)
	write(106,*) t(i), alpha(i)
	write(102,*) t(i), E(i)
	write(104,*) t(i), w(i)
	end do
close(100)
close(101)
close(102)
close(103)
close(104)
close(105)
close(106)

end program shm_arrays_verlet
