program shm_arrays

implicit none
! this program solves simple harmonic motion problems using arrays.
integer, parameter :: dp = kind(1.0d0)
integer, parameter :: sp = kind(1)
integer(4) :: i, j, n
real(dp):: m, Tau, w, k
real(dp), allocatable, dimension(:):: x, v, ke, E, U, a, t !allocate them later

write(*,*) "Type in your Tau"
read(*,*) Tau

! this allows the user to input the value for the spring stiffness constant.

write(*,*) "Put in n"
read(*,*) n
allocate(x(n), v(n), a(n), ke(n), E(n), U(n), t(n))



! initializing the stuff
m = 1.0
k = 1.0
w = sqrt(k/m)
x(1) = 1.0
v(1) = 0.0
a(1) = -(w**2.0)*x(1)
t(1) = 0
ke(1) = 0.5*m*v(1)**2.0
U(1) = 0.5*k*x(1)**2.0
E(1) = ke(1) + U(1)


do i=2, n-1
	t(i) = (i-1)*Tau 
	a(i) = -w**2.0*x(i - 1)
	v(i) = v(i-1) + Tau*a(i-1)
	x(i) = x(i-1) + ((v(i) + v(i-1))/2)*Tau
	ke(i) = 0.5*m*v(i)**2.0
	U(i) = 0.5*k*x(i)**2.0
	E(i) = ke(i) + U(i)
	end do

open(100, file="kinetic-energy.dat")

open(101, file="potential-energy.dat")

open(102, file="total-energy.dat")

open(103, file="x-array.dat")

open(104, file="velocity-array.dat")

open(105, file="w.dat")

open(106, file="a.dat")

do i=2,n-1 
	write(103,*) t(i), x(i)
	write(100,*) t(i), ke(i)
	write(101,*) t(i), U(i)
	write(106,*) t(i), a(i)
	write(102,*) t(i), E(i)
	write(104,*) t(i), v(i)
	end do
close(100)
close(101)
close(102)
close(103)
close(104)
close(105)
close(106)

end program shm_arrays


