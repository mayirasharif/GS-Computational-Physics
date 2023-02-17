program electro_RK4
! This program calculates and plots the trajectory of electromagnetism problems with the use of Runge - Kutta Methods
implicit none
! initializing all of the variables and arrays needed to solve the problem.
! gfortran -o electric-field.out electric-field.f95 && ./electric-field.out && gnuplot
! splot 'r.dat' w l
real, allocatable, dimension(:):: r, v, a, atemp, vtemp, rtemp, kr1, kr2, kr3, kr4, kv1, kv2, kv3, kv4, Electric, B, totalF
real(8):: t, GM, m, Tau, pi, n, PE, KE, q

allocate(r(3), v(3), a(3), kr1(3), kr2(3), kr3(3), kr4(3), kv1(3), kv2(3), kv3(3), kv4(3), Electric(3), B(3), totalF(3), atemp(3))
allocate(rtemp(3), vtemp(3))

pi = 4.0*atan(1.0)

! initializing all the values
! In the next three lines, each array is made equal to zero.
r = 0.0d0 ! the position
v = 0.0d0 ! the velocity
B = 0.0d0 ! the magnetic field

! setting the mass and the q constants here 
m = 1.0
q = 1.0

!the exceptions to the arrays that were equaled to zero are here.
r(1) = 1.0 ! x position
v(1) = 1.0 ! velocity in the x axis
B(3) = 0.1 ! the magnetic field
Electric(1) = 0.0d0 ! the electric field's x component
Electric(2) = 0.01  ! the electric field's y component
Electric(3) = 0.1   ! the electric field's z component

! the first derivatives for r and v in the Runge-Kutta method. The derivatives of r and v are mathematically solved to be the velocity and the acceleration respectively
kr1 = v 
kv1 = a
! hardcoding Tau in because I always put in 0.01 when prompted by the read statement
Tau = 0.01

! acceleration is represented by the total force of the electric and magnetic fields divided by mass
a(1) = q*(electric(1)+ v(2)*B(3) - v(3)*B(2))/m
a(2) = -q *(electric(2) + v(1)*B(3) - v(3)*B(1))/m
a(3) = q * (electric(3) + v(1)*B(2) - v(2)*B(1))/m
! opening all the files up 
open (unit=100, file="r.dat")
open (unit=200, file="v.dat")
open(unit=300, file="a.dat")
open(unit=400, file="KE.dat")
open(unit=500, file="PE.dat")
open(unit=600, file="E.dat")

! this is where the trajectory is calculated and written
do while (t < 10000)
   t = t + Tau
   ! first derivative through Runge - Kutta Methods
   kr1 = v 
   kv1 = a  
   vtemp = v + 0.5*Tau*kv1
   ! atemp is equal to the combined forces of electric and magnetic fields divided by mass
   atemp(1) = q*(electric(1)+ vtemp(2)*B(3) - vtemp(3)*B(2))/m ! calculating the current x acceleration and inserting the current vtemp after k
   atemp(2) = q *(electric(2) + vtemp(3)*B(1) - vtemp(1)*B(3))/m
   atemp(3) = q * (electric(3) + vtemp(1)*B(2) - vtemp(2)*B(1))/m
   ! second derivative through Runge - Kutta Methods
   kr2 = vtemp ! sorting the second derivative of r, the position to the vtemp after the first derivative
   kv2 = atemp	! sorting the second derivative of v, the velocity to the atemp after the first derivative
   vtemp = v + 0.5*Tau*kv2
   atemp(1) = q*(electric(1)+ vtemp(2)*B(3) - vtemp(3)*B(2))/m ! calculating the current x acceleration and inserting the current vtemp after k
   atemp(2) = q *(electric(2) + vtemp(3)*B(1) - vtemp(1)*B(3))/m
   atemp(3) = q * (electric(3) + vtemp(1)*B(2) - vtemp(2)*B(1))/m
   ! third derivative through Runge - Kutta Methods
   kr3 = vtemp
   kv3 = atemp
   vtemp = v + Tau*kv3
   atemp(1) = q*(electric(1)+ vtemp(2)*B(3) - vtemp(3)*B(2))/m ! calculating the current x acceleration and inserting the current vtemp after k
   atemp(2) = q *(electric(2) + vtemp(3)*B(1) - vtemp(1)*B(3))/m
   atemp(3) = q * (electric(3) + vtemp(1)*B(2) - vtemp(2)*B(1))/m
   ! fourth derivative through Runge - Kutta Methods
   kr4 = vtemp
   kv4 = atemp
   ! calculating the future approximated position and velocity 
   r = r + (Tau/6.0)*(kr1 + 2.0*kr2 + 2.0*kr3 + kr4)
   v = v + (Tau/6.0)*(kv1 + 2.0*kv2 + 2.0*kv3 + kv4)
   ! write the position, velocity and acceleration values (only position is needed, but I added velocity and acceleration just cause)
   write(100,*) r
   write(200,*) v
   write(300,*) a

end do

!r = r + (Tau/6.0)*(kr1 + 2.0*kr2 + 2.0*kr3 + kr4)
!v = v + (Tau/6.0)*(kv1 + 2.0*kv2 + 2.0*kv3 + kv4)

end program electro_RK4
   
   
    






