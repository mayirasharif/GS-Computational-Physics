program orbit_RK4
! This program solves orbital motion problems with the use of Runge - Kutta Methods
implicit none
! initializing all of the variables and arrays needed to solve the problem.

real, allocatable, dimension(:):: r, v, a, atemp, vtemp, rtemp, kr1, kr2, kr3, kr4, kv1, kv2, kv3, kv4
real(8):: t, GM, m, Tau, pi, n, PE, KE, E
!n = 3
allocate(r(3), v(3), a(3), kr1(3), kr2(3), kr3(3), kr4(3), kv1(3), kv2(3), kv3(3), kv4(3))
! get 
pi = 4.0*atan(1.0)

! initializing all the values
r = 0.0d0
v = 0.0d0

m = 1.0


r(1) = 1.0
v(2) = 2.0*pi

GM = 4.0*pi**2.0

kr1 = v
kv1 = a

Tau = 0.01
! initializing the temporary values
!rtemp = r + 0.5*Tau*kr1
!vtemp = v + 0.5*Tau*kv1
a(1) = (-(GM)/norm2(r)**2)

KE = 0.5*m*norm2(v)**2
PE = (-(GM*m))/norm2(r)
E = PE + KE	

open (unit=100, file="r.dat")
open (unit=200, file="v.dat")
open(unit=300, file="a.dat")
open(unit=400, file="KE.dat")
open(unit=500, file="PE.dat")
open(unit=600, file="E.dat")

do while (t < 20)
   t = t + Tau
   kr1 = v
   kv1 = a
   rtemp = r + 0.5*Tau*kr1
   vtemp = v + 0.5*Tau*kv1
   atemp = (-(GM)*rtemp/norm2(rtemp)**3)
   
   kr2 = vtemp
   kv2 = atemp
   rtemp = r + 0.5*Tau*kr2
   vtemp = v + 0.5*Tau*kv2
   atemp = (-(GM)*rtemp/norm2(rtemp)**3)
   
   kr3 = vtemp
   kv3 = atemp
   rtemp = r + Tau*kr3
   vtemp = v + Tau*kv3
   atemp = (-(GM)*rtemp/norm2(rtemp)**3)
   
   kr4 = vtemp
   kv4 = atemp
   
   r = r + (Tau/6.0)*(kr1 + 2.0*kr2 + 2.0*kr3 + kr4)
   v = v + (Tau/6.0)*(kv1 + 2.0*kv2 + 2.0*kv3 + kv4)
   a = (-(GM)*r/norm2(r)**3)
  !calculating the energies
  KE = 0.5*m*norm2(v)**2
  PE = (-(GM*m))/norm2(r)
  E = PE + KE	
   write(100,*) r
   write(200,*) v
   write(300,*) a
   write(400,*) KE
   write(500,*) PE
   write(600,*) E

end do

!r = r + (Tau/6.0)*(kr1 + 2.0*kr2 + 2.0*kr3 + kr4)
!v = v + (Tau/6.0)*(kv1 + 2.0*kv2 + 2.0*kv3 + kv4)

end program orbit_RK4
   
   
    






