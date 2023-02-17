program heat_equation
! this program models the movement of heat across a hypothetical rod.
! initializing 

real(8):: h, Tau, t, L, K
integer(4):: N_step, i, nmax, n_time, n, j
real, allocatable, dimension(:, :) :: temp
n_time = 999
N_step = 61
L = 1.0
Tau = 0.0001
K = 1.0
h = L/(N_step - 1)
allocate(temp((n_time + 1), N_step))
temp = 0.0d0
temp(1, 30) = 1.0/h

open (unit=100, file="heat.dat")
do n = 1, n_time
do i = 2, N_step - 1
 temp(n + 1, i) = temp(n, i) + (K*Tau)/(h**2.0)*((temp(n, i + 1)) + temp(n, i - 1) - 2.0*temp(n, i))
end do
end do


do j = 1, n_time
  if ( MOD(j, 2) .eq. 0)
  write(100,*) temp(j, :)
  end if
end do



end program heat_equation
