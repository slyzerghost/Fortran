program q2
 implicit none
 real(8) :: t,h,r(5,5),exact
 integer :: i,j
 open(10,file='in_a5q2.txt',status='old')
 open(20,file='out_a5q2.txt',status='replace')
 read(10,*) t,h
 write(20,'(a)') 'Richardson extrapolation: current (mA)'
 write(20,'(a)') 'Row h                R1              R2              R3              R4              R5'
 do i=1,5
  r(i,1)=(q(t+h)-q(t-h))/(2d0*h)
  do j=2,i
   r(i,j)=r(i,j-1)+(r(i,j-1)-r(i-1,j-1))/(4d0**(j-1)-1d0)
  end do
  write(20,'(i3,1x,f8.5,5f16.9)') i,h,(r(i,j),j=1,i)
  h=h/2d0
 end do
 exact=20d0*exp(-t/2d0)
 write(20,'(a,f16.9)') 'Approximate current = ',r(5,5)
 write(20,'(a,f16.9)') 'Exact current       = ',exact
 write(20,'(a,f16.9)') 'Absolute error      = ',abs(r(5,5)-exact)
 write(20,'(a,f12.4)') 'Current to 4 decimals = ',r(5,5)
 close(10)
 close(20)
contains
 real(8) function q(x)
  real(8),intent(in) :: x
  q=40d0*(1d0-exp(-x/2d0))
 end function
end program
