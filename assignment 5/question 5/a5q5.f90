program q5
 implicit none
 real(8) :: a,b,h,pi,r(10,10),total,exact,err
 integer :: i,j,k,n,steps
 open(10,file='in_a5q5.txt',status='old')
 open(20,file='out_a5q5.txt',status='replace')
 read(10,*) n
 if(n<1.or.n>10) stop 'Choose 1 to 10 Romberg levels'
 pi=acos(-1d0)
 a=0d0
 b=pi/4d0
 write(20,'(a)') 'Romberg extrapolation table (J)'
 write(20,'(a)') 'Level   R1             R2             R3             R4             R5             R6             R7'
 do i=1,n
  steps=2**(i-1)
  h=(b-a)/steps
  total=(f(a)+f(b))/2d0
  do k=1,steps-1
   total=total+f(a+k*h)
  end do
  r(i,1)=h*total
  do j=2,i
   r(i,j)=r(i,j-1)+(r(i,j-1)-r(i-1,j-1))/(4d0**(j-1)-1d0)
  end do
  write(20,'(i3,10f15.9)') i,(r(i,j),j=1,i)
 end do
 exact=sqrt(2d0)*(pi*pi/32d0+pi/4d0-1d0)
 err=abs(r(n,n)-exact)
 write(20,'(a,f18.12)') 'Approximate energy (J) = ',r(n,n)
 write(20,'(a,f18.12)') 'Exact energy (J)       = ',exact
 write(20,'(a,es18.9)') 'Absolute error (J)     = ',err
 write(20,'(a,f12.5)') 'Energy to 5 decimals   = ',r(n,n)
 close(10)
 close(20)
contains
 real(8) function f(t)
  real(8),intent(in) :: t
  f=t*t*cos(t)
 end function
end program
