program q1
 implicit none
 real(8) :: t,h,exact,v,err
 integer :: i
 open(10,file='in_a5q1.txt',status='old')
 open(20,file='out_a5q1.txt',status='replace')
 read(10,*) t
 exact=8d0*exp(-t/4d0)*(-0.25d0*cos(2d0*t)-2d0*sin(2d0*t))
 write(20,'(a)') 'Method                         h        Approx velocity       Exact velocity       Absolute error'
 do i=1,4
  if(i==1) then
   h=0.25d0
   v=(f(t+h)-f(t-h))/(2d0*h)
  else if(i==2) then
   h=0.25d0
   v=(-3d0*f(t)+4d0*f(t+h)-f(t+2d0*h))/(2d0*h)
  else if(i==3) then
   h=0.5d0
   v=(f(t-2d0*h)-8d0*f(t-h)+8d0*f(t+h)-f(t+2d0*h))/(12d0*h)
  else
   h=0.5d0
   v=(-25d0*f(t)+48d0*f(t+h)-36d0*f(t+2d0*h)+16d0*f(t+3d0*h)-3d0*f(t+4d0*h))/(12d0*h)
  end if
  err=abs(v-exact)
  select case(i)
  case(1)
   write(20,'(a28,4f20.9)') '3-point midpoint',h,v,exact,err
  case(2)
   write(20,'(a28,4f20.9)') '3-point endpoint',h,v,exact,err
  case(3)
   write(20,'(a28,4f20.9)') '5-point midpoint',h,v,exact,err
  case(4)
   write(20,'(a28,4f20.9)') '5-point endpoint',h,v,exact,err
  end select
 end do
 close(10)
 close(20)
contains
 real(8) function f(x)
  real(8),intent(in) :: x
  f=8d0*exp(-x/4d0)*cos(2d0*x)
 end function
end program
