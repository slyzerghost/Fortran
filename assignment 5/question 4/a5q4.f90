program q4
 implicit none
 real(8) :: a,b,h,y(0:30),trap,s13,s38,wed,exact,res(4),err,pct
 integer :: n,i,j
 character(len=22) :: names(4)

 open(10,file='in_a5q4.txt',status='old')
 open(20,file='out_a5q4.txt',status='replace')

 read(10,*) a,b,n

 if(n/=30) stop
 h=(b-a)/n

 do i=0,n
  y(i)=f(a+i*h)
 end do

 trap=y(0)+y(n)
 s13=trap
 s38=trap
 wed=0

 do i=1,n-1

  trap=trap+2*y(i)

  if(mod(i,2)==0) then
   s13=s13+2*y(i)
  else
   s13=s13+4*y(i)
  end if

  if(mod(i,3)==0) then
   s38=s38+2*y(i)
  else
   s38=s38+3*y(i)
  end if

 end do

 trap=trap*h/2
 s13=s13*h/3
 s38=s38*3*h/8

 do i=0,n-6,6
  wed=wed+y(i)+5*y(i+1)+y(i+2)+6*y(i+3)+y(i+4)+5*y(i+5)+y(i+6)
 end do

 wed=wed*3*h/10
 exact=60*log(1+b*b)-60*log(1+a*a)
 res=[trap,s13,s38,wed]

 names(1)='Trapezoidal'
 names(2)='Simpson 1/3'
 names(3)='Simpson 3/8'
 names(4)='Weddle'

 write(20,'(a,f18.10)') 'Exact heat (J) = ',exact
 write(20,'(a,f12.6)') 'Step size (s)  = ',h

 write(20,'(a22,3a24)') 'Method','Approximation(J)', &
                       'Absolute error(J)','Percent error(%)'

 do j=1,4
  err=abs(res(j)-exact)
  pct=100*err/abs(exact)
  write(20,'(a22,3f24.10)') names(j),res(j),err,pct
 end do

 close(10)
 close(20)

contains

 real(8) function f(t)
  real(8),intent(in) :: t
  f=120*t/(1+t*t)
 end function

end program
