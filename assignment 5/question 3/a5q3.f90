program q3
 implicit none
 real(8) :: t(7),s(7),h,v0,vn,a0,an
 integer :: i
 open(10,file='in_a5q3.txt',status='old')
 open(20,file='out_a5q3.txt',status='replace')
 do i=1,7
  read(10,*) t(i),s(i)
 end do
 h=t(2)-t(1)
 v0=(-3d0*s(1)+4d0*s(2)-s(3))/(2d0*h)
 vn=(3d0*s(7)-4d0*s(6)+s(5))/(2d0*h)
 a0=(2d0*s(1)-5d0*s(2)+4d0*s(3)-s(4))/(h*h)
 an=(2d0*s(7)-5d0*s(6)+4d0*s(5)-s(4))/(h*h)
 write(20,'(a)') 'Time(s)       Velocity(m/s)      Acceleration(m/s^2)'
 write(20,'(3f20.6)') t(1),v0,a0
 write(20,'(3f20.6)') t(7),vn,an
 close(10)
 close(20)
end program
