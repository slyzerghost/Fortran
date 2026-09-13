program q5
    implicit none
    integer :: n,h
    real :: time,cost
    open(20,file="out4q5.txt")
    write(20,'(a5,a12,a15,a12)') "n","Hn","Time(min)","Cost($)"
    do n=1,10
        h=hanoi(n)
        time=real(h)*3.0/60.0
        cost=real(h)*0.02
        write(20,'(i5,i12,f15.2,f12.2)') n,h,time,cost
    end do
    close(20)
contains
    recursive integer function hanoi(n) result(h)
        integer, intent(in) :: n
        if(n==1) then
            h=1
        else
            h=2*hanoi(n-1)+1
        end if
    end function hanoi
end program q5
