program q10
    implicit none
    integer :: n
    real :: a(0:50),exact,error
    open(20,file="out4q10.txt")
    a(0)=1.0
    a(1)=5.0
    do n=2,50
        a(n)=2.0*a(n-1)-a(n-2)+3.0
    end do
    write(20,'(a5,a15,a15,a15)') "n","Recurrence","Exact","Error"
    do n=1,50
        exact=1.5*real(n)**2+2.5*real(n)+1.0
        error=abs(a(n)-exact)
        write(20,'(i5,f15.2,f15.2,f15.2)') n,a(n),exact,error
    end do
    close(20)
end program q10
