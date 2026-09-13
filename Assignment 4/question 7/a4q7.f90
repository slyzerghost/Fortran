program q7
    implicit none
    integer :: n,h,m
    logical :: equal
    open(20,file="out4q7.txt")
    write(20,'(a5,a12,a12,a12)') "n","Hn","Mn","Equal?"
    do n=1,10
        h=hanoi(n)
        m=2**n-1
        equal=(h==m)
        write(20,'(i5,i12,i12,l12)') n,h,m,equal
    end do
    write(20,*)
    write(20,*) "Conclusion: Hn = Mn = 2^n - 1 for every n tested."
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
end program q7
