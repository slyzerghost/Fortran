program q6
    implicit none
    integer :: n,m
    logical :: prime
    open(20,file="out4q6.txt")
    write(20,'(a5,a12,a12)') "n","Mn","Prime?"
    do n=0,14
        m=2**n-1
        prime=isprime(m)
        write(20,'(i5,i12,l12)') n,m,prime
    end do
    close(20)
contains
    logical function isprime(num)
        integer, intent(in) :: num
        integer :: i
        if(num<2) then
            isprime=.false.
            return
        end if
        isprime=.true.
        do i=2,num-1
            if(mod(num,i)==0) then
                isprime=.false.
                exit
            end if
        end do
    end function isprime
end program q6
