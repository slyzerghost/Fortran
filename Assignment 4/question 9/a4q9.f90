program q9
    implicit none
    integer :: n,layer,total
    real :: p,cost
    p=15.0
    open(20,file="out4q9.txt")
    write(20,'(a8,a10,a16,a16)') "Layer","n^2","Total Crates","Total Cost($)"
    do n=1,20
        layer=n**2
        total=crates(n)
        cost=real(total)*p
        write(20,'(i8,i10,i16,f16.2)') n,layer,total,cost
    end do
    close(20)
contains
    recursive integer function crates(n) result(t)
        integer, intent(in) :: n
        if(n==1) then
            t=1
        else
            t=crates(n-1)+n**2
        end if
    end function crates
end program q9
