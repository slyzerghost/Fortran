program q8
    implicit none
    integer :: n
    real :: npop,k,i0,people,percentage
    npop=1000.0
    k=0.3
    i0=5.0
    open(20,file="out4q8.txt")
    write(20,'(a6,a15,a15)') "Day","In","Percentage"
    do n=1,20
        people=rumor(n,i0,k,npop)
        percentage=people/npop*100.0
        write(20,'(i6,f15.4,f14.4,a1)') n,people,percentage,"%"
    end do
    close(20)
contains
    recursive real function rumor(n,i0,k,npop) result(inow)
        integer, intent(in) :: n
        real, intent(in) :: i0,k,npop
        real :: previous
        if(n==0) then
            inow=i0
        else
            previous=rumor(n-1,i0,k,npop)
            inow=previous+(k*previous/npop)*(npop-previous)
        end if
    end function rumor
end program q8
