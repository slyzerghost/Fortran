program q11
    implicit none
    integer :: a,b,n,r,term
    integer(kind=8) :: coefficient,value
    open(10,file="in4q11.txt")
    open(20,file="out4q11.txt")
    read(10,*) a,b,n
    write(20,*) "a =",a," b =",b," n =",n
    write(20,'(a5,a8,a15,a18)') "r","Term","Coefficient","Value of term"
    do r=0,n
        term=r+1
        coefficient=fact(n)/(fact(r)*fact(n-r))
        value=coefficient*(int(a,8)**(n-r))*(int(b,8)**r)
        write(20,'(i5,i8,i15,i18)') r,term,coefficient,value
    end do
    close(10)
    close(20)
contains
    recursive integer(kind=8) function fact(n) result(f)
        integer, intent(in) :: n
        if(n<=1) then
            f=1_8
        else
            f=int(n,8)*fact(n-1)
        end if
    end function fact
end program q11
