program name
    implicit none
    integer::n
    real::recur,exact,a0,a1,a2,error
    a0=1
    a1=5

    do n=2,51
        a2=2*a1-a0+3
        exact=1.5*n**2+2.5*n+1
        error=abs(a2-exact)
        write(*,*)n,a2,exact,error
        a0=a1
        a1=a2
    end do
end program
