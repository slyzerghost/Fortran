program qs_3
    implicit none
    real::p0,p1,p2,tol,abs_er,pold
    integer::i

    open(13,file="out1q2.txt")

    p0=1.0
    p1=2.0
    tol=1.0e-5
    pold=2.0
    i=0
    write(13,'(3A16,A20)')" Iteration No. ","       Pn-1   ","        Pn     ","           |Pn-Pn-1|  "

    do
        i=i+1
        p2=p1-(f(p1)*(p1 -p0))/(f(p1)-f(p0))
        abs_er=abs(p2-pold)
        write(13,'(I16,2F16.6,F20.6)')i,p1,p2,abs_er
        if(abs_er<tol)exit
        if(f(p0)*f(p2)<0)then
            p1=p2
        else
            p0=p2
        end if
        pold=p2
    end do
    contains

    function f(x) result(res)
        real,intent(in)::x
        real::res
        res=log(x)+x**2-3.0
    end function f

end program
