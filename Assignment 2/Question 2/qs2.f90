program qs_2
    implicit none
    real::p0,p1,p2,tol,abs_er
    integer::i

    open(13,file="out1q2.txt")

    p0=0.0
    p1=1.0
    tol=1.0e-6
    i=0

    write(13,'(3A16,A20)')" Iteration No. ","        Pn-1   ","         Pn    ","        Abs Error  "

    do
        i=i+1
        p2=p1-(f(p1)*(p1 - p0))/(f(p1)-f(p0))
        abs_er=abs(p2-p1)

        write(13,'(I16,2F16.6,F20.6)')i,p1,p2,abs_er

        if(abs_er<tol)exit

        p0=p1
        p1=p2
    end do

    close(13)

contains

    function f(x) result(res)
        real,intent(in)::x
        real::res
        res=exp(-x)-x
    end function f

end program qs_2
