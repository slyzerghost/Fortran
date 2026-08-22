program qs_5
    real,dimension(4)::x
    real::pn,po,er,tol
    integer::i,k
    x=[0.0,-1.0,2.0,-2.5]
    tol=1.0e-4

    open(13,file="out1q5.txt")

 do k=1,4
    po=x(k)
    i=0
    write(13,*)"(",k,")"
    write(13,'(3A16,A20)')"Iteration No.","Pn-1","pn","|Pn-Pn-1|/|Pn|"
    do
        i=i+1
        pn=po-f(po)/df(po)
        er=abs(pn-po)/abs(pn)
        write(13,'(I16,2F16.6,F20.6)')i,po,pn,er
        if(er<tol)exit
        po=pn
    end do
 end do
    contains

    function f(x) result(res)
        real,intent(in)::x
        real::res
        res=exp(x)-3*x
    end function f

    function df(x) result(res)
        real,intent(in)::x
        real::res
        res=exp(x)-3
    end function df
end program
