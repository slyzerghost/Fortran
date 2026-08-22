program qs_4
    real::pn,po,er,tol
    integer::i
    po=0.5
    tol=1.0e-4
    i=0
    open(13,file="out1q4.txt")
    write(13,'(3A16,A20)')"Iteration No.","Pn-1","pn","|Pn-Pn-1|/|Pn|"

    do
        i=i+1
        pn=g(po)
        er=abs(pn-po)/abs(pn)
        write(13,'(I16,2F16.6,F20.6)')i,po,pn,er
        if(er<tol)exit
        po=pn
    end do

    contains

    function g(x) result(res)
        real,intent(in)::x
        real::res
        res=1.0/(1.0+x**2)
    end function g
end program
