program qs_1
    implicit none
    real::a,b,p,pold,tol,er
    integer::i
    open(12,file="out1q1.txt")
    a=2.0
    b=3.0
    tol=1.0e-4
    i=0
    write(12,'(A14,3A16,A20)')"Iteration No","   a   ","   b   ","   Pn   ","      |Pn-Pn-1|/|Pn|"
     do
        i=i+1
        p=(a+b)/2

        if(i==1)then
            write(12,'(I14,3F16.6,A20)')i,a,b,p,"_"
        else
            er=abs(p-pold)/abs(p)
             write(12,'(I14,3F16.6,F20.6)')i,a,b,p,er
             if(er<tol)exit
        end if
        if(f(a)*f(p)<0)then
            b=p
        else
            a=p
        end if
        pold=p
     end do

     contains

     function f(x) result(res)
        real,intent(in)::x
        real::res
        res=x**3-4.0*x-9.0
    end function
end program
