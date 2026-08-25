program q5
implicit none

integer :: i, n
real :: pold, p, error
real :: x0(4)
open(10,file="output5.txt")
x0(1) = 0.0
x0(2) = -1.0
x0(3) = 2.0
x0(4) = -2.5

write(10,*) "Q5 - Newton-Raphson Method"
write(10,*) "Equation: exp(x) - 3x = 0"

do i = 1, 4

    pold = x0(i)
    n = 0

    write(10,*)
    write(10,'(A,F6.2)') "Initial value x0 = ", pold
    write(10,*) "Iter      P(n-1)       Pn          Error"

    do
        n = n + 1

        p = pold - f(pold)/df(pold)

        error = abs(p - pold) / abs(p)

        write(10,'(I4,3F13.6)') n, pold, p, error

        if (error < 1.0E-5) exit

        pold = p
    end do

    write(10,'(A,F10.5)') "Root = ", p

end do

contains

real function f(x)
    real :: x
    f = exp(x) - 3.0*x
end function f

real function df(x)
    real :: x
    df = exp(x) - 3.0
end function df

end program q5
