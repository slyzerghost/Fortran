program q1
implicit none

integer :: n
real :: a, b, p, pold, error

a = 2.0
b = 3.0
pold = 0.0
n = 0
open(10,file="output1.txt")
write(10,*) "Iter       a          b          Pn         Error"

do
    n = n + 1

    p = (a + b) / 2.0

    if (n == 1) then
        error = 0.0
    else
        error = abs(p - pold) / abs(p)
    end if

    write(10,'(I4,4F12.6)') n, a, b, p, error

    if (f(a)*f(p) < 0.0) then
        b = p
    else
        a = p
    end if

    if (n > 1 .and. error < 1.0E-4) exit

    pold = p
end do

write(10,*)
write(10,'(A,F10.6)') "Root = ", p

contains

real function f(x)
    real :: x
    f = x**3 - 4.0*x - 9.0
end function f

end program q1
