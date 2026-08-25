program q3
implicit none

integer :: n
real :: a, b, p, pold, error

a = 1.0
b = 2.0
pold = 0.0
n = 0
open(10,file="output3.txt")
write(10,*) "Q3 - False Position Method"
write(10,*) "Iter       a          b          Pn         Error"

do
    n = n + 1

    p = (a*f(b) - b*f(a)) / (f(b) - f(a))

    if (n == 1) then
        error = 0.0
    else
        error = abs(p - pold)
    end if

    write(10,'(I4,4F12.6)') n, a, b, p, error

    if (f(a)*f(p) < 0.0) then
        b = p
    else
        a = p
    end if

    if (n > 1 .and. error < 1.0E-5) exit

    pold = p
end do

write(10,*)
write(10,'(A,F10.6)') "Root = ", p

contains

real function f(x)
    real :: x
    f = log(x) + x**2 - 3.0
end function f

end program q3
