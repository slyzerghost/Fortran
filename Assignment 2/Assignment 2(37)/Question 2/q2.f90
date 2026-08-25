program q2
implicit none

integer :: n
real :: p0, p1, p, error
open(10,file="output2.txt")
p0 = 0.0
p1 = 1.0
n = 0

write(10,*) "Q2 - Secant Method"
write(10,*) "Iter       P0         P1         Pn         Error"

do
    n = n + 1

    p = p1 - f(p1)*(p1-p0)/(f(p1)-f(p0))

    error = abs(p - p1)

    write(10,'(I4,4F12.6)') n, p0, p1, p, error

    if (error < 1.0E-6) exit

    p0 = p1
    p1 = p
end do

write(10,*)
write(10,'(A,F10.6)') "Root = ", p

contains

real function f(x)
    real :: x
    f = exp(-x) - x
end function f

end program q2
