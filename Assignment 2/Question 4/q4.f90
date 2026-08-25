program q4
implicit none

integer :: n
real :: pold, p, error
open(10,file="output4.txt")
pold = 0.5
n = 0

write(10,*) "Q4 - Fixed Point Iteration"
write(10,*) "Iter      P(n-1)       Pn          Error"

do
    n = n + 1

    p = 1.0 / (1.0 + pold**2)

    error = abs(p - pold) / abs(p)

    write(10,'(I4,3F13.6)') n, pold, p, error

    if (error < 1.0E-4) exit

    pold = p
end do

write(10,*)
write(10,'(A,F10.6)') "Root = ", p

end program q4
