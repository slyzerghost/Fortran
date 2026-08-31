program q4
    implicit none

    integer :: i, j, n
    real :: x(5), y(5)
    real :: xp, exact, approx, error
    real :: L

    n = 5
    xp = 1.25

    open(10,file="in3q4.txt")
    open(20,file="out3q4.txt")

    do i = 1,n
        read(10,*) x(i)
        y(i) = 0.3*x(i)*exp(0.15*x(i)**2)
    end do

    write(20,*) "FUNCTION VALUES"
    write(20,*)

write(20,'(a8,a15)')"x","f(x)"

    do i = 1,n
        write(20,'(F8.2,F15.7)') x(i),y(i)
    end do

    approx = 0.0

    do i = 1,n
        L = 1.0
        do j = 1,n
            if(i /= j) then
                L = L*(xp-x(j))/(x(i)-x(j))
            end if
        end do

        approx = approx + y(i)*L

    end do

    exact = 0.3*xp*exp(0.15*xp**2)

    error = abs(exact-approx)

    write(20,*)
    write(20,*) "Interpolated value =",approx
    write(20,*) "Exact value        =",exact
    write(20,*) "Absolute error     =",error

    close(10)
    close(20)

end program q4
