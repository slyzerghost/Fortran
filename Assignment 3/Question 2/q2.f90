program q2
    implicit none

    integer :: i, j, n
    real :: x(10), d(10,10)
    real :: xp, h, p, result, term

    n = 5
    xp = 4.5

    open(10,file="in3q2.txt")
    open(20,file="out3q2.txt")

    do i = 1,n
        read(10,*) x(i), d(i,1)
    end do

    do j = 2,n
        do i = 1,n-j+1
            d(i,j) = d(i+1,j-1) - d(i,j-1)
        end do
    end do

    write(20,*) "BACKWARD DIFFERENCE TABLE"
    write(20,*)
write(20,'(a8,5a14)')"x","y","dy","d2y","d3y","d4y"
    do i = 1,n
        write(20,'(F8.1,5F14.2)') x(i), (d(i-j+1,j),j=1,i)
    end do

    h = x(2)-x(1)
    p = (xp-x(n))/h

    result = d(n,1)
    term = 1.0

    do j = 2,n
        term = term * (p+(j-2))/(j-1)
        result = result + term*d(n-j+1,j)
    end do

    write(20,*)
    write(20,*) "p =",p
    write(20,*) "Maturity value at 4.5 years = Tk.",result

    close(10)
    close(20)

end program q2
