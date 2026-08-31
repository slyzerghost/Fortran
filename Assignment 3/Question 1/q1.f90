program q1
    implicit none

    integer :: i, j, n
    real :: x(10), d(10,10)
    real :: xp, h, p, result, term

    n = 6
    xp = 2007.0

    open(10,file="in3q1.txt")
    open(20,file="out3q1.txt")

    do i = 1,n
        read(10,*) x(i), d(i,1)
    end do


    do j = 2,n
        do i = 1,n-j+1
            d(i,j) = d(i+1,j-1) - d(i,j-1)
        end do
    end do


    write(20,*) "FORWARD DIFFERENCE TABLE"
    write(20,*)

write(20,'(a8,6a12)')"year","y","dy","d2y","d3y","d4y","d5y"

    do i = 1,n
        write(20,'(F8.0,6F12.4)') x(i), (d(i,j),j=1,n-i+1)
    end do

    h = x(2) - x(1)
    p = (xp-x(1))/h

    result = d(1,1)
    term = 1.0

    do j = 2,n
        term = term * (p-(j-2)) / (j-1)
        result = result + term*d(1,j)
    end do

    write(20,*)
    write(20,*) "p =",p
    write(20,*) "GDP in the year 2007 =",result," billion USD"

    close(10)
    close(20)

end program q1
