program q3
    implicit none

    integer :: i, j, n
    real :: x(10), d(10,10)
    real :: xp, result, term

    n = 5
    xp = 3.0

    open(10,file="in3q3.txt")
    open(20,file="out3q3.txt")

    do i = 1,n
        read(10,*) x(i), d(i,1)
    end do

    do j = 2,n
        do i = 1,n-j+1
            d(i,j) = (d(i+1,j-1)-d(i,j-1)) / &
                     (x(i+j-1)-x(i))
        end do
    end do

    write(20,*) "DIVIDED DIFFERENCE TABLE"
    write(20,*)

    do i = 1,n
        write(20,'(F8.2,5F14.6)') x(i), &
             (d(i,j),j=1,n-i+1)
    end do

    result = d(1,1)
    term = 1.0

    do j = 2,n
        term = term * (xp-x(j-1))
        result = result + term*d(1,j)
    end do

    write(20,*)
    write(20,*) "Exchange rate at x = 3 hours =",result

    close(10)
    close(20)

end program q3
