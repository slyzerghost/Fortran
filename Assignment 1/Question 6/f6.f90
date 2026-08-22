program gcd_problem
    implicit none

    integer :: a, b
    integer :: n, i
    integer :: numbers(100)
    integer :: result

    ! Part 6(i)
    print *, 'Enter two numbers:'
    read *, a, b

    result = gcd(a, b)

    print *, 'GCD of two numbers = ', result

    ! Part 6(ii)
    print *, 'How many numbers?'
    read *, n

    print *, 'Enter the numbers:'

    do i = 1, n
        read *, numbers(i)
    end do

    result = numbers(1)

    do i = 2, n
        result = gcd(result, numbers(i))
    end do

    print *, 'GCD of all numbers = ', result

contains

    recursive integer function gcd(a, b) result(g)
        implicit none

        integer, intent(in) :: a, b

        if (mod(a, b) == 0) then
            g = b
        else
            g = gcd(b, mod(a, b))
        end if

    end function gcd

end program gcd_problem
