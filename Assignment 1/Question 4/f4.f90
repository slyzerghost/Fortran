program divisor_perfect
    implicit none

    integer :: n, tau, sigma

    integer :: count

    ! Part 4(i)
    print *, 'Enter a positive integer:'
    read *, n

    call divisor_info(n, tau, sigma)

    print *, 'Number of divisors = ', tau
    print *, 'Sum of divisors = ', sigma

    ! Part 4(ii)
    print *, 'First four perfect numbers:'

    count = 0
    n = 1

    do while (count < 4)

        call divisor_info(n, tau, sigma)

        if (sigma == 2*n) then
            print *, n
            count = count + 1
        end if

        n = n + 1

    end do

contains

    subroutine divisor_info(n, tau, sigma)
        implicit none

        integer, intent(in) :: n
        integer, intent(out) :: tau, sigma
        integer :: i

        tau = 0
        sigma = 0

        do i = 1, n

            if (mod(n, i) == 0) then
                tau = tau + 1
                sigma = sigma + i
            end if

        end do

    end subroutine divisor_info

end program divisor_perfect
