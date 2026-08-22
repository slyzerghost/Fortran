program mobius_program
    implicit none

    integer :: n

    print *, 'Enter n:'
    read *, n

    print *, 'Mobius value = ', mobius(n)

contains

    integer function mobius(n)
        implicit none

        integer, intent(in) :: n
        integer :: p, temp, count

        if (n == 1) then
            mobius = 1
            return
        end if

        temp = n
        count = 0
        p = 2

        do while (p <= temp)

            if (mod(temp, p) == 0) then

                temp = temp / p
                count = count + 1

                ! If p divides again, p^2 divides n
                if (mod(temp, p) == 0) then
                    mobius = 0
                    return
                end if
            end if

            p = p + 1

        end do

        if (mod(count, 2) == 0) then
            mobius = 1
        else
            mobius = -1
        end if

    end function mobius

end program mobius_program
