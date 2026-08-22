
program matrix_problem
    implicit none

    integer :: A(4,4),B(4,4)
    integer :: i, j

    ! Read matrix A from input file
    open(10, file='in1q1.txt')
    do i = 1, 4
        read(10, *) (A(i,j), j = 1, 4)
    end do

    close(10)

    ! Copy A into B
    B = A

    ! Replace boundary elements of B with zero
    do i = 1, 4
        do j = 1, 4
            if (i == 1 .or. i == 4 .or. j == 1 .or. j == 4) then
                B(i,j) = 0
            end if
        end do
    end do

    ! Open output file
    open(20, file='out1q1.txt')

    ! Part (i): Print matrix A
    write(20, *) 'Matrix A:'

    do i = 1, 4
        write(20, *) (A(i,j), j = 1, 4)
        end do


    ! Part (ii): Print even-numbered rows
    write(20, *)
    write(20, *) 'Even-numbered rows:'

    do i = 2, 4, 2
        write(20, *) (A(i,j), j = 1, 4)
    end do

    ! Part (iii): Print boundary elements
    write(20, *)
    write(20, *) 'Boundary elements:'

    do i = 1, 4
        do j = 1, 4
            if (i == 1 .or. i == 4 .or. j == 1 .or. j == 4) then
                write(20, '(I4)',advance='no') A(i,j)
            else
                write(20, '(4X)',advance='no')
            end if
        end do
        write(20, *)
    end do

    ! Part (iv): Print matrix B
    write(20, *)
    write(20, *) 'Matrix B:'

    do i = 1, 4
        write(20, *) (B(i,j), j = 1, 4)
    end do

    close(20)

end program matrix_problem
