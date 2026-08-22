program random_matrix
    implicit none

    real :: A(7,7)
    real :: mean_value, max_value, min_value
    integer :: i, j

    ! Generate random numbers between 0 and 1
    call random_number(A)

    ! Find mean, maximum and minimum
    mean_value = sum(A) / 49.0
    max_value = maxval(A)
    min_value = minval(A)

    ! File 1: F descriptor, 10 decimal places
    open(10, file='a1q3_F.txt')

    do i = 1, 7
        write(10, '(7F14.10)') (A(i,j), j = 1, 7)
    end do

    write(10, *)
    write(10, '(A,F14.10)') 'Mean = ', mean_value
    write(10, '(A,F14.10)') 'Maximum = ', max_value
    write(10, '(A,F14.10)') 'Minimum = ', min_value

    close(10)

    ! File 2: E descriptor, 8 decimal places
    open(20, file='a1q3_E.txt')

    do i = 1, 7
        write(20, '(7E16.8)') (A(i,j), j = 1, 7)
    end do

    write(20, *)
    write(20, '(A,E16.8)') 'Mean = ', mean_value
    write(20, '(A,E16.8)') 'Maximum = ', max_value
    write(20, '(A,E16.8)') 'Minimum = ', min_value

    close(20)

    ! File 3: ES descriptor, 7 decimal places
    open(30, file='a1q3_ES.txt')

    do i = 1, 7
        write(30, '(7ES15.7)') (A(i,j), j = 1, 7)
    end do

    write(30, *)
    write(30, '(A,ES15.7)') 'Mean = ', mean_value
    write(30, '(A,ES15.7)') 'Maximum = ', max_value
    write(30, '(A,ES15.7)') 'Minimum = ', min_value

    close(30)



end program random_matrix
