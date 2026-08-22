program character_problem
    implicit none

    character(len=100) :: text, modified, clean
    character(len=1) :: ch
    integer :: i, n, k
    integer :: upper, lower, vowel, consonant, digit, space
    logical :: palindrome

    ! Open input file
    open(10, file='in1q2.txt')

    ! Read the complete line
    read(10, '(A)') text

    close(10)

    n = len_trim(text)

    upper = 0
    lower = 0
    vowel = 0
    consonant = 0
    digit = 0
    space = 0
    k = 0

    modified = text
    clean = ' '

    ! Count characters and replace vowels
    do i = 1, n

        ch = text(i:i)

        if (ch >= 'A' .and. ch <= 'Z') then
            upper = upper + 1
        end if

        if (ch >= 'a' .and. ch <= 'z') then
            lower = lower + 1
        end if

        if (ch == ' ') then
            space = space + 1
        end if

        if (ch >= '0' .and. ch <= '9') then
            digit = digit + 1
        end if

        if (ch == 'A' .or. ch == 'E' .or. ch == 'I' .or. &
            ch == 'O' .or. ch == 'U' .or. ch == 'a' .or. &
            ch == 'e' .or. ch == 'i' .or. ch == 'o' .or. &
            ch == 'u') then

            vowel = vowel + 1
            modified(i:i) = '*'

        else if ((ch >= 'A' .and. ch <= 'Z') .or. &
                 (ch >= 'a' .and. ch <= 'z')) then

            consonant = consonant + 1

        end if

        ! Make a clean lowercase version without spaces
        if (ch /= ' ') then
            k = k + 1

            if (ch >= 'A' .and. ch <= 'Z') then
                clean(k:k) = achar(iachar(ch) + 32)
            else
                clean(k:k) = ch
            end if
        end if

    end do

    ! Check palindrome
    palindrome = .true.

    do i = 1, k / 2
        if (clean(i:i) /= clean(k-i+1:k-i+1)) then
            palindrome = .false.
        end if
    end do

    ! Open output file
    open(20, file='out1q2.txt')

    ! Part (i)
    write(20, *) 'Original name:'
    write(20, '(A)') text(1:n)

    write(20, *)
    write(20, *) 'Reverse order:'

    do i = n, 1, -1
        write(20, '(A1)', advance='no') text(i:i)
    end do

    write(20, *)
    write(20, *)

    ! Part (ii)
    write(20, *) 'Uppercase letters = ', upper
    write(20, *) 'Lowercase letters = ', lower
    write(20, *) 'Vowels = ', vowel
    write(20, *) 'Consonants = ', consonant
    write(20, *) 'Digits = ', digit
    write(20, *) 'Blank spaces = ', space

    ! Part (iii)
    write(20, *)
    write(20, *) 'Modified name:'
    write(20, '(A)') modified(1:n)

    ! Part (iv)
    write(20, *)

    if (palindrome) then
        write(20, *) 'The name is a palindrome.'
    else
        write(20, *) 'The name is not a palindrome.'
    end if

    close(20)

end program character_problem
