program q4
    implicit none
    logical :: p,q,bi1,bi2,lhs
    integer :: i,j
    open(20,file="out4q4.txt")
    write(20,*) "p    q    NOT(p<->q)    p<->NOT(q)"
    do i=0,1
        do j=0,1
            p=(i==1); q=(j==1)
            call biconditional(p,q,bi1)
            lhs=.not.bi1
            call biconditional(p,.not.q,bi2)
            write(20,*) p,q,lhs,bi2
        end do
    end do
    write(20,*)
    write(20,*) "The two expressions are equivalent."
    close(20)
contains
    subroutine biconditional(a,b,result)
        logical, intent(in) :: a,b
        logical, intent(out) :: result
        result=(a.and.b).or.((.not.a).and.(.not.b))
    end subroutine biconditional
end program q4
