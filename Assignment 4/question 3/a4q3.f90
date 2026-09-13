program q3
    implicit none
    logical :: p,q,imp,bi,rhs
    integer :: i,j
    open(20,file="out4q3.txt")
    write(20,*) "p    q    p->q    p<->q    RHS"
    do i=0,1
        do j=0,1
            p=(i==1); q=(j==1)
            imp=(.not.p).or.q
            bi=(p.and.q).or.((.not.p).and.(.not.q))
            rhs=(p.and.q).or.((.not.p).and.(.not.q))
            write(20,*) p,q,imp,bi,rhs
        end do
    end do
    write(20,*)
    write(20,*) "Hence p <-> q is equivalent to (p AND q) OR (NOT p AND NOT q)."
    close(20)
end program q3
