program q1
    implicit none
    logical :: f,b,c,r1,r2,equal
    integer :: i,j,k
    open(20,file="out4q1.txt")
    write(20,*) "F    B    C    Rule1    Rule2"
    equal=.true.
    do i=0,1
        do j=0,1
            do k=0,1
                f=(i==1); b=(j==1); c=(k==1)
                r1=(f.and.b).or.(f.and.c).or.(b.and.c)
                r2=(f.and.b).or.(c.and.(f.or.b))
                if(r1.neqv.r2) equal=.false.
                write(20,*) f,b,c,r1,r2
            end do
        end do
    end do
    write(20,*)
    if(equal) then
        write(20,*) "The two rules are logically equivalent."
    else
        write(20,*) "The two rules are not logically equivalent."
    end if
    close(20)
end program q1
