program q2
    implicit none
    logical :: w,h,a,b,result,tautology
    integer :: i,j
    open(20,file="out4q2.txt")
    write(20,*) "Warm   Home   A   B   A<->B"
    tautology=.true.
    do i=0,1
        do j=0,1
            w=(i==1); h=(j==1)
            a=w.and.h
            b=.not.((.not.w).or.(.not.h))
            result=(a.and.b).or.((.not.a).and.(.not.b))
            if(.not.result) tautology=.false.
            write(20,*) w,h,a,b,result
        end do
    end do
    write(20,*)
    if(tautology) write(20,*) "A <-> B is a tautology."
    close(20)
end program q2
