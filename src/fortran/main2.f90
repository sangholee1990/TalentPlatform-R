program main2

    integer a,b,c
    
    a = 5
    b = 2
    c = 2
    
    write(*,*) "Main"
    
    call multiply(a, b, c)
    call loop()

    stop

end program

subroutine multiply(ax,bx,cx)
    implicit none

    integer ax,bx,cx

    write(*,*) "multiply"

    cx=ax*bx

end


subroutine loop()
    implicit none

    real(8), dimension(10) :: dimDel, dimR, D_mat, R_mat
    real(8) :: movAvgDiff, R1_avg, sumR_mat
    integer(8) :: i

    write(*,*) "loopTest"

    dimDel = (/ -0.06168557216, -0.004145548626, -0.1034820446, -0.07044678827, &
                  -0.05984577786, -0.1615678059, -0.0575989094, -0.004652653786, &
                   -0.07246191683, -0.08993941135 /)
    dimR = (/ 0.0003440414535, -0.000155599815, -3.40403532e-06, 0.0003233300675, &
                -0.0001917412399, 0.0002241346382, 0.0007847460569, 0.0004328739683, &
                0.001134104601, 0.0004143171478 /)
    R1_avg  = -0.06858264287
    movAvgDiff = 0.4251437736
    i = 0

    do
        i = i + 1
        ! write(*,*) "dimDel : ", dimDel
        ! write(*,*) "dimR : ", dimR
        ! write(*,*) "movAvgDiff : ", movAvgDiff

        D_mat = abs(dimDel - R1_avg)
        ! write(*,*) "D_mat : ", D_mat

        R_mat = (((movAvgDiff / abs(dimR))**1) * ((movAvgDiff / abs(D_mat))**1))**1
        ! write(*,*) "R_mat : ", R_mat

        R1_avg = abs(sum(R_mat * D_mat)/sum(R_mat))
        ! write(*,*) "R1_avg : ", R1_avg

        sumR_mat = sum(R_mat)
        ! write(*,*) "sumR_mat : ", sumR_mat

        if (sumR_mat < 0.000001) then
            write(*,*) i, sumR_mat
            write(*,*) "D_mat", D_mat
            write(*,*) "R_mat", R_mat
            write(*,*) "R1_avg", R1_avg

            exit
        endif
        
    enddo
end
