program CallFortranInR

    integer :: cx

    write(*,*) "[SRT] Main : CallFortranInR"
    
!    call TestSub(5, 2, cx)
    call TestLoop()

    write(*,*) "[END] Main : CallFortranInR"

    stop

end program

subroutine TestSub(ax, bx, cx)

    implicit none

    integer :: ax, bx, cx

    write(*,*) "[SRT] Call : TestSub"

    cx = ax * bx

    write(*,*) "cx : ", cx

    write(*,*) "[END] Call : TestSub"

    return

end


subroutine TestLoop()

    implicit none

    real(8), dimension(4) :: D_mat, R_mat, dimSumR, dimMeanR, dimValD, initDimDel, initDimR
    real(8) :: movAvgDiff, chkVal, R1_avg
!    real, dimension(:, :), allocatable :: dimR, dimD
!    real, dimension(:), allocatable :: dimAvg,z
    integer(8) :: i, j, cnt

    write(*,*) "[SRT] Call : TestLoop"
    
    open(10, file="./input-dimD.dat")
    do i = 1, 4
       read(10, *) initDimDel(i)
    enddo
    close(10)
    
    open(11, file="./input-dimR.dat")
    do i = 1, 4
       read(11, *) initDimR(i)
    enddo
    close(11)
    
     open(12, file="./input-dimAvg.dat")
     read(12, *) R1_avgsed -ie 's/GSSAPIAuthentication yes/GSSAPIAuthentication no/g;s/#UseDNS yes/UseDNS no/g' /etc/ssh/sshd_config
     close(12)
     
     open(13, file="./input-dimMovAvg.dat")
     read(13, *) movAvgDiff
     close(13)

     write(*,*) "initDimDel : ", initDimDel
     write(*,*) "initDimR : ", initDimR
     write(*,*) "R1_avg : ", R1_avg
     write(*,*) "movAvgDiff : ", movAvgDiff
     
   ! initDimDel = (/ -0.06168557216, -0.004145548626, -0.1034820446, -0.07044678827, &
   !              -0.05984577786, -0.1615678059, -0.0575989094, -0.004652653786, &
   !               -0.07246191683, -0.08993941135 /)
   ! initDimR = (/ 0.0003440414535, -0.000155599815, -3.40403532e-06, 0.0003233300675, &
   !             -0.0001917412399, 0.0002241346382, 0.0007847460569, 0.0004328739683, &
!                0.001134104601, 0.0004143171478 /)
   !  R1_avg  = -0.06858264287
   ! movAvgDiff = 0.4251437736
    
    ! dimSumR = (/ 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0/)
    ! dimSumR = (/ 0.0, 0.0, 0.0, 0.0, 0.0/)
    dimSumR = (/ 0.0, 0.0, 0.0, 0.0/)

    i = 0
    chkVal = 0.01

!    if (.not. allocated(dimR)) then
!        allocate(dimR(99999999, 10))
!    end if
!
!    if (.not. allocated(dimD)) then
!        allocate(dimD(99999999, 10))
!    end if
!
!    if (.not. allocated(dimAvg)) then
!        allocate(dimAvg(99999999))
!    end if

    open(20, file="./result-dimD.dat")
    open(21, file="./result-dimR.dat")
    open(22, file="./result-dimAvg.dat")

    do
        i = i + 1

        D_mat = abs(initDimDel - R1_avg)
!        write(*,*) "D_mat : ", D_mat

        R_mat = (((movAvgDiff / abs(initDimR))**1) * ((movAvgDiff / abs(D_mat))**1))**1
!        write(*,*) "R_mat : ", R_mat
        write(99,*) "R_mat : ", R_mat

        cnt = 0
        
        do j = 1, 4
           dimSumR(j) = dimSumR(j) + R_mat(j)
               
           if (R_mat(j) < chkVal) then
              cnt = cnt + 1
           endif
        enddo

        R1_avg = abs(sum(R_mat * D_mat)/sum(R_mat))
!        write(*,*) "R1_avg : ", R1_avg

        if (cnt == 4) then
            write(*,*) "Loop Cnt", i
            write(*,*) "Check Cnt", cnt
            write(*,*) "D_mat", D_mat
            write(*,*) "R_mat", R_mat
            write(*,*) "R1_avg", R1_avg
            write(*,*) "dimSumR", dimSumR
            write(*,*) "dimMeanR", dimSumR / real(i)
            
            dimValD = real(D_mat)
            dimMeanR = dimSumR / real(i)
            
            write(20, 130) dimValD(1), ",", dimValD(2), ",", dimValD(3), &
                        ",", dimValD(4)
                        
            write(21, 130) dimMeanR(1), ",", dimMeanR(2), ",", &
                         dimMeanR(3), ",", dimMeanR(4), ",", sum(dimMeanR)/4.0
                         
            write(22, *) R1_avg
    130     format(10(f15.5, a))
    
            close(20)
            close(21)
            close(22)

            write(*,*) "[END] Call : TestLoop"

!            deallocate(dimR)
!            deallocate(dimD)
!            deallocate(dimAvg)

            return
        endif
        
    enddo
end
