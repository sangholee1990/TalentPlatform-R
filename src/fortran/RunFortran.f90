
! gfortran RunFortran.f90
! ./a.exe namelistInfo.txt
! "E:/04. TalentPlatform/Github/TalentPlatform-R/src/fortran"

program RunFortran

	character(200) :: namelistInfo
    character(200) :: inpFile
    character(200) :: outflowFile, stoFile, rdrFile

    namelist /input/ inpFile
    namelist /output/ outflowFile, stoFile, rdrFile

    write(*,*) "[START] Main : RunFortran"
    
	! *************************************************
    ! 1. namelist 읽기
    ! *************************************************
    if (iargc() > 0) then
        call getarg(1, namelistInfo)
        write(*,*) trim(namelistInfo)
        open(9999, file = trim(namelistInfo), status='old')
        read(9999, nml=input)
        read(9999, nml=output)
        close(9999)
    else
        write(*,*) "No Namelist Info"
        write(*,*) "ex) gfortran RunFortran.f90"
        write(*,*) "    ./a.exe namelistInfo.txt"
        stop
    endif
	
    call dataProc(inpFile, outflowFile, stoFile, rdrFile)

    write(*,*) "[END] Main : RunFortran"

end program


subroutine dataProc(inpFile, outflowFile, stoFile, rdrFile)

    implicit none

    integer :: i, cnt
    integer :: errCode
    real :: dum
    real, dimension(:), allocatable :: rc, rain, s1, s2
    real, dimension(:), allocatable :: q1, q2, q3, inf1, inf2, total_q
	real, dimension(:, :), allocatable :: resData
	character(200) :: inpFile
    character(200) :: outflowFile, stoFile, rdrFile

    ! **************************************************
    ! 유출 모형 매개변수 11개 
    ! **************************************************
    ! 면적
    real, parameter :: a = 1000.0
    ! 기저유량
    real, parameter :: Qb = 15.0
    ! 초기 저류고 
    real, parameter :: s_ini = 6.0
    ! 유출공계수
    real, parameter :: a1 = 0.05, a2 = 0.15, a3 = 0.3
    ! 유출공높이
    real, parameter :: h1 = 25.0, h2 = 5.0, h3 = 8.0
    ! 침투공계수
    real, parameter :: b1 = 0.35, b2 = 0.21

    write(*,*) "[START] Call : dataProc"

    ! *************************************************
    ! 2. R에서 전처리 파일 읽기
    ! *************************************************
    ! open(10, file="./LSH0284_input.txt", status='old')
    open(10, file=trim(inpFile), status='old')
    read(10, *)

    cnt = 1
    do 
        read(10, *, iostat = errCode, end=999) dum
!        write(*,*) errCode, cnt, dum
        if (errCode > 0) exit
        cnt = cnt + 1
    enddo
999 rewind(10)

    if (.not. allocated(rc)) allocate(rc(cnt))
    if (.not. allocated(rain)) allocate(rain(cnt))
    if (.not. allocated(s1)) allocate(s1(cnt))
    if (.not. allocated(s2)) allocate(s2(cnt))
    if (.not. allocated(resData)) allocate(resData(cnt, 5))

    ! write(*,*) "[CHECK] cnt : ", cnt  
    read(10, *)
    do i = 1, cnt - 1
        read(10, *) rc(i), rain(i), s1(i), s2(i)
!        write(*,*) i, rc(i), rain(i), s1(i), s2(i)
    enddo
    close(10)

    ! *************************************************
    ! 3. 유출 모형 계산
    ! *************************************************
    if (.not. allocated(q1)) allocate(q1(cnt))
    if (.not. allocated(q2)) allocate(q2(cnt))
    if (.not. allocated(q3)) allocate(q3(cnt))
    if (.not. allocated(inf1)) allocate(inf1(cnt))
    if (.not. allocated(inf2)) allocate(inf2(cnt))
    if (.not. allocated(total_q)) allocate(total_q(cnt))

    s1 = s1 + rain

    where(s1(:) >= h1)
        q1(:) = (s1(:) - h1) * a1
        q2(:) = (s1(:) - h2) * a2
        inf1(:) = s1(:) * b1
        s1(:) = s1(:) - q1(:) - q2(:) - inf1(:)
    else where(s1(:) < h1 .and. s1(:) >= h2)
        q1(:) = 0.0
        q2(:) = (s1(:) - h2) * a2
        inf1(:) = s1(:) * b1
        s1(:) = s1(:) - q1(:) - q2(:) - inf1(:)
    else where
        q1(:) = 0.0
        q2(:) = 0.0
        inf1(:) = s1(:) * b1
        s1(:) = s1(:) - q1(:) - q2(:) - inf1(:)
    end where

    s2(:) = s2(:) + inf1(:)

    where(s2(:) >= h3)
        q3(:) = (s2(:) - h3) * a3
        inf2(:) = s2(:) * b2
        s2(:) = s2(:) - q3(:) - inf2(:)
        total_q(:) = ((q1(:) + q2(:) + q3(:)) * a / 3.6 + Qb)
    else where
        q3(:) = 0.0
        inf2(:) = s2(:) * b2
        s2(:) = s2(:) - q3(:) - inf2(:)
        total_q(:) = ((q1(:) + q2(:) + q3(:)) * a / 3.6 + Qb)
    end where

    ! write(*,*) "[CHECK] rc : ", rc  
    ! write(*,*) "[CHECK] rain : ", rain  
    ! write(*,*) "[CHECK] s1 : ", s1  
    ! write(*,*) "[CHECK] s2 : ", s2
    ! write(*,*) "[CHECK] total_q : ", total_q


    ! *************************************************
    ! 4. 결과 생성
    ! *************************************************
    ! open(11, file="./LSH0284_Outflow_2108232220.txt")
    ! open(12, file="./LSH0284_Sto_2108232230.txt")
    ! open(13, file="./LSH0284_RDR_2108232230.txt")
    open(11, file=trim(outflowFile))
    open(12, file=trim(stoFile))
    open(13, file=trim(rdrFile))

    do i = 1, cnt - 1
        ! read(10, *) rc(i), rain(i), s1(i), s2(i)
        ! write(*,"(i10, 10f10.4)") i, rc(i), rain(i), s1(i), s2(i), total_q(i), q1(i), q2(i), q3(i), inf1(i), inf2(i)
        write(11,"(3f15.3)") rc(i), rain(i), total_q(i)
        write(12,"(3f15.3)") rc(i), s1(i), s2(i)
        write(13,"(4f15.3)") rc(i), rain(i), s1(i), s2(i)
		
		resData(i, 1) = rc(i)
		resData(i, 2) = rain(i)
		resData(i, 3) = s1(i)
		resData(i, 4) = s2(i)
		resData(i, 5) = total_q(i)
    enddo

    close(11)
    close(12)
    close(13)

    ! *************************************************
    ! 5. 메모리 해제
    ! *************************************************
    deallocate(rc)
    deallocate(rain)
    deallocate(s1)
    deallocate(s2)
    deallocate(q1)
    deallocate(q2)
    deallocate(q3)
    deallocate(inf1)
    deallocate(inf2)
    deallocate(total_q)
	deallocate(resData)

    write(*,*) "[END] Call : dataProc"
    
    return
end subroutine
