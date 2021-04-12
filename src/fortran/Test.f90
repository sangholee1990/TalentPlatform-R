program main

    write(*,*) "Main"
    
    call TestDll()

    
end program

subroutine TestDll()

    real * 8 a, b, c

    write(*,*) "TestDll"

    a = 2.0
    b = 2.0
    
    c = sqrt (a ** 2 + b ** 2)

    write(*,*) c

end