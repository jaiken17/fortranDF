program main
    use df_utils
    use df_types
    use df_precision
    use df_fortranDF_new
    implicit none

    type(data_frame) :: df

    call df%new()
    call df%append([1,2,3,4],"index")
    call df%append([3.0_rk, 2.0_rk, 7.d-6, 5.0_rk],"value")
    call df%append(["hello","world","test1","test2"], "chars")
    call df%append([.true.,.true.,.false.,.true.],"truth")

    call df%write()

    print*, " " ! newline

    print*, "get column 'value': ", df%getr("value")
    print*, "get column 1: ", df%geti(1)


    call df%destroy()

    print*, " " ! newline


    ! Make truth table for AND gate
    call df%new()
    call df%append([.true.,.true.,.false.,.false.],"a")
    call df%append([.true.,.false.,.true.,.false.],"b")
    call df%append(df%getl("a") .and. df%getl("b"),"a .and. b")

    call df%write()

    call df%destroy()
   
    ! Make data frame from file
    call df%new()
    call df%read("app/test_file.txt",.true.)
    call df%write()
    print*, "Type of 'header2':",df%dtype("header2")
    print*, "Type values: "
    print*, "real:      ", REAL_NUM
    print*, "integer:   ", INTEGER_NUM
    print*, "logical:   ", LOGICAL_NUM
    print*, "character: ", CHARACTER_NUM
    print*, "complex:   ", COMPLEX_NUM

end program main
