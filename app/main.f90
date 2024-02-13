program main
    use df_precision
    use df_data_frame
    implicit none

    type(data_frame) :: df

    call df%new()
    call df%append([1,2,3,4],"index")
    call df%append([3.0_rk, 2.0_rk, 7.d-6, 5.0_rk],"value")
    call df%append(["hello","world","test1","test2"], "chars")
    call df%append([.true.,.true.,.false.,.true.],"truth")

    call df%write()

    print*, " "

    print*, df%getr("value")
    print*, df%geti(1)


    call df%destroy()

    print*, " "


    call df%new()
    call df%append([.true.,.true.,.false.,.false.],"a")
    call df%append([.true.,.false.,.true.,.false.],"b")
    call df%append(df%getl("a") .and. df%getl("b"),"a .and. b")

    call df%write()



end program main
