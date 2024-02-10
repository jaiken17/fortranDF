program main
    use,intrinsic :: iso_fortran_env, only: rk=>real64, ik=>int32
    use data_frame_module
    implicit none

    type(data_frame) :: df

    call df%new()
    call df%append([1,2,3,4],"index")
    call df%append([3.0_rk, 2.0_rk, 7.d-6, 5.0_rk],"value")
    call df%append(["hello","world","test1","test2"], " chars")
    call df%append([.true.,.true.,.false.,.true.],"truth")

    call df%write()

    call df%destroy()

    print*, " "


    call df%new()
    call df%append([.true.,.true.,.false.,.false.],"a")
    call df%append([.true.,.false.,.true.,.false.],"b")
    call df%append([.true.,.false.,.false.,.false.],"a .and. b")

    call df%write()

end program main
