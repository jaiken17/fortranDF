program main
    use,intrinsic :: iso_fortran_env, only: rk=>real64, ik=>int32
    use data_frame_module
    implicit none

    type(data_frame) :: df

    call df%new()
    call df%append([1,2,3,4],"int")

    print*, df%geti(1,3)

    call df%append([3.0_rk, 2.0_rk, 7.d-6, 5.0_rk],"float")

    print*, df%getr(2,3)

end program main
