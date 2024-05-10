program trig_functions
    use stdlib_math, only: linspace
    use df_precision, only: dp => rk
    use df_fortranDF, only: data_frame

    real(dp),parameter :: pi = 4.0*atan(1.0_dp)
    type(data_frame) :: df, inverse

    ! Generate data frame with some trig functions
    print "(a)", "Data frame of trig functions:"
    print "(a)", " "
    call df%new(enforce_length=.true.)
    call df%append(linspace(0.0_dp,2*pi,99),"x")
    call df%append(sin(df%getr("x")),"sin(x)")
    call df%append(cos(df%getr("x")),"cos(x)")
    call df%append(tan(df%getr("x")),"tan(x)")
    call df%write()
    print "(a)", " "

    ! Show that inverse of a trig function is x
    ! Use only firs pi range from original data frame
    print "(a)", "Data frame of trig functions as arguments to inverse trig functions:"
    print "(a)", " "
    call inverse%new()
    call inverse%append(df%getr(1,1,df%nrows()/2+1),"x")
    call inverse%append(df%getr("sin(x)",1,df%nrows()/2+1),"sin(x)")
    call inverse%append(asin(inverse%getr("sin(x)")),"asin(sin(x))")
    call inverse%append(df%getr("cos(x)",1,df%nrows()/2+1),"cos(x)")
    call inverse%append(acos(inverse%getr("cos(x)")),"acos(cos(x))")
    call inverse%write()

end program trig_functions