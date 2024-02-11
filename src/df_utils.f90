module df_utils
    use,intrinsic :: iso_fortran_env,only: IOSTAT_END
    use df_precision
    implicit none

    private

    public :: get_num_lines

    character(len=16),parameter :: err_msg_io = "General IO error"
    character(len=17),parameter :: err_msg_io_open = "IO error on open"

    interface get_num_lines
        procedure :: get_num_lines_unit
        procedure :: get_num_lines_filename
    end interface get_num_lines

contains

    subroutine get_num_lines_unit(unit,num_lines)
        ! Gets number of lines left in file open with unit. If total number of lines
        ! is desired, call rewind on io unit before calling this procedure.
        ! Subroutine rewinds file unit at end.
        integer,intent(in) :: unit
        integer,intent(out) :: num_lines

        integer :: io_err

        io_err = 0
        num_lines = 0
        do while(io_err == 0)
            read(unit=unit,fmt='(a)',iostat=io_err)
            num_lines = num_lines + 1
        end do

        if (io_err /= IOSTAT_END) then
            error stop err_msg_io
        end if

        rewind(unit)

    end subroutine get_num_lines_unit

    subroutine get_num_lines_filename(filename,num_lines,unit)
        ! Gets number of lines in `filename` and either closes file
        ! or returns file unit (if present)
        character(len=*),intent(in) :: filename
        integer,intent(out) :: num_lines
        integer,intent(out),optional :: unit

        character(len=:),allocatable :: err_msg
        integer :: io_err, io_unit

        open(newunit=io_unit,file=trim(adjustl(filename)),status="old",action="read",iostat=io_err)
        if (io_err /= 0) then
            err_msg = err_msg_io_open//" "//trim(adjustl(filename))
            error stop err_msg
        end if

        io_err = 0
        num_lines = 0
        do while(io_err == 0)
            read(unit=io_unit,fmt='(a)',iostat=io_err)
            num_lines = num_lines + 1
        end do

        if (io_err /= IOSTAT_END) then
            error stop err_msg_io
        end if

        if (present(unit)) then
            ! rewind and keep file open
            rewind(io_unit)
            unit = io_unit
        else
            ! close file
            close(io_unit)
        end if

    end subroutine get_num_lines_filename


end module df_utils