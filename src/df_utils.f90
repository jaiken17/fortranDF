module df_utils
    use,intrinsic :: iso_fortran_env,only: IOSTAT_END, IOSTAT_EOR
    use df_precision
    use df_types
    implicit none

    private

    public :: get_num_lines, get_len_line, what_type

    character(len=16),parameter,public :: err_msg_io_read = "IO error on read"
    character(len=17),parameter,public :: err_msg_io_write = "IO error on write"
    character(len=17),parameter,public :: err_msg_io_open = "IO error on open"
    character(len=16),parameter,public :: err_msg_io = "General IO error"

    interface get_num_lines
        procedure :: get_num_lines_unit
        procedure :: get_num_lines_filename
    end interface get_num_lines

    interface get_len_line
        procedure :: get_length_of_line_unit
        procedure :: get_length_of_line_unit_buffer
        procedure :: get_length_of_line_filename
    end interface get_len_line

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
        do while(io_err /= IOSTAT_END)
            read(unit=unit,fmt='(a)',iostat=io_err)
            if (io_err /= IOSTAT_END) num_lines = num_lines + 1
        end do

        ! if (io_err /= IOSTAT_END) then
        !     error stop err_msg_io
        ! end if

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

    subroutine get_length_of_line_unit(io_unit,length,line)
        integer,intent(in) :: io_unit
        integer,intent(out) :: length
        character(len=:),allocatable,intent(out),optional :: line

        character(len=:),allocatable :: buffer
        integer,parameter :: default_buffer_len = 20

        integer :: io_err, size
        logical :: end_of_line

        allocate(character(len=default_buffer_len) :: buffer)

        if (present(line)) then
            !allocate(character(len=1000) :: line)
            line = " "
        end if

        end_of_line = .false.
        length = 0
        do while(.not. end_of_line)
            read(unit=io_unit,fmt='(a)',iostat=io_err,size=size,advance='no') buffer
            if (io_err /= 0 .and. io_err /= IOSTAT_END .and. io_err /= IOSTAT_EOR) then
                ! unknown error
                error stop err_msg_io
            end if
            if (io_err == IOSTAT_END) then
                ! Unexpected end of file
                length = -1
                if (present(line)) line = " "
                return
            end if
            length = length + size
            if (present(line)) then
                if (length <= size) then
                    line = buffer(:size)
                else
                    if (length > len(line)) call reallocate_char(line,length)
                    line = line//buffer(:size)
                end if
            end if
            if (io_err == IOSTAT_EOR) end_of_line = .true.
        end do

        rewind(io_unit)

    end subroutine get_length_of_line_unit

    subroutine get_length_of_line_unit_buffer(io_unit,length,buffer_len,line)
        integer,intent(in) :: io_unit
        integer,intent(out) :: length
        integer,intent(in) :: buffer_len
        character(len=:),allocatable,intent(out),optional :: line

        character(len=:),allocatable :: buffer
        integer,parameter :: default_buffer_len = 20

        integer :: io_err, size
        logical :: end_of_line

        allocate(character(len=buffer_len) :: buffer)

        if (present(line)) then
            !allocate(character(len=1000) :: line)
            line = " "
        end if

        end_of_line = .false.
        length = 0
        do while(.not. end_of_line)
            read(unit=io_unit,fmt='(a)',iostat=io_err,size=size,advance='no') buffer
            if (io_err /= 0 .and. io_err /= IOSTAT_END .and. io_err /= IOSTAT_EOR) then
                ! unknown error
                error stop err_msg_io
            end if
            if (io_err == IOSTAT_END) then
                ! Unexpected end of file
                length = -1
                if (present(line)) line = " "
                return
            end if
            length = length + size
            if (present(line)) then
                if (length <= size) then
                    line = buffer(:size)
                else
                    if (length > len(line)) call reallocate_char(line,length)
                    line = line//buffer(:size)
                end if
            end if
            if (io_err == IOSTAT_EOR) end_of_line = .true.
        end do

        rewind(io_unit)

    end subroutine get_length_of_line_unit_buffer






    subroutine get_length_of_line_filename(filename,line_number,length,buffer_len,line,unit)
        character(len=*),intent(in) :: filename
        integer,intent(in) :: line_number
        integer,intent(out) :: length
        integer,intent(in),optional :: buffer_len
        character(len=:),allocatable,intent(out),optional :: line
        integer,intent(out),optional :: unit

        character(len=:),allocatable :: buffer, err_msg
        integer,parameter :: default_buffer_len = 20

        integer :: io_unit, io_err, size
        logical :: end_of_line

        integer :: i

        open(newunit=io_unit,file=trim(adjustl(filename)),status="old",action="read",iostat=io_err)
        if (io_err /= 0) then
            err_msg = err_msg_io_open//" "//trim(adjustl(filename))
            error stop err_msg
        end if

        if (present(buffer_len) .and. buffer_len > 0) then
            allocate(character(len=buffer_len) :: buffer)
        else
            allocate(character(len=default_buffer_len) :: buffer)
        end if

        if (present(line)) allocate(character(len=1000) :: line)

        ! skip to line_number
        do i=1,line_number-1
            read(unit=io_unit,fmt="(a)",iostat=io_err)
            if (io_err /= 0) then
                err_msg = err_msg_io_read//" "//trim(adjustl(filename))
                error stop err_msg
            end if 
        end do

        end_of_line = .false.
        length = 0
        do while(.not. end_of_line)
            read(unit=io_unit,fmt='(a)',iostat=io_err,size=size,advance='no') buffer
            if (io_err /= 0 .and. io_err /= IOSTAT_END .and. io_err /= IOSTAT_EOR) then
                ! unknown error
                error stop err_msg_io
            end if
            if (io_err == IOSTAT_END) then
                ! Unexpected end of file
                length = -1
                if (present(line)) line = ""
                return
            end if
            length = length + size
            if (length > len(line)) call reallocate_char(line,length)
            if (present(line)) line = trim(adjustl(line))//buffer(:size)
            if (io_err == IOSTAT_EOR) end_of_line = .true.

        end do

        if (present(unit)) then
            ! rewind and keep file open
            rewind(io_unit)
            unit = io_unit
        else
            ! close file
            close(io_unit)
        end if

    end subroutine get_length_of_line_filename

    subroutine reallocate_char(char,new_len)
        ! Helper procedure for get_length_of_file
        character(len=:),allocatable,intent(inout) :: char
        integer,intent(in) :: new_len

        character(len=len(char)) :: buffer

        buffer = char
        deallocate(char)
        allocate(character(len=new_len) :: char)

        char = buffer

    end subroutine reallocate_char



    pure function what_type(string) result(dtype)
        character(len=*),intent(in) :: string
        integer :: dtype

        integer :: rerr, ierr, lerr, cerr
        real(rk) :: rtest
        integer(ik) :: itest
        logical :: ltest
        complex(rk) :: ctest

        read(string,fmt=*,iostat=rerr) rtest
        read(string,fmt='(i10)',iostat=ierr) itest
        read(string,fmt=*,iostat=lerr) ltest
        read(string,fmt=*,iostat=cerr) ctest


        if (lerr == 0) then
            dtype = LOGICAL_NUM
        else if (ierr == 0) then
            dtype = INTEGER_NUM
        else if (rerr == 0) then
            dtype = REAL_NUM
        else if (cerr == 0) then
            dtype = COMPLEX_NUM
        else
            dtype = CHARACTER_NUM
        end if

    end function what_type




end module df_utils