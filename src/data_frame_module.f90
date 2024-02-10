module data_frame_module
    use,intrinsic :: iso_fortran_env, only: rk=>real64, ik=>int32
    implicit none
    private

    public :: data_frame

    integer,parameter,public :: INTEGER = 1,       &
                                REAL = 2,          &
                                LOGICAL = 3,       &
                                CHARACTER = 4,     &
                                COMPLEX = 5

    integer,parameter :: MAX_CHAR_LEN_DEFAULT = 100


! ~~~~~ COLUMN TYPE ~~~~~

    type :: column
        private
        
        integer :: dtype, n
        real(rk),dimension(:),allocatable :: rcol
        integer(ik),dimension(:),allocatable :: icol
        logical,dimension(:),allocatable :: lcol
        character(len=:),dimension(:),allocatable :: charcol
        complex(rk),dimension(:),allocatable :: ccol
    
    contains
        private

        procedure :: col_constructor_real,       &
                     col_constructor_integer,    &
                     col_constructor_logical,    &
                     col_constructor_character,  &
                     col_constructor_complex
        generic :: new => col_constructor_real,       &
                          col_constructor_integer,    &
                          col_constructor_logical,    &
                          col_constructor_character,  &
                          col_constructor_complex
        procedure :: destroy => col_destructor

        procedure :: get_type => get_from_col_dtype

        procedure :: get_from_col_real,         &
                     get_from_col_integer,      &
                     get_from_col_logical,      &
                     get_from_col_character,    &
                     get_from_col_complex
        procedure :: get_single_col_real,       &
                     get_single_col_integer,    &
                     get_single_col_logical,    &
                     get_single_col_character,  &
                     get_single_col_complex
        generic :: getr => get_from_col_real, get_single_col_real
        generic :: geti => get_from_col_integer, get_single_col_integer
        generic :: getl => get_from_col_logical, get_single_col_logical
        generic :: getch => get_from_col_character, get_single_col_character
        generic :: getc => get_from_col_complex, get_single_col_complex
        
        procedure :: changer => change_col_real
        procedure :: changei => change_col_integer
        procedure :: changel => change_col_logical
        procedure :: changech => change_col_character
        procedure :: changec => change_col_complex

    end type column




! ~~~~~ DATA FRAME TYPE ~~~~~

    type :: data_frame
        private

        integer :: n, col_size, max_char_len
        logical :: with_headers
        character(len=:),dimension(:),allocatable :: headers
        type(column),dimension(:),allocatable :: data_cols 

    contains
        private

        procedure,public :: new => df_constructor
        procedure,public :: destroy => df_destructor
        procedure :: add_col_real,      &
                     add_col_integer,   &
                     add_col_logical,   &
                     add_col_character, &
                     add_col_complex
        generic,public :: append => add_col_real, add_col_integer, add_col_logical,    &
                                    add_col_character, add_col_complex
        
        procedure,public :: getr => df_get_val_real
        procedure,public :: geti => df_get_val_integer
        procedure,public :: getl => df_get_val_logical
        procedure,public :: getch => df_get_val_character
        procedure,public :: getc => df_get_val_complex
    end type data_frame



contains


! ~~~~ DF Constructor

    subroutine df_constructor(this,char_len)
        class(data_frame),intent(inout) :: this
        integer,intent(in),optional :: char_len

        if (present(char_len)) then
            this%max_char_len = char_len
        else
            this%max_char_len = MAX_CHAR_LEN_DEFAULT
        end if

        this%n = 0
        this%col_size = -1  ! indicate no col_size

    end subroutine


! ~~~~ DF Destructor

    subroutine df_destructor(this)
        class(data_frame),intent(inout) :: this

        integer :: i

        do i=1,this%n
            call this%data_cols(i)%destroy()
        end do

        if (allocated(this%headers)) deallocate(this%headers)

    end subroutine df_destructor


! ~~~~ Add Column to DF

    subroutine add_col_real(this,col,header)
        class(data_frame),intent(inout) :: this
        real(rk),dimension(:),intent(in) :: col
        character(len=*),intent(in),optional :: header

        type(column),dimension(:),allocatable :: new_cols
        character(len=:),dimension(:),allocatable :: new_headers
        integer :: n

        if (this%col_size < 0) then
            this%col_size = size(col,dim=1)
        else if (this%col_size /= size(col,dim=1)) then
            error stop 'Different size columns in add col to data_frame'
        end if

        n = this%n
        if (n > 0) then
            this%n = n + 1
            allocate(new_cols(n+1))
            new_cols(1:n) = this%data_cols
            call new_cols(n+1)%new(col)
            this%data_cols = new_cols
            if (present(header)) then
                if (this%with_headers) then
                    allocate(character(this%max_char_len) :: new_headers(n+1))
                    new_headers(1:n) = this%headers
                    new_headers(n+1) = header
                    this%headers = new_headers
                else
                    error stop 'attempt to add headers to data frame that does not have headers'
                end if
            else
                if (this%with_headers) error stop 'if data frame has headers, all columns must have headers'
            end if
        else
            this%n = 1
            allocate(this%data_cols(1))
            call this%data_cols(1)%new(col)
            if (present(header)) then
                allocate(character(this%max_char_len) :: this%headers(1))
                this%headers(1) = header
                this%with_headers = .true.
            else
                this%with_headers = .false.
            end if
        end if

    end subroutine add_col_real

    subroutine add_col_integer(this,col,header)
        class(data_frame),intent(inout) :: this
        integer(ik),dimension(:),intent(in) :: col
        character(len=*),intent(in),optional :: header

        type(column),dimension(:),allocatable :: new_cols
        character(len=:),dimension(:),allocatable :: new_headers
        integer :: n

        if (this%col_size < 0) then
            this%col_size = size(col,dim=1)
        else if (this%col_size /= size(col,dim=1)) then
            error stop 'Different size columns in add col to data_frame'
        end if

        n = this%n
        if (n > 0) then
            this%n = n + 1
            allocate(new_cols(n+1))
            new_cols(1:n) = this%data_cols
            call new_cols(n+1)%new(col)
            this%data_cols = new_cols
            if (present(header)) then
                if (this%with_headers) then
                    allocate(character(this%max_char_len) :: new_headers(n+1))
                    new_headers(1:n) = this%headers
                    new_headers(n+1) = header
                    this%headers = new_headers
                else
                    error stop 'attempt to add headers to data frame that does not have headers'
                end if
            else
                if (this%with_headers) error stop 'if data frame has headers, all columns must have headers'
            end if
        else
            this%n = 1
            allocate(this%data_cols(1))
            call this%data_cols(1)%new(col)
            if (present(header)) then
                allocate(character(this%max_char_len) :: this%headers(1))
                this%headers(1) = header
                this%with_headers = .true.
            else
                this%with_headers = .false.
            end if
        end if

    end subroutine add_col_integer

    subroutine add_col_logical(this,col,header)
        class(data_frame),intent(inout) :: this
        logical,dimension(:),intent(in) :: col
        character(len=*),intent(in),optional :: header

        type(column),dimension(:),allocatable :: new_cols
        character(len=:),dimension(:),allocatable :: new_headers
        integer :: n

        if (this%col_size < 0) then
            this%col_size = size(col,dim=1)
        else if (this%col_size /= size(col,dim=1)) then
            error stop 'Different size columns in add col to data_frame'
        end if

        n = this%n
        if (n > 0) then
            this%n = n + 1
            allocate(new_cols(n+1))
            new_cols(1:n) = this%data_cols
            call new_cols(n+1)%new(col)
            this%data_cols = new_cols
            if (present(header)) then
                if (this%with_headers) then
                    allocate(character(this%max_char_len) :: new_headers(n+1))
                    new_headers(1:n) = this%headers
                    new_headers(n+1) = header
                    this%headers = new_headers
                else
                    error stop 'attempt to add headers to data frame that does not have headers'
                end if
            else
                if (this%with_headers) error stop 'if data frame has headers, all columns must have headers'
            end if
        else
            this%n = 1
            allocate(this%data_cols(1))
            call this%data_cols(1)%new(col)
            if (present(header)) then
                allocate(character(this%max_char_len) :: this%headers(1))
                this%headers(1) = header
                this%with_headers = .true.
            else
                this%with_headers = .false.
            end if
        end if

    end subroutine add_col_logical

    subroutine add_col_character(this,col,header)
        class(data_frame),intent(inout) :: this
        character(len=*),dimension(:),intent(in) :: col
        character(len=*),intent(in),optional :: header

        type(column),dimension(:),allocatable :: new_cols
        character(len=:),dimension(:),allocatable :: new_headers
        integer :: n

        if (this%col_size < 0) then
            this%col_size = size(col,dim=1)
        else if (this%col_size /= size(col,dim=1)) then
            error stop 'Different size columns in add col to data_frame'
        end if

        n = this%n
        if (n > 0) then
            this%n = n + 1
            allocate(new_cols(n+1))
            new_cols(1:n) = this%data_cols
            call new_cols(n+1)%new(col)
            this%data_cols = new_cols
            if (present(header)) then
                if (this%with_headers) then
                    allocate(character(this%max_char_len) :: new_headers(n+1))
                    new_headers(1:n) = this%headers
                    new_headers(n+1) = header
                    this%headers = new_headers
                else
                    error stop 'attempt to add headers to data frame that does not have headers'
                end if
            else
                if (this%with_headers) error stop 'if data frame has headers, all columns must have headers'
            end if
        else
            this%n = 1
            allocate(this%data_cols(1))
            call this%data_cols(1)%new(col)
            if (present(header)) then
                allocate(character(this%max_char_len) :: this%headers(1))
                this%headers(1) = header
                this%with_headers = .true.
            else
                this%with_headers = .false.
            end if
        end if

    end subroutine add_col_character

    subroutine add_col_complex(this,col,header)
        class(data_frame),intent(inout) :: this
        complex(rk),dimension(:),intent(in) :: col
        character(len=*),intent(in),optional :: header

        type(column),dimension(:),allocatable :: new_cols
        character(len=:),dimension(:),allocatable :: new_headers
        integer :: n

        if (this%col_size < 0) then
            this%col_size = size(col,dim=1)
        else if (this%col_size /= size(col,dim=1)) then
            error stop 'Different size columns in add col to data_frame'
        end if

        n = this%n
        if (n > 0) then
            this%n = n + 1
            allocate(new_cols(n+1))
            new_cols(1:n) = this%data_cols
            call new_cols(n+1)%new(col)
            this%data_cols = new_cols
            if (present(header)) then
                if (this%with_headers) then
                    allocate(character(this%max_char_len) :: new_headers(n+1))
                    new_headers(1:n) = this%headers
                    new_headers(n+1) = header
                    this%headers = new_headers
                else
                    error stop 'attempt to add headers to data frame that does not have headers'
                end if
            else
                if (this%with_headers) error stop 'if data frame has headers, all columns must have headers'
            end if
        else
            this%n = 1
            allocate(this%data_cols(1))
            call this%data_cols(1)%new(col)
            if (present(header)) then
                allocate(character(this%max_char_len) :: this%headers(1))
                this%headers(1) = header
                this%with_headers = .true.
            else
                this%with_headers = .false.
            end if
        end if

    end subroutine add_col_complex


! ~~~~ Get Single Val DF

    pure function df_get_val_real(this,i,j) result(val)
        class(data_frame),intent(in) :: this
        integer,intent(in) :: i,j
        real(rk) :: val

        val = this%data_cols(i)%getr(j)

    end function df_get_val_real

    pure function df_get_val_integer(this,i,j) result(val)
        class(data_frame),intent(in) :: this
        integer,intent(in) :: i,j
        integer(ik) :: val

        val = this%data_cols(i)%geti(j)

    end function df_get_val_integer
    
    pure function df_get_val_logical(this,i,j) result(val)
        class(data_frame),intent(in) :: this
        integer,intent(in) :: i,j
        logical :: val

        val = this%data_cols(i)%getl(j)

    end function df_get_val_logical

    pure function df_get_val_character(this,i,j) result(val)
        class(data_frame),intent(in) :: this
        integer,intent(in) :: i,j
        character(len=:),allocatable :: val

        val = this%data_cols(i)%getch(j)

    end function df_get_val_character

    pure function df_get_val_complex(this,i,j) result(val)
        class(data_frame),intent(in) :: this
        integer,intent(in) :: i,j
        complex(rk) :: val

        val = this%data_cols(i)%getc(j)

    end function df_get_val_complex







! ~~~~ Column Constructor/Setter

    subroutine col_constructor_real(this,dcol)
        class(column),intent(inout) :: this
        real(rk),dimension(:),intent(in) :: dcol

        this%dtype = REAL
        this%n = size(dcol,dim=1)
        allocate(this%rcol(this%n))
        this%rcol = dcol

    end subroutine col_constructor_real

    subroutine col_constructor_integer(this,dcol)
        class(column),intent(inout) :: this
        integer(ik),dimension(:),intent(in) :: dcol

        this%dtype = INTEGER
        this%n = size(dcol,dim=1)
        allocate(this%icol(this%n))
        this%icol = dcol

    end subroutine col_constructor_integer

    subroutine col_constructor_logical(this,dcol)
        class(column),intent(inout) :: this
        logical,dimension(:),intent(in) :: dcol

        this%dtype = LOGICAL
        this%n = size(dcol,dim=1)
        allocate(this%lcol(this%n))
        this%lcol = dcol

    end subroutine col_constructor_logical

    subroutine col_constructor_character(this,dcol)
        class(column),intent(inout) :: this
        character(len=*),dimension(:),intent(in) :: dcol

        integer :: elem_len

        this%n = size(dcol,dim=1)
        elem_len = len(dcol(1))

        this%dtype = CHARACTER
        allocate(character(elem_len) :: this%charcol(this%n))
        this%charcol = dcol

    end subroutine col_constructor_character

    subroutine col_constructor_complex(this,dcol)
        class(column),intent(inout) :: this
        complex(rk),dimension(:),intent(in) :: dcol

        this%dtype = COMPLEX
        this%n = size(dcol,dim=1)
        allocate(this%ccol(this%n))
        this%ccol = dcol

    end subroutine col_constructor_complex


! ~~~~ Column Destructor

    subroutine col_destructor(this)
        class(column),intent(inout) :: this

        if (allocated(this%rcol)) deallocate(this%rcol)
        if (allocated(this%icol)) deallocate(this%icol)
        if (allocated(this%lcol)) deallocate(this%lcol)
        if (allocated(this%ccol)) deallocate(this%ccol)
        if (allocated(this%charcol)) deallocate(this%charcol)

    end subroutine col_destructor


! ~~~~ Get Data Column from Column

    pure function get_from_col_real(this) result(col)
        class(column),intent(in) :: this
        real(rk),dimension(this%n) :: col

        if (this%dtype /= REAL) error stop 'column is not of type real'

        col = this%rcol

    end function get_from_col_real

    pure function get_from_col_integer(this) result(col)
        class(column),intent(in) :: this
        integer(ik),dimension(this%n) :: col

        if (this%dtype /= INTEGER) error stop 'column is not of type integer'

        col = this%icol

    end function get_from_col_integer

    pure function get_from_col_logical(this) result(col)
        class(column),intent(in) :: this
        logical,dimension(this%n) :: col

        if (this%dtype /= LOGICAL) error stop 'column is not of type logical'

        col = this%lcol

    end function get_from_col_logical

    pure function get_from_col_character(this) result(col)
        class(column),intent(in) :: this
        character(len=:),dimension(:),allocatable :: col

        integer :: arr_size, elem_len

        if (this%dtype /= CHARACTER) error stop 'column is not of type character'

        ! will cause segfault if col not char type
        arr_size = size(this%charcol,dim=1)
        elem_len = len(this%charcol(1))
        allocate(character(elem_len) :: col(arr_size))

        col = this%charcol

    end function get_from_col_character

    pure function get_from_col_complex(this) result(col)
        class(column),intent(in) :: this
        complex(rk),dimension(this%n) :: col

        if (this%dtype /= COMPLEX) error stop 'column is not of type complex'

        col = this%ccol

    end function get_from_col_complex


! ~~~~ Get Data Type Column

    pure function get_from_col_dtype(this) result(dtype)
        class(column),intent(in) :: this
        integer :: dtype

        dtype = this%dtype

    end function get_from_col_dtype


! ~~~~ Get Single Val Column

    pure function get_single_col_real(this,i) result(val)
        class(column),intent(in) :: this
        integer,intent(in) :: i
        real(rk) :: val

        if (this%dtype /= REAL) error stop 'column is not of type real'

        if (i > this%n) error stop 'out of bounds attempt on data column'

        val = this%rcol(i)

    end function get_single_col_real

    pure function get_single_col_integer(this,i) result(val)
        class(column),intent(in) :: this
        integer,intent(in) :: i
        integer(ik) :: val

        if (this%dtype /= INTEGER) error stop 'column is not of type integer'

        if (i > this%n) error stop 'out of bounds attempt on data column'

        val = this%icol(i)

    end function get_single_col_integer

    pure function get_single_col_logical(this,i) result(val)
        class(column),intent(in) :: this
        integer,intent(in) :: i
        logical :: val

        if (this%dtype /= LOGICAL) error stop 'column is not of type logical'

        if (i > this%n) error stop 'out of bounds attempt on data column'

        val = this%lcol(i)

    end function get_single_col_logical

    pure function get_single_col_character(this,i) result(val)
        class(column),intent(in) :: this
        integer,intent(in) :: i
        character(len=:),allocatable :: val

        if (this%dtype /= CHARACTER) error stop 'column is not of type character'

        if (i > this%n) error stop 'out of bounds attempt on data column'

        val = this%charcol(i)

    end function get_single_col_character

    pure function get_single_col_complex(this,i) result(val)
        class(column),intent(in) :: this
        integer,intent(in) :: i
        complex(rk) :: val

        if (this%dtype /= COMPLEX) error stop 'column is not of type complex'

        if (i > this%n) error stop 'out of bounds attempt on data column'

        val = this%ccol(i)

    end function get_single_col_complex


! ~~~~ Change Single Val Column

    subroutine change_col_real(this,i,val)
        class(column),intent(inout) :: this
        integer,intent(in) :: i
        real(rk),intent(in) :: val

        if (this%dtype /= REAL) error stop 'column is not of type real'

        this%rcol(i) = val

    end subroutine change_col_real

    subroutine change_col_integer(this,i,val)
        class(column),intent(inout) :: this
        integer,intent(in) :: i
        integer(ik),intent(in) :: val

        if (this%dtype /= INTEGER) error stop 'column is not of type integer'

        this%icol(i) = val

    end subroutine change_col_integer

    subroutine change_col_logical(this,i,val)
        class(column),intent(inout) :: this
        integer,intent(in) :: i
        logical,intent(in) :: val

        if (this%dtype /= LOGICAL) error stop 'column is not of type logical'

        this%lcol(i) = val

    end subroutine change_col_logical

    subroutine change_col_character(this,i,val)
        class(column),intent(inout) :: this
        integer,intent(in) :: i
        character(len=*),intent(in) :: val

        if (this%dtype /= CHARACTER) error stop 'column is not of type character'

        this%charcol(i) = val

    end subroutine change_col_character

    subroutine change_col_complex(this,i,val)
        class(column),intent(inout) :: this
        integer,intent(in) :: i
        complex(rk),intent(in) :: val

        if (this%dtype /= COMPLEX) error stop 'column is not of type complex'

        this%ccol(i) = val

    end subroutine change_col_complex







    ! subroutine read_mesa_history(filename,data)
    !     character(len=*),intent(in) :: filename
    !     real(dp),dimension(:,:),allocatable,intent(out) :: data
    !     integer,dimension(:),intent(in),optional :: cols

    !     logical :: return_all_data
    !     character(len=:),allocatable :: err_msg, line
    !     integer :: history_unit, io_err
    !     integer :: i, header_lines, num_cols, num_lines

    !     header_lines = 6

    !     return_all_data = .not. present(cols) ! return all data unless specific cols are set

    !     open(unit=history_unit,file=trim(adjustl(filename)),iostat=io_err,status="old",action="read")
    !     if (io_err /= 0) then
    !         err_msg = err_msg_io_open//" file "//trim(adjustl(filename))
    !         error stop err_msg
    !     end if


    !     call get_num_lines(history_unit,num_lines)
    !     num_lines = num_lines - 1 ! since last line is always empty

    !     ! skip header
    !     allocate(character(len=10000) :: line) ! may cause problems
    !     do i=1,header_lines
    !         read(unit=history_unit,fmt='(a)') line
    !     end do
    !     num_cols = size(split(line,' '),dim=1)

    !     allocate(data(num_lines-header_lines,num_cols))

    !     do i=1,size(data,dim=1)
    !         read(unit=history_unit,fmt=*,iostat=io_err) data(i,:)
    !         if (io_err /= 0 .and. i /= num_lines-header_lines) then
    !             err_msg = err_msg_io_read//" file "//trim(adjustl(filename))
    !             print*, i
    !             error stop err_msg
    !         end if
    !     end do

    !     close(history_unit)

    ! end subroutine read_mesa_history


end module data_frame_module
