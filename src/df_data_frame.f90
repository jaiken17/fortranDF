module df_data_frame
    use,intrinsic :: iso_fortran_env, only: STD_OUT => output_unit
    use df_precision
    use df_types
    use df_column_class
    use df_utils
    use split_mod
    implicit none
    private

    public :: data_frame

    
    integer,parameter :: MAX_CHAR_LEN_DEFAULT = 100



! ~~~~~ DATA FRAME TYPE ~~~~~

    type :: data_frame
        private
        
        integer :: n, col_size, max_char_len
        logical :: with_headers
        character(len=:),dimension(:),allocatable :: headers
        type(column),dimension(:),allocatable :: data_cols 

        logical :: initialized = .false.

    contains
        private

        procedure,public :: new => df_constructor
        procedure,public :: destroy => df_destructor

        procedure,public :: ncols => df_get_num_cols
        procedure,public :: nrows => df_get_num_rows

        procedure :: df_get_col_type_header, df_get_col_type_index
        generic,public :: dtype => df_get_col_type_header, df_get_col_type_index

        procedure :: already_header
        procedure :: add_col_real,      &
                     add_col_integer,   &
                     add_col_logical,   &
                     add_col_character, &
                     add_col_complex
        generic,public :: append => add_col_real, add_col_integer, add_col_logical,    &
                                    add_col_character, add_col_complex
        ! Public?
        procedure :: append_emptyr => add_empty_col_real
        procedure :: append_emptyi => add_empty_col_integer
        procedure :: append_emptyl => add_empty_col_logical
        procedure :: append_emptych => add_empty_col_character
        procedure :: append_emptyc => add_empty_col_complex

        
        procedure :: df_get_val_real,       &
                     df_get_val_integer,    &
                     df_get_val_logical,    &
                     df_get_val_character,  &
                     df_get_val_complex
        procedure :: df_get_val_header_real,       &
                     df_get_val_header_integer,    &
                     df_get_val_header_logical,    &
                     df_get_val_header_character,  &
                     df_get_val_header_complex
        procedure :: df_get_col_ind_real,       &
                     df_get_col_ind_integer,    &
                     df_get_col_ind_logical,    &
                     df_get_col_ind_character,  &
                     df_get_col_ind_complex
        procedure :: df_get_col_header_real,        &
                     df_get_col_header_integer,     &
                     df_get_col_header_logical,     &
                     df_get_col_header_character,   &
                     df_get_col_header_complex

        generic,public :: getr => df_get_val_real, df_get_val_header_real,              &
                                  df_get_col_ind_real, df_get_col_header_real
        generic,public :: geti => df_get_val_integer, df_get_val_header_integer,        &
                                  df_get_col_ind_integer, df_get_col_header_integer
        generic,public :: getl => df_get_val_logical, df_get_val_header_logical,        &
                                  df_get_col_ind_logical, df_get_col_header_logical
        generic,public :: getch => df_get_val_character, df_get_val_header_character,   &
                                   df_get_col_ind_character, df_get_col_header_character
        generic,public :: getc => df_get_val_complex, df_get_val_header_complex,        &
                                  df_get_col_ind_complex, df_get_col_header_complex

        procedure :: df_change_single_indices_real,         &
                     df_change_single_indices_integer,      &
                     df_change_single_indices_logical,      &
                     df_change_single_indices_character,    &
                     df_change_single_indices_complex
        procedure :: df_change_single_header_real,         &
                     df_change_single_header_integer,      &
                     df_change_single_header_logical,      &
                     df_change_single_header_character,    &
                     df_change_single_header_complex
        procedure :: df_change_col_index_real,      &
                     df_change_col_index_integer,   &
                     df_change_col_index_logical,   &
                     df_change_col_index_character, &
                     df_change_col_index_complex
        procedure :: df_change_col_header_real,         &
                     df_change_col_header_integer,      &
                     df_change_col_header_logical,      &
                     df_change_col_header_character,    &
                     df_change_col_header_complex

        generic,public :: setr => df_change_single_header_real, df_change_single_indices_real,              &
                                  df_change_col_index_real, df_change_col_header_real
        generic,public :: seti => df_change_single_header_integer, df_change_single_indices_integer,        &
                                  df_change_col_index_integer, df_change_col_header_integer
        generic,public :: setl => df_change_single_header_logical, df_change_single_indices_logical,        &
                                  df_change_col_index_logical, df_change_col_header_logical
        generic,public :: setch => df_change_single_header_character, df_change_single_indices_character,   &
                                   df_change_col_index_character, df_change_col_header_character
        generic,public :: setc => df_change_single_header_complex, df_change_single_indices_complex,        &
                                  df_change_col_index_complex, df_change_col_header_complex

        procedure :: df_write_unformatted
        generic,public :: write => df_write_unformatted

        procedure :: df_read_df_file
        generic,public :: read => df_read_df_file
    end type data_frame



contains


! ~~~~ DF Constructor

    subroutine df_constructor(this,char_len)
        class(data_frame),intent(inout) :: this
        integer,intent(in),optional :: char_len

        if (this%initialized) call this%destroy()
        this%initialized = .true.

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

        if (this%n >= 1) deallocate(this%data_cols)

        if (allocated(this%headers)) deallocate(this%headers)

        this%n = 0
        this%col_size = -1

        this%initialized = .false.

    end subroutine df_destructor


! ~~~~ DF Inquiry functions

    pure function df_get_num_cols(this) result(n)
        class(data_frame),intent(in) :: this
        integer :: n

        n = this%n

    end function df_get_num_cols

    pure function df_get_num_rows(this) result(num_rows)
        class(data_frame),intent(in) :: this
        integer :: num_rows

        num_rows = this%col_size

    end function df_get_num_rows

    pure function df_get_col_type_header(this,header) result(dtype)
        class(data_frame),intent(in) :: this
        character(len=*),intent(in) :: header
        integer :: dtype

        integer :: ind
        character(len=:),allocatable :: trunc_header

        if (.not. this%with_headers) error stop "data frame has no headers to look up"

        allocate(character(len(this%headers(1))) :: trunc_header)
        trunc_header = trim(adjustl(header))
        ind = findloc(this%headers,trunc_header,dim=1)
        if (ind < 1) error stop 'header not present in data frame'

        dtype = this%data_cols(ind)%dtype

    end function df_get_col_type_header

    pure function df_get_col_type_index(this,j) result(dtype)
        class(data_frame),intent(in) :: this
        integer,intent(in) :: j
        integer :: dtype

        dtype = this%data_cols(j)%dtype

    end function df_get_col_type_index



! ~~~~ Add Column to DF

    subroutine add_col_real(this,col,header)
        class(data_frame),intent(inout) :: this
        real(rk),dimension(:),intent(in) :: col
        character(len=*),intent(in),optional :: header

        type(column),dimension(:),allocatable :: new_cols
        character(len=:),dimension(:),allocatable :: old_headers
        integer :: n

        if (.not. this%initialized) call this%new()

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
                    if (this%already_header(header)) error stop 'all headers must be unique'
                    old_headers = this%headers
                    deallocate(this%headers)
                    allocate(character(this%max_char_len) :: this%headers(n+1))
                    this%headers(1:n) = old_headers
                    this%headers(n+1) = trim(adjustl(header))
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
        character(len=:),dimension(:),allocatable :: old_headers
        integer :: n

        if (.not. this%initialized) call this%new()

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
                    if (this%already_header(header)) error stop 'all headers must be unique'
                    old_headers = this%headers
                    deallocate(this%headers)
                    allocate(character(this%max_char_len) :: this%headers(n+1))
                    this%headers(1:n) = old_headers
                    this%headers(n+1) = trim(adjustl(header))
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
        character(len=:),dimension(:),allocatable :: old_headers
        integer :: n

        if (.not. this%initialized) call this%new()

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
                    if (this%already_header(header)) error stop 'all headers must be unique'
                    old_headers = this%headers
                    deallocate(this%headers)
                    allocate(character(this%max_char_len) :: this%headers(n+1))
                    this%headers(1:n) = old_headers
                    this%headers(n+1) = trim(adjustl(header))
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
        character(len=:),dimension(:),allocatable :: old_headers
        integer :: n

        if (.not. this%initialized) call this%new()

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
                    if (this%already_header(header)) error stop 'all headers must be unique'
                    old_headers = this%headers
                    deallocate(this%headers)
                    allocate(character(this%max_char_len) :: this%headers(n+1))
                    this%headers(1:n) = old_headers
                    this%headers(n+1) = trim(adjustl(header))
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
        character(len=:),dimension(:),allocatable :: old_headers
        integer :: n

        if (.not. this%initialized) call this%new()

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
                    if (this%already_header(header)) error stop 'all headers must be unique'
                    old_headers = this%headers
                    deallocate(this%headers)
                    allocate(character(this%max_char_len) :: this%headers(n+1))
                    this%headers(1:n) = old_headers
                    this%headers(n+1) = trim(adjustl(header))
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

! ~~~~ Add empty Col to DF

    subroutine add_empty_col_real(this,col_size,header)
        class(data_frame),intent(inout) :: this
        integer,intent(in) :: col_size
        character(len=*),intent(in),optional :: header

        type(column),dimension(:),allocatable :: new_cols
        character(len=:),dimension(:),allocatable :: old_headers
        integer :: n


        if (.not. this%initialized) call this%new()
        
        if (this%col_size < 0) then
            this%col_size = col_size
        else
            if (this%col_size /= col_size) error stop "cannot allocate a column with a different size to rest of data frame"
        end if

        n = this%n
        if (n > 0) then
            this%n = n + 1
            allocate(new_cols(n+1))
            new_cols(1:n) = this%data_cols
            call new_cols(n+1)%emptyr(this%col_size)
            this%data_cols = new_cols
            if (present(header)) then
                if (this%with_headers) then
                    if (this%already_header(header)) error stop 'all headers must be unique'
                    old_headers = this%headers
                    deallocate(this%headers)
                    allocate(character(this%max_char_len) :: this%headers(n+1))
                    this%headers(1:n) = old_headers
                    this%headers(n+1) = trim(adjustl(header))
                else
                    error stop 'attempt to add headers to data frame that does not have headers'
                end if
            else
                if (this%with_headers) error stop 'if data frame has headers, all columns must have headers'
            end if
        else
            this%n = 1
            allocate(this%data_cols(1))
            call this%data_cols(1)%emptyr(this%col_size)
            if (present(header)) then
                allocate(character(this%max_char_len) :: this%headers(1))
                this%headers(1) = header
                this%with_headers = .true.
            else
                this%with_headers = .false.
            end if
        end if

    end subroutine add_empty_col_real

    subroutine add_empty_col_integer(this,col_size,header)
        class(data_frame),intent(inout) :: this
        integer,intent(in) :: col_size
        character(len=*),intent(in),optional :: header

        type(column),dimension(:),allocatable :: new_cols
        character(len=:),dimension(:),allocatable :: old_headers
        integer :: n


        if (.not. this%initialized) call this%new()
        
        if (this%col_size < 0) then
            this%col_size = col_size
        else
            if (this%col_size /= col_size) error stop "cannot allocate a column with a different size to rest of data frame"
        end if

        n = this%n
        if (n > 0) then
            this%n = n + 1
            allocate(new_cols(n+1))
            new_cols(1:n) = this%data_cols
            call new_cols(n+1)%emptyi(this%col_size)
            this%data_cols = new_cols
            if (present(header)) then
                if (this%with_headers) then
                    if (this%already_header(header)) error stop 'all headers must be unique'
                    old_headers = this%headers
                    deallocate(this%headers)
                    allocate(character(this%max_char_len) :: this%headers(n+1))
                    this%headers(1:n) = old_headers
                    this%headers(n+1) = trim(adjustl(header))
                else
                    error stop 'attempt to add headers to data frame that does not have headers'
                end if
            else
                if (this%with_headers) error stop 'if data frame has headers, all columns must have headers'
            end if
        else
            this%n = 1
            allocate(this%data_cols(1))
            call this%data_cols(1)%emptyi(this%col_size)
            if (present(header)) then
                allocate(character(this%max_char_len) :: this%headers(1))
                this%headers(1) = header
                this%with_headers = .true.
            else
                this%with_headers = .false.
            end if
        end if

    end subroutine add_empty_col_integer

    subroutine add_empty_col_logical(this,col_size,header)
        class(data_frame),intent(inout) :: this
        integer,intent(in) :: col_size
        character(len=*),intent(in),optional :: header

        type(column),dimension(:),allocatable :: new_cols
        character(len=:),dimension(:),allocatable :: old_headers
        integer :: n


        if (.not. this%initialized) call this%new()
        
        if (this%col_size < 0) then
            this%col_size = col_size
        else
            if (this%col_size /= col_size) error stop "cannot allocate a column with a different size to rest of data frame"
        end if

        n = this%n
        if (n > 0) then
            this%n = n + 1
            allocate(new_cols(n+1))
            new_cols(1:n) = this%data_cols
            call new_cols(n+1)%emptyl(this%col_size)
            this%data_cols = new_cols
            if (present(header)) then
                if (this%with_headers) then
                    if (this%already_header(header)) error stop 'all headers must be unique'
                    old_headers = this%headers
                    deallocate(this%headers)
                    allocate(character(this%max_char_len) :: this%headers(n+1))
                    this%headers(1:n) = old_headers
                    this%headers(n+1) = trim(adjustl(header))
                else
                    error stop 'attempt to add headers to data frame that does not have headers'
                end if
            else
                if (this%with_headers) error stop 'if data frame has headers, all columns must have headers'
            end if
        else
            this%n = 1
            allocate(this%data_cols(1))
            call this%data_cols(1)%emptyl(this%col_size)
            if (present(header)) then
                allocate(character(this%max_char_len) :: this%headers(1))
                this%headers(1) = header
                this%with_headers = .true.
            else
                this%with_headers = .false.
            end if
        end if

    end subroutine add_empty_col_logical

    subroutine add_empty_col_character(this,col_size,header)
        class(data_frame),intent(inout) :: this
        integer,intent(in) :: col_size
        character(len=*),intent(in),optional :: header

        type(column),dimension(:),allocatable :: new_cols
        character(len=:),dimension(:),allocatable :: old_headers
        integer :: n


        if (.not. this%initialized) call this%new()
        
        if (this%col_size < 0) then
            this%col_size = col_size
        else
            if (this%col_size /= col_size) error stop "cannot allocate a column with a different size to rest of data frame"
        end if

        n = this%n
        if (n > 0) then
            this%n = n + 1
            allocate(new_cols(n+1))
            new_cols(1:n) = this%data_cols
            call new_cols(n+1)%emptych(this%col_size)
            this%data_cols = new_cols
            if (present(header)) then
                if (this%with_headers) then
                    if (this%already_header(header)) error stop 'all headers must be unique'
                    old_headers = this%headers
                    deallocate(this%headers)
                    allocate(character(this%max_char_len) :: this%headers(n+1))
                    this%headers(1:n) = old_headers
                    this%headers(n+1) = trim(adjustl(header))
                else
                    error stop 'attempt to add headers to data frame that does not have headers'
                end if
            else
                if (this%with_headers) error stop 'if data frame has headers, all columns must have headers'
            end if
        else
            this%n = 1
            allocate(this%data_cols(1))
            call this%data_cols(1)%emptych(this%col_size)
            if (present(header)) then
                allocate(character(this%max_char_len) :: this%headers(1))
                this%headers(1) = header
                this%with_headers = .true.
            else
                this%with_headers = .false.
            end if
        end if

    end subroutine add_empty_col_character

    subroutine add_empty_col_complex(this,col_size,header)
        class(data_frame),intent(inout) :: this
        integer,intent(in) :: col_size
        character(len=*),intent(in),optional :: header

        type(column),dimension(:),allocatable :: new_cols
        character(len=:),dimension(:),allocatable :: old_headers
        integer :: n


        if (.not. this%initialized) call this%new()
        
        if (this%col_size < 0) then
            this%col_size = col_size
        else
            if (this%col_size /= col_size) error stop "cannot allocate a column with a different size to rest of data frame"
        end if

        n = this%n
        if (n > 0) then
            this%n = n + 1
            allocate(new_cols(n+1))
            new_cols(1:n) = this%data_cols
            call new_cols(n+1)%emptyc(this%col_size)
            this%data_cols = new_cols
            if (present(header)) then
                if (this%with_headers) then
                    if (this%already_header(header)) error stop 'all headers must be unique'
                    old_headers = this%headers
                    deallocate(this%headers)
                    allocate(character(this%max_char_len) :: this%headers(n+1))
                    this%headers(1:n) = old_headers
                    this%headers(n+1) = trim(adjustl(header))
                else
                    error stop 'attempt to add headers to data frame that does not have headers'
                end if
            else
                if (this%with_headers) error stop 'if data frame has headers, all columns must have headers'
            end if
        else
            this%n = 1
            allocate(this%data_cols(1))
            call this%data_cols(1)%emptyc(this%col_size)
            if (present(header)) then
                allocate(character(this%max_char_len) :: this%headers(1))
                this%headers(1) = header
                this%with_headers = .true.
            else
                this%with_headers = .false.
            end if
        end if

    end subroutine add_empty_col_complex


! ~~~~ Check if header is not unique

    !pure function already_header(this, header)
    function already_header(this, header)
        class(data_frame),intent(in) :: this
        character(len=*),intent(in) :: header
        logical :: already_header

        character(len=this%max_char_len) :: trunc_header

        trunc_header = trim(adjustl(header))
        if (findloc(this%headers,trunc_header,dim=1) > 0) then
            already_header = .true.
        else
            already_header = .false.
        end if

    end function already_header


! ~~~~ Get Column DF with index

    pure function df_get_col_ind_real(this,i) result(col)
        class(data_frame),intent(in) :: this
        integer,intent(in) :: i
        real(rk),dimension(this%col_size) :: col

        col = this%data_cols(i)%getr()

    end function df_get_col_ind_real

    pure function df_get_col_ind_integer(this,i) result(col)
        class(data_frame),intent(in) :: this
        integer,intent(in) :: i
        integer(ik),dimension(this%col_size) :: col

        col = this%data_cols(i)%geti()

    end function df_get_col_ind_integer

    pure function df_get_col_ind_logical(this,i) result(col)
        class(data_frame),intent(in) :: this
        integer,intent(in) :: i
        logical,dimension(this%col_size) :: col

        col = this%data_cols(i)%getl()

    end function df_get_col_ind_logical

    pure function df_get_col_ind_character(this,i) result(col)
        class(data_frame),intent(in) :: this
        integer,intent(in) :: i
        character(len=:),dimension(:),allocatable :: col

        col = this%data_cols(i)%getch()

    end function df_get_col_ind_character

    pure function df_get_col_ind_complex(this,i) result(col)
        class(data_frame),intent(in) :: this
        integer,intent(in) :: i
        complex(rk),dimension(this%col_size) :: col

        col = this%data_cols(i)%getc()

    end function df_get_col_ind_complex


! ~~~~ Get Column DF from header

    pure function df_get_col_header_real(this,header) result(col)
        class(data_frame),intent(in) :: this
        character(len=*),intent(in) :: header
        real(rk),dimension(this%col_size) :: col

        integer :: ind
        character(len=:),allocatable :: trunc_header

        if (.not. this%with_headers) error stop "data frame has no headers to look up"

        allocate(character(len(this%headers(1))) :: trunc_header)
        trunc_header = trim(adjustl(header))
        ind = findloc(this%headers,trunc_header,dim=1)
        if (ind < 1) error stop 'header not present in data frame'

        col = this%data_cols(ind)%getr()

    end function df_get_col_header_real

    pure function df_get_col_header_integer(this,header) result(col)
        class(data_frame),intent(in) :: this
        character(len=*),intent(in) :: header
        integer(ik),dimension(this%col_size) :: col

        integer :: ind
        character(len=:),allocatable :: trunc_header

        if (.not. this%with_headers) error stop "data frame has no headers to look up"

        allocate(character(len(this%headers(1))) :: trunc_header)
        trunc_header = trim(adjustl(header))
        ind = findloc(this%headers,trunc_header,dim=1)
        if (ind < 1) error stop 'header not present in data frame'

        col = this%data_cols(ind)%geti()

    end function df_get_col_header_integer

    pure function df_get_col_header_logical(this,header) result(col)
        class(data_frame),intent(in) :: this
        character(len=*),intent(in) :: header
        logical,dimension(this%col_size) :: col

        integer :: ind
        character(len=:),allocatable :: trunc_header

        if (.not. this%with_headers) error stop "data frame has no headers to look up"

        allocate(character(len(this%headers(1))) :: trunc_header)
        trunc_header = trim(adjustl(header))
        ind = findloc(this%headers,trunc_header,dim=1)
        if (ind < 1) error stop 'header not present in data frame'

        col = this%data_cols(ind)%getl()

    end function df_get_col_header_logical

    pure function df_get_col_header_character(this,header) result(col)
        class(data_frame),intent(in) :: this
        character(len=*),intent(in) :: header
        character(len=:),dimension(:),allocatable :: col

        integer :: ind
        character(len=:),allocatable :: trunc_header

        if (.not. this%with_headers) error stop "data frame has no headers to look up"

        allocate(character(len(this%headers(1))) :: trunc_header)
        trunc_header = trim(adjustl(header))
        ind = findloc(this%headers,trunc_header,dim=1)
        if (ind < 1) error stop 'header not present in data frame'

        col = this%data_cols(ind)%getch()

    end function df_get_col_header_character

    pure function df_get_col_header_complex(this,header) result(col)
        class(data_frame),intent(in) :: this
        character(len=*),intent(in) :: header
        complex(rk),dimension(this%col_size) :: col

        integer :: ind
        character(len=:),allocatable :: trunc_header

        if (.not. this%with_headers) error stop "data frame has no headers to look up"

        allocate(character(len(this%headers(1))) :: trunc_header)
        trunc_header = trim(adjustl(header))
        ind = findloc(this%headers,trunc_header,dim=1)
        if (ind < 1) error stop 'header not present in data frame'

        col = this%data_cols(ind)%getc()

    end function df_get_col_header_complex


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


! ~~~~ Get single value from header

    pure function df_get_val_header_real(this,header,j) result(val)
        class(data_frame),intent(in) :: this
        character(len=*),intent(in) :: header
        integer,intent(in) :: j
        real(rk) :: val

        integer :: ind
        character(len=:),allocatable :: trunc_header

        if (.not. this%with_headers) error stop "data frame has no headers to look up"

        allocate(character(len(this%headers(1))) :: trunc_header)
        trunc_header = trim(adjustl(header))
        ind = findloc(this%headers,trunc_header,dim=1)
        if (ind < 1) error stop 'header not present in data frame'

        val = this%data_cols(ind)%getr(j)

    end function df_get_val_header_real

    pure function df_get_val_header_integer(this,header,j) result(val)
        class(data_frame),intent(in) :: this
        character(len=*),intent(in) :: header
        integer,intent(in) :: j
        integer(ik) :: val

        integer :: ind
        character(len=:),allocatable :: trunc_header

        if (.not. this%with_headers) error stop "data frame has no headers to look up"

        allocate(character(len(this%headers(1))) :: trunc_header)
        trunc_header = trim(adjustl(header))
        ind = findloc(this%headers,trunc_header,dim=1)
        if (ind < 1) error stop 'header not present in data frame'

        val = this%data_cols(ind)%geti(j)

    end function df_get_val_header_integer

    pure function df_get_val_header_logical(this,header,j) result(val)
        class(data_frame),intent(in) :: this
        character(len=*),intent(in) :: header
        integer,intent(in) :: j
        logical :: val

        integer :: ind
        character(len=:),allocatable :: trunc_header

        if (.not. this%with_headers) error stop "data frame has no headers to look up"

        allocate(character(len(this%headers(1))) :: trunc_header)
        trunc_header = trim(adjustl(header))
        ind = findloc(this%headers,trunc_header,dim=1)
        if (ind < 1) error stop 'header not present in data frame'

        val = this%data_cols(ind)%getl(j)

    end function df_get_val_header_logical

    pure function df_get_val_header_character(this,header,j) result(val)
        class(data_frame),intent(in) :: this
        character(len=*),intent(in) :: header
        integer,intent(in) :: j
        character(len=:),allocatable :: val

        integer :: ind
        character(len=:),allocatable :: trunc_header

        if (.not. this%with_headers) error stop "data frame has no headers to look up"

        allocate(character(len(this%headers(1))) :: trunc_header)
        trunc_header = trim(adjustl(header))
        ind = findloc(this%headers,trunc_header,dim=1)
        if (ind < 1) error stop 'header not present in data frame'

        val = this%data_cols(ind)%getch(j)

    end function df_get_val_header_character

    pure function df_get_val_header_complex(this,header,j) result(val)
        class(data_frame),intent(in) :: this
        character(len=*),intent(in) :: header
        integer,intent(in) :: j
        complex(rk) :: val

        integer :: ind
        character(len=:),allocatable :: trunc_header

        if (.not. this%with_headers) error stop "data frame has no headers to look up"

        allocate(character(len(this%headers(1))) :: trunc_header)
        trunc_header = trim(adjustl(header))
        ind = findloc(this%headers,trunc_header,dim=1)
        if (ind < 1) error stop 'header not present in data frame'

        val = this%data_cols(ind)%getc(j)

    end function df_get_val_header_complex


! ~~~~ Change single value of data frame -> two indices

    subroutine df_change_single_indices_real(this,i,j,val)
        class(data_frame),intent(inout) :: this
        integer,intent(in) :: i,j
        real(rk),intent(in) :: val

        call this%data_cols(i)%changer(j,val)

    end subroutine df_change_single_indices_real

    subroutine df_change_single_indices_integer(this,i,j,val)
        class(data_frame),intent(inout) :: this
        integer,intent(in) :: i,j
        integer(ik),intent(in) :: val

        call this%data_cols(i)%changei(j,val)

    end subroutine df_change_single_indices_integer

    subroutine df_change_single_indices_logical(this,i,j,val)
        class(data_frame),intent(inout) :: this
        integer,intent(in) :: i,j
        logical,intent(in) :: val

        call this%data_cols(i)%changel(j,val)

    end subroutine df_change_single_indices_logical

    subroutine df_change_single_indices_character(this,i,j,val)
        class(data_frame),intent(inout) :: this
        integer,intent(in) :: i,j
        character(len=*),intent(in) :: val

        call this%data_cols(i)%changech(j,val)

    end subroutine df_change_single_indices_character

    subroutine df_change_single_indices_complex(this,i,j,val)
        class(data_frame),intent(inout) :: this
        integer,intent(in) :: i,j
        complex(rk),intent(in) :: val

        call this%data_cols(i)%changec(j,val)

    end subroutine df_change_single_indices_complex


! ~~~~ Change single value of data frame -> header 

    subroutine df_change_single_header_real(this,header,j,val)
        class(data_frame),intent(inout) :: this
        character(len=*),intent(in) :: header
        integer,intent(in) :: j
        real(rk),intent(in) :: val
        
        integer :: ind
        character(len=:),allocatable :: trunc_header

        if (.not. this%with_headers) error stop "data frame has no headers to look up"

        allocate(character(len(this%headers(1))) :: trunc_header)
        trunc_header = trim(adjustl(header))
        ind = findloc(this%headers,trunc_header,dim=1)
        if (ind < 1) error stop 'header not present in data frame'

        call this%data_cols(ind)%changer(j,val)

    end subroutine df_change_single_header_real

    subroutine df_change_single_header_integer(this,header,j,val)
        class(data_frame),intent(inout) :: this
        character(len=*),intent(in) :: header
        integer,intent(in) :: j
        integer(ik),intent(in) :: val
        
        integer :: ind
        character(len=:),allocatable :: trunc_header

        if (.not. this%with_headers) error stop "data frame has no headers to look up"

        allocate(character(len(this%headers(1))) :: trunc_header)
        trunc_header = trim(adjustl(header))
        ind = findloc(this%headers,trunc_header,dim=1)
        if (ind < 1) error stop 'header not present in data frame'

        call this%data_cols(ind)%changei(j,val)

    end subroutine df_change_single_header_integer

    subroutine df_change_single_header_logical(this,header,j,val)
        class(data_frame),intent(inout) :: this
        character(len=*),intent(in) :: header
        integer,intent(in) :: j
        logical,intent(in) :: val
        
        integer :: ind
        character(len=:),allocatable :: trunc_header

        if (.not. this%with_headers) error stop "data frame has no headers to look up"

        allocate(character(len(this%headers(1))) :: trunc_header)
        trunc_header = trim(adjustl(header))
        ind = findloc(this%headers,trunc_header,dim=1)
        if (ind < 1) error stop 'header not present in data frame'

        call this%data_cols(ind)%changel(j,val)

    end subroutine df_change_single_header_logical

    subroutine df_change_single_header_character(this,header,j,val)
        class(data_frame),intent(inout) :: this
        character(len=*),intent(in) :: header
        integer,intent(in) :: j
        character(len=*),intent(in) :: val
        
        integer :: ind
        character(len=:),allocatable :: trunc_header

        if (.not. this%with_headers) error stop "data frame has no headers to look up"

        allocate(character(len(this%headers(1))) :: trunc_header)
        trunc_header = trim(adjustl(header))
        ind = findloc(this%headers,trunc_header,dim=1)
        if (ind < 1) error stop 'header not present in data frame'

        call this%data_cols(ind)%changech(j,val)

    end subroutine df_change_single_header_character

    subroutine df_change_single_header_complex(this,header,j,val)
        class(data_frame),intent(inout) :: this
        character(len=*),intent(in) :: header
        integer,intent(in) :: j
        complex(rk),intent(in) :: val
        
        integer :: ind
        character(len=:),allocatable :: trunc_header

        if (.not. this%with_headers) error stop "data frame has no headers to look up"

        allocate(character(len(this%headers(1))) :: trunc_header)
        trunc_header = trim(adjustl(header))
        ind = findloc(this%headers,trunc_header,dim=1)
        if (ind < 1) error stop 'header not present in data frame'

        call this%data_cols(ind)%changec(j,val)

    end subroutine df_change_single_header_complex


! ~~~~ Change col of data frame with index

    subroutine df_change_col_index_real(this,i,col)
        class(data_frame),intent(inout) :: this
        integer,intent(in) :: i
        real(rk),dimension(this%col_size) :: col

        call this%data_cols(i)%new(col)

    end subroutine df_change_col_index_real

    subroutine df_change_col_index_integer(this,i,col)
        class(data_frame),intent(inout) :: this
        integer,intent(in) :: i
        integer(ik),dimension(this%col_size) :: col

        call this%data_cols(i)%new(col)

    end subroutine df_change_col_index_integer

    subroutine df_change_col_index_logical(this,i,col)
        class(data_frame),intent(inout) :: this
        integer,intent(in) :: i
        logical,dimension(this%col_size) :: col

        call this%data_cols(i)%new(col)

    end subroutine df_change_col_index_logical

    subroutine df_change_col_index_character(this,i,col)
        class(data_frame),intent(inout) :: this
        integer,intent(in) :: i
        character(len=*),dimension(:),allocatable :: col

        if (size(col,dim=1) > this%col_size) error stop 'Different size columns in add col to data_frame'

        call this%data_cols(i)%new(col)

    end subroutine df_change_col_index_character

    subroutine df_change_col_index_complex(this,i,col)
        class(data_frame),intent(inout) :: this
        integer,intent(in) :: i
        complex(rk),dimension(this%col_size) :: col

        call this%data_cols(i)%new(col)

    end subroutine df_change_col_index_complex


! ~~~~ Change col of Data Frame with header

    subroutine df_change_col_header_real(this,header,col)
        class(data_frame),intent(inout) :: this
        character(len=*),intent(in) :: header
        real(rk),dimension(this%n),intent(in) :: col
        
        integer :: ind
        character(len=:),allocatable :: trunc_header

        if (.not. this%with_headers) error stop "data frame has no headers to look up"

        allocate(character(len(this%headers(1))) :: trunc_header)
        trunc_header = trim(adjustl(header))
        ind = findloc(this%headers,trunc_header,dim=1)
        if (ind < 1) error stop 'header not present in data frame'

        call this%data_cols(ind)%new(col)

    end subroutine df_change_col_header_real

    subroutine df_change_col_header_integer(this,header,col)
        class(data_frame),intent(inout) :: this
        character(len=*),intent(in) :: header
        integer(ik),dimension(this%n),intent(in) :: col
        
        integer :: ind
        character(len=:),allocatable :: trunc_header

        if (.not. this%with_headers) error stop "data frame has no headers to look up"

        allocate(character(len(this%headers(1))) :: trunc_header)
        trunc_header = trim(adjustl(header))
        ind = findloc(this%headers,trunc_header,dim=1)
        if (ind < 1) error stop 'header not present in data frame'

        call this%data_cols(ind)%new(col)

    end subroutine df_change_col_header_integer

    subroutine df_change_col_header_logical(this,header,col)
        class(data_frame),intent(inout) :: this
        character(len=*),intent(in) :: header
        logical,dimension(this%n),intent(in) :: col
        
        integer :: ind
        character(len=:),allocatable :: trunc_header

        if (.not. this%with_headers) error stop "data frame has no headers to look up"

        allocate(character(len(this%headers(1))) :: trunc_header)
        trunc_header = trim(adjustl(header))
        ind = findloc(this%headers,trunc_header,dim=1)
        if (ind < 1) error stop 'header not present in data frame'

        call this%data_cols(ind)%new(col)

    end subroutine df_change_col_header_logical

    subroutine df_change_col_header_character(this,header,col)
        class(data_frame),intent(inout) :: this
        character(len=*),intent(in) :: header
        character(len=*),dimension(:),intent(in) :: col
        
        integer :: ind
        character(len=:),allocatable :: trunc_header

        if (.not. this%with_headers) error stop "data frame has no headers to look up"

        allocate(character(len(this%headers(1))) :: trunc_header)
        trunc_header = trim(adjustl(header))
        ind = findloc(this%headers,trunc_header,dim=1)
        if (ind < 1) error stop 'header not present in data frame'

        if (size(col,dim=1) > this%col_size) error stop 'Different size columns in add col to data_frame'

        call this%data_cols(ind)%new(col)

    end subroutine df_change_col_header_character

    subroutine df_change_col_header_complex(this,header,col)
        class(data_frame),intent(inout) :: this
        character(len=*),intent(in) :: header
        complex(rk),dimension(this%n),intent(in) :: col
        
        integer :: ind
        character(len=:),allocatable :: trunc_header

        if (.not. this%with_headers) error stop "data frame has no headers to look up"

        allocate(character(len(this%headers(1))) :: trunc_header)
        trunc_header = trim(adjustl(header))
        ind = findloc(this%headers,trunc_header,dim=1)
        if (ind < 1) error stop 'header not present in data frame'

        call this%data_cols(ind)%new(col)

    end subroutine df_change_col_header_complex


! ~~~~ Write Data Frame

    subroutine df_write_unformatted(this,unit,iostat)
        ! If format args are given, must be in form: "i20", "f10.5", "ES30.20", etc.
        ! -> Formats canot have parentheses 
        class(data_frame),intent(in) :: this
        integer,intent(in),optional :: unit
        integer,intent(out),optional :: iostat

        character(len=100) :: rfmt, ifmt, lfmt, chfmt, cfmt, fmt_widthch, pfmt
        character(len=:),allocatable :: output_char
        integer :: fmt_widthi
        integer :: io_unit, io_err

        integer :: i, j
        integer :: num_cols, len_cols

        if (present(unit)) then
            io_unit = unit
        else
            io_unit = STD_OUT
        end if

        fmt_widthi = 23
        allocate(character(fmt_widthi) :: output_char)
        write(fmt_widthch,"(i10)") fmt_widthi
        rfmt = "ES"//trim(adjustl(fmt_widthch))//".17"    
        ifmt = "i"//trim(adjustl(fmt_widthch))
        lfmt = "l"//trim(adjustl(fmt_widthch))
        chfmt = "a"//trim(adjustl(fmt_widthch))
        cfmt = "2"//trim(adjustl(rfmt))


        num_cols = this%n
        len_cols = this%col_size
        
        call write_horiz()
        !call write_blank()

        if (this%with_headers) then
            pfmt = "(a2,"//trim(adjustl(chfmt))//",a1)"
            do j=1,num_cols
                output_char = trim(adjustl(this%headers(j)))
                write(io_unit,pfmt,iostat=io_err,advance='no') "| ", adjustr(output_char), " "
            end do
            write(io_unit,"(a1)",iostat=io_err) "|"

            !call write_blank
            call write_horiz()
        end if
        do i=1,len_cols
            do j=1,num_cols
                select case (this%data_cols(j)%dtype)
                    case (REAL)
                        pfmt = "(a2,"//trim(adjustl(rfmt))//",a1)"
                        write(io_unit,pfmt,iostat=io_err,advance='no') "| ", this%getr(j,i), " "
                    case (INTEGER)
                        pfmt = "(a2,"//trim(adjustl(ifmt))//",a1)"
                        write(io_unit,pfmt,iostat=io_err,advance='no') "| ", this%geti(j,i), " "
                    case (LOGICAL)
                        pfmt = "(a2,"//trim(adjustl(lfmt))//",a1)"
                        write(io_unit,pfmt,iostat=io_err,advance='no') "| ", this%getl(j,i), " "
                    case (CHARACTER)
                        pfmt = "(a2,"//trim(adjustl(chfmt))//",a1)"
                        output_char = trim(adjustl(this%getch(j,i)))
                        write(io_unit,pfmt,iostat=io_err,advance='no') "| ", adjustr(output_char), " "
                    case (COMPLEX)
                        error stop 'cannot print complex data type yet'
                        pfmt = "(a2,"//trim(adjustl(cfmt))//",a1)"
                        write(io_unit,pfmt,iostat=io_err,advance='no') "| ", this%getc(j,i), " "
                end select
            end do
            write(io_unit,"(a1)",iostat=io_err) "|"
            !call write_horiz()
        end do
        call write_horiz()
        
    contains

        subroutine write_blank()
            character(len=100) :: pfmt
            integer :: i,k

            pfmt = "(a2,"//trim(adjustl(chfmt))//",a1)"
            write(io_unit,pfmt,iostat=io_err,advance='no') "| ", (" ", k=1,fmt_widthi), " "
            do i=2,num_cols
                write(io_unit,pfmt,iostat=io_err,advance='no') "  ", (" ", k=1,fmt_widthi), " " 
            end do
            write(io_unit,"(a1)",iostat=io_err) "|"

        end subroutine write_blank

        subroutine write_horiz()
            character(len=100) :: pfmt, line
            integer :: i

            line = "-----------------------"
            pfmt = "(a2,"//trim(adjustl(chfmt))//",a1)"
            do i=1,num_cols
                write(io_unit,pfmt,iostat=io_err,advance='no') "--", trim(adjustl(line)), "-" 
            end do
            write(io_unit,"(a1)",iostat=io_err) "-"

        end subroutine

    end subroutine df_write_unformatted


! ~~~~ Read Data Frame from File

    subroutine df_read_df_file(this,filename,has_headers)
        class(data_frame),intent(inout) :: this
        character(len=*),intent(in) :: filename
        logical,intent(in) :: has_headers

        integer :: io_err
        character(len=:),allocatable :: err_msg

        integer :: unit, num_lines, num_cols, line_ind, line_len, dtype
        character(len=:),allocatable :: line
        character(len=:),dimension(:),allocatable :: headers, split_line

        real(rk) :: rval
        integer(ik) :: ival
        logical :: lval
        complex(rk) :: cval

        integer :: i, offset

        open(newunit=unit,file=trim(adjustl(filename)),status="old",action="read",iostat=io_err)
        if (io_err /= 0) then
            err_msg = err_msg_io_open//" "//trim(adjustl(filename))
            error stop err_msg
        end if

        call get_num_lines(unit,num_lines)
        call get_num_cols(unit,num_cols,line_len)   !returns num columns and how many chars a line is


        allocate(character(len=line_len) :: line)
        read(unit=unit,fmt='(a)',iostat=io_err) line
        if (io_err /= 0) then
            err_msg = err_msg_io_read//" "//trim(adjustl(filename))
            error stop err_msg
        end if
        split_line = split(line)

        if (has_headers) then
            headers = split_line
            read(unit=unit,fmt='(a)',iostat=io_err) line
            if (io_err /= 0) then
                err_msg = err_msg_io_read//" "//trim(adjustl(filename))
                error stop err_msg
            end if
            split_line = line
            do i=1,num_cols
                dtype = what_type(split_line(i))
                select case (dtype)
                    case (REAL)
                        call this%append_emptyr(num_lines-1,trim(adjustl(headers(i))))
                        read(split_line(i),fmt=*) rval
                        call this%setr(i,1,rval)
                    case (INTEGER)
                        call this%append_emptyi(num_lines-1,trim(adjustl(headers(i))))
                        read(split_line(i),fmt=*) ival
                        call this%seti(i,1,ival)
                    case (LOGICAL)
                        call this%append_emptyl(num_lines-1,trim(adjustl(headers(i))))
                        read(split_line(i),fmt=*) lval
                        call this%setl(i,1,lval)
                    case (CHARACTER)
                        call this%append_emptych(num_lines-1,trim(adjustl(headers(i))))
                        call this%setch(i,1,split_line(i))
                    case (COMPLEX)
                        call this%append_emptyc(num_lines-1,trim(adjustl(headers(i))))
                        read(split_line(i),fmt=*) cval
                        call this%setc(i,1,cval)
                end select
            end do
            line_ind = 2 ! 2 lines have been read
            offset = -1 ! col index will lag behind line index
        else
            do i=1,num_cols
                dtype = what_type(split_line(i))
                select case (dtype)
                    case (REAL)
                        call this%append_emptyr(num_lines-1)
                        read(split_line(i),fmt=*) rval
                        call this%setr(i,1,rval)
                    case (INTEGER)
                        call this%append_emptyi(num_lines-1)
                        read(split_line(i),fmt=*) ival
                        call this%seti(i,1,ival)
                    case (LOGICAL)
                        call this%append_emptyl(num_lines-1)
                        read(split_line(i),fmt=*) lval
                        call this%setl(i,1,lval)
                    case (CHARACTER)
                        call this%append_emptych(num_lines-1)
                        call this%setch(i,1,split_line(i))
                    case (COMPLEX)
                        call this%append_emptyc(num_lines-1)
                        read(split_line(i),fmt=*) cval
                        call this%setc(i,1,cval)
                end select
            end do
            line_ind = 1 ! only one line has been read
            offset = 0 ! column and line index are the same
        end if

        line_ind = line_ind + 1
        do while(line_ind <= num_lines)
            read(unit=unit,fmt='(a)',iostat=io_err) line
            if (io_err /= 0) then
                err_msg = err_msg_io_read//" "//trim(adjustl(filename))
                error stop err_msg
            end if
            split_line = line
            do i=1,num_cols
                select case (this%data_cols(i)%dtype)
                case (REAL)
                    read(split_line(i),fmt=*) rval
                    call this%setr(i,line_ind+offset,rval)
                case (INTEGER)
                    read(split_line(i),fmt=*) ival
                    call this%seti(i,line_ind+offset,ival)
                case (LOGICAL)
                    read(split_line(i),fmt=*) lval
                    call this%setl(i,line_ind+offset,lval)
                case (CHARACTER)
                    call this%setch(i,line_ind+offset,split_line(i))
                case (COMPLEX)
                    read(split_line(i),fmt=*) cval
                    call this%setc(i,line_ind+offset,cval)
            end select
            end do
            line_ind = line_ind + 1
        end do

    end subroutine df_read_df_file


    subroutine get_num_cols(unit,num_cols,line_len)
        integer,intent(in) :: unit
        integer,intent(out) :: num_cols
        integer,intent(out) :: line_len

        character(len=:),allocatable :: line
        character(len=:),dimension(:),allocatable :: line_split

        call get_len_line(unit,line_len,line)
        line_split = split(line," ")
        num_cols = size(line_split,dim=1)

        rewind(unit)

    end subroutine get_num_cols


















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


end module df_data_frame
