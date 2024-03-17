module df_fortranDF
    use,intrinsic :: iso_fortran_env, only: STD_OUT => output_unit
    use df_precision
    use df_types
    !use df_column_class
    use df_utils
    use split_mod
    implicit none
    private

    public :: data_frame

    
    integer,parameter :: MAX_CHAR_LEN_DEFAULT = 100



! ~~~~~ DATA FRAME TYPE ~~~~~

    type :: data_frame
        private
        
        integer :: n, max_char_len
        logical :: with_headers, enforce_length
        character(len=:),dimension(:),allocatable :: headers

        integer,dimension(:,:),allocatable :: type_loc
        integer,dimension(:),allocatable :: col_lens

        integer :: rcols, icols, lcols, chcols, ccols
        integer :: rrows_max, irows_max, lrows_max, chrows_max, crows_max, nrows_max

        real(rk),dimension(:,:),allocatable :: rdata
        integer(ik),dimension(:,:),allocatable :: idata
        logical,dimension(:,:),allocatable :: ldata
        character(len=:),dimension(:,:),allocatable :: chdata
        complex(rk),dimension(:,:),allocatable :: cdata

        logical :: initialized = .false.

    contains
        private

        procedure,public :: new => df_constructor
        procedure,public :: destroy => df_destructor

        procedure,public :: ncols => df_get_num_cols
        procedure :: df_get_num_rows, df_get_nrows_max
        generic,public :: nrows => df_get_num_rows, df_get_nrows_max
        procedure,public :: nreal_cols => df_get_num_cols_real
        procedure,public :: ninteger_cols => df_get_num_cols_integer
        procedure,public :: nlogical_cols => df_get_num_cols_logical
        procedure,public :: ncharacter_cols => df_get_num_cols_character
        procedure,public :: ncomplex_cols => df_get_num_cols_complex

        procedure :: df_get_col_type_header, df_get_col_type_index
        generic,public :: dtype => df_get_col_type_header, df_get_col_type_index

        procedure :: add_type_loc
        procedure :: add_col_len

        procedure :: add_header
        procedure :: already_header
        procedure :: add_col_real,      &
                     add_col_integer,   &
                     add_col_logical,   &
                     add_col_character, &
                     add_col_complex

        procedure :: stretch_cols_real,         &
                     stretch_cols_integer,      &
                     stretch_cols_logical,      &
                     stretch_cols_character,    &
                     stretch_cols_complex
        procedure :: add_col_check_real,      &
                     add_col_check_integer,   &
                     add_col_check_logical,   &
                     add_col_check_character, &
                     add_col_check_complex

        generic,public :: append => add_col_check_real, add_col_check_integer, add_col_check_logical,    &
                                    add_col_check_character, add_col_check_complex
        ! Public?
        procedure :: add_empty_col_real,        &
                     add_empty_col_integer,     &
                     add_empty_col_logical,     &
                     add_empty_col_character,   &
                     add_empty_col_complex
        procedure :: add_empty_col_check_real,        &
                     add_empty_col_check_integer,     &
                     add_empty_col_check_logical,     &
                     add_empty_col_check_character,   &
                     add_empty_col_check_complex
                     
        procedure,public :: append_emptyr => add_empty_col_check_real
        procedure,public :: append_emptyi => add_empty_col_check_integer
        procedure,public :: append_emptyl => add_empty_col_check_logical
        procedure,public :: append_emptych => add_empty_col_check_character
        procedure,public :: append_emptyc => add_empty_col_check_complex

        
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

        procedure,public :: write => df_write_unformatted

        procedure,public :: read => df_read_df_file
    end type data_frame



contains


! ~~~~ DF Constructor

    subroutine df_constructor(this,enforce_length,char_len)
        class(data_frame),intent(inout) :: this
        integer,intent(in),optional :: char_len
        logical,intent(in),optional :: enforce_length

        if (this%initialized) call this%destroy()
        this%initialized = .true.

        if (present(char_len)) then
            this%max_char_len = char_len
        else
            this%max_char_len = MAX_CHAR_LEN_DEFAULT
        end if

        if (present(enforce_length)) then
            this%enforce_length = enforce_length
        else
            this%enforce_length = .true.
        end if

        this%n = 0
        this%rcols = -1
        this%icols = -1
        this%lcols = -1
        this%chcols = -1
        this%ccols = -1
        this%rrows_max = -1
        this%irows_max = -1
        this%lrows_max = -1
        this%chrows_max = -1
        this%crows_max = -1
        if (this%enforce_length) this%nrows_max = -1

    end subroutine


! ~~~~ DF Destructor

    subroutine df_destructor(this)
        class(data_frame),intent(inout) :: this

        if (allocated(this%rdata)) deallocate(this%rdata)
        if (allocated(this%idata)) deallocate(this%idata)
        if (allocated(this%ldata)) deallocate(this%ldata)
        if (allocated(this%chdata)) deallocate(this%chdata)
        if (allocated(this%cdata)) deallocate(this%cdata)

        if (allocated(this%type_loc)) deallocate(this%type_loc)

        if (allocated(this%headers)) deallocate(this%headers)

        this%n = 0
        this%rcols = -1
        this%icols = -1
        this%lcols = -1
        this%chcols = -1
        this%ccols = -1
        this%rrows_max = -1
        this%irows_max = -1
        this%lrows_max = -1
        this%chrows_max = -1
        this%crows_max = -1
        this%nrows_max = -1

        this%max_char_len = 0

        this%initialized = .false.

    end subroutine df_destructor


! ~~~~ DF Inquiry functions

    pure function df_get_num_cols(this) result(n)
        class(data_frame),intent(in) :: this
        integer :: n

        n = this%n

    end function df_get_num_cols


    pure function df_get_num_cols_real(this) result(ncols)
        class(data_frame),intent(in) :: this
        integer :: ncols

        ncols = this%rcols

    end function df_get_num_cols_real

    pure function df_get_num_cols_integer(this) result(ncols)
        class(data_frame),intent(in) :: this
        integer :: ncols

        ncols = this%icols

    end function df_get_num_cols_integer

    pure function df_get_num_cols_logical(this) result(ncols)
        class(data_frame),intent(in) :: this
        integer :: ncols

        ncols = this%lcols

    end function df_get_num_cols_logical

    pure function df_get_num_cols_character(this) result(ncols)
        class(data_frame),intent(in) :: this
        integer :: ncols

        ncols = this%chcols

    end function df_get_num_cols_character

    pure function df_get_num_cols_complex(this) result(ncols)
        class(data_frame),intent(in) :: this
        integer :: ncols

        ncols = this%ccols

    end function df_get_num_cols_complex

    pure function df_get_num_rows(this,ind) result(num_rows)
        class(data_frame),intent(in) :: this
        integer,intent(in) :: ind
        integer :: num_rows

        if (ind > this%ncols()) error stop 'ind out of range of cols'

        num_rows = this%col_lens(ind)

    end function df_get_num_rows

    pure function df_get_nrows_max(this) result(nrows_max)
        class(data_frame),intent(in) :: this
        integer :: nrows_max

        nrows_max = this%nrows_max

    end function df_get_nrows_max

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

        dtype = this%type_loc(ind,1)

    end function df_get_col_type_header

    pure function df_get_col_type_index(this,j) result(dtype)
        class(data_frame),intent(in) :: this
        integer,intent(in) :: j
        integer :: dtype

        dtype = this%type_loc(j,1)

    end function df_get_col_type_index


! ~~~~ add type_loc

    subroutine add_type_loc(this,dtype,loc)
        class(data_frame),intent(inout) :: this
        integer,intent(in) :: dtype, loc

        integer,dimension(:,:),allocatable :: type_loc_tmp

        if (this%n > 0) then
            allocate(type_loc_tmp(this%n+1,2))
            type_loc_tmp(1:this%n,:) = this%type_loc
            type_loc_tmp(this%n+1,:) = [dtype,loc]
            this%type_loc = type_loc_tmp
        else
            allocate(this%type_loc(1,2))
            this%type_loc(1,:) = [dtype,loc]
        end if


    end subroutine add_type_loc


! ~~~~ Add col_len

    subroutine add_col_len(this,col_len)
        class(data_frame),intent(inout) :: this
        integer,intent(in) :: col_len

        if (.not. allocated(this%col_lens)) then
            allocate(this%col_lens(1))
            this%col_lens(1) = col_len

            return
        end if

        block
            integer :: num_cols
            integer,dimension(size(this%col_lens,dim=1)+1) :: new_lens

            num_cols = size(this%col_lens)

            new_lens(:num_cols) = this%col_lens
            new_lens(num_cols+1) = col_len
            this%col_lens = new_lens
        end block

    end subroutine add_col_len


! ~~~~ Add Column to DF

    subroutine add_col_real(this,col)
        class(data_frame),intent(inout) :: this
        real(rk),dimension(:),intent(in) :: col

        real(rk),dimension(:,:),allocatable :: new_cols
        integer :: n, rcols, end

        if (.not. this%initialized) call this%new()

        if (this%rrows_max < 0) this%rrows_max = size(col,dim=1)

        end = size(col,dim=1)

        n = this%n
        rcols = this%rcols
        if (n > 0) then
            if (rcols > 0) then
                allocate(new_cols(this%rrows_max,rcols+1))
                new_cols(:,1:rcols) = this%rdata
                new_cols(:end,rcols+1) = col
                this%rdata = new_cols
                this%rcols = rcols + 1
                call this%add_type_loc(REAL_NUM,rcols+1)
            else
                allocate(this%rdata(this%rrows_max,1))
                this%rdata(:,1) = col
                this%rcols = 1
                call this%add_type_loc(REAL_NUM,1)
            end if
            this%n = n + 1
        else
            call this%add_type_loc(REAL_NUM,1)
            this%n = 1
            this%rcols = 1
            allocate(this%rdata(this%rrows_max,1))
            this%rdata(:end,1) = col
        end if

    end subroutine add_col_real

    subroutine add_col_integer(this,col)
        class(data_frame),intent(inout) :: this
        integer(ik),dimension(:),intent(in) :: col

        integer(ik),dimension(:,:),allocatable :: new_cols
        integer :: n, icols, end

        if (.not. this%initialized) call this%new()

        if (this%irows_max < 0) this%irows_max = size(col,dim=1)

        end = size(col,dim=1)

        n = this%n
        icols = this%icols
        if (n > 0) then
            if (icols > 0) then
                allocate(new_cols(this%irows_max,icols+1))
                new_cols(:,1:icols) = this%idata
                new_cols(:end,icols+1) = col
                this%idata = new_cols
                this%icols = icols + 1
                call this%add_type_loc(INTEGER_NUM,icols+1)
            else
                allocate(this%idata(this%irows_max,1))
                this%idata(:end,1) = col
                this%icols = 1
                call this%add_type_loc(INTEGER_NUM,1)
            end if
            this%n = n + 1
        else
            call this%add_type_loc(INTEGER_NUM,1)
            this%n = 1
            this%icols = 1
            allocate(this%idata(this%irows_max,1))
            this%idata(:,1) = col
        end if

    end subroutine add_col_integer

    subroutine add_col_logical(this,col)
        class(data_frame),intent(inout) :: this
        logical,dimension(:),intent(in) :: col

        logical,dimension(:,:),allocatable :: new_cols
        integer :: n, lcols, end

        if (.not. this%initialized) call this%new()

        if (this%lrows_max < 0) this%lrows_max = size(col,dim=1)

        end = size(col,dim=1)

        n = this%n
        lcols = this%lcols
        if (n > 0) then
            if (lcols > 0) then
                allocate(new_cols(this%lrows_max,lcols+1))
                new_cols(:,1:lcols) = this%ldata
                new_cols(:end,lcols+1) = col
                this%ldata = new_cols
                this%lcols = lcols + 1
                call this%add_type_loc(LOGICAL_NUM,lcols+1)
            else
                allocate(this%ldata(this%lrows_max,1))
                this%ldata(:end,1) = col
                this%lcols = 1
                call this%add_type_loc(LOGICAL_NUM,1)
            end if
            this%n = n + 1
        else
            call this%add_type_loc(LOGICAL_NUM,1)
            this%n = 1
            this%lcols = 1
            allocate(this%ldata(this%lrows_max,1))
            this%ldata(:,1) = col
        end if

    end subroutine add_col_logical

    subroutine add_col_character(this,col)
        class(data_frame),intent(inout) :: this
        character(len=*),dimension(:),intent(in) :: col

        character(len=:),dimension(:,:),allocatable :: new_cols
        integer :: n,chcols, end

        if (.not. this%initialized) call this%new()

        if (this%chrows_max < 0) this%chrows_max = size(col,dim=1)

        end = size(col,dim=1)

        n = this%n
        chcols = this%chcols
        if (n > 0) then
            if (chcols > 0) then
                allocate(character(this%max_char_len) :: new_cols(this%chrows_max,chcols+1))
                new_cols(:,1:chcols) = this%chdata
                new_cols(:end,chcols+1) = col
                this%chdata = new_cols
                this%chcols = chcols + 1
                call this%add_type_loc(CHARACTER_NUM,chcols+1)
            else
                allocate(character(this%max_char_len) :: this%chdata(this%chrows_max,1))
                this%chdata(:end,1) = col
                this%chcols = 1
                call this%add_type_loc(CHARACTER_NUM,1)
            end if
            this%n = n + 1
        else
            call this%add_type_loc(CHARACTER_NUM,1)
            this%n = 1
            this%chcols = 1
            allocate(character(this%max_char_len) :: this%chdata(this%chrows_max,1))
            this%chdata(:,1) = col
        end if

    end subroutine add_col_character

    subroutine add_col_complex(this,col)
        class(data_frame),intent(inout) :: this
        complex(rk),dimension(:),intent(in) :: col

        complex(rk),dimension(:,:),allocatable :: new_cols
        integer :: n, ccols, end

        if (.not. this%initialized) call this%new()

        if (this%crows_max < 0) this%crows_max = size(col,dim=1)

        end = size(col,dim=1)

        n = this%n
        ccols = this%ccols
        if (n > 0) then
            if (ccols > 0) then
                allocate(new_cols(this%crows_max,ccols+1))
                new_cols(:,1:ccols) = this%cdata
                new_cols(:end,ccols+1) = col
                this%cdata = new_cols
                this%ccols = ccols + 1
            else
                allocate(this%cdata(this%crows_max,1))
                this%cdata(:end,1) = col
                this%ccols = 1
                call this%add_type_loc(COMPLEX_NUM,1)
            end if
            this%n = n + 1
        else
            call this%add_type_loc(COMPLEX_NUM,1)
            this%n = 1
            this%ccols = 1
            allocate(this%cdata(this%crows_max,1))
            this%cdata(:,1) = col
        end if

    end subroutine add_col_complex

! ~~~~ Add header

    subroutine add_header(this,header)
        class(data_frame),intent(inout) :: this
        character(len=*),intent(in) :: header

        character(len=:),dimension(:),allocatable :: new_headers
        integer :: num_headers

        if (.not. this%with_headers) error stop 'cannot add header to data_frame not using headers'
        

        if (.not. allocated(this%headers)) then
            allocate(character(len=this%max_char_len) :: this%headers(1))
            this%headers(1) = trim(adjustl(header))
        else
            num_headers = size(this%headers,dim=1)
            if (this%already_header(header)) error stop 'all headers must be unique'
            allocate(character(len(this%headers)) :: new_headers(num_headers+1))
            new_headers(1:num_headers) = this%headers
            new_headers(num_headers+1) = trim(adjustl(header))
            this%headers = new_headers
        end if

    end subroutine add_header



! ~~~~ Add col to variable len DF

    subroutine add_col_check_real(this,col,header)
        class(data_frame),intent(inout) :: this
        real(rk),dimension(:),intent(in) :: col
        character(len=*),intent(in),optional :: header

        integer :: col_len


        col_len = size(col,dim=1)

        if (this%enforce_length .and. (this%nrows_max > 0 .and. this%nrows_max /= size(col,dim=1))) then
            error stop 'Different size columns in add col to data_frame'
        end if

        if (this%nrows_max < 0 .or.     &
            (.not. this%enforce_length .and. col_len > this%nrows_max)) then
                this%nrows_max = col_len
        end if

        if (this%n < 1) then
            this%with_headers = .false.
            if (present(header)) this%with_headers = .true.
        end if

        if (this%rrows_max < 0) then
            call this%add_col_real(col)
            if (.not. this%enforce_length) call this%add_col_len(col_len)
            if (present(header)) then
                call this%add_header(header)
            else
                if (this%with_headers) error stop 'if columns have headers, all columns must have headers'
            end if

            return
        end if

        if ((.not. this%enforce_length) .and. col_len > this%rrows_max) call this%stretch_cols_real(col_len)
        call this%add_col_real(col)
        if (.not. this%enforce_length) call this%add_col_len(col_len)

        if (present(header)) then
            call this%add_header(header)
        else
            if (this%with_headers) error stop 'if columns have headers, all columns must have headers'
        end if

    end subroutine add_col_check_real

    subroutine add_col_check_integer(this,col,header)
        class(data_frame),intent(inout) :: this
        integer(ik),dimension(:),intent(in) :: col
        character(len=*),intent(in),optional :: header

        integer :: col_len

        
        col_len = size(col,dim=1)

        if (this%enforce_length .and. (this%nrows_max > 0 .and. this%nrows_max /= size(col,dim=1))) then
            error stop 'Different size columns in add col to data_frame'
        end if

        if (this%nrows_max < 0 .or.      &
            (.not. this%enforce_length .and. col_len > this%nrows_max)) then
                this%nrows_max = col_len
        end if

        if (this%n < 1) then
            this%with_headers = .false.
            if (present(header)) this%with_headers = .true.
        end if

        if (this%irows_max < 0) then
            call this%add_col_integer(col)
            if (.not. this%enforce_length) call this%add_col_len(col_len)
            if (present(header)) then
                call this%add_header(header)
            else
                if (this%with_headers) error stop 'if columns have headers, all columns must have headers'
            end if

            return
        end if

        if ((.not. this%enforce_length) .and. col_len > this%irows_max) call this%stretch_cols_integer(col_len)
        call this%add_col_integer(col)
        if (.not. this%enforce_length) call this%add_col_len(col_len)

        if (present(header)) then
            call this%add_header(header)
        else
            if (this%with_headers) error stop 'if columns have headers, all columns must have headers'
        end if

    end subroutine add_col_check_integer

    subroutine add_col_check_logical(this,col,header)
        class(data_frame),intent(inout) :: this
        logical,dimension(:),intent(in) :: col
        character(len=*),intent(in),optional :: header

        integer :: col_len

        
        col_len = size(col,dim=1)

        if (this%enforce_length .and. (this%nrows_max > 0 .and. this%nrows_max /= size(col,dim=1))) then
            error stop 'Different size columns in add col to data_frame'
        end if

        if (this%nrows_max < 0 .or.      &
            (.not. this%enforce_length .and. col_len > this%nrows_max)) then
                this%nrows_max = col_len
        end if

        if (this%n < 1) then
            this%with_headers = .false.
            if (present(header)) this%with_headers = .true.
        end if

        if (this%lrows_max < 0) then
            call this%add_col_logical(col)
            if (.not. this%enforce_length) call this%add_col_len(col_len)
            if (present(header)) then
                call this%add_header(header)
            else
                if (this%with_headers) error stop 'if columns have headers, all columns must have headers'
            end if

            return
        end if

        if ((.not. this%enforce_length) .and. col_len > this%lrows_max) call this%stretch_cols_logical(col_len)
        call this%add_col_logical(col)
        if (.not. this%enforce_length) call this%add_col_len(col_len)

        if (present(header)) then
            call this%add_header(header)
        else
            if (this%with_headers) error stop 'if columns have headers, all columns must have headers'
        end if

    end subroutine add_col_check_logical

    subroutine add_col_check_character(this,col,header)
        class(data_frame),intent(inout) :: this
        character(len=*),dimension(:),intent(in) :: col
        character(len=*),intent(in),optional :: header

        integer :: col_len

        
        col_len = size(col,dim=1)

        if (this%enforce_length .and. (this%nrows_max > 0 .and. this%nrows_max /= size(col,dim=1))) then
            error stop 'Different size columns in add col to data_frame'
        end if

        if (this%nrows_max < 0 .or.      &
            (.not. this%enforce_length .and. col_len > this%nrows_max)) then
                this%nrows_max = col_len
        end if

        if (this%n < 1) then
            this%with_headers = .false.
            if (present(header)) this%with_headers = .true.
        end if

        if (this%chrows_max < 0) then
            call this%add_col_character(col)
            if (.not. this%enforce_length) call this%add_col_len(col_len)
            if (present(header)) then
                call this%add_header(header)
            else
                if (this%with_headers) error stop 'if columns have headers, all columns must have headers'
            end if

            return
        end if

        if ((.not. this%enforce_length) .and. col_len > this%chrows_max) call this%stretch_cols_character(col_len)
        call this%add_col_character(col)
        if (.not. this%enforce_length) call this%add_col_len(col_len)

        if (present(header)) then
            call this%add_header(header)
        else
            if (this%with_headers) error stop 'if columns have headers, all columns must have headers'
        end if

    end subroutine add_col_check_character

    subroutine add_col_check_complex(this,col,header)
        class(data_frame),intent(inout) :: this
        complex(rk),dimension(:),intent(in) :: col
        character(len=*),intent(in),optional :: header

        integer :: col_len

        
        col_len = size(col,dim=1)

        if (this%enforce_length .and. (this%nrows_max > 0 .and. this%nrows_max /= size(col,dim=1))) then
            error stop 'Different size columns in add col to data_frame'
        end if

        if (this%nrows_max < 0 .or.      &
            (.not. this%enforce_length .and. col_len > this%nrows_max)) then
                this%nrows_max = col_len
        end if

        if (this%n < 1) then
            this%with_headers = .false.
            if (present(header)) this%with_headers = .true.
        end if

        if (this%crows_max < 0) then
            call this%add_col_complex(col)
            if (.not. this%enforce_length) call this%add_col_len(col_len)
            if (present(header)) then
                call this%add_header(header)
            else
                if (this%with_headers) error stop 'if columns have headers, all columns must have headers'
            end if

            return
        end if

        if ((.not. this%enforce_length) .and. col_len > this%crows_max) call this%stretch_cols_complex(col_len)
        call this%add_col_complex(col)
        if (.not. this%enforce_length) call this%add_col_len(col_len)

        if (present(header)) then
            call this%add_header(header)
        else
            if (this%with_headers) error stop 'if columns have headers, all columns must have headers'
        end if

    end subroutine add_col_check_complex

! ~~~~ Check num_rows

    ! function nrows_max_all_same(this) result(all_same)
    !     class(data_frame),intent(in) :: this
    !     logical :: all_same

    !     all_same = .true.
    !     all_same = all_same .and. this%rrows_max == this%irows_max
    !     all_same = all_same .and. this%irows_max == this%lrows_max
    !     all_same = all_same .and. this%lrows_max == this%chrows_max
    !     all_same = all_same .and. this%chrows_max == this%crows_max
    !     all_same = sall_same .and. this%crows_max == this%nrows_max

    ! end function nrows_max_all_same



! ~~~~ Stretch array holding cols

    subroutine stretch_cols_real(this,new_size)
        class(data_frame),intent(inout) :: this
        integer,intent(in) :: new_size

        real(rk),dimension(new_size,this%rcols) :: new_rdata

        if (new_size < this%rrows_max) error stop 'attempt to allocate fewer rows than previous'

        new_rdata(:this%rrows_max,:) = this%rdata
        this%rdata = new_rdata
        this%rrows_max = new_size

    end subroutine stretch_cols_real

    subroutine stretch_cols_integer(this,new_size)
        class(data_frame),intent(inout) :: this
        integer,intent(in) :: new_size

        integer(ik),dimension(new_size,this%icols) :: new_idata

        if (new_size < this%irows_max) error stop 'attempt to allocate fewer rows than previous'

        new_idata(:this%irows_max,:) = this%idata
        this%idata = new_idata
        this%irows_max = new_size

    end subroutine stretch_cols_integer

    subroutine stretch_cols_logical(this,new_size)
        class(data_frame),intent(inout) :: this
        integer,intent(in) :: new_size

        logical,dimension(new_size,this%lcols) :: new_ldata

        if (new_size < this%lrows_max) error stop 'attempt to allocate fewer rows than previous'

        new_ldata(:this%lrows_max,:) = this%ldata
        this%ldata = new_ldata
        this%lrows_max = new_size

    end subroutine stretch_cols_logical

    subroutine stretch_cols_character(this,new_size)
        class(data_frame),intent(inout) :: this
        integer,intent(in) :: new_size

        character(this%max_char_len),dimension(new_size,this%chcols) :: new_chdata

        if (new_size < this%chrows_max) error stop 'attempt to allocate fewer rows than previous'

        new_chdata(:this%chrows_max,:) = this%chdata
        this%chdata = new_chdata
        this%chrows_max = new_size

    end subroutine stretch_cols_character

    subroutine stretch_cols_complex(this,new_size)
        class(data_frame),intent(inout) :: this
        integer,intent(in) :: new_size

        complex(rk),dimension(new_size,this%ccols) :: new_cdata

        if (new_size < this%crows_max) error stop 'attempt to allocate fewer rows than previous'

        new_cdata(:this%crows_max,:) = this%cdata
        this%cdata = new_cdata
        this%crows_max = new_size

    end subroutine stretch_cols_complex


! ~~~~ Add empty Col to DF

    subroutine add_empty_col_real(this,col_size)
        class(data_frame),intent(inout) :: this
        integer,intent(in) :: col_size

        real(rk),dimension(:,:),allocatable :: new_cols
        integer :: n,rcols


        if (.not. this%initialized) call this%new()
        
        if (this%rrows_max < 0) this%rrows_max = col_size

        n = this%n
        rcols = this%rcols
        if (n > 0) then
            if (rcols > 0) then
                allocate(new_cols(this%rrows_max,rcols+1))
                new_cols(:,1:rcols) = this%rdata
                this%rdata = new_cols
                this%rcols = rcols + 1
                call this%add_type_loc(REAL_NUM,rcols+1)
            else
                allocate(this%rdata(this%rrows_max,1))
                this%rcols = 1
                call this%add_type_loc(REAL_NUM,1)
            end if
            this%n = n + 1
        else
            call this%add_type_loc(REAL_NUM,1)
            this%n = 1
            this%rcols = 1
            allocate(this%rdata(this%rrows_max,1))
        end if

    end subroutine add_empty_col_real

    subroutine add_empty_col_integer(this,col_size)
        class(data_frame),intent(inout) :: this
        integer,intent(in) :: col_size

        integer(ik),dimension(:,:),allocatable :: new_cols
        integer :: n, icols


        if (.not. this%initialized) call this%new()
        
        if (this%irows_max < 0) this%irows_max = col_size

        n = this%n
        icols = this%icols
        if (n > 0) then
            if (icols > 0) then
                allocate(new_cols(this%irows_max,icols+1))
                new_cols(:,1:icols) = this%idata
                this%idata = new_cols
                this%icols = icols + 1
                call this%add_type_loc(INTEGER_NUM,icols+1)
            else
                allocate(this%idata(this%irows_max,1))
                this%icols = 1
                call this%add_type_loc(INTEGER_NUM,1)
            end if
            this%n = n + 1
        else
            call this%add_type_loc(INTEGER_NUM,1)
            this%n = 1
            this%icols = 1
            allocate(this%idata(this%irows_max,1))
        end if

    end subroutine add_empty_col_integer

    subroutine add_empty_col_logical(this,col_size)
        class(data_frame),intent(inout) :: this
        integer,intent(in) :: col_size

        logical,dimension(:,:),allocatable :: new_cols
        integer :: n, lcols


        if (.not. this%initialized) call this%new()
        
        if (this%lrows_max < 0) this%lrows_max = col_size

        n = this%n
        lcols = this%lcols
        if (n > 0) then
            if (lcols > 0) then
                allocate(new_cols(this%lrows_max,lcols+1))
                new_cols(:,1:lcols) = this%ldata
                this%ldata = new_cols
                this%lcols = lcols + 1
                call this%add_type_loc(LOGICAL_NUM,lcols+1)
            else
                allocate(this%ldata(this%lrows_max,1))
                this%lcols = 1
                call this%add_type_loc(LOGICAL_NUM,1)
            end if
            this%n = n + 1
        else
            call this%add_type_loc(LOGICAL_NUM,1)
            this%n = 1
            this%lcols = 1
            allocate(this%ldata(this%lrows_max,1))
        end if

    end subroutine add_empty_col_logical

    subroutine add_empty_col_character(this,col_size)
        class(data_frame),intent(inout) :: this
        integer,intent(in) :: col_size

        character(len=:),dimension(:,:),allocatable :: new_cols
        integer :: n, chcols


        if (.not. this%initialized) call this%new()
        
        if (this%chrows_max < 0) this%chrows_max = col_size

        n = this%n
        chcols = this%chcols
        if (n > 0) then
            if (chcols > 0) then
                allocate(character(this%max_char_len) :: new_cols(this%chrows_max,chcols+1))
                new_cols(:,1:chcols) = this%chdata
                this%chdata = new_cols
                this%chcols = chcols + 1
                call this%add_type_loc(CHARACTER_NUM,chcols+1)
            else
                allocate(character(this%max_char_len) :: this%chdata(this%chrows_max,1))
                this%chcols = 1
                call this%add_type_loc(CHARACTER_NUM,1)
            end if
            this%n = n + 1
        else
            call this%add_type_loc(CHARACTER_NUM,1)
            this%n = 1
            this%chcols = 1
            allocate(character(this%max_char_len) :: this%chdata(this%chrows_max,1))
        end if

    end subroutine add_empty_col_character

    subroutine add_empty_col_complex(this,col_size)
        class(data_frame),intent(inout) :: this
        integer,intent(in) :: col_size

        complex(rk),dimension(:,:),allocatable :: new_cols
        integer :: n, ccols


        if (.not. this%initialized) call this%new()
        
        if (this%crows_max < 0) this%crows_max = col_size

        n = this%n
        ccols = this%ccols
        if (n > 0) then
            if (ccols > 0) then
                allocate(new_cols(this%crows_max,ccols+1))
                new_cols(:,1:ccols) = this%cdata
                this%cdata = new_cols
                this%ccols = ccols + 1
                call this%add_type_loc(COMPLEX_NUM,ccols+1)
            else
                allocate(this%cdata(this%crows_max,1))
                this%ccols = 1
                call this%add_type_loc(COMPLEX_NUM,1)
            end if
            this%n = n + 1
        else
            call this%add_type_loc(COMPLEX_NUM,1)
            this%n = 1
            this%ccols = 1
            allocate(this%cdata(this%crows_max,1))
        end if

    end subroutine add_empty_col_complex

! ~~~~ Add empty call logic

    subroutine add_empty_col_check_real(this,col_size,header)
        class(data_frame),intent(inout) :: this
        integer,intent(in) :: col_size
        character(len=*),intent(in),optional :: header

        if (this%enforce_length .and. (this%nrows_max > 0 .and. this%nrows_max /= col_size)) then
            error stop 'Different size columns in add col to data_frame'
        end if

        if (this%nrows_max < 0 .or.      &
            (.not. this%enforce_length .and. col_size > this%nrows_max)) then
                this%nrows_max = col_size
        end if

        if (this%n < 1) then
            this%with_headers = .false.
            if (present(header)) this%with_headers = .true.
        end if

        if (this%rrows_max < 0) then
            call this%add_empty_col_real(col_size)
            if (.not. this%enforce_length) call this%add_col_len(col_size)
            if (present(header)) then
                call this%add_header(header)
            else
                if (this%with_headers) error stop 'if columns have headers, all columns must have headers'
            end if

            return
        end if

        if ((.not. this%enforce_length) .and. col_size > this%rrows_max) then
            call this%stretch_cols_real(col_size)
        end if
        call this%add_empty_col_real(col_size)
        if (.not. this%enforce_length) call this%add_col_len(col_size)

        if (present(header)) then
            call this%add_header(header)
        else
            if (this%with_headers) error stop 'if columns have headers, all columns must have headers'
        end if

    end subroutine add_empty_col_check_real

    subroutine add_empty_col_check_integer(this,col_size,header)
        class(data_frame),intent(inout) :: this
        integer,intent(in) :: col_size
        character(len=*),intent(in),optional :: header

        if (this%enforce_length .and. (this%nrows_max > 0 .and. this%nrows_max /= col_size)) then
            error stop 'Different size columns in add col to data_frame'
        end if

        if (this%nrows_max < 0 .or.      &
            (.not. this%enforce_length .and. col_size > this%nrows_max)) then
                this%nrows_max = col_size
        end if

        if (this%n < 1) then
            this%with_headers = .false.
            if (present(header)) this%with_headers = .true.
        end if

        if (this%irows_max < 0) then
            call this%add_empty_col_integer(col_size)
            if (.not. this%enforce_length) call this%add_col_len(col_size)
            if (present(header)) then
                call this%add_header(header)
            else
                if (this%with_headers) error stop 'if columns have headers, all columns must have headers'
            end if

            return
        end if

        if ((.not. this%enforce_length) .and. col_size > this%irows_max) then
            call this%stretch_cols_integer(col_size)
        end if
        call this%add_empty_col_integer(col_size)
        if (.not. this%enforce_length) call this%add_col_len(col_size)

        if (present(header)) then
            call this%add_header(header)
        else
            if (this%with_headers) error stop 'if columns have headers, all columns must have headers'
        end if

    end subroutine add_empty_col_check_integer

    subroutine add_empty_col_check_logical(this,col_size,header)
        class(data_frame),intent(inout) :: this
        integer,intent(in) :: col_size
        character(len=*),intent(in),optional :: header

        if (this%enforce_length .and. (this%nrows_max > 0 .and. this%nrows_max /= col_size)) then
            error stop 'Different size columns in add col to data_frame'
        end if

        if (this%nrows_max < 0 .or.      &
            (.not. this%enforce_length .and. col_size > this%nrows_max)) then
                this%nrows_max = col_size
        end if

        if (this%n < 1) then
            this%with_headers = .false.
            if (present(header)) this%with_headers = .true.
        end if

        if (this%lrows_max < 0) then
            call this%add_empty_col_logical(col_size)
            if (.not. this%enforce_length) call this%add_col_len(col_size)
            if (present(header)) then
                call this%add_header(header)
            else
                if (this%with_headers) error stop 'if columns have headers, all columns must have headers'
            end if

            return
        end if

        if ((.not. this%enforce_length) .and. col_size > this%lrows_max) then
            call this%stretch_cols_logical(col_size)
        end if
        call this%add_empty_col_logical(col_size)
        if (.not. this%enforce_length) call this%add_col_len(col_size)

        if (present(header)) then
            call this%add_header(header)
        else
            if (this%with_headers) error stop 'if columns have headers, all columns must have headers'
        end if

    end subroutine add_empty_col_check_logical

    subroutine add_empty_col_check_character(this,col_size,header)
        class(data_frame),intent(inout) :: this
        integer,intent(in) :: col_size
        character(len=*),intent(in),optional :: header

        if (this%enforce_length .and. (this%nrows_max > 0 .and. this%nrows_max /= col_size)) then
            error stop 'Different size columns in add col to data_frame'
        end if

        if (this%nrows_max < 0 .or.      &
            (.not. this%enforce_length .and. col_size > this%nrows_max)) then
                this%nrows_max = col_size
        end if

        if (this%n < 1) then
            this%with_headers = .false.
            if (present(header)) this%with_headers = .true.
        end if

        if (this%chrows_max < 0) then
            call this%add_empty_col_character(col_size)
            if (.not. this%enforce_length) call this%add_col_len(col_size)
            if (present(header)) then
                call this%add_header(header)
            else
                if (this%with_headers) error stop 'if columns have headers, all columns must have headers'
            end if

            return
        end if

        if ((.not. this%enforce_length) .and. col_size > this%chrows_max) then
            call this%stretch_cols_character(col_size)
        end if
        call this%add_empty_col_character(col_size)
        if (.not. this%enforce_length) call this%add_col_len(col_size)

        if (present(header)) then
            call this%add_header(header)
        else
            if (this%with_headers) error stop 'if columns have headers, all columns must have headers'
        end if

    end subroutine add_empty_col_check_character

    subroutine add_empty_col_check_complex(this,col_size,header)
        class(data_frame),intent(inout) :: this
        integer,intent(in) :: col_size
        character(len=*),intent(in),optional :: header

        if (this%enforce_length .and. (this%nrows_max > 0 .and. this%nrows_max /= col_size)) then
            error stop 'Different size columns in add col to data_frame'
        end if

        if (this%nrows_max < 0 .or.      &
            (.not. this%enforce_length .and. col_size > this%nrows_max)) then
                this%nrows_max = col_size
        end if

        if (this%n < 1) then
            this%with_headers = .false.
            if (present(header)) this%with_headers = .true.
        end if

        if (this%crows_max < 0) then
            call this%add_empty_col_complex(col_size)
            if (.not. this%enforce_length) call this%add_col_len(col_size)
            if (present(header)) then
                call this%add_header(header)
            else
                if (this%with_headers) error stop 'if columns have headers, all columns must have headers'
            end if

            return
        end if

        if ((.not. this%enforce_length) .and. col_size > this%crows_max) then
            call this%stretch_cols_complex(col_size)
        end if
        call this%add_empty_col_complex(col_size)
        if (.not. this%enforce_length) call this%add_col_len(col_size)

        if (present(header)) then
            call this%add_header(header)
        else
            if (this%with_headers) error stop 'if columns have headers, all columns must have headers'
        end if

    end subroutine add_empty_col_check_complex


! ~~~~ Check if header is not unique

    !pure function already_header(this, header)
    function already_header(this, header)
        class(data_frame),intent(in) :: this
        character(len=*),intent(in) :: header
        logical :: already_header

        character(len=this%max_char_len) :: trunc_header

        if (.not. allocated(this%headers)) then
            already_header = .false.
            return
        end if

        trunc_header = trim(adjustl(header))
        if (findloc(this%headers,trunc_header,dim=1) > 0) then
            already_header = .true.
        else
            already_header = .false.
        end if

    end function already_header


! ~~~~ Get Column DF with index

    pure function df_get_col_ind_real(this,i,full) result(col)
        class(data_frame),intent(in) :: this
        integer,intent(in) :: i
        logical,intent(in),optional :: full ! return full column
        real(rk),dimension(:),allocatable :: col

        integer :: ind, end

        if (this%type_loc(i,1) /= REAL_NUM) error stop 'column is not of real type'
        ind = this%type_loc(i,2)

        if (.not. this%enforce_length) then
            end = this%col_lens(i)
        else
            end = this%nrows_max
        end if
        
        if (present(full)) then
            if (full) end = this%rrows_max
        end if

        col = this%rdata(:end,ind)

    end function df_get_col_ind_real

    pure function df_get_col_ind_integer(this,i,full) result(col)
        class(data_frame),intent(in) :: this
        integer,intent(in) :: i
        logical,intent(in),optional :: full ! return full column
        integer(ik),dimension(:),allocatable :: col

        integer :: ind, end

        if (this%type_loc(i,1) /= INTEGER_NUM) error stop 'column is not of integer type'
        ind = this%type_loc(i,2)

        if (.not. this%enforce_length) then
            end = this%col_lens(i)
        else
            end = this%nrows_max
        end if

        if (present(full)) then
            if (full) end = this%irows_max
        end if

        col = this%idata(:end,ind)

    end function df_get_col_ind_integer

    pure function df_get_col_ind_logical(this,i,full) result(col)
        class(data_frame),intent(in) :: this
        integer,intent(in) :: i
        logical,intent(in),optional :: full ! return full column
        logical,dimension(:),allocatable :: col

        integer :: ind, end

        if (this%type_loc(i,1) /= LOGICAL_NUM) error stop 'column is not of logical type'
        ind = this%type_loc(i,2)

        if (.not. this%enforce_length) then
            end = this%col_lens(i)
        else
            end = this%nrows_max
        end if

        if (present(full)) then
            if (full) end = this%lrows_max
        end if

        col = this%ldata(:end,ind)

    end function df_get_col_ind_logical

    pure function df_get_col_ind_character(this,i,full) result(col)
        class(data_frame),intent(in) :: this
        integer,intent(in) :: i
        logical,intent(in),optional :: full ! return full column
        character(len=:),dimension(:),allocatable :: col

        integer :: ind, end

        if (this%type_loc(i,1) /= CHARACTER_NUM) error stop 'column is not of character type'
        ind = this%type_loc(i,2)

        if (.not. this%enforce_length) then
            end = this%col_lens(i)
        else
            end = this%nrows_max
        end if

        if (present(full)) then
            if (full) end = this%chrows_max
        end if

        col = this%chdata(:end,ind)

    end function df_get_col_ind_character

    pure function df_get_col_ind_complex(this,i,full) result(col)
        class(data_frame),intent(in) :: this
        integer,intent(in) :: i
        logical,intent(in),optional :: full ! return full column
        complex(rk),dimension(:),allocatable :: col

        integer :: ind, end

        if (this%type_loc(i,1) /= COMPLEX_NUM) error stop 'column is not of complex type'
        ind = this%type_loc(i,2)

        if (.not. this%enforce_length) then
            end = this%col_lens(i)
        else
            end = this%nrows_max
        end if

        if (present(full)) then
            if (full) end = this%crows_max
        end if

        col = this%cdata(:end,ind)

    end function df_get_col_ind_complex


! ~~~~ Get Column DF from header

    pure function df_get_col_header_real(this,header,full) result(col)
        class(data_frame),intent(in) :: this
        character(len=*),intent(in) :: header
        logical,intent(in),optional :: full ! return full column
        real(rk),dimension(:),allocatable :: col

        ! should be refactored so that `data_index` is `ind`, and `ind` is `i` to match index version
        ! of function
        integer :: ind, data_index, end
        character(len=:),allocatable :: trunc_header

        if (.not. this%with_headers) error stop "data frame has no headers to look up"

        allocate(character(len(this%headers(1))) :: trunc_header)
        trunc_header = trim(adjustl(header))
        ind = findloc(this%headers,trunc_header,dim=1)
        if (ind < 1) error stop 'header not present in data frame'

        if (this%type_loc(ind,1) /= REAL_NUM) error stop 'column is not of real type'
        data_index = this%type_loc(ind,2)

        if (.not. this%enforce_length) then
            end = this%col_lens(ind)
        else
            end = this%nrows_max
        end if

        if (present(full)) then
            if (full) end = this%rrows_max
        end if

        col = this%rdata(:end,data_index)

    end function df_get_col_header_real

    pure function df_get_col_header_integer(this,header,full) result(col)
        class(data_frame),intent(in) :: this
        character(len=*),intent(in) :: header
        logical,intent(in),optional :: full ! return full column
        integer(ik),dimension(:),allocatable :: col

        integer :: ind, data_index, end
        character(len=:),allocatable :: trunc_header

        if (.not. this%with_headers) error stop "data frame has no headers to look up"

        allocate(character(len(this%headers(1))) :: trunc_header)
        trunc_header = trim(adjustl(header))
        ind = findloc(this%headers,trunc_header,dim=1)
        if (ind < 1) error stop 'header not present in data frame'

        if (this%type_loc(ind,1) /= INTEGER_NUM) error stop 'column is not of integer type'
        data_index = this%type_loc(ind,2)

        if (.not. this%enforce_length) then
            end = this%col_lens(ind)
        else
            end = this%nrows_max
        end if
        
        if (present(full)) then
            if (full) end = this%irows_max
        end if

        col = this%idata(:end,data_index)

    end function df_get_col_header_integer

    pure function df_get_col_header_logical(this,header,full) result(col)
        class(data_frame),intent(in) :: this
        character(len=*),intent(in) :: header
        logical,intent(in),optional :: full ! return full column
        logical,dimension(:),allocatable :: col

        integer :: ind, data_index, end
        character(len=:),allocatable :: trunc_header

        if (.not. this%with_headers) error stop "data frame has no headers to look up"

        allocate(character(len(this%headers(1))) :: trunc_header)
        trunc_header = trim(adjustl(header))
        ind = findloc(this%headers,trunc_header,dim=1)
        if (ind < 1) error stop 'header not present in data frame'

        if (this%type_loc(ind,1) /= LOGICAL_NUM) error stop 'column is not of logical type'
        data_index = this%type_loc(ind,2)

        if (.not. this%enforce_length) then
            end = this%col_lens(ind)
        else
            end = this%nrows_max
        end if
        
        if (present(full)) then
            if (full) end = this%lrows_max
        end if

        col = this%ldata(:end,data_index)

    end function df_get_col_header_logical

    pure function df_get_col_header_character(this,header,full) result(col)
        class(data_frame),intent(in) :: this
        character(len=*),intent(in) :: header
        logical,intent(in),optional :: full ! return full column
        character(len=:),dimension(:),allocatable :: col

        integer :: ind, data_index, end
        character(len=:),allocatable :: trunc_header

        if (.not. this%with_headers) error stop "data frame has no headers to look up"

        allocate(character(len(this%headers(1))) :: trunc_header)
        trunc_header = trim(adjustl(header))
        ind = findloc(this%headers,trunc_header,dim=1)
        if (ind < 1) error stop 'header not present in data frame'

        if (this%type_loc(ind,1) /= CHARACTER_NUM) error stop 'column is not of character type'
        data_index = this%type_loc(ind,2)

        if (.not. this%enforce_length) then
            end = this%col_lens(ind)
        else
            end = this%nrows_max
        end if
        
        if (present(full)) then
            if (full) end = this%chrows_max
        end if

        col = this%chdata(:end,data_index)

    end function df_get_col_header_character

    pure function df_get_col_header_complex(this,header,full) result(col)
        class(data_frame),intent(in) :: this
        character(len=*),intent(in) :: header
        logical,intent(in),optional :: full ! return full column
        complex(rk),dimension(:),allocatable :: col

        integer :: ind, data_index, end
        character(len=:),allocatable :: trunc_header

        if (.not. this%with_headers) error stop "data frame has no headers to look up"

        allocate(character(len(this%headers(1))) :: trunc_header)
        trunc_header = trim(adjustl(header))
        ind = findloc(this%headers,trunc_header,dim=1)
        if (ind < 1) error stop 'header not present in data frame'

        if (this%type_loc(ind,1) /= COMPLEX_NUM) error stop 'column is not of complex type'
        data_index = this%type_loc(ind,2)

        if (.not. this%enforce_length) then
            end = this%col_lens(ind)
        else
            end = this%nrows_max
        end if
        
        if (present(full)) then
            if (full) end = this%crows_max
        end if

        col = this%cdata(:end,data_index)

    end function df_get_col_header_complex


! ~~~~ Get Single Val DF

    pure function df_get_val_real(this,i,j) result(val)
        class(data_frame),intent(in) :: this
        integer,intent(in) :: i,j
        real(rk) :: val

        integer :: ind

        if (this%type_loc(i,1) /= REAL_NUM) error stop 'column is not of real type'
        ind = this%type_loc(i,2)

        val = this%rdata(j,ind)

    end function df_get_val_real

    pure function df_get_val_integer(this,i,j) result(val)
        class(data_frame),intent(in) :: this
        integer,intent(in) :: i,j
        integer(ik) :: val

        integer :: ind

        if (this%type_loc(i,1) /= INTEGER_NUM) error stop 'column is not of integer type'
        ind = this%type_loc(i,2)

        val = this%idata(j,ind)

    end function df_get_val_integer
    
    pure function df_get_val_logical(this,i,j) result(val)
        class(data_frame),intent(in) :: this
        integer,intent(in) :: i,j
        logical :: val

        integer :: ind

        if (this%type_loc(i,1) /= LOGICAL_NUM) error stop 'column is not of logical type'
        ind = this%type_loc(i,2)

        val = this%ldata(j,ind)

    end function df_get_val_logical

    pure function df_get_val_character(this,i,j) result(val)
        class(data_frame),intent(in) :: this
        integer,intent(in) :: i,j
        character(len=:),allocatable :: val

        integer :: ind

        if (this%type_loc(i,1) /= CHARACTER_NUM) error stop 'column is not of character type'
        ind = this%type_loc(i,2)

        val = this%chdata(j,ind)

    end function df_get_val_character

    pure function df_get_val_complex(this,i,j) result(val)
        class(data_frame),intent(in) :: this
        integer,intent(in) :: i,j
        complex(rk) :: val

        integer :: ind

        if (this%type_loc(i,1) /= COMPLEX_NUM) error stop 'column is not of complex type'
        ind = this%type_loc(i,2)

        val = this%cdata(j,ind)

    end function df_get_val_complex


! ~~~~ Get single value from header

    pure function df_get_val_header_real(this,header,j) result(val)
        class(data_frame),intent(in) :: this
        character(len=*),intent(in) :: header
        integer,intent(in) :: j
        real(rk) :: val

        integer :: ind, data_index
        character(len=:),allocatable :: trunc_header

        if (.not. this%with_headers) error stop "data frame has no headers to look up"

        allocate(character(len(this%headers(1))) :: trunc_header)
        trunc_header = trim(adjustl(header))
        ind = findloc(this%headers,trunc_header,dim=1)
        if (ind < 1) error stop 'header not present in data frame'

        if (this%type_loc(ind,1) /= REAL_NUM) error stop 'column is not of real type'
        data_index = this%type_loc(ind,2)
        val = this%rdata(j,data_index)


    end function df_get_val_header_real

    pure function df_get_val_header_integer(this,header,j) result(val)
        class(data_frame),intent(in) :: this
        character(len=*),intent(in) :: header
        integer,intent(in) :: j
        integer(ik) :: val

        integer :: ind, data_index
        character(len=:),allocatable :: trunc_header

        if (.not. this%with_headers) error stop "data frame has no headers to look up"

        allocate(character(len(this%headers(1))) :: trunc_header)
        trunc_header = trim(adjustl(header))
        ind = findloc(this%headers,trunc_header,dim=1)
        if (ind < 1) error stop 'header not present in data frame'

        if (this%type_loc(ind,1) /= INTEGER_NUM) error stop 'column is not of integer type'
        data_index = this%type_loc(ind,2)
        val = this%idata(j,data_index)

    end function df_get_val_header_integer

    pure function df_get_val_header_logical(this,header,j) result(val)
        class(data_frame),intent(in) :: this
        character(len=*),intent(in) :: header
        integer,intent(in) :: j
        logical :: val

        integer :: ind, data_index
        character(len=:),allocatable :: trunc_header

        if (.not. this%with_headers) error stop "data frame has no headers to look up"

        allocate(character(len(this%headers(1))) :: trunc_header)
        trunc_header = trim(adjustl(header))
        ind = findloc(this%headers,trunc_header,dim=1)
        if (ind < 1) error stop 'header not present in data frame'

        if (this%type_loc(ind,1) /= LOGICAL_NUM) error stop 'column is not of logical type'
        data_index = this%type_loc(ind,2)
        val = this%ldata(j,data_index)

    end function df_get_val_header_logical

    pure function df_get_val_header_character(this,header,j) result(val)
        class(data_frame),intent(in) :: this
        character(len=*),intent(in) :: header
        integer,intent(in) :: j
        character(len=:),allocatable :: val

        integer :: ind, data_index
        character(len=:),allocatable :: trunc_header

        if (.not. this%with_headers) error stop "data frame has no headers to look up"

        allocate(character(len(this%headers(1))) :: trunc_header)
        trunc_header = trim(adjustl(header))
        ind = findloc(this%headers,trunc_header,dim=1)
        if (ind < 1) error stop 'header not present in data frame'

        if (this%type_loc(ind,1) /= CHARACTER_NUM) error stop 'column is not of character type'
        data_index = this%type_loc(ind,2)
        val = this%chdata(j,data_index)

    end function df_get_val_header_character

    pure function df_get_val_header_complex(this,header,j) result(val)
        class(data_frame),intent(in) :: this
        character(len=*),intent(in) :: header
        integer,intent(in) :: j
        complex(rk) :: val

        integer :: ind, data_index
        character(len=:),allocatable :: trunc_header

        if (.not. this%with_headers) error stop "data frame has no headers to look up"

        allocate(character(len(this%headers(1))) :: trunc_header)
        trunc_header = trim(adjustl(header))
        ind = findloc(this%headers,trunc_header,dim=1)
        if (ind < 1) error stop 'header not present in data frame'

        if (this%type_loc(ind,1) /= COMPLEX_NUM) error stop 'column is not of complex type'
        data_index = this%type_loc(ind,2)
        val = this%cdata(j,data_index)

    end function df_get_val_header_complex


! ~~~~ Change single value of data frame -> two indices

    subroutine df_change_single_indices_real(this,i,j,val)
        class(data_frame),intent(inout) :: this
        integer,intent(in) :: i,j
        real(rk),intent(in) :: val

        integer :: ind

        if (this%type_loc(i,1) /= REAL_NUM) error stop 'column is not of real type'
        ind = this%type_loc(i,2)

        this%rdata(j,ind) = val

    end subroutine df_change_single_indices_real

    subroutine df_change_single_indices_integer(this,i,j,val)
        class(data_frame),intent(inout) :: this
        integer,intent(in) :: i,j
        integer(ik),intent(in) :: val

        integer :: ind

        if (this%type_loc(i,1) /= INTEGER_NUM) error stop 'column is not of integer type'
        ind = this%type_loc(i,2)

        this%idata(j,ind) = val

    end subroutine df_change_single_indices_integer

    subroutine df_change_single_indices_logical(this,i,j,val)
        class(data_frame),intent(inout) :: this
        integer,intent(in) :: i,j
        logical,intent(in) :: val

        integer :: ind

        if (this%type_loc(i,1) /= LOGICAL_NUM) error stop 'column is not of logical type'
        ind = this%type_loc(i,2)

        this%ldata(j,ind) = val

    end subroutine df_change_single_indices_logical

    subroutine df_change_single_indices_character(this,i,j,val)
        class(data_frame),intent(inout) :: this
        integer,intent(in) :: i,j
        character(len=*),intent(in) :: val

        integer :: ind

        if (this%type_loc(i,1) /= CHARACTER_NUM) error stop 'column is not of character type'
        ind = this%type_loc(i,2)

        this%chdata(j,ind) = val

    end subroutine df_change_single_indices_character

    subroutine df_change_single_indices_complex(this,i,j,val)
        class(data_frame),intent(inout) :: this
        integer,intent(in) :: i,j
        complex(rk),intent(in) :: val

        integer :: ind

        if (this%type_loc(i,1) /= COMPLEX_NUM) error stop 'column is not of complex type'
        ind = this%type_loc(i,2)

        this%cdata(j,ind) = val

    end subroutine df_change_single_indices_complex


! ~~~~ Change single value of data frame -> header 

    subroutine df_change_single_header_real(this,header,j,val)
        class(data_frame),intent(inout) :: this
        character(len=*),intent(in) :: header
        integer,intent(in) :: j
        real(rk),intent(in) :: val
        
        integer :: ind, data_index
        character(len=:),allocatable :: trunc_header

        if (.not. this%with_headers) error stop "data frame has no headers to look up"

        allocate(character(len(this%headers(1))) :: trunc_header)
        trunc_header = trim(adjustl(header))
        ind = findloc(this%headers,trunc_header,dim=1)
        if (ind < 1) error stop 'header not present in data frame'

        if (this%type_loc(ind,1) /= REAL_NUM) error stop 'column is not of real type'
        data_index = this%type_loc(ind,2)
        this%rdata(j,data_index) = val

    end subroutine df_change_single_header_real

    subroutine df_change_single_header_integer(this,header,j,val)
        class(data_frame),intent(inout) :: this
        character(len=*),intent(in) :: header
        integer,intent(in) :: j
        integer(ik),intent(in) :: val
        
        integer :: ind, data_index
        character(len=:),allocatable :: trunc_header

        if (.not. this%with_headers) error stop "data frame has no headers to look up"

        allocate(character(len(this%headers(1))) :: trunc_header)
        trunc_header = trim(adjustl(header))
        ind = findloc(this%headers,trunc_header,dim=1)
        if (ind < 1) error stop 'header not present in data frame'

        if (this%type_loc(ind,1) /= INTEGER_NUM) error stop 'column is not of integer type'
        data_index = this%type_loc(ind,2)
        this%idata(j,data_index) = val

    end subroutine df_change_single_header_integer

    subroutine df_change_single_header_logical(this,header,j,val)
        class(data_frame),intent(inout) :: this
        character(len=*),intent(in) :: header
        integer,intent(in) :: j
        logical,intent(in) :: val
        
        integer :: ind, data_index
        character(len=:),allocatable :: trunc_header

        if (.not. this%with_headers) error stop "data frame has no headers to look up"

        allocate(character(len(this%headers(1))) :: trunc_header)
        trunc_header = trim(adjustl(header))
        ind = findloc(this%headers,trunc_header,dim=1)
        if (ind < 1) error stop 'header not present in data frame'

        if (this%type_loc(ind,1) /= LOGICAL_NUM) error stop 'column is not of logical type'
        data_index = this%type_loc(ind,2)
        this%ldata(j,data_index) = val

    end subroutine df_change_single_header_logical

    subroutine df_change_single_header_character(this,header,j,val)
        class(data_frame),intent(inout) :: this
        character(len=*),intent(in) :: header
        integer,intent(in) :: j
        character(len=*),intent(in) :: val
        
        integer :: ind, data_index
        character(len=:),allocatable :: trunc_header

        if (.not. this%with_headers) error stop "data frame has no headers to look up"

        allocate(character(len(this%headers(1))) :: trunc_header)
        trunc_header = trim(adjustl(header))
        ind = findloc(this%headers,trunc_header,dim=1)
        if (ind < 1) error stop 'header not present in data frame'

        if (this%type_loc(ind,1) /= CHARACTER_NUM) error stop 'column is not of character type'
        data_index = this%type_loc(ind,2)
        this%chdata(j,data_index) = val

    end subroutine df_change_single_header_character

    subroutine df_change_single_header_complex(this,header,j,val)
        class(data_frame),intent(inout) :: this
        character(len=*),intent(in) :: header
        integer,intent(in) :: j
        complex(rk),intent(in) :: val
        
        integer :: ind, data_index
        character(len=:),allocatable :: trunc_header

        if (.not. this%with_headers) error stop "data frame has no headers to look up"

        allocate(character(len(this%headers(1))) :: trunc_header)
        trunc_header = trim(adjustl(header))
        ind = findloc(this%headers,trunc_header,dim=1)
        if (ind < 1) error stop 'header not present in data frame'

        if (this%type_loc(ind,1) /= COMPLEX_NUM) error stop 'column is not of complex type'
        data_index = this%type_loc(ind,2)
        this%cdata(j,data_index) = val

    end subroutine df_change_single_header_complex


! ~~~~ Change col of data frame with index

    subroutine df_change_col_index_real(this,i,col)
        class(data_frame),intent(inout) :: this
        integer,intent(in) :: i
        real(rk),dimension(:),intent(in) :: col

        integer :: data_ind, col_len

        col_len = size(col,dim=1)

        if (this%enforce_length .and. (this%nrows_max /= size(col,dim=1))) then
            error stop 'Different size columns in add col to data_frame'
        end if

        if (this%type_loc(i,1) /= REAL_NUM) error stop 'column is not of real type'
        data_ind = this%type_loc(i,2)

        if ((.not. this%enforce_length) .and. col_len > this%rrows_max) call this%stretch_cols_real(col_len)
        this%rdata(:col_len,data_ind) = col
        if (.not. this%enforce_length) this%col_lens(i) = col_len

    end subroutine df_change_col_index_real

    subroutine df_change_col_index_integer(this,i,col)
        class(data_frame),intent(inout) :: this
        integer,intent(in) :: i
        integer(ik),dimension(:),intent(in) :: col

        integer :: data_ind, col_len

        col_len = size(col,dim=1)

        if (this%enforce_length .and. (this%nrows_max /= size(col,dim=1))) then
            error stop 'Different size columns in add col to data_frame'
        end if

        if (this%type_loc(i,1) /= INTEGER_NUM) error stop 'column is not of integer type'
        data_ind = this%type_loc(i,2)

        if ((.not. this%enforce_length) .and. col_len > this%irows_max) call this%stretch_cols_integer(col_len)
        this%idata(:col_len,data_ind) = col
        if (.not. this%enforce_length) this%col_lens(i) = col_len

    end subroutine df_change_col_index_integer

    subroutine df_change_col_index_logical(this,i,col)
        class(data_frame),intent(inout) :: this
        integer,intent(in) :: i
        logical,dimension(:),intent(in) :: col

        integer :: data_ind, col_len

        col_len = size(col,dim=1)

        if (this%enforce_length .and. (this%nrows_max /= size(col,dim=1))) then
            error stop 'Different size columns in add col to data_frame'
        end if

        if (this%type_loc(i,1) /= LOGICAL_NUM) error stop 'column is not of logical type'
        data_ind = this%type_loc(i,2)

        if ((.not. this%enforce_length) .and. col_len > this%lrows_max) call this%stretch_cols_logical(col_len)
        this%ldata(:col_len,data_ind) = col
        if (.not. this%enforce_length) this%col_lens(i) = col_len

    end subroutine df_change_col_index_logical

    subroutine df_change_col_index_character(this,i,col)
        class(data_frame),intent(inout) :: this
        integer,intent(in) :: i
        character(len=*),dimension(:),intent(in) :: col

        integer :: data_ind, col_len

        col_len = size(col,dim=1)

        if (this%enforce_length .and. (this%nrows_max /= size(col,dim=1))) then
            error stop 'Different size columns in add col to data_frame'
        end if

        if (this%type_loc(i,1) /= CHARACTER_NUM) error stop 'column is not of character type'
        data_ind = this%type_loc(i,2)

        if ((.not. this%enforce_length) .and. col_len > this%chrows_max) call this%stretch_cols_character(col_len)
        this%chdata(:col_len,data_ind) = col
        if (.not. this%enforce_length) this%col_lens(i) = col_len

    end subroutine df_change_col_index_character

    subroutine df_change_col_index_complex(this,i,col)
        class(data_frame),intent(inout) :: this
        integer,intent(in) :: i
        complex(rk),dimension(:),intent(in) :: col

        integer :: data_ind, col_len

        col_len = size(col,dim=1)

        if (this%enforce_length .and. (this%nrows_max /= size(col,dim=1))) then
            error stop 'Different size columns in add col to data_frame'
        end if

        if (this%type_loc(i,1) /= COMPLEX_NUM) error stop 'column is not of complex type'
        data_ind = this%type_loc(i,2)

        if ((.not. this%enforce_length) .and. col_len > this%crows_max) call this%stretch_cols_complex(col_len)
        this%cdata(:col_len,data_ind) = col
        if (.not. this%enforce_length) this%col_lens(i) = col_len

    end subroutine df_change_col_index_complex


! ~~~~ Change col of Data Frame with header

    subroutine df_change_col_header_real(this,header,col)
        class(data_frame),intent(inout) :: this
        character(len=*),intent(in) :: header
        real(rk),dimension(:),intent(in) :: col
        
        integer :: ind, data_index, col_len
        character(len=:),allocatable :: trunc_header

        col_len  = size(col,dim=1)

        if (this%enforce_length .and. (this%nrows_max /= size(col,dim=1))) then
            error stop 'Different size columns in add col to data_frame'
        end if

        if (.not. this%with_headers) error stop "data frame has no headers to look up"

        allocate(character(len(this%headers(1))) :: trunc_header)
        trunc_header = trim(adjustl(header))
        ind = findloc(this%headers,trunc_header,dim=1)
        if (ind < 1) error stop 'header not present in data frame'

        if (this%type_loc(ind,1) /= REAL_NUM) error stop 'column is not of real type'
        data_index = this%type_loc(ind,2)

        if ((.not. this%enforce_length) .and. col_len > this%rrows_max) call this%stretch_cols_real(col_len)
        this%rdata(:col_len,data_index) = col
        if (.not. this%enforce_length) this%col_lens(ind) = col_len

    end subroutine df_change_col_header_real

    subroutine df_change_col_header_integer(this,header,col)
        class(data_frame),intent(inout) :: this
        character(len=*),intent(in) :: header
        integer(ik),dimension(:),intent(in) :: col
        
        integer :: ind, data_index, col_len
        character(len=:),allocatable :: trunc_header

        col_len  = size(col,dim=1)

        if (this%enforce_length .and. (this%nrows_max /= size(col,dim=1))) then
            error stop 'Different size columns in add col to data_frame'
        end if

        if (.not. this%with_headers) error stop "data frame has no headers to look up"

        allocate(character(len(this%headers(1))) :: trunc_header)
        trunc_header = trim(adjustl(header))
        ind = findloc(this%headers,trunc_header,dim=1)
        if (ind < 1) error stop 'header not present in data frame'

        if (this%type_loc(ind,1) /= INTEGER_NUM) error stop 'column is not of integer type'
        data_index = this%type_loc(ind,2)

        if ((.not. this%enforce_length) .and. col_len > this%irows_max) call this%stretch_cols_integer(col_len)
        this%idata(:col_len,data_index) = col
        if (.not. this%enforce_length) this%col_lens(ind) = col_len

    end subroutine df_change_col_header_integer

    subroutine df_change_col_header_logical(this,header,col)
        class(data_frame),intent(inout) :: this
        character(len=*),intent(in) :: header
        logical,dimension(:),intent(in) :: col
        
        integer :: ind, data_index, col_len
        character(len=:),allocatable :: trunc_header

        col_len  = size(col,dim=1)

        if (this%enforce_length .and. (this%nrows_max /= size(col,dim=1))) then
            error stop 'Different size columns in add col to data_frame'
        end if

        if (.not. this%with_headers) error stop "data frame has no headers to look up"

        allocate(character(len(this%headers(1))) :: trunc_header)
        trunc_header = trim(adjustl(header))
        ind = findloc(this%headers,trunc_header,dim=1)
        if (ind < 1) error stop 'header not present in data frame'

        if (this%type_loc(ind,1) /= LOGICAL_NUM) error stop 'column is not of logical type'
        data_index = this%type_loc(ind,2)

        if ((.not. this%enforce_length) .and. col_len > this%lrows_max) call this%stretch_cols_logical(col_len)
        this%ldata(:col_len,data_index) = col
        if (.not. this%enforce_length) this%col_lens(ind) = col_len

    end subroutine df_change_col_header_logical

    subroutine df_change_col_header_character(this,header,col)
        class(data_frame),intent(inout) :: this
        character(len=*),intent(in) :: header
        character(len=this%max_char_len),dimension(:),intent(in) :: col
        
        integer :: ind, data_index, col_len
        character(len=:),allocatable :: trunc_header

        col_len  = size(col,dim=1)

        if (this%enforce_length .and. (this%nrows_max /= size(col,dim=1))) then
            error stop 'Different size columns in add col to data_frame'
        end if

        if (.not. this%with_headers) error stop "data frame has no headers to look up"

        allocate(character(len(this%headers(1))) :: trunc_header)
        trunc_header = trim(adjustl(header))
        ind = findloc(this%headers,trunc_header,dim=1)
        if (ind < 1) error stop 'header not present in data frame'

        if (this%type_loc(ind,1) /= CHARACTER_NUM) error stop 'column is not of character type'
        data_index = this%type_loc(ind,2)

        if ((.not. this%enforce_length) .and. col_len > this%chrows_max) call this%stretch_cols_character(col_len)
        this%chdata(:col_len,data_index) = col
        if (.not. this%enforce_length) this%col_lens(ind) = col_len

    end subroutine df_change_col_header_character

    subroutine df_change_col_header_complex(this,header,col)
        class(data_frame),intent(inout) :: this
        character(len=*),intent(in) :: header
        complex(rk),dimension(:),intent(in) :: col
        
        integer :: ind, data_index, col_len
        character(len=:),allocatable :: trunc_header

        col_len  = size(col,dim=1)

        if (this%enforce_length .and. (this%nrows_max /= size(col,dim=1))) then
            error stop 'Different size columns in add col to data_frame'
        end if

        if (.not. this%with_headers) error stop "data frame has no headers to look up"

        allocate(character(len(this%headers(1))) :: trunc_header)
        trunc_header = trim(adjustl(header))
        ind = findloc(this%headers,trunc_header,dim=1)
        if (ind < 1) error stop 'header not present in data frame'

        if (this%type_loc(ind,1) /= COMPLEX_NUM) error stop 'column is not of complex type'
        data_index = this%type_loc(ind,2)

        if ((.not. this%enforce_length) .and. col_len > this%crows_max) call this%stretch_cols_complex(col_len)
        this%cdata(:col_len,data_index) = col
        if (.not. this%enforce_length) this%col_lens(ind) = col_len

    end subroutine df_change_col_header_complex


! ~~~~ Write Data Frame

    !subroutine df_write_unformatted(this,unit,iostat)
    subroutine df_write_unformatted(this,unit)
        ! If format args are given, must be in form: "i20", "f10.5", "ES30.20", etc.
        ! -> Formats canot have parentheses 
        class(data_frame),intent(in) :: this
        integer,intent(in),optional :: unit
        !integer,intent(out),optional :: iostat

        character(len=100) :: rfmt, ifmt, lfmt, chfmt, cfmt, fmt_widthch, pfmt
        character(len=:),allocatable :: output_char
        integer :: fmt_widthi
        integer :: io_unit, io_err

        integer :: i, j
        integer :: num_cols, len_cols

        real(rk) :: rval, rfill
        integer(ik) :: ival, ifill
        logical :: lval, lfill
        character(len=this%max_char_len) :: chval, chfill
        complex(rk) :: cval, cfill

        logical :: outside_col

        rfill = 0.0_rk
        ifill = 0
        lfill = .false.
        chfill = trim(adjustl("null"))
        cfill = 0.0_rk


        if (present(unit)) then
            io_unit = unit
        else
            io_unit = STD_OUT
        end if

        fmt_widthi = 23
        allocate(character(fmt_widthi) :: output_char)
        write(fmt_widthch,"(i10)") fmt_widthi
        rfmt = "ES"//trim(adjustl(fmt_widthch))//".16"    
        ifmt = "i"//trim(adjustl(fmt_widthch))
        lfmt = "l"//trim(adjustl(fmt_widthch))
        chfmt = "a"//trim(adjustl(fmt_widthch))
        cfmt = "2"//trim(adjustl(rfmt))


        num_cols = this%n
        len_cols = this%nrows_max
        
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
                select case (this%type_loc(j,1))
                    case (REAL_NUM)
                        pfmt = "(a2,"//trim(adjustl(rfmt))//",a1)"
                        if (i > this%rrows_max) then
                            rval = rfill
                        else
                            rval = this%getr(j,i)
                        end if
                        write(io_unit,pfmt,iostat=io_err,advance='no') "| ", rval, " "
                    case (INTEGER_NUM)
                        pfmt = "(a2,"//trim(adjustl(ifmt))//",a1)"
                        if (i > this%irows_max) then
                            ival = ifill
                        else
                            ival = this%geti(j,i)
                        end if
                        write(io_unit,pfmt,iostat=io_err,advance='no') "| ", ival, " "
                    case (LOGICAL_NUM)
                        pfmt = "(a2,"//trim(adjustl(lfmt))//",a1)"
                        if (i > this%lrows_max) then
                            lval = lfill
                        else
                            lval = this%getl(j,i)
                        end if
                        write(io_unit,pfmt,iostat=io_err,advance='no') "| ", lval, " "
                    case (CHARACTER_NUM)
                        pfmt = "(a2,"//trim(adjustl(chfmt))//",a1)"
                        if (i > this%chrows_max) then
                            chval = chfill
                        else
                            chval = this%getch(j,i)
                        end if
                        output_char = trim(adjustl(chval))
                        write(io_unit,pfmt,iostat=io_err,advance='no') "| ", adjustr(output_char), " "
                    case (COMPLEX_NUM)
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

        allocate(character(len=line_len+100) :: line)
        read(unit=unit,fmt='(a)',iostat=io_err) line
        if (io_err /= 0) then
            err_msg = err_msg_io_read//" "//trim(adjustl(filename))
            error stop err_msg
        end if
        split_line = split(line," ")

        if (has_headers) then
            headers = split_line
            read(unit=unit,fmt='(a)',iostat=io_err) line
            if (io_err /= 0) then
                err_msg = err_msg_io_read//" "//trim(adjustl(filename))
                error stop err_msg
            end if
            split_line = split(line," ")
            do i=1,num_cols
                dtype = what_type(split_line(i))
                select case (dtype)
                    case (REAL_NUM)
                        call this%append_emptyr(num_lines-1,trim(adjustl(headers(i))))
                        read(split_line(i),fmt=*) rval
                        call this%setr(i,1,rval)
                    case (INTEGER_NUM)
                        call this%append_emptyi(num_lines-1,trim(adjustl(headers(i))))
                        read(split_line(i),fmt=*) ival
                        call this%seti(i,1,ival)
                    case (LOGICAL_NUM)
                        call this%append_emptyl(num_lines-1,trim(adjustl(headers(i))))
                        read(split_line(i),fmt=*) lval
                        call this%setl(i,1,lval)
                    case (CHARACTER_NUM)
                        call this%append_emptych(num_lines-1,trim(adjustl(headers(i))))
                        call this%setch(i,1,split_line(i))
                    case (COMPLEX_NUM)
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
                    case (REAL_NUM)
                        call this%append_emptyr(num_lines)
                        read(split_line(i),fmt=*) rval
                        call this%setr(i,1,rval)
                    case (INTEGER_NUM)
                        call this%append_emptyi(num_lines)
                        read(split_line(i),fmt=*) ival
                        call this%seti(i,1,ival)
                    case (LOGICAL_NUM)
                        call this%append_emptyl(num_lines)
                        read(split_line(i),fmt=*) lval
                        call this%setl(i,1,lval)
                    case (CHARACTER_NUM)
                        call this%append_emptych(num_lines)
                        call this%setch(i,1,split_line(i))
                    case (COMPLEX_NUM)
                        call this%append_emptyc(num_lines)
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
            split_line = split(line," ")
            do i=1,num_cols
                select case (this%type_loc(i,1))
                case (REAL_NUM)
                    read(split_line(i),fmt=*) rval
                    call this%setr(i,line_ind+offset,rval)
                case (INTEGER_NUM)
                    read(split_line(i),fmt=*) ival
                    call this%seti(i,line_ind+offset,ival)
                case (LOGICAL_NUM)
                    read(split_line(i),fmt=*) lval
                    call this%setl(i,line_ind+offset,lval)
                case (CHARACTER_NUM)
                    call this%setch(i,line_ind+offset,split_line(i))
                case (COMPLEX_NUM)
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
        line_split = split(line)
        num_cols = size(line_split,dim=1)

        rewind(unit)

    end subroutine get_num_cols

end module df_fortranDF
