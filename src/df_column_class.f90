module df_column_class
    use df_precision
    implicit none

    integer,parameter,public :: INTEGER = 1,       &
                                REAL = 2,          &
                                LOGICAL = 3,       &
                                CHARACTER = 4,     &
                                COMPLEX = 5




! ~~~~~ COLUMN TYPE ~~~~~

    type :: column
        private
        
        integer,public :: dtype, n
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
        generic,public :: new => col_constructor_real,       &
                                 col_constructor_integer,    &
                                 col_constructor_logical,    &
                                 col_constructor_character,  &
                                 col_constructor_complex
        procedure,public :: destroy => col_destructor

        procedure,public :: get_type => get_from_col_dtype

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
        generic,public :: getr => get_from_col_real, get_single_col_real
        generic,public :: geti => get_from_col_integer, get_single_col_integer
        generic,public :: getl => get_from_col_logical, get_single_col_logical
        generic,public :: getch => get_from_col_character, get_single_col_character
        generic,public :: getc => get_from_col_complex, get_single_col_complex
        
        procedure,public :: changer => change_col_real
        procedure,public :: changei => change_col_integer
        procedure,public :: changel => change_col_logical
        procedure,public :: changech => change_col_character
        procedure,public :: changec => change_col_complex

    end type column


contains

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




end module df_column_class