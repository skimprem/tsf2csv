!****************************************************************************
!
!  PROGRAM: tsf2csv 
!
!  PURPOSE: Convert TSF-files to table file with comma-separated values
!
!****************************************************************************

program tsf2csv
    implicit none

    character(*), parameter :: version = '1.3'
    character(*) :: arg*250, tsfile*250, outfile*250, separator*1
    logical exis, tsfile_bl, outfile_bl, verbose_mode_bl, separator_bl
    integer(4) :: i, j
    tsfile_bl = .false.
    outfile_bl = .false.
    verbose_mode_bl = .false.
    separator_bl = .false.
    separator = ','

    if (command_argument_count() == 0) then
        call print_help()
        stop
    end if


    do while(j < command_argument_count())
        j = j + 1
        call get_command_argument(j, arg)

        select case(arg)
        case('-f', '--file')
            tsfile_bl = .true.
            j = j + 1
            call get_command_argument(j, arg)
            if(blank_string(arg)) call print_error(5, '-f')
            tsfile = adjustl(arg)
        case('-o', '--output')
            outfile_bl = .true.
            j = j + 1
            call get_command_argument(j, arg)
            if(blank_string(arg)) call print_error(5, '-o')
            outfile = adjustl(arg)
        case('-V', '--verbose')
            verbose_mode_bl = .true.
        case('-s', '--separator')
            separator_bl = .true.
            j = j + 1
            call get_command_argument(j, arg)
            if(blank_string(arg)) call print_error(5, '-s')
            separator = adjustl(arg)
        case('-v', '--version')
            print '(2a)', 'tsf2csv version: ', version
            stop
        case('-h', '--help')
            call print_help()
            stop
        end select
    end do

    if(verbose_mode_bl .eqv. .true. .and. outfile_bl .eqv. .false.) verbose_mode_bl = .false.

    if(tsfile_bl) then
        inquire(file=tsfile,exist=exis)
        if(exis .eqv. .false.) call print_error(7, trim(adjustl(tsfile)))
        if(verbose_mode_bl) print *, 'Open TSF-file: ', trim(tsfile)
        if(outfile_bl) then
            call tsfreader(tsfile, verbose_mode_bl, separator, outfile)
        else
            call tsfreader(tsfile, verbose_mode_bl, separator)
        end if
    else
        call print_error()
    end if

    contains

    subroutine tsfreader(input_filename, verbose_mode, separator, output_filename)
    implicit none
    type channel
        character(250) :: location, instrument, channel_name, units, string
        real(8) :: val
    end type
    type tsftype
        character(*) :: version*10, timeformat*30, comment*10000
        real(8) :: undetval
        integer(4) :: increment, channels_number, countinfo
    end type


    character(*), intent(in) :: input_filename, separator
    character(*), intent(in), optional :: output_filename
    character(5000) :: string
    character(20) :: data_time
    character(1) :: yn = 'y'
    character(*) :: back*22
    integer(4) :: un, i, j, k, run, ios, step
    type(tsftype) :: tsfdata
    type(channel) :: channels(:)
    logical, intent(in) :: verbose_mode
    logical :: exis
    real(8) :: real_value, progress, coef

    allocatable channels

    un = fun()
    tsfdata%countinfo = 0
    open(un, file = trim(input_filename), action = 'read', status = 'old')

    do
        read(un, '(a)', end=1) string
        string = adjustl(string)
        i = index(string, ' ')
        string = string(1:i-1)
        
        select case(trim(string))
        case('[TSF-file]')
            backspace(un)
            read(un, *) string, tsfdata%version
            if(verbose_mode) print *, 'TSF-file version: ', trim(tsfdata%version)
        case('[UNDETVAL]')
            backspace(un)
            read(un, *) string, tsfdata%undetval
            if(verbose_mode) print *, 'Undetermined values: ', trim(real2char(tsfdata%undetval))
        case('[TIMEFORMAT]')
            backspace(un)
            read(un, *) string, tsfdata%timeformat
            if(verbose_mode) print *, 'Time format: ', trim(tsfdata%timeformat)
        case('[INCREMENT]')
            backspace(un)
            read(un, *) string, tsfdata%increment
            if(verbose_mode) print *, 'Increment: ', trim(int2char(tsfdata%increment)), ' sec'
        case('[CHANNELS]')
            tsfdata%channels_number = 0
            do while(trim(string) /= "")
                read(un, '(a)') string
                tsfdata%channels_number = tsfdata%channels_number + 1
            end do
            tsfdata%channels_number = tsfdata%channels_number - 1
            if(verbose_mode) print *, 'Channels: ', trim(int2char(tsfdata%channels_number))
            allocate(channels(tsfdata%channels_number))
            do k = 1, tsfdata%channels_number + 1
                backspace(un)
            end do
            do k = 1, tsfdata%channels_number
                read(un, '(a)') string
                j = index(string, ':')
                channels(k)%location = string(1:j-1)
                string = string(j+1:)
                j = index(string, ':')
                channels(k)%instrument = string(1:j-1)
                string = string(j+1:)
                channels(k)%channel_name = string
                if(verbose_mode) print *, '  ', trim(int2char(k)), ':',&
                trim(channels(k)%location), ':', trim(channels(k)%instrument), ':', trim(channels(k)%channel_name)
            end do
        case('[UNITS]')
            if(verbose_mode) print *, 'Channel units:'
            do k = 1, tsfdata%channels_number
                read(un, '(a)') channels(k)%units
                if(verbose_mode) print *, '  ', trim(int2char(k)), ':', trim(channels(k)%units)
            end do
        case('[COMMENT]')
            read(un, '(a)') string
            string = adjustl(string)
            tsfdata%comment = trim(string)
            do while(trim(string) /= "")
                read(un, '(a)') string
                string = adjustl(string)
                k = len_trim(tsfdata%comment)
                tsfdata%comment(k+2:) = trim(string)
            end do
            if(verbose_mode) print *, 'Comment:', trim(tsfdata%comment)
        case('[COUNTINFO]')
            backspace(un)
            read(un, *) string, tsfdata%countinfo
            if(verbose_mode) print *, 'Count of records: ', trim(int2char(tsfdata%countinfo))
        case('[DATA]')
            j = 0; k = 0
            do while(trim(string) /= "" .and. k >= 0)
                read(un, '(a)', iostat = k) string
                j = j + 1
            end do
            j = j - 1
            2 continue
            if(verbose_mode) print *, 'Count of rows: ', trim(int2char(j))
            do k = 1, j + 1
                backspace(un)
            end do
            
            if(tsfdata%countinfo > j) then
                if(verbose_mode) print *, 'WARNING! COUNTINFO greater then rows number in DATA blok'
                tsfdata%countinfo = j
            else if(tsfdata%countinfo < j .and. tsfdata%countinfo > 0) then
                if(verbose_mode) print *, 'WARNING! COUNTINFO less then rows number in DATA block'
            else if(tsfdata%countinfo == 0) then
                if(verbose_mode) print *, 'WARNING! COUNTINFO is not specified or equal to zero'
                tsfdata%countinfo = j
            end if
            if(present(output_filename)) then
                run = fun()
                open(run, file = output_filename, iostat = ios, action = 'write', status = 'new')
                if(ios /= 0) then
                    inquire(file=trim(output_filename),exist=exis)
                    if(exis) then
                        call print_error(10, trim(output_filename))
                        do
                            read(*, *) yn
                            if(trim(adjustl(yn)) == 'y' .or. trim(adjustl(yn)) == 'Y') then
                                close(run)
                                open(run, file=output_filename, action = 'write', status = 'old')
                                exit
                            else if(trim(adjustl(yn)) == 'n' .or. trim(adjustl(yn)) == 'N') then
                                stop
                            else
                                cycle
                            end if
                        end do
                    end if
                end if
            else
                run = 6
            end if

            write(run, *) 'date_time', (separator//trim(channels(k)%channel_name), k = 1, tsfdata%channels_number)
            
            if(verbose_mode) then
                progress = 0.0
                write(6, '(1x,a)', advance = 'no') 'In progress... ['
                step = 2
                coef = 100.0 / tsfdata%countinfo
            end if

            do k = 1, tsfdata%countinfo

                string = ""
                read(un, '(a, a)') data_time, string
                read(string, *) (channels(j)%string, j = 1, tsfdata%channels_number)
                call delete_blank(data_time)
                do j = 1, tsfdata%channels_number
                  if(char2real(channels(j)%string, real_value) .eqv. .true.) then
                    if(real_value == tsfdata%undetval) channels(j)%string = ""
                  else
                    print *, 'ERROR! ***'
                    stop
                  end if
                end do
                write(run, *) trim(data_time), (separator//trim(channels(j)%string), j = 1, tsfdata%channels_number)
                
                if(verbose_mode) then
                    progress = progress + coef
                    if(progress/step > 1) then
                        write(6,'(a)', advance = 'no') '='
                        step = step + 2
                    end if
                end if

            end do

            if(verbose_mode) then
                write(6, '(a)') '] 100%'
                !write(6, '(a)') repeat('=',50)
            end if

            close(run)

        end select
    end do
    
    1 continue

    return
        
    end subroutine tsfreader
    
    subroutine delete_blank(string)
        implicit none
        character(*), intent(inout) :: string
        integer(4) :: i
        string = adjustl(string)
        i = 1
        do while(i > 0)
            i = index(trim(string), ' ')
            string = string(1:i-1)//string(i+1:)
        end do
    end subroutine delete_blank

    !subroutine remove_symbol(string, symbol)
        !implicit none
        !character(*), intent(inout) :: string
        !character(1), intent(in) :: symbol
        !integer(4) :: i
        !string = adjustl(string)
        !i = 1
        !do while(i > 0)
            !i = index(trim(string), ' ')
            !string = string(1:i-1)//string(i+1:)
        !end do
    !end subroutine remove_symbol

    subroutine print_help()
        print '(a)', 'usage: tsf2csv [OPTIONS] [PARAMETER]'
        print '(a)', ''
        print '(a)', 'tsf2csv options:'
        print '(a)', ''
        print '(a)', '-f, --file      input TSF-file'
        print '(a)', ''
        print '(a)', '-o, --output    output CSV-file'
        print '(a)', ''
        print '(a)', '-s, --separator [optionaly] column separator, must be specified'
        print '(a)', '                in quotes " " (dafault comma ",")'
        print '(a)', ''
        print '(a)', '-V, --verbose   [optionaly] vebose mode'
        print '(a)', ''
        print '(a)', '-v, --version   print version information and exit'
        print '(a)', ''
        print '(a)', '-h, --help      this help'
    end subroutine

    ! generation function for free unint number
    function fun()
        ! f - free
        ! u - unit
        ! n - number
        integer(4) fun 
        logical :: State
        integer(4) :: i
        State=.true.
        i = 0
        do while(State .eqv. .true.)
            i = i + 1
            inquire(unit = i, opened = State)
        end do
        fun = i
        return
    end function fun

    function char2real(string, double)
        implicit none
        logical :: char2real
        character(*), intent(in) :: string
        integer :: i
        real(8), intent(out) :: double
        char2real = .true.
        read(string, *, iostat = i) double
        if (i /= 0) char2real = .false.
    end function

    character(250) function real2char(val, fraction_len)
        implicit none
        real(8), intent(in) :: val
        integer, intent(in), optional :: fraction_len
        character(250) :: output, frmt, up, post
        real2char = ''
        if(present(fraction_len)) then
            write(output, *) val
            write(up, *) scan(trim(adjustl(output)), '.') + fraction_len
            write(post, *) fraction_len
            write(frmt, *) '(f'//trim(adjustl(up))//'.'//trim(adjustl(post))//')'
            write(output, frmt) val
        else
            write(output, *) val
        end if
        real2char = adjustl(output)
        return
    end function real2char

    character(250) function int2char(val, frmt)
        implicit none
        integer(4), intent(in) :: val
        character(20), intent(in), optional :: frmt
        character(250) :: output
        if(present(frmt)) then
            write(output, frmt) val
        else
            write(output, *) val
        end if
        int2char = adjustl(output)
        return
    end function int2char

    function blank_string(string)
        implicit none
        logical :: blank_string
        character(*) :: string
        blank_string = .false.
        if (trim(adjustl(string)) == '') blank_string = .true.
    end function

    subroutine print_error(i, arg)
        ! i-parameter
        ! 1: unrecognized option -> stop
        ! 2: do not set option 'arg' -> stop
        ! 3: do not set necessary option -> stop
        ! 4: unrecognized parameter 'arg' -> stop
        ! 5: do not set parameter 'arg' -> stop
        ! 6: do not set necessary parameter -> stop
        ! 7: file not exist -> stop
        ! 8: file not exist -> return
        ! 9: unrecognized parameter 'arg' -> return
        implicit none
        character(*), optional :: arg
        integer, optional :: i
        print '(/)'
        if (present(i)) then
            select case(i)
            case (1)
                print '(a,a,a,/)', 'ERROR: Unrecognized command-line option "', arg, '"'
            case (2)
                print '(a,a,a,/)', 'ERROR: Do not set option "', arg, '"'
            case (3)
                print '(a,/)', 'ERROR: Do not set necessary option!'
            case (4)
                print '(a,a,a,/)', 'ERROR: Unrecognized command-line parameter "', arg, '"'
            case (5)
                print '(a,a,a,/)', 'ERROR: Do not set parameter of option "',&
                arg, '"'
            case (6)
                print '(a,/)', 'ERROR: Do not set necessary parameter!'
            case (7)
                print '(a,a,a,/)', 'ERROR: No such file "', arg, '"'
            case (8)
                print '(a,a,a,/)', 'ERROR: No such file "', arg, '"'
                return
            case (9)
                print '(a,a,a,/)', 'ERROR: Unrecognized parameter "', arg, '" &
                in input file'
                !call print_help()
                return
            case (10)
                write(6, '(1x,a,a,a)', advance = 'no') 'WARNING: File "', arg, '" is exist! Overwrite? [y/n]:'
                return
            case default
                print '(a,/)', 'ERROR!'
            end select
        else
            print '(a,/)', 'ERROR!'
        end if
        !call print_help()
        stop
    end subroutine

end program tsf2csv
