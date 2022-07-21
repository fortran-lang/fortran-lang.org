# SYSTEM_index
### accessing external system information

# COMMAND_ARGUMENT_COUNT
## __Name__

__command\_argument\_count__(3) - \[SYSTEM:COMMAND LINE\] Get number of command line arguments

## __Syntax__
```fortran
    result = command_argument_count()

     integer function command_argument_count() result(count)
     integer :: count
```
## __Description__

   __command\_argument\_count()__ returns the number of arguments passed
   on the command line when the containing program was invoked.

## __Arguments__

None

## __Returns__

  - __count__
    : The return value is of type default _integer_. It is the number of
    arguments passed on the command line when the program was invoked.

## __Examples__

Sample program:

```fortran
program demo_command_argument_count
implicit none
integer :: count
   count = command_argument_count()
   print *, count
end program demo_command_argument_count
```

Sample output:

```bash
   # the command verb does not count
   ./test_command_argument_count
       0
   # quoted strings may count as one argument
   ./test_command_argument_count count arguments
       2
   ./test_command_argument_count 'count arguments'
       1
```
## __Standard__

Fortran 2003 and later

## __See Also__

[__get\_command__(3)](GET_COMMAND),
[__get\_command\_argument__(3)](GET_COMMAND_ARGUMENT)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# GET_COMMAND
## __Name__

__get\_command__(3) - \[SYSTEM:COMMAND LINE\] Get the entire command line

## __Syntax__
```fortran
   call get_command(command, length, status)

    subroutine get_command(command,length,status)
    character(len=*),intent(out),optional :: command
    integer,intent(out),optional :: length
    integer,intent(out),optional :: status
```
## __Description__

Retrieve the entire command line that was used to invoke the program.

Note that what is typed on the command line is often processed by
a shell. The shell typically processes special characters and white
space before passing it to the program. The processing can typically be
turned off by turning off globbing or quoting the command line arguments
and/or changing the default field separators, but this should rarely
be necessary.

## __Returns__

  - __command__
    : Shall be of type _character_ and of default kind. If
    __command__ is present, stores the entire command line that was used to
    invoke the program in __command__.

  - __length__
    : Shall be of type _integer_ and of default kind. If __length__
    is present, it is assigned the length of the command line.

  - __status__
    : Shall be of type _integer_ and of default kind. If __status__
    is present, it is assigned 0 upon success of the command, __-1__ if
    __command__ is too short to store the command line, or a positive value
    in case of an error.

## __Examples__

Sample program:

```fortran
program demo_get_command
implicit none
integer                      :: COMMAND_LINE_LENGTH
character(len=:),allocatable :: COMMAND_LINE
   ! get command line length
   call get_command(length=COMMAND_LINE_LENGTH)
   ! allocate string big enough to hold command line
   allocate(character(len=COMMAND_LINE_LENGTH) :: COMMAND_LINE)
   ! get command line as a string
   call get_command(command=COMMAND_LINE)
   ! trim leading spaces just in case
   COMMAND_LINE=adjustl(COMMAND_LINE)
   write(*,'("OUTPUT:",a)')COMMAND_LINE
end program demo_get_command
```
Results:
```bash
     # note that shell expansion removes some of the whitespace
     # without quotes
     ./test_get_command  arguments    on command   line to   echo

     OUTPUT:./test_get_command arguments on command line to echo

     # using the bash shell with single quotes
     ./test_get_command  'arguments  *><`~[]!{}?"\'| '

     OUTPUT:./test_get_command arguments  *><`~[]!{}?"'|
```
## __Standard__

Fortran 2003 and later

## __See Also__

[__get\_command\_argument__(3)](GET_COMMAND_ARGUMENT),
[__command\_argument\_count__(3)](COMMAND_ARGUMENT_COUNT)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# GET_COMMAND_ARGUMENT
## __Name__

__get\_command\_argument__(3) - \[SYSTEM:COMMAND LINE\] Get command line arguments

## __Syntax__
```fortran
     call get_command_argument(number, value, length, status)

     subroutine get_command_argument(number,value,length.status)
     integer,intent(in)                    :: number
     character(len=*),intent(out),optional :: value
     integer,intent(out),optional          :: length
     integer,intent(out),optional          :: status
```
## __Description__

Retrieve the __number__-th argument that was passed on the command line
when the containing program was invoked.

There is not anything specifically stated about what an argument is but
in practice the arguments are split on whitespace unless the arguments
are quoted and IFS values (Internal Field Separators) used by common
shells are ignored.

## __Options__

  - __number__
    : Shall be a scalar of type __integer__, __number \>= 0__. If __number =
    0__, __value__ is set to the name of the program (on systems that support
    this feature).

## __Returns__

  - __value__
    :Shall be a scalar of type _character_ and of default kind. After
    get\_command\_argument returns, the __value__ argument holds the
    __number__-th command line argument. If __value__ can not hold the argument,
    it is truncated to fit the length of __value__. If there are less than
    __number__ arguments specified at the command line, __value__ will be filled
    with blanks.

  - __length__
    :(Optional) Shall be a scalar of type _integer_. The __length__
    argument contains the length of the __number__-th command line argument.

  - __status__
    :(Optional) Shall be a scalar of type _integer_. If the argument
    retrieval fails, __status__ is a positive number; if __value__ contains a
    truncated command line argument, __status__ is __-1__; and otherwise the
    __status__ is zero.

## __Examples__

Sample program:

```fortran
program demo_get_command_argument
implicit none
character(len=255)           :: progname
integer                      :: stat
integer                      :: count,i, longest, argument_length
integer,allocatable          :: istat(:), ilen(:)
character(len=:),allocatable :: args(:)
  !
  ! get number of arguments
  count = command_argument_count()
  write(*,*)'The number of arguments is ',count
  !
  ! simple usage
  !
  call get_command_argument (0, progname, status=stat)
  if (stat == 0) then
     print *, "The program's name is " // trim (progname)
  endif
  !
  ! showing how to make an array to hold any argument list
  !
  ! find longest argument
  !
  longest=0
  do i=0,count
     call get_command_argument(number=i,length=argument_length)
     longest=max(longest,argument_length)
  enddo
  !
  ! allocate string array big enough to hold command line 
  ! argument strings and related information
  !
  allocate(character(len=longest) :: args(0:count))
  allocate(istat(0:count))
  allocate(ilen(0:count))
  !
  ! read the arguments into the array
  !
  do i=0,count
    call get_command_argument(i, args(i),status=istat(i),length=ilen(i))
  enddo
  !
  ! show the results
  !
  write (*,'(i3.3,1x,i0.5,1x,i0.5,1x,"[",a,"]")') &
  & (i,istat(i),ilen(i),args(i)(:ilen(i)),i=0,count)
end program demo_get_command_argument
```
Results:
```text
/demo_get_command_argument a    test  'of getting   arguments  ' "  leading"

 The number of arguments is            5
 The program's name is xxx
000 00000 00003 [./test_get_command_argument]
001 00000 00001 [a]
003 00000 00004 [test]
004 00000 00024 [of getting   arguments  ]
005 00000 00018 [  leading]
```
## __Standard__

Fortran 2003 and later

## __See Also__

[__get\_command__(3)](GET_COMMAND),
[__command\_argument\_count__(3)](COMMAND_ARGUMENT_COUNT)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# CPU_TIME
## __Name__

__cpu\_time__(3) - \[SYSTEM:TIME\] return CPU processor time in seconds

## __Syntax__
```fortran
     call cpu_time(time)
     real,intent(out) :: time
```
## __Description__

Returns a _real_ value representing the elapsed CPU time in seconds. This
is useful for testing segments of code to determine execution time.

The exact definition of time is left imprecise because of the
variability in what different processors are able to provide.

If no time source is available, TIME is set to a negative value.

Note that TIME may contain a system dependent, arbitrary offset and may
not start with 0.0. For cpu\_time the absolute value is meaningless.
Only differences between subsequent calls, as shown in the example
below, should be used.

A processor for which a single result is inadequate (for example, a
parallel processor) might choose to provide an additional version for
which time is an array.

## __Returns__

  - __TIME__
    : The type shall be _real_ with __intent(out)__. It is assigned a
    processor-dependent approximation to the processor time in seconds.
    If the processor cannot return a meaningful time, a
    processor-dependent negative value

      - __is returned.__
        The start time is left imprecise because the purpose is to time
        sections of code, as in the example. This might or might not
        include system overhead time.

## __Examples__

Sample program:

```fortran
program demo_cpu_time
implicit none
real :: start, finish
   !
   call cpu_time(start)
   ! put code to test here
   call cpu_time(finish)
   !
   ! writes processor time taken by the piece of code.
   print '("Processor Time = ",f6.3," seconds.")',finish-start
end program demo_cpu_time
```
  Results:
```text
   Processor Time =  0.000 seconds.
```
## __Standard__

Fortran 95 and later

## __See Also__

[__system\_clock__(3)](SYSTEM_CLOCK),
[__date\_and\_time__(3)](DATE_AND_TIME)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# DATE_AND_TIME
## __Name__

__date\_and\_time__(3) - \[SYSTEM:TIME\] gets current time

## __Syntax__
```fortran
    subroutine date_and_time(date, time, zone, values)

     character(len=8),intent(out),optional :: date
     character(len=10),intent(out),optional :: time
     character(len=5),intent(out),optional :: zone
     integer,intent(out),optional :: values(8)
```
## __Description__

__date\_and\_time(date, time, zone, values)__ gets the corresponding
date and time information from the real-time system clock.

Unavailable time and date _character_ parameters return blanks.

## __Arguments__

  - __date__
    : A character string of default kind of the form CCYYMMDD, of length 8 or larger.

  - __time__
    : A character string of default kind of the form HHMMSS.SSS, of length 10 or larger.

  - __zone__
    : A character string of default kind of the form (+-)HHMM, of length 5 or larger,
    representing the difference with respect to Coordinated Universal Time (UTC).

  - __values__
    : An _integer_ array of eight elements that contains:

      - __values__(1)
      : The year
      - __values__(2)
      : The month
      - __values__(3)
      : The day of the month
      - __values__(4)
      : Time difference with UTC in minutes
      - __values__(5)
      : The hour of the day
      - __values__(6)
      : The minutes of the hour
      - __values__(7)
      : The seconds of the minute
      - __values__(8)
      : The milliseconds of the second

## __Examples__

Sample program:

```fortran
program demo_date_and_time
implicit none
character(len=8)     :: date
character(len=10)    :: time
character(len=5)     :: zone
integer,dimension(8) :: values

    call date_and_time(date,time,zone,values)

    ! using keyword arguments
    call date_and_time(DATE=date,TIME=time,ZONE=zone)
    print '(*(g0))','DATE="',date,'" TIME="',time,'" ZONE="',zone,'"'

    call date_and_time(VALUES=values)
    write(*,'(i5,a)') &
     & values(1),' - The year', &
     & values(2),' - The month', &
     & values(3),' - The day of the month', &
     & values(4),' - Time difference with UTC in minutes', &
     & values(5),' - The hour of the day', &
     & values(6),' - The minutes of the hour', &
     & values(7),' - The seconds of the minute', &
     & values(8),' - The milliseconds of the second'
end program demo_date_and_time
```
Results:
```
   DATE="20201222" TIME="165738.779" ZONE="-0500"
    2020 - The year
      12 - The month
      22 - The day of the month
    -300 - Time difference with UTC in minutes
      16 - The hour of the day
      57 - The minutes of the hour
      38 - The seconds of the minute
     779 - The milliseconds of the second
```
## __Standard__

Fortran 95 and later

## __See Also__

[__cpu\_time__(3)](CPU_TIME),
[__system\_clock__(3)](SYSTEM_CLOCK)

## __Resources__
 date and time conversion, formatting and computation 

 - [M_time](https://github.com/urbanjost/M_time) 
 - [datetime](https://github.com/wavebitscientific/datetime-fortran)
 - [datetime-fortran](https://github.com/wavebitscientific/datetime-fortran)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# SYSTEM_CLOCK
## __Name__

__system\_clock__(3) - \[SYSTEM:TIME\] Return numeric data from a real-time clock.


## __Syntax__
```fortran
subroutine system_clock(count, count_rate, count_max)

   integer,intent(out),optional  :: count
   integer,intent(out),optional  :: count_rate
    ! or !
   real,intent(out),optional     :: count_rate
   integer,intent(out),optional  :: count_max
```
## __Description__

__system\_clock__ lets you measure durations of time with the precision of
the smallest time increment generally available on a system by returning
processor-dependent values based on the current value of the processor
clock. The __clock__ value is incremented by one for each clock count until
the value __count\_max__ is reached and is then reset to zero at the next
count. __clock__ therefore is a modulo value that lies in the range __0 to
count\_max__. __count\_rate__ and __count\_max__ are assumed constant (even though
CPU rates can vary on a single platform).

__count\_rate__ is system dependent and can vary depending on the kind of
the arguments.

If there is no clock, or querying the clock fails, __count__ is set to
__-huge(count)__, and __count\_rate__ and __count\_max__ are set to zero.

__system\_clock__ is typically used to measure short time intervals (system
clocks may be 24-hour clocks or measure processor clock ticks since
boot, for example). It is most often used for measuring or tracking the
time spent in code blocks in lieu of using profiling tools.

## __Arguments__

  - __count__
    : (optional) shall be an _integer_ scalar. It is assigned a
    processor-dependent value based on the current value of the
    processor clock, or __-huge(count)__ if there is no clock. The
    processor-dependent value is incremented by one for each clock count
    until the value __count\_max__ is reached and is reset to zero at the
    next count. It lies in the range __0__ to __count\_max__ if there is a
    clock.

  - __count\_rate__
    : (optional) shall be an _integer_ or _real_ scalar. It is assigned a
    processor-dependent approximation to the number of processor clock
    counts per second, or zero if there is no clock.

  - __count\_max__
    : (optional) shall be an _integer_ scalar. It is assigned the maximum
    value that __COUNT__ can have, or zero if there is no clock.

## __Examples__

Sample program:
```fortran
program demo_system_clock
implicit none
integer, parameter :: wp = kind(1.0d0)
integer :: count, count_rate, count_max
integer :: start, finish
real    :: time_read

   call system_clock(count, count_rate, count_max)
   write(*,*) count, count_rate, count_max

   call system_clock(start, count_rate)
   ! <<<< code to time
   call system_clock(finish)
   time_read=(finish-start)/real(count_rate,wp)
   write(*,'(a30,1x,f7.4,1x,a)') 'time * : ', time_read, ' seconds'

end program demo_system_clock
```
If the processor clock is a 24-hour clock that registers time at
approximately 18.20648193 ticks per second, at 11:30 A.M. the reference
```fortran
      call system_clock (count = c, count_rate = r, count_max = m)
```
defines

```text
      C = (11*3600+30*60)*18.20648193 = 753748,
      R = 18.20648193, and
      M = 24*3600*18.20648193-1 = 1573039.
```
## __Standard__

Fortran 95 and later

## __See Also__

[__date\_and\_time__(3)](DATE_AND_TIME),
[__cpu\_time__(3)](CPU_TIME)

###### fortran-lang intrinsic descriptions
# EXECUTE_COMMAND_LINE
## __Name__

__execute\_command\_line__(3) - \[SYSTEM:PROCESSES\] Execute a shell command


## __Syntax__
```fortran
   subroutine execute_command_line(command, wait, exitstat, cmdstat, cmdmsg)

    character(len=*),intent(in)  :: command
    logical,intent(in),optional  :: wait
    integer,intent(out),optional :: exitstat
    integer,intent(out),optional :: cmdstat
    character(len=*),intent(out),optional :: cmdmsg
```
## __Description__

The __command__ argument is passed to the shell and executed. (The shell is
generally __sh__(1) on Unix systems, and cmd.exe on Windows.) If __wait__ is
present and has the value __.false.__, the execution of the command is
asynchronous if the system supports it; otherwise, the command is
executed synchronously.

The three last arguments allow the user to get status information. After
synchronous execution, __exitstat__ contains the integer exit code of the
command, as returned by __system__. __cmdstat__ is set to zero if the command
line was executed (whatever its exit status was). __cmdmsg__ is assigned an
error message if an error has occurred.

Note that the system call need not be thread-safe. It is the
responsibility of the user to ensure that the system is not called
concurrently if required.

When the command is executed synchronously, __execute\_command\_line__
returns after the command line has completed execution. Otherwise,
__execute\_command\_line__ returns without waiting.

## __Arguments__

  - __command__
    : a default _character_ scalar containing the command line to be
    executed. The interpretation is programming-environment dependent.

  - __wait__
    : (Optional) a default _logical_ scalar. If __wait__ is present with the
    value .false., and the processor supports asynchronous execution of
    the command, the command is executed asynchronously; otherwise it is
    executed synchronously.

  - __exitstat__
    : (Optional) an _integer_ of the default kind with __intent(inout)__. If
    the command is executed synchronously, it is assigned the value of
    the processor-dependent exit status. Otherwise, the value of
    __exitstat__ is unchanged.

  - __cmdstat__
    : (Optional) an _integer_ of default kind with __intent(inout)__. If an
    error condition occurs and __cmdstat__ is not present, error termination
    of execution of the image is initiated.

    It is assigned the value __-1__ if the processor does not support
    command line execution, a processor-dependent positive value if an
    error condition occurs, or the value __-2__ if no error condition
    occurs but __wait__ is present with the value false and the processor
    does not support asynchronous execution. Otherwise it is assigned
    the value 0.

  - __cmdmsg__
    : (Optional) a _character_ scalar of the default kind. It is an __intent
    (inout)__ argument.If an error condition occurs, it is assigned a
    processor-dependent explanatory message.Otherwise, it is unchanged.

## __Examples__

Sample program:

```fortran
program demo_exec
implicit none
   integer :: i

   call execute_command_line("external_prog.exe", exitstat=i)
   print *, "Exit status of external_prog.exe was ", i

   call execute_command_line("reindex_files.exe", wait=.false.)
   print *, "Now reindexing files in the background"
end program demo_exec
```

## __Note__

Because this intrinsic is making a system call, it is very system
dependent. Its behavior with respect to signaling is processor
dependent. In particular, on POSIX-compliant systems, the SIGINT and
SIGQUIT signals will be ignored, and the SIGCHLD will be blocked. As
such, if the parent process is terminated, the child process might not
be terminated alongside.

## __Standard__

Fortran 2008 and later

###### fortran-lang intrinsic descriptions
# GET_ENVIRONMENT_VARIABLE
## __Name__

__get\_environment\_variable__(3) - \[SYSTEM:ENVIRONMENT\] Get an environmental variable

## __Syntax__
```fortran
  call get_environment_variable(name, value, length, status, trim_name)

   character(len=*),intent(in) :: name
   character(len=*),intent(out),optional :: value
   integer,intent(out),optional :: length
   integer,intent(out),optional :: status
   logical,intent(out),optional :: trim_name
```
## __Description__

Get the __value__ of the environmental variable __name__.

Note that __get\_environment\_variable__(3) need not be thread-safe. It
is the responsibility of the user to ensure that the environment is not
being updated concurrently.

## __Options__

  - __name__
    : The name of the environment variable to query.

    Shall be a scalar of type _character_ and of default kind.

## __Returns__

  - __value__
    : The value of the environment variable being queried.

    Shall be a scalar of type _character_ and of default kind.
    The value of __name__ is stored in __value__. If __value__ is not large enough
    to hold the data, it is truncated. If __name__ is not set, __value__ will be
    filled with blanks.

  - __length__
    : Argument __length__ contains the length needed for storing the
    environment variable __name__ or zero if it is not present.

    Shall be a scalar of type _integer_ and of default kind.

  - __status__
    : __status__ is __-1__ if __value__ is present but too short for the
    environment variable; it is __1__ if the environment variable does not
    exist and __2__ if the processor does not support environment variables;
    in all other cases __status__ is zero.

    Shall be a scalar of type _integer_ and of default kind.

  - __trim\_name__
    : If __trim\_name__ is present with the value __.false.__, the trailing blanks in
    __name__ are significant; otherwise they are not part of the environment
    variable name.

    Shall be a scalar of type _logical_ and of default kind.

## __Examples__

Sample program:

```fortran
program demo_getenv
implicit none
character(len=:),allocatable :: homedir
character(len=:),allocatable :: var
     var='HOME'
     homedir=get_env(var)
     write (*,'(a,"=""",a,"""")')var,homedir

contains

function get_env(NAME,DEFAULT) result(VALUE)
! a function that makes calling get_environment_variable(3) simple
implicit none
character(len=*),intent(in)          :: NAME
character(len=*),intent(in),optional :: DEFAULT
character(len=:),allocatable         :: VALUE
integer                              :: howbig
integer                              :: stat
integer                              :: length
   ! get length required to hold value
   length=0
   VALUE=''
   if(NAME.ne.'')then
      call get_environment_variable( &
      & NAME, length=howbig,status=stat,trim_name=.true.)
      select case (stat)
      case (1)
       !*!print *, NAME, " is not defined in the environment. Strange..."
       VALUE=''
      case (2)
       !*!print *, &
       !*!"This processor does not support environment variables. Boooh!"
       VALUE=''
      case default
       ! make string to hold value of sufficient size
       if(allocated(VALUE))deallocate(VALUE)
       allocate(character(len=max(howbig,1)) :: VALUE)
       ! get value
       call get_environment_variable( &
       & NAME,VALUE,status=stat,trim_name=.true.)
       if(stat.ne.0)VALUE=''
      end select
   endif
   if(VALUE.eq.''.and.present(DEFAULT))VALUE=DEFAULT
end function get_env

end program demo_getenv
```

Typical Results:

```text
   HOME="/home/urbanjs"
```

## __Standard__

Fortran 2003 and later

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
