---
layout: book
title: execute_command_line
permalink: /learn/intrinsics/EXECUTE_COMMAND_LINE
---
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
