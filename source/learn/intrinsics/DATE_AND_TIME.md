---
layout: book
title: date_and_time
permalink: /learn/intrinsics/DATE_AND_TIME
---
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
