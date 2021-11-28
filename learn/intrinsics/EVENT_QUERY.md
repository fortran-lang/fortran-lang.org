---
layout: book
title: event_query
permalink: /learn/intrinsics/EVENT_QUERY
---
## __Name__

__event\_query__(3) - \[COLLECTIVE\] Query whether a coarray event has occurred
(GFDL)

## __Syntax__

call __EVENT\_QUERY__(EVENT, COUNT \[, STAT\])

## __Description__

EVENT\_QUERY assigns the number of events to COUNT which have been
posted to the EVENT variable and not yet been removed by calling
EVENT\_WAIT. When STAT is present and the invocation was successful, it
is assigned the value 0. If it is present and the invocation has failed,
it is assigned a positive value and COUNT is assigned the value __-1__.

## __Arguments__

  - __EVENT__
    (intent(in)) Scalar of type event\_type, defined in
    iso\_fortran\_env; shall not be coindexed.

  - __COUNT__
    (intent(out))Scalar integer with at least the precision of default
    integer.

  - __STAT__
    (OPTIONAL) Scalar default-kind integer variable.

## __Examples__

Sample program:

```fortran
   program demo_event_query
     use iso_fortran_env
     implicit none
     type(event_type) :: event_value_has_been_set[*]
     integer :: cnt
     if (this_image() == 1) then
       call event_query(event_value_has_been_set, cnt)
       if (cnt > 0) write(*,*) "Value has been set"
     elseif (this_image() == 2) then
       event post(event_value_has_been_set[1])
     endif
   end program demo_event_query
```

## __Standard__

TS 18508 or later
