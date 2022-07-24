## event\_query
### __Name__

__event\_query__(3) - \[COLLECTIVE\] Query whether a coarray event has occurred


### __Syntax__
```fortran
call event_query(event, count, stat)
```
### __Description__

__event\_query__ assigns the number of events to __count__ which have been
posted to the __event__ variable and not yet been removed by calling
__event\_wait__. When __stat__ is present and the invocation was successful, it
is assigned the value __0__. If it is present and the invocation has failed,
it is assigned a positive value and __count__ is assigned the value __-1__.

### __Arguments__

  - __event__
    : (intent(in)) Scalar of type event\_type, defined in
    iso\_fortran\_env; shall not be coindexed.

  - __count__
    : (intent(out))Scalar integer with at least the precision of default
    _integer_.

  - __stat__
    : (OPTIONAL) Scalar default-kind _integer_ variable.

### __Examples__

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

### __Standard__

TS 18508 or later

####### fortran-lang intrinsic descriptions
