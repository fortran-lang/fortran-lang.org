---
layout: book
title: Fortran Intrinsics
permalink: /learn/intrinsics
---
This is a collection of extended descriptions of the Fortran intrinsics
based on the reference document
"[Current F2018 Working Document as of April 2018](http://isotc.iso.org/livelink/livelink?func=ll&objId=19442438&objAction=Open)"
with an emphasis on including a complete working example program that
describes common use cases. Vendor-specific extensions are not included.

The standard documents and most vendor-supplied descriptions of the
intrinsics are often very brief and concise; often to the point
of the functionality of the intrinsics being obscure, particulary to
someone new to the procedure.

By describing the procedures here in greater detail, including a working
example, and being able to provide links to additional resources
(including additional documents at fortran-lang.org and related
discussions in Fortran Discourse) these documents will hopefully be a
valuable asset for Fortran programmers.

This is a community-driven resource and everyone is encouraged to improve
on the documents. For contribution guidelines see
[MINIBOOKS](https://github.com/fortran-lang/fortran-lang.org/blob/master/MINIBOOKS.md).


## General Intrinsics

|----------------------------|||||------------------------------------------------------------------||------------------------------------------------------------------|
| category                   ||||| page                                                             || description                                                      |
|----------------------------|||||------------------------------------------------------------------||------------------------------------------------------------------|
| *ARRAY:CONSTRUCTION*       ||||| [__merge__](/learn/intrinsics/MERGE)                             || &#9679; Merge variables                                          |
| *ARRAY:CONSTRUCTION*       ||||| [__pack__](/learn/intrinsics/PACK)                               || &#9679; Pack an array into an array of rank one                  |
| *ARRAY:CONSTRUCTION*       ||||| [__spread__](/learn/intrinsics/SPREAD)                           || &#9679; Add a dimension to an array                              |
| *ARRAY:CONSTRUCTION*       ||||| [__unpack__](/learn/intrinsics/UNPACK)                           || &#9679; Store the elements of a vector in an array of higher rank|
|----------------------------|||||------------------------------------------------------------------||------------------------------------------------------------------|
| *ARRAY:INQUIRY*            ||||| [__allocated__](/learn/intrinsics/ALLOCATED)                     || &#9679; Status of an allocatable entity                          |
| *ARRAY:INQUIRY*            ||||| [__is\_contiguous__](/learn/intrinsics/IS_CONTIGUOUS)            || &#9679; Test if object is contiguous                             |
| *ARRAY:INQUIRY*            ||||| [__lbound__](/learn/intrinsics/LBOUND)                           || &#9679; Lower dimension bounds of an array                       |
| *ARRAY:INQUIRY*            ||||| [__rank__](/learn/intrinsics/RANK)                               || &#9679; Rank of a data object                                    |
| *ARRAY:INQUIRY*            ||||| [__shape__](/learn/intrinsics/SHAPE)                             || &#9679; Determine the shape of an array                          |
| *ARRAY:INQUIRY*            ||||| [__size__](/learn/intrinsics/SIZE)                               || &#9679; Determine the size of an array                           |
| *ARRAY:INQUIRY*            ||||| [__ubound__](/learn/intrinsics/UBOUND)                           || &#9679; Upper dimension bounds of an array                       |
|----------------------------|||||------------------------------------------------------------------||------------------------------------------------------------------|
| *ARRAY:LOCATION*           ||||| [__maxloc__](/learn/intrinsics/MAXLOC)                           || &#9679; Location of the maximum value within an array            |
| *ARRAY:LOCATION*           ||||| [__minloc__](/learn/intrinsics/MINLOC)                           || &#9679; Location of the minimum value within an array            |
|----------------------------|||||------------------------------------------------------------------||------------------------------------------------------------------|
| *ARRAY:MANIPULATION*       ||||| [__transpose__](/learn/intrinsics/TRANSPOSE)                     || &#9679; Transpose an array of rank two                           |
|----------------------------|||||------------------------------------------------------------------||------------------------------------------------------------------|
| *ARRAY:REDUCTION*          ||||| [__all__](/learn/intrinsics/ALL)                                 || &#9679; Determines if all the values are true                    |
| *ARRAY:REDUCTION*          ||||| [__any__](/learn/intrinsics/ANY)                                 || &#9679; Determines if any of the values in the logical array are true. |
| *ARRAY:REDUCTION*          ||||| [__count__](/learn/intrinsics/COUNT)                             || &#9679; Count function                                           |
| *ARRAY:REDUCTION*          ||||| [__maxval__](/learn/intrinsics/MAXVAL)                           || &#9679; Determines the maximum value in an array or row          |
| *ARRAY:REDUCTION*          ||||| [__minval__](/learn/intrinsics/MINVAL)                           || &#9679; Minimum value of an array                                |
| *ARRAY:REDUCTION*          ||||| [__product__](/learn/intrinsics/PRODUCT)                         || &#9679; Product of array elements                                |
| *ARRAY:REDUCTION*          ||||| [__sum__](/learn/intrinsics/SUM)                                 || &#9679; Sum the elements of an array                             |
|----------------------------|||||------------------------------------------------------------------||------------------------------------------------------------------|
| *ARRAY:RESHAPE*            ||||| [__reshape__](/learn/intrinsics/RESHAPE)                         || &#9679; Function to reshape an array                             |
|----------------------------|||||------------------------------------------------------------------||------------------------------------------------------------------|
| *MATH:RANDOM*              ||||| [__random\_number__](/learn/intrinsics/RANDOM_NUMBER)            || &#9679; Pseudo-random number                                     |
| *MATH:RANDOM*              ||||| [__random\_seed__](/learn/intrinsics/RANDOM_SEED)                || &#9679; Initialize a pseudo-random number sequence               |
|----------------------------|||||------------------------------------------------------------------||------------------------------------------------------------------|
| *MATH:TRIG*                ||||| [__acos__](/learn/intrinsics/ACOS)                               || &#9679; Arccosine function                                       |
| *MATH:TRIG*                ||||| [__acosh__](/learn/intrinsics/ACOSH)                             || &#9679; Inverse hyperbolic cosine function                       |
| *MATH:TRIG*                ||||| [__asin__](/learn/intrinsics/ASIN)                               || &#9679; Arcsine function                                         |
| *MATH:TRIG*                ||||| [__asinh__](/learn/intrinsics/ASINH)                             || &#9679; Inverse hyperbolic sine function                         |
| *MATH:TRIG*                ||||| [__atan__](/learn/intrinsics/ATAN)                               || &#9679; Arctangent function                                      |
| *MATH:TRIG*                ||||| [__atan2__](/learn/intrinsics/ATAN2)                             || &#9679; Arctangent function                                      |
| *MATH:TRIG*                ||||| [__atanh__](/learn/intrinsics/ATANH)                             || &#9679; Inverse hyperbolic tangent function                      |
| *MATH:TRIG*                ||||| [__cos__](/learn/intrinsics/COS)                                 || &#9679; Cosine function                                          |
| *MATH:TRIG*                ||||| [__cosh__](/learn/intrinsics/COSH)                               || &#9679; Hyperbolic cosine function                               |
| *MATH:TRIG*                ||||| [__sin__](/learn/intrinsics/SIN)                                 || &#9679; Sine function                                            |
| *MATH:TRIG*                ||||| [__sinh__](/learn/intrinsics/SINH)                               || &#9679; Hyperbolic sine function                                 |
| *MATH:TRIG*                ||||| [__tan__](/learn/intrinsics/TAN)                                 || &#9679; Tangent function                                         |
| *MATH:TRIG*                ||||| [__tanh__](/learn/intrinsics/TANH)                               || &#9679; Hyperbolic tangent function                              |
|----------------------------|||||------------------------------------------------------------------||------------------------------------------------------------------|
| *MATH*                     ||||| [__bessel\_j0__](/learn/intrinsics/BESSEL_J0)                    || &#9679; Bessel function of the first kind of order 0             |
| *MATH*                     ||||| [__bessel\_j1__](/learn/intrinsics/BESSEL_J1)                    || &#9679; Bessel function of the first kind of order 1             |
| *MATH*                     ||||| [__bessel\_jn__](/learn/intrinsics/BESSEL_JN)                    || &#9679; Bessel function of the first kind                        |
| *MATH*                     ||||| [__bessel\_y0__](/learn/intrinsics/BESSEL_Y0)                    || &#9679; Bessel function of the second kind of order 0            |
| *MATH*                     ||||| [__bessel\_y1__](/learn/intrinsics/BESSEL_Y1)                    || &#9679; Bessel function of the second kind of order 1            |
| *MATH*                     ||||| [__bessel\_yn__](/learn/intrinsics/BESSEL_YN)                    || &#9679; Bessel function of the second kind                       |
| *MATH*                     ||||| [__erf__](/learn/intrinsics/ERF)                                 || &#9679; Error function                                           |
| *MATH*                     ||||| [__er__](/learn/intrinsics/ERFC)                                 || &#9679; Complementary error function                             |
| *MATH*                     ||||| [__er\_scaled__](/learn/intrinsics/ERFC_SCALED)                  || &#9679; Error function                                           |
| *MATH*                     ||||| [__exp__](/learn/intrinsics/EXP)                                 || &#9679; Exponential function                                     |
| *MATH*                     ||||| [__gamma__](/learn/intrinsics/GAMMA)                             || &#9679; Gamma function                                           |
| *MATH*                     ||||| [__hypot__](/learn/intrinsics/HYPOT)                             || &#9679; Euclidean distance function                              |
| *MATH*                     ||||| [__log__](/learn/intrinsics/LOG)                                 || &#9679; Logarithm function                                       |
| *MATH*                     ||||| [__log10__](/learn/intrinsics/LOG10)                             || &#9679; Base 10 logarithm function                               |
| *MATH*                     ||||| [__log\_gamma__](/learn/intrinsics/LOG_GAMMA)                    || &#9679; Logarithm of the Gamma function                          |
| *MATH*                     ||||| [__norm2__](/learn/intrinsics/NORM2)                             || &#9679; Euclidean vector norm                                    |
| *MATH*                     ||||| [__sqrt__](/learn/intrinsics/SQRT)                               || &#9679; Square-root function                                     |
|----------------------------|||||------------------------------------------------------------------||------------------------------------------------------------------|
| *NUMERIC:TYPE*             ||||| [__aimag__](/learn/intrinsics/AIMAG)                             || &#9679; Imaginary part of complex number                         |
| *NUMERIC:TYPE*             ||||| [__cmplx__](/learn/intrinsics/CMPLX)                             || &#9679; Complex conversion function                              |
| *NUMERIC:TYPE*             ||||| [__int__](/learn/intrinsics/INT)                                 || &#9679; Convert to integer type                                  |
| *NUMERIC:TYPE*             ||||| [__nint__](/learn/intrinsics/NINT)                               || &#9679; Nearest whole number                                     |
| *NUMERIC:TYPE*             ||||| [__real__](/learn/intrinsics/REAL)                               || &#9679; Convert to real type                                     |
| *NUMERIC:TYPE*             ||||| [__float__](/learn/intrinsics/FLOAT)                             || &#9679; Convert integer to default real                          |
| *NUMERIC:TYPE*             ||||| [__dble__](/learn/intrinsics/DBLE)                               || &#9679; Double conversion function                               |
| *NUMERIC:TYPE*             ||||| [__sngl__](/learn/intrinsics/SNGL)                               || &#9679; Convert double precision real to default real            |
|----------------------------|||||------------------------------------------------------------------||------------------------------------------------------------------|
| *NUMERIC*                  ||||| [__abs__](/learn/intrinsics/ABS)                                 || &#9679; Absolute value                                           |
| *NUMERIC*                  ||||| [__aint__](/learn/intrinsics/AINT)                               || &#9679; Truncate to a whole number                               |
| *NUMERIC*                  ||||| [__anint__](/learn/intrinsics/ANINT)                             || &#9679; Nearest whole number                                     |
| *NUMERIC*                  ||||| [__ceiling__](/learn/intrinsics/CEILING)                         || &#9679; Integer ceiling function                                 |
| *NUMERIC*                  ||||| [__conjg__](/learn/intrinsics/CONJG)                             || &#9679; Complex conjugate function                               |
| *NUMERIC*                  ||||| [__dim__](/learn/intrinsics/DIM)                                 || &#9679; Positive difference                                      |
| *NUMERIC*                  ||||| [__dprod__](/learn/intrinsics/DPROD)                             || &#9679; Double product function                                  |
| *NUMERIC*                  ||||| [__floor__](/learn/intrinsics/FLOOR)                             || &#9679; Integer floor function                                   |
| *NUMERIC*                  ||||| [__max__](/learn/intrinsics/MAX)                                 || &#9679; Maximum value of an argument list                        |
| *NUMERIC*                  ||||| [__min__](/learn/intrinsics/MIN)                                 || &#9679; Minimum value of an argument list                        |
| *NUMERIC*                  ||||| [__mod__](/learn/intrinsics/MOD)                                 || &#9679; Remainder function                                       |
| *NUMERIC*                  ||||| [__modulo__](/learn/intrinsics/MODULO)                           || &#9679; Modulo function                                          |
| *NUMERIC*                  ||||| [__sign__](/learn/intrinsics/SIGN)                               || &#9679; Sign copying function                                    |
|----------------------------|||||------------------------------------------------------------------||------------------------------------------------------------------|
| *LOGICAL:TYPE*             ||||| [__logical__](/learn/intrinsics/LOGICAL)                                    || &#9679; Converts one kind of _logical_ variable to another             |
|----------------------------|||||------------------------------------------------------------------||------------------------------------------------------------------|
| *TRANSFORMATIONAL*         ||||| [__cshift__](/learn/intrinsics/CSHIFT)                           || &#9679; Circular shift elements of an array                      |
| *TRANSFORMATIONAL*         ||||| [__dot\_product__](/learn/intrinsics/DOT_PRODUCT)                || &#9679; Dot product function                                     |
| *TRANSFORMATIONAL*         ||||| [__eoshift__](/learn/intrinsics/EOSHIFT)                         || &#9679; End-off shift elements of an array                       |
| *TRANSFORMATIONAL*         ||||| [__matmul__](/learn/intrinsics/MATMUL)                           || &#9679; Matrix multiplication                                    |
| *TRANSFORMATIONAL*         ||||| [__null__](/learn/intrinsics/NULL)                               || &#9679; Function that returns a disassociated pointer            |
| *TRANSFORMATIONAL*         ||||| [__parity__](/learn/intrinsics/PARITY)                           || &#9679; Reduction with exclusive OR                              |
|----------------------------|||||------------------------------------------------------------------||------------------------------------------------------------------|
| *KIND:INQUIRY*             ||||| [__kind__](/learn/intrinsics/KIND)                               || &#9679; Kind of an entity                                        |
|----------------------------|||||------------------------------------------------------------------||------------------------------------------------------------------|
| *KIND*                     ||||| [__selected\_char\_kind__](/learn/intrinsics/SELECTED_CHAR_KIND) || &#9679; Choose character kind such as "Unicode"                  |
| *KIND*                     ||||| [__selected\_int\_kind__](/learn/intrinsics/SELECTED_INT_KIND)   || &#9679; Choose integer kind                                      |
| *KIND*                     ||||| [__selected\_real\_kind__](/learn/intrinsics/SELECTED_REAL_KIND) || &#9679; Choose real kind                                         |
|----------------------------|||||------------------------------------------------------------------||------------------------------------------------------------------|
| *TBD*                      ||||| [__associated__](/learn/intrinsics/ASSOCIATED)                   || &#9679; Status of a pointer or pointer/target pair               |
| *TBD*                      ||||| [__extends\_type\_of__](/learn/intrinsics/EXTENDS_TYPE_OF)       || &#9679; Determine if the dynamic type of A is an extension of the dynamic type of MOLD.|
| *TBD*                      ||||| [__findloc__](/learn/intrinsics/FINDLOC)                         || &#9679; Locate first element of ARRAY identified by MASK along dimension DIM having a value|
| *TBD*                      ||||| [__is\_iostat\_end__](/learn/intrinsics/IS_IOSTAT_END)           || &#9679; Test for end-of-file value                               |
| *TBD*                      ||||| [__is\_iostat\_eor__](/learn/intrinsics/IS_IOSTAT_EOR)           || &#9679; Test for end-of-record value                             |
| *TBD*                      ||||| [__maskl__](/learn/intrinsics/MASKL)                             || &#9679; Left justified mask                                      |
| *TBD*                      ||||| [__maskr__](/learn/intrinsics/MASKR)                             || &#9679; Right justified mask                                     |
| *TBD*                      ||||| [__move\_alloc__](/learn/intrinsics/MOVE_ALLOC)                  || &#9679; Move allocation from one object to another               |
| *TBD*                      ||||| [__present__](/learn/intrinsics/PRESENT)                         || &#9679; Determine whether an optional dummy argument is specified|
| *TBD*                      ||||| [__same\_type\_as__](/learn/intrinsics/SAME_TYPE_AS)             || &#9679; Query dynamic types for equality                         |
|----------------------------|||||------------------------------------------------------------------||------------------------------------------------------------------|

## Character 
### basic procedures specifically for manipulating _character_ variables

|----------------------------|||||-----------------------------------------------------------------------------||------------------------------------------------------------------------|
| category                   ||||| page                                                                        || description                                                            |
|----------------------------|||||-----------------------------------------------------------------------------||------------------------------------------------------------------------|
| *CHARACTER*                ||||| [__achar__](/learn/intrinsics/ACHAR)                                        || &#9679; Return a character in specified position in the ASCII sequence |
| *CHARACTER*                ||||| [__adjustl__](/learn/intrinsics/ADJUSTL)                                    || &#9679; Left-adjust a string                                           |
| *CHARACTER*                ||||| [__adjustr__](/learn/intrinsics/ADJUSTR)                                    || &#9679; Right-adjust a string                                          |
| *CHARACTER*                ||||| [__char__](/learn/intrinsics/CHAR)                                          || &#9679; Character conversion function                                  |
| *CHARACTER*                ||||| [__iachar__](/learn/intrinsics/IACHAR)                                      || &#9679; Code in ASCII collating sequence                               |
| *CHARACTER*                ||||| [__ichar__](/learn/intrinsics/ICHAR)                                        || &#9679; Character-to-integer conversion function                       |
| *CHARACTER*                ||||| [__index__](/learn/intrinsics/INDEX)                                        || &#9679; Position of a substring within a string                        |
| *CHARACTER*                ||||| [__len__](/learn/intrinsics/LEN)                                            || &#9679; Length of a character entity                                   |
| *CHARACTER*                ||||| [__len\_trim__](/learn/intrinsics/LEN_TRIM)                                 || &#9679; Length of a character entity without trailing blank characters |
| *CHARACTER*                ||||| [__lge__](/learn/intrinsics/LGE)                                            || &#9679; Lexical greater than or equal                                  |
| *CHARACTER*                ||||| [__lgt__](/learn/intrinsics/LGT)                                            || &#9679; Lexical greater than                                           |
| *CHARACTER*                ||||| [__lle__](/learn/intrinsics/LLE)                                            || &#9679; Lexical less than or equal                                     |
| *CHARACTER*                ||||| [__llt__](/learn/intrinsics/LLT)                                            || &#9679; Lexical less than                                              |
| *CHARACTER*                ||||| [__new\_line__](/learn/intrinsics/NEW_LINE)                                 || &#9679; New line character                                             |
| *CHARACTER*                ||||| [__repeat__](/learn/intrinsics/REPEAT)                                      || &#9679; Repeated string concatenation                                  |
| *CHARACTER*                ||||| [__scan__](/learn/intrinsics/SCAN)                                          || &#9679; Scan a string for the presence of a set of characters          |
| *CHARACTER*                ||||| [__trim__](/learn/intrinsics/TRIM)                                          || &#9679; Remove trailing blank characters of a string                   |
| *CHARACTER*                ||||| [__verify__](/learn/intrinsics/VERIFY)                                      || &#9679; Scan a string for the absence of a set of characters           |
|----------------------------|||||-----------------------------------------------------------------------------||------------------------------------------------------------------------|

## System Environment
### accessing external system information

|----------------------------|||||-----------------------------------------------------------------------------||------------------------------------------------------------------------|
| category                   ||||| page                                                                        || description                                                            |
|----------------------------|||||-----------------------------------------------------------------------------||------------------------------------------------------------------------|
| *SYSTEM:ENVIRONMENT*       ||||| [__command\_argument\_count__](/learn/intrinsics/COMMAND_ARGUMENT_COUNT)    || &#9679; Get number of command line arguments                           |
| *SYSTEM:ENVIRONMENT*       ||||| [__cpu\_time__](/learn/intrinsics/CPU_TIME)                                 || &#9679; Return CPU processor time in seconds                           |
| *SYSTEM:ENVIRONMENT*       ||||| [__date\_and\_time__](/learn/intrinsics/DATE_AND_TIME)                      || &#9679; Gets current time                                              |
| *SYSTEM:ENVIRONMENT*       ||||| [__execute\_command\_line__](/learn/intrinsics/EXECUTE_COMMAND_LINE)        || &#9679; Execute a shell command                                        |
| *SYSTEM:ENVIRONMENT*       ||||| [__get\_command__](/learn/intrinsics/GET_COMMAND)                           || &#9679; Get the entire command line                                    |
| *SYSTEM:ENVIRONMENT*       ||||| [__get\_command\_argument__](/learn/intrinsics/GET_COMMAND_ARGUMENT)        || &#9679; Get command line arguments                                     |
| *SYSTEM:ENVIRONMENT*       ||||| [__get\_environment\_variable__](/learn/intrinsics/GET_ENVIRONMENT_VARIABLE)|| &#9679; Get an environmental variable                                  |
| *SYSTEM:ENVIRONMENT*       ||||| [__system\_clock__](/learn/intrinsics/SYSTEM_CLOCK)                         || &#9679; Return numeric data from a real-time clock.                    |
|----------------------------|||||-----------------------------------------------------------------------------||------------------------------------------------------------------------|

## C interface
#### procedures useful for binding to C interfaces

|----------------------------|||||-----------------------------------------------------------------------------||------------------------------------------------------------------------|
| category                   ||||| page                                                                        || description                                                            |
|----------------------------|||||-----------------------------------------------------------------------------||------------------------------------------------------------------------|
| *ISO\_C\_BINDING*          ||||| [__c\_associated__](/learn/intrinsics/C_ASSOCIATED)                         || &#9679; Status of a C pointer                                          |
| *ISO\_C\_BINDING*          ||||| [__c\_f\_pointer__](/learn/intrinsics/C_F_POINTER)                          || &#9679; Convert C into Fortran pointer                                 |
| *ISO\_C\_BINDING*          ||||| [__c\_f\_procpointer__](/learn/intrinsics/C_F_PROCPOINTER)                  || &#9679; Convert C into Fortran procedure pointer                       |
| *ISO\_C\_BINDING*          ||||| [__c\_funloc__](/learn/intrinsics/C_FUNLOC)                                 || &#9679; Obtain the C address of a procedure                            |
| *ISO\_C\_BINDING*          ||||| [__c\_loc__](/learn/intrinsics/C_LOC)                                       || &#9679; Obtain the C address of an object                              |
| *ISO\_C\_BINDING*          ||||| [__c\_sizeof__](/learn/intrinsics/C_SIZEOF)                                 || &#9679; Size in bytes of an expression                                 |
|----------------------------|||||-----------------------------------------------------------------------------||------------------------------------------------------------------------|

## Compiler Information
#### information about compiler and compiler options used for building

|----------------------------|||||-----------------------------------------------------------------------------||------------------------------------------------------------------------|
| category                   ||||| page                                                                        || description                                                            |
|----------------------------|||||-----------------------------------------------------------------------------||------------------------------------------------------------------------|
| *COMPILER:INQUIRY*         ||||| [__compiler\_options__](/learn/intrinsics/COMPILER_OPTIONS)                 || &#9679; Options passed to the compiler                                 |
| *COMPILER:INQUIRY*         ||||| [__compiler\_version__](/learn/intrinsics/COMPILER_VERSION)                 || &#9679; Compiler version string                                        |
|----------------------------|||||-----------------------------------------------------------------------------||------------------------------------------------------------------------|

## Bit-level
#### bit-level inquiry and manipulation

|----------------------------|||||-----------------------------------------------------------------------------||------------------------------------------------------------------------|
| category                   ||||| page                                                                        || description                                                            |
|----------------------------|||||-----------------------------------------------------------------------------||------------------------------------------------------------------------|
| *BIT:COMPARE*              ||||| [__bge__](/learn/intrinsics/BGE)                                            || &#9679; Bitwise greater than or equal to                               |
| *BIT:COMPARE*              ||||| [__bgt__](/learn/intrinsics/BGT)                                            || &#9679; Bitwise greater than                                           |
| *BIT:COMPARE*              ||||| [__ble__](/learn/intrinsics/BLE)                                            || &#9679; Bitwise less than or equal to                                  |
| *BIT:COMPARE*              ||||| [__blt__](/learn/intrinsics/BLT)                                            || &#9679; Bitwise less than                                              |
|----------------------------|||||-----------------------------------------------------------------------------||------------------------------------------------------------------------|
| *BIT:INQUIRY*              ||||| [__bit\_size__](/learn/intrinsics/BIT_SIZE)                                 || &#9679; Bit size inquiry function                                      |
| *BIT:INQUIRY*              ||||| [__leadz__](/learn/intrinsics/LEADZ)                                        || &#9679; Number of leading zero bits of an integer                      |
| *BIT:INQUIRY*              ||||| [__popcnt__](/learn/intrinsics/POPCNT)                                      || &#9679; Number of bits set                                             |
| *BIT:INQUIRY*              ||||| [__poppar__](/learn/intrinsics/POPPAR)                                      || &#9679; Parity of the number of bits set                               |
| *BIT:INQUIRY*              ||||| [__storage\_size__](/learn/intrinsics/STORAGE_SIZE)                         || &#9679; Storage size in bits                                           |
| *BIT:INQUIRY*              ||||| [__trailz__](/learn/intrinsics/TRAILZ)                                      || &#9679; Number of trailing zero bits of an integer                     |
|----------------------------|||||-----------------------------------------------------------------------------||------------------------------------------------------------------------|
| *BIT:MANIPULATION*         ||||| [__btest__](/learn/intrinsics/BTEST)                                        || &#9679; Bit test function                                              |
| *BIT:MANIPULATION*         ||||| [__dshiftl__](/learn/intrinsics/DSHIFTL)                                    || &#9679; Combines bits of arguments I and J                             |
| *BIT:MANIPULATION*         ||||| [__dshiftr__](/learn/intrinsics/DSHIFTR)                                    || &#9679; Combines bits of arguments I and J                             |
| *BIT:MANIPULATION*         ||||| [__iall__](/learn/intrinsics/IALL)                                          || &#9679; Bitwise and of array elements                                  |
| *BIT:MANIPULATION*         ||||| [__iand__](/learn/intrinsics/IAND)                                          || &#9679; Bitwise logical and                                            |
| *BIT:MANIPULATION*         ||||| [__iany__](/learn/intrinsics/IANY)                                          || &#9679; Bitwise or of array elements                                   |
| *BIT:MANIPULATION*         ||||| [__ibclr__](/learn/intrinsics/IBCLR)                                        || &#9679; Clear bit                                                      |
| *BIT:MANIPULATION*         ||||| [__ibits__](/learn/intrinsics/IBITS)                                        || &#9679; Bit extraction                                                 |
| *BIT:MANIPULATION*         ||||| [__ibset__](/learn/intrinsics/IBSET)                                        || &#9679; Set bit                                                        |
| *BIT:MANIPULATION*         ||||| [__ieor__](/learn/intrinsics/IEOR)                                          || &#9679; Bitwise logical exclusive or                                   |
| *BIT:MANIPULATION*         ||||| [__ior__](/learn/intrinsics/IOR)                                            || &#9679; Bitwise logical inclusive or                                   |
| *BIT:MANIPULATION*         ||||| [__iparity__](/learn/intrinsics/IPARITY)                                    || &#9679; Bitwise exclusive or of array elements                         |
| *BIT:MANIPULATION*         ||||| [__ishft__](/learn/intrinsics/ISHFT)                                        || &#9679; Shift bits                                                     |
| *BIT:MANIPULATION*         ||||| [__ishftc__](/learn/intrinsics/ISHFTC)                                      || &#9679; Shift bits circularly                                          |
| *BIT:MANIPULATION*         ||||| [__merge\_bits__](/learn/intrinsics/MERGE_BITS)                             || &#9679; Merge of bits under mask                                       |
| *BIT:MANIPULATION*         ||||| [__mvbits__](/learn/intrinsics/MVBITS)                                      || &#9679; Move bits from one integer to another                          |
| *BIT:MANIPULATION*         ||||| [__not__](/learn/intrinsics/NOT)                                            || &#9679; Logical negation                                               |
| *BIT:MANIPULATION*         ||||| [__shifta__](/learn/intrinsics/SHIFTA)                                      || &#9679; Shift bits right with fill                                     |
| *BIT:MANIPULATION*         ||||| [__shiftl__](/learn/intrinsics/SHIFTL)                                      || &#9679; Shift bits left                                                |
| *BIT:MANIPULATION*         ||||| [__shiftr__](/learn/intrinsics/SHIFTR)                                      || &#9679; Shift bits right                                               |
| *BIT:MANIPULATION*         ||||| [__transfer__](/learn/intrinsics/TRANSFER)                                  || &#9679; Transfer bit patterns                                          |
|----------------------------|||||-----------------------------------------------------------------------------||------------------------------------------------------------------------|

## Numeric Model
#### These routines support controlling and querying the current numeric model.

|----------------------------|||||-----------------------------------------------------------------------------||------------------------------------------------------------------------|
| category                   ||||| page                                                                        || description                                                            |
|----------------------------|||||-----------------------------------------------------------------------------||------------------------------------------------------------------------|
| *MODEL\_COMPONENTS*        ||||| [__exponent__](/learn/intrinsics/EXPONENT)                                  || &#9679; Exponent function                                              |
| *MODEL\_COMPONENTS*        ||||| [__fraction__](/learn/intrinsics/FRACTION)                                  || &#9679; Fractional part of the model representation                    |
| *MODEL\_COMPONENTS*        ||||| [__nearest__](/learn/intrinsics/NEAREST)                                    || &#9679; Nearest representable number                                   |
| *MODEL\_COMPONENTS*        ||||| [__rrspacing__](/learn/intrinsics/RRSPACING)                                || &#9679; Reciprocal of the relative spacing                             |
| *MODEL\_COMPONENTS*        ||||| [__scale__](/learn/intrinsics/SCALE)                                        || &#9679; Scale a real value                                             |
| *MODEL\_COMPONENTS*        ||||| [__set\_exponent__](/learn/intrinsics/SET_EXPONENT)                         || &#9679; Set the exponent of the model                                  |
| *MODEL\_COMPONENTS*        ||||| [__spacing__](/learn/intrinsics/SPACING)                                    || &#9679; Smallest distance between two numbers of a given type          |
|----------------------------|||||-----------------------------------------------------------------------------||------------------------------------------------------------------------|
| *NUMERIC\_MODEL*           ||||| [__digits__](/learn/intrinsics/DIGITS)                                      || &#9679; Significant digits function                                    |
| *NUMERIC\_MODEL*           ||||| [__epsilon__](/learn/intrinsics/EPSILON)                                    || &#9679; Epsilon function                                               |
| *NUMERIC\_MODEL*           ||||| [__huge__](/learn/intrinsics/HUGE)                                          || &#9679; Largest number of a kind                                       |
| *NUMERIC\_MODEL*           ||||| [__maxexponent__](/learn/intrinsics/MAXEXPONENT)                            || &#9679; Maximum exponent of a real kind                                |
| *NUMERIC\_MODEL*           ||||| [__minexponent__](/learn/intrinsics/MINEXPONENT)                            || &#9679; Minimum exponent of a real kind                                |
| *NUMERIC\_MODEL*           ||||| [__precision__](/learn/intrinsics/PRECISION)                                || &#9679; Decimal precision of a real kind                               |
| *NUMERIC\_MODEL*           ||||| [__radix__](/learn/intrinsics/RADIX)                                        || &#9679; Base of a model number                                         |
| *NUMERIC\_MODEL*           ||||| [__range__](/learn/intrinsics/RANGE)                                        || &#9679; Decimal exponent range of a real kind                          |
| *NUMERIC\_MODEL*           ||||| [__tiny__](/learn/intrinsics/TINY)                                          || &#9679; Smallest positive number of a real kind                        |
|----------------------------|||||-----------------------------------------------------------------------------||------------------------------------------------------------------------|

## Parallel Programming
#### These routines support parallel programming using co_arrays and co_indexed arrays.

|----------------------------|||||-----------------------------------------------------------------------------||------------------------------------------------------------------------|
| category                   ||||| page                                                                        || description                                                            |
|----------------------------|||||-----------------------------------------------------------------------------||------------------------------------------------------------------------|
| *COLLECTIVE*               ||||| [__co\_broadcast__](/learn/intrinsics/CO_BROADCAST)                         || &#9679; Copy a value to all images the current set of images           |
| *COLLECTIVE*               ||||| [__co\_lbound__](/learn/intrinsics/CO_LBOUND)                               || &#9679; Lower codimension bounds of an array                           |
| *COLLECTIVE*               ||||| [__co\_max__](/learn/intrinsics/CO_MAX)                                     || &#9679; Maximal value on the current set of images                     |
| *COLLECTIVE*               ||||| [__co\_min__](/learn/intrinsics/CO_MIN)                                     || &#9679; Minimal value on the current set of images                     |
| *COLLECTIVE*               ||||| [__co\_reduce__](/learn/intrinsics/CO_REDUCE)                               || &#9679; Reduction of values on the current set of images               |
| *COLLECTIVE*               ||||| [__co\_sum__](/learn/intrinsics/CO_SUM)                                     || &#9679; Sum of values on the current set of images                     |
| *COLLECTIVE*               ||||| [__co\_ubound__](/learn/intrinsics/CO_UBOUND)                               || &#9679; Upper codimension bounds of an array                           |
| *COLLECTIVE*               ||||| [__event\_query__](/learn/intrinsics/EVENT_QUERY)                           || &#9679; Query whether a coarray event has occurred                     |
| *COLLECTIVE*               ||||| [__image\_index__](/learn/intrinsics/IMAGE_INDEX)                           || &#9679; Cosubscript to image index conversion                          |
| *COLLECTIVE*               ||||| [__num\_images__](/learn/intrinsics/NUM_IMAGES)                             || &#9679; Number of images                                               |
| *COLLECTIVE*               ||||| [__this\_image__](/learn/intrinsics/THIS_IMAGE)                             || &#9679; Cosubscript index of this image                                |
|----------------------------|||||-----------------------------------------------------------------------------||------------------------------------------------------------------------|
| *ATOMIC:BIT\_MANIPULATION* ||||| [__atomic\_and__](/learn/intrinsics/ATOMIC_AND)                             || &#9679; Atomic bitwise AND operation                                   |
| *ATOMIC:BIT\_MANIPULATION* ||||| [__atomic\_fetch\_and__](/learn/intrinsics/ATOMIC_FETCH_AND)                || &#9679; Atomic bitwise AND operation with prior fetch                  |
| *ATOMIC:BIT\_MANIPULATION* ||||| [__atomic\_fetch\_or__](/learn/intrinsics/ATOMIC_FETCH_OR)                  || &#9679; Atomic bitwise OR operation with prior fetch                   |
| *ATOMIC:BIT\_MANIPULATION* ||||| [__atomic\_fetch\_xor__](/learn/intrinsics/ATOMIC_FETCH_XOR)                || &#9679; Atomic bitwise XOR operation with prior fetch                  |
| *ATOMIC:BIT\_MANIPULATION* ||||| [__atomic\_or__](/learn/intrinsics/ATOMIC_OR)                               || &#9679; Atomic bitwise OR operation                                    |
| *ATOMIC:BIT\_MANIPULATION* ||||| [__atomic\_xor__](/learn/intrinsics/ATOMIC_XOR)                             || &#9679; Atomic bitwise OR operation                                    |
|----------------------------|||||-----------------------------------------------------------------------------||------------------------------------------------------------------------|
| *ATOMIC*                   ||||| [__atomic\_add__](/learn/intrinsics/ATOMIC_ADD)                             || &#9679; Atomic ADD operation                                           |
| *ATOMIC*                   ||||| [__atomic\_cas__](/learn/intrinsics/ATOMIC_CAS)                             || &#9679; Atomic compare and swap                                        |
| *ATOMIC*                   ||||| [__atomic\_define__](/learn/intrinsics/ATOMIC_DEFINE)                       || &#9679; Setting a variable atomically                                  |
| *ATOMIC*                   ||||| [__atomic\_fetch\_add__](/learn/intrinsics/ATOMIC_FETCH_ADD)                || &#9679; Atomic ADD operation with prior fetch                          |
| *ATOMIC*                   ||||| [__atomic\_ref__](/learn/intrinsics/ATOMIC_REF)                             || &#9679; Obtaining the value of a variable atomically                   |
|----------------------------|||||-----------------------------------------------------------------------------||------------------------------------------------------------------------|

## Text Content Copyrights
The above table and the documents themselves indication the license under which
the document falls.

Many of the documents presented here are modified versions of man-pages from the
[Fortran Wiki](https://fortranwiki.org)
and as such are available under the terms of the GNU
Free Documentation License [      ](GNU_Free_Documentation_License.md)
with no invariant sections, front-cover texts, or back-cover texts.


If you contribute to this site by modifying the files marked as GFDL,
you thereby agree to license the contributed material to the public
under the GFDL (version 1.2 or any later version published by the Free
Software Foundation, with no invariant sections, front-cover texts,
or back-cover texts).

If you contribute new material you thereby agree to release it under
the MIT license.

###### Written in [Markdown](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet) [kramdown](https://kramdown.gettalong.org/syntax.html)

### BUG OR ISSUE
Some work local, some work when installed perhaps because no <base> statement is used but this file is moved
to a directory above all the other pages. b,d,f,g work locally, even though the intrinsics directory is not
specified so basing must be done but not sure where; and .md suffix is looked for. So which ones work installed?

A[merge]({{site.baseurl}}/learn/intrinsics/MERGE.md)
C[merge](/learn/intrinsics/MERGE.md)
E[merge](./MERGE.md)

b[merge]({{site.baseurl}}/learn/intrinsics/MERGE)
d[merge](/learn/intrinsics/MERGE)
f[merge](./MERGE)
g[merge](MERGE)
