---
layout: book
title: Operators and Control Flow
permalink: /learn/quickstart/operators_control_flow
navbar: Learn
---

One of the powerful advantages of computer algorithms, compared to simple mathematical formulae,
comes in the form program _branching_ whereby the program can decide which instructions to 
execute next based on a logical condition.

There two main forms of controlling program flow:

- _Conditional_ (if): choose program path based on a boolean (true or false) value

- _Loop_: repeat a portion of code multiple times



## Logical operators
Before we use a conditional branching operator, we need to be able to form
a logical expression.

To form a logical expression the following set of relational operators are available:

| Operator &nbsp;  | Alternative &nbsp;    | Description                                                     |
|:----------------:|:---------------------:|-----------------------------------------------------------------|
| `==`             | `.eq.`                | Tests for equality of two operands                              |
| `/=`             | `.ne.`                | Test for inequality of two operands                             |
| `> `             | `.gt.`                | Tests if left operand is strictly greater than right operand    |
| `< `             | `.lt.`                | Tests if left operand is strictly less than right operand       |
| `>=`             | `.ge.`                | Tests if left operand is greater than or equal to right operand |
| `<=`             | `.le.`                | Tests if left operand is less than or equal to right operand    |

<br>

as well as the following logical operators:

| Operator &nbsp; | Description                                                          |
|:---------------------:|----------------------------------------------------------------|
| `.and.`         | TRUE if both left and right operands are TRUE                        |
| `.or.`          | TRUE if either left or right or both operands are TRUE               |
| `.not.`         | TRUE if right operand is FALSE                                       |
| `.eqv.`         | TRUE if left operand has same logical value as right operand         |
| `.neqv.`        | TRUE if left operand has the opposite logical value as right operand |

<br>


## Conditional construct (`if`)

In the following examples, a conditional `if` construct is used to print out a
message to describe the nature of the `angle` variable:

__Example:__ single branch `if`

```fortran
  if (angle < 90.0) then
    print *, 'Angle is acute'
  end if
```

In this first example, the code within the `if` construct is __only executed if__ the
test expression (`angle < 90.0`) is true.

{% include tip.html content="It is good practice to indent code within constructs such as `if` and `do`
to make code more readable." %}

We can add alternative branch to the construct using the `else` keyword:

__Example:__ two-branch `if-else`

```fortran
  if (angle < 90.0) then
    print *, 'Angle is acute'
  else
    print *, 'Angle is obtuse'
  end if
```

Now there are two _branches_ in the `if` construct, but __only one branch is executed__ depending
on the logical expression following the `if` keyword.

We can actually add any number of branches using `else if` to specify more conditions:

__Example:__ multi-branch  `if-elseif-else`
```fortran
  if (age < 90.0) then
    print *, 'Angle is acute'
  else if (angle < 180.0) then
    print *, 'Angle is obtuse'
  else
    print *, 'Angle is reflex'
  end if
```

When multiple conditional expressions are used, each conditional expression is tested only if none of the previous
expressions have evaluated to true.

## Loop constructs (`do`)

In the following example a `do` loop construct is used to print out the numbers in
a sequence.
The `do` loop has an integer _counter_ variable which is used to track which iteration of the loop
is currently executing, in this example we use a common name for this counter variable: `i`.

When we define the start of the `do` loop we use our counter variable name followed by an equals (`=`) sign
to specify the start value and final value of our counting variable.

__Example:__ `do` loop

```fortran
  integer :: i
  do i=1,10
    print *, i
  end do
```

__Example:__ `do` loop with skip

```fortran
  integer :: i
  do i=1,10,2    
    print *, i   ! Print odd numbers
  end do
```


__Example:__ `do while` loop

```fortran
  integer :: i
  i = 1
  do while (i<11)   
    print *, i
    i = i + 1
  end do
```