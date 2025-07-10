# About

This package adds vector/list support to the Tcl expr command.

It's a proof-of-concept implementation: Use it at your own risk.

# Example

This example computes the median absolute deviation of a vector/list:

    set x {2.3 3.1 2.8 2.5 3.2}
    set mad [expr {median(abs(sub($x, median($x))))}]

It's possible to intermix scalar values and vectors:

    set x {1.0 2.0 3.0 4.0 5.0}
    set y [expr {sub(mul($x,2),1)}]
    # y: 1.0 3.0 5.0 7.0 9.0
    
Using the "vec" function, you can create a list with expressions.

    set x 5.0
    set y [expr {vec($x+1,$x*2,$x/4)}]
    # y: 6.0 10.0 1.25

# Usage

Put package into auto_path and load it:

    package require expr++

Changes get into effect after loading the package.

Beside replacing existing functions the package adds some new expr functions:

    nroot(x, n) - Compute the Nth root of X
    sgn(x) - Signum function
    fix(x) - Return integral part X
    trunc(x) - Truncate the fractional part of X
    asinh(x) - Inverse hyperbolic sine
    acosh(x) - Inverse hyperbolic cosine
    atanh(x) - Inverse hyperbolic tangens
    sat(x, z, a) - Saturate X in range [a, z]
    fit(x, type) - Fit (saturate) value X to range of given type
    mean(x) - Compute the mean value (average) of X
    median(x) - Compute the median value of X

Tcl does not allow to replace expr operators. Therefore new expr functions
are added that can handle vectors/lists:

    add(a, b) - Add scalars or lists/vectors A and B
    sub(a, b) - Subtract scalars or lists/vectors A and B
    mul(a, b) - Multiply scalars or lists/vectors A and B
    div(a, b) - Divide scalars or lists/vectors A and B
    mod(a, b) - Modulo supporting scalars and lists/vectors

The capability of round() function is extended by optional new arguments
to specify the rounding mode and integral saturation:

    round(x, mode, sat)

By default the behavior of round() is unchanged.
Mode can be one of the following:

    "infinity" (round ties away from zero)
    "zero" (round ties towards zero)
    "ceil" (round ties towards +inf)
    "floor" (round ties towards -inf)

The sat argument corresponds to the type argument of the fit() function which are:

    "int" (integer of arbitrary size)
    "bool" (boolean value)
    "double" (floating point value)
    "byte" (8 bit unsigned integer)
    "int8" (8 bit signed integer)
    "int16" (16 bit signed integer)
    "int32" (32 bit signed integer)
    "int64" (64 bit signed integer)
    "uint8" (8 bit unsigned integer)
    "uint16" (16 bit unsigned integer)
    "uint32" (32 bit unsigned integer)
    "uint64" (64 bit unsigned integer)

To compare elements of lists/vectors a cmp() math function has been added:

    cmp(op, a, b) - Compare scalar or list/vector A and B using comparison operator op

The op argument can be set to one of the following values:

    "eq" - Test elements for equality (A == B)
    "ne" - Test elements for inequality (A != B)
    "gt" - Test if A elements are greater than B elements (A > B)
    "ge" - Test if A elements are greater/equal than B elements (A >= B)
    "lt" - Test if A elements are lower than B elements (A < B)
    "le" - Test if A elements are lower/equal B elements (A <= B)

Bit manipulation of lists/vectors is supported by the following math functions:

    bitand(a, b) - Bitwise AND of scalars or elements in lists/vectors A and B
    bitor(a, b) - Bitwise OR of scalars or elements in lists/vectors A and B
    bitxor(a, b) - Bitwise XOR of scalars or elements in lists/vectors A and B
    bitnot(x) - Bitwise NOT of scalars or elements in list/vector X
    bitget(x, k, [n]) - Get N bits (default: 1) from X starting at zero-based index K
    bitshift(x, k) - Bit shifting of elements in X by K positions

The functions bitshift() and bitget() support K being a list/vector.

Note that expr is implicitely used within the conditionals of control structures (if, while, for, ...).
The features added by this package are therefore available in their conditionals as well.
