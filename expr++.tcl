package require Tcl 8.5


namespace eval ::tcl::mathfunc::legacy {

   # Note: Leading underscore in internal proc names is intentional to prevent
   # them from being used in expr math as functions.

   # When renaming commands in namespace ::tcl::mathfunc they're going to forward
   # to wrong or non-existing math functions in namespace ::tcl::mathfunc::legacy.
   # Therefore we track renaming math functions and keep names in sync.
   proc _sync {prev next args} {
      # If command does not exist anymore: do nothing
      set src ::tcl::mathfunc::legacy::[namespace tail $prev]
      if {[namespace which -command $src] ne ""} {
         set dest ::tcl::mathfunc::legacy::[namespace tail $next]
         if {[namespace which -command $dest] ne ""} {
            rename $dest {}
         }
         rename $src $dest
      }
   }
   
   # Moves given math function to ::tcl::mathfunc::legacy namespace
   proc _move {what} {
      set y false
      # If source function does not exist: do nothing
      set src ::tcl::mathfunc::$what
      if {[namespace which -command $src] ne ""} {
         set dest ::tcl::mathfunc::legacy::$what
         # Check if a previous invocation has already moved this math function
         if {[namespace which -command $dest] eq ""} {
            rename $src $dest
         }
         set y true
      }
      return $y
   }
   
   # Add new functions
   
   # Nth root of X
   #
   # This is computed via
   #    y = x**(1/n) = exp(ln(x**(1/n))) = exp(ln(x)/n)
   #
   # We could compute this via pow() too, but pow()
   # does not handle some special cases for negative x.
   # For example:
   #    pow(-1, 1.0/3.0) -> domain error
   # but
   #    nroot(-1, 3) == -1
   #
   # Note that cbrt(x) is equal to nroot(x, 3)
   # and sqrt(x) is equal to nroot(x, 2).
   #
   
   proc ::tcl::mathfunc::nroot {x n} {
      if {![string is double -strict $x] || ![string is double -strict $n]} {
         error "argument must be numeric"
      }
      if {($x != $x) && ($n != $n)} {
         error "argument is nan"
      }
      if {$n == 0} {
         error "argument n must be non-zero"
      }
      if {$x != 0} {
         if {$x > 0} {
            # nroot(x, n) for x > 0:
            #    y = x**(1/n) = exp(ln(x**(1/n))) = exp(ln(x)/n)
            set y [expr {exp(log($x)/double($n))}]
         } else {
            set k [expr {entier($n)}]
            # For negative x we can solve nroot(x, n) if n is an odd integer.
            if {(0 == ($k & 1)) || ($n != double($k))} {
               error "if x < 0 n must be non-zero odd integer"
            }
            set y [expr {-exp(log(abs($x))/double($n))}]
         }
      } else {
         # nroot(0, n):
         #    i.   y = 0**(1/n) = 0
         #    ii.  y = 0**(1/-n) = 1/(0**(1/n)) = +Inf
         if {$n > 0} {
            set y 0.0
         } else {
            set y Inf
         }
      }
      return $y
   }
   
   # Signum function
   #
   #        | -1    if x < 0
   #    y = |  0    if x == 0
   #        |  1    if x > 0
   #
   # Note that sgn(-0.0) gives 0
   #
   
   proc ::tcl::mathfunc::sgn {x} {
      if {![string is double -strict $x]} {
         error "argument must be numeric"
      }
      if {$x != $x} {
         error "argument is nan"
      }
      # Could use Y = (X > 0) - (X < 0) to compute sgn() function
      # but that seems slower than simple if/else checks.
      if {$x != 0} {
         if {$x > 0} {
            set y 1
         } else {
            set y -1
         }
      } else {
         set y 0
      }
      return $y
   }
   
   # Return integral part of x as a double value rounding toward zero.
   # In Tcl this is different from round() which returns an integer value.
   # Unlike round() this function can return +Inf and -Inf.
   # This function is equivalent to trunc().
   #
   
   proc ::tcl::mathfunc::fix {x} {
      if {![string is double -strict $x]} {
         error "argument must be numeric"
      }
      # Let NaN propagate into expr:
      # Math functions will throw errors...
      if {$x > 0.0} {
         set y [::tcl::mathfunc::floor $x]
      } else {
         set y [::tcl::mathfunc::ceil $x]
      }
      return $y
   }
   
   # Inverse hyperbolic sine
   #
   #    y = log(x + sqrt(x**2 + 1))
   #
   
   proc ::tcl::mathfunc::asinh {x} {
      if {![string is double -strict $x]} {
         error "argument must be numeric"
      }
      # Let NaN propagate into expr:
      # Math functions will throw errors...
      return [expr {log($x + sqrt($x**2 + 1.0))}]
   }
   
   # Inverse hyperbolic cosine
   #
   #    y = log(x + sqrt(x**2 - 1))        for x >= 1
   #
   
   proc ::tcl::mathfunc::acosh {x} {
      if {![string is double -strict $x]} {
         error "argument must be numeric"
      }
      # We can't produce complex results so x < 1 is an error 
      if {$x < 1} {
         error "acosh(x) requires x >= 1"
      }
      # Let NaN propagate into expr:
      # Math functions will throw errors...
      return [expr {log($x + sqrt($x**2 - 1.0))}]
   }
   
   # Inverse hyperbolic tangent
   #
   #    y = 0.5 * log((1 + x) / (1 - x))   for -1 <= x <= 1
   #
   # Common definition exclude -1 and 1 as a value for x,
   # but with floating point the result is representable:
   #    atanh(-1) = -Inf
   #    atanh(1)  = Inf
   #
   
   proc ::tcl::mathfunc::atanh {x} {
      if {![string is double -strict $x]} {
         error "argument must be numeric"
      }
      if {$x != $x} {
         error "argument is nan"
      }
      if {($x <= -1.0) || ($x >= 1.0)} {
         if {$x == 1.0} {
            return Inf
         } else {
            if {$x == -1.0} {
               return -Inf
            }
         }
         error "atanh(x) requires -1 <= x <= 1"
      }
      return [expr {0.5 * log((1.0 + $x) / (1.0 - $x))}]
   }
   
   
   # Saturate x within range [a, z]
   #
   #        | a   if x < a
   #    y = | x   if a <= x <= z
   #        | z   if x > z
   #
   
   proc ::tcl::mathfunc::sat {x z {a -inf}} {
      set n [llength $x]
      set nz [llength $z]
      if {$nz == 1} {
         set z [lrepeat $n $z]
      } else {
         if {$nz != $n} {
            error "dimension mismatch"
         }
      }
      set na [llength $a]
      if {$na == 1} {
         set a [lrepeat $n $a]
      } else {
         if {$na != $n} {
            error "dimension mismatch"
         }
      }
      set y {}
      for {set i 0} {$i < $n} {incr i} {
         lappend y [expr {max(min([lindex $x $i], [lindex $z $i]), [lindex $a $i])}]
      }
      return $y
   }
   
   
   # Fit value to range of given type
   #
   # Type must be a string and can be one of the following values:
   #
   #    int        integer of arbitrary size
   #    bool       boolean value
   #    double     floating point value
   #    int8      -128 ... 127
   #    int16     -32768 ... 32767
   #    int32     -2147483648 ... 2147483647
   #    int64     -9223372036854775808 ... 9223372036854775807
   #    byte       0 ... 255
   #    uint8      0 ... 255
   #    uint16     0 ... 65535
   #    uint32     0 ... 4294967295
   #    uint64     0 ... 18446744073709551615
   #

   proc ::tcl::mathfunc::fit {x type} {
      if {[llength $x] <= 0} {
         error "missing arguments"
      }
      switch -nocase -- $type {
         "int" {
            set a -inf
            set z +inf
         }
         "int8" {
            set a [expr {-(2**7)}]
            set z [expr {2**7 - 1}]
         }
         "int16" {
            set a [expr {-(2**15)}]
            set z [expr {2**15 - 1}]
         }
         "int32" {
            set a [expr {-(2**31)}]
            set z [expr {2**31 - 1}]
         }
         "int64" {
            set a [expr {-(2**63)}]
            set z [expr {2**63 - 1}]
         }
         "uint" {
            set a 0
            set b +inf
         }
         "byte" -
         "uint8" {
            set a 0
            set z [expr {2**8 - 1}]
         }
         "uint16" {
            set a 0
            set z [expr {2**16 - 1}]
         }
         "uint32" {
            set a 0
            set z [expr {2**32 - 1}]
         }
         "uint64" {
            set a 0
            set z [expr {2**64 - 1}]
         }
         "bool" {
            set y {}
            foreach p $x {
               lappend y [expr {bool($p)}]
            }
            return $y
         }
         "double" {
            set y {}
            foreach p $x {
               lappend y [expr {double($p)}]
            }
            return $y
         }
         default {
            error "unknown type"
         }
      }
      set y {}
      foreach p [::tcl::mathfunc::sat $x $z $a] {
         # Manually round ties towards zero:
         # We can't use normal round() function here!
         if {$p < 0} {
            set p [::tcl::mathfunc::floor [expr {$p + 0.5}]]
         } else {
            set p [::tcl::mathfunc::ceil [expr {$p - 0.5}]]
         }
         lappend y [::tcl::mathfunc::entier $p]
      }
      return $y
   }
   
   
   # Replace (single argument) math functions
   
   foreach n {sin asin sinh cos acos cosh tan atan tanh asinh acosh atanh exp sqrt isqrt log log10 ceil floor round abs int double bool entier wide sgn fix} {
      # Ignore non-existing math functions
      if {[_move $n]} {
         proc ::tcl::mathfunc::$n {x args} {
            if {([llength $x] > 1) && ([llength $args] > 0)} {
               error "list followed by additional arguments"
            }
            set whoami [namespace tail [dict get [info frame 0] proc]]
            set y {}
            foreach p [list {*}$x {*}$args] {
               lappend y [::tcl::mathfunc::legacy::$whoami $p]
            }
            return $y
         }
         # Trace command renaming
         trace add command ::tcl::mathfunc::$n rename ::tcl::mathfunc::legacy::_sync
      }
   }
   
   # Fix min() and max() functions
   
   foreach n {min max} {
      # Ignore non-existing math functions
      if {[_move $n]} {
         proc ::tcl::mathfunc::$n {x args} {
            # We allow a single list argument or multiple scalar arguments only
            if {([llength $x] > 1) && ([llength $args] > 0)} {
               error "list followed by additional arguments"
            }
            set whoami [namespace tail [dict get [info frame 0] proc]]
            # At this point x must be scalar or x is single argument
            foreach p $args {
               if {[llength $p] > 1} {
                  error "can't mix scalar and list arguments"
               }
            }
            return [::tcl::mathfunc::legacy::$whoami {*}$x {*}$args]
         }
         # Trace command renaming
         trace add command ::tcl::mathfunc::$n rename ::tcl::mathfunc::legacy::_sync
      }
   }
   
   # pow() function arguments:
   #
   # a) pow(scalar, scalar)
   #    y = a ** b
   # b) pow(vector, scalar) (N > 1)
   #    y = [a_1 a_2 ... a_N] ** b
   #      = [a_1**b a_2**b ... a_N**b]
   # c) pow(scalar, vector) (N > 1)
   #    y = a ** [b_1 b_2 ... b_N]
   #      = [a**b_1 a**b_2 ... a**b_N]
   # d) pow(vector, vector) (N > 1)
   #    y = [a_1 a_2 ... a_N] ** [b_1 b_2 ... b_N]
   #      = [a_1**b_1 a_2**b_2 ... a_N**b_N]

   foreach n {pow atan2 fmod hypot nroot fit} {
      # Ignore non-existing math functions
      if {[_move $n]} {
         proc ::tcl::mathfunc::$n {a b} {
            set whoami [namespace tail [dict get [info frame 0] proc]]
            set na [llength $a]
            set nb [llength $b]
            set y {}
            if {$na == $nb} {
               # Both arguments having same number of elements
               # e.g. both are scalar or both are equal size vectors
               foreach aa [list {*}$a] bb [list {*}$b] {
                  lappend y [::tcl::mathfunc::legacy::$whoami $aa $bb]
               }
            } else {
               if {$na == 1} {
                  # First argument is a scalar but second argument is a vector
                  foreach bb [list {*}$b] {
                     lappend y [::tcl::mathfunc::legacy::$whoami $a $bb]
                  }
               } else {
                  if {$nb == 1} {
                     # First argument is a vector but second argument is scalar
                     foreach aa [list {*}$a] {
                        lappend y [::tcl::mathfunc::legacy::$whoami $aa $b]
                     }
                  } else {
                     # Two vectors that differ in size
                     error "bad argument dimension"
                  }
               }
            }
            return $y
         }
         # Trace command renaming
         trace add command ::tcl::mathfunc::$n rename ::tcl::mathfunc::legacy::_sync
      }
   }
   
   if {[_move round]} {
   
      # Round X to an integer value applying an optional rounding mode and range limit.
      # Rounding mode can be one of the following strings:
      # - "infinity": Round ties away from zero towards infinity.
      #               This is the default.
      # - "zero":     Round ties towards zero.
      # - "ceil":     Round ties towards positive infinity (+inf).
      # - "floor":    Round ties towards negative infinity (-inf).
      #
      
      proc ::tcl::mathfunc::round {x {mode "infinity"} {type "int"}} {
         set y {}
         foreach p $x {
            switch -nocase -- $mode {
               "infinity" {
                  # Round ties away from zero towards infinity
                  if {$p < 0} {
                     set q [::tcl::mathfunc::ceil [expr {$p - 0.5}]]
                  } else {
                     set q [::tcl::mathfunc::floor [expr {$p + 0.5}]]
                  }
               }
               "zero" {
                  # Round ties towards zero
                  if {$p < 0} {
                     set q [::tcl::mathfunc::floor [expr {$p + 0.5}]]
                  } else {
                     set q [::tcl::mathfunc::ceil [expr {$p - 0.5}]]
                  }
               }
               "ceil" {
                  # Round ties towards positive infinity (+inf)
                  set q [::tcl::mathfunc::floor [expr {$p + 0.5}]]
               }
               "floor" {
                  # Round ties towards negative infinity (-inf)
                  set q [::tcl::mathfunc::ceil [expr {$p - 0.5}]]
               }
               default {
                  error "invalid rounding mode - $mode"
               }
            }
            lappend y $q
         }
         return [::tcl::mathfunc::fit $y $type]
      }
      # Trace command renaming
      trace add command ::tcl::mathfunc::round rename ::tcl::mathfunc::legacy::_sync
   }
   
   # Execute specified operation defined in ::tcl::mathop on its arguments.
   # This function supports scalar and lists:
   #
   # a) Reduction of vector to scalar value
   #    i.   OP(vector)
   #         y = x_1 OP x_2 OP ... OP x_N
   #    ii.  OP(scalar, ... scalar)
   #         y = x_1 OP x_2 OP ... OP x_N
   #
   # b) Any combination of scalar values and vectors of equal length
   #    i.   OP(scalar, vector)
   #         y = [a OP b_1, a OP b_2, ... a OP b_N]
   #    ii.  OP(vector, scalar)
   #         y = [a_1 OP b, a_2 OP b, ... a_N OP b]
   #    iii. OP(vector, vector)
   #         y = [a_1 OP b_1, a_2 OP b_2, ... a_N OP b_N]
   
   proc _exec_op {op args} {
      set n [llength $args]
      if {$n <= 0} {
         error "missing arguments"
      }
      if {$n > 1} {
         # Check for dimension mismatch first
         set max 1
         foreach p $args {
            set k [llength $p]
            if {$k > 1} {
               if {($max > 1) && ($max != $k)} {
                  error "dimension mismatch"
               }
               set max $k
            }
         }
         # Arguments can be any mix of scalar values and vectors.
         # First we expand scalar values to match vector length.
         set v {}
         foreach p $args {
            if {[llength $p] > 1} {
               lappend v $p
            } else {
               lappend v [lrepeat $max $p]
            }
         }
         # Next we're going to process all these vectors.
         for {set i 0} {$i < $max} {incr i} {
            set a($i) [lindex $v 0 $i]
         }
         foreach b [lrange $v 1 end] {
            for {set i 0} {$i < $max} {incr i} {
               set a($i) [::tcl::mathop::$op $a($i) [lindex $b $i]]
            }
         }
         # Build result
         set y {}
         for {set i 0} {$i < $max} {incr i} {
            lappend y $a($i)
         }
      } else {
         # If there's a single argument assume it's
         # a list and process all its elements.
         set p [lindex $args 0]
         if {[llength $p] <= 0} {
            error "missing arguments"
         }
         set y [lindex $p 0]
         foreach b [lrange $p 1 end] {
            set y [::tcl::mathop::$op $y $b]
         }
      }
      return $y
   }
   
   # New math functions being added: no need to move any original implementation
   #
   
   proc ::tcl::mathfunc::add {a args} {
      return [::tcl::mathfunc::legacy::_exec_op + $a {*}$args]
   }
   
   proc ::tcl::mathfunc::sub {a args} {
      return [::tcl::mathfunc::legacy::_exec_op - $a {*}$args]
   }
   
   proc ::tcl::mathfunc::mul {a args} {
      return [::tcl::mathfunc::legacy::_exec_op * $a {*}$args]
   }
   
   proc ::tcl::mathfunc::div {a args} {
      return [::tcl::mathfunc::legacy::_exec_op / $a {*}$args]
   }
   
   proc ::tcl::mathfunc::mod {a args} {
      return [::tcl::mathfunc::legacy::_exec_op % $a {*}$args]
   }
   
   # We can't override compare operators so we add cmp() functions
   #
   
   proc ::tcl::mathfunc::cmpeq {a args} {
      if {[llength $args] <= 0} {
         set n [llength $a]
         if {$n <= 1} {
            error "need two arguments or list"
         }
         set y 1
         set k [lindex $a 0]
         for {set i 1} {$i < $n} {incr i} {
            set p [lindex $a $i]
            if {!($k == $p)} {
               set y 0
               break
            }
            set k $p
         }
      } else {
         set y [::tcl::mathfunc::legacy::_exec_op == $a {*}$args]
      }
      return $y
   }
   
   proc ::tcl::mathfunc::cmpne {a args} {
      if {[llength $args] <= 0} {
         set n [llength $a]
         if {$n <= 1} {
            error "need two arguments or list"
         }
         set y 1
         set k [lindex $a 0]
         for {set i 1} {$i < $n} {incr i} {
            set p [lindex $a $i]
            if {!($k != $p)} {
               set y 0
               break
            }
            set k $p
         }
      } else {
         set y [::tcl::mathfunc::legacy::_exec_op != $a {*}$args]
      }
      return $y
   }
   
   proc ::tcl::mathfunc::cmpgt {a args} {
      if {[llength $args] <= 0} {
         set n [llength $a]
         if {$n <= 1} {
            error "need two arguments or list"
         }
         set y 1
         set k [lindex $a 0]
         for {set i 1} {$i < $n} {incr i} {
            set p [lindex $a $i]
            if {!($k > $p)} {
               set y 0
               break
            }
            set k $p
         }
      } else {
         set y [::tcl::mathfunc::legacy::_exec_op > $a {*}$args]
      }
      return $y
   }
   
   proc ::tcl::mathfunc::cmpge {a args} {
      if {[llength $args] <= 0} {
         set n [llength $a]
         if {$n <= 1} {
            error "need two arguments or list"
         }
         set y 1
         set k [lindex $a 0]
         for {set i 1} {$i < $n} {incr i} {
            set p [lindex $a $i]
            if {!($k >= $p)} {
               set y 0
               break
            }
            set k $p
         }
      } else {
         set y [::tcl::mathfunc::legacy::_exec_op >= $a {*}$args]
      }
      return $y
   }
   
   proc ::tcl::mathfunc::cmplt {a args} {
      if {[llength $args] <= 0} {
         set n [llength $a]
         if {$n <= 1} {
            error "need two arguments or list"
         }
         set y 1
         set k [lindex $a 0]
         for {set i 1} {$i < $n} {incr i} {
            set p [lindex $a $i]
            if {!($k < $p)} {
               set y 0
               break
            }
            set k $p
         }
      } else {
         set y [::tcl::mathfunc::legacy::_exec_op < $a {*}$args]
      }
      return $y
   }
   
   proc ::tcl::mathfunc::cmple {a args} {
      if {[llength $args] <= 0} {
         set n [llength $a]
         if {$n <= 1} {
            error "need two arguments or list"
         }
         set y 1
         set k [lindex $a 0]
         for {set i 1} {$i < $n} {incr i} {
            set p [lindex $a $i]
            if {!($k <= $p)} {
               set y 0
               break
            }
            set k $p
         }
      } else {
         set y [::tcl::mathfunc::legacy::_exec_op <= $a {*}$args]
      }
      return $y
   }
   
   proc ::tcl::mathfunc::cmp {a b} {
      set na [llength $a]
      set nb [llength $b]
      if {$na == 1} {
         # A is a scalar
         if {$nb != 1} {
            set a [lrepeat $nb $a]
         }
      } else {
         # A is a list and B can be scalar or list
         if {$nb != 1} {
            if {$nb != $na} {
               error "dimension mismatch"
            }
         } else {
            set b [lrepeat $na $b]
         }
      }
      set y {}
      foreach p $a q $b {
         if {$p < $q} {
            lappend y -1
         } else {
            if {$p > $q} {
               lappend y 1
            } else {
               lappend y 0
            }
         }
      }
      return $y
   }
   
   # Bit manipulation on lists/vectors
   
   proc ::tcl::mathfunc::bitand {a args} {
      return [::tcl::mathfunc::legacy::_exec_op & $a {*}$args]
   }
   
   proc ::tcl::mathfunc::bitor {a args} {
      return [::tcl::mathfunc::legacy::_exec_op | $a {*}$args]
   }
   
   proc ::tcl::mathfunc::bitxor {a args} {
      return [::tcl::mathfunc::legacy::_exec_op ^ $a {*}$args]
   }
   
   proc ::tcl::mathfunc::bitnot {x args} {
      set n [llength $args]
      if {$n == 0} {
         # Single scalar or list
         set y {}
         foreach p $x {
            lappend y [expr {~$p}]
         }
      } else {
         # List of scalars
         if {[llength $x] > 1} {
            error "bad argument dimension"
         }
         set y [expr {~$x}]
         foreach p $args {
            lappend y [expr {~$p}]
         }
      }
      return $y
   }
   
   # Bit shifting
   #
   # A positive shift count performs a shift-left.
   # A negative shift count performs a shift-right.
   #
   #    BITSHIFT(scalar, scalar)
   #       y = x shift b
   #    BITSHIFT(scalar, vector)
   #       y = [x shift b_0, x shift b_1, ...]
   #    BITSHIFT(vector, scalar)
   #       y = [x_0 shift b, x_1 shift b, x_2 shift b, ...]
   #    BITSHIFT(vector, vector)
   #       y = [x_0 shift b_0, x_1 shift b_1, ...]
   #
   
   proc ::tcl::mathfunc::bitshift {x b} {
      set x_len [llength $x]
      set b_len [llength $b]
      if {$x_len != $b_len} {
         if {$x_len == 1} {
            set x [lrepeat $b_len $x]
         } else {
            if {$b_len == 1} {
               set b [lrepeat $x_len $b]
            } else {
               error "dimension mismatch"
            }
         }
      }
      set y {}
      foreach px $x pb $b {
         # Positive b is shift-left, negative b is shift-right
         if {$pb >= 0} {
            lappend y [expr {$px << $pb}]
         } else {
            lappend y [expr {$px >> -$pb}]
         }
      }
      return $y
   }
   
   # Get N bits in X starting at position I
   #
   # Both arguments I and N can be either a scalar value or a list/vector of same length as X.
   # Note that I is a zero-based index while GNU Octave uses one-based indices.
   
   proc ::tcl::mathfunc::bitget {x i {n 1}} {
      set x_len [llength $x]
      set i_len [llength $i]
      if {$x_len != $i_len} {
         if {$x_len == 1} {
            set x [lrepeat $i_len $x]
         } else {
            if {$i_len == 1} {
               set i [lrepeat $x_len $i]
            } else {
               error "dimension mismatch"
            }
         }
      }
      # At this point X and I have equal dimension
      set y {}
      set n_len [llength $n]
      if {$n_len == 1} {
         # N is scalar value
         if {![string is entier -strict $n] || ($n < 0)} {
            error "argument must be numeric bit length - $n"
         }
         set m [expr {(1 << $n) - 1}]
         foreach px $x pi $i {
            if {![string is entier -strict $pi] || ($pi < 0)} {
               error "negative bit index - $pi"
            }
            lappend y [expr {($px >> $pi) & $m}]
         }
      } else {
         # N is a list/vector
         if {$n_len != $x_len} {
            error "dimension mismatch"
         }
         foreach px $x pi $i pn $n {
            if {![string is entier -strict $pi] || ($pi < 0)} {
               error "negative bit index - $pi"
            }
            if {![string is entier -strict $pn] || ($pn < 0)} {
               error "argument must be numeric bit length - $pn"
            }
            lappend y [expr {($px >> $pi) & ((1 << $pn) - 1)}]
         }
      }
      return $y
   }
   
   # Additional math functions
   
   # Compute the arithmetic mean of specified arguments:
   # i.    MEAN(vector)
   # ii.   MEAN(scalar, ... scalar)
   #
   # For N being the number of elements in x:
   #
   #       y = (x_1 + x_2 + ... + x_N) / double(N)
   #
   
   proc ::tcl::mathfunc::mean {x args} {
      set n [llength $x]
      set m [llength $args]
      if {$n > 1} {
         # First argument is a vector:
         # We're going to process its elements.
         if {$m > 0} {
            error "list followed by additional arguments"
         }
         set y 0
         foreach p $x {
            set y [expr {$y + $p}]
         }
      } else {
         # At this point X must be scalar:
         # Check for mixing non-scalar arguments.
         foreach p $args {
            if {[llength $p] > 1} {
               error "can't mix scalar and list arguments"
            }
         }
         set y $x
         foreach p $args {
            set y [expr {$y + $p}]
         }
         incr n $m
      }
      return [expr {$y / double($n)}]
   }
   
   # Compute the median value of specified arguments:
   # i.   MEDIAN(vector)
   # ii.  MEDIAN(scalar, ... scalar)
   #
   # For N being the number of elements in x:
   #
   #          | x[int(N/2)]                              if N odd
   #      y = | 
   #          | 0.5 * ( x[int(N/2)] + x[int(N/2)-1] )    if N even
   #
   
   proc ::tcl::mathfunc::median {x args} {
      set n [llength $x]
      set m [llength $args]
      if {$n > 1} {
         # First argument is a vector:
         # We're going to process its elements.
         if {$m > 0} {
            error "list followed by additional arguments"
         }
         set p [lsort -real $x]
      } else {
         # At this point X must be scalar:
         # Check for mixing non-scalar arguments.
         foreach p $args {
            if {[llength $p] > 1} {
               error "can't mix scalar and list arguments"
            }
         }
         set p [lsort -real [list $x {*}$args]]
         incr n $m
      }
      set k [expr {$n >> 1}]
      if {0 != ($n & 1)} {
         # Odd number N of elements:
         # Median is the center element p[N/2]
         set y [lindex $p $k]
      } else {
         set a [lindex $p [expr {$k - 1}]]
         set b [lindex $p $k]
         set y [expr {($a + $b) * 0.5}]
      }
      return $y
   }
   
   # Truncates the fractional part (round towards zero) the specified arguments.
   #
   #         | floor(x)    if x > 0
   #     y = |
   #         | ceil(x)     if x <= 0
   #
   
   proc ::tcl::mathfunc::trunc {x args} {
      set n [llength $x]
      if {$n > 1} {
         # First argument is a vector:
         # We're going to process its elements.
         if {[llength $args] > 0} {
            error "list followed by additional arguments"
         }
         set p $x
      } else {
         # At this point X must be scalar:
         # Check for mixing non-scalar arguments.
         foreach p $args {
            if {[llength $p] > 1} {
               error "can't mix scalar and list arguments"
            }
         }
         set p [list $x {*}$args]
      }
      # Let NaN propagate into expr:
      # Math functions will throw errors...
      set y {}
      foreach a $p {
         lappend y [expr {($a > 0.0) ? floor($a) : ceil($a)}]
      }
      return $y
   }

   # Return a list from expr command
   #
   # Note this requires to fully qualify ::list in all commands in ::tcl::mathfunc namespace
   #
   
   proc ::tcl::mathfunc::list {args} {
      return $args
   }
   
   # A mix between Python range() and MATLAB/GNU Octave range supporting the syntax
   #
   #    range(N)            - 0, 1, 2, ... N-1
   #    range(A, Z)         - A, A+1, ... Z-1
   #    range(A, Z, STEP)   - A+i*STEP < Z
   #
   
   proc ::tcl::mathfunc::range {x args} {
      set y {}
      switch -- [llength $args] {
         0 {
            # range(N): 0 ... N-1
            if {![string is entier -strict $x]} {
               error "range boundary must be integer"
            }
            for {set i 0} {$i < $x} {incr i} {
               lappend y $i
            }
         }
         1 {
            # range(a, z)
            set z [lindex $args 0]
            if {![string is entier -strict $x] || ![string is entier -strict $z]} {
               error "range boundary must be integer"
            }
            if {$z > $x} {
               # positive direction
               for {set i $x} {$i < $z} {incr i} {
                  lappend y $i
               }
            } else {
               # negative direction
               for {set i $x} {$i > $z} {incr i -1} {
                  lappend y $i
               }
            }
         }
         2 {
            # range(a, z, inc)
            set z [lindex $args 0]
            if {![string is double -strict $z] || ![string is double -strict $z]} {
               error "range boundary must be numeric"
            }
            if {($z != $z) || (abs($z) == Inf) || ($x != $x) || (abs($x) == Inf)} {
               error "range boundary is not finite"
            }
            set inc [lindex $args 1]
            if {$inc == 0} {
               error "increment value must be non-zero"
            }
            if {$inc > 0} {
               # positive direction
               if {$z < $x} {
                  error "bad range: stop value must be greater than start value"
               }
               set p $x
               for {set i 0} {$p < $z} {incr i} {
                  lappend y $p
                  set p [expr {$x + ($i * $inc)}]
               }
            } else {
               # negative direction
               if {$z > $x} {
                  error "bad range: stop value must be lower than start value"
               }
               set p $x
               for {set i 0} {$p > $z} {incr i} {
                  lappend y $p
                  set p [expr {$x + ($i * $inc)}]
               }
            }
         }
         default {
            error "bad argument count"
         }
      }
      return $y
   }
   
   # Combine elements of lists A (keys) and B (values) returning a single list containing key/value pairs.
   # The result can be used as input to "array set" command.
   #
   
   proc ::tcl::mathfunc::pair {a b} {
      set na [llength $a]
      set nb [llength $b]
      if {($na <= 0) || ($nb <= 0)} {
         error "dimension mismatch"
      }
      set y {}
      for {set i 0} {$i < max($na,$nb)} {incr i} {
         lappend y [lindex $a [expr {$i % $na}]] [lindex $b [expr {$i % $nb}]]
      }
      return $y
   }
   
   # Create a list by duplicating n-times the value X.
   # This is a substitute for GNU Octave ones() and zeros() functions.
   #
   #    dup(N, 0) - zeros(1, N) [see GNU Octave]
   #    dup(N, 1) - ones(1, N) [see GNU Octave]
   #    dup(1, x1, x2, ...) - return arguments as a list (x1, x2 ... ) [alternative list() function]
   #    dup({n1 n2 ...}, x1, x2, ...) - return list (n1 x x1, n2 x x2, ... )
   #
   
   proc ::tcl::mathfunc::dup {n x args} {
      set nn [llength $n]
      if {$nn == 0} {
         error "bad size: must be an integer greater or equal zero"
      }
      foreach c $n {
         if {![string is integer -strict $c] || ($c < 0)} {
            error "bad size: must be an integer greater or equal zero"
         }
      }
      # if X is a list no additional arguments are allowed
      if {[llength $args] > 0} {
         if {[llength $x] > 1} {
            error "dimension mismatch"
         }
      }
      lappend p {*}$x {*}$args
      set pn [llength $p]
      set y {}
      if {$pn > 0} {
         for {set i 0} {$i < max($nn, $pn)} {incr i} {
            lappend y {*}[lrepeat [lindex $n [expr {$i % $nn}]] [lindex $p [expr {$i % $pn}]]]
         }
      }
      return $y
   }
   
   # Cleanup
   rename _move {}
   # End namespace eval
}


package provide expr++ 1.0.1
