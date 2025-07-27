if {![package vsatisfies [package provide Tcl] 8.5]} {return}
package ifneeded expr++ 1.0.1 [list source [file join $dir expr++.tcl]]
