define double @average(double %x, double %y) {
entry:
  %addtmp = fadd double %x, %y
  %multmp = fmul double %addtmp, 5.000000e-01
  ret double %multmp
}
