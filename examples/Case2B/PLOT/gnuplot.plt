#GNUPLOT macro
unset format x
unset format y
unset format z
unset logscale x
unset logscale y
unset logscale z
unset size
unset key
unset title

set format cb "10^{%L}"
set logscale cb
set terminal png size 1000,800

set output "View_Case2B.png"
set xrange [ -120.0 : 120.0 ]
set yrange [ -120.0 : 120.0 ]
set size 1.0, 1.0
set xlabel "x (m)"
set ylabel "y (m)"
set cblabel "Ambient dose equivalent (uSv/h)"

set pm3d
set pm3d map
set ticslevel 0
set cbrange[1e-13:1e-06]
splot "../result.out" with pm3d