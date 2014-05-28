reset
# define axis
# remove border on top and right and set color to gray
set style line 11 lc rgb '#808080' lt 1
set border 3 back ls 11
set tics nomirror
# define grid
set style line 12 lc rgb '#808080' lt 0 lw 1
set grid back ls 12

set style line 11 linecolor rgb '#dd181f' linetype 1 linewidth 2  # red
set style line 13 linecolor rgb '#aaaaaa' linetype 1 linewidth 1  # red
set style line 12 linecolor rgb '#0060ad' linetype 1 linewidth 2  # blue

set style line 1 lc rgb '#800000' lt 1 lw 2
set style line 2 lc rgb '#ff0000' lt 1 lw 2
set style line 3 lc rgb '#ff4500' lt 1 lw 2
set style line 4 lc rgb '#ffa500' lt 1 lw 2
set style line 5 lc rgb '#006400' lt 1 lw 2
set style line 6 lc rgb '#0000ff' lt 1 lw 2
set style line 7 lc rgb '#9400d3' lt 1 lw 2

set border lw 2
set title "Normalized Benchmark Result of the Test Cases"
set terminal "pngcairo" size 800, 480 enhanced
set key top left
set output "all.png"
set xlabel "Test Cases"
set ylabel "Normalized Instruction (self / best)"
set xrange [:39]
set yrange [:10]
plot for [i=2:35] 'all.csv' u :((column(i))) with lines ls 13 title '', \
    for [i=6:6] 'all.csv' u :((column(i))) with lines ls 11 title 'zzy7896321', \
    for [i=5:5] 'all.csv' u :((column(i))) with lines ls 7 title 'ascii991218', \
    for [i=1:1] 'all.csv' u :((column(i))) with lines ls 12 title 'Determinant (CIBIC)'

set border lw 2
set title "Normalized Benchmark Result of the Test Cases"
set terminal "pngcairo" size 800, 480 enhanced
set key top left
set output "all2.png"
set xlabel "Test Cases"
set ylabel "Normalized Instruction (self / best)"
set yrange [:10]
plot for [i=2:35] 'all.csv' u :((column(i))) with lines ls 13 title '', \
    for [i=4:4] 'all.csv' u :((column(i))) with lines ls 5 title 'cycycy', \
    for [i=3:3] 'all.csv' u :((column(i))) with lines ls 3 title 'jiziwei', \
    for [i=1:1] 'all.csv' u :((column(i))) with lines ls 12 title 'Determinant (CIBIC)'

set border lw 2
set title "Normalized Benchmark Result of the Test Cases"
set terminal "pngcairo" size 800, 480 enhanced
set key top left
set output "all3.png"
set xlabel "Test Cases"
set ylabel "Normalized Instruction (self / best)"
set yrange [:10]
plot for [i=2:35] 'all.csv' u :((column(i))) with lines ls 13 title '', \
    for [i=7:7] 'all.csv' u :((column(i))) with lines ls 3 title 'daerduomkch', \
    for [i=2:2] 'all.csv' u :((column(i))) with lines ls 1 title 'sadkangaroo', \
    for [i=1:1] 'all.csv' u :((column(i))) with lines ls 12 title 'Determinant (CIBIC)'
