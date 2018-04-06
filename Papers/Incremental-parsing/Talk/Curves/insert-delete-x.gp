# gnuplot
set terminal postscript eps color
set output "insert-delete-x.eps"
set title "Inserting and deleting a constituent character"
set key off inside center # top
set xlabel "Number of lines"
set ylabel "Time in ms"
set yrange [0:]

plot "insert-delete-x.res" using ($1*$2):3 with lines lc rgb 'blue'

