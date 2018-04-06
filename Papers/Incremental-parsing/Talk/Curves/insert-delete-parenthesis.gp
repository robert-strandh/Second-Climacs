# gnuplot

set terminal postscript eps color
set output "insert-delete-parenthesis.eps"
set title "Inserting and deleting a left parenthesis"
set key off inside center top
set yrange [0:]
set xlabel "Number of forms"
set ylabel "Time in ms"

plot 'insert-delete-parenthesis.res' using 1:3 with lines lc rgb 'green'

