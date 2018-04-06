# gnuplot

set terminal postscript eps color
set output "insert-delete-double-quote.eps"
set title "Inserting and deleting a double quote"
set key off inside center top
set xlabel "Number of characters"
set ylabel "Time in ms"
set yrange [0:]
plot 'insert-delete-double-quote.res' using ($1*$2*$3):4 with lines lc rgb 'red'

