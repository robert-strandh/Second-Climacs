set terminal eps
set output "insert-delete-x.eps"
set title "Inserting and deleting a constituent character"
set key off inside center top
set xlabel "forms X form size"
set ylabel "Time in ms"

plot 'insert-delete-x.res' using 1
plot 'insert-delete-x.res' using ($1*$2):3 with lines lc rgb 'blue' # dt 2


