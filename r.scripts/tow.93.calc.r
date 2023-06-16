# 1/06 kmk
#6/12 elg converted to R
# calc the kgC of each tree from the 1993 Old40 dbh data


tow.93.calc<-function(){
  # read in the data from eft

	tow.93.dbh <<- read.table("trees/data/dendrometer/1993/tow.93.dbh.txt", header = T, sep = "\t", row.names = NULL, as.is = T)
	# screen out the trees < 10 cm DBH (A5 120, B2 184, B4 264, D1 537, D2 579, E4 858, F2 978, G5 288)

  tow.93.dbh <<- tow.93.dbh[tow.93.dbh[,4]>=10,] 
	# calc the kg C of each tree using the subroutine, calc.kgC
	# make sure you're using the correct version of calc.kgC

	source("trees/s.scripts/calc.kgC.txt")

  tow.93.kgC <<- calc.kgC("tow.93.dbh", 3, 4:ncol(tow.93.dbh))
	#-----------------------------------

}
