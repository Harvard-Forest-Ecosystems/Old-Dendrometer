	# 5/09 lkw
	# 1/12 lkw - updated
	# 7/12 elg converted script to R. This script is called by run.all.r
	# This script calculates aboveground new primary productivity for the EMS plots using the method of Clark et al. 2001
	# this script needs external objects to run --  run trees script: hf.tree.calc and litter scripts: lit.data.import & lit.sum.calc
	#---------------------------------------------------------------------------
	# Creates new object to fill with factors needed to compute ANPP for the EMS plots
	tow.anpp <- matrix(NA, nrow = 17, ncol = 5, dimnames = list(c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007","2008", "2009","2010","2011","2012","2013","2014","2015","2016"), 	c("AGWI", "AGWI.ingrowth", "fine.litterfall", "ANPP.MgC/ha", "ANPP.gC/m^2")))
	#---------------------------------------------------------------------------	
	# places AGWI in summary object 
	tow.anpp[c(1:17),1] <- tow.ann.sum[c(8:24), 1]
	#---------------------------------------------------------------------------
	# calculates AGWI.ingrowth
	# 1999	
	tow.99.ingr.dbh <- tow.99.rcrt.dbh
	# Puts a 10cm dbh placeholder in ingrowth object so that the the actual ingrowth into the size class (kgC actual-kgC 10cm dbh) can be calculated
	tow.99.ingr.dbh[,(ncol(tow.99.ingr.dbh)+1)] <- 10
	# make sure you're using the correct version of calc.kgC
	source("trees/s.scripts/calc.kgC.txt")
	# calculated the kgC for each recruited tree, as well as the kgC of 10cm for that tree species in the last column
	tow.99.ingr.kgC <- calc.kgC("tow.99.ingr.dbh", 3, 4:ncol(tow.99.ingr.dbh))
	#---------------------------------------------------------------------------
	#2003
	tow.03.ingr.dbh <- tow.03.rcrt.dbh
	tow.03.ingr.dbh[,(ncol(tow.03.ingr.dbh)+1)] <- 10
	tow.03.ingr.kgC <- calc.kgC("tow.03.ingr.dbh", 3, 4:ncol(tow.03.ingr.dbh))
	#2004
	tow.04.ingr.dbh <- tow.04.rcrt.dbh
	tow.04.ingr.dbh[,(ncol(tow.04.ingr.dbh)+1)] <- 10
	tow.04.ingr.kgC <- calc.kgC("tow.04.ingr.dbh", 3, 4:ncol(tow.04.ingr.dbh))	
	#2006
	tow.06.ingr.dbh <- tow.06.rcrt.dbh
	tow.06.ingr.dbh[,(ncol(tow.06.ingr.dbh)+1)] <- 10
	tow.06.ingr.kgC <- calc.kgC("tow.06.ingr.dbh", 3, 4:ncol(tow.06.ingr.dbh))
	#2008
	tow.08.ingr.dbh <- tow.08.rcrt.dbh
	tow.08.ingr.dbh[,(ncol(tow.08.ingr.dbh)+1)] <- 10
	tow.08.ingr.kgC <- calc.kgC("tow.08.ingr.dbh", 3, 4:ncol(tow.08.ingr.dbh))
	#2010 (needed for 2009)
	tow.10.ingr.dbh <- tow.10.rcrt.dbh
	tow.10.ingr.dbh[,(ncol(tow.10.ingr.dbh)+1)] <- 10
	tow.10.ingr.kgC <- calc.kgC("tow.10.ingr.dbh", 3, 4:ncol(tow.10.ingr.dbh))
	#2012
	tow.12.ingr.dbh <- tow.12.rcrt.dbh
	tow.12.ingr.dbh[,(ncol(tow.12.ingr.dbh)+1)] <- 10
	tow.12.ingr.kgC <- calc.kgC("tow.12.ingr.dbh", 3, 4:ncol(tow.12.ingr.dbh))
		#2014
	tow.14.ingr.dbh <- tow.14.rcrt.dbh
	tow.14.ingr.dbh[,(ncol(tow.14.ingr.dbh)+1)] <- 10
	tow.14.ingr.kgC <- calc.kgC("tow.14.ingr.dbh", 3, 4:ncol(tow.14.ingr.dbh))
	
		#2016
	tow.16.ingr.dbh <- tow.16.rcrt.dbh
	tow.16.ingr.dbh[,(ncol(tow.16.ingr.dbh)+1)] <- 10
	tow.16.ingr.kgC <- calc.kgC("tow.16.ingr.dbh", 3, 4:ncol(tow.16.ingr.dbh))
	#---------------------------------------------------------------------------
	# places AGWI.ingrowth in summary object - the amount of agwb that was gained through growth into the size class (>10cm DBH) - Subtracts sum of kgC at 10cm from max sum kgC for new recruits = ingrowth kgC 
	# 1994-1999 (**there is no fine litterfall data for this time period, only leaves were sorted**)
	#tow.anpp[2:7, "AGWI.ingrowth"] <- ((max(apply(tow.99.ingr.kgC[, 21:ncol(tow.99.ingr.kgC)], 2, sum)))/1000/(10^2 * pi * 34) * 10000)/6
	# 2000-2003
	tow.anpp[1:4, "AGWI.ingrowth"] <- ((((max(apply(tow.03.ingr.kgC[, 9:ncol(tow.03.ingr.kgC)], 2, sum)))-(min(apply(tow.03.ingr.kgC[, 9:ncol(tow.03.ingr.kgC)], 2, sum))))/1000/(10^2 * pi * 34) * 10000)/4)
	# 2004
	tow.anpp[5, "AGWI.ingrowth"] <- ((max(apply(tow.04.ingr.kgC[, 7:ncol(tow.04.ingr.kgC)], 2, sum)))-(min(apply(tow.04.ingr.kgC[, 7:ncol(tow.04.ingr.kgC)], 2, sum))))/1000/(10^2 * pi * 34) * 10000
	# 2005-2006
	tow.anpp[6:7, "AGWI.ingrowth"] <- (((max(apply(tow.06.ingr.kgC[, 5:ncol(tow.06.ingr.kgC)], 2, sum)))-(min(apply(tow.06.ingr.kgC[, 5:ncol(tow.06.ingr.kgC)], 2, sum))))/1000/(10^2 * pi * 34) * 10000)/2
	# 2007-2008
	tow.anpp[8:9, "AGWI.ingrowth"] <- (((max(apply(tow.08.ingr.kgC[, 5:ncol(tow.08.ingr.kgC)], 2, sum)))-(min(apply(tow.08.ingr.kgC[, 5:ncol(tow.08.ingr.kgC)], 2, sum))))/1000/(10^2 * pi *34) * 10000)/2
	# 2009
	tow.anpp[10:11, "AGWI.ingrowth"] <- (((max(apply(tow.10.ingr.kgC[, 5:ncol(tow.10.ingr.kgC)], 2, sum)))-(min(apply(tow.10.ingr.kgC[, 5:ncol(tow.10.ingr.kgC)], 2, sum))))/1000/(10^2 * pi *34) * 10000)/2
	# 2012
	tow.anpp[12:13, "AGWI.ingrowth"] <- (((max(apply(tow.12.ingr.kgC[, 5:ncol(tow.12.ingr.kgC)], 2, sum)))-(min(apply(tow.12.ingr.kgC[, 5:ncol(tow.12.ingr.kgC)], 2, sum))))/1000/(10^2 * pi *34) * 10000)/2
	# 2014
	tow.anpp[14:15, "AGWI.ingrowth"] <- (((max(apply(tow.14.ingr.kgC[, 5:ncol(tow.14.ingr.kgC)], 2, sum,na.rm=T)))-(min(apply(tow.14.ingr.kgC[, 5:ncol(tow.14.ingr.kgC)], 2, sum,na.rm=T))))/1000/(10^2 * pi *34) * 10000)/2
	# 2016
	tow.anpp[16:17, "AGWI.ingrowth"] <- (((max(apply(tow.16.ingr.kgC[, 5:ncol(tow.16.ingr.kgC)], 2, sum,na.rm=T)))-(min(apply(tow.16.ingr.kgC[, 5:ncol(tow.16.ingr.kgC)], 2, sum,na.rm=T))))/1000/(10^2 * pi *34) * 10000)/2
	
  
  #---------------------------------------------------------------------------
	# adds fine litterfall to summary object **make sure to run litterfall scripts first**
	tow.anpp[c(1:12),3] <- lit.sum[c(3:14),6]
	#---------------------------------------------------------------------------
	# calculates ANPP in mgC/ha and gC/m^2 in summary object
	#calculate mgC/ha
	for (i in 1:nrow(tow.anpp)){	
	tow.anpp[i,4] <- sum(tow.anpp[i,c(1:3)])
	}
	#calculate gC/m^2	
	for (i in 1:nrow(tow.anpp)){	
	tow.anpp[i,5] <- tow.anpp[i,4]*100
	}
	#---------------------------------------------------------------------------
	assign("tow.anpp",tow.anpp,pos=1)