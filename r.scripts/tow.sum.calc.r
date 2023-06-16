	# 8/06 kmk
	# 1/12 lkw - updated 
	# 7/12 elg converted script to R. This script is called by run.all.r
	# make an annual summary object
	# first, take out plots H3, H4, G3, where measurements were discontinued in 2001 due to flooding
	# and plots A4, A5, B5, which were harvested and became part of the bradys dataset in 1999
	remove <- as.data.frame(c("H3", "H4", "G3", "A4", "A5", "B5"))
	dimnames(remove)[[2]] <- "plot"
	# 1993
	junk <- is.element(tow.93.kgC[, "plot"], remove[, "plot"])
	tow.93.kgC <- tow.93.kgC[!junk,  ]
	# 1998
	junk <- is.element(tow.98.kgC[, "plot"], remove[, "plot"])
	tow.98.kgC <- tow.98.kgC[!junk,  ]
	junk <- is.element(tow.98.mort.kgC[, "plot"], remove[, "plot"])
	tow.98.mort.kgC <- tow.98.mort.kgC[!junk,  ]
	# 1999
	junk <- is.element(tow.99.kgC[, "plot"], remove[, "plot"])
	tow.99.kgC <- tow.99.kgC[!junk,  ]
	junk <- is.element(tow.99.mort.kgC[, "plot"], remove[, "plot"])
	tow.99.mort.kgC <- tow.99.mort.kgC[!junk,  ]
	junk <- is.element(tow.99.rcrt.kgC[, "plot"], remove[, "plot"])
	tow.99.rcrt.kgC <- tow.99.rcrt.kgC[!junk,  ]
	# 2000
	junk <- is.element(tow.00.kgC[, "plot"], remove[, "plot"])
	tow.00.kgC <- tow.00.kgC[!junk,  ]
	junk <- is.element(tow.00.mort.kgC[, "plot"], remove[, "plot"])
	tow.00.mort.kgC <- tow.00.mort.kgC[!junk,  ]
	# now that the kgC object for each year is comprised of trees from the same plots, 
	# the number of trees counted each year should be equal to that of the previous year, 
	# minus any trees lost to mortality and plus any trees gained through recruitment.
	#---------------------------------------------------------------------------------
	# make a matrix to fill with aboveground woody increment (AGWI), aboveground woody biomass (AGWB), 
	# biomass lost to mortality and gained through recruitment for all years, all in MgC/ha
	tow.ann.sum <- matrix(NA, nrow = 24, ncol = 4, dimnames = list(c("1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011","2012","2013","2014","2015","2016"), c("AGWI", "AGWB", "recruit", "mort")))
	# AGWB - the total maximum carbon in the aboveground live biomass
	tow.ann.sum[1, "AGWB"] <- (sum(tow.93.kgC[, "dbh.tape"]))/1000/(10^2 * pi * 34) * 10000
	tow.ann.sum[6, "AGWB"] <- (max(apply(tow.98.kgC[, 5:ncol(tow.98.kgC)], 2, sum)))/1000/(10^2 * pi * 34) * 10000
	tow.ann.sum[7, "AGWB"] <- (max(apply(tow.99.kgC[, 5:ncol(tow.99.kgC)], 2, sum)))/1000/(10^2 * pi * 34) * 10000
	tow.ann.sum[8, "AGWB"] <- (max(apply(tow.00.kgC[, 5:ncol(tow.00.kgC)], 2, sum)))/1000/(10^2 * pi * 34) * 10000
	tow.ann.sum[9, "AGWB"] <- (max(apply(tow.01.kgC[, 5:ncol(tow.01.kgC)], 2, sum)))/1000/(10^2 * pi * 34) * 10000
	tow.ann.sum[10, "AGWB"] <- (max(apply(tow.02.kgC[, 5:ncol(tow.02.kgC)], 2, sum)))/1000/(10^2 * pi * 34) * 10000
	tow.ann.sum[11, "AGWB"] <- (max(apply(tow.03.kgC[, 5:ncol(tow.03.kgC)], 2, sum)))/1000/(10^2 * pi * 34) * 10000
	tow.ann.sum[12, "AGWB"] <- (max(apply(tow.04.kgC[, 5:ncol(tow.04.kgC)], 2, sum)))/1000/(10^2 * pi * 34) * 10000
	tow.ann.sum[13, "AGWB"] <- (max(apply(tow.05.kgC[, 5:ncol(tow.05.kgC)], 2, sum)))/1000/(10^2 * pi * 34) * 10000
	tow.ann.sum[14, "AGWB"] <- (max(apply(tow.06.kgC[, 5:ncol(tow.06.kgC)], 2, sum)))/1000/(10^2 * pi * 34) * 10000
	tow.ann.sum[15, "AGWB"] <- (max(apply(tow.07.kgC[, 5:ncol(tow.07.kgC)], 2, sum)))/1000/(10^2 * pi * 34) * 10000
	tow.ann.sum[16, "AGWB"] <- (max(apply(tow.08.kgC[, 5:ncol(tow.08.kgC)], 2, sum)))/1000/(10^2 * pi * 34) * 10000
	tow.ann.sum[17, "AGWB"] <- (max(apply(tow.09.kgC[, 5:ncol(tow.09.kgC)], 2, sum)))/1000/(10^2 * pi * 34) * 10000
	tow.ann.sum[18, "AGWB"] <- (max(apply(tow.10.kgC[, 5:ncol(tow.10.kgC)], 2, sum)))/1000/(10^2 * pi * 34) * 10000
	tow.ann.sum[19, "AGWB"] <- (max(apply(tow.11.kgC[, 5:ncol(tow.11.kgC)], 2, sum)))/1000/(10^2 * pi * 34) * 10000
	tow.ann.sum[20, "AGWB"] <- (max(apply(tow.12.kgC[, 5:ncol(tow.12.kgC)], 2, sum, na.rm=T)))/1000/(10^2 * pi * 34) * 10000
	tow.ann.sum[21, "AGWB"] <- (max(apply(tow.13.kgC[, 5:ncol(tow.13.kgC)], 2, sum, na.rm=T)))/1000/(10^2 * pi * 34) * 10000
	tow.ann.sum[22, "AGWB"] <- (max(apply(tow.14.kgC[, 5:ncol(tow.14.kgC)], 2, sum, na.rm=T)))/1000/(10^2 * pi * 34) * 10000
	tow.ann.sum[23, "AGWB"] <- (max(apply(tow.15.kgC[, 5:ncol(tow.15.kgC)], 2, sum, na.rm=T)))/1000/(10^2 * pi * 34) * 10000
	tow.ann.sum[24, "AGWB"] <- (max(apply(tow.16.kgC[, 5:ncol(tow.16.kgC)], 2, sum, na.rm=T)))/1000/(10^2 * pi * 34) * 10000


  #---------------------------------------------------------------------------------
	# AGWI - the difference between this year's maximum C and the last year's last measured C
	# since no measurements were taken between '93 & '98, we'll take a 4 yr avg
	tow.ann.sum[2:5, "AGWI"] <- diff(c(tow.ann.sum[1, "AGWB"], (sum(tow.98.kgC[, "ref.dbh.tape"])/1000/(10^2 * pi * 34) * 	10000)))/4
	# AGWB for 94-97, based on the avg annual increment for that time
	for(i in 2:5)
	{
	tow.ann.sum[i, "AGWB"]<-tow.ann.sum[i-1, "AGWB"] + tow.ann.sum[i, "AGWI"]
	}
	# add 25% to 1998 AGWI to account for the time it took the trees to grow into their bands
	tow.ann.sum[6, "AGWI"] <- diff(c((sum(tow.98.kgC[, "ref.dbh.tape"])/1000/(10^2 * pi * 34) * 10000), tow.ann.sum[6, 	"AGWB"]))*1.25
	tow.ann.sum[7, "AGWI"] <- diff(c((sum(tow.99.kgC[, "ref.dbh"])/1000/(10^2 * pi * 34) * 10000), tow.ann.sum[7, "AGWB"]))
	tow.ann.sum[8, "AGWI"] <- diff(c((sum(tow.00.kgC[, "ref.dbh"])/1000/(10^2 * pi * 34) * 10000), tow.ann.sum[8, "AGWB"]))
	tow.ann.sum[9, "AGWI"] <- diff(c((sum(tow.01.kgC[, "ref.dbh.tape"])/1000/(10^2 * pi * 34) * 10000), tow.ann.sum[9, 	"AGWB"]))
	tow.ann.sum[10, "AGWI"] <- diff(c((sum(tow.02.kgC[, "ref.dbh.tape"])/1000/(10^2 * pi * 34) * 10000), tow.ann.sum[10, 	"AGWB"]))
	tow.ann.sum[11, "AGWI"] <- diff(c((sum(tow.03.kgC[, "ref.dbh.tape"])/1000/(10^2 * pi * 34) * 10000), tow.ann.sum[11, 	"AGWB"]))
	tow.ann.sum[12, "AGWI"] <- diff(c((sum(tow.04.kgC[, "ref.dbh"])/1000/(10^2 * pi * 34) * 10000), tow.ann.sum[12, 	"AGWB"]))
	tow.ann.sum[13, "AGWI"] <- diff(c((sum(tow.05.kgC[, "ref.dbh"])/1000/(10^2 * pi * 34) * 10000), tow.ann.sum[13, 	"AGWB"]))
	tow.ann.sum[14, "AGWI"] <- diff(c((sum(tow.06.kgC[, "ref.dbh.tape"])/1000/(10^2 * pi * 34) * 10000), tow.ann.sum[14, 	"AGWB"]))
	tow.ann.sum[15, "AGWI"] <- diff(c((sum(tow.07.kgC[, "ref.dbh"])/1000/(10^2 * pi * 34) * 10000), tow.ann.sum[15, 	"AGWB"]))
	tow.ann.sum[16, "AGWI"] <- diff(c((sum(tow.08.kgC[, "ref.dbh"])/1000/(10^2 * pi * 34) * 10000), tow.ann.sum[16, 	"AGWB"]))
	tow.ann.sum[17, "AGWI"] <- diff(c((sum(tow.09.kgC[, "ref.dbh"])/1000/(10^2 * pi * 34) * 10000), tow.ann.sum[17, 	"AGWB"]))
	tow.ann.sum[18, "AGWI"] <- diff(c((sum(tow.10.kgC[, "ref.dbh"])/1000/(10^2 * pi * 34) * 10000), tow.ann.sum[18, 	"AGWB"]))
	tow.ann.sum[19, "AGWI"] <- diff(c((sum(tow.11.kgC[, "ref.dbh"])/1000/(10^2 * pi * 34) * 10000), tow.ann.sum[19, 	"AGWB"]))
	tow.ann.sum[20, "AGWI"] <- diff(c((sum(tow.12.kgC[, "ref.dbh"])/1000/(10^2 * pi * 34) * 10000), tow.ann.sum[20, 	"AGWB"]))
  tow.ann.sum[21, "AGWI"] <- diff(c((sum(tow.13.kgC[, "ref.dbh"])/1000/(10^2 * pi * 34) * 10000), tow.ann.sum[21, 	"AGWB"]))
  tow.ann.sum[22, "AGWI"] <- diff(c((sum(tow.14.kgC[, "ref.dbh"])/1000/(10^2 * pi * 34) * 10000), tow.ann.sum[22, 	"AGWB"]))
  tow.ann.sum[23, "AGWI"] <- diff(c((sum(tow.15.kgC[, "ref.dbh"])/1000/(10^2 * pi * 34) * 10000), tow.ann.sum[23, 	"AGWB"]))
  tow.ann.sum[24, "AGWI"] <- diff(c((sum(tow.16.kgC[, "ref.dbh"])/1000/(10^2 * pi * 34) * 10000), tow.ann.sum[24, 	"AGWB"]))


  #---------------------------------------------------------------------------------
	# recruitment - the amount of agwb that was gained through 'birth'
	# 1994-1999 
	tow.ann.sum[2:7, "recruit"] <- ((max(apply(tow.99.rcrt.kgC[, 21:ncol(tow.99.rcrt.kgC)], 2, sum)))/1000/(10^2 * pi * 34) 	* 10000)/6
	# 2000-2003
	tow.ann.sum[8:11, "recruit"] <- ((max(apply(tow.03.rcrt.kgC[, 9:ncol(tow.03.rcrt.kgC)], 2, sum)))/1000/(10^2 * pi * 34) 	* 10000)/4
	# 2004
	tow.ann.sum[12, "recruit"] <- (max(apply(tow.04.rcrt.kgC[, 7:ncol(tow.04.rcrt.kgC)], 2, sum)))/1000/(10^2 * pi * 34) * 	10000
	# 2005-2006
	tow.ann.sum[13:14, "recruit"] <- ((max(apply(tow.06.rcrt.kgC[, 5:ncol(tow.06.rcrt.kgC)], 2, sum)))/1000/(10^2 * pi * 34) 	* 10000)/2
	# 2007-2008 
	tow.ann.sum[15:17, "recruit"] <- ((max(apply(tow.08.rcrt.kgC[, 5:ncol(tow.08.rcrt.kgC)], 2, sum)))/1000/(10^2 * pi *34) * 	10000)/2
	# 2009-2010
	tow.ann.sum[17:18, "recruit"] <- ((max(apply(tow.10.rcrt.kgC[, 5:ncol(tow.10.rcrt.kgC)], 2, sum)))/1000/(10^2 * pi *34) * 	10000)/2	
	# 2011-2012
	tow.ann.sum[19:20, "recruit"] <- ((max(apply(tow.12.rcrt.kgC[, 5:ncol(tow.12.rcrt.kgC)], 2, sum)))/1000/(10^2 * pi *34) * 	10000)/2	
	# 2013-2014
	tow.ann.sum[21:22, "recruit"] <- ((max(apply(tow.14.rcrt.kgC[, 5:ncol(tow.14.rcrt.kgC)], 2, sum),na.rm=T))/1000/(10^2 * pi *34) * 	10000)/2	
 # 2015-2016
	tow.ann.sum[23:24, "recruit"] <- ((max(apply(tow.16.rcrt.kgC[, 5:ncol(tow.16.rcrt.kgC)], 2, sum),na.rm=T))/1000/(10^2 * pi *34) * 	10000)/2	

  
  #---------------------------------------------------------------------------------
	# mortality - the amount of agwb that was lost to mortality
	# since we cannot distinguish what year trees found to be dead in 98 actually died, we will take a 5 yr avg 
	tow.ann.sum[2:6, "mort"] <- (sum(tow.98.mort.kgC[, 5])/1000/(10^2 * pi * 34) * 10000)/5
	tow.ann.sum[7, "mort"] <- sum(tow.99.mort.kgC[, 5])/1000/(10^2 * pi * 34) * 10000
	tow.ann.sum[8, "mort"] <- sum(tow.00.mort.kgC[, 5])/1000/(10^2 * pi * 34) * 10000
	tow.ann.sum[9, "mort"] <- sum(tow.01.mort.kgC[, 5])/1000/(10^2 * pi * 34) * 10000
	tow.ann.sum[10, "mort"] <- sum(tow.02.mort.kgC[, 5])/1000/(10^2 * pi * 34) * 10000
	tow.ann.sum[11, "mort"] <- sum(tow.03.mort.kgC[, 5])/1000/(10^2 * pi * 34) * 10000
	tow.ann.sum[12, "mort"] <- sum(tow.04.mort.kgC[, 5])/1000/(10^2 * pi * 34) * 10000
	tow.ann.sum[13, "mort"] <- sum(tow.05.mort.kgC[, 5])/1000/(10^2 * pi * 34) * 10000
	tow.ann.sum[14, "mort"] <- sum(tow.06.mort.kgC[, 5])/1000/(10^2 * pi * 34) * 10000
	tow.ann.sum[15, "mort"] <- sum(tow.07.mort.kgC[, 5])/1000/(10^2 * pi * 34) * 10000
	tow.ann.sum[16, "mort"] <- sum(tow.08.mort.kgC[, 5])/1000/(10^2 * pi * 34) * 10000
	tow.ann.sum[17, "mort"] <- sum(tow.09.mort.kgC[, 5])/1000/(10^2 * pi * 34) * 10000
	tow.ann.sum[18, "mort"] <- sum(tow.10.mort.kgC[, 5])/1000/(10^2 * pi * 34) * 10000
	tow.ann.sum[19, "mort"] <- sum(tow.11.mort.kgC[, 5])/1000/(10^2 * pi * 34) * 10000
	tow.ann.sum[20, "mort"] <- sum(tow.12.mort.kgC[, 5])/1000/(10^2 * pi * 34) * 10000
	tow.ann.sum[21, "mort"] <- sum(tow.13.mort.kgC[, 5])/1000/(10^2 * pi * 34) * 10000
	tow.ann.sum[22, "mort"] <- sum(tow.14.mort.kgC[, 5])/1000/(10^2 * pi * 34) * 10000
	tow.ann.sum[23, "mort"] <- sum(tow.15.mort.kgC[, 5])/1000/(10^2 * pi * 34) * 10000
	tow.ann.sum[24, "mort"] <- sum(tow.16.mort.kgC[, 4])/1000/(10^2 * pi * 34) * 10000
  #---------------------------------------------------------------------------------
	assign("tow.ann.sum", tow.ann.sum, pos=1)
