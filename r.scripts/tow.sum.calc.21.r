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
tow.ann.sum <- matrix(NA, nrow = length(1993:2021), ncol = 4, 
                      dimnames = list(c(as.character(1993:2021)), c("AGWI.new", "AGWB", "recruit", "mort")))
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
# 2016-2017
tow.ann.sum[25, "recruit"] <- ((max(apply(tow.17.rcrt.kgC[, 5:ncol(tow.17.rcrt.kgC)], 2, sum),na.rm=T))/1000/(10^2 * pi *34) * 	10000)	
# 2017-2018
tow.ann.sum[26, "recruit"] <- ((max(apply(tow.18.rcrt.kgC[, 5:ncol(tow.18.rcrt.kgC)], 2, sum, na.rm = T),na.rm=T))/1000/(10^2 * pi *34) * 	10000)	
# 2018-2019
tow.ann.sum[27, "recruit"] <- ((max(apply(tow.19.rcrt.kgC[, 5:ncol(tow.19.rcrt.kgC)], 2, sum, na.rm = T),na.rm=T))/1000/(10^2 * pi *34) * 	10000)	
# 2019-2020
tow.ann.sum[28, "recruit"] <- ((max(apply(tow.20.rcrt.kgC[, 5:ncol(tow.20.rcrt.kgC)], 2, sum, na.rm = T),na.rm=T))/1000/(10^2 * pi *34) * 	10000)	
# 2020-2021
tow.ann.sum[29, "recruit"] <- ((max(apply(tow.21.rcrt.kgC[, 5:ncol(tow.21.rcrt.kgC)], 2, sum, na.rm = T),na.rm=T))/1000/(10^2 * pi *34) * 	10000)	

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
tow.ann.sum[24, "mort"] <- sum(tow.16.mort.kgC[, 5])/1000/(10^2 * pi * 34) * 10000
tow.ann.sum[25, "mort"] <- sum(tow.17.mort.kgC[, 5])/1000/(10^2 * pi * 34) * 10000
tow.ann.sum[26, "mort"] <- sum(tow.18.mort.kgC[, 5])/1000/(10^2 * pi * 34) * 10000
tow.ann.sum[27, "mort"] <- sum(tow.19.mort.kgC[, 5])/1000/(10^2 * pi * 34) * 10000	
tow.ann.sum[28, "mort"] <- sum(tow.20.mort.kgC[, 5])/1000/(10^2 * pi * 34) * 10000	
tow.ann.sum[29, "mort"] <- sum(tow.21.mort.kgC[, 5])/1000/(10^2 * pi * 34) * 10000	


#---------------------------------------------------------------------------------
# AGWB - the total maximum carbon in the aboveground live biomass, as maximum biomass for a given tree in that year (may be on different dates for each tree)
tow.ann.sum[1, "AGWB"] <- (sum(tow.93.kgC[, "dbh.tape"]))/1000/(10^2 * pi * 34) * 10000
tow.ann.sum[6, "AGWB"] <- (sum(apply(tow.98.kgC[, 5:ncol(tow.98.kgC)], 1, max, na.rm=T)))/1000/(10^2 * pi * 34) * 10000
tow.ann.sum[7, "AGWB"] <- (sum(apply(tow.99.kgC[, 5:ncol(tow.99.kgC)], 1, max, na.rm=T)))/1000/(10^2 * pi * 34) * 10000
tow.ann.sum[8, "AGWB"] <- (sum(apply(tow.00.kgC[, 5:ncol(tow.00.kgC)], 1, max, na.rm=T)))/1000/(10^2 * pi * 34) * 10000
tow.ann.sum[9, "AGWB"] <- (sum(apply(tow.01.kgC[, 5:ncol(tow.01.kgC)], 1, max, na.rm=T)))/1000/(10^2 * pi * 34) * 10000
tow.ann.sum[10, "AGWB"] <- (sum(apply(tow.02.kgC[, 5:ncol(tow.02.kgC)], 1, max, na.rm=T)))/1000/(10^2 * pi * 34) * 10000
tow.ann.sum[11, "AGWB"] <- (sum(apply(tow.03.kgC[, 5:ncol(tow.03.kgC)], 1, max, na.rm=T)))/1000/(10^2 * pi * 34) * 10000
tow.ann.sum[12, "AGWB"] <- (sum(apply(tow.04.kgC[, 5:ncol(tow.04.kgC)], 1, max, na.rm=T)))/1000/(10^2 * pi * 34) * 10000
tow.ann.sum[13, "AGWB"] <- (sum(apply(tow.05.kgC[, 5:ncol(tow.05.kgC)], 1, max, na.rm=T)))/1000/(10^2 * pi * 34) * 10000
tow.ann.sum[14, "AGWB"] <- (sum(apply(tow.06.kgC[, 5:ncol(tow.06.kgC)], 1, max, na.rm=T)))/1000/(10^2 * pi * 34) * 10000
tow.ann.sum[15, "AGWB"] <- (sum(apply(tow.07.kgC[, 5:ncol(tow.07.kgC)], 1, max, na.rm=T)))/1000/(10^2 * pi * 34) * 10000
tow.ann.sum[16, "AGWB"] <- (sum(apply(tow.08.kgC[, 5:ncol(tow.08.kgC)], 1, max, na.rm=T)))/1000/(10^2 * pi * 34) * 10000
tow.ann.sum[17, "AGWB"] <- (sum(apply(tow.09.kgC[, 5:ncol(tow.09.kgC)], 1, max, na.rm=T)))/1000/(10^2 * pi * 34) * 10000
tow.ann.sum[18, "AGWB"] <- (sum(apply(tow.10.kgC[, 5:ncol(tow.10.kgC)], 1, max, na.rm=T)))/1000/(10^2 * pi * 34) * 10000
tow.ann.sum[19, "AGWB"] <- (sum(apply(tow.11.kgC[, 5:ncol(tow.11.kgC)], 1, max, na.rm=T)))/1000/(10^2 * pi * 34) * 10000
tow.ann.sum[20, "AGWB"] <- (sum(apply(tow.12.kgC[, 5:ncol(tow.12.kgC)], 1, max, na.rm=T)))/1000/(10^2 * pi * 34) * 10000
tow.ann.sum[21, "AGWB"] <- (sum(apply(tow.13.kgC[, 5:ncol(tow.13.kgC)], 1, max, na.rm=T)))/1000/(10^2 * pi * 34) * 10000
tow.ann.sum[22, "AGWB"] <- (sum(apply(tow.14.kgC[, 5:ncol(tow.14.kgC)], 1, max, na.rm=T)))/1000/(10^2 * pi * 34) * 10000
tow.ann.sum[23, "AGWB"] <- (sum(apply(tow.15.kgC[, 5:ncol(tow.15.kgC)], 1, max, na.rm=T)))/1000/(10^2 * pi * 34) * 10000
tow.ann.sum[24, "AGWB"] <- (sum(apply(tow.16.kgC[, 5:ncol(tow.16.kgC)], 1, max, na.rm=T)))/1000/(10^2 * pi * 34) * 10000
tow.ann.sum[25, "AGWB"] <- (sum(apply(tow.17.kgC[, 5:ncol(tow.17.kgC)], 1, max, na.rm=T)))/1000/(10^2 * pi * 34) * 10000
tow.ann.sum[26, "AGWB"] <- (sum(apply(tow.18.kgC[, 5:ncol(tow.18.kgC)], 1, max, na.rm=T)))/1000/(10^2 * pi * 34) * 10000
tow.ann.sum[27, "AGWB"] <- (sum(apply(tow.19.kgC[, 5:ncol(tow.19.kgC)], 1, max, na.rm=T)))/1000/(10^2 * pi * 34) * 10000
tow.ann.sum[28, "AGWB"] <- (sum(apply(tow.20.kgC[, 5:ncol(tow.20.kgC)], 1, max, na.rm=T)))/1000/(10^2 * pi * 34) * 10000
tow.ann.sum[29, "AGWB"] <- (sum(apply(tow.21.kgC[, 5:ncol(tow.21.kgC)], 1, max, na.rm=T)))/1000/(10^2 * pi * 34) * 10000


#---------------------------------------------------------------------------------
# AGWI - new method the difference between this year's maximum C and the last year's maximum C 
# on a tree-by-tree basis, not sum of trees in on collection date
# only includes matching trees from the previous year, previous year's recruits have to be matched separately
# since no measurements were taken between '93 & '98, we'll take a 5 yr avg
tow.ann.sum[2:6, "AGWI.new"] <- diff(c(sum(tow.93.kgC[splus.is.element(tow.93.kgC[,1:2], tow.98.kgC[,1:2]), "dbh.tape"])/1000/(10^2 * pi * 34) * 10000, tow.ann.sum[6, "AGWB"]))/5
# AGWB for 94-98, based on the avg annual increment for that time
for(i in 2:6){
  tow.ann.sum[i, "AGWB"]<-tow.ann.sum[i-1, "AGWB"] + tow.ann.sum[i, "AGWI.new"]
}

tow.ann.sum[7, "AGWI.new"] <- (sum(apply(tow.99.kgC[splus.is.element(tow.99.kgC[,1:2], tow.98.kgC[,1:2]), 5:ncol(tow.99.kgC)], 1, max, na.rm=T)) -                                 sum(apply(tow.98.kgC[splus.is.element(tow.98.kgC[,1:2], tow.99.kgC[,1:2]), 5:ncol(tow.98.kgC)], 1, max, na.rm=T))) /1000/(10^2 * pi * 34) * 10000
tow.ann.sum[8, "AGWI.new"] <- tow.ann.sum["2000", "AGWB"] -
  ((sum(apply(tow.99.kgC[splus.is.element(tow.99.kgC[,1:2], tow.00.kgC[,1:2]), 5:ncol(tow.99.kgC)], 1, max, na.rm=T)) +
      sum(apply(tow.99.rcrt.kgC[splus.is.element(tow.99.rcrt.kgC[,1:2], tow.00.kgC[,1:2]), 5:ncol(tow.99.rcrt.kgC)], 1, max, na.rm=T)))/1000/(10^2 * pi * 34) * 10000)
tow.ann.sum[9, "AGWI.new"] <- tow.ann.sum["2001", "AGWB"] - 
  (sum(apply(tow.00.kgC[splus.is.element(tow.00.kgC[,1:2], tow.01.kgC[,1:2]), 5:ncol(tow.00.kgC)], 1, max, na.rm=T))/1000/(10^2 * pi * 34) * 10000)
tow.ann.sum[10, "AGWI.new"] <- tow.ann.sum["2002", "AGWB"] - 
  (sum(apply(tow.01.kgC[splus.is.element(tow.01.kgC[,1:2], tow.02.kgC[,1:2]), 5:ncol(tow.01.kgC)], 1, max, na.rm=T))/1000/(10^2 * pi * 34) * 10000)
tow.ann.sum[11, "AGWI.new"] <- tow.ann.sum["2003", "AGWB"] - 
  (sum(apply(tow.02.kgC[splus.is.element(tow.02.kgC[,1:2], tow.03.kgC[,1:2]), 5:ncol(tow.02.kgC)], 1, max, na.rm=T))/1000/(10^2 * pi * 34) * 10000)
tow.ann.sum[12, "AGWI.new"] <- tow.ann.sum["2004", "AGWB"] - 
  ((sum(apply(tow.03.kgC[splus.is.element(tow.03.kgC[,1:2], tow.04.kgC[,1:2]), 5:ncol(tow.03.kgC)], 1, max, na.rm=T)) +
      sum(apply(tow.03.rcrt.kgC[splus.is.element(tow.03.rcrt.kgC[,1:2], tow.04.kgC[,1:2]), 5:ncol(tow.03.rcrt.kgC)], 1, max, na.rm=T)))/1000/(10^2 * pi * 34) * 10000)
tow.ann.sum[13, "AGWI.new"] <- tow.ann.sum["2005", "AGWB"] - 
  ((sum(apply(tow.04.kgC[splus.is.element(tow.04.kgC[,1:2], tow.05.kgC[,1:2]), 5:ncol(tow.04.kgC)], 1, max, na.rm=T)) +
      sum(apply(tow.04.rcrt.kgC[splus.is.element(tow.04.rcrt.kgC[,1:2], tow.05.kgC[,1:2]), 5:ncol(tow.04.rcrt.kgC)], 1, max, na.rm=T)))/1000/(10^2 * pi * 34) * 10000)
tow.ann.sum[14, "AGWI.new"] <- tow.ann.sum["2006", "AGWB"] - 
  (sum(apply(tow.05.kgC[splus.is.element(tow.05.kgC[,1:2], tow.06.kgC[,1:2]), 5:ncol(tow.05.kgC)], 1, max, na.rm=T))/1000/(10^2 * pi * 34) * 10000)
tow.ann.sum[15, "AGWI.new"] <- tow.ann.sum["2007", "AGWB"] - 
  ((sum(apply(tow.06.kgC[splus.is.element(tow.06.kgC[,1:2], tow.07.kgC[,1:2]), 5:ncol(tow.06.kgC)], 1, max, na.rm=T)) +
      sum(apply(tow.06.rcrt.kgC[splus.is.element(tow.06.rcrt.kgC[,1:2], tow.07.kgC[,1:2]), 5:ncol(tow.06.rcrt.kgC)], 1, max, na.rm=T)))/1000/(10^2 * pi * 34) * 10000)
tow.ann.sum[16, "AGWI.new"] <- tow.ann.sum["2008", "AGWB"] - 
  (sum(apply(tow.07.kgC[splus.is.element(tow.07.kgC[,1:2], tow.08.kgC[,1:2]), 5:ncol(tow.07.kgC)], 1, max, na.rm=T))/1000/(10^2 * pi * 34) * 10000)
tow.ann.sum[17, "AGWI.new"] <- tow.ann.sum["2009", "AGWB"] - 
  ((sum(apply(tow.08.kgC[splus.is.element(tow.08.kgC[,1:2], tow.09.kgC[,1:2]), 5:ncol(tow.08.kgC)], 1, max, na.rm=T)) +
      sum(apply(tow.08.rcrt.kgC[splus.is.element(tow.08.rcrt.kgC[,1:2], tow.09.kgC[,1:2]), 5:ncol(tow.08.rcrt.kgC)], 1, max, na.rm=T)))/1000/(10^2 * pi * 34) * 10000)
tow.ann.sum[18, "AGWI.new"] <- tow.ann.sum["2010", "AGWB"] - 
  (sum(apply(tow.09.kgC[splus.is.element(tow.09.kgC[,1:2], tow.10.kgC[,1:2]), 5:ncol(tow.09.kgC)], 1, max, na.rm=T))/1000/(10^2 * pi * 34) * 10000)
tow.ann.sum[19, "AGWI.new"] <- tow.ann.sum["2011", "AGWB"] - 
  ((sum(apply(tow.10.kgC[splus.is.element(tow.10.kgC[,1:2], tow.11.kgC[,1:2]), 5:ncol(tow.10.kgC)], 1, max, na.rm=T)) +
      sum(apply(tow.10.rcrt.kgC[splus.is.element(tow.10.rcrt.kgC[,1:2], tow.11.kgC[,1:2]), 5:ncol(tow.10.rcrt.kgC)], 1, max, na.rm=T)))/1000/(10^2 * pi * 34) * 10000)
tow.ann.sum[20, "AGWI.new"] <- tow.ann.sum["2012", "AGWB"] - 
  (sum(apply(tow.11.kgC[splus.is.element(tow.11.kgC[,1:2], tow.12.kgC[,1:2]), 5:ncol(tow.11.kgC)], 1, max, na.rm=T))/1000/(10^2 * pi * 34) * 10000)
tow.ann.sum[21, "AGWI.new"] <- tow.ann.sum["2013", "AGWB"] - 
  ((sum(apply(tow.12.kgC[splus.is.element(tow.12.kgC[,1:2], tow.13.kgC[,1:2]), 5:ncol(tow.12.kgC)], 1, max, na.rm=T)) +
      sum(apply(tow.12.rcrt.kgC[splus.is.element(tow.12.rcrt.kgC[,1:2], tow.13.kgC[,1:2]), 5:ncol(tow.12.rcrt.kgC)], 1, max, na.rm=T)))/1000/(10^2 * pi * 34) * 10000)
tow.ann.sum[22, "AGWI.new"] <- tow.ann.sum["2014", "AGWB"] - 
  (sum(apply(tow.13.kgC[splus.is.element(tow.13.kgC[,1:2], tow.14.kgC[,1:2]), 5:ncol(tow.13.kgC)], 1, max, na.rm=T))/1000/(10^2 * pi * 34) * 10000)
tow.ann.sum[23, "AGWI.new"] <- tow.ann.sum["2015", "AGWB"] - 
  ((sum(apply(tow.14.kgC[splus.is.element(tow.14.kgC[,1:2], tow.15.kgC[,1:2]), 5:ncol(tow.14.kgC)], 1, max, na.rm=T)) +
      sum(apply(tow.14.rcrt.kgC[splus.is.element(tow.14.rcrt.kgC[,1:2], tow.15.kgC[,1:2]), 5:ncol(tow.14.rcrt.kgC)], 1, max, na.rm=T)))/1000/(10^2 * pi * 34) * 10000)
tow.ann.sum[24, "AGWI.new"] <- tow.ann.sum["2016", "AGWB"] - 
  (sum(apply(tow.15.kgC[splus.is.element(tow.15.kgC[,1:2], tow.16.kgC[,1:2]), 5:ncol(tow.15.kgC)], 1, max, na.rm=T))/1000/(10^2 * pi * 34) * 10000)
tow.ann.sum[25, "AGWI.new"] <- tow.ann.sum["2017", "AGWB"] - 
  ((sum(apply(tow.16.kgC[splus.is.element(tow.16.kgC[,1:2], tow.17.kgC[,1:2]), 5:ncol(tow.16.kgC)], 1, max, na.rm=T)) + 
      sum(apply(tow.16.rcrt.kgC[splus.is.element(tow.16.rcrt.kgC[,1:2], tow.17.kgC[,1:2]), 5:ncol(tow.16.rcrt.kgC)], 1, max, na.rm=T))) /1000/(10^2 * pi * 34) * 10000)
tow.ann.sum[26, "AGWI.new"] <- tow.ann.sum["2018", "AGWB"] - 
  ((sum(apply(tow.17.kgC[splus.is.element(tow.17.kgC[,1:2], tow.18.kgC[,1:2]), 5:ncol(tow.17.kgC)], 1, max, na.rm=T)) +
      sum(apply(tow.17.rcrt.kgC[splus.is.element(tow.17.rcrt.kgC[,1:2], tow.18.kgC[,1:2]), 5:ncol(tow.17.rcrt.kgC)], 1, max, na.rm=T))) /1000/(10^2 * pi * 34) * 10000)
tow.ann.sum[27, "AGWI.new"] <- tow.ann.sum["2019", "AGWB"] - 
  ((sum(apply(tow.18.kgC[splus.is.element(tow.18.kgC[,1:2], tow.19.kgC[,1:2]), 5:ncol(tow.18.kgC)], 1, max, na.rm=T)) +
      sum(apply(tow.18.rcrt.kgC[splus.is.element(tow.18.rcrt.kgC[,1:2], tow.19.kgC[,1:2]), 5:ncol(tow.18.rcrt.kgC)], 1, max, na.rm=T))) /1000/(10^2 * pi * 34) * 10000)
tow.ann.sum[28, "AGWI.new"] <- tow.ann.sum["2020", "AGWB"] - 
  ((sum(apply(tow.19.kgC[splus.is.element(tow.19.kgC[,1:2], tow.20.kgC[,1:2]), 5:ncol(tow.19.kgC)], 1, max, na.rm=T)) +
      sum(apply(tow.19.rcrt.kgC[splus.is.element(tow.19.rcrt.kgC[,1:2], tow.20.kgC[,1:2]), 5:ncol(tow.19.rcrt.kgC)], 1, max, na.rm=T))) /1000/(10^2 * pi * 34) * 10000)
tow.ann.sum[29, "AGWI.new"] <- tow.ann.sum["2021", "AGWB"] - 
  ((sum(apply(tow.20.kgC[splus.is.element(tow.20.kgC[,1:2], tow.21.kgC[,1:2]), 5:ncol(tow.20.kgC)], 1, max, na.rm=T)) +
      sum(apply(tow.20.rcrt.kgC[splus.is.element(tow.20.rcrt.kgC[,1:2], tow.21.kgC[,1:2]), 5:ncol(tow.20.rcrt.kgC)], 1, max, na.rm=T))) /1000/(10^2 * pi * 34) * 10000)


# #---------------------------------------------------------------------------------
# # AGWI - old method the difference between this year's maximum C and the last year's last measured C
# # since no measurements were taken between '93 & '98, we'll take a 4 yr avg
# tow.ann.sum[2:5, "AGWI"] <- diff(c(tow.ann.sum[1, "AGWB"], (sum(tow.98.kgC[, "ref.dbh.tape"])/1000/(10^2 * pi * 34) * 	10000)))/4
# # AGWB for 94-97, based on the avg annual increment for that time
# for(i in 2:5)
# {
#   tow.ann.sum[i, "AGWB"]<-tow.ann.sum[i-1, "AGWB"] + tow.ann.sum[i, "AGWI"]
# }
# # add 25% to 1998 AGWI to account for the time it took the trees to grow into their bands
# tow.ann.sum[6, "AGWI"] <- diff(c((sum(tow.98.kgC[, "ref.dbh.tape"])/1000/(10^2 * pi * 34) * 10000), tow.ann.sum[6, "AGWB"]))*1.25
# tow.ann.sum[7, "AGWI"] <- diff(c((sum(tow.99.kgC[, "ref.dbh"])/1000/(10^2 * pi * 34) * 10000), tow.ann.sum[7, "AGWB"]))
# tow.ann.sum[8, "AGWI"] <- diff(c((sum(tow.00.kgC[, "ref.dbh"])/1000/(10^2 * pi * 34) * 10000), tow.ann.sum[8, "AGWB"]))
# tow.ann.sum[9, "AGWI"] <- diff(c((sum(tow.01.kgC[, "ref.dbh.tape"])/1000/(10^2 * pi * 34) * 10000), tow.ann.sum[9, "AGWB"]))
# tow.ann.sum[10, "AGWI"] <- diff(c((sum(tow.02.kgC[, "ref.dbh.tape"])/1000/(10^2 * pi * 34) * 10000), tow.ann.sum[10, "AGWB"]))
# tow.ann.sum[11, "AGWI"] <- diff(c((sum(tow.03.kgC[, "ref.dbh.tape"])/1000/(10^2 * pi * 34) * 10000), tow.ann.sum[11, "AGWB"]))
# tow.ann.sum[12, "AGWI"] <- diff(c((sum(tow.04.kgC[, "ref.dbh"])/1000/(10^2 * pi * 34) * 10000), tow.ann.sum[12, "AGWB"]))
# tow.ann.sum[13, "AGWI"] <- diff(c((sum(tow.05.kgC[, "ref.dbh"])/1000/(10^2 * pi * 34) * 10000), tow.ann.sum[13, "AGWB"]))
# tow.ann.sum[14, "AGWI"] <- diff(c((sum(tow.06.kgC[, "ref.dbh.tape"])/1000/(10^2 * pi * 34) * 10000), tow.ann.sum[14, "AGWB"]))
# tow.ann.sum[15, "AGWI"] <- diff(c((sum(tow.07.kgC[, "ref.dbh"])/1000/(10^2 * pi * 34) * 10000), tow.ann.sum[15, "AGWB"]))
# tow.ann.sum[16, "AGWI"] <- diff(c((sum(tow.08.kgC[, "ref.dbh"])/1000/(10^2 * pi * 34) * 10000), tow.ann.sum[16, "AGWB"]))
# tow.ann.sum[17, "AGWI"] <- diff(c((sum(tow.09.kgC[, "ref.dbh"])/1000/(10^2 * pi * 34) * 10000), tow.ann.sum[17, "AGWB"]))
# tow.ann.sum[18, "AGWI"] <- diff(c((sum(tow.10.kgC[, "ref.dbh"])/1000/(10^2 * pi * 34) * 10000), tow.ann.sum[18, "AGWB"]))
# tow.ann.sum[19, "AGWI"] <- diff(c((sum(tow.11.kgC[, "ref.dbh"])/1000/(10^2 * pi * 34) * 10000), tow.ann.sum[19, "AGWB"]))
# tow.ann.sum[20, "AGWI"] <- diff(c((sum(tow.12.kgC[, "ref.dbh"])/1000/(10^2 * pi * 34) * 10000), tow.ann.sum[20, "AGWB"]))
# tow.ann.sum[21, "AGWI"] <- diff(c((sum(tow.13.kgC[, "ref.dbh"])/1000/(10^2 * pi * 34) * 10000), tow.ann.sum[21, "AGWB"]))
# tow.ann.sum[22, "AGWI"] <- diff(c((sum(tow.14.kgC[, "ref.dbh"])/1000/(10^2 * pi * 34) * 10000), tow.ann.sum[22, "AGWB"]))
# tow.ann.sum[23, "AGWI"] <- diff(c((sum(tow.15.kgC[, "ref.dbh"])/1000/(10^2 * pi * 34) * 10000), tow.ann.sum[23, "AGWB"]))
# tow.ann.sum[24, "AGWI"] <- diff(c((sum(tow.16.kgC[, "ref.dbh"])/1000/(10^2 * pi * 34) * 10000), tow.ann.sum[24, "AGWB"]))
# tow.ann.sum[25, "AGWI"] <- diff(c((sum(tow.17.kgC[, "ref.dbh"])/1000/(10^2 * pi * 34) * 10000), tow.ann.sum[25, "AGWB"]))


assign("tow.ann.sum", tow.ann.sum, pos=1)
write.csv(tow.ann.sum, "trees/tow.ann.sum.new.csv", quote = F)

