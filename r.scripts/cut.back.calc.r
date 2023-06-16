# 5/06 kmk, updated 8/06
# in 2004 there were 3 trees and in 2006 4 trees that were recruited but had 
# DBHs that were too large to have grown in since the last recruitment survey.
# A4 247, A5 239, A5 647, & Cindy 246 are all located near the edge of the plot and were probably previoulsly thought out
# Marsha 753 looked sort of dead and was probably assumed to be so
# Bobby 245 was laying horizontal on the ground but was still alive
# Cindy 248 was part of a tree that was already being measured, but split below DBH
# Let's back-calculate their DBHs and insert them into the previous years' DBH & kgC datasets. 
# We must do so through 2001 for the trees in the outer 5m portion of the plot b/c the plots were expanded to 15m in 2001
# and through 1999 for the trees in in the original 10 m radius plot
# Note that this script can only be run after all cut.yr.calc fnctns for all years have been run, but not before cut.sum.calc
# 7/12 elg converted script to R. This script is called by run.all.r
#----------------------------------------------------
# make a matrix in which to list their back calculated DBHs
cut.back.calc.dbh <- matrix(NA, 7, 10)
cut.back.calc.dbh <- as.data.frame(cut.back.calc.dbh)
dimnames(cut.back.calc.dbh)[[2]] <- c("plot", "tag", "spp", "dbh.99", "dbh.00", "dbh.01", "dbh.02", "dbh.03", "dbh.04", "dbh.05")
cut.back.calc.dbh[1, 1:3] <- c("A4", 247, "ro")
cut.back.calc.dbh[2, 1:3] <- c("A5", 239, "wp")
cut.back.calc.dbh[3, 1:3] <- c("A5", 647, "rm")
cut.back.calc.dbh[4, 1:3] <- c("Bobby", 245, "rm")
cut.back.calc.dbh[5, 1:3] <- c("Cindy", 246, "hem")
cut.back.calc.dbh[6, 1:3] <- c("Cindy", 248, "beech")
cut.back.calc.dbh[7, 1:3] <- c("Marsha", 753, "beech")
#----------------------------------------------
# to calc growth rates for trees found in 2004,
# find all the trees that were present from 2001-2003
junk <- paste(cut.03.dbh[,1],cut.03.dbh[,2]) %in% paste(cut.01.dbh[,1],cut.01.dbh[,2])
# list them in an object w/ their DBHs from the beginning of 2001 & end of 2003
cut.01.03.dbh <- cut.03.dbh[junk, c(1:3,ncol(cut.03.dbh))]
junk<-paste(cut.01.dbh[,1],cut.01.dbh[,2]) %in% paste(cut.03.dbh[,1],cut.03.dbh[,2])
cut.01.03.dbh <- cbind(cut.01.03.dbh[, 1:3], cut.01.dbh[junk, 6], cut.01.03.dbh[, 4])
dimnames(cut.01.03.dbh)[[2]] <- c(dimnames(cut.01.03.dbh)[[2]][1:3], "dbh.yr.begin.01", "dbh.yr.end.03")
#
# back-calc A4 247 by applying an avg growth rate of red oaks in plot A4 w/ DBHs 30-40 cm
# first pull out those trees
junk <- cut.01.03.dbh[, 3] == "ro" & cut.01.03.dbh[,1]=="A4" & cut.01.03.dbh[,4]<=40 & cut.01.03.dbh[,4]>=30
cut.01.03.dbh[junk, ]
# calc their avg growth rate (cm/yr) for 01-03
	for(i in 1:length(cut.01.03.dbh[junk,])) {
		junk2 <- mean(cut.01.03.dbh[junk, 5] - cut.01.03.dbh[junk, 4])/3
	}
# fill in the back-calculated dbhs for A4 247, which had a dbh of 35.1 in 2004
for(i in 1:3) {
		cut.back.calc.dbh[1, 9 - i] <- 35.1 - junk2 * i
	}
# A5 239
junk <- cut.01.03.dbh[, 3] == "wp" & cut.01.03.dbh[,1]=="A5" & cut.01.03.dbh[,4]<=14
	for(i in 1:length(cut.01.03.dbh[junk,])) {
		junk2 <- mean(cut.01.03.dbh[junk, 5] - cut.01.03.dbh[junk, 4])/3
	}
for(i in 1:3) {
		cut.back.calc.dbh[2, 9 - i] <- 14 - junk2 * i
	}
# Marsha 753
junk <- cut.01.03.dbh[, 3] == "beech" & cut.01.03.dbh[,1]=="Marsha" & cut.01.03.dbh[,4]<=15
cut.01.03.dbh[junk, ]
for(i in 1:length(cut.01.03.dbh[junk,])) 
	{
		junk2 <- mean(cut.01.03.dbh[junk, 5] - cut.01.03.dbh[junk, 4])/3
	}
for(i in 1:3) 
	{
		cut.back.calc.dbh[7, 9 - i] <- 10.1 - junk2 * i
	}
#--------------------------------------------------------
# To calc growth rates for trees found in 2005 & that are in the outer 5 m portion of the plots, 
# find all the trees that were present from 2001-2005
junk<-splus.is.element(cut.05.dbh[, 1:2], cut.01.dbh[, 1:2])
# list them in an object w/ their DBHs from the beginning of 2001 & end of 2005
cut.01.05.dbh <- cut.05.dbh[junk, c(1:3,ncol(cut.05.dbh))]
junk <- splus.is.element(cut.01.dbh[, 1:2],cut.05.dbh[, 1:2])
cut.01.05.dbh <- cbind(cut.01.05.dbh[, 1:3], cut.01.dbh[junk, 6], cut.01.05.dbh[, 4])
dimnames(cut.01.05.dbh)[[2]] <- c(dimnames(cut.01.05.dbh)[[2]][1:3], "dbh.yr.begin.01", "dbh.yr.end.05")
# A5 647
junk <- cut.01.05.dbh[, 3] == "rm" & cut.01.05.dbh[,1]=="A5" &  cut.01.05.dbh[,5]>=10
for(i in 1:length(cut.01.05.dbh[junk,])) 
	{
		junk2 <- mean(cut.01.05.dbh[junk, 5] - cut.01.05.dbh[junk, 4])/5
	}
for(i in 1:5) 
	{
		cut.back.calc.dbh[3, 11 - i] <- 17.1 - junk2 * i
	}
# Bobby 245
junk <- cut.01.05.dbh[, 3] == "rm" & cut.01.05.dbh[,1]=="Bobby" &  cut.01.05.dbh[,4]<=20
for(i in 1:length(cut.01.05.dbh[junk,])) 
	{
		junk2 <- mean(cut.01.05.dbh[junk, 5] - cut.01.05.dbh[junk, 4])/5
	}
for(i in 1:5) 
	{
		cut.back.calc.dbh[4, 11 - i] <- 9.7 - junk2 * i
	}
# Cindy 248
junk <- cut.01.05.dbh[, 3] == "beech" & cut.01.05.dbh[,1]=="Cindy" &  cut.01.05.dbh[,4]<=15
for(i in 1:length(cut.01.05.dbh[junk,])) 
	{
		junk2 <- mean(cut.01.05.dbh[junk, 5] - cut.01.05.dbh[junk, 4])/5
	}
for(i in 1:5) 
	{
		cut.back.calc.dbh[6, 11 - i] <- 8.8 - junk2 * i
	}
#--------------------------------------------------------
# to calc growth rates for trees found in 2005 & that are in the inner 10 m portion of the plots, 
# find all the trees that were present from 1999-2005
junk <- splus.is.element(cut.05.dbh[, 1:2], cut.99.dbh[, 1:2])
# list them in an object w/ their dbhs from the beginning of 1999 & end of 2005
cut.99.05.dbh <- cut.05.dbh[junk, c(1:3,ncol(cut.05.dbh))]
junk <- splus.is.element(cut.99.dbh[, 1:2],cut.05.dbh[, 1:2])
cut.99.05.dbh <- cbind(cut.99.05.dbh[, 1:3], cut.99.dbh[junk, 5], cut.99.05.dbh[, 4])
dimnames(cut.99.05.dbh)[[2]] <- c(dimnames(cut.99.05.dbh)[[2]][1:3], "dbh.yr.begin.99", "dbh.yr.end.05")
# Cindy 246
junk <- cut.99.05.dbh[, 3] == "hem" & cut.99.05.dbh[,1]=="Cindy" &  cut.99.05.dbh[,4]<=15
for(i in 1:length(cut.99.05.dbh[junk,])) 
	{
		junk2 <- mean(cut.99.05.dbh[junk, 5] - cut.99.05.dbh[junk, 4])/7
	}
for(i in 1:7) 
	{
		cut.back.calc.dbh[5, 11 - i] <- 11.2 - junk2 * i
	}
#--------------------------------------------------------
# now put the trees into the dbh & kgC datasets for 01-03
# 1999
tmp<-as.data.frame(matrix(NA, nrow(cut.back.calc.dbh[!is.na(cut.back.calc.dbh[, "dbh.99"]),]), ncol(cut.99.dbh)))
names(tmp)<-names(cut.99.dbh)
cut.99.dbh <- rbind(cut.99.dbh,tmp)
cut.99.dbh[nrow(cut.99.dbh), 1:3] <- cut.back.calc.dbh[!is.na(cut.back.calc.dbh[, "dbh.99"]), 1:3]	
cut.99.dbh[nrow(cut.99.dbh), 4:ncol(cut.99.dbh)] <- cut.back.calc.dbh[!is.na(cut.back.calc.dbh[, "dbh.99"]), 4]
cut.99.dbh <- cut.99.dbh[order(cut.99.dbh[, 1], as.numeric(cut.99.dbh[, 2])),  ]
cut.99.kgC <- calc.kgC("cut.99.dbh", 3, 4:ncol(cut.99.dbh))
# 2000
tmp<-as.data.frame(matrix(NA, nrow(cut.back.calc.dbh[!is.na(cut.back.calc.dbh[, "dbh.00"]),]), ncol(cut.00.dbh)))
names(tmp)<-names(cut.00.dbh)
cut.00.dbh <- rbind(cut.00.dbh, tmp)
cut.00.dbh[nrow(cut.00.dbh), 1:3] <- cut.back.calc.dbh[!is.na(cut.back.calc.dbh[,"dbh.00"]), 1:3]	
cut.00.dbh[nrow(cut.00.dbh), 4:ncol(cut.00.dbh)] <- cut.back.calc.dbh[!is.na(cut.back.calc.dbh[, "dbh.00"]), 5]
cut.00.dbh <- cut.00.dbh[order(cut.00.dbh[, 1], as.numeric(cut.00.dbh[, 2])),  ]
cut.00.kgC <- calc.kgC("cut.00.dbh", 3, 4:ncol(cut.00.dbh))
# 2001
tmp<-as.data.frame( matrix(NA, nrow(cut.back.calc.dbh), ncol(cut.01.dbh)))
names(tmp)<-names(cut.01.dbh)
cut.01.dbh <- rbind(cut.01.dbh,tmp)
cut.01.dbh[(nrow(cut.01.dbh)-6):nrow(cut.01.dbh), 1:3] <- cut.back.calc.dbh[, 1:3]	
cut.01.dbh[(nrow(cut.01.dbh)-6):nrow(cut.01.dbh), 4:ncol(cut.01.dbh)] <- cut.back.calc.dbh[, 6]
cut.01.dbh <- cut.01.dbh[order(cut.01.dbh[, 1], as.numeric(cut.01.dbh[, 2])),  ]
cut.01.kgC <- calc.kgC("cut.01.dbh", 3, 4:ncol(cut.01.dbh))
# 2002
tmp<-as.data.frame(matrix(NA, nrow(cut.back.calc.dbh), ncol(cut.02.dbh)))
names(tmp)<-names(cut.02.dbh)
cut.02.dbh <- rbind(cut.02.dbh, tmp)
cut.02.dbh[(nrow(cut.02.dbh)-6):nrow(cut.02.dbh), 1:3] <- cut.back.calc.dbh[, 1:3]
cut.02.dbh[(nrow(cut.02.dbh)-6):nrow(cut.02.dbh), 4:ncol(cut.02.dbh)] <- cut.back.calc.dbh[, 7]
cut.02.dbh <- cut.02.dbh[order(cut.02.dbh[, 1], as.numeric(cut.02.dbh[, 2])),  ]
cut.02.kgC <- calc.kgC("cut.02.dbh", 3, 4:ncol(cut.02.dbh))
# 2003
tmp<-data.frame(matrix(NA, nrow(cut.back.calc.dbh), ncol(cut.03.dbh)))
names(tmp)<-names(cut.03.dbh)
cut.03.dbh <- rbind(cut.03.dbh, tmp)
cut.03.dbh[(nrow(cut.03.dbh)-6):nrow(cut.03.dbh), 1:3] <- cut.back.calc.dbh[, 1:3]
cut.03.dbh[(nrow(cut.03.dbh)-6):nrow(cut.03.dbh), 4:ncol(cut.03.dbh)] <- cut.back.calc.dbh[, 8]
cut.03.dbh <- cut.03.dbh[order(cut.03.dbh[, 1], as.numeric(cut.03.dbh[, 2])),  ]
cut.03.kgC <- calc.kgC("cut.03.dbh", 3, 4:ncol(cut.03.dbh))
# 2004
tmp<-as.data.frame(matrix(NA, nrow(cut.back.calc.dbh[!is.na(cut.back.calc.dbh[, "dbh.04"]),]), ncol(cut.04.dbh)))
names(tmp)<-names(cut.04.dbh)
cut.04.dbh <- rbind(cut.04.dbh, tmp)
cut.04.dbh[(nrow(cut.04.dbh)-3):nrow(cut.04.dbh), 1:3]<- cut.back.calc.dbh[!is.na(cut.back.calc.dbh[, "dbh.04"]), 1:3]
cut.04.dbh[(nrow(cut.04.dbh)-3):nrow(cut.04.dbh), 4:ncol(cut.04.dbh)] <- cut.back.calc.dbh[!is.na(cut.back.calc.dbh[, "dbh.04"]), 9]
cut.04.dbh <- cut.04.dbh[order(cut.04.dbh[, 1], as.numeric(cut.04.dbh[, 2])),  ]
cut.04.kgC <- calc.kgC("cut.04.dbh", 3, 4:ncol(cut.04.dbh))
# 2005
tmp<-as.data.frame(matrix(NA, nrow(cut.back.calc.dbh[!is.na(cut.back.calc.dbh[, "dbh.05"]),]), ncol(cut.05.dbh)))
names(tmp)<-names(cut.05.dbh)
cut.05.dbh <- rbind(cut.05.dbh, tmp)
cut.05.dbh[(nrow(cut.05.dbh)-3):nrow(cut.05.dbh), 1:3]<- cut.back.calc.dbh[!is.na(cut.back.calc.dbh[, "dbh.05"]), 1:3]
cut.05.dbh[(nrow(cut.05.dbh)-3):nrow(cut.05.dbh), 4:ncol(cut.05.dbh)] <- cut.back.calc.dbh[!is.na(cut.back.calc.dbh[, "dbh.05"]), 10]
cut.05.dbh <- cut.05.dbh[order(cut.05.dbh[, 1], as.numeric(cut.05.dbh[, 2])),  ]
cut.05.kgC <- calc.kgC("cut.05.dbh", 3, 4:ncol(cut.05.dbh))
#----------------------------------------------------------
assign("cut.99.dbh", cut.99.dbh, pos=1)
assign("cut.99.kgC", cut.99.kgC, pos=1)
assign("cut.00.dbh", cut.00.dbh, pos=1)
assign("cut.00.kgC", cut.00.kgC, pos=1)
assign("cut.01.dbh", cut.01.dbh, pos=1)
assign("cut.01.kgC", cut.01.kgC, pos=1)
assign("cut.02.dbh", cut.02.dbh, pos=1)
assign("cut.02.kgC", cut.02.kgC, pos=1)
assign("cut.03.dbh", cut.03.dbh, pos=1)
assign("cut.03.kgC", cut.03.kgC, pos=1)
assign("cut.04.dbh", cut.04.dbh, pos=1)
assign("cut.04.kgC", cut.04.kgC, pos=1)
assign("cut.05.dbh", cut.05.dbh, pos=1)
assign("cut.05.kgC", cut.05.kgC, pos=1)
