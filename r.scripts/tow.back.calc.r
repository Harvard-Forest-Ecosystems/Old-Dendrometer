	# 1/05 kmk, updated 7/06
	# 7/12 elg converted script to R. This script is called by run.all.r
	# B4 726 was recruited in 2004, and C1 217, D1 266 & H1 204 were recruited in 2006
	# all with DBHs too large to have grown in since the previous recruitment surveys
	# they were probably missed b/c they were near the plot border or b/c they looked dead but weren't
	# we must back-calculate their DBHs through 1993
	# & insert them into the DBH & kgC data from previous years
	# so as to be included in all annual summaries
	# note that this script can only be run after all tow.yr.calc fnctns have been run, but not before tow.sum.calc
	#----------------------------------------------
	# make a matrix in which to list the back calculated dbhs
	tow.back.calc.dbh <- matrix(NA, 4, 12)
	tow.back.calc.dbh <- as.data.frame(tow.back.calc.dbh)
	dimnames(tow.back.calc.dbh)[[2]] <- c("plot", "tag", "spp", "dbh.93", "dbh.98", "dbh.99", "dbh.00", "dbh.01", "dbh.02", "dbh.03", "dbh.04", "dbh.05")
	tow.back.calc.dbh[1, 1:3] <- c("B4", 726, "ro")
	tow.back.calc.dbh[2, 1:3] <- c("C1", 217, "hem")
	tow.back.calc.dbh[3, 1:3] <- c("D1", 266, "wp")
	tow.back.calc.dbh[4, 1:3] <- c("H1", 204, "rm")
	#----------------------------------------------
	# calc a growth rate to apply to B4 726
	# find all the trees that were present from 1993-2003
	junk <- paste(tow.03.dbh[,1],tow.03.dbh[,2]) %in% paste(tow.93.dbh[,1],tow.93.dbh[,2])
	# list them in an object w/ their dbhs from the beginning of 1993 & end of 2003
	tow.93.03.dbh <- tow.03.dbh[junk, c(1:3, ncol(tow.03.dbh))]
	junk <- paste(tow.93.dbh[,1],tow.93.dbh[,2]) %in%  paste(tow.03.dbh[,1],tow.03.dbh[,2])
	tow.93.03.dbh <- cbind(tow.93.03.dbh[, 1:3], tow.93.dbh[junk, 4], tow.93.03.dbh[, 4])
	dimnames(tow.93.03.dbh)[[2]] <- c(dimnames(tow.93.03.dbh)[[2]][1:3], dimnames(tow.93.dbh)[[2]][4], "dbh.yr.end.03")
	# list only the red oaks w/ dbhs 30-40 cm
	junk <- tow.93.03.dbh[, 3] == "ro" & tow.93.03.dbh[, 5] <= 40 & tow.93.03.dbh[, 5] >= 30
	tow.93.03.dbh <- tow.93.03.dbh[junk,  ]
	# list only the red oaks in the SW transects
	junk <- tow.93.03.dbh[, 1] == "A1" | tow.93.03.dbh[, 1] == "A2" | tow.93.03.dbh[, 1] == "A3" | tow.93.03.dbh[, 1] == "B1" | tow.93.03.dbh[, 1] == "B2" | tow.93.03.dbh[, 1] == "B3" | tow.93.03.dbh[, 1] == "B4" | tow.93.03.dbh[, 1] == "C1" | tow.93.03.dbh[, 1] == "C2" | tow.93.03.dbh[, 1] == "C3" | tow.93.03.dbh[, 1] == "C4" | tow.93.03.dbh[, 1] == "C5" | tow.93.03.dbh[, 1] == "D1" | tow.93.03.dbh[, 1] == "D2" | tow.93.03.dbh[, 1] == "D3" | tow.93.03.dbh[, 1] == "D4" | tow.93.03.dbh[, 1] == "D5"
	tow.93.03.dbh <- tow.93.03.dbh[junk,  ]
	# calc their avg growth rate (cm/yr)
	for(i in 1:length(tow.93.03.dbh)) {
		junk <- mean(tow.93.03.dbh[, 5] - tow.93.03.dbh[, 4])/11
	}
	# fill in the B4 726's back-calculated DBHs
	for(i in 1:6) {
		tow.back.calc.dbh[1, 11 - i] <- 35.1 - junk * i
		tow.back.calc.dbh[1, 4] <- 35.1 - junk * 11
	}
	#----------------------------------------------
	# calc a growth rate to apply to C1 217
	# find all the trees that were present from 1993-2005
	junk <- paste(tow.05.dbh[,1],tow.03.dbh[,2]) %in% paste(tow.93.dbh[,1],tow.93.dbh[,2])
	# list them in an object w/ their dbhs from the beginning of 1993 & end of 2005
	tow.93.05.dbh <- tow.05.dbh[junk, c(1:3, ncol(tow.05.dbh))]
	junk <- paste(tow.93.dbh[,1],tow.93.dbh[,2]) %in% paste(tow.05.dbh[,1],tow.03.dbh[,2])
	tow.93.05.dbh <- cbind(tow.93.05.dbh[, 1:3], tow.93.dbh[junk, 4], tow.93.05.dbh[, 4])
	dimnames(tow.93.05.dbh)[[2]] <- c(dimnames(tow.93.05.dbh)[[2]][1:3], dimnames(tow.93.dbh)[[2]][4], "dbh.yr.end.05")
	# list only the hemlocks w/ dbhs 25-35 cm
	junk <- tow.93.05.dbh[, 3] == "hem" & tow.93.05.dbh[, 5] <= 35 & tow.93.05.dbh[, 5] >= 25
	tow.93.05.dbh <- tow.93.05.dbh[junk,  ]
	# calc their avg growth rate (cm/yr)
	for(i in 1:length(tow.93.05.dbh)) {
		junk <- mean(tow.93.05.dbh[, 5] - tow.93.05.dbh[, 4])/13
	}
	# fill in the C1 217's back-calculated DBHs
	for(i in 1:8) {
		tow.back.calc.dbh[2, 13 - i] <- 28.6 - junk * i
		tow.back.calc.dbh[2, 4] <- 28.6 - junk * 13
	}
	#----------------------------------------------
	# calc a growth rate to apply to D1 266
	# find all the trees that were present from 1993-2005
		junk <- paste(tow.05.dbh[,1],tow.03.dbh[,2]) %in% paste(tow.93.dbh[,1],tow.93.dbh[,2])
	# list them in an object w/ their dbhs from the beginning of 1993 & end of 2005
	tow.93.05.dbh <- tow.05.dbh[junk, c(1:3, ncol(tow.05.dbh))]
		junk <- paste(tow.93.dbh[,1],tow.93.dbh[,2]) %in% paste(tow.05.dbh[,1],tow.03.dbh[,2])
	tow.93.05.dbh <- cbind(tow.93.05.dbh[, 1:3], tow.93.dbh[junk, 4], tow.93.05.dbh[, 4])
	dimnames(tow.93.05.dbh)[[2]] <- c(dimnames(tow.93.05.dbh)[[2]][1:3], dimnames(tow.93.dbh)[[2]][4], "dbh.yr.end.05")
	# list only the white pines w/ dbhs 40-50 cm
	junk <- tow.93.05.dbh[, 3] == "wp" & tow.93.05.dbh[, 5] <= 50 & tow.93.05.dbh[, 5] >= 40
	tow.93.05.dbh <- tow.93.05.dbh[junk,  ]
	# calc their avg growth rate (cm/yr)
	for(i in 1:length(tow.93.05.dbh)) {
		junk <- mean(tow.93.05.dbh[, 5] - tow.93.05.dbh[, 4])/13
	}
	# fill in the D1 266's back-calculated DBHs
	for(i in 1:8) {
		tow.back.calc.dbh[3, 13 - i] <- 43.2 - junk * i
		tow.back.calc.dbh[3, 4] <- 43.2 - junk * 13
	}
	#----------------------------------------------
	# calc a growth rate to apply to H1 204
	# find all the trees that were present from 1993-2005
	junk <- paste(tow.05.dbh[,1],tow.03.dbh[,2]) %in% paste(tow.93.dbh[,1],tow.93.dbh[,2])
	# list them in an object w/ their dbhs from the beginning of 1993 & end of 2005
	tow.93.05.dbh <- tow.05.dbh[junk, c(1:3, ncol(tow.05.dbh))]
	junk <- paste(tow.93.dbh[,1],tow.93.dbh[,2]) %in% paste(tow.05.dbh[,1],tow.03.dbh[,2])
	tow.93.05.dbh <- cbind(tow.93.05.dbh[, 1:3], tow.93.dbh[junk, 4], tow.93.05.dbh[, 4])
	dimnames(tow.93.05.dbh)[[2]] <- c(dimnames(tow.93.05.dbh)[[2]][1:3], dimnames(tow.93.dbh)[[2]][4], "dbh.yr.end.05")
	# list only the red maples w/ dbhs 12-16 cm
	junk <- tow.93.05.dbh[, 3] == "rm" & tow.93.05.dbh[, 5] <= 16 & tow.93.05.dbh[, 5] >= 12
	tow.93.05.dbh <- tow.93.05.dbh[junk,  ]
	# calc their avg growth rate (cm/yr)
	for(i in 1:length(tow.93.05.dbh)) {
		junk <- mean(tow.93.05.dbh[, 5] - tow.93.05.dbh[, 4])/13
	}
	# fill in the D1 266's back-calculated DBHs
	for(i in 1:8) {
		tow.back.calc.dbh[4, 13 - i] <- 14.2 - junk * i
		tow.back.calc.dbh[4, 4] <- 14.2 - junk * 13
	}
	#--------------------------------------------------------
	# now put the tree into the dbh & kgC datasets for 93-03
	# 1993
	rbind(tow.93.dbh,data.frame(plot=rep(NA,4),tag=rep(NA,4),spp=rep(NA,4),dbh.tape=rep(NA,4)))
	tow.93.dbh[(nrow(tow.93.dbh)-3):nrow(tow.93.dbh),  ] <- tow.back.calc.dbh[, 1:4]
	tow.93.dbh <- tow.93.dbh[order(tow.93.dbh[, 1], as.numeric(tow.93.dbh[, 2])),  ]
	tow.93.kgC <- calc.kgC("tow.93.dbh", 3, 4)
	# 1998
	tow.98.dbh <- rbind(tow.98.dbh, array(NA, dim=dim(tow.98.dbh[1:4,]),dimnames=dimnames(tow.98.dbh[1:4,])))
	tow.98.dbh[(nrow(tow.98.dbh)-3):nrow(tow.98.dbh), 1:3] <- tow.back.calc.dbh[, 1:3]
	tow.98.dbh[(nrow(tow.98.dbh)-3):nrow(tow.98.dbh), 4:ncol(tow.98.dbh)] <- tow.back.calc.dbh[, 5]
	tow.98.dbh <- tow.98.dbh[order(tow.98.dbh[, 1], as.numeric(tow.98.dbh[, 2])),  ]
	tow.98.kgC <- calc.kgC("tow.98.dbh", 3, 4:ncol(tow.98.dbh))
	# 1999
	tow.99.dbh <- rbind(tow.99.dbh, array(NA, dim=dim(tow.99.dbh[1:4,]),dimnames=dimnames(tow.99.dbh[1:4,])))
	tow.99.dbh[(nrow(tow.99.dbh)-3):nrow(tow.99.dbh), 1:3] <- tow.back.calc.dbh[, 1:3]
	tow.99.dbh[(nrow(tow.99.dbh)-3):nrow(tow.99.dbh), 4:ncol(tow.99.dbh)] <- tow.back.calc.dbh[, 6]
	tow.99.dbh <- tow.99.dbh[order(tow.99.dbh[, 1], as.numeric(tow.99.dbh[, 2])),  ]
	tow.99.kgC <- calc.kgC("tow.99.dbh", 3, 4:ncol(tow.99.dbh))
	# 2000
	tow.00.dbh <- rbind(tow.00.dbh, array(NA, dim=dim(tow.00.dbh[1:4,]),dimnames=dimnames(tow.00.dbh[1:4,])))
	tow.00.dbh[(nrow(tow.00.dbh)-3):nrow(tow.00.dbh), 1:3] <- tow.back.calc.dbh[, 1:3]
	tow.00.dbh[(nrow(tow.00.dbh)-3):nrow(tow.00.dbh), 4:ncol(tow.00.dbh)] <-tow.back.calc.dbh[, 7]
	tow.00.dbh <- tow.00.dbh[order(tow.00.dbh[, 1], as.numeric(tow.00.dbh[, 2])),  ]
	tow.00.kgC <- calc.kgC("tow.00.dbh", 3, 4:ncol(tow.00.dbh))
	# 2001
	tow.01.dbh <- rbind(tow.01.dbh, array(NA, dim=dim(tow.01.dbh[1:4,]),dimnames=dimnames(tow.01.dbh[1:4,])))
	tow.01.dbh[(nrow(tow.01.dbh)-3):nrow(tow.01.dbh), 1:3] <- tow.back.calc.dbh[, 1:3]
	tow.01.dbh[(nrow(tow.01.dbh)-3):nrow(tow.01.dbh), 4:ncol(tow.01.dbh)] <- tow.back.calc.dbh[, 8]
	tow.01.dbh <- tow.01.dbh[order(tow.01.dbh[, 1], as.numeric(tow.01.dbh[, 2])),  ]
	tow.01.kgC <- calc.kgC("tow.01.dbh", 3, 4:ncol(tow.01.dbh))
	# 2002
	tow.02.dbh <- rbind(tow.02.dbh, array(NA, dim=dim(tow.02.dbh[1:4,]),dimnames=dimnames(tow.02.dbh[1:4,])))
	tow.02.dbh[(nrow(tow.02.dbh)-3):nrow(tow.02.dbh), 1:3] <- tow.back.calc.dbh[, 1:3]
	tow.02.dbh[(nrow(tow.02.dbh)-3):nrow(tow.02.dbh), 4:ncol(tow.02.dbh)] <- tow.back.calc.dbh[, 9]
	tow.02.dbh <- tow.02.dbh[order(tow.02.dbh[, 1], as.numeric(tow.02.dbh[, 2])),  ]
	tow.02.kgC <- calc.kgC("tow.02.dbh", 3, 4:ncol(tow.02.dbh))
	# 2003
	tow.03.dbh <- rbind(tow.03.dbh, array(NA, dim=dim(tow.03.dbh[1:4,]),dimnames=dimnames(tow.03.dbh[1:4,])))
	tow.03.dbh[(nrow(tow.03.dbh)-3):nrow(tow.03.dbh), 1:3] <- tow.back.calc.dbh[, 1:3]
	tow.03.dbh[(nrow(tow.03.dbh)-3):nrow(tow.03.dbh), 4:ncol(tow.03.dbh)] <- tow.back.calc.dbh[, 10]
	tow.03.dbh <- tow.03.dbh[order(tow.03.dbh[, 1], as.numeric(tow.03.dbh[, 2])),  ]
	tow.03.kgC <- calc.kgC("tow.03.dbh", 3, 4:ncol(tow.03.dbh))
	# 2004
	tow.04.dbh <- rbind(tow.04.dbh, array(NA, dim=dim(tow.04.dbh[1:3,]),dimnames=dimnames(tow.04.dbh[1:3,])))
	tow.04.dbh[(nrow(tow.04.dbh)-2):nrow(tow.04.dbh), 1:3] <- tow.back.calc.dbh[2:4, 1:3]
	tow.04.dbh[(nrow(tow.04.dbh)-2):nrow(tow.04.dbh), 4:ncol(tow.04.dbh)] <- tow.back.calc.dbh[2:4, 11]
	tow.04.dbh <- tow.04.dbh[order(tow.04.dbh[, 1], as.numeric(tow.04.dbh[, 2])),  ]
	tow.04.kgC <- calc.kgC("tow.04.dbh", 3, 4:ncol(tow.04.dbh))
	# 2005
	tow.05.dbh <- rbind(tow.05.dbh, array(NA, dim=dim(tow.05.dbh[1:3,]),dimnames=dimnames(tow.05.dbh[1:3,])))
	tow.05.dbh[(nrow(tow.05.dbh)-2):nrow(tow.05.dbh), 1:3] <- tow.back.calc.dbh[2:4, 1:3]
	tow.05.dbh[(nrow(tow.05.dbh)-2):nrow(tow.05.dbh), 4:ncol(tow.05.dbh)] <- tow.back.calc.dbh[2:4, 12]
	tow.05.dbh <- tow.05.dbh[order(tow.05.dbh[, 1], as.numeric(tow.05.dbh[, 2])),  ]
	tow.05.kgC <- calc.kgC("tow.05.dbh", 3, 4:ncol(tow.05.dbh))
	#----------------------------------------------------------
	assign("tow.93.dbh", tow.93.dbh, pos=1)
	assign("tow.93.kgC", tow.93.kgC, pos=1)
	assign("tow.98.dbh", tow.98.dbh, pos=1)
	assign("tow.98.kgC", tow.98.kgC, pos=1)
	assign("tow.99.dbh", tow.99.dbh, pos=1)
	assign("tow.99.kgC", tow.99.kgC, pos=1)
	assign("tow.00.dbh", tow.00.dbh, pos=1)
	assign("tow.00.kgC", tow.00.kgC, pos=1)
	assign("tow.01.dbh", tow.01.dbh, pos=1)
	assign("tow.01.kgC", tow.01.kgC, pos=1)
	assign("tow.02.dbh", tow.02.dbh, pos=1)
	assign("tow.02.kgC", tow.02.kgC, pos=1)
	assign("tow.03.dbh", tow.03.dbh, pos=1)
	assign("tow.03.kgC", tow.03.kgC, pos=1)		
	assign("tow.04.dbh", tow.04.dbh, pos=1)
	assign("tow.04.kgC", tow.04.kgC, pos=1)		
	assign("tow.05.dbh", tow.05.dbh, pos=1)
	assign("tow.05.kgC", tow.05.kgC, pos=1)
