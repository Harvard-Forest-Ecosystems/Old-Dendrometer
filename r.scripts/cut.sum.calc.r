# 7/12 elg converted script to R. This script is called by run.all.r

# make an annual summary object
# first, take out the plot, Jan, where measurements were discontinued in 2002 b/c the plot wasn't cut
remove <- as.data.frame("Jan")
dimnames(remove)[[2]] <- "plot"
# 1999
junk <- is.element(cut.99.kgC[, "plot"], remove[, "plot"])
cut.99.kgC <- cut.99.kgC[!junk,  ]
# 2000
junk <- is.element(cut.00.kgC[, "plot"], remove[, "plot"])
cut.00.kgC <- cut.00.kgC[!junk,  ]
junk <- is.element(cut.00.mort.kgC[, "plot"], remove[, "plot"])
cut.00.mort.kgC <- cut.00.mort.kgC[!junk,  ]
# 2001
junk <- is.element(cut.01.kgC[, "plot"], remove[, "plot"])
cut.01.kgC <- cut.01.kgC[!junk,  ]
junk <- is.element(cut.01.mort.kgC[, "plot"], remove[, "plot"])
cut.01.mort.kgC <- cut.01.mort.kgC[!junk,  ]
# now that the kgC object for each year is comprised of trees from the same plots, 
# the number of trees counted each year should be equal to that of the previous year, 
# minus any trees lost to mortality and plus any trees gained through recruitment.
#---------------------------------------------------------------------------------
# make a matrix to fill with aboveground woody increment (AGWI), aboveground woody biomass (AGWB), 
# biomass lost to mortality and gained through recruitment for all years, all in MgC/ha
cut.ann.sum <- matrix(NA, nrow = 9, ncol = 4, dimnames = list(c("1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007"), c("AGWI", "AGWB", "recruit", "mort")))
# AGWB - the maximum total carbon in the aboveground live biomass
cut.ann.sum[1, "AGWB"] <- (max(apply(cut.99.kgC[, 5:ncol(cut.99.kgC)], 2, sum)))/1000/(10^2 * pi * 8) * 10000
cut.ann.sum[2, "AGWB"] <- (max(apply(cut.00.kgC[, 5:ncol(cut.00.kgC)], 2, sum)))/1000/(10^2 * pi * 8) * 10000
cut.ann.sum[3, "AGWB"] <- (max(apply(cut.01.kgC[, 5:ncol(cut.01.kgC)], 2, sum)))/1000/(15^2 * pi * 8) * 10000
cut.ann.sum[4, "AGWB"] <- (max(apply(cut.02.kgC[, 5:ncol(cut.02.kgC)], 2, sum)))/1000/(15^2 * pi * 8) * 10000
cut.ann.sum[5, "AGWB"] <- (max(apply(cut.03.kgC[, 5:ncol(cut.03.kgC)], 2, sum)))/1000/(15^2 * pi * 8) * 10000
cut.ann.sum[6, "AGWB"] <- (max(apply(cut.04.kgC[, 5:ncol(cut.04.kgC)], 2, sum)))/1000/(15^2 * pi * 8) * 10000
cut.ann.sum[7, "AGWB"] <- (max(apply(cut.05.kgC[, 5:ncol(cut.05.kgC)], 2, sum)))/1000/(15^2 * pi * 8) * 10000
cut.ann.sum[8, "AGWB"] <- (max(apply(cut.06.kgC[, 5:ncol(cut.06.kgC)], 2, sum)))/1000/(15^2 * pi * 8) * 10000
cut.ann.sum[9, "AGWB"] <- (max(apply(cut.07.kgC[, 5:ncol(cut.07.kgC)], 2, sum)))/1000/(15^2 * pi * 8) * 10000
#--------------------------------------------------------------
# AGWI - the difference between this year's maximum C and the last year's last measured C
# since no measurements were taken between 93 & 98, we will take a 5 yr avg
cut.ann.sum[1, "AGWI"] <- diff(c((sum(cut.99.kgC[, "ref.dbh.tape"])/1000/(10^2 * pi * 8) * 10000), cut.ann.sum[1, "AGWB"]))
cut.ann.sum[2, "AGWI"] <- diff(c((sum(cut.00.kgC[, "ref.dbh"])/1000/(10^2 * pi * 8) * 10000), cut.ann.sum[2, "AGWB"]))
cut.ann.sum[3, "AGWI"] <- diff(c((sum(cut.01.kgC[, "ref.dbh"])/1000/(15^2 * pi * 8) * 10000), cut.ann.sum[3, "AGWB"]))
cut.ann.sum[4, "AGWI"] <- diff(c((sum(cut.02.kgC[, "ref.dbh.tape"])/1000/(15^2 * pi * 8) * 10000), cut.ann.sum[4, "AGWB"]))
cut.ann.sum[5, "AGWI"] <- diff(c((sum(cut.03.kgC[, "ref.dbh.tape"])/1000/(15^2 * pi * 8) * 10000), cut.ann.sum[5, "AGWB"]))
cut.ann.sum[6, "AGWI"] <- diff(c((sum(cut.04.kgC[, "ref.dbh"])/1000/(15^2 * pi * 8) * 10000), cut.ann.sum[6, "AGWB"]))
cut.ann.sum[7, "AGWI"] <- diff(c((sum(cut.05.kgC[, "ref.dbh"])/1000/(15^2 * pi * 8) * 10000), cut.ann.sum[7, "AGWB"]))
cut.ann.sum[8, "AGWI"] <- diff(c((sum(cut.06.kgC[, "ref.dbh.tape"])/1000/(15^2 * pi * 8) * 10000), cut.ann.sum[8, "AGWB"]))
cut.ann.sum[9, "AGWI"] <- diff(c((sum(cut.07.kgC[, "ref.dbh"])/1000/(15^2 * pi * 8) * 10000), cut.ann.sum[9, "AGWB"]))
#---------------------------------------------------------------------
# recruitment - the amount of AGWB that was gained through 'birth'
# cut.03.rcrt.kgC.in represents the recruitment in the inner 10m radius plot since 1999
cut.ann.sum[2:5, "recruit"] <- ((max(apply(cut.03.rcrt.kgC.in[, 5:ncol(cut.03.rcrt.kgC.in)], 2, sum)))/1000/(10^2 * pi * 8) * 10000)/4
# cut.03.rcrt.kgC.out represents the recruitment in the outer 5 m ring since the 2001 plot expansion
cut.ann.sum[4:5, "recruit"] <- cut.ann.sum[4:5, 3] + ((max(apply(cut.03.rcrt.kgC.out[, 5:ncol(cut.03.rcrt.kgC.out)], 2, sum)))/1000/((15^2-10^2) * pi * 8) * 10000)/2
cut.ann.sum[6, "recruit"] <- (max(apply(cut.04.rcrt.kgC[, 5:ncol(cut.04.rcrt.kgC)], 2, sum)))/1000/(15^2 * pi * 8) * 10000
cut.ann.sum[7:8, "recruit"] <- ((max(apply(cut.06.rcrt.kgC[, 5:ncol(cut.06.rcrt.kgC)], 2, sum)))/1000/(15^2 * pi * 8) * 10000)/2
#---------------------------------------------------------------------
# mortality - the amount of agwb that was lost to mortality
cut.ann.sum[2, "mort"] <- sum(cut.00.mort.kgC[, 5])/1000/(10^2 * pi * 8) * 10000
cut.ann.sum[3, "mort"] <- sum(cut.01.mort.kgC[, 5])/1000/(10^2 * pi * 8) * 10000
cut.ann.sum[4, "mort"] <- sum(cut.02.mort.kgC[, 5])/1000/(15^2 * pi * 8) * 10000
cut.ann.sum[5, "mort"] <- sum(cut.03.mort.kgC[, 5])/1000/(15^2 * pi * 8) * 10000
cut.ann.sum[6, "mort"] <- sum(cut.04.mort.kgC[, 5])/1000/(15^2 * pi * 8) * 10000
cut.ann.sum[7, "mort"] <- sum(cut.05.mort.kgC[, 5])/1000/(15^2 * pi * 8) * 10000
cut.ann.sum[8, "mort"] <- sum(cut.06.mort.kgC[, 5])/1000/(15^2 * pi * 8) * 10000
cut.ann.sum[9, "mort"] <- sum(cut.07.mort.kgC[, 5])/1000/(15^2 * pi * 8) * 10000
#---------------------------------------------------------------------
assign("cut.ann.sum", cut.ann.sum, pos=1)
