tow.ann.sum.spp <- NULL

tow.93.kgC.pk <- cbind(tow.93.kgC[,c("plot","spp")],tow.93.kgC[, "dbh.tape"])
tow.ann.sum.spp <- rbind(tow.ann.sum.spp,
	c(1993,tapply(tow.93.kgC.pk[, 3], tow.93.kgC.pk[,"spp"], sum)/
	1000/(10^2 * pi * 34) * 10000))
tow.98.kgC.pk <- cbind(tow.98.kgC[,c("plot","spp")],
	apply(tow.98.kgC[, 5:ncol(tow.98.kgC)], 1, max))
tow.ann.sum.spp <- rbind(tow.ann.sum.spp,
	c(1998,tapply(tow.98.kgC.pk[, 3], tow.98.kgC.pk[,"spp"], sum)/
	1000/(10^2 * pi * 34) * 10000))
tow.99.kgC.pk <- cbind(tow.99.kgC[,c("plot","spp")],
	apply(tow.99.kgC[, 5:ncol(tow.99.kgC)], 1, max))
tow.ann.sum.spp <- rbind(tow.ann.sum.spp,
	c(1999,tapply(tow.99.kgC.pk[, 3], tow.99.kgC.pk[,"spp"], sum)/
	1000/(10^2 * pi * 34) * 10000))
tow.00.kgC.pk <- cbind(tow.00.kgC[,c("plot","spp")],
	apply(tow.00.kgC[, 5:ncol(tow.00.kgC)], 1, max))
tow.ann.sum.spp <- rbind(tow.ann.sum.spp,
	c(2000,tapply(tow.00.kgC.pk[, 3], tow.00.kgC.pk[,"spp"], sum)/
	1000/(10^2 * pi * 34) * 10000))
tow.01.kgC.pk <- cbind(tow.01.kgC[,c("plot","spp")],
	apply(tow.01.kgC[, 5:ncol(tow.01.kgC)], 1, max))
tow.ann.sum.spp <- rbind(tow.ann.sum.spp,
	c(2001,tapply(tow.01.kgC.pk[, 3], tow.01.kgC.pk[,"spp"], sum)/
	1000/(10^2 * pi * 34) * 10000))
tow.02.kgC.pk <- cbind(tow.02.kgC[,c("plot","spp")],
	apply(tow.02.kgC[, 5:ncol(tow.02.kgC)], 1, max))
tow.ann.sum.spp <- rbind(tow.ann.sum.spp,
	c(2002,tapply(tow.02.kgC.pk[, 3], tow.02.kgC.pk[,"spp"], sum)/
	1000/(10^2 * pi * 34) * 10000))
tow.03.kgC.pk <- cbind(tow.03.kgC[,c("plot","spp")],
	apply(tow.03.kgC[, 5:ncol(tow.03.kgC)], 1, max))
tow.ann.sum.spp <- rbind(tow.ann.sum.spp,
	c(2003,tapply(tow.03.kgC.pk[, 3], tow.03.kgC.pk[,"spp"], sum)/
	1000/(10^2 * pi * 34) * 10000))
tow.04.kgC.pk <- cbind(tow.04.kgC[,c("plot","spp")],
	apply(tow.04.kgC[, 5:ncol(tow.04.kgC)], 1, max))
tow.ann.sum.spp <- rbind(tow.ann.sum.spp,
	c(2004,tapply(tow.04.kgC.pk[, 3], tow.04.kgC.pk[,"spp"], sum)/
	1000/(10^2 * pi * 34) * 10000))
tow.05.kgC.pk <- cbind(tow.05.kgC[,c("plot","spp")],
	apply(tow.05.kgC[, 5:ncol(tow.05.kgC)], 1, max))
tow.ann.sum.spp <- rbind(tow.ann.sum.spp,
	c(2005,tapply(tow.05.kgC.pk[, 3], tow.05.kgC.pk[,"spp"], sum)/
	1000/(10^2 * pi * 34) * 10000))
tow.06.kgC.pk <- cbind(tow.06.kgC[,c("plot","spp")],
	apply(tow.06.kgC[, 5:ncol(tow.06.kgC)], 1, max))
tow.ann.sum.spp <- rbind(tow.ann.sum.spp,
	c(2006,tapply(tow.06.kgC.pk[, 3], tow.06.kgC.pk[,"spp"], sum)/
	1000/(10^2 * pi * 34) * 10000))
tow.07.kgC.pk <- cbind(tow.07.kgC[,c("plot","spp")],
	apply(tow.07.kgC[, 5:ncol(tow.07.kgC)], 1, max))
tow.ann.sum.spp <- rbind(tow.ann.sum.spp,
	c(2007,tapply(tow.07.kgC.pk[, 3], tow.07.kgC.pk[,"spp"], sum)/
	1000/(10^2 * pi * 34) * 10000))
tow.08.kgC.pk <- cbind(tow.08.kgC[,c("plot","spp")],
	apply(tow.08.kgC[, 5:ncol(tow.08.kgC)], 1, max))
tow.ann.sum.spp <- rbind(tow.ann.sum.spp,
	c(2008,tapply(tow.08.kgC.pk[, 3], tow.08.kgC.pk[,"spp"], sum)/
	1000/(10^2 * pi * 34) * 10000))
tow.09.kgC.pk <- cbind(tow.09.kgC[,c("plot","spp")],
	apply(tow.09.kgC[, 5:ncol(tow.09.kgC)], 1, max))
tow.ann.sum.spp <- rbind(tow.ann.sum.spp,
	c(2009,tapply(tow.09.kgC.pk[, 3], tow.09.kgC.pk[,"spp"], sum)/
	1000/(10^2 * pi * 34) * 10000))
tow.10.kgC.pk <- cbind(tow.10.kgC[,c("plot","spp")],
	apply(tow.10.kgC[, 5:ncol(tow.10.kgC)], 1, max))
tow.ann.sum.spp <- rbind(tow.ann.sum.spp,
	c(2010,tapply(tow.10.kgC.pk[, 3], tow.10.kgC.pk[,"spp"], sum)/
	1000/(10^2 * pi * 34) * 10000))
tow.11.kgC.pk <- cbind(tow.11.kgC[,c("plot","spp")],
	apply(tow.11.kgC[, 5:ncol(tow.11.kgC)], 1, max))
tow.ann.sum.spp <- rbind(tow.ann.sum.spp,
	c(2011,tapply(tow.11.kgC.pk[, 3], tow.11.kgC.pk[,"spp"], sum)/
	1000/(10^2 * pi * 34) * 10000))
#tow.12.kgC.pk <- cbind(tow.12.kgC[,c("plot","spp")],
	#apply(tow.12.kgC[, 5:ncol(tow.12.kgC)], 1, max))
#tow.ann.sum.spp <- rbind(tow.ann.sum.spp,
	#c(2012,tapply(tow.12.kgC.pk[, 3], tow.12.kgC.pk[,"spp"], sum)/
	#1000/(10^2 * pi * 34) * 10000))
#tow.13.kgC.pk <- cbind(tow.13.kgC[,c("plot","spp")],
	#apply(tow.13.kgC[, 5:ncol(tow.13.kgC)], 1, max))
#tow.ann.sum.spp <- rbind(tow.ann.sum.spp,
	#c(2013,tapply(tow.13.kgC.pk[, 3], tow.13.kgC.pk[,"spp"], sum)/
	#1000/(10^2 * pi * 34) * 10000))
tow.ann.sum.spp[,-1] <- signif(tow.ann.sum.spp[,-1],4)
colnames(tow.ann.sum.spp) <- c("Year",names(tow.ann.sum.spp[1,-1]))
 tow.ann.sum.spp <- tow.ann.sum.spp[,c(1,(1+order(tow.ann.sum.spp[15,-1],decreasing=T)))]
# put some padding on matrix to leave room for legend
tow.ann.sum.spp <- rbind(tow.ann.sum.spp,rep(NA,16),rep(NA,16),rep(NA,16))
# combine the 'other' species
tow.ann.sum.spp.dom<- cbind(tow.ann.sum.spp[,1:4],apply(tow.ann.sum.spp[,5:16],1,sum))
dimnames(tow.ann.sum.spp.dom)<-list(tow.ann.sum.spp[,1],c(names(tow.ann.sum.spp[1,1:4]),"other"))
tow.ann.sum.spp.dom <- tow.ann.sum.spp.dom[,-1]
par(mar=c(5,5.5,1,1),las=1)
barplot(t(tow.ann.sum.spp.dom[,4:1]),legend.text=T,
	col=c("brown","green","blue","red"),
xlab="Year",cex.lab=1.75,
ylab=expression(paste("Above-ground Biomass"," ","MgC ha"^{-1})))
dev.copy(png,"AGWB_spp_9311.png")
dev.off()
apply(tow.ann.sum.spp.dom[,4:1],2,diff) -> agwi.spp.dom
 agwi.spp.dom[1,]<- agwi.spp.dom[1,]/5

par(mar=c(5,5.5,1,1),las=1)
barplot(t(agwi.spp.dom),
	col=c("brown","green","blue","red"),legend.text=T,
	xlab="Year",cex.lab=1.75,
ylab=expression(paste("Above-ground Biomass Increment ","MgC ha"^{-1} ,"y"^{-1})))
par(mar=c(5,5.5,1,1),las=1)
matplot(as.numeric(names(agwi.spp.dom[1:14,1])), agwi.spp.dom[1:14,],
pch=1:4,type="b", col=c("brown","green","blue","red"),lty=rep(1,4),
 lwd=1.5,xlab="Year",cex.lab=1.75,cex.axis=1.5,
 ylab=expression(paste("AGWI ","MgC ha"^{-1} ,"y"^{-1})))
axis(side=1,at=1998:2011,labels=F)
legend("bottomright",names(agwi.spp.dom[1,]),
pch=1:4, ncol=2,col=c("brown","green","blue","red"),bty="n")
abline(h=0,lty=3)
dev.copy(png,"AGWI_spp_98_11.png")
dev.off()
