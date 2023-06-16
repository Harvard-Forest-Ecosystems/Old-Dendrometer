# 9/06 kmk
# 7/12 elg converted script to R. This script is called by run.all.r

# to calc error estimates on annual fluxes, we need to generate plot means
plots<-unique(cut.99.kgC[, "plot"])
# 1999
cut.99.by.plot<-matrix(NA, nrow=length(plots), ncol=2, dimnames=list(c(plots), c("AGWB", "AGWI")))
for(i in 1:length(plots))
{
	cut.99.by.plot[i, "AGWB"]<-sum(cut.99.kgC[cut.99.kgC[, "plot"]==plots[i], 20])/1000/(10^2 * pi) * 10000
	cut.99.by.plot[i, "AGWI"]<-diff(c((sum(cut.99.kgC[cut.99.kgC[, "plot"]==plots[i], 4])/1000/(10^2 * pi) * 10000), cut.99.by.plot[i, "AGWB"]))
}
# 2000
cut.00.by.plot<-matrix(NA, nrow=length(plots), ncol=4, dimnames=list(c(plots), c("AGWB", "AGWI", "recruit", "mort")))
for(i in 1:length(plots))
{
	cut.00.by.plot[i, "AGWB"]<-sum(cut.00.kgC[cut.00.kgC[, "plot"]==plots[i], 22])/1000/(10^2 * pi) * 10000
	cut.00.by.plot[i, "AGWI"]<-diff(c((sum(cut.00.kgC[cut.00.kgC[, "plot"]==plots[i], "ref.dbh"])/1000/(10^2 * pi) * 10000), cut.00.by.plot[i, "AGWB"]))
	cut.00.by.plot[i, "recruit"]<-(sum(cut.03.rcrt.kgC.in[cut.03.rcrt.kgC.in[, "plot"]==plots[i], 14])/1000/(10^2 * pi) * 10000)/4
	cut.00.by.plot[i, "mort"]<-sum(cut.00.mort.kgC[cut.00.mort.kgC[, "plot"]==plots[i], 5])/1000/(10^2 * pi) * 10000
}
# 2001
cut.01.by.plot<-matrix(NA, nrow=length(plots), ncol=4, dimnames=list(c(plots), c("AGWB", "AGWI", "recruit", "mort")))
for(i in 1:length(plots))
{
	cut.01.by.plot[i, "AGWB"]<-sum(cut.01.kgC[cut.01.kgC[, "plot"]==plots[i], 20])/1000/(15^2 * pi) * 10000
	cut.01.by.plot[i, "AGWI"]<-diff(c((sum(cut.01.kgC[cut.01.kgC[, "plot"]==plots[i], "ref.dbh"])/1000/(15^2 * pi) * 10000), cut.01.by.plot[i, "AGWB"]))
	cut.01.by.plot[i, "recruit"]<-(sum(cut.03.rcrt.kgC.in[cut.03.rcrt.kgC.in[, "plot"]==plots[i], 14])/1000/(10^2 * pi) * 10000)/4
	cut.01.by.plot[i, "mort"]<-sum(cut.01.mort.kgC[cut.01.mort.kgC[, "plot"]==plots[i], 5])/1000/(10^2 * pi) * 10000
}
# 2002
cut.02.by.plot<-matrix(NA, nrow=length(plots), ncol=4, dimnames=list(c(plots), c("AGWB", "AGWI", "recruit", "mort")))
for(i in 1:length(plots))
{
	cut.02.by.plot[i, "AGWB"]<-sum(cut.02.kgC[cut.02.kgC[, "plot"]==plots[i], 22])/1000/(15^2 * pi) * 10000
	cut.02.by.plot[i, "AGWI"]<-diff(c((sum(cut.02.kgC[cut.02.kgC[, "plot"]==plots[i], "ref.dbh.tape"])/1000/(15^2 * pi) * 10000), cut.02.by.plot[i, "AGWB"]))
	cut.02.by.plot[i, "recruit"]<-(sum(cut.03.rcrt.kgC.in[cut.03.rcrt.kgC.in[, "plot"]==plots[i], 14])/1000/(10^2 * pi) * 10000)/4
	cut.02.by.plot[i, "recruit"]<-cut.02.by.plot[i,3]+(sum(cut.03.rcrt.kgC.out[cut.03.rcrt.kgC.out[, "plot"]==plots[i], 14])/1000/((15^2-10^2) * pi) * 10000)/2
	cut.02.by.plot[i, "mort"]<-sum(cut.02.mort.kgC[cut.02.mort.kgC[, "plot"]==plots[i], 5])/1000/(15^2 * pi) * 10000
}
# 2003
cut.03.by.plot<-matrix(NA, nrow=length(plots), ncol=4, dimnames=list(c(plots), c("AGWB", "AGWI", "recruit", "mort")))
for(i in 1:length(plots))
{
	cut.03.by.plot[i, "AGWB"]<-sum(cut.03.kgC[cut.03.kgC[, "plot"]==plots[i], 14])/1000/(15^2 * pi) * 10000
	cut.03.by.plot[i, "AGWI"]<-diff(c((sum(cut.03.kgC[cut.03.kgC[, "plot"]==plots[i], "ref.dbh.tape"])/1000/(15^2 * pi) * 10000), cut.03.by.plot[i, "AGWB"]))
	cut.03.by.plot[i, "recruit"]<-(sum(cut.03.rcrt.kgC.in[cut.03.rcrt.kgC.in[, "plot"]==plots[i], 14])/1000/(10^2 * pi) * 10000)/4
	cut.03.by.plot[i, "recruit"]<-cut.03.by.plot[i,3]+(sum(cut.03.rcrt.kgC.out[cut.03.rcrt.kgC.out[, "plot"]==plots[i], 14])/1000/((15^2-10^2) * pi) * 10000)/2
	cut.03.by.plot[i, "mort"]<-sum(cut.03.mort.kgC[cut.03.mort.kgC[, "plot"]==plots[i], 5])/1000/(15^2 * pi) * 10000
}
# 2004
cut.04.by.plot<-matrix(NA, nrow=length(plots), ncol=4, dimnames=list(c(plots), c("AGWB", "AGWI", "recruit", "mort")))
for(i in 1:length(plots))
{
	cut.04.by.plot[i, "AGWB"]<-sum(cut.04.kgC[cut.04.kgC[, "plot"]==plots[i], 8])/1000/(15^2 * pi) * 10000
	cut.04.by.plot[i, "AGWI"]<-diff(c((sum(cut.04.kgC[cut.04.kgC[, "plot"]==plots[i], "ref.dbh"])/1000/(15^2 * pi) * 10000), cut.04.by.plot[i, "AGWB"]))
	cut.04.by.plot[i, "recruit"]<-sum(cut.04.rcrt.kgC[cut.04.rcrt.kgC[, "plot"]==plots[i], 8])/1000/(15^2 * pi) * 10000
	cut.04.by.plot[i, "mort"]<-sum(cut.04.mort.kgC[cut.04.mort.kgC[, "plot"]==plots[i], 5])/1000/(15^2 * pi) * 10000
}
# 2005
cut.05.by.plot<-matrix(NA, nrow=length(plots), ncol=4, dimnames=list(c(plots), c("AGWB", "AGWI", "recruit", "mort")))
for(i in 1:length(plots))
{
	cut.05.by.plot[i, "AGWB"]<-sum(cut.05.kgC[cut.05.kgC[, "plot"]==plots[i], 7])/1000/(15^2 * pi) * 10000
	cut.05.by.plot[i, "AGWI"]<-diff(c((sum(cut.05.kgC[cut.05.kgC[, "plot"]==plots[i], "ref.dbh"])/1000/(15^2 * pi) * 10000), cut.05.by.plot[i, "AGWB"]))
	cut.05.by.plot[i, "recruit"]<-(sum(cut.06.rcrt.kgC[cut.06.rcrt.kgC[, "plot"]==plots[i], 8])/1000/(15^2 * pi) * 10000)/2
	cut.05.by.plot[i, "mort"]<-sum(cut.05.mort.kgC[cut.05.mort.kgC[, "plot"]==plots[i], 5])/1000/(15^2 * pi) * 10000
}
# 2006
cut.06.by.plot<-matrix(NA, nrow=length(plots), ncol=4, dimnames=list(c(plots), c("AGWB", "AGWI", "recruit", "mort")))
for(i in 1:length(plots))
{
	cut.06.by.plot[i, "AGWB"]<-sum(cut.06.kgC[cut.06.kgC[, "plot"]==plots[i], 8])/1000/(15^2 * pi) * 10000
	cut.06.by.plot[i, "AGWI"]<-diff(c((sum(cut.06.kgC[cut.06.kgC[, "plot"]==plots[i], "ref.dbh.tape"])/1000/(15^2 * pi) * 10000), cut.06.by.plot[i, "AGWB"]))
	cut.06.by.plot[i, "recruit"]<-(sum(cut.06.rcrt.kgC[cut.06.rcrt.kgC[, "plot"]==plots[i], 8])/1000/(15^2 * pi) * 10000)/2
	cut.06.by.plot[i, "mort"]<-sum(cut.06.mort.kgC[cut.06.mort.kgC[, "plot"]==plots[i], 5])/1000/(15^2 * pi) * 10000
}
# 2007
cut.07.by.plot<-matrix(NA, nrow=length(plots), ncol=4, dimnames=list(c(plots), c("AGWB", "AGWI", "recruit", "mort")))
for(i in 1:length(plots))
{
	cut.07.by.plot[i, "AGWB"]<-sum(cut.07.kgC[cut.07.kgC[, "plot"]==plots[i], 8])/1000/(15^2 * pi) * 10000
	cut.07.by.plot[i, "AGWI"]<-diff(c((sum(cut.07.kgC[cut.07.kgC[, "plot"]==plots[i], "ref.dbh"])/1000/(15^2 * pi) * 10000), cut.07.by.plot[i, "AGWB"]))
	#cut.07.by.plot[i, "recruit"]<-(sum(cut.08.rcrt.kgC[cut.08.rcrt.kgC[, "plot"]==plots[i], 8])/1000/(15^2 * pi) * 10000)/2
	cut.07.by.plot[i, "mort"]<-sum(cut.07.mort.kgC[cut.07.mort.kgC[, "plot"]==plots[i], 5])/1000/(15^2 * pi) * 10000
}
#
assign("cut.99.by.plot", cut.99.by.plot, pos=1)
assign("cut.00.by.plot", cut.00.by.plot, pos=1)
assign("cut.01.by.plot", cut.01.by.plot, pos=1)
assign("cut.02.by.plot", cut.02.by.plot, pos=1)
assign("cut.03.by.plot", cut.03.by.plot, pos=1)
assign("cut.04.by.plot", cut.04.by.plot, pos=1)
assign("cut.05.by.plot", cut.05.by.plot, pos=1)
assign("cut.06.by.plot", cut.06.by.plot, pos=1)
assign("cut.07.by.plot", cut.07.by.plot, pos=1)
#----------------------------------
# make a matrix to fill w/ 95% CIs of the annual fluxes
# we want error bars for the positive and negative portions of the fluxes
# where the positive portion is agwi + recruitment and the negative flux is mortality
cut.ann.stats<-matrix(NA, nrow=9, ncol=2, dimnames=list(c("1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007"), c("pos.95CI", "neg.95CI")))
source("trees/s.scripts/CI.calc.txt")
# 1999
cut.ann.stats[1, "pos.95CI"]<-CI.calc(cut.99.by.plot[, "AGWI"])
# 2000
junk<-matrix(NA, nrow(cut.00.by.plot), 1)
for (i in 1:nrow(cut.00.by.plot))
	{
		junk[i]<-cut.00.by.plot[i, "AGWI"] + cut.00.by.plot[i, "recruit"]
	}
cut.ann.stats[2, "pos.95CI"]<-CI.calc(junk)
cut.ann.stats[2, "neg.95CI"]<-CI.calc(tow.00.by.plot[, "mort"])
# 2001
junk<-matrix(NA, nrow(cut.01.by.plot), 1)
for (i in 1:nrow(cut.01.by.plot))
	{
		junk[i]<-cut.01.by.plot[i, "AGWI"]+cut.01.by.plot[i, "recruit"]
	}
cut.ann.stats[3, "pos.95CI"]<-CI.calc(junk)
cut.ann.stats[3, "neg.95CI"]<-CI.calc(cut.01.by.plot[, "mort"])
# 2002
junk<-matrix(NA, nrow(cut.02.by.plot), 1)
for (i in 1:nrow(cut.02.by.plot))
	{
		junk[i]<-cut.02.by.plot[i, "AGWI"] + cut.02.by.plot[i, "recruit"]
	}
cut.ann.stats[4, "pos.95CI"]<-CI.calc(junk)
cut.ann.stats[4, "neg.95CI"]<-CI.calc(cut.02.by.plot[, "mort"])
# 2003
junk<-matrix(NA, nrow(cut.03.by.plot), 1)
for (i in 1:nrow(cut.03.by.plot))
	{
		junk[i]<-cut.03.by.plot[i, "AGWI"] + cut.03.by.plot[i, "recruit"]
	}
cut.ann.stats[5, "pos.95CI"]<-CI.calc(junk)
cut.ann.stats[5, "neg.95CI"]<-CI.calc(cut.03.by.plot[, "mort"])
# 2004
junk<-matrix(NA, nrow(cut.04.by.plot), 1)
for (i in 1:nrow(cut.04.by.plot))
	{
		junk[i]<-cut.04.by.plot[i,2] + cut.04.by.plot[i, "recruit"]
	}
cut.ann.stats[6, "pos.95CI"]<-CI.calc(junk)
cut.ann.stats[6, "neg.95CI"]<-CI.calc(cut.04.by.plot[, "mort"])
# 2005
junk<-matrix(NA, nrow(cut.05.by.plot), 1)
for (i in 1:nrow(cut.05.by.plot))
	{
		junk[i]<-cut.05.by.plot[i, "AGWI"] + cut.05.by.plot[i, "recruit"]
	}
cut.ann.stats[7, "pos.95CI"]<-CI.calc(junk)
cut.ann.stats[7, "neg.95CI"]<-CI.calc(cut.05.by.plot[, "mort"])
# 2006
junk<-matrix(NA, nrow(cut.06.by.plot), 1)
for (i in 1:nrow(cut.06.by.plot))
	{
		junk[i]<-cut.06.by.plot[i, "AGWI"] + cut.06.by.plot[i, "recruit"]
	}
cut.ann.stats[8, "pos.95CI"]<-CI.calc(junk)
cut.ann.stats[8, "neg.95CI"]<-CI.calc(cut.06.by.plot[, "mort"])
# 2007
cut.ann.stats[9, "pos.95CI"]<-CI.calc(cut.07.by.plot[, "AGWI"])
cut.ann.stats[9, "neg.95CI"]<-CI.calc(cut.07.by.plot[, "mort"])
#
assign("cut.ann.stats", cut.ann.stats, pos=1)
#----------------------------------------------------------------------------
# plot gross fluxes in biomass
# fill '07 recruitment w/ 0 until survey is done in '08
cut.ann.sum[is.na(cut.ann.sum)]<-0
# subtract 13 from the 2001 mort value b/c we're going to take out a segment of the y-axis
cut.ann.sum[3, 4]<-cut.ann.sum[3, 4]-13

library(gplots)
# plot the data w/ no axes
plot.data<-rbind(cut.ann.sum[,1], cut.ann.sum[,3], -cut.ann.sum[,4])
  par(mfrow = c(2, 1))
  par(mar = c(0, 4, 4, 2))
  tmp<-barplot(plot.data[-3, ], axes=F,yaxs = "i", ylim = c(0, 3.2), las = 2,names.arg=rep("",9),col=c(3,4))
  plotCI(tmp,plot.data[1, ]+plot.data[2,],uiw=cut.ann.stats[,"pos.95CI"],add=T,type="n",gap=0)  
  par(mar = c(5, 4, 0, 2),xpd=NA)    
  lower = plot.data[3, ]-cut.ann.stats[, "neg.95CI"]
  upper = plot.data[3, ]+cut.ann.stats[, "neg.95CI"]
  upper[3]<-NA
  lower[3]<-NA
  barplot2(plot.data[3, ],axes=F, yaxs = "i", ylim = c(-3, 0), las = 2,col=2,names.arg=c(1999:2007),plot.ci=T,ci.l=lower,ci.u=upper,xpd=NA, ylab="Aboveground woody increment (MgC/ha)")
  
  axis(at=c(-3:3), side=2, labels=c(-16,-15,-1,0,1,2,3), cex=1.03, line=-.5)
# add a break in the y-axis

segments(-.13,-1.5,.07,-1.3, col=1, lwd=2, lty=1)
segments(-.13,-1.65,.07,-1.45, col=1, lwd=2, lty=1)
text(3.15,-2.2,"+/- 11.41",cex=.7,col="white")  
  
  legend("bottom",legend=c("growth","recruitment", "mortality"),fill=c(3,4,2))
  

  
  
