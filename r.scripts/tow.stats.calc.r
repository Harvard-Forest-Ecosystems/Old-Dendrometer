# 3/06 kmk
# 1/12 lkw - updated 
# 7/12 elg converted script to R. This script is called by run.all.r
# to calc error estimates on annual fluxes, we need to generate plot means
plots<-unique(tow.93.kgC[, "plot"])
# 1993
tow.93.by.plot<-matrix(NA, nrow=length(plots), ncol=1, dimnames=list(c(plots), c("AGWB")))
for(i in 1:length(plots))
{
tow.93.by.plot[i, "AGWB"]<-(sum(tow.93.kgC[tow.93.kgC[, "plot"]==plots[i], "dbh.tape"]))/1000/(10^2 * pi) * 10000
}
# 1994-97
tow.94.97.by.plot<-matrix(NA, nrow=length(plots), ncol=3, dimnames=list(c(plots), c("AGWI", "recruit", "mort")))
for(i in 1:length(plots))
{
tow.94.97.by.plot[i, "AGWI"]<-diff(c(tow.93.by.plot[dimnames(tow.93.by.plot)[[1]]==plots[i], 1], sum(tow.98.kgC[tow.98.kgC[, "plot"]==plots[i], 4])/1000/(10^2 * pi) * 10000))/4
tow.94.97.by.plot[i, "recruit"]<-(sum(tow.99.rcrt.kgC[tow.99.rcrt.kgC[, "plot"]==plots[i], 22])/1000/(10^2 * pi) * 10000)/6
tow.94.97.by.plot[i, "mort"]<-(sum(tow.98.mort.kgC[tow.98.mort.kgC[, "plot"]==plots[i], 5])/1000/(10^2 * pi) * 10000)/5
}
# 1998
tow.98.by.plot<-matrix(NA, nrow=length(plots), ncol=4, dimnames=list(c(plots), c("AGWB", "AGWI", "recruit", "mort")))
for(i in 1:length(plots))
{
	tow.98.by.plot[i, "AGWB"]<-sum(tow.98.kgC[tow.98.kgC[, "plot"]==plots[i], 21])/1000/(10^2 * pi) * 10000
	tow.98.by.plot[i, "AGWI"]<-(diff(c((sum(tow.98.kgC[tow.98.kgC[, "plot"]==plots[i], "ref.dbh.tape"])/1000/(10^2 * pi) * 10000), tow.98.by.plot[i, "AGWB"])))*1.25
	tow.98.by.plot[i, "recruit"]<-(sum(tow.99.rcrt.kgC[tow.99.rcrt.kgC[, "plot"]==plots[i], 22])/1000/(10^2 * pi) * 10000)/6
	tow.98.by.plot[i, "mort"]<-(sum(tow.98.mort.kgC[tow.98.mort.kgC[, "plot"]==plots[i], 5])/1000/(10^2 * pi) * 10000)/5
}
# 1999
tow.99.by.plot<-matrix(NA, nrow=length(plots), ncol=4, dimnames=list(c(plots), c("AGWB", "AGWI", "recruit", "mort")))
for(i in 1:length(plots))
{
	tow.99.by.plot[i, "AGWB"]<-sum(tow.99.kgC[tow.99.kgC[, "plot"]==plots[i], 22])/1000/(10^2 * pi) * 10000
	tow.99.by.plot[i, "AGWI"]<-diff(c((sum(tow.99.kgC[tow.99.kgC[, "plot"]==plots[i], "ref.dbh"])/1000/(10^2 * pi) * 10000), tow.99.by.plot[i, "AGWB"]))
	tow.99.by.plot[i, "recruit"]<-(sum(tow.99.rcrt.kgC[tow.99.rcrt.kgC[, "plot"]==plots[i], 22])/1000/(10^2 * pi) * 10000)/6
	tow.99.by.plot[i, "mort"]<-sum(tow.99.mort.kgC[tow.99.mort.kgC[, "plot"]==plots[i], 5])/1000/(10^2 * pi) * 10000
}
# 2000
tow.00.by.plot<-matrix(NA, nrow=length(plots), ncol=4, dimnames=list(c(plots), c("AGWB", "AGWI", "recruit", "mort")))
for(i in 1:length(plots))
{
	tow.00.by.plot[i, "AGWB"]<-sum(tow.00.kgC[tow.00.kgC[, "plot"]==plots[i], 23])/1000/(10^2 * pi) * 10000
	tow.00.by.plot[i, "AGWI"]<-diff(c((sum(tow.00.kgC[tow.00.kgC[, "plot"]==plots[i], "ref.dbh"])/1000/(10^2 * pi) * 10000), tow.00.by.plot[i, "AGWB"]))
	tow.00.by.plot[i, "recruit"]<-(sum(tow.03.rcrt.kgC[tow.03.rcrt.kgC[, "plot"]==plots[i], 14])/1000/(10^2 * pi) * 10000)/4
	tow.00.by.plot[i, "mort"]<-sum(tow.00.mort.kgC[tow.00.mort.kgC[, "plot"]==plots[i], 5])/1000/(10^2 * pi) * 10000
}
# 2001
tow.01.by.plot<-matrix(NA, nrow=length(plots), ncol=4, dimnames=list(c(plots), c("AGWB", "AGWI", "recruit", "mort")))
for(i in 1:length(plots))
{
	tow.01.by.plot[i, "AGWB"]<-sum(tow.01.kgC[tow.01.kgC[, "plot"]==plots[i], 18])/1000/(10^2 * pi) * 10000
	tow.01.by.plot[i, "AGWI"]<-diff(c((sum(tow.01.kgC[tow.01.kgC[, "plot"]==plots[i], "ref.dbh.tape"])/1000/(10^2 * pi) * 10000), tow.01.by.plot[i, "AGWB"]))
	tow.01.by.plot[i, "recruit"]<-(sum(tow.03.rcrt.kgC[tow.03.rcrt.kgC[, "plot"]==plots[i], 14])/1000/(10^2 * pi) * 10000)/4
	tow.01.by.plot[i, "mort"]<-sum(tow.01.mort.kgC[tow.01.mort.kgC[, "plot"]==plots[i], 5])/1000/(10^2 * pi) * 10000
}
# 2002
tow.02.by.plot<-matrix(NA, nrow=length(plots), ncol=4, dimnames=list(c(plots), c("AGWB", "AGWI", "recruit", "mort")))
for(i in 1:length(plots))
{
	tow.02.by.plot[i, "AGWB"]<-sum(tow.02.kgC[tow.02.kgC[, "plot"]==plots[i], 22])/1000/(10^2 * pi) * 10000
	tow.02.by.plot[i, "AGWI"]<-diff(c((sum(tow.02.kgC[tow.02.kgC[, "plot"]==plots[i], "ref.dbh.tape"])/1000/(10^2 * pi) * 10000), tow.02.by.plot[i, "AGWB"]))
	tow.02.by.plot[i, "recruit"]<-(sum(tow.03.rcrt.kgC[tow.03.rcrt.kgC[, "plot"]==plots[i], 14])/1000/(10^2 * pi) * 10000)/4
	tow.02.by.plot[i, "mort"]<-sum(tow.02.mort.kgC[tow.02.mort.kgC[, "plot"]==plots[i], 5])/1000/(10^2 * pi) * 10000
}
# 2003
tow.03.by.plot<-matrix(NA, nrow=length(plots), ncol=4, dimnames=list(c(plots), c("AGWB", "AGWI", "recruit", "mort")))
for(i in 1:length(plots))
{
	tow.03.by.plot[i, "AGWB"]<-sum(tow.03.kgC[tow.03.kgC[, "plot"]==plots[i], 14])/1000/(10^2 * pi) * 10000
	tow.03.by.plot[i, "AGWI"]<-diff(c((sum(tow.03.kgC[tow.03.kgC[, "plot"]==plots[i], "ref.dbh.tape"])/1000/(10^2 * pi) * 10000), tow.03.by.plot[i, "AGWB"]))
	tow.03.by.plot[i, "recruit"]<-(sum(tow.03.rcrt.kgC[tow.03.rcrt.kgC[, "plot"]==plots[i], 14])/1000/(10^2 * pi) * 10000)/4
	tow.03.by.plot[i, "mort"]<-sum(tow.03.mort.kgC[tow.03.mort.kgC[, "plot"]==plots[i], 5])/1000/(10^2 * pi) * 10000
}
# 2004
tow.04.by.plot<-matrix(NA, nrow=length(plots), ncol=4, dimnames=list(c(plots), c("AGWB", "AGWI", "recruit", "mort")))
for(i in 1:length(plots))
{
	tow.04.by.plot[i, "AGWB"]<-sum(tow.04.kgC[tow.04.kgC[, "plot"]==plots[i], 8])/1000/(10^2 * pi) * 10000
	tow.04.by.plot[i, "AGWI"]<-diff(c((sum(tow.04.kgC[tow.04.kgC[, "plot"]==plots[i], "ref.dbh"])/1000/(10^2 * pi) * 10000), tow.04.by.plot[i, "AGWB"]))
	tow.04.by.plot[i, "recruit"]<-sum(tow.04.rcrt.kgC[tow.04.rcrt.kgC[, "plot"]==plots[i], 8])/1000/(10^2 * pi) * 10000
	tow.04.by.plot[i, "mort"]<-sum(tow.04.mort.kgC[tow.04.mort.kgC[, "plot"]==plots[i], 5])/1000/(10^2 * pi) * 10000
}
# 2005
tow.05.by.plot<-matrix(NA, nrow=length(plots), ncol=4, dimnames=list(c(plots), c("AGWB", "AGWI", "recruit", "mort")))
for(i in 1:length(plots))
{
	tow.05.by.plot[i, "AGWB"]<-sum(tow.05.kgC[tow.05.kgC[, "plot"]==plots[i], 7])/1000/(10^2 * pi) * 10000
	tow.05.by.plot[i, "AGWI"]<-diff(c((sum(tow.05.kgC[tow.05.kgC[, "plot"]==plots[i], "ref.dbh"])/1000/(10^2 * pi) * 10000), tow.05.by.plot[i, "AGWB"]))
	tow.05.by.plot[i, "recruit"]<-(sum(tow.06.rcrt.kgC[tow.06.rcrt.kgC[, "plot"]==plots[i], 8])/1000/(10^2 * pi) * 10000)/2
	tow.05.by.plot[i, "mort"]<-sum(tow.05.mort.kgC[tow.05.mort.kgC[, "plot"]==plots[i], 5])/1000/(10^2 * pi) * 10000
}
# 2006
tow.06.by.plot<-matrix(NA, nrow=length(plots), ncol=4, dimnames=list(c(plots), c("AGWB", "AGWI", "recruit", "mort")))
for(i in 1:length(plots))
{
	tow.06.by.plot[i, "AGWB"]<-sum(tow.06.kgC[tow.06.kgC[, "plot"]==plots[i], 8])/1000/(10^2 * pi) * 10000
	tow.06.by.plot[i, "AGWI"]<-diff(c((sum(tow.06.kgC[tow.06.kgC[, "plot"]==plots[i], "ref.dbh.tape"])/1000/(10^2 * pi) * 10000), tow.06.by.plot[i, 1]))
	tow.06.by.plot[i, "recruit"]<-(sum(tow.06.rcrt.kgC[tow.06.rcrt.kgC[, "plot"]==plots[i], 8])/1000/(10^2 * pi) * 10000)/2
	tow.06.by.plot[i, "mort"]<-sum(tow.06.mort.kgC[tow.06.mort.kgC[, "plot"]==plots[i], 5])/1000/(10^2 * pi) * 10000
}
# 2007
tow.07.by.plot<-matrix(NA, nrow=length(plots), ncol=4, dimnames=list(c(plots), c("AGWB", "AGWI", "recruit", "mort")))
for(i in 1:length(plots))
{
	tow.07.by.plot[i, "AGWB"]<-sum(tow.07.kgC[tow.07.kgC[, "plot"]==plots[i], 8])/1000/(10^2 * pi) * 10000
	tow.07.by.plot[i, "AGWI"]<-diff(c((sum(tow.07.kgC[tow.07.kgC[, "plot"]==plots[i], "ref.dbh"])/1000/(10^2 * pi) * 10000), tow.07.by.plot[i, "AGWB"]))
	tow.07.by.plot[i, "recruit"]<-(sum(tow.08.rcrt.kgC[tow.08.rcrt.kgC[, "plot"]==plots[i], 8])/1000/(10^2 * pi) * 10000)/2
	tow.07.by.plot[i, "mort"]<-sum(tow.07.mort.kgC[tow.07.mort.kgC[, "plot"]==plots[i], 5])/1000/(10^2 * pi) * 10000
}
# 2008
tow.08.by.plot<-matrix(NA, nrow=length(plots), ncol=4, dimnames=list(c(plots), c("AGWB", "AGWI", "recruit", "mort")))
for(i in 1:length(plots))
{
	tow.08.by.plot[i, "AGWB"]<-sum(tow.08.kgC[tow.08.kgC[, "plot"]==plots[i], 8])/1000/(10^2 * pi) * 10000
	tow.08.by.plot[i, "AGWI"]<-diff(c((sum(tow.08.kgC[tow.08.kgC[, "plot"]==plots[i], "ref.dbh"])/1000/(10^2 * pi) * 10000), tow.08.by.plot[i, "AGWB"]))
	tow.08.by.plot[i, "recruit"]<-(sum(tow.08.rcrt.kgC[tow.08.rcrt.kgC[, "plot"]==plots[i], 8])/1000/(10^2 * pi) * 10000)/2
	tow.08.by.plot[i, "mort"]<-sum(tow.08.mort.kgC[tow.08.mort.kgC[, "plot"]==plots[i], 5])/1000/(10^2 * pi) * 10000
}
# 2009
tow.09.by.plot<-matrix(NA, nrow=length(plots), ncol=4, dimnames=list(c(plots), c("AGWB", "AGWI", "recruit", "mort")))
for(i in 1:length(plots))
{
	tow.09.by.plot[i, "AGWB"]<-sum(tow.09.kgC[tow.09.kgC[, "plot"]==plots[i], 8])/1000/(10^2 * pi) * 10000
	tow.09.by.plot[i, "AGWI"]<-diff(c((sum(tow.09.kgC[tow.09.kgC[, "plot"]==plots[i], "ref.dbh"])/1000/(10^2 * pi) * 10000), tow.09.by.plot[i, "AGWB"]))
	tow.09.by.plot[i, "recruit"]<-(sum(tow.10.rcrt.kgC[tow.10.rcrt.kgC[, "plot"]==plots[i], 8])/1000/(10^2 * pi) * 10000)/2 
	tow.09.by.plot[i, "mort"]<-sum(tow.09.mort.kgC[tow.09.mort.kgC[, "plot"]==plots[i], 5])/1000/(10^2 * pi) * 10000
}
# 2010
tow.10.by.plot<-matrix(NA, nrow=length(plots), ncol=4, dimnames=list(c(plots), c("AGWB", "AGWI", "recruit", "mort")))
for(i in 1:length(plots))
{
	tow.10.by.plot[i, "AGWB"]<-sum(tow.10.kgC[tow.10.kgC[, "plot"]==plots[i], 8])/1000/(10^2 * pi) * 10000
	tow.10.by.plot[i, "AGWI"]<-diff(c((sum(tow.10.kgC[tow.10.kgC[, "plot"]==plots[i], "ref.dbh"])/1000/(10^2 * pi) * 10000), tow.10.by.plot[i, "AGWB"]))
	tow.10.by.plot[i, "recruit"]<-(sum(tow.10.rcrt.kgC[tow.10.rcrt.kgC[, "plot"]==plots[i], 8])/1000/(10^2 * pi) * 10000)/2 
	tow.10.by.plot[i, "mort"]<-sum(tow.10.mort.kgC[tow.10.mort.kgC[, "plot"]==plots[i], 5])/1000/(10^2 * pi) * 10000
}
# 2011

tow.11.by.plot<-matrix(NA, nrow=length(plots), ncol=4, dimnames=list(c(plots), c("AGWB", "AGWI", "recruit", "mort")))
for(i in 1:length(plots))
{
	tow.11.by.plot[i, "AGWB"]<-sum(tow.11.kgC[tow.11.kgC[, "plot"]==plots[i], 8])/1000/(10^2 * pi) * 10000
	tow.11.by.plot[i, "AGWI"]<-diff(c((sum(tow.11.kgC[tow.11.kgC[, "plot"]==plots[i], "ref.dbh"])/1000/(10^2 * pi) * 10000), tow.11.by.plot[i, "AGWB"]))
	tow.11.by.plot[i, "recruit"]<-(sum(tow.12.rcrt.kgC[tow.12.rcrt.kgC[, "plot"]==plots[i], 8])/1000/(10^2 * pi) * 10000)/2 
	tow.11.by.plot[i, "mort"]<-sum(tow.11.mort.kgC[tow.11.mort.kgC[, "plot"]==plots[i], 5])/1000/(10^2 * pi) * 10000
}

#2012
tow.12.by.plot<-matrix(NA, nrow=length(plots), ncol=4, dimnames=list(c(plots), c("AGWB", "AGWI", "recruit", "mort")))
for(i in 1:length(plots))
{
	tow.12.by.plot[i, "AGWB"]<-sum(tow.12.kgC[tow.12.kgC[, "plot"]==plots[i], 8],na.rm=T)/1000/(10^2 * pi) * 10000
	tow.12.by.plot[i, "AGWI"]<-diff(c((sum(tow.12.kgC[tow.12.kgC[, "plot"]==plots[i], "ref.dbh"],na.rm=T)/1000/(10^2 * pi) * 10000), tow.12.by.plot[i, "AGWB"]))
	tow.12.by.plot[i, "recruit"]<-(sum(tow.12.rcrt.kgC[tow.12.rcrt.kgC[, "plot"]==plots[i], 8])/1000/(10^2 * pi) * 10000)/2 
	tow.12.by.plot[i, "mort"]<-sum(tow.12.mort.kgC[tow.12.mort.kgC[, "plot"]==plots[i], 5])/1000/(10^2 * pi) * 10000
}

#2013
tow.13.by.plot<-matrix(NA, nrow=length(plots), ncol=4, dimnames=list(c(plots), c("AGWB", "AGWI", "recruit", "mort")))
for(i in 1:length(plots))
{
	tow.13.by.plot[i, "AGWB"]<-sum(tow.13.kgC[tow.13.kgC[, "plot"]==plots[i], 8],na.rm=T)/1000/(10^2 * pi) * 10000
	tow.13.by.plot[i, "AGWI"]<-diff(c((sum(tow.13.kgC[tow.13.kgC[, "plot"]==plots[i], "ref.dbh"],na.rm=T)/1000/(10^2 * pi) * 10000), tow.13.by.plot[i, "AGWB"]))
	tow.13.by.plot[i, "recruit"]<-(sum(tow.14.rcrt.kgC[tow.14.rcrt.kgC[, "plot"]==plots[i], 8])/1000/(10^2 * pi) * 10000)/2 
	tow.13.by.plot[i, "mort"]<-sum(tow.13.mort.kgC[tow.13.mort.kgC[, "plot"]==plots[i], 5])/1000/(10^2 * pi) * 10000
}

#2014
tow.14.by.plot<-matrix(NA, nrow=length(plots), ncol=4, dimnames=list(c(plots), c("AGWB", "AGWI", "recruit", "mort")))
for(i in 1:length(plots))
{
	tow.14.by.plot[i, "AGWB"]<-sum(tow.14.kgC[tow.14.kgC[, "plot"]==plots[i], 8],na.rm=T)/1000/(10^2 * pi) * 10000
	tow.14.by.plot[i, "AGWI"]<-diff(c((sum(tow.14.kgC[tow.14.kgC[, "plot"]==plots[i], "ref.dbh"],na.rm=T)/1000/(10^2 * pi) * 10000), tow.14.by.plot[i, "AGWB"]))
	tow.14.by.plot[i, "recruit"]<-(sum(tow.14.rcrt.kgC[tow.14.rcrt.kgC[, "plot"]==plots[i], 8])/1000/(10^2 * pi) * 10000)/2 
	tow.14.by.plot[i, "mort"]<-sum(tow.14.mort.kgC[tow.14.mort.kgC[, "plot"]==plots[i], 5])/1000/(10^2 * pi) * 10000
}

#2015
tow.15.by.plot<-matrix(NA, nrow=length(plots), ncol=4, dimnames=list(c(plots), c("AGWB", "AGWI", "recruit", "mort")))
for(i in 1:length(plots))
{
	tow.15.by.plot[i, "AGWB"]<-sum(tow.15.kgC[tow.15.kgC[, "plot"]==plots[i], 8],na.rm=T)/1000/(10^2 * pi) * 10000
	tow.15.by.plot[i, "AGWI"]<-diff(c((sum(tow.15.kgC[tow.15.kgC[, "plot"]==plots[i], "ref.dbh"],na.rm=T)/1000/(10^2 * pi) * 10000), tow.15.by.plot[i, "AGWB"]))
	tow.15.by.plot[i, "recruit"]<-(sum(tow.16.rcrt.kgC[tow.16.rcrt.kgC[, "plot"]==plots[i], 8])/1000/(10^2 * pi) * 10000)/2 
	tow.15.by.plot[i, "mort"]<-sum(tow.15.mort.kgC[tow.15.mort.kgC[, "plot"]==plots[i], 5])/1000/(10^2 * pi) * 10000
}

#2016
tow.16.by.plot<-matrix(NA, nrow=length(plots), ncol=4, dimnames=list(c(plots), c("AGWB", "AGWI", "recruit", "mort")))
for(i in 1:length(plots))
{
	tow.16.by.plot[i, "AGWB"]<-sum(tow.16.kgC[tow.16.kgC[, "plot"]==plots[i], 8],na.rm=T)/1000/(10^2 * pi) * 10000
	tow.16.by.plot[i, "AGWI"]<-diff(c((sum(tow.16.kgC[tow.16.kgC[, "plot"]==plots[i], "ref.dbh"],na.rm=T)/1000/(10^2 * pi) * 10000), tow.16.by.plot[i, "AGWB"]))
	tow.16.by.plot[i, "recruit"]<-(sum(tow.16.rcrt.kgC[tow.16.rcrt.kgC[, "plot"]==plots[i], 8])/1000/(10^2 * pi) * 10000)/2 
	tow.16.by.plot[i, "mort"]<-sum(tow.16.mort.kgC[tow.16.mort.kgC[, "plot"]==plots[i], 4])/1000/(10^2 * pi) * 10000
}



#
assign("tow.93.by.plot", tow.93.by.plot, pos=1)
assign("tow.94.97.by.plot", tow.94.97.by.plot, pos=1)
assign("tow.98.by.plot", tow.98.by.plot, pos=1)
assign("tow.99.by.plot", tow.99.by.plot, pos=1)
assign("tow.00.by.plot", tow.00.by.plot, pos=1)
assign("tow.01.by.plot", tow.01.by.plot, pos=1)
assign("tow.02.by.plot", tow.02.by.plot, pos=1)
assign("tow.03.by.plot", tow.03.by.plot, pos=1)
assign("tow.04.by.plot", tow.04.by.plot, pos=1)
assign("tow.05.by.plot", tow.05.by.plot, pos=1)
assign("tow.06.by.plot", tow.06.by.plot, pos=1)
assign("tow.07.by.plot", tow.07.by.plot, pos=1)
assign("tow.08.by.plot", tow.08.by.plot, pos=1)
assign("tow.09.by.plot", tow.09.by.plot, pos=1)
assign("tow.10.by.plot", tow.10.by.plot, pos=1)
assign("tow.11.by.plot", tow.11.by.plot, pos=1)
assign("tow.12.by.plot", tow.12.by.plot, pos=1)
assign("tow.13.by.plot", tow.13.by.plot, pos=1)
assign("tow.14.by.plot", tow.14.by.plot, pos=1)
assign("tow.15.by.plot", tow.15.by.plot, pos=1)
assign("tow.16.by.plot", tow.16.by.plot, pos=1)
#------------------------------------------------------
# make a matrix to fill w/ 95% CIs of the annual fluxes
# we want error bars for the positive and negative portions of the fluxes
# where the positive portion is AGWI + recruitment and the negative flux is mortality
tow.ann.stats<-matrix(NA, nrow=23, ncol=2, dimnames=list(c("1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011","2012","2013","2014","2015","2016"), c("pos.95CI", "neg.95CI")))
agwi.ci<- -matrix(NA, nrow=23, ncol=1, dimnames=list(c("1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011","2012","2013","2014","2015","2016"), c("agwi.ci")))
# 1994-1997
junk<-matrix(NA, nrow(tow.94.97.by.plot), 1)
for (i in 1:nrow(tow.94.97.by.plot))
	{
		junk[i]<-tow.94.97.by.plot[i, "AGWI"] + tow.94.97.by.plot[i, "recruit"]
	}
source("trees/s.scripts/CI.calc.txt")
tow.ann.stats[1:4, "pos.95CI"]<-CI.calc(junk)
agwi.ci[1:4, 1]<-CI.calc(tow.94.97.by.plot[, "AGWI"])
tow.ann.stats[1:4, "neg.95CI"]<-CI.calc(tow.94.97.by.plot[, "mort"])
# 1998
junk<-matrix(NA, nrow(tow.98.by.plot), 1)
for (i in 1:nrow(tow.98.by.plot))
	{
		junk[i]<-tow.98.by.plot[i, "AGWI"] + tow.98.by.plot[i, "recruit"]
	}
tow.ann.stats[5, "pos.95CI"]<-CI.calc(junk)
agwi.ci[5, 1]<-CI.calc(tow.98.by.plot[, "AGWI"])
tow.ann.stats[5, "neg.95CI"]<-CI.calc(tow.98.by.plot[, "mort"])
# 1999
junk<-matrix(NA, nrow(tow.99.by.plot), 1)
for (i in 1:nrow(tow.99.by.plot))
	{
		junk[i]<-tow.99.by.plot[i, "AGWI"] + tow.99.by.plot[i, "recruit"]
	}
tow.ann.stats[6, "pos.95CI"]<-CI.calc(junk)
agwi.ci[6, 1]<-CI.calc(tow.99.by.plot[, "AGWI"])
tow.ann.stats[6, "neg.95CI"]<-CI.calc(tow.99.by.plot[, "mort"])
# 2000
junk<-matrix(NA, nrow(tow.00.by.plot), 1)
for (i in 1:nrow(tow.00.by.plot))
	{
		junk[i]<-tow.00.by.plot[i, "AGWI"] + tow.00.by.plot[i, "recruit"]
	}
tow.ann.stats[7, "pos.95CI"]<-CI.calc(junk)
agwi.ci[7, 1]<-CI.calc(tow.00.by.plot[, "AGWI"])
tow.ann.stats[7, "neg.95CI"]<-CI.calc(tow.00.by.plot[, "mort"])
# 2001
junk<-matrix(NA, nrow(tow.01.by.plot), 1)
for (i in 1:nrow(tow.01.by.plot))
	{
		junk[i]<-tow.01.by.plot[i, "AGWI"] + tow.01.by.plot[i, "recruit"]
	}
tow.ann.stats[8, "pos.95CI"]<-CI.calc(junk)
agwi.ci[8, 1]<-CI.calc(tow.01.by.plot[, "AGWI"])
tow.ann.stats[8, "neg.95CI"]<-CI.calc(tow.01.by.plot[, "mort"])
# 2002
junk<-matrix(NA, nrow(tow.02.by.plot), 1)
for (i in 1:nrow(tow.02.by.plot))
	{
		junk[i]<-tow.02.by.plot[i, "AGWI"] + tow.02.by.plot[i, "recruit"]
	}
tow.ann.stats[9, "pos.95CI"]<-CI.calc(junk)
agwi.ci[9, 1]<-CI.calc(tow.02.by.plot[, "AGWI"])
tow.ann.stats[9, "neg.95CI"]<-CI.calc(tow.02.by.plot[, "mort"])
# 2003
junk<-matrix(NA, nrow(tow.03.by.plot), 1)
for (i in 1:nrow(tow.03.by.plot))
	{
		junk[i]<-tow.03.by.plot[i, "AGWI"] + tow.03.by.plot[i, "recruit"]
	}
tow.ann.stats[10, "pos.95CI"]<-CI.calc(junk)
agwi.ci[10, 1]<-CI.calc(tow.03.by.plot[, "AGWI"])
tow.ann.stats[10, "neg.95CI"]<-CI.calc(tow.03.by.plot[, "mort"])
# 2004
junk<-matrix(NA, nrow(tow.04.by.plot), 1)
for (i in 1:nrow(tow.04.by.plot))
	{
		junk[i]<-tow.04.by.plot[i, "AGWI"] + tow.04.by.plot[i, "recruit"]
	}
tow.ann.stats[11, "pos.95CI"]<-CI.calc(junk)
agwi.ci[11, 1]<-CI.calc(tow.04.by.plot[, "AGWI"])
tow.ann.stats[11, "neg.95CI"]<-CI.calc(tow.04.by.plot[, "mort"])
# 2005
junk<-matrix(NA, nrow(tow.05.by.plot), 1)
for (i in 1:nrow(tow.05.by.plot))
	{
		junk[i]<-tow.05.by.plot[i, "AGWI"] + tow.05.by.plot[i, "recruit"]
	}
tow.ann.stats[12, "pos.95CI"]<-CI.calc(junk)
agwi.ci[12, 1]<-CI.calc(tow.05.by.plot[, "AGWI"])
tow.ann.stats[12, "neg.95CI"]<-CI.calc(tow.05.by.plot[, "mort"])
# 2006
junk<-matrix(NA, nrow(tow.06.by.plot), 1)
for (i in 1:nrow(tow.06.by.plot))
	{
		junk[i]<-tow.06.by.plot[i, "AGWI"] + tow.06.by.plot[i, "recruit"]
	}
tow.ann.stats[13, "pos.95CI"]<-CI.calc(junk)
agwi.ci[13, 1]<-CI.calc(tow.06.by.plot[, "AGWI"])
tow.ann.stats[13, "neg.95CI"]<-CI.calc(tow.06.by.plot[, "mort"])
# 2007
junk<-matrix(NA, nrow(tow.07.by.plot), 1)
for (i in 1:nrow(tow.07.by.plot))
	{
		junk[i]<-tow.07.by.plot[i, "AGWI"] + tow.07.by.plot[i, "recruit"]
	}
tow.ann.stats[14, "pos.95CI"]<-CI.calc(junk)
agwi.ci[14, 1]<-CI.calc(tow.07.by.plot[, "AGWI"])
tow.ann.stats[14, "neg.95CI"]<-CI.calc(tow.07.by.plot[, "mort"])
# 2008
junk<-matrix(NA, nrow(tow.08.by.plot), 1)
for (i in 1:nrow(tow.08.by.plot))
	{
		junk[i]<-tow.08.by.plot[i, "AGWI"] + tow.08.by.plot[i, "recruit"]
	}
tow.ann.stats[15, "pos.95CI"]<-CI.calc(junk)
agwi.ci[15, 1]<-CI.calc(tow.08.by.plot[, "AGWI"])
tow.ann.stats[15, "neg.95CI"]<-CI.calc(tow.08.by.plot[, "mort"])
# 2009
junk<-matrix(NA, nrow(tow.09.by.plot), 1)
for (i in 1:nrow(tow.09.by.plot))
	{
		junk[i]<-tow.09.by.plot[i, "AGWI"] + tow.09.by.plot[i, "recruit"]
	}
tow.ann.stats[16, "pos.95CI"]<-CI.calc(junk)
agwi.ci[16, 1]<-CI.calc(tow.09.by.plot[, "AGWI"])
tow.ann.stats[16, "neg.95CI"]<-CI.calc(tow.09.by.plot[, "mort"])
# 2010
junk<-matrix(NA, nrow(tow.10.by.plot), 1)
for (i in 1:nrow(tow.10.by.plot))
	{
		junk[i]<-tow.10.by.plot[i, "AGWI"] + tow.10.by.plot[i, "recruit"]
	}
tow.ann.stats[17, "pos.95CI"]<-CI.calc(junk)
agwi.ci[17, 1]<-CI.calc(tow.10.by.plot[, "AGWI"])
tow.ann.stats[17, "neg.95CI"]<-CI.calc(tow.10.by.plot[, "mort"])
# 2011
junk<-matrix(NA, nrow(tow.11.by.plot), 1)
for (i in 1:nrow(tow.11.by.plot))
	{
		junk[i]<-tow.11.by.plot[i, "AGWI"] + tow.11.by.plot[i, "recruit"]
	}
tow.ann.stats[18, "pos.95CI"]<-CI.calc(junk)
agwi.ci[18, 1]<-CI.calc(tow.11.by.plot[, "AGWI"])
tow.ann.stats[18, "neg.95CI"]<-CI.calc(tow.11.by.plot[, "mort"])
# 2012
junk<-matrix(NA, nrow(tow.12.by.plot), 1)
for (i in 1:nrow(tow.12.by.plot))
	{
		junk[i]<-tow.12.by.plot[i, "AGWI"] + tow.12.by.plot[i, "recruit"]
	}
tow.ann.stats[19, "pos.95CI"]<-CI.calc(junk)
agwi.ci[19, 1]<-CI.calc(tow.12.by.plot[, "AGWI"])
tow.ann.stats[19, "neg.95CI"]<-CI.calc(tow.12.by.plot[, "mort"])
# 2013
junk<-matrix(NA, nrow(tow.13.by.plot), 1)
for (i in 1:nrow(tow.13.by.plot))
	{
		junk[i]<-tow.13.by.plot[i, "AGWI"] + tow.13.by.plot[i, "recruit"]
	}
tow.ann.stats[20, "pos.95CI"]<-CI.calc(junk)
agwi.ci[20, 1]<-CI.calc(tow.13.by.plot[, "AGWI"])
tow.ann.stats[20, "neg.95CI"]<-CI.calc(tow.13.by.plot[, "mort"])

# 2014
junk<-matrix(NA, nrow(tow.14.by.plot), 1)
for (i in 1:nrow(tow.14.by.plot))
	{
		junk[i]<-tow.14.by.plot[i, "AGWI"] + tow.14.by.plot[i, "recruit"]
	}
tow.ann.stats[21, "pos.95CI"]<-CI.calc(junk)
agwi.ci[21, 1]<-CI.calc(tow.14.by.plot[, "AGWI"])
tow.ann.stats[21, "neg.95CI"]<-CI.calc(tow.14.by.plot[, "mort"])

# 2015
junk<-matrix(NA, nrow(tow.15.by.plot), 1)
for (i in 1:nrow(tow.15.by.plot))
	{
		junk[i]<-tow.15.by.plot[i, "AGWI"] + tow.15.by.plot[i, "recruit"]
	}
tow.ann.stats[22, "pos.95CI"]<-CI.calc(junk)
agwi.ci[22, 1]<-CI.calc(tow.15.by.plot[, "AGWI"])
tow.ann.stats[22, "neg.95CI"]<-CI.calc(tow.15.by.plot[, "mort"])

# 2016
junk<-matrix(NA, nrow(tow.16.by.plot), 1)
for (i in 1:nrow(tow.16.by.plot))
	{
		junk[i]<-tow.16.by.plot[i, "AGWI"] + tow.16.by.plot[i, "recruit"]
	}
tow.ann.stats[23, "pos.95CI"]<-CI.calc(junk)
agwi.ci[23, 1]<-CI.calc(tow.16.by.plot[, "AGWI"])
tow.ann.stats[23, "neg.95CI"]<-CI.calc(tow.16.by.plot[, "mort"])



#
assign("tow.ann.stats", tow.ann.stats, pos=1)
#----------------------------------------------------------------------------
# plot gross fluxes in live biomass
library(gplots)
plot.data<-rbind(tow.ann.sum[5:24, 1], tow.ann.sum[5:24, 3],-tow.ann.sum[5:24, 4])
plot.data<-replace(plot.data,is.na(plot.data),0)
  par(mfrow = c(2, 1))
  par(mar = c(0, 4, 4, 2))
  
  tmp<-barplot(plot.data[-3, ], yaxs = "i", ylim = c(0, 3.2), las = 2,names.arg=rep("",20),col=c(3,4))
  plotCI(tmp,plot.data[1, ]+plot.data[2,],uiw=tow.ann.stats[4:23, "pos.95CI"],add=T,type="n",gap=0)  
  par(mar = c(5, 4, 0, 2),xpd=NA)    
  lower = plot.data[3, ]-tow.ann.stats[4:23, "neg.95CI"]
  upper = plot.data[3, ]+tow.ann.stats[4:23, "neg.95CI"]
  barplot2(plot.data[3, ], yaxs = "i", ylim = c(-3, 0), las = 2,col=2,names.arg=c("'94-'97",1998:2016),plot.ci=T,ci.l=lower,ci.u=upper,xpd=NA, ylab="Aboveground woody increment (MgC/ha)")
  
  legend("bottom",legend=c("growth","recruitment", "mortality"),fill=c(3,4,2))
