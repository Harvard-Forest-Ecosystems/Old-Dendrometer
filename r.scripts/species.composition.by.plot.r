#elg 7/12
#
#before running this script run.all.r should be run.
#This script will process species composition data at all the different plots.
#

tmp<-tow.11.dbh #analysis is limited to those species and plots surveyed in 2011
plots<-unique(tmp[,1]) #vector of all plots surveyed
spp<-unique(tmp[,3]) #vector of all species
all.data.ba<-list()#basal area data for all years
all.data.bai<-list() #basal area index data for all years
all.data.kgC<-list()#mass data for all years in kgC
all.data.stems<-list()#stem count data for all years
all.data.norm.ba<-list()#basal area data normalized to portion of plot total for all years
all.data.norm.kgC<-list()#biomass data normalized to portion of plot total for all years
all.data.norm.stems<-list()#stem count data normalized to portion of plot total for all years

all.data.leaf<-list() 
all.data.norm.leaf<-list()

#takes a vector and returns the same vector normatlized so that all the elements sum to 1
norm<-function(x){
 x/sum(x,na.rm=T)
}

years<-list.files("trees/data/dendrometer")
for(year in years){
diameter<-get(paste("tow.",substr(year,3,4),".dbh",sep=""))#these objects are created by the run.all.r script

mass<-get(paste("tow.",substr(year,3,4),".kgC",sep=""))#these objects are created by the run.all.r script
data.ba<-as.data.frame(matrix(NA,nrow=length(spp),ncol=length(plots)))
data.bai<-as.data.frame(matrix(NA,nrow=length(spp),ncol=length(plots)))
data.kgC<-as.data.frame(matrix(NA,ncol=length(plots),nrow=length(spp)))
data.stems<-as.data.frame(matrix(NA,ncol=length(plots),nrow=length(spp)))
dimnames(data.ba)<-list(spp,plots)
dimnames(data.bai)<-list(spp,plots)
dimnames(data.kgC)<-list(spp,plots) 
dimnames(data.kgC)<-list(spp,plots)

for(pl in plots){
  for (sp in spp){
      data.ba[sp,pl]<-sum((((diameter[diameter[,3]==sp&diameter[,1]==pl,4])/100/2)^2)*pi) #basal area in square  meters
      data.bai[sp,pl]<-sum((((diameter[diameter[,3]==sp&diameter[,1]==pl,4])/100/2)^2)*pi)/(10^2*pi) #basal area index
      data.kgC[sp,pl]<-sum(mass[mass[,3]==sp&mass[,1]==pl,4]) # kgC
      data.stems[sp,pl]<-length(mass[mass[,3]==sp&mass[,1]==pl,4]) # stem count
  }

}
all.data.kgC[[year]]<-data.kgC
all.data.ba[[year]]<-data.ba
all.data.bai[[year]]<-data.bai
all.data.stems[[year]]<-data.stems



norm.data.ba<-apply(as.matrix(data.ba),2,norm)
norm.data.kgC<-apply(as.matrix(data.kgC),2,norm)
norm.data.stems<-apply(as.matrix(data.stems),2,norm)




all.data.norm.ba[[year]]<-norm.data.ba
all.data.norm.kgC[[year]]<-norm.data.kgC
all.data.norm.stems[[year]]<-norm.data.stems

}
#species BAI
spp.bai<-data.frame(matrix(as.numeric(unlist(lapply(all.data.bai,function(x){apply(x,1,mean)}))),ncol=15,byrow=T))
colnames(spp.bai)<-spp
spp.bai<-data.frame(year=years[1:nrow(spp.bai)],spp.bai)
#write.csv(spp.bai,file="C:\\Users\\egoldman\\Documents\\Biometry\\dendrometers\\spp.bai.csv")


#comparison to leaf litter data
yrs<-substr(years,3,4)
leaf.years<-years[is.element(yrs,substr(objects()[grep(glob2rx("lit.??"),objects())],5,6))] #years that leaf data is available
leaf.years<-leaf.years[leaf.years!="2005"&leaf.years!="2004"]

spp.leaf<-dimnames(lit.09)[[1]][5:18]
plots.leaf<-dimnames(lit.09)[[3]]

for(year in leaf.years){
lit.data<-get(paste("lit.",substr(year,3,4),sep=""))
leaf.data<-as.data.frame(matrix(NA,ncol=length(plots.leaf),nrow=length(spp.leaf)))
dimnames(leaf.data)<-list(spp.leaf,plots.leaf)


for(pl in plots.leaf){
  for(sp in spp.leaf){
    if(sp %in% dimnames(lit.data)[[1]]){
      leaf.data[sp,pl]<-sum(lit.data[sp,,pl])
    }else{
      leaf.data[sp,pl]<-0
    }
  
  }
}

norm.leaf.data<-apply(as.matrix(leaf.data),2,norm)

all.data.leaf[[year]]<-leaf.data
all.data.norm.leaf[[year]]<-norm.leaf.data
}



cols.leaf<-rainbow(length(spp.leaf))
par(mfrow=c(2,1))
barplot(as.matrix(norm.leaf.data),col=cols.leaf)
plot(NA,xpd=NA)
legend("topleft",legend=spp.leaf,fill=cols.leaf,ncol=3)

convert<-function(x){
 tmp<-as.data.frame(matrix(0,ncol=length(plots.leaf),nrow=length(spp.leaf)))
 dimnames(tmp)<-list(spp.leaf,plots.leaf)
 tmp["ash",]<-x["ash",]
 tmp["beech",]<-x["beech",]
 tmp["birch",]<-x["yb",]+x["wb",]+x["gb",]+x["bb",]
 tmp["cherry",]<-x["cherry",]
 tmp["hemlock",]<-x["hem",]
 tmp["oak",]<-x["ro",]+x["bo",]
 tmp["r.maple",]<-x["rm",]
 tmp["r.pine",]<-x["rp",]
 tmp["spruce",]<-x["ws",]
 tmp["str.maple",]<-x["sm",]
 tmp["w.pine",]<-x["wp",]
 tmp
}

all.converted.norm.data<-list()
for(year in years){
all.converted.norm.data[[year]]<-apply(convert(all.data.kgC[[year]]),2,norm)

}


 #compare leaf data to biomass data in a given year
 year<-"2009" #year must be between 1998 and 2009 (excluding 2004 and 2005) since those are the years where there was both litter data and biomass data
par(mfrow=c(3,1))
barplot(as.matrix(all.converted.norm.data[[year]]),col=cols.leaf,xlab="biomass kgC")
barplot(as.matrix(all.data.norm.leaf[[year]]),col=cols.leaf,xlab="leaves kgC")
plot(NA,xpd=NA)
legend("topleft",legend=spp.leaf,fill=cols.leaf,ncol=3,cex=1.2)

par(mfrow=c(1,1))
leaf.bio.ratio<-all.data.norm.leaf[[year]]/all.converted.norm.data[[year]]
leaf.bio.ratio[is.nan(leaf.bio.ratio)|!is.finite(leaf.bio.ratio)]<-NA
apply(leaf.bio.ratio,1,mean)
boxplot(t(leaf.bio.ratio),ylab="% leaf/%biomass",xlab=year)
abline(h=1)

m1<-apply(all.data.norm.leaf[[year]],1,mean)*100
m2<-apply(all.converted.norm.data[[year]],1,mean)*100
sd1<-apply(all.data.norm.leaf[[year]],1,sd)*100
sd2<-apply(all.converted.norm.data[[year]],1,sd)*100

plot(m1,m2,col=cols.leaf,pch=15,xlab="%litter by weight",ylab="% biomass")
plotCI(m1,m2,uiw=sd1,err="x",add=T,type="n",gap=0,barcol=cols.leaf)
plotCI(m1,m2,uiw=sd2,er="y",add=T,type="n",gap=0,barcol=cols.leaf)
legend("topleft",legend=spp.leaf,fill=cols.leaf,ncol=2)
abline(0,1)

plot(all.data.norm.leaf[[year]]*100,all.converted.norm.data[[year]]*100,col=cols.leaf,xlab="%litter by mass",ylab="% biomass")
legend("topleft",legend=spp2,fill=cols,ncol=2)
legend("topleft",legend=spp.leaf,fill=cols.leaf,ncol=2)
abline(0,1) 



 #total difference from 1993 to 2011 by plot

ord<-order(apply(abs(all.data.ba[["1993"]]-all.data.ba[["2011"]]),2,sum))

cols<-rainbow(length(spp))

#progression of species composition over time ordered by total change in composition
for(year in years[1:15]){
par(ask=T)
 barplot(as.matrix(all.data.ba[[year]])[,ord],col=cols,xlab=year,ylab="basal area")
}


 

#total species compostion
#Here all the species data from all the plots are summed

total.93<-apply(all.data.ba[["1993"]],1,sum)
total.11<-apply(all.data.ba[["2011"]],1,sum)
plot(total.93,total.11,col=cols,pch=16,,main="Total basal area for all plots summed",xlab="1993",ylab="2011")
abline(0,1)
legend("topleft",legend=spp,fill=cols,ncol=3,cex=1)

pct.chng<-(total.11-total.93)/total.93
barplot(pct.chng,col=cols,main="change in basal area for all plots",ylab="% change in basal area between 1993 and 2011")


par(mfrow=c(6,6),mar=c(4,1,1,1),oma=c(1,1,3,1))
for(pl in plots){
  plot(as.matrix(all.data.ba[["1993"]][,pl]),as.matrix(all.data.ba[["2011"]][,pl]),col=cols,pch=16,xlab=pl,cex=2)
  abline(0,1)
}
plot(NA,xpd=NA)
legend("topleft",legend=spp,fill=cols,ncol=3,cex=1)
title("Change in basal area at each plot",outer=T)




#changes in dominant species


#this function finds the dominant species of tree (>50% of basal area) or the combination of the most abundant trees that sum to >50%
recur<-function(x,nms="",mx=0){
  if(max(x)+mx>.5){
    substr(tmp<-paste(nms,names(which.max(x))),2,nchar(tmp))
  }else{
    tmp<-x[x!=max(x)]
    names(tmp)<-names(x)[x!=max(x)]
    recur(tmp,paste(nms,names(which.max(x))),sum(mx,max(x)))
  }
}



dom.11<-c()
dom.93<-c()
for(pl in plots){
 dom.11<-c(dom.11,recur(all.data.norm.ba[["2011"]][,pl]))   #dominant species in 2011
 dom.93<-c(dom.93,recur(all.data.norm.ba[["1993"]][,pl]))   #dominant species in 1993
}

change.plots<-plots[dom.11!=dom.93]

 par(mfrow=c(2,1))
 barplot(as.matrix(all.data.ba[["1993"]][,change.plots]),col=cols,xlab="1993",main="Changes in dominant species")
 barplot(as.matrix(all.data.ba[["2011"]][,change.plots]),col=cols,xlab="2011")
 legend("topleft",legend=spp,fill=cols,ncol=3,cex=.55)
  cbind(change.plots,dom.93[dom.93!=dom.11],dom.11[dom.93!=dom.11])




 trends.ba<-list()
 trends.kgC<-list()
 trends.leaf<-list()
 trends.leaf.norm<-list()
 for(pl in plots){
  tmp.ba<-NULL
  tmp.kgC<-NULL
  tmp.leaf<-NULL
  tmp.leaf.norm<-NULL
  for(y in years[1:15]){ 
    tmp.ba<-rbind(tmp.ba,t(all.data.ba[[y]][,pl])) 
    tmp.kgC<-rbind(tmp.kgC,t(all.data.kgC[[y]][,pl]))
    if(y %in% leaf.years){ 
      tmp.leaf<-rbind(tmp.leaf,t(all.data.leaf[[y]][,pl]))
      tmp.leaf.norm<-rbind(tmp.leaf.norm,t(all.data.norm.leaf[[y]][,pl]))
    }  
  }
  dimnames(tmp.ba)<-list(years[1:15],spp)
  dimnames(tmp.kgC)<-list(years[1:15],spp)
  dimnames(tmp.leaf)<-list(leaf.years,spp.leaf)
  dimnames(tmp.leaf.norm)<-list(leaf.years,spp.leaf)
  trends.ba[[pl]]<-tmp.ba
  trends.kgC[[pl]]<-tmp.kgC
  trends.leaf[[pl]]<-tmp.leaf
  trends.leaf.norm[[pl]]<-tmp.leaf.norm
 }

 numeric.years<-as.numeric(years[1:15])
 numeric.years.leaf<-as.numeric(leaf.years)

#plot timeseries for each species at each plot
time.series<-function(trends,metric="Basal Area",yrs=numeric.years,sp=spp,cls=cols){
par(mfrow=c(6,6),oma=c(1,1,2,1),mar=c(4,4,1,1),xpd=F) 
 for(t in 1:length(trends)){
    plot(yrs,trends[[t]][,1],type="n",xlab=plots[t],ylab=metric,ylim=c(min(trends[[t]][trends[[t]]!=0]),max(trends[[t]])))
    for(ln in 1:length(sp)){
      lines(yrs,trends[[t]][,ln],col=cls[ln],lwd=2)
    }
 
 }
 
 total<-0
 for(t in trends){
    total<-total+t
 }
 plot(yrs,total[,1],type="n",xlab="Total",ylab=metric,ylim=c(min(total[total!=0]),max(total)))
    for(ln in 1:length(sp)){
      lines(yrs,total[,ln],col=cls[ln],lwd=2)
    }
 par(mar=c(1,1,1,1),xpd=NA)
  plot(1,type="n",xlab="",ylab="",axes=F)
legend("topleft",legend=sp,fill=cls,ncol=3,cex=1)
title(main="Species composition changes over time",outer=T)
 }

time.series(trends.ba) 

time.series(trends.kgC,"kgC")

time.series(trends.leaf,metric="dry leaf mass",yrs=numeric.years.leaf,sp=spp.leaf,cls=cols.leaf)
time.series(trends.leaf.norm,metric="% total dry leaf mass",yrs=numeric.years.leaf,sp=spp.leaf,cls=cols.leaf)
 
#plot time series for just plots that have changed dominant species
change.time.series<-function(trends,metric="Basal Area",yrs=numeric.years,sp=spp,cls=cols){
par(mfrow=c(4,4),oma=c(0,0,2,0),mar=c(4,4,1,1),xpd=F)
 for(t in change.plots){
    plot(yrs,trends[[t]][,1],type="n",xlab=t,ylab=metric,ylim=c(min(trends[[t]][trends[[t]]!=0]),max(trends[[t]])))
    for(ln in 1:length(sp)){
      lines(yrs,trends[[t]][,ln],col=cls[ln],lwd=2)
    }
 
 }
 
  par(mar=c(1,1,1,1),xpd=NA)
  plot(1,type="n",xlab="",ylab="",axes=F)
legend("topleft",legend=sp,fill=cls,ncol=3,cex=1)
title(main="Species composition changes over time",outer=T)
}


change.time.series(trends.ba) 

change.time.series(trends.kgC,"kgC")

change.time.series(trends.leaf,metric="dry leaf mass",yrs=numeric.years.leaf,sp=spp.leaf,cls=cols.leaf)
change.time.series(trends.leaf.norm,metric="% total dry leaf mass",yrs=numeric.years.leaf,sp=spp.leaf,cls=cols.leaf)


select.plots<-c("A1","B2","B3","B4","C5","D1","D2","E1","E4","F1","F2","F3","G4","G5","H1")

select.time.series<-function(trends,metric="Basal Area",yrs=numeric.years,sp=spp,cls=cols){
par(mfrow=c(4,4),oma=c(0,0,2,0),mar=c(4,4,1,1),xpd=F)
 for(t in select.plots){
    plot(yrs,trends[[t]][,1],type="n",xlab=t,ylab=metric,ylim=c(min(trends[[t]][trends[[t]]!=0]),max(trends[[t]])))
    for(ln in 1:length(sp)){
      lines(yrs,trends[[t]][,ln],col=cls[ln],lwd=2)
    }
 
 }
 
  par(mar=c(1,1,1,1),xpd=NA)
  plot(1,type="n",xlab="",ylab="",axes=F)
legend("topleft",legend=sp,fill=cls,ncol=3,cex=1)
title(main="Species composition changes over time",outer=T)
}
