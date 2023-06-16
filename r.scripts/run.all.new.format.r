library("plyr")
if(Sys.info()[["nodename"]]=="eft"){
  dir<-"/DATA/HF_DATA/HF_BIOMETRY_WOFSY/"
}else{
  dir<-"//eft.as.harvard.edu/DATA/HF_DATA/HF_BIOMETRY_WOFSY/"
}

setwd(dir)


source("trees/r.scripts/new.dbh.make.r")
source("trees/r.scripts/calc.kgC.r")
source("R.scripts/old.functions.r")

dend.data<-read.table("trees/data/ems.dend.all.txt",stringsAsFactors=F,header=T)
dbh.data<-read.table("trees/data/ems.dbh.all.txt",stringsAsFactors=F,header=T)
spp.data<-read.table("trees/data/ems.spp.all.txt",stringsAsFactors=F,header=T)

dend.data<-dend.data[order(dend.data$plot,dend.data$tag,dend.data$year,dend.data$jday),]
dbh.data<-dbh.data[order(dbh.data$plot,dbh.data$tag,dbh.data$year,dbh.data$jday),]

years<-as.character(list.files("trees/data/dendrometer"))

#get mortality and recruitment data 
recruit.tow<-list()
dead.tow<-list()
for(year in years){
  yr<-substr(year,3,4)
  fl<-paste("trees/data/dendrometer/",year,"/tow.",yr,".mort.txt",sep="")
  if(file.exists(fl)){
    tmp<-read.table(file=fl,header=T)
    dead.tow[[year]]<-tmp
  }
  fl<-paste("trees/data/dendrometer/",year,"/tow.",yr,".recruits.txt",sep="")
   if(file.exists(fl)){
    tmp<-read.table(file=fl,header=T)
    recruit.tow[[year]]<-tmp
  }
}
#years that there is data for the cut plots
cut.years<-c("1999","2000","2001","2002","2003","2004","2005","2006","2007")
dead.cut<-list()
recruit.cut<-list()
for(year in cut.years){
  yr<-substr(year,3,4)
  fl<-paste("trees/data/dendrometer/",year,"/cut.",yr,".mort.txt",sep="")
  if(file.exists(fl)){
    tmp<-read.table(file=fl,header=T)
    dead.cut[[year]]<-tmp
  }
  fl<-paste("trees/data/dendrometer/",year,"/cut.",yr,".recruits.txt",sep="")
   if(file.exists(fl)){
    tmp<-read.table(file=fl,header=T)
    recruit.cut[[year]]<-tmp
  }
}

for(year in years){
  yr<-substr(year,3,4)
  dead.trees<-dead.tow[[year]]
  if(!is.null(dead.trees)){
    for(i in 1:nrow(dead.trees)){
      dend.data<-dend.data[!(dend.data$plot==dead.trees[i,1]&dend.data$tag==dead.trees[i,2]&dend.data$year>=as.numeric(year)),]
    }
  }

}

trees<-unique(dend.data[,c("plot","tag")])
dend.data$date<-strptime(paste(dend.data$year,dend.data$jday),"%Y %j")
dend.dates<-sort(unique(dend.data$date))
dbh.data$date<-strptime(paste(dbh.data$year,dbh.data$jday),format="%Y %j")
spp.col<-as.character(apply(trees,1,function(x){spp.data$spp[spp.data$plot==as.character(x[1])&spp.data$tag==as.numeric(x[2])]}) )
tmp<-data.frame(matrix(NA,ncol=length(dend.dates),nrow=nrow(trees)))
all.dbh<-data.frame(trees,spp.col,tmp)
names(all.dbh)<-c("plot","tag","spp",as.numeric(dend.dates))

source("trees/r.scripts/new.dbh.make.r")

for(i in 1:nrow(trees)){
  dbh<-dbh.data[dbh.data$plot==trees[i,1]&dbh.data$tag==trees[i,2],]
  dend<-dend.data[dend.data$plot==trees[i,1]&dend.data$tag==trees[i,2],]
  x.vals<-unique(dend$date)
  x.vals<-sort(x.vals)
  band.nums<-unique(dend$dend.num)
  all.estimates<-as.data.frame(matrix(NA,nrow=length(x.vals),ncol=nrow(dbh)*max(band.nums)))
  col.num<-1
  ########################
  # fill in missing data when bands switch
  if(length(band.nums)>1){
    for(bnd in 1:(length(band.nums)-1)){
      last.measure<-max(dend$date[dend$dend.num==band.nums[bnd]])
      first.measure.of.next<-min(dend$date[dend$dend.num==band.nums[bnd+1]])
      if(last.measure <first.measure.of.next & as.numeric(first.measure.of.next)-as.numeric(last.measure)< 300*24*60*60){
         tmp.row<-dend[dend$date==first.measure.of.next&dend$dend.num==band.nums[bnd+1],]
         tmp.row$dend.num<-band.nums[bnd]
         tmp.row$dend.meas <-dend$dend.meas[dend$date==last.measure&dend$dend.num==band.nums[bnd]] 
         dend<-rbind(dend,tmp.row)
      }
    }
  }
  
  ########################
  for(band in band.nums){
   ind<-dend$dend.num==band #the rows for band
   if(sum(ind)>1){
   band.rng<-range(dend$date[ind]) #date range for band
   dbh.in.rng<-dbh$date>=(band.rng[1])&dbh$date<=(band.rng[2]) #which dbh measurement were during date range for band i
   #add first dbh measurement since dbh and dend readings were done on different days in 1998
   if(band==1&dbh[1,"year"]==1998){
     dbh.in.rng[1]<-T
   }
   band.measures<-dend[ind,"dend.meas"]
   band.dates<-dend[ind,"date"]
   if(any(dbh.in.rng)){
      for(j in which(dbh.in.rng)){
        closest<-which.min((abs(band.dates-dbh$date[j])))
        ref.cal<-band.measures[closest]
        tmp<-new.dbh.make(band.measures,rep(dbh$DBH[j],length(band.measures)),rep(ref.cal,length(band.measures)))
        all.estimates[x.vals %in% band.dates,col.num]<-tmp
        names(all.estimates)[col.num]<-paste("band",band,"dbh",j,sep=".")
        col.num<-col.num+1
      }
    }
  }else{
    band.nums<-band.nums[-which(band.nums==band)] #remove band from list if there's only 1 reading
  }
  }
  all.estimates<-all.estimates[,1:(col.num-1)]
  if(!is.null(ncol(all.estimates))){
####fill gaps - only works if overlapping data is added from the paper record
  if(any(duplicated(dend$date))){
  for(l in 1:ncol(all.estimates)){ #look at each column of estimates
    tmp<-strsplit(names(all.estimates)[l],".",fixed=T)[[1]]
    band<-as.numeric(tmp[2])
    dbh.measure<-as.numeric(tmp[4])
    if(is.na(tail(all.estimates[,l],1))){#check if this column has NA values which need to be filled at the end
      for(ref.band in (band+1):max(band.nums)){ #look at each future band to fill in vlaues
        if(!any(names(all.estimates)==paste("band.",ref.band,".dbh.",dbh.measure,sep=""))){ #check for duplicate calculations in other columns
          ref.ind<-tail(which(!is.na(all.estimates[,l])),1)
          ref.dbh<-all.estimates[ref.ind,l]
          ref.date<-x.vals[ref.ind]
          ref.dend<-dend[dend$dend.num==ref.band&dend$date==ref.date,"dend.meas"]
          for(m in (ref.ind+1):nrow(all.estimates)){  #for each NA value in column
           fill.date<-x.vals[m]
            fill.dend<-dend[dend$dend.num==ref.band&dend$date==fill.date,"dend.meas"]
            if(length(fill.dend)==1&length(ref.dend)==1){
             all.estimates[m,l]<-new.dbh.make(fill.dend,ref.dbh,ref.dend)
            }
          }
        }
      }
    }
    if(is.na(head(all.estimates[,l],1))){#check if this column has NA values which need to be filled at the beginning
      for(ref.band in min(band.nums):(band-1)){ #look at each previous band to fill in vlaues
        if(!any(names(all.estimates)==paste("band.",ref.band,".dbh.",dbh.measure,sep=""))){ #check for duplicate calculations in other columns
          ref.ind<-head(which(!is.na(all.estimates[,l])),1)
          ref.dbh<-all.estimates[ref.ind,l]
          ref.date<-x.vals[ref.ind]
          ref.dend<-dend[dend$dend.num==ref.band&dend$date==ref.date,"dend.meas"]
          for(m in (ref.ind-1):1){  #for each NA value in column
           fill.date<-x.vals[m]
            fill.dend<-dend[dend$dend.num==ref.band&dend$date==fill.date,"dend.meas"]
            if(length(fill.dend)==1&length(ref.dend)==1){
             all.estimates[m,l]<-new.dbh.make(fill.dend,ref.dbh,ref.dend)
            }
          }
        }
      }
    }
    #lines(x.vals,all.estimates[,l],col=dbh.measure)
  }
  }
#  fix problem on overlapping bands having extra weight in average calculation
  
    median.estimate<-apply(all.estimates,1,function(x){median(x,na.rm=T)})
  }else{
    median.estimate<-all.estimates
  }
  all.dbh[i,as.character(as.numeric(x.vals))]<-median.estimate
  #matplot(as.numeric(x.vals),all.estimates)
  #lines(as.numeric(x.vals),median.estimate,lwd=3)

}

#############BACK CALC###############################################################
# B4 726 was recruited in 2004, and C1 217, D1 266 & H1 204 were recruited in 2006
	# all with DBHs too large to have grown in since the previous recruitment surveys
	# they were probably missed b/c they were near the plot border or b/c they looked dead but weren't
	# we must back-calculate their DBHs through 1993
	# & insert them into the DBH & kgC data from previous years
	# so as to be included in all annual summaries

  back.calc<-function(pl,tg,spp,start.year,end.year,min.size,max.size,transects="NONE"){
    dbh.subset<-all.dbh[,3+which((dend.dates$year+1900) %in% start.year:end.year)]
    pik.years<-apply(dbh.subset,1,function(x){!any(is.na(x))})
    pik.spp<- all.dbh$spp == spp
    pik.size<- dbh.subset[,ncol(dbh.subset)]>=min.size&dbh.subset[,ncol(dbh.subset)]<=max.size
    dbh.sub<-all.dbh[pik.years&pik.spp&pik.size,]

    if(transects=="SW"){
      dbh.sub<-dbh.sub[grep("[A-D][1-5]",dbh.sub$plot),]  #look at just plots in the SW transect
    }
  ref.date<-which(!is.na(all.dbh[all.dbh$plot==pl&all.dbh$tag==tg,-(1:3)]))[1]+3
  normalized.growth<-(dbh.sub[,-(1:3)]-dbh.sub[,ref.date])/dbh.sub[,ref.date]  #normalized growth for similar trees
  target.tree<-all.dbh[all.dbh$plot==pl&all.dbh$tag==tg,]
  first.dbh<- as.numeric(target.tree[ref.date])
  norm.target<-(target.tree[-(1:3)]-first.dbh)/first.dbh  #normalized growth for target tree
  
  tmp<-apply(normalized.growth,1,function(x){x-norm.target})
  tmp<-matrix(unlist(tmp),nrow=length(dend.dates),byrow=F)
  tmp<- abs(apply(tmp,2,sum,na.rm=T))
  wts<-abs(tmp-max(tmp))/max(tmp) #weights to apply to similar trees based on how similar their growth rates were
  
  #matplot(as.numeric(dend.dates),t(normalized.growth))
  #lines(as.numeric(dend.dates),norm.target,lwd=2)
  #lines(as.numeric(dend.dates),apply(normalized.growth,2,weighted.mean,w=wts^2),col=1,lty=2,lwd=2)
  
  modeled.tree<- apply(normalized.growth,2,weighted.mean,w=wts^2)
  modeled.tree[!is.na(norm.target)]<-norm.target[!is.na(norm.target)]
  modeled.tree<-modeled.tree*first.dbh+first.dbh  #use model for time when there is no data for tree
  
  #plot(as.numeric(dend.dates),modeled.tree)
  all.dbh[all.dbh$plot==pl&all.dbh$tag==tg,-(1:3)] <<-modeled.tree
  #modeled.tree
  }
  # calc a growth rate to apply to C1 217
  back.calc("B4",726,"ro",1998,2003,30,40,"SW")
  back.calc("C1",217,"hem",1998,2005,25,35)
  back.calc("D1",266,"wp",1998,2005,40,50)
  back.calc("H1",204,"rm",1998,2005,12,16)
 
#fix recruitments with missing data and extra data
years.rec<-names(recruit.tow) 
for(year in years.rec){
  rec.ind<- splus.is.element(all.dbh[,1:2],recruit.tow[[year]][,1:2])
  year.ind<-3+which(dend.dates$year+1900==as.numeric(year))
  missing.recruits<-apply(all.dbh[,year.ind],1,function(x){all(is.na(x))})&rec.ind
  if(any(missing.recruits)){
    #whenever dendroband data is missing during year of recruitment, take the smallest reading present in the record
    #following the year of recruitment
    all.dbh[missing.recruits,max(year.ind)]<- apply(all.dbh[missing.recruits,1:ncol(all.dbh)>max(year.ind)],1,min,na.rm=T)
  }
  
#whenever there is dendroband data present prior to date of recruitment set to NA 
  all.dbh[rec.ind,4:(min(year.ind)-1)]<-NA
  
}


  
#calculate kgC from DBHs
kgC<-calc.kgC("all.dbh",3,c(4:ncol(all.dbh)))
kgC.dates<-as.POSIXlt(as.numeric(names(kgC)[c(-1,-2,-3)]),origin="1970-1-1")


#fill gaps with linear interpolation between  
for(i in 1:nrow(kgC)){
  if(grepl("01+0",paste(as.character(as.numeric((is.na(kgC[i,-c(1:3)])))),collapse=""))){  #is there a gap?
    gaps<-gregexpr("01+0",paste(as.character(as.numeric((is.na(kgC[i,-c(1:3)])))),collapse=""))[[1]]
    for(j in 1:length(gaps)){
      gap.start<-gaps[j]+1
      gap.end<-gap.start+as.numeric(attributes(gaps)$match.length[j])-3
      kgC[i,(gap.start+3):(gap.end+3)]<-approx(as.numeric(kgC.dates[c(gap.start-1,gap.end+1)]),kgC[i,c(gap.start-1+3,gap.end+1+3)],as.numeric(kgC.dates[gap.start:gap.end]))$y
      all.dbh[i,(gap.start+3):(gap.end+3)]<-approx(as.numeric(kgC.dates[c(gap.start-1,gap.end+1)]),all.dbh[i,c(gap.start-1+3,gap.end+1+3)],as.numeric(kgC.dates[gap.start:gap.end]))$y
      
    }
  }
}
  

total.kgC<-apply(kgC[,c(-1,-2,-3)],2,function(x){sum(x,na.rm=T)}) 
kgC.by.plot<-apply(kgC[,-(1:3)],2,function(y){aggregate(y,list(kgC$plot),function(x){sum(x,na.rm=T)})$x})
kgC.by.plot<-data.frame(unique(kgC$plot),kgC.by.plot)
names(kgC.by.plot)[1]<-"plot"
names(kgC.by.plot)[-1]<-substring(names(kgC.by.plot)[-1],2)


matplot(as.POSIXct(kgC.dates),t(kgC.by.plot[,-1]),type="l",ylim=c(0,max(kgC.by.plot[,-1])),ylab="kgC",xlab="Date",main="individual plot level biomass",xaxt="n")

axis.POSIXct(1,as.POSIXct(kgC.dates))

 source("trees/s.scripts/CI.calc.txt")
MgCha.by.plot<-kgC.by.plot
MgCha.by.plot[,-1]<-(MgCha.by.plot[-1]/1000)/(100*pi/10000)

MgCha<-apply(MgCha.by.plot[,-1],2,mean)
MgCha.CI<-apply(MgCha.by.plot[,-1],2,CI.calc)


#plot mean biomass over time wiht 95% CI
#plot(kgC.dates,MgCha,type="l",ylim=range(c(MgCha+MgCha.CI,MgCha-MgCha.CI)))
#lines(kgC.dates,MgCha+MgCha.CI,lty=2)
#lines(kgC.dates,MgCha-MgCha.CI,lty=2)

#tmp<-tapply(MgCha,kgC.dates$year+1900,max,na.rm=T)
#plot(as.numeric(names(tmp)),tmp,type="o",xlab="Year",ylab="Mg C / ha", main="Above Ground Woody Biomass",cex.axis=3,cex.lab=3,cex.main=3,lwd=2,pch=16)
#need to clean up data on new recruits before we can calculate 
#annual increment/recruit/mortality

#all.dbh[,-(1:3)][all.dbh[,-(1:3)]<10&!is.na(all.dbh[,-(1:3)])]<-NA #remove all DBH values below 10 cm 




 
years<-unique(kgC.dates$year+1900)
#calculate AGWI
inc<-NULL
rec<-NULL
mort<-NULL

for(year in years[-1]){
  tmp<-kgC[,which(kgC.dates$year+1900==year)+3]
  tmp.last.year<-kgC[,which(kgC.dates$year+1900==(year-1))+3]
  present.last.year<-apply(tmp.last.year,1,function(x){any(is.na(x))})
  present.this.year<-apply(tmp,1,function(x){any(is.na(x))})   
  recruited<-apply(tmp.last.year,1,function(x){all(is.na(x))})&!apply(tmp,1,function(x){all(is.na(x))}) 
  died<-!apply(tmp.last.year,1,function(x){any(is.na(x))})&apply(tmp,1,function(x){any(is.na(x))})     
  max.tmp<-apply(tmp,1,max,na.rm=T)
  max.tmp[!is.finite(max.tmp)]<-NA
  max.tmp.last.year<-apply(tmp.last.year,1,max,na.rm=T)
  max.tmp.last.year[!is.finite(max.tmp.last.year)]<-NA
  #hist(max.tmp-max.tmp.last.year,main=year)
  inc[as.character(year)]<-(sum(max.tmp-max.tmp.last.year,na.rm=T)/1000)/(100*pi*34/10000)
  rec[as.character(year)]<-(sum(max.tmp[recruited],na.rm=T)/1000)/(100*pi*34/10000) 
  mort[as.character(year)]<-(sum(apply(cbind(max.tmp.last.year[died],max.tmp[died]),1,max,na.rm=T))/1000)/(100*pi*34/10000) 
}

#adjust recruitment to be annual rates
rec["1999"]<-rec["1999"]/6 #recruitment survey in 1999 was first since plot establishment in 1993 (6 years)
prev.rec.year<-1999
for(year in  years[-1][rec>0][-1]){
   rec[as.character((prev.rec.year+1):year)]<-rec[as.character(year)]/(year-prev.rec.year) 
   prev.rec.year<-year
}

library(gplots)

#have not yet implemented the confidence intervals for this data yet. We should 
#try and use the method Josh Benmergui worked out for his paper, rather than
#just reimplementing the old method (elg 2016)
plot.data<-rbind(inc, rec,-mort)
plot.data<-replace(plot.data,is.na(plot.data),0)
  par(mfrow = c(2, 1))
  par(mar = c(0, 4, 4, 2))
  
  tmp<-barplot(plot.data[-3, ], yaxs = "i", ylim = c(0, 3.2), las = 2,names.arg=rep("",ncol(plot.data)),col=c(3,4))
  #plotCI(tmp,plot.data[1, ]+plot.data[2,],uiw=tow.ann.stats[4:21, "pos.95CI"],add=T,type="n",gap=0)  
  par(mar = c(5, 4, 0, 2),xpd=NA)    
  #lower = plot.data[3, ]-tow.ann.stats[4:22, "neg.95CI"]
  #upper = plot.data[3, ]+tow.ann.stats[4:22, "neg.95CI"]
  barplot2(plot.data[3, ], yaxs = "i", ylim = c(-3, 0), las = 2,col=2,names.arg=c("'94-'97",1998:2016),plot.ci=T,ci.l=lower,ci.u=upper,xpd=NA, ylab="Aboveground woody increment (MgC/ha)")
  
  legend("bottom",legend=c("growth","recruitment", "mortality"),fill=c(3,4,2))




#bad.data<-apply(kgC[,-(1:3)],1,function(x){any(diff(aggregate(as.numeric(x),list(kgC.dates$year+1900),max)[,2])<(-1),na.rm=T)})
#bad.data<- bad.data | apply(kgC[,-(1:3)],1,function(x){any(diff(aggregate(as.numeric(x),list(kgC.dates$year+1900),max)[,2])>20,na.rm=T)})
#
#
#par(ask=T)
#for(i in which(bad.data)){
#  plot(kgC.dates,kgC[i,-(1:3)],type="o",main=paste(kgC[i,1:2],as.character(kgC[i,3])))
#}
#
#get.biggest <- function(x){
# tmp<-range(x,na.rm=T)
# 
#tmp[which.max(abs(tmp))]
#}
#data.range<-apply(kgC[,-(1:3)],1,function(x){get.biggest(diff(aggregate(as.numeric(x),list(kgC.dates$year+1900),max)[,2]))})
#

#barplot(rbind(plot.data[1,-1:-2],inc),names.arg=dimnames(plot.data)[[2]][-1:-2],beside=T,col=2:1,ylab="Above Ground Woody Increment (MgC/ha)",main="Tree Growth") 
#legend("topleft",legend=c("old method","new method"),fill=2:1) 
colnames(kgC)[-1:-3]<-as.character(dend.dates)
colnames(all.dbh)[-1:-3]<-as.character(dend.dates)



write.table(kgC,"trees/all.kgC.txt",sep="\t",quote=F,row.names=F)
write.table(all.dbh,"trees/all.dbh.txt",sep="\t",quote=F,row.names=F)

agwi<-cbind(as.numeric(names(inc)),inc)
colnames(agwi)<-c("Year","AGWI")
rownames(agwi)<-NULL
agwi<-data.frame(agwi,recruitment=rec,mortality=mort)
write.table(agwi,"trees/all.agwi.txt",quote=F,row.names=F)


#Calculate AGWI.ingrowth 

anpp<-data.frame(year=2000:2016,AGWI=NA,AGWI.ingrowth=NA,fine.litterfall=NA,ANPP=NA)
anpp$AGWI[anpp$year %in% agwi[,"Year"]]<-agwi[agwi[,"Year"] %in% anpp$year,"AGWI"]
#go through each year
years.rec<-names(recruit.tow)[-1] #skip 1999 since there's litterfall data that year is only leaves
prev.recruit<-"1999"
for(year in years.rec){
#pick out new recruits
#tmp<-all.dbh[splus.is.element(all.dbh[,1:2],recruit.tow[[year]][,1:2]),c(1:3,3+which(dend.dates$year+1900==as.numeric(year)))]
tmp<-all.dbh[splus.is.element(all.dbh[,1:2],recruit.tow[[year]][,1:2]),]
year.cols<-3+which(dend.dates$year+1900==as.numeric(year))
empty.ones<-apply(tmp[,year.cols],1,function(x){all(is.na(x))}) #recruits that don't have any band readings
tmp[empty.ones,year.cols[1]]<-apply(tmp[empty.ones,3+which(dend.dates$year+1900>as.numeric(year))],1,min,na.rm=T) #take the smallest band reading from after the year of recruitment
tmp<-tmp[,c(1:3,year.cols)]
tmp<-cbind(tmp,10)


#find the max kgC for that year for each recruit
#subtract off kgC of 10cm dbh tree of that species
n<-ncol(tmp)
tmp.kgC<-calc.kgC("tmp",3,4:n)

rec<-sum(apply(tmp.kgC[4:n],1,max,na.rm=T))/1000/(10^2 * pi *34) * 10000/(as.numeric(year)-as.numeric(prev.recruit))

ingrowth<-sum(apply(tmp.kgC[4:n],1,max,na.rm=T)- apply(tmp.kgC[4:n],1,min,na.rm=T))/1000/(10^2 * pi *34) * 10000
ingrowth<-ingrowth/(as.numeric(year)-as.numeric(prev.recruit)) #account for time interval since previous recruitment survey

anpp[anpp$year %in% (as.numeric(prev.recruit)+1):as.numeric(year),"AGWI.ingrowth"]<-ingrowth 
prev.recruit<-year
}


#fine litterfall
source("\\\\Eft\\data\\HF_DATA\\HF_BIOMETRY_WOFSY\\litter\\r.scripts\\lit.data.import.r")
source("\\\\Eft\\data\\HF_DATA\\HF_BIOMETRY_WOFSY\\litter\\r.scripts\\lit.sum.calc.r")

 lit.data.import()
 lit.sum.calc()

anpp[anpp$year %in% rownames(lit.sum),"fine.litterfall"] <-as.numeric(lit.sum[rownames(lit.sum)%in% anpp$year,"total"])

#calculate ANPP = AGWI +AGWI.ingrowth + fine.litterfall
anpp$ANPP<-apply(anpp[,2:4],1,sum)


#compare vs old method. need to run other script to get tow.anpp object
plot(as.numeric(rownames(tow.anpp)),tow.anpp[,"AGWI"],col=2,type="o")
lines(anpp$year,anpp$AGWI,type="o")

plot(as.numeric(rownames(tow.anpp)),tow.anpp[,"ANPP.MgC/ha"],col=2,type="o")
lines(anpp$year,anpp$ANPP,type="o")

#annual recruitment and mortality are now calculated in a standard way
#reported recruitment year are taken to be fact, even if after calculations
#they turn out to cross 10cm DBH on some other year. this is necessary because,
#otherwise a few trees start getting recruited in non-recruitment years, and 
#this makes figuring out annual rates difficult. the alternative method for 
#doing this would be to apply a universal backcalc like Josh Benmergui did to 
#find the true recruitment date for all individuals. I'm not sure if this is 
#actually better though, because we don't have good data on growth rates for 
#trees under 10cm DBH. often this method results in trees recruiting many years
#earlier than reported, which seems unlikely, unless recruitment surveys are
#very bad. (elg 2016)