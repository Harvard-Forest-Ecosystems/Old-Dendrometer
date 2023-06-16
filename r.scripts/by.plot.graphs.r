# 7/12 elg script for plotting biomass metrics for individual plots. must be run after run.all.r

metrics<-c("AGWB","AGWI","recruit","mort")
run<-1   #change this number to run the code for the different metrics above 1 is AGWB, 2 is AGWI, etc.

for (p in plots){
  tmp<-c()
  for (y in years[2:15]){
    yr<-substr(y,3,4)
    if(exists(paste("tow.",yr,".by.plot",sep=""))){
      data<-get(paste("tow.",yr,".by.plot",sep=""))
      tmp<-c(tmp,data[p,run])
    }
  }
  assign(paste(p,".",metrics[run],sep=""),tmp,pos=1)
}

par(mfrow=c(6,6))

for(p in plots){
 plot(get(paste(p,".",metrics[run],sep="")),type="l",ylab=metrics[run],xlab=p)
}





all<-matrix(NA,nrow=14,ncol=34)
for(i in 1:length(plots)){
 all[,i]<-get(paste(plots[i],".",metrics[run],sep=""))

}
par(mfrow=c(1,1))
plot(all[,1],x=years[2:15],type="n",ylim=c(min(all[,to.graph]),max(all[,to.graph])),ylab=metrics[run],xlab="year")
to.graph<-1:length(plots)  #all plots
#to.graph<-c(1:5) #change this to determine which plots get graphed
for(i in to.graph){
    lines(all[,i],x=years[2:15],col=i)
}
legend("topleft",legend=plots[to.graph],fill=to.graph)
