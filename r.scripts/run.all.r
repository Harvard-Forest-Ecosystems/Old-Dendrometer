# 7/12 elg converted scripts to run in R and reduced some redundancy of code by writing a single tow.x.calc.r 
# script that can be run for any year. When new years of data are added, the script will automatically include
# them, but tow.sum.calc.R,tow.stats.calc.R, and  anpp.calc.R must be updated
if(Sys.info()[["nodename"]]=="eft"){
  dir<-"/DATA/HF_DATA/HF_BIOMETRY_WOFSY/"
}else{
  dir<-"//eft.as.harvard.edu/DATA/HF_DATA/HF_BIOMETRY_WOFSY/"
}

setwd(dir)

source("R.scripts/old.functions.r")
source("trees/r.scripts/tow.x.calc.r")
source("trees/r.scripts/tow.93.calc.r")

years<-list.files("trees/data/dendrometer")

#get mortality and recruitment data 
recruit.tow<-list()
dead.tow<-list()
for(year in years){
  yr<-substr(year,3,4)
  fl<-paste("trees/data/dendrometer/",year,"/tow.",yr,".mort.txt",sep="")
  if(file.exists(fl)){
    tmp<-read.table(file=fl,header=T)
    dead.tow[[yr]]<-tmp
  }
  fl<-paste("trees/data/dendrometer/",year,"/tow.",yr,".recruits.txt",sep="")
   if(file.exists(fl)){
    tmp<-read.table(file=fl,header=T)
    recruit.tow[[yr]]<-tmp
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
    dead.cut[[yr]]<-tmp
  }
  fl<-paste("trees/data/dendrometer/",year,"/cut.",yr,".recruits.txt",sep="")
   if(file.exists(fl)){
    tmp<-read.table(file=fl,header=T)
    recruit.cut[[yr]]<-tmp
  }
}


#run calculations for all the tower plots
for(y in years){
  if(y=="1993"){
    tow.93.calc()
  }else{
    tow.x.calc(y)
  }
}

source("trees/r.scripts/tow.back.calc.r")
source("trees/r.scripts/tow.sum.calc.r")
source("trees/r.scripts/tow.stats.calc.r")

#litter data processing (for ANPP calculations)
source("litter/r.scripts/lit.data.import.r")
source("litter/r.scripts/lit.stats.r")
source("litter/r.scripts/lit.sum.calc.r")
lit.data.import()
lit.sum.calc()
lit.stats()


source("trees/r.scripts/anpp.calc.r")



#run the calculations for all the cut plots
for (y in cut.years){
  tow.x.calc(y,"cut")
}

source("trees/r.scripts/cut.back.calc.r")
source("trees/r.scripts/cut.sum.calc.r")
source("trees/r.scripts/cut.stats.calc.r")
