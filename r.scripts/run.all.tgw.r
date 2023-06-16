# 7/12 elg converted scripts to run in R and reduced some redundancy of code by writing a single tow.x.calc.r 
# script that can be run for any year. When new years of data are added, the script will automatically include
# them, but tow.sum.calc.R,tow.stats.calc.R, and  anpp.calc.R must be updated
locale <- readline(prompt="Running on eft or local machine? (e/l)")
# locale <- "l"
# read in the data from eft.as.harvard.edu
if(locale == "e"){
  if(Sys.info()[["nodename"]]=="eft"){
    setwd("/DATA/HF_DATA/HF_BIOMETRY_WOFSY/")
  }else{
    setwd("//EFT/DATA/HF_DATA/HF_BIOMETRY_WOFSY/")
  }
  setwd(dir)
  #install required packages if not in library already
  if(sum(grepl("dplyr", installed.packages()[,1]))==0){
    install.packages("tidyverse")
  }
  if(sum(grepl("lubridate", installed.packages()[,1]))==0){
    install.packages("lubridate")
  }
}else if(locale == "l"){
  #TGW local copy
  if(Sys.info()[["nodename"]] == "BURT") {
    setwd("D:/Desktop/Google Drive/Whitby HF files/HF_BIOMETRY_WOFSY copy")
  } else {
    setwd("G:/My Drive/Whitby HF Files/HF_BIOMETRY_WOFSY copy")
  }
}

getwd()

source("R.scripts/old.functions.r")
source("trees/r.scripts/tow.x.calc.r")
source("trees/r.scripts/tow.93.calc.r")
source("trees/s.scripts/CI.calc.txt")

years<-list.files("trees/data/dendrometer", pattern = "\\d{4}")

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

#look at tow.17.kgC for errors, uncharacteristically low AGWI
# tow.17.kgC$inc <- apply(tow.17.kgC[, 5:8], 1, max) - tow.17.kgC$ref.dbh
# summary(tow.17.kgC$inc )
# neg.inc <- tow.17.kgC[tow.17.kgC$inc < 0, ]


#litter data processing (for ANPP calculations)
source("litter/r.scripts/lit.data.import.r")
source("litter/r.scripts/lit.sum.calc.r")
source("litter/r.scripts/lit.stats.r")
lit.data.import()
lit.sum.calc()
lit.stats()


source("trees/r.scripts/anpp.calc.r")
write.csv(tow.anpp, "trees/tow.anpp.old.csv", quote=F, row.names=T)
write.csv(tow.ann.sum, "trees/tow.ann.sum.old.csv", quote=F, row.names=T)

#re-run to make new calculations of AGWI and ANPP with the max biomass in prev year as reference
source("trees/r.scripts/tow.sum.calc.17.r")
source("trees/r.scripts/tow.stats.calc.r")
source("trees/r.scripts/anpp.calc.r")
write.csv(tow.anpp, "trees/tow.anpp.new.csv", quote=F, row.names=T)
write.csv(tow.ann.sum, "trees/tow.ann.sum.new.csv", quote=F, row.names=T)

#run the calculations for all the cut plots
for (y in cut.years){
  tow.x.calc(y,"cut")
}

source("trees/r.scripts/cut.back.calc.r")
source("trees/r.scripts/cut.sum.calc.r")
source("trees/r.scripts/cut.stats.calc.r")

