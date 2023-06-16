# 7/12 elg
#this script reads in the DBH data files and prepares them for plotting of individual tree's DBH over time
#run the main block of code first to read in and organize the DBH data, then use the graphing functions at the bottom to create the desired graphs

###############################
#    Main Code Block          #
###############################

#set working directory to DBH folder on EFT
setwd("//eft/data/HF_DATA/HF_BIOMETRY_WOFSY/trees/data/DBH/")

#initializes objects
years<-list.files()
dbh<-list()
dates<-list()
species<-list()



for(y in years){
  #1993 is different because there is only 1 DBH and no reference DBH column
  if(y=="1993"){
   year.n<-strtoi(y)   #numerical version of year eg 1993 instead of "1993"
    two.ch<-substr(y,3,4)   #two character version of year eg "93" instead of "1993"
   file.name<-paste("./",y,"/tow.",two.ch,".dbh.txt", sep="")  #file name to be read
    tmp<-read.table(file=file.name,header=T) #read in the file for this year
    for(i in 1:dim(tmp)[1]){
       id<-paste(tmp[i,1],tmp[i,2]) #id for each tree is "plot_id tree_id" eg "A1 1"
          dbh[[id]]<-tmp[i,4] #DBH measured in 1993
          dates[[id]]<-100 #no julian date recorded for 1993 data, I chose 100 randomly
          species[[id]]<-tmp[i,3] #stores species abbreviation for this individual
    }
  }else{
    year.n<-strtoi(y)    #numerical version of year eg 1993 instead of "1993"
    two.ch<-substr(y,3,4) #two character version of year eg "93" instead of "1993"
    file.name<-paste("./",y,"/tow.",two.ch,".dbh.txt", sep="")  #file name to be read
    tmp<-read.table(file=file.name,header=T)  #read in the file for this year
    for(i in 1:dim(tmp)[1]){
      id<-paste(tmp[i,1],tmp[i,2])   #id for each tree is "plot_id tree_id" eg "A1 1"
      if(id %in% names(dbh)){ #if DBHs have aleady been recorded for this tree in previous years
          dbh[[id]]<-c(dbh[[id]],tmp[i,5:length(tmp)]) #append the DBHs for this year to the previous years
          dates[[id]]<-c(dates[[id]],strtoi(substr(names(tmp)[5:length(names(tmp))],6,9))+365*(year.n-1993)) #and append this years julian dates + however many days have passed since 1993 (this makes it so that all the data plots on a single graph)
      }else{  #if this is the first year there are DBH measurements for this individual
        dbh[[id]]<-tmp[i,5:length(tmp)]  #add DBH for this individual as a new element to dbh
        dates[[id]]<-strtoi(substr(names(tmp)[5:length(names(tmp))],6,9))+365*(year.n-1993)) #add this years julian dates as new element to dates + however many days have passed since 1993 (this makes it so that all the data plots on a single graph)
        species[[id]]<-tmp[i,3] #stores species abbreviation for this individual
      }
    }
  }
}

#######################################
#   Graphing Functions                #
#######################################

#function for plotting DBH of an individual tree over time.
#this function can take either the id, eg "A1 1" of the indivual or it's index of dbh, eg 1
plot.tree<-function(id){
  plot(dates[[id]],dbh[[id]], xlab="Days since Jan 1 2003",ylab="DBH(cm)",main=paste(names(dbh)[id]," (",species[[id]],")",sep=""))
}

#function for plotting multiple trees on the same graph
#this function takes in a vector of ids (eg c("A1 1","A2 6", "B4 1")) or a vector of numbers (eg c(1,13,54))
plot.trees<-function(ids){
 mina<-100  #initializes the minimum DBH variable
 maxa<-0    #initializes the maximum DBH variable
 leg<-c()
 for(id in ids){
    maxa<-max(maxa,max(unlist(dbh[[id]]))) #finds the maximum DBH value for setting scale of the plot
    mina<-min(mina,(unlist(dbh[[id]])))    #finds the minimum DBH value for setting the scale of the plot
    leg<-c(leg,paste(names(dbh)[id],species[[id]])) #saves the species names of these individuals for the legend
 }
 plot(dates[[ids[1]]],dbh[[ids[1]]], xlab="Days since Jan 1 2003",ylab="DBH(cm)",ylim=c(mina,maxa),pch=1,col=1)
 for(i in 2:length(ids)){
    points(dates[[ids[i]]],dbh[[ids[i]]],pch=i,col=i)
  }
  
  
  legend("bottomleft",leg,pch=1:length(ids),col=1:length(ids),bg="white")  #legend here cuts off 1993 data point, but can be moved
}

#plots all the trees of a single species
#this function takes in a string abreviation of the species name (eg "rm", "bo", or "hem")
#warning: there are a lot of trees for some species and the graphs may be messy
plot.species<-function(sp){
 plot.trees(which(unlist(species)==sp))
}

#this confusingly named function plots all the trees in a single plot
#this function takes in the plot name (eg "A1", "B4", or "H5")
plot.plot<-function(pl){
 plot.trees(which(substr(names(dbh),1,2)==pl))
}
