	# 1/12 lkw
	# 6/12 elg converted script to R and made more scalable by allowing all years to be processed by a single, flexible script 
	# except for 1993 which has data in a different format (just DBH, no dendro) and so needs to have its own processing script
	# There are a few cases where certain years need to be processed slightly differently for various reasons, and those cases are
  # handled in this script appropriately by if statements such as: if(yr=="01"&group=="cut") 
	#
  # calc dbhs then kgC from any years dendro data from either Old40 plots or bradys 
	
	# year is the full year as either an integer or a string. e.g. "2004"
	# group should be either "tow" or "cut" depending on if you are processing data from tower plots or the cut plots 
	tow.x.calc <- function(year,group="tow"){
    yr<-substr(year,3,4)
	 	# read in dendrometer data from eft
	
	  fl<-paste("trees/data/dendrometer/",year,"/",group,".",yr,".dend.txt",sep="")
    if(file.exists(fl)){
      dend <- read.table(fl, header = T, sep = "\t", row.names = NULL, as.is=T)
     }else{
      return()
     }
              #remove extra column from cut.01.dend
              if(yr=="01"&group=="cut"){
                  dend<- dend[,c(1:3,5:dim(dend)[2])]
              }
    
    # convert dendro measurements to dbhs using the subroutine, new.dbh.make
 	  # make a dummy object to fill w/ the ref #s (ref.dbh & ref.dend) for each date, which are required by new.dbh.make
  	dummy <- cbind(dend[, 1:6], matrix(NA, length(dend[, 1]), ncol(dend[, 7:ncol(dend)]) * 3))
    num.obs<-ncol(dend)-4   #number of observations
    junk <- seq(6, num.obs*3, 3)
    dimnames(dummy)[[2]][junk] <- dimnames(dend)[[2]][6:ncol(dend)]
    # read in the object that contains corrected ref #s for when new bands were installed or old bands were modified
    # but only if there is a file for the corrected ref #s. 1999 cut for example is missing this file.  
    ref.dir<-paste("trees/data/dendrometer/",year,"/",group,".",yr,".ref.txt",sep="")
    if(file.exists(ref.dir)){
      dummy.corr <- read.table(ref.dir, header = T, sep = "\t", row.names = NULL, as.is = T)
       # remove extra column from cut.01.ref.txt
       if(yr=="01"&group=="cut"){
        dummy.corr<-dummy.corr[,c(1:3,5:54)]
       }
      dummy[,1:6]<-dummy.corr[,1:6]
      ref.file.exists<-TRUE
    }else{
     ref.file.exists<-FALSE
    }
               
    # make an object to contain the calculated dbhs
    DBH <- cbind(dend[, c(1:4)], matrix(NA, length(dend[, 1]), ncol(dend[, 6:ncol(dend)])))
    dimnames(DBH)[[2]][5:ncol(DBH)] <- dimnames(dend)[[2]][6:ncol(dend)]
    # make sure you're using the correct version of new.dbh.make
    source("trees/r.scripts/new.dbh.make.r")
    # then apply the fnctn
    for(i in 1:length(junk)) { 
      # distribute dendro measurements into every 3rd column of the dummy obj to make room for ref columns
      dummy[, junk[i]] <- dend[, i + 5]
      # calc dbhs and fill ref dbh collumns w/ the previous dates' dbhs
      dummy[, junk[i] + 1] <- new.dbh.make(dummy[, junk[i]], dummy[, junk[i] - 2], dummy[, junk[i] - 1])
      # put the dbhs in a seperate object
      
      # fill ref dend columns w/ the previous dates' dendro measurements
      dummy[, junk[i] + 2] <- dummy[, junk[i]]
      
      DBH[, i + 4] <- dummy[, junk[i] + 1]
      # replace ref #s w/ corrected ref #s 
      if(ref.file.exists){
        corr <- !is.na(dummy.corr[,junk[i]+1])
        dummy[corr, junk[i] + 1] <- dummy.corr[corr, junk[i] + 1]
        corr <- !is.na(dummy.corr[, junk[i] + 2])
        dummy[corr, junk[i] + 2] <- dummy.corr[corr, junk[i] + 2]   
      }
      
    }
	    
            		# for 1998 screen out trees that were less than 10cm DBH in 1993 since they should be in the 1999 recruitment survey
                if(yr=="98"){
                  DBH<-DBH[DBH[,1]!="B4"|DBH[,2]!=264,]
                  DBH<-DBH[DBH[,1]!="D2"|DBH[,2]!=579,]
                  DBH<-DBH[DBH[,1]!="F2"|DBH[,2]!=978,]
                }
                
                #screen out trees with dbh <10cm and assigns DBH to a global environment variable so that calc.kgC can access it
                #the brady plots have a 5 cm cutoff
                
                if(group=="tow"){
                  DBH<<-DBH[DBH[,4]>=10|is.na(DBH[,4]),] 
                }else if(group=="cut"){
                  DBH<<-DBH[DBH[,4]>=5|is.na(DBH[,4]),]
                }                
            	

    # calc the kg C of each tree using the subroutine, calc.kgC
    # make sure you're using the correct version of calc.kgC
    source("trees/r.scripts/calc.kgC.r")
    kgC <- calc.kgC("DBH", 3, 4:ncol(DBH))
    #-------------------------------------------------------
    # take the trees that died this year out of the dbh & kgC data
    # first list the plot numbers then the tag numbers of the dead trees
	
    dead<-get(paste("dead.",group,sep=""))[[yr]]
    # locate the dead trees in the date
    junk<-splus.is.element(DBH[,1:2],dead[,1:2])  #
    # put the dead trees in a seperate object
    mort.dbh <- DBH[junk,  ]
    # pull dead trees out of the data 
    DBH <- DBH[!junk,  ]
    # do the same thing to the kgC data
    junk<-splus.is.element(kgC[,1:2],dead[,1:2])
    mort.kgC <- kgC[junk,  ]
    kgC <- kgC[!junk,  ]

                # in 1998 take the tree that was already dead in 93 out of the mort data (C4 486)
                if(yr=="98"&group=="tow"){
                  mort.dbh<-mort.dbh[mort.dbh[,1]!="C4"|mort.dbh[,2]!=486,]
                  mort.kgC<-mort.kgC[mort.kgC[,1]!="C4"|mort.kgC[,2]!=486,]
                }else if (yr=="99"&group=="tow"){
                #in 1999 take out the trees that were already dead in 1998 (C4 455 & H2 335)
                  mort.dbh<-mort.dbh[c(1:2,4:21),]
                  mort.kgC<-mort.kgC[c(1:2,4:21),]
                }
	# take this year's recruits out of the dbh & kgC data              
  # and put them in a separate object for separate quantification
	# these trees won't be included in the woody incrememnt or total biomass until next year
	recruit.all<-get(paste("recruit.",group,sep=""))
    if(yr %in% names(recruit.all)){
      recruit<-get(paste("recruit.",group,sep=""))[[yr]]
      # locate the recruited trees in the dbh data
      junk<- splus.is.element(DBH[,1:2],recruit[,1:2])
      # put them in a seperate object
      rcrt.dbh <- DBH[junk,  ]
      # then pull them out of the dbh data 
      DBH <- DBH[!junk,  ]
      # do the same for the kgC data
      junk<-splus.is.element(kgC[,1:2],recruit[,1:2])
      rcrt.kgC <- kgC[junk,  ]
      gC <- kgC[!junk,  ]
      
      assign(paste(group,".",yr,".rcrt.dbh",sep=""),rcrt.dbh,pos=1)
      assign(paste(group,".",yr,".rcrt.kgC",sep=""),rcrt.kgC,pos=1)
    }
                #In 2003 the radius of the cut plots were extended to 15m, so the recruits from this year are split between
                #the original 10m radius and the outer 5m for accounting purposes.
                if(yr=="03"&group=="cut"){
                  recruit<-data.frame(plot=c("A4", "A4", "A4", "A4", "A4", "B5", "B5", "Cindy", "Cindy", "Peter"),tag=c(92, 93, 94, 95, 96, 100, 577, 30, 79, 190))
                  # locate the recruited trees in the data
                  junk <- splus.is.element(DBH[, 1:2], recruit[, 1:2])
                  # put them in a seperate object
                  cut.03.rcrt.dbh.in <- DBH[junk,  ]
                  # then pull them out of the data 
                  DBH <- DBH[!junk,  ]
                  # do the same thing to w/ the kgC data
                  junk <- splus.is.element(kgC[, 1:2], recruit[, 1:2])
                  cut.03.rcrt.kgC.in <- kgC[junk,  ]
                  kgC <- kgC[!junk,  ]
                  # do the same for the recruits in the outer 5 m ring
                  recruit<-data.frame(plot=c("A5", "A5", "A5", "B5", "B5", "B5", "Cindy", "Cindy", "Cindy", "Marsha", "Marsha", "Marsha", "Marsha", "Marsha", "Marsha", "Marsha", "Marsha", "Peter"),tag=c(578, 579, 580, 97, 295, 549, 36, 78, 80, 7, 10, 20, 26, 27, 28, 29, 91, 93))
                  junk <- splus.is.element(DBH[, 1:2], recruit[, 1:2])
                  cut.03.rcrt.dbh.out <- DBH[junk,  ]
                  DBH <- DBH[!junk,  ]
                  junk <- splus.is.element(kgC[, 1:2], recruit[, 1:2])
                  cut.03.rcrt.kgC.out <- kgC[junk,  ]
                  kgC <- kgC[!junk,  ]
                  
                  assign("cut.03.rcrt.dbh.in", cut.03.rcrt.dbh.in, pos=1)
                  assign("cut.03.rcrt.dbh.out", cut.03.rcrt.dbh.out, pos=1)
                  assign("cut.03.rcrt.kgC.in", cut.03.rcrt.kgC.in, pos=1)
                  assign("cut.03.rcrt.kgC.out", cut.03.rcrt.kgC.out, pos=1)
                  
                }
                # some trees were thought to have been lost in the harvest in 2001, but were found alive in 2002
                # change these trees' tag #s to the #s they were later re-labeled w/ in 2002
                # Marsha 505 and Marsha 512 were also found to still be alive, but are under 10 cm so are already removed from the data
                if((yr=="99"|yr=="00"|yr=="01")&group=="cut"){
                  change<-data.frame(plot=c("A4", "A4", "A4", "A4"),tag=c(76, 77, 80, 89))
                  junk<- splus.is.element(DBH[,1:2],change[,1:2])
                  DBH[junk, 2] <- c(141, 141.5, 142, 140)
                  DBH <- DBH[order(DBH[, 1], DBH[, 2]),  ]
                  junk2 <- paste(kgC[,1],kgC[,2]) %in% paste(change[,1],change[,2])
                  junk<-splus.is.element(kgC[,1:2],change[,1:2])
                  kgC[junk, 2] <- c(141, 141.5, 142, 140)
                  kgC <- kgC[order(kgC[, 1], kgC[, 2]),  ]
                }
             
    #-----------------------------------
    assign(paste(group,".",yr,".dbh",sep=""), DBH,pos=1)
    assign(paste(group,".",yr,".kgC",sep=""), kgC,pos=1)
    assign(paste(group,".",yr,".mort.dbh",sep=""), mort.dbh,pos=1)
    assign(paste(group,".",yr,".mort.kgC",sep=""), mort.kgC,pos=1)

  }
