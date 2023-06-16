library(tidyverse)
library(lubridate)
library(zoo)
setwd("../data/dendrometer/2021")
tow.21 <- bind_rows(read.csv("tow.21.132.dend.csv", header = T, na.strings = "NA"),
  read.csv("tow.21.182.dend.csv", header = T, na.strings = "NA"),
  read.csv("tow.21.236.dend.csv", header = T, na.strings = "NA"),
  read.csv("tow.21.287.dend.csv", header = T, na.strings = "NA"))

dend.all.20 <- read.table("../../ems.dend.98.20.txt", sep = "\t", header = T, as.is = T)
dbh.all.20 <- read.table("../../ems.dbh.98.20.txt", sep = "\t", header = T, as.is = T)


# remove bands with NA dend.meas if they have another valid band
trees <- unique(tow.21[, c("plot", "tag", "jday")])
for(i in 1:nrow(trees)){
  tree.obs <- tow.21[tow.21$plot == trees[i,1] & tow.21$tag == trees[i,2] & tow.21$jday == trees[i,3], ]
  if(nrow(tree.obs) > 1){
    if(sum(is.na(tree.obs$dend)) < nrow(tree.obs)){
      tow.21<- tow.21[!(tow.21$plot == trees[i,1] & tow.21$tag == trees[i,2] &  tow.21$jday == trees[i,3] & is.na(tow.21$dend)), ]
    }
  }
}

names(tow.21)
str(tow.21)

dend.data.21 <- tow.21 %>% select(plot, tag, jday, year, dend.num, dend.meas = dend) %>% 
  arrange(plot, tag) %>% 
  filter(!is.na(dend.meas))
any(duplicated(dend.data.21[ , c(1:3,5)]))
# should be FALSE
dend.data.21[duplicated(dend.data.21[ , c(1:3,5)]), ]

dend.all.21 <- rbind(dend.all.20, dend.data.21)
write.table(dend.all.21, "../../ems.dend.98.21.txt", sep = "\t", quote = F, row.names = F)

dbh.data.21 <- tow.21 %>% select(plot, tag, jday, year, DBH = dbh) %>% 
  mutate(plot.tag.day = paste(plot, tag, jday, sep = "-")) %>% 
  filter(!is.na(DBH), !duplicated(plot.tag.day)) %>% 
  select(-plot.tag.day)

any(duplicated(dbh.data.21))

dbh.all.21 <- rbind(dbh.all.20, dbh.data.21)
write.table(dbh.all.21, "../../ems.dbh.98.21.txt", sep = "\t", quote = F, row.names = F)

dend.dbh.21 <- tow.21 %>% select(plot, tag, spp, jday, year, dend.num, dend.meas = dend, DBH = dbh) %>% 
  arrange(plot,tag)

spp.data.21 <- tow.21 %>% select(plot, tag, spp) %>% 
  mutate(plot.tag = interaction(plot, tag)) %>% 
  filter(!duplicated(plot.tag)) %>% 
  select(-plot.tag)

# Create ref DBH according to old rules ----------------------------------------------------------------------
# commented out all following code since it only needs to run once
# uses previous year's last dend measurement as reference (may be sensitive to timing of last measurement)

tow.21.ref.tmp <- tow.21 %>%
  select(plot, tag, spp, jday, year, dend.num, ref.dend, ref.dbh, dend.meas = dend, DBH = dbh) %>%
  mutate(ref.dbh = NA, ref.dend = NA) %>% # clear ref.dend and ref.dbh which are not from last obs of previous year
  group_by(plot, tag, dend.num) %>%
  gather(var, obs, dend.meas, DBH, ref.dend, ref.dbh) %>%
  unite(tmp, jday, var) %>%
  spread(tmp, obs) %>%
  rename(jday.132 = '132_dend.meas', jday.132.DBH = '132_DBH', ref.dend = '132_ref.dend', ref.dbh = '132_ref.dbh',
    jday.182 = '182_dend.meas', ref.dbh.182 = '182_DBH',
    jday.236 = '236_dend.meas', ref.dbh.236 = '236_DBH', 
    jday.287 = '287_dend.meas', ref.dbh.287 = '287_DBH') %>%
  ungroup() %>% 
  mutate(ID = paste(plot, tag, dend.num, sep = "-"),
    # overwrite ref.dend and ref.dbh if new DBH taken on first day of year
    ref.dend = ifelse(!is.na(jday.132) & !is.na(jday.132.DBH), jday.132, ref.dend),
    ref.dbh = ifelse(!is.na(jday.132) & !is.na(jday.132.DBH), jday.132.DBH, ref.dbh),
    ref.dend.182 =  ifelse(!is.na(jday.182) & !is.na(ref.dbh.182), jday.182, NA),
    ref.dend.236 =  ifelse(!is.na(jday.236) & !is.na(ref.dbh.236), jday.236, NA),
    ref.dend.287 =  ifelse(!is.na(jday.287) & !is.na(ref.dbh.287), jday.287, NA),
    plot.tag = paste(plot, tag, sep = "-")) %>%
  select(plot.tag, plot, tag, ID, spp, dend.num,  ref.dbh, ref.dend, jday.132,
    ref.dbh.182, ref.dend.182, jday.182,
    ref.dbh.236, ref.dend.236, jday.236,
    ref.dbh.287, ref.dend.287, jday.287)


# create reference that is all current plot, tag, dend.num, ID combinations
trees.21 <- tow.21.ref.tmp %>% select(plot, tag, dend.num, ID)

setwd("../../../../")
getwd()
tow.20.ref <- read.table("trees/data/dendrometer/2020/tow.20.ref.txt", header = T, sep = "\t")

source("R.scripts/old.functions.r")
source("trees/r.scripts/tgw 2017/dbh.fcn.tgw.r")
source("trees/r.scripts/tow.x.calc.r")

# get mortality and recruitment data (from run.all.r)
years<-list.files("trees/data/dendrometer", pattern = "\\d{4}")
years <- as.numeric(years)
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

dend.dbh.all.21 <- left_join(dend.all.21, dbh.all.21)
tow.x.calc("2020")

tow.20.all.dbh <- bind_rows(tow.20.dbh, tow.20.rcrt.dbh, tow.20.mort.dbh)
dend.dbh.20.327 <- dend.dbh.all.21 %>% filter(year == 2020 & jday == 327)
tow.20.last <- dend.dbh.20.327[ , c("plot", "tag", "dend.num", "dend.meas", "DBH")] %>%
  rename("ref.dend" = "dend.meas", "dbh.327" = "DBH") %>%
  left_join(tow.20.all.dbh[ , c(1,2,9)]) %>%
  rename("ref.dbh" = "jday.327") %>%
  right_join(trees.21) %>% 
  arrange(ID)

summary(tow.20.last)

any(duplicated(tow.20.last))

# create df with most recent dend-dbh pair for reference up to 2021
refs.21 <- dend.dbh.all.21 %>% group_by(plot, tag, dend.num) %>%
  mutate(date = as_date(paste0(year, jday), format = "%Y%j"),
    ID = paste(plot, tag, dend.num, sep = "-")) %>%
  filter(!is.na(DBH)) %>% # have to do separate filter calls to do in this squence
  filter(date == max(date)) %>%
  rename(ref.dend = dend.meas, ref.dbh = DBH, ref.date = date) %>%
  select(plot, tag, dend.num, ID, ref.date, ref.dend, ref.dbh) %>%
  right_join(trees.21) %>% 
  arrange(ID)

# Fill 2020 trees without ref values with 2021 data if available
missing.ID <- tow.20.last$ID[is.na(tow.20.last$ref.dbh)]
refs.21.to.fill <- refs.21 %>% filter(ID %in% missing.ID)
# Not all missing refs in 2020 can be filled with 2021 data, create fill ID from matching trees
fill.ID.21 <- refs.21.to.fill$ID[!is.na(refs.21.to.fill$ref.date)]


tow.20.last[which(tow.20.last$ID %in% missing.ID), c("ID", "ref.dend", "ref.dbh")]   
refs.21.to.fill[which(refs.21.to.fill$ID %in% fill.ID.21) , c("ID", "ref.dend", "ref.dbh")]

tow.20.last[is.na(tow.20.last$ref.dend), ]

tow.20.last <- tow.20.last %>%
  group_by(plot, tag) %>%
  arrange(plot, tag, dend.num) %>% 
  # some new double bands don't have a measured DBH yet, fill from older bands calculated DBH where possible
  mutate(ref.dbh = na.locf(ref.dbh, na.rm = F)) %>%
  select(plot, tag, dend.num, ID, ref.dend.20.327 = ref.dend, ref.dbh.20.327 = ref.dbh)

summary(tow.21.ref.tmp)
tow.21.ref.tmp <- left_join(tow.21.ref.tmp, tow.20.last[,c("ID", "ref.dend.20.327", "ref.dbh.20.327")]) %>%
  mutate(ref.dbh = ifelse(is.na(ref.dbh), ref.dbh.20.327, ref.dbh),
    ref.dend = ifelse(is.na(ref.dend), ref.dend.20.327, ref.dend)) %>%
  select(-contains('.327'), -ID)

tow.21.ref.tmp[is.na(tow.21.ref.tmp$ref.dend),]
# dend matches except for recruits
# will dupliacte values for double bands

# Filter duplicate dendrometer bands and only keep one with complete data
# -----------------------------------------------------------------------------------------------------------

dup <- tow.21.ref.tmp[duplicated(tow.21.ref.tmp[ ,"plot.tag"]), ]
tow.21.ref.dup <- tow.21.ref.tmp[tow.21.ref.tmp$plot.tag %in% dup$plot.tag, ] %>%
  arrange(plot.tag, dend.num)
trees <- unique(tow.21.ref.dup$plot.tag)

# loop requires that file is sorted ascending plot.tag & dend.num
tmp.trees <- list()
overlap <- as.character()

# t <- "B3-212"
# a = 0
for (t in trees){
  bnds <- tow.21.ref.dup[tow.21.ref.dup$plot.tag == t, ]
  #### need to update column nums after each dend measurement ####
  keep <- apply(bnds, 1, function(x){!any(is.na(x[c(6,7,8,11,14,17)]))})
  # if only one band is complete, keep it
  if (sum(keep) == 1){
    tmp.row <- bnds[keep, ]
    # if all rows have all dendrometer measurements, keep the oldest one
  } else if (sum(keep) == length(keep)){
    tmp.row <- bnds[keep & bnds$dend.num == min(bnds$dend.num), ]
    
  }else if (sum(keep) == 0){
    # if neither band has all the measurements, overwrite old band with new, where new band !is.na
    # give warning for trees with 3 or more bands
    if(length(keep) > 2){
      warning(paste("more than two bands for", bnds$plot.tag, "Need to review manually. \n"))
      tmp.row <- bnds
    } else {
      warning(paste("can't automate band switching for", bnds$plot.tag,"Need to correct manually for switching bands"))
      overlap <- c(overlap, t)
    }
  }
  if(exists("tmp.row")){
    tmp.trees[[t]] <- tmp.row
  }
  rm(tmp.row)
}
tmp.trees <- bind_rows(tmp.trees)
tmp.trees[duplicated(tmp.trees), ]
overlap
# make sure transition to new band has ref.dend when switching to new
overlap.trees <- tow.21.ref.tmp[tow.21.ref.tmp$plot.tag %in% overlap, ] %>% arrange(plot.tag, dend.num)

# keep only first row for each duplicate tree tag
# overlap.trees <- overlap.trees[c(1,3,5,7,9,11), ]

# combine all the non-duplicated trees and remove dend.num category
tow.21.ref <- bind_rows(tow.21.ref.tmp[!tow.21.ref.tmp$plot.tag %in% dup$plot.tag, ],
  tmp.trees[!tmp.trees$plot.tag %in% overlap, ],
  overlap.trees) %>%
  ungroup() %>%
  mutate(ref.dbh.xxx = NA,
    ref.dend.xxx = NA) %>%
  select(-plot.tag, -dend.num) %>%
  arrange(plot, tag)

# new recruits, copy ref.dbh.256 to ref.dbh (these are the only ones with NA for ref.dbh)
# tow.21.ref$ref.dbh[is.na(tow.21.ref$ref.dbh)] <- tow.21.ref$ref.dbh.256[is.na(tow.21.ref$ref.dbh)]


# Look at ratio of first dend meas of year and ref to see if they are matched correclty
tow.21.ref.check <- tow.21.ref %>% mutate(ref.check = round((ref.dend - jday.132)/jday.132, 4))
plot(tow.21.ref.check$ref.check)
# check that all trees are still present
all.equal(sort(unique(tow.21.ref.tmp$plot.tag)),  sort(unique(paste(tow.21.ref$plot, tow.21.ref$tag, sep = "-"))))
tow.21.ref.tmp$plot.tag[!unique(tow.21.ref.tmp$plot.tag) %in% unique(paste(tow.21.ref$plot, tow.21.ref$tag, sep = "-"))]

setwd("trees/data/dendrometer/2021")

# remove trees that died preivious year if they were still in data
dead.last <- interaction(tow.21.ref$plot, tow.21.ref$tag) %in% 
  interaction(tow.20.mort.dbh$plot, tow.20.mort.dbh$tag)
sum(dead.last)
tow.21.ref <- tow.21.ref[!dead.last, ]
write.table(tow.21.ref, "tow.21.ref.txt", sep = "\t", quote = F, row.names = F)

# read in file has manually filled in ref.dbh and ref.dend for recruits
# as well as back filling the first dendrometer measurement to the day of first DBH record
# and added two extra columns for tow.x.calc to work, 'ref.dend.xxx', 'ref.dbh.xxx'
# this will need to be updated manually if tow.21.ref.tmp changes
# tow.21.ref.tmp2 <- read.table("tow.21.ref.tmp2.txt", header=T) %>% arrange(plot, tag)

# create dend.text from final ref.txt
#### need to manually update col nums with additional measurements ####
tow.21.dend.txt <- tow.21.ref[,c(1:5, 6, 9, 12, 15)]
write.table(tow.21.dend.txt, "tow.21.dend.txt", sep = "\t", quote = F, row.names = F)
#check for any duplicates in ref file
tow.21.ref[(duplicated(interaction(tow.21.ref$plot, tow.21.ref$tag))), ]

