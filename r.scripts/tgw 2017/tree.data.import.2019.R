library(tidyverse)
library(lubridate)
library(zoo)
setwd("C:/Users/tgw475/Desktop/Whitby HF Files/HF_BIOMETRY_WOFSY copy/trees/data/dendrometer/2019")
tow.19 <- bind_rows(read.csv("tow.19.135.dend.csv", header = T, na.strings = "NA"),
  read.csv("tow.19.169.dend.csv", header = T, na.strings = "NA"),
  read.csv("tow.19.200.dend.csv", header = T, na.strings = "NA"),
  read.csv("tow.19.241.dend.csv", header = T, na.strings = "NA"),
  read.csv("tow.19.296.dend.csv", header = T, na.strings = "NA")) 

dend.all.18 <- read.table("../../ems.dend.98.18.txt", sep = "\t", header = T, as.is = T)
dbh.all.18 <- read.table("../../ems.dbh.98.18.txt", sep = "\t", header = T, as.is = T)


# remove bands with NA dend.meas if they have another valid band
trees <- unique(tow.19[, c("plot", "tag", "jday")])
for(i in 1:nrow(trees)){
  tree.obs <- tow.19[tow.19$plot == trees[i,1] & tow.19$tag == trees[i,2] & tow.19$jday == trees[i,3], ]
  if(nrow(tree.obs) > 1){
    if(sum(is.na(tree.obs$dend)) < nrow(tree.obs)){
      tow.19<- tow.19[!(tow.19$plot == trees[i,1] & tow.19$tag == trees[i,2] &  tow.19$jday == trees[i,3] & is.na(tow.19$dend)), ]
    }
  }
}

names(tow.19)
str(tow.19)

dend.data.19 <- tow.19 %>% select(plot, tag, jday, year, dend.num, dend.meas = dend) %>% 
  arrange(plot, tag) %>% 
  filter(!is.na(dend.meas))
any(duplicated(dend.data.19[ , c(1:3,5)]))
dend.data.19[duplicated(dend.data.19[ , c(1:3,5)]), ]

dend.all.19 <- rbind(dend.all.18, dend.data.19)
write.table(dend.all.19, "../../ems.dend.98.19.txt", sep = "\t", quote = F, row.names = F)

dbh.data.19 <- tow.19 %>% select(plot, tag, jday, year, DBH = dbh) %>% 
  mutate(plot.tag.day = paste(plot, tag, jday, sep = "-")) %>% 
  filter(!is.na(DBH), !duplicated(plot.tag.day)) %>% 
  select(-plot.tag.day)

any(duplicated(dbh.data.19))

dbh.all.19 <- rbind(dbh.all.18, dbh.data.19)
write.table(dbh.all.19, "../../ems.dbh.98.19.txt", sep = "\t", quote = F, row.names = F)

dend.dbh.19 <- tow.19 %>% select(plot, tag, spp, jday, year, dend.num, dend.meas = dend, DBH = dbh) %>% 
  arrange(plot,tag)


spp.data.19 <- tow.19 %>% select(plot, tag, spp) %>% 
  mutate(plot.tag = interaction(plot, tag)) %>% 
  filter(!duplicated(plot.tag)) %>% 
  select(-plot.tag)

# Create ref DBH according to old rules ----------------------------------------------------------------------
# commented out all following code since it only needs to run once
# uses previous year's last dend measurement as reference (may be sensitive to timing of last measurement)

tow.19.ref.tmp <- tow.19 %>%
  select(plot, tag, spp, jday, year, dend.num, ref.dend, ref.dbh, dend.meas = dend, DBH = dbh) %>%
  mutate(ref.dbh = NA, ref.dend = NA) %>% # clear ref.dend and ref.dbh which are not from last obs of previous year
  group_by(plot, tag, dend.num) %>%
  gather(var, obs, dend.meas, DBH, ref.dend, ref.dbh) %>%
  unite(tmp, jday, var) %>%
  spread(tmp, obs) %>%
  rename(jday.135 = '135_dend.meas', jday.135.DBH = '135_DBH', ref.dend = '135_ref.dend', ref.dbh = '135_ref.dbh',
    jday.169 = '169_dend.meas', ref.dbh.169 = '169_DBH', jday.200 = '200_dend.meas', ref.dbh.200 = '200_DBH',
    jday.241 = '241_dend.meas', ref.dbh.241 = '241_DBH', jday.296 = '296_dend.meas', ref.dbh.296 = '296_DBH') %>% 
  ungroup() %>% 
  mutate(ID = paste(plot, tag, dend.num, sep = "-"),
         # overwrite ref.dend and ref.dbh if new DBH taken on first day of year
         ref.dend = ifelse(!is.na(jday.135) & !is.na(jday.135.DBH), jday.135, ref.dend),
         ref.dbh = ifelse(!is.na(jday.135) & !is.na(jday.135.DBH), jday.135.DBH, ref.dbh),
         ref.dend.169 =  ifelse(!is.na(jday.169) & !is.na(ref.dbh.169), jday.169, NA),
         ref.dend.200 =  ifelse(!is.na(jday.200) & !is.na(ref.dbh.200), jday.200, NA),
         ref.dend.241 =  ifelse(!is.na(jday.241) & !is.na(ref.dbh.241), jday.241, NA),
         ref.dend.296 =  ifelse(!is.na(jday.296) & !is.na(ref.dbh.296), jday.296, NA),
         plot.tag = paste(plot, tag, sep = "-")) %>%
  select(plot.tag, plot, tag, ID, spp, dend.num,  ref.dbh, ref.dend, jday.135,
    ref.dbh.169, ref.dend.169, jday.169, ref.dbh.200, ref.dend.200, jday.200,
    ref.dbh.241, ref.dend.241, jday.241, ref.dbh.296, ref.dend.296, jday.296)


# create reference that is all current plot, tag, dend.num, ID combinations
trees.19 <- tow.19.ref.tmp %>% select(plot, tag, dend.num, ID)

setwd("C:/Users/tgw475/Desktop/Whitby HF Files/HF_BIOMETRY_WOFSY copy/")
tow.18.ref <- read.table("trees/data/dendrometer/2018/tow.18.ref.txt", header = T, sep = "\t")

source("R.scripts/old.functions.r")
source("trees/r.scripts/tgw 2017/dbh.fcn.tgw.r")
source("trees/r.scripts/tow.x.calc.r")

# get mortality and recruitment data (from run.all.r)
years<-list.files("trees/data/dendrometer", pattern = "\\d{4}")
years <- years[as.numeric(years)<=2019]
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

dend.dbh.all.19 <- left_join(dend.all.19, dbh.all.19)
tow.x.calc("2018")

tow.18.all.dbh <- bind_rows(tow.18.dbh, tow.18.rcrt.dbh, tow.18.mort.dbh)
dend.dbh.18.312 <- dend.dbh.all.19 %>% filter(year == 2018 & jday == 312)
tow.18.last <- dend.dbh.18.312[ , c("plot", "tag", "dend.num", "dend.meas", "DBH")] %>%
  rename("ref.dend" = "dend.meas", "dbh.312" = "DBH") %>%
  left_join(tow.18.all.dbh[ , c(1,2,9)]) %>%
  rename("ref.dbh" = "jday.312") %>%
  right_join(trees.19)

summary(tow.18.last)

any(duplicated(tow.18.last))

# create df with most recent dend-dbh pair for reference up to 2019
refs.19 <- dend.dbh.all.19 %>% group_by(plot, tag, dend.num) %>%
  mutate(date = as_date(paste0(year, jday), format = "%Y%j", tz = "EST"),
         ID = paste(plot, tag, dend.num, sep = "-")) %>%
  filter(!is.na(DBH)) %>% # have to do separate filter calls to do in this squence
  filter(date == max(date)) %>%
  rename(ref.dend = dend.meas, ref.dbh = DBH, ref.date = date) %>%
  select(plot, tag, dend.num, ID, ref.date, ref.dend, ref.dbh) %>%
  right_join(trees.19)

# Fill 2018 trees without ref values with 2019 data if available
missing.ID <- tow.18.last$ID[is.na(tow.18.last$ref.dbh)]
refs.19.to.fill <- refs.19 %>% filter(ID %in% missing.ID)
# Not all missing refs in 2018 can be filled with 2019 data, create fill ID from matching trees
fill.ID.19 <- refs.19.to.fill$ID[!is.na(refs.19.to.fill$ref.date)]

tow.18.last[which(tow.18.last$ID %in% fill.ID.19), c("ref.dend", "ref.dbh")] <-
  refs.19.to.fill[which(refs.19.to.fill$ID %in% fill.ID.19) , c("ref.dend", "ref.dbh")]

tow.18.last[is.na(tow.18.last$ref.dend), ]

tow.18.last <- tow.18.last %>%
  group_by(plot, tag) %>%
  arrange(plot, tag, dend.num) %>% 
  # some new double bands don't have a measured DBH yet, fill from older bands calculated DBH where possible
  mutate(ref.dbh = na.locf(ref.dbh, na.rm = F)) %>%
  select(plot, tag, dend.num, ID, ref.dend.18.312 = ref.dend, ref.dbh.18.312 = ref.dbh)

summary(tow.19.ref.tmp)
tow.19.ref.tmp <- left_join(tow.19.ref.tmp, tow.18.last[,c("ID", "ref.dend.18.312", "ref.dbh.18.312")]) %>%
  mutate(ref.dbh = ifelse(is.na(ref.dbh), ref.dbh.18.312, ref.dbh),
         ref.dend = ifelse(is.na(ref.dend), ref.dend.18.312, ref.dend)) %>%
  select(-contains('.312'), -ID)

tow.19.ref.tmp[is.na(tow.19.ref.tmp$ref.dend),]
# dend matches except for recruits
# will dupliacte values for double bands

# Filter duplicate dendrometer bands and only keep one with complete data
# -----------------------------------------------------------------------------------------------------------

dup <- tow.19.ref.tmp[duplicated(tow.19.ref.tmp[ ,"plot.tag"]), ]
tow.19.ref.dup <- tow.19.ref.tmp[tow.19.ref.tmp$plot.tag %in% dup$plot.tag, ] %>%
  arrange(plot.tag, dend.num)
trees <- unique(tow.19.ref.dup$plot.tag)

# loop requires that file is sorted ascending plot.tag & dend.num
tmp.trees <- list()
overlap <- as.character()

# t <- "B3-212"
# a = 0
for (t in trees){
  bnds <- tow.19.ref.dup[tow.19.ref.dup$plot.tag == t, ]
  #### need to update column nums after each dend measurement ####
  keep <- apply(bnds, 1, function(x){!any(is.na(x[c(6,7,8,11,14,17,20)]))})
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
#make sure transition to new band has ref.dend when switching to new
overlap.trees <- tow.19.ref.tmp[tow.19.ref.tmp$plot.tag %in% overlap, ] %>% arrange(plot.tag, dend.num)
# for first three trees band switched in doy 241 and 296, overwirte last 6 cols of older band with new
overlap.trees[c(1,3,5), c(15:20)] <- overlap.trees[c(2,4,6), c(15:20)] 
# some incomplete duplicates is are double-banded trees that died, keep the older band for each
overlap.trees <- overlap.trees[c(1,3,5,7,9), ]

# combine all the non-duplicated trees and remove dend.num category
tow.19.ref <- bind_rows(tow.19.ref.tmp[!tow.19.ref.tmp$plot.tag %in% dup$plot.tag, ],
                        tmp.trees[!tmp.trees$plot.tag %in% overlap, ],
                        overlap.trees) %>%
  ungroup() %>%
  mutate(ref.dbh.xxx = NA,
         ref.dend.xxx = NA) %>%
  select(-plot.tag, -dend.num) %>%
  arrange(plot, tag)

# new recruits, copy ref.dbh.256 to ref.dbh (these are the only ones with NA for ref.dbh)
# tow.19.ref$ref.dbh[is.na(tow.19.ref$ref.dbh)] <- tow.19.ref$ref.dbh.256[is.na(tow.19.ref$ref.dbh)]


# Look at ratio of first dend meas of year and ref to see if they are matched correclty
tow.19.ref.check <- tow.19.ref %>% mutate(ref.check = round((ref.dend - jday.135)/jday.135, 4))
plot(tow.19.ref.check$ref.check)
# check that all trees are still present
all.equal(sort(unique(tow.19.ref.tmp$plot.tag)),  sort(unique(paste(tow.19.ref$plot, tow.19.ref$tag, sep = "-"))))
setwd("C:/Users/tgw475/Desktop/Whitby HF Files/HF_BIOMETRY_WOFSY copy/trees/data/dendrometer/2019")

# remove trees that died preivious year if they were still in data
dead.last <- interaction(tow.19.ref$plot, tow.19.ref$tag) %in% 
  interaction(tow.18.mort.dbh$plot, tow.18.mort.dbh$tag)
tow.19.ref <- tow.19.ref[!dead.last, ]
write.table(tow.19.ref, "tow.19.ref.txt", sep = "\t", quote = F, row.names = F)

# read in file has manually filled in ref.dbh and ref.dend for recruits
# as well as back filling the first dendrometer measurement to the day of first DBH record
# and added two extra columns for tow.x.calc to work, 'ref.dend.xxx', 'ref.dbh.xxx'
# this will need to be updated manually if tow.19.ref.tmp changes
# tow.19.ref.tmp2 <- read.table("tow.19.ref.tmp2.txt", header=T) %>% arrange(plot, tag)

# create dend.text from final ref.txt
#### need to manually update col nums with additional measurements ####
tow.19.dend.txt <- tow.19.ref[,c(1:5,6,9,12,15,18)]
write.table(tow.19.dend.txt, "tow.19.dend.txt", sep = "\t", quote = F, row.names = F)
any(duplicated(interaction(tow.19.ref$plot, tow.19.ref$tag)))
