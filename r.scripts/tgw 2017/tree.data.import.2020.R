library(tidyverse)
library(lubridate)
library(zoo)
setwd("HF_BIOMETRY_WOFSY copy/trees/data/dendrometer/2020")
tow.20 <- bind_rows(read.csv("tow.20.108.dend.csv", header = T, na.strings = "NA"),
  read.csv("tow.20.170.dend.csv", header = T, na.strings = "NA"),
  read.csv("tow.20.218.dend.csv", header = T, na.strings = "NA"),
  read.csv("tow.20.267.dend.csv", header = T, na.strings = "NA"),
  read.csv("tow.20.327.dend.csv", header = T, na.strings = "NA"))

dend.all.19 <- read.table("../../ems.dend.98.19.txt", sep = "\t", header = T, as.is = T)
dbh.all.19 <- read.table("../../ems.dbh.98.19.txt", sep = "\t", header = T, as.is = T)


# remove bands with NA dend.meas if they have another valid band
trees <- unique(tow.20[, c("plot", "tag", "jday")])
for(i in 1:nrow(trees)){
  tree.obs <- tow.20[tow.20$plot == trees[i,1] & tow.20$tag == trees[i,2] & tow.20$jday == trees[i,3], ]
  if(nrow(tree.obs) > 1){
    if(sum(is.na(tree.obs$dend)) < nrow(tree.obs)){
      tow.20<- tow.20[!(tow.20$plot == trees[i,1] & tow.20$tag == trees[i,2] &  tow.20$jday == trees[i,3] & is.na(tow.20$dend)), ]
    }
  }
}

names(tow.20)
str(tow.20)

dend.data.20 <- tow.20 %>% select(plot, tag, jday, year, dend.num, dend.meas = dend) %>% 
  arrange(plot, tag) %>% 
  filter(!is.na(dend.meas))
any(duplicated(dend.data.20[ , c(1:3,5)]))
# should be FALSE
dend.data.20[duplicated(dend.data.20[ , c(1:3,5)]), ]

dend.all.20 <- rbind(dend.all.19, dend.data.20)
write.table(dend.all.20, "../../ems.dend.98.20.txt", sep = "\t", quote = F, row.names = F)

dbh.data.20 <- tow.20 %>% select(plot, tag, jday, year, DBH = dbh) %>% 
  mutate(plot.tag.day = paste(plot, tag, jday, sep = "-")) %>% 
  filter(!is.na(DBH), !duplicated(plot.tag.day)) %>% 
  select(-plot.tag.day)

any(duplicated(dbh.data.20))

dbh.all.20 <- rbind(dbh.all.19, dbh.data.20)
write.table(dbh.all.20, "../../ems.dbh.98.20.txt", sep = "\t", quote = F, row.names = F)

dend.dbh.20 <- tow.20 %>% select(plot, tag, spp, jday, year, dend.num, dend.meas = dend, DBH = dbh) %>% 
  arrange(plot,tag)


spp.data.20 <- tow.20 %>% select(plot, tag, spp) %>% 
  mutate(plot.tag = interaction(plot, tag)) %>% 
  filter(!duplicated(plot.tag)) %>% 
  select(-plot.tag)

# Create ref DBH according to old rules ----------------------------------------------------------------------
# commented out all following code since it only needs to run once
# uses previous year's last dend measurement as reference (may be sensitive to timing of last measurement)

tow.20.ref.tmp <- tow.20 %>%
  select(plot, tag, spp, jday, year, dend.num, ref.dend, ref.dbh, dend.meas = dend, DBH = dbh) %>%
  mutate(ref.dbh = NA, ref.dend = NA) %>% # clear ref.dend and ref.dbh which are not from last obs of previous year
  group_by(plot, tag, dend.num) %>%
  gather(var, obs, dend.meas, DBH, ref.dend, ref.dbh) %>%
  unite(tmp, jday, var) %>%
  spread(tmp, obs) %>%
  rename(jday.108 = '108_dend.meas', jday.108.DBH = '108_DBH', ref.dend = '108_ref.dend', ref.dbh = '108_ref.dbh',
    jday.170 = '170_dend.meas', ref.dbh.170 = '170_DBH',
    jday.218 = '218_dend.meas', ref.dbh.218 = '218_DBH', 
    jday.267 = '267_dend.meas', ref.dbh.267 = '267_DBH',
    jday.327 = '327_dend.meas', ref.dbh.327 = '327_DBH') %>%
  ungroup() %>% 
  mutate(ID = paste(plot, tag, dend.num, sep = "-"),
    # overwrite ref.dend and ref.dbh if new DBH taken on first day of year
    ref.dend = ifelse(!is.na(jday.108) & !is.na(jday.108.DBH), jday.108, ref.dend),
    ref.dbh = ifelse(!is.na(jday.108) & !is.na(jday.108.DBH), jday.108.DBH, ref.dbh),
    ref.dend.170 =  ifelse(!is.na(jday.170) & !is.na(ref.dbh.170), jday.170, NA),
    ref.dend.218 =  ifelse(!is.na(jday.218) & !is.na(ref.dbh.218), jday.218, NA),
    ref.dend.267 =  ifelse(!is.na(jday.267) & !is.na(ref.dbh.267), jday.267, NA),
    ref.dend.327 =  ifelse(!is.na(jday.327) & !is.na(ref.dbh.327), jday.327, NA),
    plot.tag = paste(plot, tag, sep = "-")) %>%
  select(plot.tag, plot, tag, ID, spp, dend.num,  ref.dbh, ref.dend, jday.108,
    ref.dbh.170, ref.dend.170, jday.170,
    ref.dbh.218, ref.dend.218, jday.218,
    ref.dbh.267, ref.dend.267, jday.267,
    ref.dbh.327, ref.dend.327, jday.327)


# create reference that is all current plot, tag, dend.num, ID combinations
trees.20 <- tow.20.ref.tmp %>% select(plot, tag, dend.num, ID)

setwd("../../../../")
getwd()
tow.19.ref <- read.table("trees/data/dendrometer/2019/tow.19.ref.txt", header = T, sep = "\t")

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

dend.dbh.all.20 <- left_join(dend.all.20, dbh.all.20)
tow.x.calc("2019")

tow.19.all.dbh <- bind_rows(tow.19.dbh, tow.19.rcrt.dbh, tow.19.mort.dbh)
dend.dbh.19.296 <- dend.dbh.all.20 %>% filter(year == 2019 & jday == 296)
tow.19.last <- dend.dbh.19.296[ , c("plot", "tag", "dend.num", "dend.meas", "DBH")] %>%
  rename("ref.dend" = "dend.meas", "dbh.296" = "DBH") %>%
  left_join(tow.19.all.dbh[ , c(1,2,9)]) %>%
  rename("ref.dbh" = "jday.296") %>%
  right_join(trees.20) %>% 
  arrange(ID)

summary(tow.19.last)

any(duplicated(tow.19.last))

# create df with most recent dend-dbh pair for reference up to 2020
refs.20 <- dend.dbh.all.20 %>% group_by(plot, tag, dend.num) %>%
  mutate(date = as_date(paste0(year, jday), format = "%Y%j"),
    ID = paste(plot, tag, dend.num, sep = "-")) %>%
  filter(!is.na(DBH)) %>% # have to do separate filter calls to do in this squence
  filter(date == max(date)) %>%
  rename(ref.dend = dend.meas, ref.dbh = DBH, ref.date = date) %>%
  select(plot, tag, dend.num, ID, ref.date, ref.dend, ref.dbh) %>%
  right_join(trees.20) %>% 
  arrange(ID)

# Fill 2019 trees without ref values with 2020 data if available
missing.ID <- tow.19.last$ID[is.na(tow.19.last$ref.dbh)]
refs.20.to.fill <- refs.20 %>% filter(ID %in% missing.ID)
# Not all missing refs in 2019 can be filled with 2020 data, create fill ID from matching trees
fill.ID.20 <- refs.20.to.fill$ID[!is.na(refs.20.to.fill$ref.date)]


tow.19.last[which(tow.19.last$ID %in% missing.ID), c("ID", "ref.dend", "ref.dbh")]   
refs.20.to.fill[which(refs.20.to.fill$ID %in% fill.ID.20) , c("ID", "ref.dend", "ref.dbh")]

tow.19.last[is.na(tow.19.last$ref.dend), ]

tow.19.last <- tow.19.last %>%
  group_by(plot, tag) %>%
  arrange(plot, tag, dend.num) %>% 
  # some new double bands don't have a measured DBH yet, fill from older bands calculated DBH where possible
  mutate(ref.dbh = na.locf(ref.dbh, na.rm = F)) %>%
  select(plot, tag, dend.num, ID, ref.dend.19.296 = ref.dend, ref.dbh.19.296 = ref.dbh)

summary(tow.20.ref.tmp)
tow.20.ref.tmp <- left_join(tow.20.ref.tmp, tow.19.last[,c("ID", "ref.dend.19.296", "ref.dbh.19.296")]) %>%
  mutate(ref.dbh = ifelse(is.na(ref.dbh), ref.dbh.19.296, ref.dbh),
    ref.dend = ifelse(is.na(ref.dend), ref.dend.19.296, ref.dend)) %>%
  select(-contains('.296'), -ID)

tow.20.ref.tmp[is.na(tow.20.ref.tmp$ref.dend),]
# dend matches except for recruits
# will dupliacte values for double bands

# Filter duplicate dendrometer bands and only keep one with complete data
# -----------------------------------------------------------------------------------------------------------

dup <- tow.20.ref.tmp[duplicated(tow.20.ref.tmp[ ,"plot.tag"]), ]
tow.20.ref.dup <- tow.20.ref.tmp[tow.20.ref.tmp$plot.tag %in% dup$plot.tag, ] %>%
  arrange(plot.tag, dend.num)
trees <- unique(tow.20.ref.dup$plot.tag)

# loop requires that file is sorted ascending plot.tag & dend.num
tmp.trees <- list()
overlap <- as.character()

# t <- "B3-212"
# a = 0
for (t in trees){
  bnds <- tow.20.ref.dup[tow.20.ref.dup$plot.tag == t, ]
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
# make sure transition to new band has ref.dend when switching to new
overlap.trees <- tow.20.ref.tmp[tow.20.ref.tmp$plot.tag %in% overlap, ] %>% arrange(plot.tag, dend.num)
# A2-25 dend switch on jday 267, copy new band onto old
overlap.trees[1, c(15:ncol(overlap.trees))] <- overlap.trees[2, c(15:ncol(overlap.trees))]
# A3-66 died by tipping over, and had two bands, keep dend.num 2 row 3
# D3-265 switched on jday 218, copy new band onto old
overlap.trees[5, c(12:ncol(overlap.trees))] <- overlap.trees[6, c(12:ncol(overlap.trees))]
# D3-606 dend switch on jday 267, copy new band onto old
overlap.trees[7, c(15:ncol(overlap.trees))] <- overlap.trees[8, c(15:ncol(overlap.trees))]
# D4-638 switched on jday 218, copy new band onto old
overlap.trees[9, c(12:ncol(overlap.trees))] <- overlap.trees[10, c(12:ncol(overlap.trees))]
# H1-303 died (suppressed) with two bands keep dend.num 1 row 11

# keep only first row for each duplicate tree tag
overlap.trees <- overlap.trees[c(1,3,5,7,9,11), ]

# combine all the non-duplicated trees and remove dend.num category
tow.20.ref <- bind_rows(tow.20.ref.tmp[!tow.20.ref.tmp$plot.tag %in% dup$plot.tag, ],
  tmp.trees[!tmp.trees$plot.tag %in% overlap, ],
  overlap.trees) %>%
  ungroup() %>%
  mutate(ref.dbh.xxx = NA,
    ref.dend.xxx = NA) %>%
  select(-plot.tag, -dend.num) %>%
  arrange(plot, tag)

# new recruits, copy ref.dbh.256 to ref.dbh (these are the only ones with NA for ref.dbh)
# tow.20.ref$ref.dbh[is.na(tow.20.ref$ref.dbh)] <- tow.20.ref$ref.dbh.256[is.na(tow.20.ref$ref.dbh)]


# Look at ratio of first dend meas of year and ref to see if they are matched correclty
tow.20.ref.check <- tow.20.ref %>% mutate(ref.check = round((ref.dend - jday.108)/jday.108, 4))
plot(tow.20.ref.check$ref.check)
# check that all trees are still present
all.equal(sort(unique(tow.20.ref.tmp$plot.tag)),  sort(unique(paste(tow.20.ref$plot, tow.20.ref$tag, sep = "-"))))
tow.20.ref.tmp$plot.tag[!unique(tow.20.ref.tmp$plot.tag) %in% unique(paste(tow.20.ref$plot, tow.20.ref$tag, sep = "-"))]

setwd("trees/data/dendrometer/2020")

# remove trees that died preivious year if they were still in data
dead.last <- interaction(tow.20.ref$plot, tow.20.ref$tag) %in% 
  interaction(tow.19.mort.dbh$plot, tow.19.mort.dbh$tag)
sum(dead.last)
tow.20.ref <- tow.20.ref[!dead.last, ]
write.table(tow.20.ref, "tow.20.ref.txt", sep = "\t", quote = F, row.names = F)

# read in file has manually filled in ref.dbh and ref.dend for recruits
# as well as back filling the first dendrometer measurement to the day of first DBH record
# and added two extra columns for tow.x.calc to work, 'ref.dend.xxx', 'ref.dbh.xxx'
# this will need to be updated manually if tow.20.ref.tmp changes
# tow.20.ref.tmp2 <- read.table("tow.20.ref.tmp2.txt", header=T) %>% arrange(plot, tag)

# create dend.text from final ref.txt
#### need to manually update col nums with additional measurements ####
tow.20.dend.txt <- tow.20.ref[,c(1:5, 6, 9, 12, 15, 18)]
write.table(tow.20.dend.txt, "tow.20.dend.txt", sep = "\t", quote = F, row.names = F)
#check for any duplicates in ref file
tow.20.ref[(duplicated(interaction(tow.20.ref$plot, tow.20.ref$tag))), ]

