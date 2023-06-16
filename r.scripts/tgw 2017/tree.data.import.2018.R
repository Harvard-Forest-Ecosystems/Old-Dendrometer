library(tidyverse)
library(lubridate)
library(zoo)
setwd("C:/Users/tgw475/Desktop/Whitby HF Files/HF_BIOMETRY_WOFSY copy/trees/data/dendrometer/2018")
tow.18 <- bind_rows(read.csv("tow.18.123.dend.csv", header = T, na.strings = "NA"), 
                    read.csv("tow.18.149.dend.csv", header = T, na.strings = "NA"),
                    read.csv("tow.18.208.dend.csv", header = T, na.strings = "NA"),
                    read.csv("tow.18.256.dend.csv", header = T, na.strings = "NA"),
                    read.csv("tow.18.312.dend.csv", header = T, na.strings = "NA"))

dend.all.17 <- read.table("../../ems.dend.98.17.txt", sep = "\t", header = T, as.is = T)
dbh.all.17 <- read.table("../../ems.dbh.98.17.txt", sep = "\t", header = T, as.is = T)


# remove bands with NA dend.meas if they have another valid band
trees <- unique(tow.18[, c("plot", "tag", "jday")])
for(i in 1:nrow(trees)){
  tree.obs <- tow.18[tow.18$plot == trees[i,1] & tow.18$tag == trees[i,2] & tow.18$jday == trees[i,3], ]
  if(nrow(tree.obs) > 1){
    if(sum(is.na(tree.obs$dend)) < nrow(tree.obs)){
      tow.18<- tow.18[!(tow.18$plot == trees[i,1] & tow.18$tag == trees[i,2] &  tow.18$jday == trees[i,3] & is.na(tow.18$dend)), ]
    }
  }
}

names(tow.18)
str(tow.18)

dend.data.18 <- tow.18 %>% select(plot, tag, jday, year, dend.num, dend.meas = dend) %>% 
  arrange(plot, tag) %>% 
  filter(!is.na(dend.meas))
any(duplicated(dend.data.18[ , c(1:3,5)]))
dend.data.18[duplicated(dend.data.18[ , c(1:3,5)]), ]
# should return 0 rows

dend.all.18 <- rbind(dend.all.17, dend.data.18)
write.table(dend.all.18, "../../ems.dend.98.18.txt", sep = "\t", quote = F, row.names = F)

dbh.data.18 <- tow.18 %>% select(plot, tag, jday, year, DBH = dbh) %>% 
  mutate(plot.tag.day = paste(plot, tag, jday, sep = "-")) %>% 
  filter(!is.na(DBH), !duplicated(plot.tag.day)) %>% 
  select(-plot.tag.day)

any(duplicated(dbh.data.18))

dbh.all.18 <- rbind(dbh.all.17, dbh.data.18)
write.table(dbh.all.18, "../../ems.dbh.98.18.txt", sep = "\t", quote = F, row.names = F)

dend.dbh.18 <- tow.18 %>% select(plot, tag, spp, jday, year, dend.num, dend.meas = dend, DBH = dbh) %>% 
  arrange(plot,tag)


spp.data.18 <- tow.18 %>% select(plot, tag, spp) %>% 
  mutate(plot.tag = interaction(plot, tag)) %>% 
  filter(!duplicated(plot.tag)) %>% 
  select(-plot.tag)

# Create ref DBH according to old rules ----------------------------------------------------------------------
# commented out all following code since it only needs to run once
# uses previous year's last dend measurement as reference (may be sensitive to timing of last measurement)

tow.18.ref.tmp <- tow.18 %>%
  select(plot, tag, spp, jday, year, dend.num, ref.dend, ref.dbh, dend.meas = dend, DBH = dbh) %>%
  mutate(ref.dbh = NA, ref.dend = NA) %>% # clear ref.dend and ref.dbh which are not from last obs of previous year
  group_by(plot, tag, dend.num) %>%
  gather(var, obs, dend.meas, DBH, ref.dend, ref.dbh) %>%
  unite(tmp, jday, var) %>%
  spread(tmp, obs) %>%
  rename(ref.dend = '123_ref.dend', ref.dbh = '123_ref.dbh', jday.123 = '123_dend.meas',jday.123.DBH = '123_DBH',
         ref.dbh.149 = '149_DBH', jday.149 = '149_dend.meas', ref.dbh.208 = '208_DBH', jday.208 = '208_dend.meas',
         ref.dbh.256 = '256_DBH', jday.256 = '256_dend.meas', ref.dbh.312 = '312_DBH', jday.312 = '312_dend.meas') %>%
  mutate(ID = paste(plot, tag, dend.num, sep = "-"),
         # overwrite ref.dend and ref.dbh if new DBH taken on first day of year
         # ref.dend = ifelse(!is.na(jday.123) & !is.na(jday.123.DBH), jday.123, ref.dend),
         # ref.dbh = ifelse(!is.na(jday.123) & !is.na(jday.123.DBH), jday.123.DBH, ref.dbh),
         ref.dend.149 =  ifelse(!is.na(jday.149) & !is.na(ref.dbh.149), jday.149, NA),
         ref.dend.208 =  ifelse(!is.na(jday.208) & !is.na(ref.dbh.208), jday.208, NA),
         ref.dend.256 =  ifelse(!is.na(jday.256) & !is.na(ref.dbh.256), jday.256, NA),
         ref.dend.312 =  ifelse(!is.na(jday.312) & !is.na(ref.dbh.312), jday.312, NA),
         plot.tag = paste(plot, tag, sep = "-")) %>%
  select(plot.tag, plot, tag, ID, spp, dend.num,  ref.dbh, ref.dend, jday.123,
         ref.dbh.149, ref.dend.149, jday.149, ref.dbh.208, ref.dend.208, jday.208,
         ref.dbh.256, ref.dend.256, jday.256, ref.dbh.312, ref.dend.312, jday.312)

# create reference that is all current plot, tag, dend.num, ID combinations
trees.18 <- tow.18.ref.tmp %>% select(plot, tag, dend.num, ID)

setwd("C:/Users/tgw475/Desktop/Whitby HF Files/HF_BIOMETRY_WOFSY copy/")
tow.17.ref <- read.table("trees/data/dendrometer/2017/tow.17.ref.txt", header = T, sep = "\t")
tow.17.ref[is.na(tow.17.ref$ref.dbh) | is.na(tow.17.ref$ref.dend), ]

source("R.scripts/old.functions.r")
source("trees/r.scripts/tgw 2017/dbh.fcn.tgw.r")
source("trees/r.scripts/tow.x.calc.r")

# get mortality and recruitment data (from run.all.r)
years<-list.files("trees/data/dendrometer", pattern = "\\d{4}")
years <- years[as.numeric(years)<=2018]
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

dend.dbh.all.18 <- left_join(dend.all.18, dbh.all.18)
tow.x.calc("2017")

tow.17.all.dbh <- bind_rows(tow.17.dbh, tow.17.rcrt.dbh, tow.17.mort.dbh)
dend.dbh.17.334 <- dend.dbh.all.18 %>% filter(year == 2017 & jday == 334)
dend.dbh.17.334[is.na(dend.dbh.17.334$ref.dbh) | is.na(dend.dbh.17.334$ref.dend), ]

tow.17.last <- dend.dbh.17.334[ , c("plot", "tag", "dend.num", "dend.meas", "DBH")] %>%
  rename("ref.dend" = "dend.meas", "dbh.334" = "DBH") %>%
  left_join(tow.17.all.dbh[ , c(1,2,8)]) %>%
  rename("ref.dbh" = "jday.334") %>%
  right_join(trees.18)

summary(tow.17.last)
# 69 dendrometers from 2017 were not present in 2016

any(duplicated(tow.17.last))

# create df with most recent dend-dbh pair for reference up to 2018
refs.18 <- dend.dbh.all.18 %>% group_by(plot, tag, dend.num) %>%
  mutate(date = as_date(paste0(year, jday), format = "%Y%j", tz = "EST"),
         ID = paste(plot, tag, dend.num, sep = "-")) %>%
  filter(!is.na(DBH)) %>% # have to do separate filter calls to do in this squence
  filter(date == max(date)) %>%
  rename(ref.dend = dend.meas, ref.dbh = DBH, ref.date = date) %>%
  select(plot, tag, dend.num, ID, ref.date, ref.dend, ref.dbh) %>%
  right_join(trees.18)

# Fill 2017 trees without ref values with 2018 data if available
missing.ID <- tow.17.last$ID[is.na(tow.17.last$ref.dbh)]
refs.18.to.fill <- refs.18 %>% filter(ID %in% missing.ID)
# Not all missing refs in 2017 can be filled with 2018 data, create fill ID from matching trees
fill.ID.18 <- refs.18.to.fill$ID[!is.na(refs.18.to.fill$ref.date)]

tow.17.last[which(tow.17.last$ID %in% fill.ID.18), c("ref.dend", "ref.dbh")] <-
  refs.18.to.fill[which(refs.18.to.fill$ID %in% fill.ID.18) , c("ref.dend", "ref.dbh")]

tow.17.last[is.na(tow.17.last$ref.dend), ]

tow.17.last <- tow.17.last %>%
  group_by(plot, tag) %>%
  # some new double bands don't have a measured DBH yet, fill from older bands calculated DBH where possible
  mutate(ref.dbh = na.locf(ref.dbh, na.rm = F)) %>%
  select(plot, tag, dend.num, ID, ref.dend.17.334 = ref.dend, ref.dbh.17.334 = ref.dbh)

summary(tow.18.ref.tmp)
tow.18.ref.tmp <- left_join(tow.18.ref.tmp, tow.17.last[,c("ID", "ref.dend.17.334", "ref.dbh.17.334")]) %>%
  mutate(ref.dbh = ifelse(is.na(ref.dbh), ref.dbh.17.334, ref.dbh),
         ref.dend = ifelse(is.na(ref.dend), ref.dend.17.334, ref.dend)) %>%
  select(-contains('.334'), -ID)

tow.18.ref.tmp[is.na(tow.18.ref.tmp$ref.dend),]
# dend matches except for recruits
# will dupliacte values for double bands

# Filter duplicate dendrometer bands and only keep one with complete data
# -----------------------------------------------------------------------------------------------------------

dup <- tow.18.ref.tmp[duplicated(tow.18.ref.tmp[ ,"plot.tag"]), ]
tow.18.ref.dup <- tow.18.ref.tmp[tow.18.ref.tmp$plot.tag %in% dup$plot.tag, ] %>%
  arrange(plot.tag, dend.num)
trees <- unique(tow.18.ref.dup$plot.tag)

# loop requires that file is sorted ascending plot.tag & dend.num
tmp.trees <- list()
overlap <- as.character()

# t <- "B3-212"
# a = 0
for (t in trees){
  bnds <- tow.18.ref.dup[tow.18.ref.dup$plot.tag == t, ]
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
overlap.trees <- tow.18.ref.tmp[tow.18.ref.tmp$plot.tag %in% overlap, ] %>% arrange(plot.tag, dend.num)
overlap.trees
# c5-290 switched bands but there's no ref DBH, fill with calc.DBH of earlier dend on jday 123
# calc_dbh(88.33, 87.9, 16.5) == 16.51584, 
overlap.trees$ref.dbh[6] <- 16.51584
overlap.trees$ref.dend[6] <- 33.2
# switch from band 1 to 2 for A1-18 and B1-154 (actually same tree where plots overlap) on jday.312
overlap.trees[1, 18:20] <- overlap.trees[2, 18:20]
overlap.trees[3, 18:20] <- overlap.trees[4, 18:20]
# double-banded tree H2-356 died, keep older band (row 7)
overlap.trees <- overlap.trees[c(1,3,6,7), ]

# combine all the non-duplicated trees and remove dend.num category
tow.18.ref <- bind_rows(tow.18.ref.tmp[!tow.18.ref.tmp$plot.tag %in% dup$plot.tag, ],
  tmp.trees[!tmp.trees$plot.tag %in% overlap, ],
  overlap.trees) %>%
  ungroup() %>%
  mutate(ref.dbh.xxx = NA,
         ref.dend.xxx = NA) %>%
  select(-plot.tag, -dend.num) %>%
  arrange(plot, tag)

# new recruits, copy ref.dbh.256 to ref.dbh (these are the only ones with NA for ref.dbh)
tow.18.ref$ref.dbh[is.na(tow.18.ref$ref.dbh)] <- tow.18.ref$ref.dbh.256[is.na(tow.18.ref$ref.dbh)]


#Loot at ratio of first dend meas of year and ref to see if they are matched correclty
tow.18.ref.check <- tow.18.ref %>% mutate(ref.check = round((ref.dend - jday.123)/jday.123, 4))
plot(tow.18.ref.check$ref.check)
# check that all trees are still present
all.equal(sort(unique(tow.18.ref.tmp$plot.tag)),  sort(unique(paste(tow.18.ref$plot, tow.18.ref$tag, sep = "-"))))
setwd("C:/Users/tgw475/Desktop/Whitby HF Files/HF_BIOMETRY_WOFSY copy/trees/data/dendrometer/2018")

# remove trees that died preivious year if they were still in data
dead.last <- interaction(tow.18.ref$plot, tow.18.ref$tag) %in% interaction(tow.17.mort.dbh$plot, tow.17.mort.dbh$tag) 
tow.18.ref <- tow.18.ref[!dead.last, ]
write.table(tow.18.ref, "tow.18.ref.txt", sep = "\t", quote = F, row.names = F)

# read in file has manually filled in ref.dbh and ref.dend for recruits
# as well as back filling the first dendrometer measurement to the day of first DBH record
# and added two extra columns for tow.x.calc to work, 'ref.dend.xxx', 'ref.dbh.xxx'
# this will need to be updated manually if tow.18.ref.tmp changes
# tow.18.ref.tmp2 <- read.table("tow.18.ref.tmp2.txt", header=T) %>% arrange(plot, tag)

# create dend.text from final ref.txt
tow.18.dend.txt <- tow.18.ref[,c(1:5,6,9,12,15,18)]
write.table(tow.18.dend.txt, "tow.18.dend.txt", sep = "\t", quote = F, row.names = F)
any(duplicated(interaction(tow.18.ref$plot, tow.18.ref$tag)))
# should return F, no duplicate trees
