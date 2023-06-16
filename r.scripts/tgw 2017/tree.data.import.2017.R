library(tidyverse)
library(lubridate)
library(zoo)
setwd("C:/Users/tgw475/Desktop/Whitby HF Files/HF_BIOMETRY_WOFSY copy/trees/data/dendrometer/2017")
tow.17 <- bind_rows(read.csv("tow.17.107.dend.csv", header = T, na.strings = "NA"), 
                read.csv("tow.17.171.dend.csv", header = T, na.strings = "NA"),
                read.csv("tow.17.226.dend.csv", header = T, na.strings = "NA"),
                read.csv("tow.17.334.dend.csv", header = T, na.strings = "NA")) 

# remove bands with NA dend.meas if they have another valid band
trees <- unique(tow.17[, c("plot", "tag", "jday")])
for(i in 1:nrow(trees)){
  tree.obs <- tow.17[tow.17$plot == trees[i,1] & tow.17$tag == trees[i,2] & tow.17$jday == trees[i,3], ]
  if(nrow(tree.obs) > 1){
    if(sum(is.na(tree.obs$dend)) < nrow(tree.obs)){
      tow.17<- tow.17[!(tow.17$plot == trees[i,1] & tow.17$tag == trees[i,2] & tow.17$jday == trees[i,3] & 
                          is.na(tow.17$dend)), ]
    }
  }
}
setwd(dir="C:/Users/tgw475/Desktop/Whitby HF Files/HF_BIOMETRY_WOFSY copy/")
dend.all <- read.table("trees/data/ems.dend.98.16.txt", sep = "\t", header = T)
dbh.all <- read.table("trees/data/ems.dbh.98.16.txt", sep = "\t", header = T, as.is = T)

names(tow.17)
str(tow.17)

dend.data.17 <- tow.17 %>% select(plot, tag, jday, year, dend.num, dend.meas = dend) %>% 
  arrange(plot, tag) %>% 
  filter(!is.na(dend.meas))

dend.all.17 <- rbind(dend.all, dend.data.17)
write.table(dend.all.17, "trees/data/ems.dend.98.17.txt", sep = "\t", quote = F, row.names = F)
  
dbh.data.17 <- tow.17 %>% select(plot, tag, jday, year, DBH = dbh) %>% 
  mutate(plot.tag.day = paste(plot, tag, jday, sep = "-")) %>% 
  filter(!is.na(DBH), !duplicated(plot.tag.day)) %>% 
  select(-plot.tag.day)
  
any(duplicated(dbh.data.17))

dbh.all.17 <- rbind(dbh.all, dbh.data.17)
write.table(dbh.all.17, "trees/data/ems.dbh.98.17.txt", sep = "\t", quote = F, row.names = F)


dend.dbh.17 <- tow.17 %>% select(plot, tag, spp, jday, year, dend.num, dend.meas = dend, DBH = dbh) %>% 
  arrange(plot,tag)

spp.data.17 <- tow.17 %>% select(plot, tag, spp) %>% 
  mutate(plot.tag = interaction(plot, tag)) %>% 
  filter(!duplicated(plot.tag)) %>% 
  select(-plot.tag)

# Create ref DBH according to old rules ----------------------------------------------------------------------
# commented out all following code since it only needs to run once
# uses previous year's last dend measurement as reference (may be sensitive to timing of last measurement)

tow.17.ref.tmp <- tow.17 %>%
  select(plot, tag, spp, jday, year, dend.num, ref.dend, ref.dbh, dend.meas = dend, DBH = dbh) %>%
  mutate(ref.dend = NA, ref.dbh = NA) %>%
  group_by(plot, tag, dend.num) %>%
  gather(var, obs, dend.meas, DBH, ref.dend, ref.dbh) %>% unite(tmp, jday, var) %>% spread(tmp, obs) %>%
  rename(ref.dend = '107_ref.dend', ref.dbh = '107_ref.dbh', jday.107 = '107_dend.meas', jday.107.DBH = '107_DBH',
         ref.dbh.171 = '171_DBH', jday.171 = '171_dend.meas', ref.dbh.226 = '226_DBH',
         jday.226 = '226_dend.meas', ref.dbh.334 = '334_DBH', jday.334 = '334_dend.meas') %>%
  mutate(ID = paste(plot, tag, dend.num, sep = "-"),
         #ref.dend = ifelse(!is.na(jday.107) & !is.na(jday.107.DBH), jday.107, ref.dend),
         # overwrite ref.dend and ref.dbh if new DBH taken on first day of year
         #ref.dbh = ifelse(!is.na(jday.107) & !is.na(jday.107.DBH), jday.107.DBH, ref.dbh),
         ref.dend.171 =  ifelse(!is.na(jday.171) & !is.na(ref.dbh.171), jday.171, NA),
         ref.dend.226 =  ifelse(!is.na(jday.226) & !is.na(ref.dbh.226), jday.226, NA),
         ref.dend.334 =  ifelse(!is.na(jday.334) & !is.na(ref.dbh.334), jday.334, NA),
         plot.tag = paste(plot, tag, sep = "-")) %>%
  select(plot.tag, plot, tag, ID, spp, dend.num,  ref.dbh, ref.dend, jday.107,
         ref.dbh.171, ref.dend.171, jday.171,
         ref.dbh.226, ref.dend.226, jday.226,
         ref.dbh.334, ref.dend.334, jday.334)

# create reference that is all current plot, tag, dend.num, ID combinations
trees.17 <- tow.17.ref.tmp %>% select(plot, tag, dend.num, ID)

any(duplicated(trees.17))

dend.dbh.all.17 <- left_join(dend.all.17, dbh.all.17)
# read in tow.16.ref.txt
tow.16.ref <- read.table("trees/data/dendrometer/2016/tow.16.ref.txt", header = T, sep = "\t")
nrow(tow.16.ref)
source("trees/r.scripts/tgw 2017/dbh.fcn.tgw.r")
source("trees/r.scripts/tow.x.calc.r")

# get mortality and recruitment data (from run.all.r)
years<-list.files("trees/data/dendrometer", pattern = "\\d{4}")
years <- years[as.numeric(years)<=2017]
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

tow.x.calc("2016")
tow.16.all.dbh <- bind_rows(tow.16.dbh, tow.16.mort.dbh)
nrow(tow.16.all.dbh)
tow.16.all.dbh[duplicated(tow.16.all.dbh),]

dend.dbh.16.290 <- dend.dbh.all.17 %>% filter(year == 2016 & jday == 290)
tow.16.last <- dend.dbh.16.290[ , c("plot", "tag", "dend.num", "dend.meas", "DBH")] %>%
  rename("ref.dend" = "dend.meas", "dbh.290" = "DBH") %>%
  left_join(tow.16.all.dbh[ , c(1,2,8)]) %>%
  rename("ref.dbh" = "jday.290") %>%
  right_join(trees.17)

summary(tow.16.last)
# 69 dendrometers from 2017 were not present in 2016

any(duplicated(tow.16.last))
tow.16.last[duplicated(tow.16.last),]
# create df with most recent dend-dbh pair for reference up to 2017
refs.17 <- dend.dbh.all.17 %>% group_by(plot, tag, dend.num) %>%
  mutate(date = as_date(paste0(year, jday), format = "%Y%j", tz = "EST"),
         ID = paste(plot, tag, dend.num, sep = "-")) %>%
  filter(!is.na(DBH)) %>% # have to do separate filter calls to do in this squence
  filter(date == max(date)) %>%
  rename(ref.dend = dend.meas, ref.dbh = DBH, ref.date = date) %>%
  select(plot, tag, dend.num, ID, ref.date, ref.dend, ref.dbh) %>%
  right_join(trees.17)

summary(refs.17)

# Fill 2016 trees without ref values with 2018 data if available
missing.ID <- tow.16.last$ID[is.na(tow.16.last$ref.dbh)]
refs.17.to.fill <- refs.17 %>% filter(ID %in% missing.ID)
# Not all missing refs in 2017 can be filled with 2018 data, create fill ID from matching trees
fill.ID.17 <- refs.17.to.fill$ID[!is.na(refs.17.to.fill$ref.date)]

length(unique(interaction(refs.17[, c("plot", "tag", "dend.num")])))
length(unique(refs.17$ID))

tow.16.last[which(tow.16.last$ID %in% fill.ID.17), c("ref.dend", "ref.dbh")] <-
  refs.17.to.fill[which(refs.17.to.fill$ID %in% fill.ID.17) , c("ref.dend", "ref.dbh")]

tow.16.last <- tow.16.last %>%
  # group_by(plot, tag, dend.num) %>%
  # # some double bands don't have a measured DBH yet, fill from older bands calculated DBH where possible
  # mutate(calc.dbh = ifelse(!is.na(ref.dend) & !is.na(ref.dbh) & !is.na(dend.meas), calc_dbh(dend.meas, ref.dend, ref.dbh), NA)) %>%
  group_by(plot, tag) %>%
  # some double bands don't have a measured DBH yet, fill from older bands calculated DBH where possible
  mutate(ref.dbh = na.locf(ref.dbh, na.rm = F)) %>%
  select(plot, tag, dend.num, ID, ref.dend.16.290 = ref.dend, ref.dbh.16.290 = ref.dbh)


# reset working directory
setwd("C:/Users/tgw475/Desktop/Whitby HF Files/HF_BIOMETRY_WOFSY copy/trees/data/dendrometer/2017")

tow.17.ref.tmp <- left_join(tow.17.ref.tmp, tow.16.last[,c("ID", "ref.dend.16.290", "ref.dbh.16.290")]) %>%
  mutate(ref.dbh = ifelse(is.na(ref.dbh), ref.dbh.16.290, ref.dbh),
         ref.dend = ifelse(is.na(ref.dend), ref.dend.16.290, ref.dend)) %>%
  select(-contains('.290'), -ID)

tow.17.ref.tmp[is.na(tow.17.ref.tmp$ref.dend),]
summary(tow.17.ref.tmp)
# dend matches except for recruits
# will dupliacte values for double bands,
# but doesn't matter in this case since all trees had dbh tape meas on first measurement of year

# Filter duplicate dendrometer bands and only keep one with complete data ------------------------------------
# many replacement bands were installed this year, need to keep only one from overlapping data

dup <- tow.17.ref.tmp[duplicated(tow.17.ref.tmp[ ,"plot.tag"]), ]
tow.17.ref.dup <- tow.17.ref.tmp[tow.17.ref.tmp$plot.tag %in% dup$plot.tag, ] %>% arrange(plot.tag, dend.num)
trees <- unique(tow.17.ref.dup$plot.tag)

# loop requires that file is sorted ascending plot.tag & dend.num
tmp.trees <- list()
overlap <- as.character()
# t <- "B3-215"
for (t in trees){
  bnds <- tow.17.ref.dup[tow.17.ref.dup$plot.tag == t, ]
  keep <- apply(bnds, 1, function(x){!any(is.na(x[c(6,7,8,11,14, 17)]))})
  # if only one band is complete, keep it
  if (sum(keep) == 1){
    tmp.row <- bnds[keep, ]
    # if all rows have all dendrometer measurements, keep the oldest one
  } else if (sum(keep) == length(keep)){
    tmp.row <- bnds[keep & bnds$dend.num == min(bnds$dend.num), ]
  } else if (sum(keep) == 0){
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
  # a = a+1
}
 tmp.trees <- bind_rows(tmp.trees)
tmp.trees[duplicated(tmp.trees), ]
overlap

#make sure transition to new band has ref.dend when switching to new
overlap.trees <- tow.17.ref.tmp[tow.17.ref.tmp$plot.tag %in% overlap, ] %>% arrange(plot.tag, dend.num)
#manually overwrite new dend onto old dend on and after jday 226
overlap.trees[1 , 12:17] <- overlap.trees[2 , 12:17] # B3-223
overlap.trees[3 , 12:17] <- overlap.trees[4 , 12:17] # D3-605
overlap.trees[5 , 15:17] <- overlap.trees[6 , 15:17] # G5-741
overlap.trees <- overlap.trees[c(1,3,5), ]



# combine all the non-duplicated trees and remove dend.num category
tow.17.ref <- bind_rows(tow.17.ref.tmp[!tow.17.ref.tmp$plot.tag %in% trees, ],
                        tmp.trees[!tmp.trees$plot.tag %in% overlap, ],
                        overlap.trees) %>%
  ungroup() %>%
  select(-plot.tag, -dend.num) %>%
  arrange(plot, tag)

tow.17.ref.check <- tow.17.ref %>% mutate(ref.check = (ref.dend - jday.107)/jday.107)
any(duplicated(tow.17.ref.check))

# check that all trees are still present
nrow(unique(tow.17.ref.tmp[, 1:2])) == nrow(unique(tow.17.ref[, 1:2]))
write.table(tow.17.ref, "tow.17.ref.tmp.txt", sep = "\t", quote = F, row.names = F)

# read in file has manually filled in ref.dbh and ref.dend for recruits
# as well as back filling the first dendrometer measurement to the day of first DBH record
# and added two extra columns for tow.x.calc to work, 'ref.dend.xxx', 'ref.dbh.xxx'
# this will need to be updated manually if tow.17.ref.tmp changes
tow.17.ref.tmp2 <- read.table("tow.17.ref.tmp2.txt", header=T) %>% arrange(plot, tag)
tow.17.ref.tmp2[duplicated(tow.17.ref.tmp2),]

# remove trees that died preivious year if they were still in data
dead.last <- interaction(tow.17.ref.tmp2$plot, tow.17.ref.tmp2$tag) %in% 
  interaction(tow.16.mort.dbh$plot, tow.16.mort.dbh$tag) 
# no matching trees
tow.17.ref.tmp2 <- tow.17.ref.tmp2[!dead.last, ]

# create dend.text from final ref.txt
tow.17.dend.txt <- tow.17.ref.tmp2[,c(1:5,6,9,12,15)]

plot(apply(tow.17.dend.txt[, c("ref.dend", "jday.334")], 1, diff))
sort(apply(tow.17.dend.txt[, c("ref.dend", "jday.334")], 1, diff))
sum(apply(tow.17.dend.txt[, c("ref.dend", "jday.334")], 1, diff), na.rm = T)
# 2537.949

write.table(tow.17.ref.tmp2, "tow.17.ref.txt", sep = "\t", quote = F, row.names = F)
write.table(tow.17.dend.txt, "tow.17.dend.txt", sep = "\t", quote = F, row.names = F)
any(duplicated(interaction(tow.17.ref.tmp2$plot, tow.17.ref.tmp2$tag)))

