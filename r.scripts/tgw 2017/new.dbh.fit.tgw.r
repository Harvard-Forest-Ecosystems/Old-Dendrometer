library(tidyverse)
library(lubridate)
library(broom)
library(zoo)

# Import qa/qc dataset from "tree.qaqc.test.R"
# need to remove trees on the year they die and exclude from this analysis & move to own dataset

setwd(dir="C:/Users/tgw475/Desktop/Whitby HF Files/HF_BIOMETRY_WOFSY copy/trees/")
source("r.scripts/tgw 2017/dbh.fcn.tgw.r")
# data import not needed 2017 added to csv file below
#source("r.scripts/tgw 2017/tree.data.import.2017.R")
setwd(dir="C:/Users/tgw475/Desktop/Whitby HF Files/HF_BIOMETRY_WOFSY copy/trees/")

dend.dbh <- read.csv(file = "ems.dend.dbh.all.csv") %>% 
  bind_rows(tow.17.dend.dbh, tow.18.dend.dbh) %>% 
  group_by(plot, tag) %>% 
  mutate(date = as_date(paste(year, jday), format = "%Y%j", tz = "EST")) %>% 
  arrange(plot, tag, date, dend.num) %>% 
  filter(!(is.na(dend.meas) & is.na(DBH))) %>% 
  mutate(circ = (DBH*10) * pi) %>% 
  select(plot, tag, spp, year, jday, date, dend.num, dend.meas, DBH, circ)
dend.dbh$spp <- as.factor(dend.dbh$spp)
# entries without dend.num, correspond to DBH-only measures
length(dend.dbh$dend.num[is.na(dend.dbh$dend.num) & !is.na(dend.dbh$DBH)])



head(dend.dbh)
# loop to import and combine mortality and recruitment data into one data frame each
years<-as.character(list.files("data/dendrometer"))
recruit.tow <- data.frame(year = numeric(), plot = character(), tag = numeric())
mort.tow <- data.frame(year = numeric(), plot = character(), tag = numeric())
for(year in years){
  yr<-substr(year,3,4)
  fl<-paste("data/dendrometer/",year,"/tow.",yr,".mort.txt",sep="")
  if(file.exists(fl)){
    tmp <- read.table(file=fl,header=T, sep = "")
    tmp$year <- as.numeric(year)
    mort.tow <- rbind(mort.tow, tmp[,c("year","plot", "tag")])
  }
  fl<-paste("data/dendrometer/",year,"/tow.",yr,".recruits.txt",sep="")
  if(file.exists(fl)){
    tmp <- read.table(file=fl,header=T, sep = "")
    tmp$year <- as.numeric(year)
    recruit.tow <- rbind(recruit.tow, tmp[,c("year","plot", "tag")])
  }
}

#  recruit.cut <- data.frame(year = numeric(), plot = character(), tag = numeric())
#  mort.cut <- data.frame(year = numeric(), plot = character(), tag = numeric())
#  for(year in years){
#    yr<-substr(year,3,4)
#    fl<-paste("data/dendrometer/",year,"/cut.",yr,".mort.txt",sep="")
#    if(file.exists(fl)){
#      tmp <- read.table(file=fl,header=T)
#      tmp$year <- as.numeric(year)
#      mort.cut <- rbind(mort.cut, tmp[,c("year","plot", "tag")])
#    }
#    fl<-paste("data/dendrometer/",year,"/cut.",yr,".recruits.txt",sep="")
#    if(file.exists(fl)){
#      tmp <- read.table(file=fl,header=T)
#      tmp$year <- as.numeric(year)
#      recruit.cut <- rbind(recruit.cut, tmp[,c("year","plot", "tag")])
#    }
#  }

# data entry errors and outliers to correct ---------------------------------------------------------------------------------

# F3 12 skipped dend.num 2
dend.dbh$dend.num[dend.dbh$plot == "F3" & dend.dbh$tag == 12 & dend.dbh$dend.num > 1 &!is.na(dend.dbh$dend.num)] <- dend.dbh$dend.num[dend.dbh$plot == "F3" & dend.dbh$tag == 12 & dend.dbh$dend.num > 1& !is.na(dend.dbh$dend.num)] - 1
# C1 351 remove redundant dend.num 4 installed incorrectly and removed w/in a year
dend.dbh <- dend.dbh[!(dend.dbh$plot == "C1" & dend.dbh$tag == 351 & dend.dbh$dend.num == 4), ]
# rename dend.num 5 to dend.num 4
dend.dbh$dend.num[dend.dbh$plot == "C1" & dend.dbh$tag == 351 & dend.dbh$dend.num == 5] <- 4
# C3 425 remove 5cm too low dbh in 2000
dend.dbh$DBH[dend.dbh$plot == "C3" & dend.dbh$tag == 425 & dend.dbh$year == 2000 & dend.dbh$jday == 94] <- NA
# G4 258 high growth 6-8-04
dend.dbh$dend.meas[dend.dbh$plot == "G4" & dend.dbh$tag == 258 & dend.dbh$year == 2004 & dend.dbh$jday == 160] <- 25.47
# F4 55 bb high DBH removed 7-30-2000
dend.dbh$DBH[dend.dbh$plot == "F4" & dend.dbh$tag == 55 & dend.dbh$year == 2001 & dend.dbh$DBH == 29.4 ] <- NA
# G5 90 dend # 1 must have multiple holes, smaller distance in 2017 remove for now until corrections made
dend.dbh <- dend.dbh[!(dend.dbh$plot == "G5" & dend.dbh$tag == 90 & dend.dbh$year == 2017 & dend.dbh$dend.num == 1), ]
# C1 225 Beech, exceptionally high
dend.dbh$DBH[dend.dbh$plot == "C1" & dend.dbh$tag == 225 & dend.dbh$year == 2012 & dend.dbh$DBH == 51.7 ] <- NA
# A2 92 has two high DBH after switching to dend # 2
dend.dbh$DBH[dend.dbh$plot == "A2" & dend.dbh$tag == 92 & dend.dbh$year < 2015 & dend.dbh$dend.num == 2] <- NA
# D1 531 has a calculated DBH (not recorded in datasheets) when switching to dend # 2
dend.dbh$DBH[dend.dbh$plot == "D1" & dend.dbh$tag == 531 & dend.dbh$DBH == 33.98] <- NA
# B4 263 has a high outlier in 2000 "doubleband"
dend.dbh$DBH[dend.dbh$plot == "B4" & dend.dbh$tag == 263 & dend.dbh$DBH == 17.21] <- NA
# E1 678 dend.num 2 had spurious data in 2013 before being continually measured in 2015
dend.dbh <- dend.dbh[!(dend.dbh$plot == "E1" & dend.dbh$tag == 678 & dend.dbh$dend.num == 2 & dend.dbh$year == 2013),]
# F3 737 has outlier high DBH in 2014 15.7
dend.dbh$DBH[dend.dbh$plot == "F3" & dend.dbh$tag == 737 & dend.dbh$DBH == 15.7] <- NA
# G5 289 new hole punched 05-03-18 for dend.num 3. Do not use previous data
dend.dbh <- dend.dbh[!(dend.dbh$plot == "G5" & dend.dbh$tag == 289 & dend.dbh$dend.num == 3 & dend.dbh$year < 2018), ]
# B4 276 12-12-2013 erroneously high dend measurement, carry forward previous measurement
dend.dbh$dend.meas[dend.dbh$plot == "B4" & dend.dbh$tag == 276 & dend.dbh$date == "2013-12-12" ] <- 36.19
# G5 90 inside hole measured in mid 2015. Notes on 2018-05-03 say "outer hole 89.40, inner hole 43.58,  
# Previous measurements were only inner hole; they were converted to outer by the difference between these two"
dend.dbh$dend.meas[dend.dbh$plot == "G5" & dend.dbh$tag == 90 & (dend.dbh$date == "2015-06-22" | dend.dbh$date == "2015-08-27")] <- 
  dend.dbh$dend.meas[dend.dbh$plot == "G5" & dend.dbh$tag == 90 & (dend.dbh$date == "2015-06-22" | dend.dbh$date == "2015-08-27")] + 45.97
# G5 90 used bad conversion between inner and outer hole fixed using differecnce measured in 2018: see previous
dend.dbh$dend.meas[dend.dbh$plot == "G5" & dend.dbh$tag == 90 & dend.dbh$dend.meas == 93.46] <- 36.77 + 45.97
dend.dbh$dend.meas[dend.dbh$plot == "G5" & dend.dbh$tag == 90 & dend.dbh$dend.meas == 94.03] <- 37.64 + 45.97 
# H2 347 typo in data entry: low value 2015-08-27
dend.dbh$dend.meas[dend.dbh$plot == "H2" & dend.dbh$tag == 347 & dend.dbh$date == "2015-08-27"] <- 23.13
# F1 933 typo in data entry: low value 2012-06-27
dend.dbh$dend.meas[dend.dbh$plot == "F1" & dend.dbh$tag == 933 & dend.dbh$date == "2012-06-27"] <- 79.13
# A1 19 dend.num 3 installed in late 2009, had NA in 2010, simply removed first measurement to avoid gap in record
dend.dbh <- dend.dbh[!(dend.dbh$plot == "A1" & dend.dbh$tag == 19 & dend.dbh$dend.num == 3 & dend.dbh$year == "2009"),]
# F2 987 typo in data entry: high value 2014-12-03
dend.dbh$dend.meas[dend.dbh$plot == "F2" & dend.dbh$tag == 987 & dend.dbh$date == "2014-12-03"] <- 32.49
# F5 133 typo in data entry: high value 2013-12-12
dend.dbh$dend.meas[dend.dbh$plot == "F5" & dend.dbh$tag == 133 & dend.dbh$date == "2013-12-12"] <- 46.9
# C4 498 typo in data entry: high value 2011-05-10
dend.dbh$dend.meas[dend.dbh$plot == "C4" & dend.dbh$tag == 498 & dend.dbh$date == "2011-05-10"] <- 23.58
# E2 708 band.num 1 broke and was missing for most of 2004 and 2005, is filled as band 2 ==0 should be band.num 1 and fill previous measurement forward
dend.dbh$dend.num[dend.dbh$plot == "E2" & dend.dbh$tag == 708 & dend.dbh$date < "2005-11-02"] <- 1
dend.dbh$dend.meas[dend.dbh$plot == "E2" & dend.dbh$tag == 708 & dend.dbh$date < "2005-11-02" & dend.dbh$date > "2004-04-12"] <- 32.99
# C4 570 first dend.num skips to 2, and is renamed to 1 prior to 2017, when actual second band installed
dend.dbh$dend.num[dend.dbh$plot == "C4" & dend.dbh$tag == 570 & dend.dbh$year < 2017] <- 1
# D1 532 has too long overlap between bands, and error at end of dend.num 3
dend.dbh <- dend.dbh[!(dend.dbh$plot == "D1" & dend.dbh$tag == 532 & dend.dbh$dend.num == 3 & dend.dbh$year > 2015),] # dendmeas was >130mm which is much bigger than needed
dend.dbh <- dend.dbh[!(dend.dbh$plot == "D1" & dend.dbh$tag == 532 & dend.dbh$dend.num == 4 & dend.dbh$date < "2014-06-10"),] # removed first year of dend 4
# F3 7 dend.num 3 typo in data entry: low value 2016-04-22, and too much dend overlap
dend.dbh$dend.meas[dend.dbh$plot == "F3" & dend.dbh$tag == 7 & dend.dbh$dend.num == 3 & dend.dbh$date == "2016-04-22"] <- 36.92
dend.dbh <- dend.dbh[!(dend.dbh$plot == "F3" & dend.dbh$tag == 7 & dend.dbh$dend.num == 2 & dend.dbh$year > 2016),] # dendmeas was >130mm which is much bigger than needed
dend.dbh <- dend.dbh[!(dend.dbh$plot == "F3" & dend.dbh$tag == 7 & dend.dbh$dend.num == 3 & dend.dbh$year < 2016),] # removed first year of dend 4
# F5 114 dend.num 1/3 long overlap, shinking on 2 for first measuremtns, erase in 2015
dend.dbh <- dend.dbh[!(dend.dbh$plot == "F5" & dend.dbh$tag == 114 & dend.dbh$dend.num == 2 & dend.dbh$year < 2016),]


# Translate last dend date forward when bands switch if there is no overlap ---------------------------------------
obs.dates <- unique(dend.dbh$date)
obs.trees <- unique(dend.dbh[ , c("plot", "tag", "spp")])
tmp2 <- dend.dbh[0, ]

for(i in 1:nrow(obs.trees)){
  tmp <- dend.dbh[dend.dbh$plot == obs.trees$plot[i] & dend.dbh$tag == obs.trees$tag[i], ]
  max.date <- max(tmp$date)
  min.date <- min(tmp$date)
  tree.dates <- data.frame(obs.trees[i, 1:3], date = obs.dates[obs.dates >= min.date & obs.dates <= max.date])
  band.count <- na.omit(unique(tmp$dend.num))
  if(length(band.count)>1){
    for(bnd in 1:(length(band.count)-1)){
      date.end <- max(tmp$date[tmp$dend.num == bnd & !is.na(tmp$dend.meas)], na.rm = T)
      date.start <- min(tmp$date[tmp$dend.num == bnd+1 & !is.na(tmp$dend.meas)], na.rm = T)
      if(date.end < date.start & (date.end - date.start) < 300){ #copy forward if no overlap and gap is <300 days
        tmp.row <- tmp[tmp$dend.num == bnd & tmp$date == date.end, ]
        tmp.row$date <- date.start
        tmp <- bind_rows(tmp.row, tmp)
      }
      if(date.end > date.start & abs(date.start - date.date.end) > 180){ #delete first 90 days of new dend if overlap is > 180 days
        tmp <- tmp[!(tmp$dend.num == bnd + 1 & tmp$date < date.start + 90), ]
      }
    }
  }
  tmp <- full_join(tmp, tree.dates, by = c("plot", "tag","spp", "date"))
  tmp2<- bind_rows(tmp2, tmp)
}

nrow(tmp2) - nrow(dend.dbh)
tmp2 <- arrange(tmp2, plot, tag, date, dend.num)
# 387 missing observations and dendrometer switch values added to dataset
filled.rows <- anti_join(tmp2, dend.dbh, by = c("plot", "tag", "date", "dend.num")) %>% arrange(plot, tag, date, dend.num)

# ---------------------------------------------------------------------------------
#  Use zoo::na.approx to interpolate missing dend.meas values within a contiguous dendroband record. Leading and trailing NA's will be left as is

dend.dbh <- tmp2 %>% arrange(plot, tag, date, dend.num) %>% 
  group_by(plot, tag) %>% 
  mutate(year = ifelse(is.na(year), as_date(strftime(date, format = "%Y")), year),
         jday = ifelse(is.na(jday), as_date(strftime(date, format = "%j")), jday),
         dend.num = na.locf(dend.num, na.rm=F), # carry forward dend.num to empty obs dates first
         dend.num = na.locf(dend.num, na.rm=F, fromLast = T)) %>% #  then carry backwards to fill beginning of record
  group_by(plot, tag, dend.num) %>%
  mutate(dend.meas = na.approx(dend.meas, date, na.rm = F))

# tested with new column at first and compared values
# there were 21 filled cases, where NA's had flanking values on either side to interpolate from
# length(which(is.na(dend.dbh$dend.meas) & !is.na(dend.dbh$dend.meas2)))

# ---------------------------------------------------------------------------------
# loop to filter out ref DBH within the first 3 months of dendroband record, if ther are at least 3 ref DBHs for that band
obs.dates <- unique(dend.dbh$date)
obs.trees <- unique(dend.dbh[ , c("plot", "tag", "spp")])

a <- length(dend.dbh$DBH[!is.na(dend.dbh$DBH)])
for(i in 1:nrow(obs.trees)){
  tree <- dend.dbh[dend.dbh$plot == obs.trees$plot[i] & dend.dbh$tag == obs.trees$tag[i], ]
  for(d in 1: length(!is.na(unique(tree$dend.num)))){
    band <- tree[tree$dend.num == d, ]
    refs <- band[complete.cases(band[, c("dend.meas", "DBH")]), ]
    if(nrow(refs)>1){
      min.date <- min(band$date)
      min.ref <- min(refs$date)
      if((min.date - min.ref) < 90){
        dend.dbh$DBH[dend.dbh$plot == tree$plot[1] & dend.dbh$tag == tree$tag[1] & dend.dbh$dend.num == d & dend.dbh$date == min.ref] <- NA
      }
    }
  }
}
b <- length(dend.dbh$DBH[!is.na(dend.dbh$DBH)])
a-b

# ---------------------------------------------------------------------------------
#  flag dead trees and new recruits # # # # 
# convert factors to character to prevent error of mismatched levels when comparing
mort.tow$plot <- as.character(mort.tow$plot)
recruit.tow$plot <- as.character(recruit.tow$plot)
dend.dbh$plot <- as.character(dend.dbh$plot)
dend.dbh$mort <- 0
dend.dbh$recruit <- 0

# label new recruits
for(year in years){
  new.trees <- recruit.tow[recruit.tow$year == year, ]
  if(nrow(new.trees)>0){
    for(i in 1:nrow(new.trees)){
      # make sure new recruits are in dend.dbh data on correct year
      dend.dbh$recruit[(dend.dbh$plot == new.trees$plot[i] & dend.dbh$tag == new.trees$tag[i] & 
                          dend.dbh$year <= as.numeric(year))] <- 1 
    }
  }
}

# label dead trees
for(year in years){
  dead.trees <- mort.tow[mort.tow$year == year, ]
  if(nrow(dead.trees) > 0){
    for(i in 1:nrow(dead.trees)){
      # tag trees as dead on the year of mortality
      dend.dbh$mort[(dend.dbh$plot == dead.trees$plot[i] & dend.dbh$tag == dead.trees$tag[i] &
                       dend.dbh$year == as.numeric(year))] <- 1
      # remove any measurements after year of mortality
      dend.dbh <- dend.dbh[!(dend.dbh$plot == dead.trees$plot[i] & dend.dbh$tag == dead.trees$tag[i] &
                               dend.dbh$year > as.numeric(year)), ]
    }
  }
}
dend.dbh$plot <- as.factor(dend.dbh$plot)

# create separate file of trees on year they died and later
dend.dbh.mort <- full_join(mort.tow, dend.dbh[dend.dbh$mort == 1, ]) %>% arrange(plot, tag, year, date)
nrow(unique(dend.dbh.mort[ ,c(2,3)]))
nrow(mort.tow)

# create separate file of trees on year they were recruited (some trees were measured below 10cm and show up sooner)
dend.dbh.rec <- full_join(recruit.tow, dend.dbh[dend.dbh$recruit == 1, ]) %>% arrange(plot, tag, year, date)
nrow(unique(dend.dbh.rec[ ,c(2,3)]))
nrow(recruit.tow)

# some trees show up before the recorded year of recruitment. 
# leave in for now, but will filter our any trees < 10 cm DBH after calculations in case refdend and refdbh are in pre-recruitment year

# ---------------------------------------------------------------------------------
# functions to use broom package for modeling, while skipping over data with insufficient measurements 
dend_mod <- function(df){
  ifelse(sum(!is.na(df$circ)) < 3, list(NA), return(lm(circ ~ dend.meas, data = df, na.action = na.omit)))
}
dend_tidy <- function(df){
  ifelse(is.na(df), list(NA), tidy(df))
}
dend_glance <- function(df){
  ifelse(is.na(df), list(NA), glance(df))
}
dend_augment <- function(df){
  ifelse(is.na(df), list(NA), augment(df))
}
list_refdend <- function(df){
  ifelse(sum(complete.cases(df[, c("DBH", "dend.meas")])) < 1, list(NA), list(df$dend.meas[complete.cases(df$DBH, df$dend.meas)]))
}
list_refdbh <- function(df){
  ifelse(sum(complete.cases(df[, c("DBH", "dend.meas")])) < 1, list(NA), list(df$DBH[complete.cases(df$DBH, df$dend.meas)]))
}
#  df<-
#  dbh_alone <- function(df){
#    if(sum(!is.na(df$DBH)) == 1){
#      for(i in (seq_along(df$dend.meas))){
#        if(!is.na(df$DBH[i] & is.na(df$dend.meas[i]))){
#          closest<-which.min((abs(band.dates-dbh$date[i])))
#          closest<-which.min((abs(df$date[!is.na(dend.meas)]-df$date[i])))
#        }
#      }
#    }
#    if(sum(!is.na(df$DBH) & is.na(df$dend.meas)) < sum(!is.na(df$DBH))){
#      filter(df, is.na(dend.meas))
#    }
#  }

# ---------------------------------------------------------------------------------
#  Create nested lists by dend.num of refdend and refdbh from days where measurements coincide
list1 <- dend.dbh  %>% 
  group_by(plot, tag, dend.num) %>%
  arrange(plot, tag, dend.num) %>% 
  nest() %>% 
  mutate(refdends = flatten(map(data, list_refdend)), 
         refdbhs = flatten(map(data, list_refdbh)))


list2 <- list1 %>% 
  select(data) %>% 
  modify_depth(2, "dend.meas") %>%
  flatten() %>% 
  list(
    list1 %>% select(refdends), 
    list1 %>% select(refdbhs))

# initialize values for calculating the median DBH for each band
start <- Sys.time()
dends <- list2[[1]]
refdends <- flatten(list2[[2]])
refdbhs <- flatten(list2[[3]])

# dbh.med object will have the same data structure as the original dendrometer readings
dbh.med <- dends

# there are 98 dendrometers that do not have a DBH and dendrometer pair
list1[which(is.na(refdends) | is.na(refdbhs)), ]
length(refdbhs[which(is.na(refdends) | is.na(refdbhs))])

# loop to create dbh.med and merge with other data
for(d in seq_along(dends)){
  dend <- dends[[d]]
  refdend <- refdends[[d]]
  refdbh <- refdbhs[[d]]
  calcdbhs <- as_tibble(matrix(nrow = length(dend), ncol = length(refdbh)))
  # only run calc.dbh function if refdend is not NA
  for(x in seq_along(refdend)){
    if(!is.na(refdend[x])){
      if(!is.na(refdbh[x])){
        for(obs in seq_along(dend)){
          if(!is.na(dend[obs])){
            calcdbhs[obs, x] <- calcdbh <- calc_dbh(dend[obs], refdend[x], refdbh[x])
          }else{
            calcdbhs[obs, x] <- NA
          }
        }
      }
    }
    # only calculate median if there are more than 1 refdbh
    dbh.med[[d]] <- ifelse(length(refdbh > 1), list(apply(calcdbhs, 1, median, na.rm = T), list(calcdbhs)))
  }
  # print(as.character(d))
}
refdbhs[d]
refdends[d]


# rm(dends, refdends, refdbhs, dend, refdend, refdbh, calcdbhs, d, x, obs)
dbh.med <- flatten(dbh.med)
list1$dbh.med <- dbh.med

dend.dbh <- unnest(list1, data, dbh.med) %>% group_by(plot, tag, dend.num) %>% 
  mutate(dbh.med = ifelse(is.na(dbh.med) & !is.na(DBH), DBH, dbh.med)) %>% 
  arrange(plot, tag, date, dend.num)
dend.dbh.check <- dend.dbh[is.na(dend.dbh$dbh.med), ]
rm(dbh.med, refdbhs, refdends)

# correct errors for bands with overlapping dates
dend.dbh.test<- dend.dbh %>% group_by(plot, tag, spp, date) %>% 
   summarize(dbh.med = median(dbh.med))

end <-Sys.time()
start - end

# comparison to plots from "\\Eft\data\HF_DATA\HF_BIOMETRY_WOFSY\figures\elg figures\dend figures"
#  p <- dend.dbh %>% filter(plot == "A1", tag == 18)
#  p2 <- dend.dbh.test %>% filter(plot == "A1", tag == 18)
#  p <- dend.dbh %>% filter(plot == "A2", tag == 25)
#  p <- dend.dbh %>% filter(plot == "A3", tag == 51)
#  p2 <- dend.dbh.test %>% filter(plot == "A3", tag == 51)
#  p <- dend.dbh %>% filter(plot == "D1", tag == 537)
#  p2 <- dend.dbh.test %>% filter(plot == "D1", tag == 537)
#  p <- dend.dbh %>% filter(plot == "D4", tag == 633)
#  p <- dend.dbh %>% filter(plot == "D2", tag == 587)
#  p <- dend.dbh %>% filter(plot == "C1", tag == 351)
#  p2 <- dend.dbh.test %>% filter(plot == "C1", tag == 351)
#  #  p <- dend.dbh %>% filter(plot == "C3", tag == 425)
#  #  p2 <- dend.dbh.test %>% filter(plot == "C3", tag == 425)
#  #  p <- dend.dbh %>% filter(plot == "B4", tag == 726)
#  #  p2 <- dend.dbh.test %>% filter(plot == "B4", tag == 726)
#  p <- dend.dbh %>% filter(plot == "F4", tag == 735)
#  p2 <- dend.dbh.test %>% filter(plot == "F4", tag == 735)
#  p <- dend.dbh %>% filter(plot == "A1", tag == 6)
#  p2 <- dend.dbh.test %>% filter(plot == "A1", tag == 6)
#  p <- dend.dbh %>% filter(plot == "A2", tag == 92)
#  p2 <- dend.dbh.test %>% filter(plot == "A2", tag == 92)
p <- dend.dbh %>% filter(plot == "A3", tag == 65)
p2 <- dend.dbh.test %>% filter(plot == "A3", tag == 65)

ptitle <- paste(p$plot[1], p$tag[1], p$spp[1], sep = " ")
ggplot(p, aes(date, dbh.med, group = dend.num)) +geom_line() + geom_point(aes(date, DBH)) +labs(title = ptitle)
ggplot(p2, aes(date, dbh.med)) +geom_line() + labs(title = ptitle) +geom_point(data = p, aes(date, DBH))

q <- dend.dbh %>% filter(spp == "rm", tag < 100) %>% group_by(plot, tag, dend.num)
q2 <- dend.dbh.test %>% filter(spp == "rm", tag < 100) %>% group_by(plot, tag)

qtitle <- paste(q$spp[1], sep = " ")
ggplot(q, aes(date, dbh.med, group = interaction(plot,tag,dend.num), color = interaction(plot, tag))) +geom_line() + geom_point(aes(date, DBH)) +labs(title = qtitle)
ggplot(q2, aes(date, dbh.med, color = interaction(plot,tag))) +geom_line() + labs(title = qtitle) +geom_point(data = q, aes(date, DBH))


# ---------------------------------------------------------------------------------
#  create average growth increment curves for back-calc and gap filling between recruitment and dendroband installation
#  will have an avearage DBH increment

inc.list <- data.frame()
spp.list <- data.frame()

for(s in unique(dend.dbh$spp)){
  subset <- dend.dbh %>% filter(spp == s & mort == 0) %>% 
    group_by(plot, tag) %>% 
    mutate(dbh.inc = replace(dbh.med, !is.na(dbh.med), c(0, diff(na.omit(dbh.med))))) %>% 
    select(date, year, jday, plot, tag, dend.num, dbh.med, dbh.inc) %>%
    unite(plottag, plot, tag) %>% # simplify tree ID
    group_by(plottag, date) %>% 
    summarize(dbh.inc = mean(dbh.inc),dbh.med = mean(dbh.med)) %>% #  when there are multiple overlapping bands use the mean value
    ungroup()
    a <-1
  for(i in seq(from = 10, to = max(subset$dbh.med, na.rm=T), by= 10)){
    inc <- subset %>% group_by(plottag) %>% 
      filter(dbh.med >= i & dbh.med < i+10) %>%
      select(date, plottag, dbh.inc) %>% 
      group_by(date) %>% 
      summarize(dbh.inc.mean = mean(dbh.inc, na.rm=T)) %>% 
      mutate(spp = s, 
             dbh.class = i) %>% 
      ungroup()
    inc.list <- bind_rows(inc.list, inc)
    a<-a+1
  }
  spp.list <- bind_rows(spp.list, inc.list)
  inc.list <- data.frame()
}

growth.increments <- bind_rows(spp.list) %>% 
  group_by(spp, dbh.class) %>%
  mutate(cum.inc = cumsum(dbh.inc.mean))

any(duplicated(growth.increments))

ggplot(growth.increments, aes(date, dbh.inc.mean, group = interaction(spp, dbh.class), color = spp)) + geom_line()
growth.increments %>% ggplot(aes(date, cum.inc, group = dbh.class, color = as.factor(dbh.class))) + geom_line() +facet_wrap(~spp)



back_calc_new <- function(df, p, t){
  # requires tidyverse package to be loaded
  target.tree <- df[df$plot == p & df$tag == t, ]
  target.spp <- target.tree$spp[1]
  dbh.lo <- target.tree$dbh.med[1]-5
  dbh.hi <- target.tree$dbh.med[1]+5
  start.date <- target.tree$date[1]
  sw <- c("A1", "A2", "A3", "B1", "B2", "B3", "B4", "C1", "C2", "C3", "C4", "C5", "D1", "D2", "D3", "D4", "D5")
  if(target.tree$plot[1] %in% sw){ 
    subset <- df %>% filter(plot %in% sw, spp == target.spp, dbh.med > dbh.lo & dbh.med < dbh.hi)
  }else{
    subset <- df %>% filter(spp == target.spp, dbh.med > dbh.lo & dbh.med < dbh.hi)
  }
  inc <- subset %>% group_by(plot, tag) %>% 
    mutate(dbh.inc = replace(dbh.med, !is.na(dbh.med), c(0, diff(na.omit(dbh.med))))) %>% 
    select(date, dbh.inc) %>% 
    group_by(date) %>% 
    summarise(dbh.inc = mean(dbh.inc, na.rm=T)) %>% 
    mutate(dbh.inc = -1*dbh.inc) %>% 
    filter(date < start.date) %>% 
    mutate(dbh.inc.rev = rev(dbh.inc),
           inv.growth = rev(cumsum(dbh.inc.rev)),
           dbh.med = dbh.lo + 5 + inv.growth, 
           plot = p,
           tag = t) %>% 
    select(plot, tag, date, dbh.med)
  target.tree <- bind_rows(target.tree, inc)
}

p = "B4"
t = 726
a <- back_calc_new(dend.dbh.test, p, t)

ptitle <- paste(p, t, sep = " ")
ggplot(a, aes(date, dbh.med)) + geom_line()  +labs(title = ptitle)


rm(s)
rm(i)
# ---------------------------------------------------------------------------------
#  # mapping broom functions to extract linear models of circumference to dend.meas
#  dend.fit <- dend.dbh %>%
#    group_by(plot, tag, dend.num) %>% 
#    filter(!is.na(circ)) %>% 
#    filter(n() >= 3) %>% 
#    nest()
#  
#  
#  dend.fit <- dend.fit %>% 
#    mutate(mod = map(data, dend_mod),
#           tidy = map(mod, tidy),
#           glance = map(mod, glance),
#           augment = map(mod, augment),
#           rsq = map_dbl(glance, "r.squared")
#           )
#  
#  dend.tidy <- dend.fit %>% unnest(tidy) %>% 
#    select(plot, tag, dend.num, rsq, term, estimate) %>% 
#    spread(term, estimate) %>% 
#    rename(slope = dend.meas, int = `(Intercept)`) %>%
#    left_join(dend.fit[,1:4]) %>% 
#    unnest()
#  
#  #  pdf(file = "r.scripts/tgw 2017/EMS DBH Linear Fit 2017.pdf")
#  gg.models <- dend.tidy %>% ggplot(aes(rsq, slope)) 
#  gg.models + geom_point() +
#    geom_smooth(se=F) +
#    ylab("Dendroband-specific Linear Model Slope")
#  
#  rsq.hist <- dend.tidy %>% ggplot(aes(x=rsq))
#  rsq.hist + geom_histogram(binwidth = 0.025)+
#    xlab("Dendroband-specific Linear Model R^2")
#  
#  slope.hist <- dend.tidy %>% ggplot(aes(x=slope, colour = slope))
#  slope.hist + geom_histogram(binwidth = 0.01) +
#    xlim(-0.25,.25) +
#    xlab("Dendroband-specific Slope")
#  
#  dend.augment <- dend.fit %>% unnest(augment) %>% 
#    select(-c(10:12)) %>% 
#    left_join(unnest(dend.fit[,1:4]))
#  
#  
#  # ---------------------------------------------------------------------------------
# repeat linear models of DBH but relative to time
dbh.date.fit <- dend.dbh %>%
  group_by(plot,tag) %>%
  filter(!is.na(DBH)) %>%
  filter(n() >= 3) %>%
  mutate(rel.date = decimal_date(date) - decimal_date(min(date))) %>%
  nest()

date_mod <- function(df){
  return(lm(DBH ~ rel.date, data = df, na.action = na.omit))
}

dbh.date.fit <- dbh.date.fit %>%
  mutate(mod = map(data, date_mod),
         tidy = map(mod, tidy),
         glance = map(mod, glance),
         augment = map(mod, augment),
         rsq = map_dbl(glance, "r.squared")
  )

dbh.date.tidy <- dbh.date.fit %>% unnest(tidy) %>%
  select(plot, tag, rsq, term, estimate) %>%
  spread(term, estimate) %>%
  rename(slope = `rel.date`, int = `(Intercept)`) %>%
  left_join(dbh.date.fit[,1:3]) %>%
  unnest()

dbh.date.augment <- dbh.date.fit %>% unnest(augment) %>%
  right_join(unnest(dbh.date.fit[,1:3])) %>%
  select(-(.hat:.cooksd))

review.dbh <- dbh.date.augment %>% filter(abs(.std.resid)>2.5 | rsq < .5)


gg.mod.date <- dbh.date.tidy %>% ggplot(aes(rsq, slope))
gg.mod.date + geom_point()+
  geom_smooth(se=F)+
  xlab("Linear Model R^2")+
  ylab("Tree-Specific Slope")

rsq.date.hist <- dbh.date.tidy %>% ggplot(aes(x=rsq))
rsq.date.hist + geom_histogram(binwidth = 0.025) +
  xlab("Tree-specific Linear Model R^2")


slope.date.hist <- dbh.date.tidy %>% ggplot(aes(x=slope, colour = slope))
slope.date.hist + geom_histogram(binwidth = 0.05) +
  xlab("Tree-specific Linear Model Slope")
#  dev.off()

#  # ---------------------------------------------------------------------------------
#  # cumulative increment of dendrobands relative to cumulative increment in tree circumference
#  
#  dend.dbh.inc <- dend.dbh %>% 
#    group_by(plot, tag, dend.num) %>%
#    mutate(dend.inc = c(0,diff(dend.meas)),
#           dend.inc.sum = cumsum(dend.inc),
#           circ.inc = replace(circ, !is.na(circ), c(0,diff(na.omit(circ)))),
#           circ.inc.sum = replace(circ.inc, !is.na(circ.inc), cumsum(na.omit(circ.inc))),
#           med.circ = (dbh.med*10) * pi,
#           med.circ.inc = replace(med.circ, !is.na(med.circ), c(0,diff(na.omit(med.circ)))),
#           med.circ.inc.sum = replace(med.circ.inc, !is.na(med.circ.inc), cumsum(na.omit(med.circ.inc)))) %>% 
#    filter(!is.na(circ.inc.sum),
#           !is.na(dend.inc.sum)) %>%
#    mutate(n = n()) %>% 
#    filter(n >= 3) %>% 
#    nest()
#  
#  #  dend.overlap <- dend.dbh.inc %>% unnest() %>% 
#  #    group_by(plot, tag, date) %>% 
#  #    mutate(n = n()) %>% 
#  #    filter(n > 1) %>% 
#  #    filter(!is.na(med.circ.inc.sum)) %>% 
#  #    group_by(plot, tag) %>% 
#  #    nest()
#  
#  inc_mod <- function(df){
#    return(lm(circ.inc.sum ~ dend.inc.sum, data = df, na.action = na.omit))
#  }
#  
#  dend.dbh.inc.fit <- dend.dbh.inc %>%
#    mutate(mod = map(data, possibly(inc_mod, NA)),
#           tidy = map(mod, tidy),
#           glance = map(mod, glance),
#           augment = map(mod, augment),
#           rsq = map_dbl(glance, "r.squared")
#    )
#  
#  dend.dbh.inc.tidy <- dend.dbh.inc.fit %>% unnest(tidy) %>%
#    select(plot, tag, dend.num, rsq, term, estimate) %>%
#    spread(term, estimate) %>%
#    rename(slope = `dend.inc.sum`, int = `(Intercept)`) %>%
#    left_join(dend.dbh.inc.fit[,1:4]) %>% 
#    unnest()
#  
#  dend.dbh.inc.aug <- dend.dbh.inc.fit %>% unnest(augment) %>% 
#    left_join(unnest(dend.dbh.inc[,1:4]))
#  
#  
#  
#  
#  gg.mod.inc <- dend.dbh.inc.tidy %>% ggplot(aes(rsq, slope)) 
#  gg.mod.inc + geom_point()+
#    geom_smooth(se=F)+
#    xlab("Linear Model R^2")+
#    ylab("Tree-Specific Slope")
#  
#  rsq.inc.hist <- dend.dbh.inc.tidy %>% ggplot(aes(x=rsq))
#  rsq.inc.hist + geom_histogram(binwidth = 0.05) +
#    xlab("Tree-specific Linear Model R^2")
#  
#  
#  slope.inc.hist <- dend.dbh.inc.tidy %>% ggplot(aes(x=slope, colour = slope))
#  slope.inc.hist + geom_histogram(binwidth = .05) +
#    xlab("Tree-specific Linear Model Slope")
#  
#  
#  
#  # ------------------------------------------------------------------------------------------------------------------------------
#  # Create dataset to visualize timing of spring start of growth/rate of change
#  #  gg.dend.dbh <- dend.dbh %>%
#  #    group_by(plot, tag, dend.num) %>% 
#  #    mutate(dend.lag = dend.meas - lag(dend.meas, 1)) %>% 
#  #    ggplot(aes(x = jday, y = dend.lag, colour = factor(year)))
#  #  # compare to average date of budbreak using HF Archive phenology data
#  #  phen <- read.csv(file = "C:/Users/tgw475/Desktop/Whitby HF Files/Online Datasets/hf003-06-mean-spp.csv", na.strings = "NA")
#  #  phen <- phen %>% group_by(year) %>% 
#  #    summarize_all(funs(mean(., na.rm = T))) %>% 
#  #    select(-species) %>% 
#  #    filter(year %in% c(1998:2014))
#  #  gg.phen <- ggplot(phen, aes(y = bb.jd, x = "Bud Break 50%", group = factor(year), colour = factor(year)))
#  #  gg.phen + geom_hline(aes(yintercept = bb.jd, colour = factor(year))) +
#  #    ylim(0,365) + ggtitle("All-species Average Spring Phenology") +
#  #    scale_color_hue()
#  #  
#  #  gg.dend.dbh +
#  #    geom_point(alpha = 0.5, shape = 1)+
#  #    scale_color_hue()
#  #  gg.dend.dbh +
#  #    stat_summary(fun.y = mean, geom = "point", shape = 1, aes(group = year)) +
#  #    stat_summary(fun.y = mean, geom = "line", alpha = 0.8, linetype = "longdash", aes(colour = factor(year), group = year)) +
#  #    geom_vline(data = phen, aes(xintercept = bb.jd, colour = factor(year)), show.legend = F, alpha = 0.5) +
#  #    scale_color_hue() +
#  #    xlim(75,250)
#  #    

