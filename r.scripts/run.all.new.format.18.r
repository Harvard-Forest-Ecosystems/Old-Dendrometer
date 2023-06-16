# set working directory to run on EFT ---------------------------------------------------------------------------------
# if (Sys.info()[["nodename"]] == "eft") {
#   dir <- "/DATA/HF_DATA/HF_BIOMETRY_WOFSY/"
# } else {
#   dir <- "//eft.as.harvard.edu/DATA/HF_DATA/HF_BIOMETRY_WOFSY/"
#   
# }
# Local directory for TGW 
dir <- "C:/Users/tgw475/Desktop/Whitby HF Files/HF_BIOMETRY_WOFSY copy/"

setwd(dir)

source("trees/r.scripts/new.dbh.make.r")
source("trees/r.scripts/calc.kgC.r")
source("trees/r.scripts/tgw 2017/calc.kgc.tgw.r")
source("R.scripts/old.functions.r")
source("trees/s.scripts/CI.calc.txt")

library(tidyverse)
library(ggplot2)
library(lubridate)
library(zoo)

# read in data files ---------------------------------------------------------------------------------------------------

dend.data <- read.table("trees/data/ems.dend.all.txt", stringsAsFactors = F, header = T, as.is = T) %>% 
  rbind(tow.17.dend, tow.18.dend) %>% arrange(plot, tag, year, jday) %>% 
  mutate(ifelse(dend.meas == 0, NA, dend.meas))
dbh.data <- read.table("trees/data/ems.dbh.all.txt", stringsAsFactors = F, header = T, as.is = T) %>% 
  rbind(tow.17.dbh, tow.18.dbh) %>% arrange(plot, tag, year, jday)
# 1993 has only one DBH tape measurement, unknown collection date
tow.93.dbh <- read.table("trees/data/dendrometer/1993/tow.93.dbh.txt", header = T, sep = "\t", as.is = T) %>%
  mutate(year = 1993, 
         jday = 364/2) %>% 
  filter(!plot %in% c("A4", "A5", "B5", "G3", "H3", "H4")) %>%  # take out flooded and harvest plots
  select(plot, tag, year, jday, DBH = dbh.tape)
spp.data <- read.table("trees/data/ems.spp.all.txt", stringsAsFactors = F, header = T, sep = "\t", as.is = T)

dend.data$date <- as_date(strptime(paste(dend.data$year, dend.data$jday), "%Y %j"))
dbh.data$date <- as_date(strptime(paste(dbh.data$year, dbh.data$jday), format = "%Y %j"))



# data entry errors and outliers to correct -----------------------------------------------------------------------------

# F3 12 skipped dend.num 2
dend.data$dend.num[dend.data$plot == "F3" & dend.data$tag == 12 & dend.data$dend.num > 1 & !is.na(dend.data$dend.num)] <- 
  dend.data$dend.num[dend.data$plot == "F3" & dend.data$tag == 12 & dend.data$dend.num > 1 & !is.na(dend.data$dend.num)] - 1
# C1 351 remove redundant dend.num 4 installed incorrectly and removed w/in a year
dend.data <- dend.data[!(dend.data$plot == "C1" & dend.data$tag == 351 & dend.data$dend.num == 4), ]
# rename dend.num 5 to dend.num 4
dend.data$dend.num[dend.data$plot == "C1" & dend.data$tag == 351 & dend.data$dend.num == 5] <- 4
# C3 425 remove 5cm too low dbh in 2000
dbh.data <- dbh.data[!(dbh.data$plot == "C3" & dbh.data$tag == 425 & dbh.data$DBH == 15.54), ]
# G4 258 high growth 6-8-04
dend.data$dend.meas[dend.data$plot == "G4" & dend.data$tag == 258 & dend.data$year == 2004 & dend.data$jday == 160] <- 25.47
# F4 55 bb high DBH removed 7-30-2000
dbh.data <- dbh.data[!(dbh.data$plot == "F4" & dbh.data$tag == 55 & dbh.data$year == 2001 & dbh.data$DBH == 29.4), ]
# C1 225 Beech, exceptionally high
dbh.data <- dbh.data[!(dbh.data$plot == "C1" & dbh.data$tag == 225 & dbh.data$year == 2012 & dbh.data$DBH == 51.7), ]
# A2 92 has two abnormally high DBH after switching to dend # 2
dbh.data <- dbh.data[!(dbh.data$plot == "A2" & dbh.data$tag == 92 & dbh.data$year > 2010 & dbh.data$year < 2017), ]
# D1 531 has a calculated DBH (not recorded in datasheets) when switching to dend # 2
dbh.data <- dbh.data[!(dbh.data$plot == "D1" & dbh.data$tag == 531 & dbh.data$DBH == 33.98), ]
# B4 263 has a high outlier in 2000 'doubleband'
dbh.data <- dbh.data[!(dbh.data$plot == "B4" & dbh.data$tag == 263 & dbh.data$DBH == 17.21), ]
# E1 678 dend.num 2 had spurious data in 2013 before being continually measured in 2015
dend.data <- dend.data[!(dend.data$plot == "E1" & dend.data$tag == 678 & dend.data$dend.num == 2 & dend.data$year == 2013), ]
# F3 737 has outlier high DBH in 2014 15.7
dbh.data <- dbh.data[!(dbh.data$plot == "F3" & dbh.data$tag == 737 & dbh.data$DBH == 15.7), ]
# G5 289 new hole punched 05-03-18 for dend.num 3. Do not use previous data
dend.data <- dend.data[!(dend.data$plot == "G5" & dend.data$tag == 289 & dend.data$dend.num == 3 & dend.data$year < 2018), ]
# B4 276 12-12-2013 erroneously high dend measurement, carry forward previous measurement
dend.data$dend.meas[dend.data$plot == "B4" & dend.data$tag == 276 & dend.data$date == "2013-12-12" ] <- 36.19
# G5 90 inside hole measured in mid 2015. Notes on 2018-05-03 say "outer hole 89.40, inner hole 43.58,  
  # Previous measurements were only inner hole; they were converted to outer by the difference between these two"
dend.data$dend.meas[dend.data$plot == "G5" & dend.data$tag == 90 & (dend.data$date == "2015-06-22" | dend.data$date == "2015-08-27")] <- 
  dend.data$dend.meas[dend.data$plot == "G5" & dend.data$tag == 90 & (dend.data$date == "2015-06-22" | dend.data$date == "2015-08-27")] + 45.97
# G5 90 used bad conversion between inner and outer hole fixed using differecnce measured in 2018: see previous
dend.data$dend.meas[dend.data$plot == "G5" & dend.data$tag == 90 & dend.data$dend.meas == 93.46] <- 36.77 + 45.97
dend.data$dend.meas[dend.data$plot == "G5" & dend.data$tag == 90 & dend.data$dend.meas == 94.03] <- 37.64 + 45.97 
# H2 347 typo in data entry: low value 2015-08-27
dend.data$dend.meas[dend.data$plot == "H2" & dend.data$tag == 347 & dend.data$date == "2015-08-27"] <- 23.13
# F1 933 typo in data entry: low value 2012-06-27
dend.data$dend.meas[dend.data$plot == "F1" & dend.data$tag == 933 & dend.data$date == "2012-06-27"] <- 79.13
# A1 19 dend.num 3 installed in late 2009, had NA in 2010, simply removed first measurement to avoid gap in record
dend.data <- dend.data[!(dend.data$plot == "A1" & dend.data$tag == 19 & dend.data$dend.num == 3 & dend.data$year == "2009"),]
# F2 987 typo in data entry: high value 2014-12-03
dend.data$dend.meas[dend.data$plot == "F2" & dend.data$tag == 987 & dend.data$date == "2014-12-03"] <- 32.49
# F5 133 typo in data entry: high value 2013-12-12
dend.data$dend.meas[dend.data$plot == "F5" & dend.data$tag == 133 & dend.data$date == "2013-12-12"] <- 46.9
# C4 498 typo in data entry: high value 2011-05-10
dend.data$dend.meas[dend.data$plot == "C4" & dend.data$tag == 498 & dend.data$date == "2011-05-10"] <- 23.58
# E2 708 band.num 1 broke and was missing for most of 2004 and 2005, is filled as band 2 ==0 should be band.num 1 and fill previous measurement forward
dend.data$dend.num[dend.data$plot == "E2" & dend.data$tag == 708 & dend.data$date < "2005-11-02"] <- 1
dend.data$dend.meas[dend.data$plot == "E2" & dend.data$tag == 708 & dend.data$date < "2005-11-02" & dend.data$date > "2004-04-12"] <- 32.99
# C4 570 first dend.num skips to 2, and is renamed to 1 prior to 2017, when actual second band installed
dend.data$dend.num[dend.data$plot == "C4" & dend.data$tag == 570 & dend.data$year < 2017] <- 1
# D1 532 has too long overlap between bands, and error at end of dend.num 3
dend.data <- dend.data[!(dend.data$plot == "D1" & dend.data$tag == 532 & dend.data$dend.num == 3 & dend.data$year > 2015),] # dendmeas was >130mm which is much bigger than needed
dend.data <- dend.data[!(dend.data$plot == "D1" & dend.data$tag == 532 & dend.data$dend.num == 4 & dend.data$date < "2014-06-10"),] # removed first year of dend 4
# F3 7 dend.num 3 typo in data entry: low value 2016-04-22, and too much dend overlap
dend.data$dend.meas[dend.data$plot == "F3" & dend.data$tag == 7 & dend.data$dend.num == 3 & dend.data$date == "2016-04-22"] <- 36.92
dend.data <- dend.data[!(dend.data$plot == "F3" & dend.data$tag == 7 & dend.data$dend.num == 2 & dend.data$year > 2016),] # dendmeas was >130mm which is much bigger than needed
dend.data <- dend.data[!(dend.data$plot == "F3" & dend.data$tag == 7 & dend.data$dend.num == 3 & dend.data$year < 2016),] # removed first year of dend 4
# F5 114 dend.num 1/3 long overlap, shinking on 2 for first measuremtns, erase in 2015
dend.data <- dend.data[!(dend.data$plot == "F5" & dend.data$tag == 114 & dend.data$dend.num == 2 & dend.data$year < 2016),]

# Translate last dend date forward when bands switch if there is no overlap ---------------------------------------
obs.dates <- unique(dend.data$date)
obs.trees <- unique(dend.data[ , c("plot", "tag")])
tmp2 <- dend.data[0, ]

for(i in 1:nrow(obs.trees)){
  tmp <- dend.data[dend.data$plot == obs.trees$plot[i] & dend.data$tag == obs.trees$tag[i], ]
  max.date <- max(tmp$date)
  min.date <- min(tmp$date)
  tree.dates <- data.frame(obs.trees[i, 1:2], date = obs.dates[obs.dates >= min.date & obs.dates <= max.date])
  band.count <- na.omit(unique(tmp$dend.num))
  if(length(band.count)>1){
    for(bnd in 1:(length(band.count)-1)){
      date.end <- max(tmp$date[tmp$dend.num == bnd & !is.na(tmp$dend.meas)], na.rm = T)
      date.start <- min(tmp$date[tmp$dend.num == bnd+1 & !is.na(tmp$dend.meas)], na.rm = T)
      # if(date.end < date.start & (date.end - date.start) < 300){ #copy forward if no overlap and gap is <300 days
      #   tmp.row <- tmp[tmp$dend.num == bnd & tmp$date == date.end, ]
      #   tmp.row$date <- date.start
      #   tmp <- bind_rows(tmp.row, tmp)
      # }
      if(date.end > date.start & abs(date.start - date.end) > 180){ #delete first 90 days of new dend if overlap is > 180 days
        tmp <- tmp[!(tmp$dend.num == bnd + 1 & tmp$date < date.start + 90), ]
      }
    }
  }
  tmp <- full_join(tmp, tree.dates, by = c("plot", "tag", "date"))
  tmp2<- rbind(tmp2, tmp)
}

nrow(tmp2) - nrow(dend.data)
str(tmp2)
tmp2 <- as.data.frame(arrange(tmp2, plot, tag, date, dend.num))
# 387 missing observations and dendrometer switch values added to dataset
filled.rows <- anti_join(tmp2, dend.data, by = c("plot", "tag", "date", "dend.num")) %>% arrange(plot, tag, date, dend.num)

# ---------------------------------------------------------------------------------
#  Use zoo::na.approx to interpolate missing dend.meas values within a contiguous dendroband record. Leading and trailing NA's will be left as is

dend.data <- tmp2 %>% arrange(plot, tag, date, dend.num) %>%
  group_by(plot, tag) %>%
  mutate(year = ifelse(is.na(year), as.numeric(as_date(date, format = "%Y")), year),
         jday = ifelse(is.na(jday), as.numeric(as_date(date, format = "%j")), jday),
         dend.num = na.locf(dend.num, na.rm=F), # carry forward dend.num to empty obs dates first
         dend.num = na.locf(dend.num, na.rm=F, fromLast = T)) %>% #  then carry backwards to fill beginning of record
  group_by(plot, tag, dend.num) %>%
  mutate(dend.meas = na.approx(dend.meas, date, na.rm = F))

dend.data <- as.data.frame(dend.data)
# tested with new column at first and compared values
# there were 21 filled cases, where NA's had flanking values on either side to interpolate from
# length(which(is.na(dend.dbh$dend.meas) & !is.na(dend.dbh$dend.meas2)))

# DBH data reduction ----------------------------------------------------------------------------------------------------

# loop filters out ref DBH within the first 3 months of dendroband record
# if there are at least 2 ref DBH's for that band, as the band
# is most likely still loose in this time period

obs.trees <- unique(dend.data[, c("plot", "tag")])

a <- length(dbh.data$DBH[!is.na(dbh.data$DBH)])
start <- Sys.time()

for (i in 1:nrow(obs.trees)) {
  tree <- dend.data[dend.data$plot == obs.trees$plot[i] & dend.data$tag == obs.trees$tag[i], ]
  for (d in 1:length(!is.na(unique(tree$dend.num)))) {
    band <- tree[tree$dend.num == d, ]
    band.range <- c(min(band$date), max(band$date))
    refs <- dbh.data$date[dbh.data$plot == tree$plot[1] & dbh.data$tag == tree$tag[1] & dbh.data$date >= band.range[1] & 
                            dbh.data$date <= band.range[2]]
    if (length(refs) > 2) {
      min.ref <- min(refs)
      if (as.numeric(min.ref - band.range[1]) < 90) {
        dbh.data$DBH[dbh.data$plot == tree$plot[1] & dbh.data$tag == tree$tag[1] & dbh.data$date == min.ref] <- NA
      }
    }
  }
}
dbh.data <- dbh.data[!is.na(dbh.data$DBH), ]
b <- length(dbh.data$DBH[!is.na(dbh.data$DBH)])
end <- Sys.time()
a - b
start - end
# there are 378 early dbh records that will not be used

# Mortality and recruitment data ---------------------------------------------------------------------------------

years <- as.character(list.files("trees/data/dendrometer"))

recruit.tow <- list()
dead.tow <- list()
for (year in years) {
  yr <- substr(year, 3, 4)
  fl <- paste("trees/data/dendrometer/", year, "/tow.", yr, ".mort.txt", sep = "")
  if (file.exists(fl)) {
    tmp <- read.table(file = fl, header = T)
    dead.tow[[year]] <- tmp
  }
  fl <- paste("trees/data/dendrometer/", year, "/tow.", yr, ".recruits.txt", sep = "")
  if (file.exists(fl)) {
    tmp <- read.table(file = fl, header = T)
    recruit.tow[[year]] <- tmp
  }
}
# years that there is data for the cut plots
cut.years <- c("1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007")
dead.cut <- list()
recruit.cut <- list()
for (year in cut.years) {
  yr <- substr(year, 3, 4)
  fl <- paste("trees/data/dendrometer/", year, "/cut.", yr, ".mort.txt", sep = "")
  if (file.exists(fl)) {
    tmp <- read.table(file = fl, header = T)
    dead.cut[[year]] <- tmp
  }
  fl <- paste("trees/data/dendrometer/", year, "/cut.", yr, ".recruits.txt", sep = "")
  if (file.exists(fl)) {
    tmp <- read.table(file = fl, header = T)
    recruit.cut[[year]] <- tmp
  }
}

# remove trees in dendroband data after they are recorded as dead
for (year in years) {
  dead.trees <- dead.tow[[year]]
  if (!is.null(dead.trees)) {
    for (i in 1:nrow(dead.trees)) {
      dend.data <- dend.data[!(dend.data$plot == dead.trees[i, 1] & dend.data$tag == dead.trees[i, 2] & 
                                 dend.data$year > as.numeric(year)), ]
      dbh.data <- dbh.data[!(dbh.data$plot == dead.trees[i, 1] & dbh.data$tag == dead.trees[i, 2] & 
                               dbh.data$year > as.numeric(year)), ]
    }
  }
}
# remove trees in dendroband data if they are present before recruitment year
for (year in years) {
  rec.trees <- recruit.tow[[year]]
  if (!is.null(rec.trees)) {
    for (i in 1:nrow(rec.trees)) {
      dend.data <- dend.data[!(dend.data$plot == rec.trees[i, 1] & dend.data$tag == rec.trees[i, 2] & 
                                 dend.data$year < as.numeric(year)),]
      dbh.data <- dbh.data[!(dbh.data$plot == rec.trees[i, 1] & dbh.data$tag == rec.trees[i, 2] & 
                               dbh.data$year < as.numeric(year)), ]
    }
  }
}

# DBH calculations ----------------------------------------------------------------------------------------------

# initialize loop with tree list and empty data.frame
trees <- as.data.frame(unique(dend.data[, c("plot", "tag")]))
dend.dates <- sort(unique(dend.data$date))
spp.col <- as.character(apply(trees, 1, function(x) {
  spp.data$spp[spp.data$plot == as.character(x[1]) & spp.data$tag == as.numeric(x[2])]
}))
tmp <- data.frame(matrix(NA, ncol = length(dend.dates), nrow = nrow(trees)))
all.dbh <- data.frame(trees, spp.col, tmp)
names(all.dbh) <- c("plot", "tag", "spp", as.numeric(dend.dates))

# this takes over 5 minutes to process

start <- Sys.time()
pdf(file = "trees/median_dbh_plots.pdf", paper = "USr")

for (i in 1:nrow(trees)) {
  tree.name <- paste(trees[i,1], trees[i,2], sep = "-")
  dbh <- dbh.data[dbh.data$plot == trees[i, 1] & dbh.data$tag == trees[i, 2], ]
  dend <- dend.data[dend.data$plot == trees[i, 1] & dend.data$tag == trees[i, 2], ]
  x.vals <- unique(dend$date)
  x.vals <- sort(x.vals)
  band.nums <- unique(dend$dend.num)
  all.estimates <- as.data.frame(matrix(NA, nrow = length(x.vals), ncol = nrow(dbh) * max(band.nums)))
  col.num <- 1
  # fill in missing data when bands switch
  if (length(band.nums) > 1) {
    for (bnd in 1:(length(band.nums) - 1)) {
      last.measure <- max(dend$date[dend$dend.num == band.nums[bnd]])
      first.measure.of.next <- min(dend$date[dend$dend.num == band.nums[bnd + 1]])
      if (last.measure < first.measure.of.next & as.numeric(first.measure.of.next) - as.numeric(last.measure) < 300 * 24 * 60 * 
        60) {
        tmp.row <- dend[dend$date == first.measure.of.next & dend$dend.num == band.nums[bnd + 1], ]
        tmp.row$dend.num <- band.nums[bnd]
        tmp.row$dend.meas <- dend$dend.meas[dend$date == last.measure & dend$dend.num == band.nums[bnd]]
        dend <- rbind(dend, tmp.row)
      }
    }
  }
  dend <- dend[order(dend$dend.num, dend$date),]
  for (band in band.nums) {
    # the rows for band
    ind <- dend$dend.num == band
    if (sum(ind) > 1) {
      # date range for band
      band.rng <- range(dend$date[ind])
      # which dbh measurement were during date range for band i
      dbh.in.rng <- dbh$date >= (band.rng[1]) & dbh$date <= (band.rng[2])
      # add first dbh measurement since dbh and dend readings were done on different days in 1998
      if (band == 1 & dbh[1, "year"] == 1998) {
        dbh.in.rng[1] <- T
      }
      band.measures <- dend[ind, "dend.meas"]
      band.dates <- dend[ind, "date"]
      if (any(dbh.in.rng)) {
        for (j in which(dbh.in.rng)) {
          closest <- which.min((abs(band.dates - dbh$date[j])))
          ref.cal <- band.measures[closest]
          tmp <- new.dbh.make(band.measures, rep(dbh$DBH[j], length(band.measures)), rep(ref.cal, length(band.measures)))
          all.estimates[x.vals %in% band.dates, col.num] <- tmp
          names(all.estimates)[col.num] <- paste("band", band, "dbh", j, sep = ".")
          col.num <- col.num + 1
        }
      }
    } else {
      band.nums <- band.nums[-which(band.nums == band)]  #remove band from list if there's only 1 reading
    }
  }
  # trim empty columns from all.estimates
  all.estimates <- all.estimates[, 1:(col.num - 1)]
  if (!is.null(ncol(all.estimates))) {
    # fill gaps - only works if overlapping data is added from the paper record
    if (any(duplicated(dend$date))) {
      for (l in 1:ncol(all.estimates)) {
        # look at each column of estimates
        tmp <- strsplit(names(all.estimates)[l], ".", fixed = T)[[1]]
        band <- as.numeric(tmp[2])
        dbh.measure <- as.numeric(tmp[4])
        # check if this column has NA values which need to be filled at the end look at each future band to fill in vlaues
        if (is.na(tail(all.estimates[, l], 1))) {
          for (ref.band in (band + 1):max(band.nums)) {
            # check for duplicate calculations in other columns
            # if (!any(names(all.estimates) == paste("band.", ref.band, ".dbh.", dbh.measure, sep = ""))) {
              ref.ind <- tail(which(!is.na(all.estimates[, l])), 1)
              ref.dbh <- all.estimates[ref.ind, l]
              ref.date <- x.vals[ref.ind]
              ref.dend <- dend[dend$dend.num == ref.band & dend$date == ref.date, "dend.meas"]
              for (m in (ref.ind + 1):nrow(all.estimates)) {
                # for each NA value in column
                fill.date <- x.vals[m]
                fill.dend <- dend[dend$dend.num == ref.band & dend$date == fill.date, "dend.meas"]
                if (length(fill.dend) == 1 & length(ref.dend) == 1) {
                  all.estimates[m, l] <- new.dbh.make(fill.dend, ref.dbh, ref.dend)
                }
              }
            # }
          }
        }
        # check if this column has NA values which need to be filled at the beginning. look at each previous band to fill in vlaues check for
        if (is.na(head(all.estimates[, l], 1))) {
          # duplicate calculations in other columns
          for (ref.band in (band - 1):min(band.nums)) {
            # if (!any(names(all.estimates) == paste("band.", ref.band, ".dbh.", dbh.measure, sep = ""))) {
              ref.ind <- head(which(!is.na(all.estimates[, l])), 1)
              ref.dbh <- all.estimates[ref.ind, l]
              ref.date <- x.vals[ref.ind]
              ref.dend <- dend[dend$dend.num == ref.band & dend$date == ref.date, "dend.meas"]
              for (m in (ref.ind - 1):1) {
                # for each NA value in column
                fill.date <- x.vals[m]
                fill.dend <- dend[dend$dend.num == ref.band & dend$date == fill.date, "dend.meas"]
                if (length(fill.dend) == 1 & length(ref.dend) == 1) {
                  all.estimates[m, l] <- new.dbh.make(fill.dend, ref.dbh, ref.dend)
                  
                }
              }
            # }
          }
        }
        
        # matplot(x.vals, all.estimates[, l], col = dbh.measure)
      }
    }
    # fix problem on overlapping bands having extra weight in average calculation
    median.estimate <- apply(all.estimates, 1, function(x) {median(x, na.rm = T)})
  } else {
    median.estimate <- all.estimates
  }
  if (!is.na(median.estimate)) {
    all.dbh[i, as.character(as.numeric(x.vals))] <- median.estimate
    
    matplot(as.POSIXct(x.vals), all.estimates, main = tree.name, ylab = "DBH", xlab = "Date", xaxt = "n", type = "l")
    axis.POSIXct(1, as.POSIXct(x.vals))
    lines(as.POSIXct(x.vals), median.estimate, lwd = 5)
    points(as.POSIXct(dbh$date), dbh$DBH, pch = 2, cex = 2)
    
  } else {
    all.dbh[i, as.character(as.numeric(dbh$date))] <- dbh$DBH
  }
  
}
dev.off()
end <- Sys.time()
start - end

# Incorporate 1993 DBH tape measurements -----------------------------------------------------------------------------

# I arbitratily chose the midpoint of 1993 for jday
names(tow.93.dbh) <- c("plot", "tag", "year", "jday", as.POSIXct("1993-182", format = "%Y-%j", tz = "EST", origin = "1970-1-1"))
# update dend.dates since it now has 1993 at beginning
dend.dates <- c(as.POSIXct("1993-182", format = "%Y-%j", tz = "EST", origin = "1970-1-1"), dend.dates)
all.dbh <- full_join(all.dbh, tow.93.dbh[, c(1, 2, 5)])
#1993 data are at end of record. Need to resort the columns chronologically then reconvert to character column names
all.dbh <- all.dbh[, c("plot", "tag", "spp", as.character(as.numeric(sort(as.POSIXct(as.numeric(colnames(all.dbh[-1:-3])), 
                                                                                     origin = "1970-1-1")))))]
# create all.dbh.dates since they no longer correspond with dend.dates
dend.dates <- as.POSIXlt(as.numeric(colnames(all.dbh[-1:-3])), origin = "1970-1-1")

# Back calculate DBH for large recruits ------------------------------------------------------------------------------

# B4 726 was recruited in 2004, and C1 217, D1 266 & H1 204 were recruited in 2006 all with DBHs too large to have grown in since the
# previous recruitment surveys they were probably missed b/c they were near the plot border or b/c they looked dead but weren't we
# must back-calculate their DBHs through 1993 & insert them into the DBH & kgC data from previous years so as to be included in all
# annual summaries

back.calc <- function(pl, tg, spp, start.year, end.year, min.size, max.size, transects = "NONE") {
  dbh.subset <- all.dbh[, 3 + which((dend.dates$year + 1900) %in% start.year:end.year)]
  pik.years <- apply(dbh.subset, 1, function(x) {!any(is.na(x))} )
  pik.spp <- all.dbh$spp == spp
  pik.size <- dbh.subset[, ncol(dbh.subset)] >= min.size & dbh.subset[, ncol(dbh.subset)] <= max.size
  dbh.sub <- all.dbh[pik.years & pik.spp & pik.size, ]
  
  if (transects == "SW") {
    dbh.sub <- dbh.sub[grep("[A-D][1-5]", dbh.sub$plot), ]  # look at just plots in the SW transect
  }
  
  ref.date <- which(!is.na(all.dbh[all.dbh$plot == pl & all.dbh$tag == tg, -(1:3)]))[1] + 3
  normalized.growth <- (dbh.sub[, -(1:3)] - dbh.sub[, ref.date])/dbh.sub[, ref.date]  #normalized growth for similar trees
  target.tree <- all.dbh[all.dbh$plot == pl & all.dbh$tag == tg, ]
  first.dbh <- as.numeric(target.tree[ref.date])
  norm.target <- (target.tree[-(1:3)] - first.dbh)/first.dbh  #normalized growth for target tree
  
  tmp <- apply(normalized.growth, 1, function(x) {x - norm.target} )
  tmp <- matrix(unlist(tmp), nrow = length(dend.dates), byrow = F)
  tmp <- abs(apply(tmp, 2, sum, na.rm = T))
  wts <- abs(tmp - max(tmp))/max(tmp)  #weights to apply to similar trees based on how similar their growth rates were
  
  # matplot(as.numeric(dend.dates), t(normalized.growth)) lines(as.numeric(dend.dates), norm.target, lwd = 2)
  # lines(as.numeric(dend.dates), apply(normalized.growth, 2, weighted.mean, w = wts^2), col = 1, lty = 2, lwd = 2)
  
  modeled.tree <- apply(normalized.growth, 2, weighted.mean, w = wts^2)
  modeled.tree[!is.na(norm.target)] <- norm.target[!is.na(norm.target)]
  modeled.tree <- modeled.tree * first.dbh + first.dbh  #use model for time when there is no data for tree
  
  # plot(as.numeric(dend.dates), modeled.tree)
  all.dbh[all.dbh$plot == pl & all.dbh$tag == tg, -(1:3)] <- modeled.tree
  # modeled.tree
}

# calc a growth rate to apply to C1 217
back.calc("B4", 726, "ro", 1993, 2003, 30, 40, "SW")
back.calc("C1", 217, "hem", 1993, 2005, 25, 35)
back.calc("D1", 266, "wp", 1993, 2005, 40, 50)
back.calc("H1", 204, "rm", 1993, 2005, 12, 16)

# fix recruitments with missing data and extra data -------------------------------------------------------------------------

years.rec <- names(recruit.tow)
for (year in years.rec) {
  rec.ind <- splus.is.element(all.dbh[, 1:2], recruit.tow[[year]][, 1:2])
  year.ind <- 3 + which(dend.dates$year + 1900 == as.numeric(year))
  missing.recruits <- apply(all.dbh[, year.ind], 1, function(x) {all(is.na(x))} ) & rec.ind
  if (any(missing.recruits) & (as.numeric(year) < max(as.numeric(years.rec)))) {
    # whenever dendroband data is missing during year of recruitment, 
    # take the smallest reading present in the record following the year of recruitment
    all.dbh[missing.recruits, max(year.ind)] <- apply(all.dbh[missing.recruits, 1:ncol(all.dbh) >
                                                                max(year.ind)], 1, min, na.rm = T)
  }
  # whenever there is dendroband data present prior to date of recruitment set to NA
  all.dbh[rec.ind, 4:(min(year.ind) - 1)] <- NA
}

# for current year recruits without dbh at end of record(because no dend and DBH overlap to use in calc.DBH), 
# fill with previous, non-NA value
year = "2017"
rec.ind <- splus.is.element(all.dbh[, 1:2], recruit.tow[[year]][, 1:2])
year.ind <- 3 + which(dend.dates$year + 1900 == as.numeric(year))
recs <- all.dbh[rec.ind, ]
for (r in 1:nrow(recs)) {
  for (c in 5:ncol(recs)) {
    recs[r, c] <- ifelse(is.na(recs[r, c]) & !is.na(recs[r, c - 1]), recs[r, c - 1], recs[r, c])
  }
}
all.dbh[rec.ind, ] <- recs

# Create long format to merge with HF069 archive file for trees---------------------------------------------------------------

all.dbh.long <- all.dbh %>% group_by(plot, tag) %>% 
  gather(date, dbh, c(4:ncol(all.dbh)), na.rm = T, convert = T) %>% 
  mutate(date = as.POSIXct(as.numeric(date), origin = "1970-1-1"),
         plottag = paste(plot, tag, sep="-"),
         year = as.numeric(format(date, "%Y")),
         jday = as.numeric(format(date, "%j")), 
         site = "ems", 
         tree.type = "live", 
         dbh = round(dbh, 3), 
         KgC = calc_kgc(spp, dbh),
         BA = pi*((dbh/100)/2)^2,
         date = format(date, "%Y-%m-%d")) %>% 
  select(date, year, doy = jday, tree.type, site, plot, plottag, tag, species = spp, dbh, KgC, BA)

# label 'tree.type' as dead for trees in the year they were recorded in dead.tow, and recruits similarly
years <- unique(dend.data$year)
dead.ids <- NULL
rec.ids <- NULL
for (year in years) {
  mort <- dead.tow[[as.character(year)]]
  rec <- recruit.tow[[as.character(year)]]
  if (!is.null(mort)) {
    for (i in 1:nrow(mort)) {
      # have to do loop for each row in data.frame because of duplicate tag #'s
      tmp <- which(all.dbh.long$year == year & all.dbh.long$plot == mort$plot[i] & all.dbh.long$tag == mort$tag[i])
      dead.ids <- c(dead.ids, tmp)
    }
  }
  if (!is.null(rec)) {
    for (i in 1:nrow(rec)) {
      tmp <- which(all.dbh.long$year == year & all.dbh.long$plot %in% rec$plot[i] & all.dbh.long$tag %in% rec$tag[i])
      rec.ids <- c(rec.ids, tmp)
    }
  }
}
all.dbh.long$tree.type[dead.ids] <- "dead"
all.dbh.long$tree.type[rec.ids] <- "recruit"

rm(mort, rec, dead.ids, rec.ids, tmp)

ems.HF069 <- all.dbh.long %>% mutate(nindivs = NA) %>% 
  select(date, year, doy, tree.type, site, plot, tag, species, dbh)

write.csv(all.dbh.long, "trees/ems.all.dbh.long.csv", quote = F, row.names = F)

dead.tree.names.recent <-  unique(all.dbh.long$plottag[all.dbh.long$tree.type == "dead" & all.dbh.long$year >=2015])

trees.recently.dead <- all.dbh.long %>% group_by(plot, tag, year) %>%
  mutate(last = doy == max(doy),
         plottag = paste(plot, tag, sep = "-")) %>% 
  filter(tree.type == "dead", year >= 2015, last == T) %>% 
  select(plot, tag, plottag, species, year, dbh) %>% 
  arrange(year, plot, tag)

write.csv(trees.recently.dead, "../ems.recently.dead.trees.csv", quote = F, row.names = F)

# Graphs to see how the median dbh function is working -----------------------------------------------------------------

# make temporary data sets for plotting
p <- all.dbh.long %>% filter(plottag %in% dead.tree.names.recent) %>% 
  mutate(date = as_date(date), dbh.class = floor(dbh/5)*5)
dbh.data$date <- as_date(dbh.data$date)
p2 <- dbh.data %>% mutate(plottag = paste(plot, tag, sep="-"),
                                          dbh.class = floor(DBH/5)*5)
p2 <- p2 %>% filter(plottag %in% dead.tree.names.recent) %>% 
  mutate(date = as_date(date))

# ptitle <- paste(p$plot, p$tag, sep = " ")[1]
ptitle <- "Recently Dead Tree DBH"
ggplot(p, aes(date, dbh, group = plottag)) + 
  geom_line(aes(color = species)) + 
  labs(title = ptitle) + 
  # geom_point(data = p2, inherit.aes = F, color = "black", aes(date, DBH, group = plottag)) +
  facet_wrap(~dbh.class, scales = "free")

# other trees of interest that had overlapping bands or were back calculated
# filter(plot == 'A1', tag == 18) 
# filter(plot == 'A2', tag == 25) 
# filter(plot == 'A3', tag == 51)
# filter(plot == "C1", tag == 217)
# filter(plot == 'D1', tag == 537)
# filter(plot == 'D2', tag == 587)
# filter(plot == 'D4', tag == 633)

# calculate kgC from DBHs then convert to MgC/ha -----------------------------------------------------------------------

kgC <- calc.kgC("all.dbh", "spp", c(4:ncol(all.dbh)))
kgC.dates <- as.POSIXlt(as.numeric(names(kgC[c(-1:-3)])), origin = "1970-1-1")
str(all.dbh$spp)

# fill gaps with linear interpolation between
for (i in 1:nrow(kgC)) {
  if (grepl("01+0", paste(as.character(as.numeric((is.na(kgC[i, -c(1:3)])))), collapse = ""))) {
    # is there a gap?
    gaps <- gregexpr("01+0", paste(as.character(as.numeric((is.na(kgC[i, -c(1:3)])))), collapse = ""))[[1]]
    for (j in 1:length(gaps)) {
      gap.start <- gaps[j] + 1
      gap.end <- gap.start + as.numeric(attributes(gaps)$match.length[j]) - 3
      kgC[i, (gap.start + 3):(gap.end + 3)] <- approx(as.numeric(kgC.dates[c(gap.start - 1, gap.end + 1)]), kgC[i, c(gap.start - 
        1 + 3, gap.end + 1 + 3)], as.numeric(kgC.dates[gap.start:gap.end]))$y
      all.dbh[i, (gap.start + 3):(gap.end + 3)] <- approx(as.numeric(kgC.dates[c(gap.start - 1, gap.end + 1)]), all.dbh[i, c(gap.start - 
        1 + 3, gap.end + 1 + 3)], as.numeric(kgC.dates[gap.start:gap.end]))$y
    }
  }
}

total.kgC <- apply(kgC[, c(-1, -2, -3)], 2, function(x) {sum(x, na.rm = T)} )
kgC.by.plot <- apply(kgC[, -(1:3)], 2, function(y) {aggregate(y, list(kgC$plot), function(x) {sum(x, na.rm = T)} )$x} )
kgC.by.plot <- data.frame(unique(kgC$plot), kgC.by.plot)
names(kgC.by.plot)[1] <- "plot"
names(kgC.by.plot)[-1] <- substring(names(kgC.by.plot)[-1], 2)

matplot(as.POSIXct(kgC.dates), t(kgC.by.plot[, -1]), type = "l", ylim = c(0, max(kgC.by.plot[, -1])), ylab = "kgC", xlab = "Date", main = "Plot-level Biomass", 
  xaxt = "n")
axis.POSIXct(1, as.POSIXct(kgC.dates))

MgCha.by.plot <- kgC.by.plot
MgCha.by.plot[, -1] <- (MgCha.by.plot[-1]/1000)/(100 * pi/10000)

MgCha <- apply(MgCha.by.plot[, -1], 2, mean)
MgCha.CI <- apply(MgCha.by.plot[, -1], 2, CI.calc)

# plot mean biomass over time with 95% CI
plot(kgC.dates, MgCha, type = "l", ylim = range(c(MgCha + MgCha.CI, MgCha - MgCha.CI)), main = "Mean Plot-level Biomass +/- 95% CI", 
  ylab = "MgC/ha", xlab = "Date")
lines(kgC.dates, MgCha + MgCha.CI, lty = 2)
lines(kgC.dates, MgCha - MgCha.CI, lty = 2)

# tmp <-tapply(MgCha, kgC.dates$year+1900, max, na.rm = T) plot(as.numeric(names(tmp)), tmp, type='o', xlab='Year', ylab='Mg C / ha',
# main='Above Ground Woody Biomass', cex.axis = 3, cex.lab = 3, cex.main = 3, lwd = 2, pch = 16) need to clean up data on new
# recruits before we can calculate annual increment/recruit/mortality all.dbh[, -(1:3)][all.dbh[, -(1:3)]<10&!is.na(all.dbh[,
# -(1:3)])] <-NA #remove all DBH values below 10 cm

# calculate AGWI, AGWB, recruitment, and mortality ---------------------------------------------------------------------------

years <- unique(kgC.dates$year + 1900)
# inc abbreviates annual growth increment which is the annual difference in peak biomass
# inc.old uses the calculated DBH of the final measurement in the previous year as the baseline, 
# which may introduce noise into the data based on timing of the final measurement
inc <- NULL
inc.old <- NULL
rec <- NULL
mort <- NULL
AGWB <- NULL

for (y in 1:length(years)) {
  tmp <- kgC[, which(kgC.dates$year + 1900 == years[y]) + 3]
  if (years[y] == 1998) {
    tmp.last.year <- kgC[, c(1, which(kgC.dates$year + 1900 == years[y - 1])) + 3]  #needs to remain data.frame class, so I duplicated the single vector
  } else if (years[y] > 1998) {
    tmp.last.year <- kgC[, which(kgC.dates$year + 1900 == years[y - 1]) + 3]
  } else if (years[y] == 1993) {
    tmp.last.year <- NULL
  }
  if (!is.null(tmp.last.year)) {
    # these original caclulations don't work because some NA's are gaps in data, and some dead trees were filled until end of the year it
    # is left in for reference only
     
    # absent.last.year <- apply(tmp.last.year, 1, function(x){all(is.na(x))}) present.this.year <- !apply(tmp, 1,
    # function(x){all(is.na(x))}) present.last.year <- !apply(tmp.last.year, 1, function(x){any(is.na(x))}) absent.this.year <-
    # apply(tmp, 1, function(x){any(is.na(x))})# This may shift some mortality one year later(see 2005), if it was recorded as dead on
    # the last recording in the year recruited <- absent.last.year & present.this.year died <- present.last.year & absent.this.year
    
    died2 <- splus.is.element(kgC[, 1:2], dead.tow[[as.character(years[y])]])
    recruited2 <- splus.is.element(kgC[, 1:2], recruit.tow[[as.character(years[y])]])
    max.tmp <- apply(tmp, 1, max, na.rm = T)
    max.tmp[!is.finite(max.tmp)] <- NA
    max.tmp.last.year <- apply(tmp.last.year, 1, max, na.rm = T)
    max.tmp.last.year[!is.finite(max.tmp.last.year)] <- NA
    tmp.colsums <- as.numeric(apply(tmp, 2, max, na.rm = T))
    max.tmp.old <- tmp[, which.max(tmp.colsums)]
    
    # hist(max.tmp-max.tmp.last.year, main = year)
    AGWB[as.character(years[y])] <- (sum(max.tmp[!recruited2 & !died2], na.rm = T)/1000)/(100 * pi * 34/10000)
    inc[as.character(years[y])] <- (sum(max.tmp[!recruited2 & !died2] - max.tmp.last.year[!recruited2 & !died2], na.rm = T)/1000)/(100 * 
      pi * 34/10000)
    inc.old[as.character(years[y])] <- (sum(max.tmp.old[!recruited2 & !died2] - tmp.last.year[, ncol(tmp.last.year)][!recruited2 & 
      !died2], na.rm = T)/1000)/(100 * pi * 34/10000)
    rec[as.character(years[y])] <- (sum(max.tmp[recruited2], na.rm = T)/1000)/(100 * pi * 34/10000)
    mort[as.character(years[y])] <- (sum(apply(cbind(max.tmp.last.year[died2], max.tmp[died2]), 1, max, na.rm = T))/1000)/(100 * 
      pi * 34/10000)
    if (any(recruited2)) {
      # recruits may grow after initial DBH measurement, and increment is calculated relative to first measurement
      recs.max <- max.tmp[recruited2]
      recs <- tmp[recruited2, ]
      rec.inc <- numeric()
      for (i in 1:nrow(recs)) {
        tmp.row <- recs[i, ][!is.na(recs[i, ])]
        rec.inc[i] <- ((recs.max[i] - tmp.row[1])/1000)/(100 * pi * 34/10000)
      }
      inc[as.character(years[y])] <- inc[as.character(years[y])] + sum(rec.inc)
    }
  } else {
    # get max AGWB in each year by selecting peak biomass in each year
    AGWB[as.character(years[y])] <- (sum(tmp, na.rm = T)/1000)/(100 * pi * 34/10000)
  }
}

# adjust recruitment to be annual rates
prev.rec.year <- 1993
for (year in years[-1][rec > 0]) {
  rec[as.character((prev.rec.year + 1):year)] <- rec[as.character(year)]/(year - prev.rec.year)
  prev.rec.year <- year
}
rec <- rec[sort(names(rec))]

# change growth increment, AGWB, and mortality, to be an an annual rate for 1993:1998
inc["1998"] <- inc["1998"]/5  #this only includes trees that were alive in 1998, 
# assume all mortality happened at end of 1997 to interpolate other increments
AGWB["1997"] <- AGWB["1998"] - inc["1998"]
AGWB[as.character(1994:1996)] <- NA
AGWB <- AGWB[sort(names(AGWB))]
inc[as.character(1994:1997)] <- (AGWB["1997"] - AGWB["1993"])/4
for (y in 1994:1997) {
  AGWB[as.character(y)] <- AGWB[as.character(y - 1)] + inc[as.character(y)]
}
inc.old["1998"] <- inc.old["1998"]/5
inc.old[as.character(1994:1997)] <- ((AGWB["1998"] - inc.old["1998"]) - AGWB["1993"])/4

mort[as.character(1994:1998)] <- mort["1998"]/5

AGWB <- AGWB[sort(names(AGWB))]
mort <- -mort[sort(names(mort))]
inc <- inc[sort(names(inc))]
inc.old <- inc.old[sort(names(inc.old))]

# have not yet implemented the confidence intervals for this data yet. We should try and use the method Josh Benmergui worked out for
# his paper, rather than just reimplementing the old method (elg 2016)

# plot of biomass increments with new method
plot.data <- rbind(inc, rec, mort)[, 4:length(inc)]
plot.data <- replace(plot.data, is.na(plot.data), 0)
par(mfrow = c(2, 1))
par(mar = c(0, 5, 5, 2))

tmp <- barplot(plot.data[-3, ], yaxs = "i", ylim = c(0, 3.2), las = 2, names.arg = rep("", ncol(plot.data)), col = c(3, 4), main = "Median DBH AGWI (from peak AGWB previous year)")
  # plotCI(tmp, plot.data[1, ]+plot.data[2, ], uiw = tow.ann.stats[4:21, 'pos.95CI'], add = T, type='n', gap = 0)
par(mar = c(5, 5, 0, 2), xpd = NA)
  # lower = plot.data[3, ]-tow.ann.stats[4:22, 'neg.95CI'] upper = plot.data[3, ]+tow.ann.stats[4:22, 'neg.95CI']
barplot(plot.data[3, ], yaxs = "i", ylim = c(-3, 0), las = 2, col = 2, names.arg = c("1994:1997", as.character(c(1998:2017))), ylab = "       Aboveground woody increment (MgC/ha)", 
  xpd = NA)
  # plot.ci = T, ci.l = lower, ci.u = upper, xpd = NA, ylab='Aboveground woody increment (MgC/ha)')
legend("bottom", legend = c("growth", "recruitment", "mortality"), fill = c(3, 4, 2))

# 
plot.data <- rbind(inc.old, rec, mort)[, 4:length(inc.old)]
plot.data <- replace(plot.data, is.na(plot.data), 0)
par(mfrow = c(2, 1))
par(mar = c(0, 5, 5, 2))

tmp <- barplot(plot.data[-3, ], yaxs = "i", ylim = c(0, 3.2), las = 2, names.arg = rep("", ncol(plot.data)), col = c(3, 4), main = "Median DBH AGWI (from last observation of AGWB previous year)")
  # plotCI(tmp, plot.data[1, ]+plot.data[2, ], uiw = tow.ann.stats[4:21, 'pos.95CI'], add = T, type='n', gap = 0)
par(mar = c(5, 5, 0, 2), xpd = NA)
  # lower = plot.data[3, ]-tow.ann.stats[4:22, 'neg.95CI'] upper = plot.data[3, ]+tow.ann.stats[4:22, 'neg.95CI']
barplot(plot.data[3, ], yaxs = "i", ylim = c(-3, 0), las = 2, col = 2, names.arg = c("1994:1997", as.character(c(1998:2017))), ylab = "       Aboveground woody increment (MgC/ha)", 
  xpd = NA)
  # plot.ci = T, ci.l = lower, ci.u = upper, xpd = NA, ylab='Aboveground woody increment (MgC/ha)')
legend("bottom", legend = c("growth", "recruitment", "mortality"), fill = c(3, 4, 2))

# bad.data <-apply(kgC[, -(1:3)], 1, function(x){any(diff(aggregate(as.numeric(x), list(kgC.dates$year+1900), max)[, 2])<(-1), na.rm
# = T)}) bad.data <- bad.data | apply(kgC[, -(1:3)], 1, function(x){any(diff(aggregate(as.numeric(x), list(kgC.dates$year+1900),
# max)[, 2])>20, na.rm = T)}) par(ask = T) for(i in which(bad.data)){ plot(kgC.dates, kgC[i, -(1:3)], type='o', main = paste(kgC[i,
# 1:2], as.character(kgC[i, 3]))) } get.biggest <- function(x){ tmp <-range(x, na.rm = T) tmp[which.max(abs(tmp))] } data.range
# <-apply(kgC[, -(1:3)], 1, function(x){get.biggest(diff(aggregate(as.numeric(x), list(kgC.dates$year+1900), max)[, 2]))})
# barplot(rbind(plot.data[1, -1:-2], inc), names.arg = dimnames(plot.data)[[2]][-1:-2], beside = T, col = 2:1, ylab='Above Ground
# Woody Increment (MgC/ha)', main='Tree Growth') legend('topleft', legend = c('old method', 'new method'), fill = 2:1)

# finalise biomass data and export to text file
colnames(kgC)[-1:-3] <- as.character(format(kgC.dates, "%Y-%m-%d"))
colnames(all.dbh)[-1:-3] <- as.character(format(kgC.dates, "%Y-%m-%d"))
getwd()
write.table(kgC, "trees/all.kgC.17.txt", sep = "\t", quote = F, row.names = F)
write.table(all.dbh, "trees/all.dbh.17.txt", sep = "\t", quote = F, row.names = F)

# Summarize AGWI table and Calculate ANPP ---------------------------------------------------------------------------------

# initalise objects to fill in loop
inc["1993"] <- NA
inc <- inc[sort(names(inc))]
agwi <- data.frame(as.numeric(names(inc)), inc)
colnames(agwi) <- c("Year", "AGWI")
agwi <- data.frame(agwi, AGWB, recruitment = c(NA, rec), mortality = c(NA, mort))
rownames(agwi) <- NULL
anpp <- data.frame(year = 2000:max(dend.dates$year + 1900), AGWI = NA, AGWI.ingrowth = NA, fine.litterfall = NA, ANPP = NA)
anpp$AGWI[anpp$year %in% agwi$Year] <- agwi[agwi$Year %in% anpp$year, "AGWI"]
# go through each year
years.rec <- names(recruit.tow)[-1]  #skip 1999 since there's litterfall data that year is only leaves
prev.recruit <- "1999"
for (year in years.rec) {
  # pick out new recruits tmp <-all.dbh[splus.is.element(all.dbh[, 1:2], recruit.tow[[year]][, 1:2]), c(1:3,
  # 3+which(dend.dates$year+1900 == as.numeric(year)))]
  tmp <- all.dbh[splus.is.element(all.dbh[, 1:2], recruit.tow[[year]][, 1:2]), ]
  year.cols <- 3 + which(dend.dates$year + 1900 == as.numeric(year))
  empty.ones <- apply(tmp[, year.cols], 1, function(x) {all(is.na(x))}) 
  #recruits that don't have any band readings
  if (sum(empty.ones) > 0) {
    tmp[empty.ones, year.cols[1]] <- apply(tmp[empty.ones, 3 + which(dend.dates$year + 1900 > as.numeric(year))], 1, min, na.rm = T)  #take the smallest band reading from after the year of recruitment
  }
  tmp <- tmp[, c(1:3, year.cols)]
  tmp <- cbind(tmp, 10)
  sum(empty.ones)
    # find the max kgC for that year for each recruit subtract off kgC of 10cm dbh tree of that species
  n <- ncol(tmp)
  tmp.kgC <- calc.kgC("tmp", 3, 4:n)
  rec <- sum(apply(tmp.kgC[4:n], 1, max, na.rm = T))/1000/(10^2 * pi * 34) * 10000/(as.numeric(year) - as.numeric(prev.recruit))
  ingrowth <- sum(apply(tmp.kgC[4:n], 1, max, na.rm = T) - apply(tmp.kgC[4:n], 1, min, na.rm = T))/1000/(10^2 * pi * 34) * 10000
  ingrowth <- ingrowth/(as.numeric(year) - as.numeric(prev.recruit))  #account for time interval since previous recruitment survey
  anpp[anpp$year %in% (as.numeric(prev.recruit) + 1):as.numeric(year), "AGWI.ingrowth"] <- ingrowth
  prev.recruit <- year
}


# fine litterfall
source("litter/r.scripts/litter.12.tgw/lit.data.import.df.tgw.r")
setwd(dir) # reset working directory
source("litter/r.scripts/litter.12.tgw/lit.sum.r")
setwd(dir)

anpp[anpp$year %in% unique(lit.sum.ems$year), "fine.litterfall"] <- (lit.sum.ems[lit.sum.ems$year %in% anpp$year & lit.sum.ems$litter.type == "fine.litter", "Mg.c.ha"])

# calculate ANPP = AGWI +AGWI.ingrowth + fine.litterfall
anpp$ANPP <- apply(anpp[, 2:4], 1, sum)

agwi$ANPP <- NA
agwi$ANPP[agwi$Year %in% anpp$year] <- anpp$ANPP

write.csv(agwi, "trees/ems.agwi.17.csv", quote = F, row.names = F)
write.csv(anpp, "trees/ems.anpp.17.csv", quote = F, row.names = F)

source("trees/r.scripts/run.all.r")
tow.ann.sum.old <- read.csv("trees/tow.ann.sum.old.csv")

tow.ann.sum <- as.data.frame(tow.ann.sum)
tow.anpp.old <- read.csv("trees/tow.anpp.old.csv")
tow.anpp <- read.csv("trees/tow.anpp.new.csv")
tow.anpp$Year <- anpp$year

row.names(tow.anpp) <- NULL
tow.anpp.old$Year <- tow.anpp$Year


# compare AGWI and ANPP new vs old methods ------------------------------------------------------------------------------------------ 

par(mfrow = c(1, 1))
par(mar = c(4, 4, 2, 2))
plot(rownames(tow.ann.sum), tow.ann.sum.old$AGWI, col = 2, type = "o", lty = 1, ylim = c(0.5, 4), 
     ylab = "AGWI (MgC/ha)", xlab = "Year")
lines(rownames(tow.ann.sum), tow.ann.sum$AGWI.new, col = 2, type = "o", lty = 2)
lines(agwi$Year[-1], inc.old, type = "o", lty = 1)
lines(agwi$Year, agwi$AGWI, type = "o", lty = 2, col = 3)

legend("topleft", legend = c("old.ref", "old.max", "new.ref", "new.max"), col = c(2, 2, 1, 3), lty = c(1, 2, 1, 2), pch = "o")

plot(c(2000:2013), tow.anpp.old$ANPP.MgC.ha[1:14], col = 2, type = "o", xlab = "Year", 
     ylab = "ANPP (MgC/ha/yr)", ylim = c(2.5, 5))
lines(anpp$year, anpp$ANPP, type = "o")
legend("topleft", legend = c("old.ref", "new.max"), col = c(2, 1), lty = c(1, 2, 1, 2), pch = "o")

# annual recruitment and mortality are now calculated in a standard way reported recruitment year are taken to be fact, even if after
# calculations they turn out to cross 10cm DBH on some other year. this is necessary because, otherwise a few trees start getting
# recruited in non-recruitment years, and this makes figuring out annual rates difficult. the alternative method for doing this would
# be to apply a universal backcalc like Josh Benmergui did to find the true recruitment date for all individuals. I'm not sure if
# this is actually better though, because we don't have good data on growth rates for trees under 10cm DBH. often this method results
# in trees recruiting many years earlier than reported, which seems unlikely, unless recruitment surveys are very bad. (elg 2016)

