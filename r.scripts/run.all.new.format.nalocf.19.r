# set working directory to run on EFT ------------------------------------------------------------------------
# if (Sys.info()[["nodename"]] == "eft") {
#   dir <- "/DATA/HF_DATA/HF_BIOMETRY_WOFSY/"
# } else {
#   dir <- "//eft.as.harvard.edu/DATA/HF_DATA/HF_BIOMETRY_WOFSY/"
#   
# }
# # Local directory for TGW 
# dir <- "C:/Users/tgw475/Desktop/Whitby HF Files/HF_BIOMETRY_WOFSY copy/"
# if (Sys.info()[["nodename"]] == "BURT") {
#   dir <- "D:/Desktop/Google Drive/Whitby HF files/HF_BIOMETRY_WOFSY copy"
# }
# setwd(dir)

# all directories relative to ..HF_BIOMETRY_WOFSY/trees/r.scripts where .RProj file is
dir <- getwd()
source("new.dbh.make.r")
source("calc.kgC.r")
source("calc_kgc_tgw2.r")
source("../../R.scripts/old.functions.r")
source("../s.scripts/CI.calc.txt")

library(tidyverse)
library(lubridate)
library(zoo)

# read in data files -----------------------------------------------------------------------------------------

dend.data <- read.table("../data/ems.dend.98.21.txt", stringsAsFactors = F, header = T, as.is = T) %>% 
  arrange(plot, tag, year, jday)

dbh.data <- read.table("../data/ems.dbh.98.21.txt", stringsAsFactors = F, header = T, as.is = T) %>% 
  arrange(plot, tag, year, jday)

# 1993 has only one DBH tape measurement, unknown collection date
tow.93.dbh <- read.table("../data/dendrometer/1993/tow.93.dbh.txt", header = T, sep = "\t", as.is = T) %>%
  mutate(year = 1993, 
    jday = 364/2) %>% 
  filter(!plot %in% c("A4", "A5", "B5", "G3", "H3", "H4")) %>%  # take out flooded and harvest plots
  select(plot, tag, year, jday, DBH = dbh.tape)

# species need to be updated manually when new recruits are added
spp.data <- read.table("../data/ems.spp.all.txt", stringsAsFactors = F, header = T, sep = "\t", as.is = T)

# make sure no trees are missing a species code, should return 0 rows
dend.data[which(interaction(dend.data[ , 1:2]) %in% interaction(spp.data[ , 1:2]) == F), ]

dend.data$date <- as_date(strptime(paste(dend.data$year, dend.data$jday), format ="%Y %j"))
dbh.data$date <- as_date(strptime(paste(dbh.data$year, dbh.data$jday), format = "%Y %j"))

# data entry errors and outliers to correct ------------------------------------------------------------------

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
# large recruit H1-204 from back.calc incorrectly recruited in 2005, I deleted it from tow.2007.mort.txt, tow.2005.rec.txt and dend.data
# dendrometer on ever shrank. Probably was always a standing dead snag
dend.data <- dend.data[!(dend.data$plot == "H1" & dend.data$tag == 204),]
dbh.data <- dbh.data[!(dbh.data$plot == "H1" & dbh.data$tag == 204),]
spp.data <- spp.data[!(spp.data$plot == "H1" & spp.data$tag == 204),]
# large recruit D2-698 from back.calc incorrectly lirecruited in 2003. I deleted it from tow.2005.mort.txt, tow.2003.rec.txt and dend.data
# dendrometer on ever shrank. Probably was always a standing dead snag
dend.data <- dend.data[!(dend.data$plot == "D2" & dend.data$tag == 698),]
dbh.data <- dbh.data[!(dbh.data$plot == "D2" & dbh.data$tag == 698),]
spp.data <- spp.data[!(spp.data$plot == "D2" & spp.data$tag == 698),]
# B4 270 has dendnum 2 with no measurements, change dend.num 2 to 3 and just skip to 3
dend.data$dend.num[dend.data$plot == "B4" & dend.data$tag == 270 & dend.data$dend.num == 3] <- 2
# C4 486 first measured in 1998 DBH 10.8, recruited and died in same year. Just remove from data
dend.data <- dend.data[!(dend.data$plot == "C4" & dend.data$tag == 486), ]
dbh.data <- dbh.data[!(dbh.data$plot == "C4" & dbh.data$tag == 486), ]
# F1 940 never had second dend, dendnum 2 entered as all 0's for 2002 when it was recorded dead, change to dend 1 so NA filling works
dend.data$dend.num[dend.data$plot == "F1" & dend.data$tag == 940] <- 1 
# C5-518 typo in field collection on 5-15-19 35 not 25
dend.data$dend.meas[dend.data$plot == "C5" & dend.data$tag == 518 & dend.data$date == "2019-05-15"] <- 35.83
# E1-678 has plenty of overlap and bad spike in data in 2017, rm dend 1 in 2017
dend.data <- dend.data[!(dend.data$plot == "E1" & dend.data$tag == 678 & dend.data$year == 2017 & dend.data$dend.num == 1 ), ]
# C1-217 dend 1 skips an observation on 2012-12-04, fill with previous dend
new.row <- dend.data[(dend.data$plot == "C1" & dend.data$tag == 217 & dend.data$date == "2012-12-04" ), ]
new.row$dend.meas <- 88.39
new.row$dend.num <- 1
dend.data <- rbind(dend.data, new.row) %>% arrange(plot, tag, year, jday)
# C5 724 has too large of a measurement when the hole was obscured in 2015-08-27, 2015-11-03, 2016-04-22
# found in notes that he should have deleted the previous increment of 4.15 and can't be accurate,
# because the forgot to draw a new pencil line. just delete this date onward.
dend.data <- dend.data[!(dend.data$plot == "C5" & dend.data$tag == 724 & dend.data$dend.num == 2 & 
    dend.data$date >= "2015-08-27"), ]
# D5 658 18-05-03 too high
dend.data$dend.meas[(dend.data$plot == "D5" & dend.data$tag == 658 & dend.data$dend.num == 2 & 
    dend.data$date == "2018-05-03")] <- 35.58
# E3 764 dend 2 2014-08-12 too probably 85 not 95 to match before and after measurements
dend.data$dend.meas[(dend.data$plot == "E3" & dend.data$tag == 764 & dend.data$dend.num == 2 & 
    dend.data$date == "2014-08-12")] <- 85.64
# Missing dend data was filled with a 0, which will mess up median calculation. replace with NA's and let na.locf fill
# other than in 1998 when denddrometers weren't installed on trees that had died after initial 1993 survey
# F3 117 has overlap, remove dend 4 after 6-20-17 when there was a gap in the data
dend.data <- dend.data[!(dend.data$plot == "F5" & dend.data$tag == 117 & dend.data$dend.num == 4 & 
    dend.data$date >= "2017-06-20"), ]
dend.data$dend.meas[(dend.data$year > 1998 & dend.data$dend.meas == 0)] <- NA 
# F3 7 new band 4 in 2018, holes changed, delete before 2018
dend.data <- dend.data[!(dend.data$plot == "F3" & dend.data$tag == 7 & dend.data$dend.num == 4 & 
    dend.data$year < 2019), ]
# C5 519 band 2 had data gap in 2017, and sufficient overlap with band 3. Delete band 2 in 2017
dend.data <- dend.data[!(dend.data$plot == "C5" & dend.data$tag == 519 & dend.data$dend.num == 2 & 
    dend.data$year >= 2017), ]
# D2 590 band loose 10-16-16 and 4-17-17 carry last good measurement forward 27.60
dend.data$dend.meas[dend.data$plot == "D2" & dend.data$tag == 590 & 
    (dend.data$date == "2016-10-16" | dend.data$date == "2017-04-17")] <- 27.6
# D3 604 Band 1 left on too long, delete in 2017
dend.data <- dend.data[!(dend.data$plot == "D3" & dend.data$tag == 604 & dend.data$dend.num == 1 & 
    dend.data$year >= 2017), ]
# plot == "E5", tag == 734 remove first measure of dend 2 still settling
dend.data <- dend.data[!(dend.data$plot == "E5" & dend.data$tag == 734 & dend.data$dend.num == 2 & 
    dend.data$date == "2017-04-17"), ]
# plot == "F5", tag == 127, spike in 2002-05-09, previous measurement carried forward
dend.data$dend.meas[dend.data$plot == "F5" & dend.data$tag == 127 & 
    dend.data$date == "2002-05-09"] <- 12.15
# G1-175 lots of overlap b/w band 2 and 3, delete band 2 in 2007
dend.data <- dend.data[!(dend.data$plot == "G1" & dend.data$tag == 175 & dend.data$dend.num == 2 & 
    dend.data$year == 2017), ]
# H2-330 high peak in 2009 fix from data sheet to 25.15
dend.data$dend.meas[dend.data$plot == "H2" & dend.data$tag == 330 & 
    dend.data$date == "2009-12-01"] <- 25.15
# E2 720 gap in dend 1, delete in 2017
dend.data <- dend.data[!(dend.data$plot == "E2" & dend.data$tag == 720 & dend.data$dend.num == 1 & 
    dend.data$year == 2017), ]
# F1 236 gap in dend 2, delete in 2017
dend.data <- dend.data[!(dend.data$plot == "F1" & dend.data$tag == 236 & dend.data$dend.num == 2 & 
    dend.data$year == 2017), ]
# D1 357 dend 1 has bad increment using pencil line, as hole behind buckle delete after first measurement in 2015
dend.data <- dend.data[!(dend.data$plot == "D1" & dend.data$tag == 537 & dend.data$dend.num == 1 & 
    dend.data$date > "2015-05-13"), ]
# F2 982 erroneous low dbh in 2015-11-03, delete
dbh.data <- dbh.data[!(dbh.data$plot == "F2" & dbh.data$tag == 982 & dbh.data$date == "2015-11-03"), ]
# H2-334 dend 1 gap, delete 2015-06-22
dend.data <- dend.data[!(dend.data$plot == "H2" & dend.data$tag == 334 & dend.data$dend.num == 1 & 
    dend.data$date == "2015-06-22"), ]
# H2-336 one missing record dend 1 2015-05-13, use dend 2 increment for same date (19.85 - 20.98 = -1.13)
# shrank slightly over winter
new.row <- dend.data[(dend.data$plot == "H2" & dend.data$tag == 336 & dend.data$date == "2015-05-13" ), ]
new.row$dend.meas <- 133.5 - 1.13
new.row$dend.num <- 1
dend.data <- rbind(dend.data, new.row) %>% arrange(plot, tag, year, jday)
# E3-777 2014-08-12 low measurement changed from 82.34 to 92.34
dend.data$dend.meas[dend.data$plot == "E3"& dend.data$tag == 777 & dend.data$date == "2014-08-12"] <- 92.34
# G5-269 2014-08-12 high dend was 114.27 redone to 104.27 based on following measurement
dend.data$dend.meas[dend.data$plot == "G5"& dend.data$tag == 269 & dend.data$date == "2014-08-12"] <- 104.27
# E5-899 dend 2 gap , remove after 2013
dend.data <- dend.data[!(dend.data$plot == "E5" & dend.data$tag == 899 & dend.data$dend.num == 2 & 
    dend.data$year > 2013), ]
# C3-406 2010-04-27 measurement jumps +10mm, notes say "dend broken" fill with previous measurement 28.06
dend.data$dend.meas[(dend.data$plot == "C3" & dend.data$tag == 406 & dend.data$date == "2010-04-27")] <- 28.06
# G2-215 2010-06-01 jumps by ~5mm 26.72, fill with previous measurement 22.81
dend.data$dend.meas[(dend.data$plot == "G2" & dend.data$tag == 215 & dend.data$date == "2010-06-01")] <- 22.81
# E3-738 band 2 tighening 2015, plenty of overlap, delete that year
dend.data <- dend.data[!(dend.data$plot == "E3" & dend.data$tag == 738 & dend.data$dend.num == 2 & 
    dend.data$year <= 2015), ]
#D2-584 2006-08-27 "band adjusted" change to dend.num 2 after this date
dend.data$dend.num[(dend.data$plot == "D2" & dend.data$tag == 584 & dend.data$date >= "2006-08-27")] <- 2
# F5-77 2006-06-01 dend spiked high to 39.99 cahnged to 36.99 to match flanking measurements
dend.data$dend.meas[(dend.data$plot == "F5" & dend.data$tag == 77 & dend.data$date == "2006-06-01")] <- 36.99
# E2-708 2005-11-02 first measurement for dend 2 hadnt settled yet, fill with next measurement 6.58
dend.data$dend.meas[(dend.data$plot == "E2" & dend.data$tag == 708 & dend.data$date == "2005-11-02")] <- 6.58
# A2-31 2003-07-29 high peak 58.85, changed to 53.85
dend.data$dend.meas[(dend.data$plot == "A2" & dend.data$tag == 31 & dend.data$date == "2003-07-29")] <- 53.85
# A3-65 2003-07-29 high peak 15.53, fill with previous 2003-07-17 12.63
dend.data$dend.meas[(dend.data$plot == "A3" & dend.data$tag == 65 & dend.data$date == "2003-07-29")] <- 12.63
# F5-119 2012-03-28 high peak 18.80, fill with previous 2011-12-01 16.35
dend.data$dend.meas[(dend.data$plot == "F5" & dend.data$tag == 119 & dend.data$date == "2012-03-28")] <- 16.35
# F5-127 2003-04-27 high peak 15.59, fill with previous 13.29
dend.data$dend.meas[(dend.data$plot == "F5" & dend.data$tag == 127 & dend.data$date == "2003-04-27")] <- 13.29
# A3-53 2011-05-10 high peak 16.43 fill with previous 15.19
dend.data$dend.meas[(dend.data$plot == "A3" & dend.data$tag == 53 & dend.data$date == "2011-05-10")] <- 15.19
# D3- 609 DBH erased in data sheet was correct
dbh.data$DBH[(dbh.data$plot == "D3" & dbh.data$tag == 609 & dbh.data$date == "2002-04-21") ] <- 30.4
# E4-816 2002-08-02 high peak 27.85 fill with previous 26.85
dend.data$dend.meas[(dend.data$plot == "E4" & dend.data$tag == 816 & dend.data$date == "2002-08-02")] <- 26.85
# F1-957  dbh 10 cm too high, delete
dbh.data <- dbh.data[!(dbh.data$plot == "F1" & dbh.data$tag == 957 & dbh.data$date == "2001-07-30"), ]
# F2-987 band replaced on 2002-04-21 still settling, fill back first two dend meas with 2002-05-22
dend.data$dend.meas[(dend.data$plot == "F2" & dend.data$tag == 987 & dend.data$date == "2002-04-21")] <- 20.63
dend.data$dend.meas[(dend.data$plot == "F2" & dend.data$tag == 987 & dend.data$date == "2002-05-09")] <- 20.63
# F2-987 band replaced on 2002-04-21 still settling, change to dendnum 2 after this day
dend.data$dend.num[(dend.data$plot == "F2" & dend.data$tag == 987 & dend.data$date >= "2002-04-21")] <- 2
# one january measurement in 1999-01-05, many high values probably from ice buildup in bark or under band remove
dend.data <- dend.data[!dend.data$date == "1999-01-05", ]
# E3-772 band not tight when first installed 2005-11-02, 15.3, fill back from 2006-05-01, 13.40
dend.data$dend.meas[(dend.data$plot == "E3" & dend.data$tag == 772 & dend.data$date == "2005-11-02")] <- 13.40
# E5	885	2001 jday 211 DBH much too high	27.5
dbh.data <- dbh.data[!(dbh.data$plot == "E5" & dbh.data$tag == 885 & dbh.data$date == "2001-07-30"), ]
# E5-890 high peak when band switching, delete old band 1 in 2017, enough overlap
dend.data <- dend.data[!(dend.data$plot == "E5" & dend.data$tag == 890 & dend.data$dend.num == 1 & 
    dend.data$year == 2017), ]
# F1-937 2004-04-12 very high, notes say tree fell on band so data is bad. Fill from previous 2003-11-13 47.39
dend.data$dend.meas[(dend.data$plot == "F1" & dend.data$tag == 937 & dend.data$date == "2004-04-12")] <- 47.39
# F2-977 2017-08-14 questionable pencil line increment. Plenty of overlap, just delete old band after this date
dend.data <- dend.data[!(dend.data$plot == "F2" & dend.data$tag == 977 & dend.data$dend.num == 1 & 
    dend.data$date >= "2017-08-14"), ]
# F3-1 2001-07-30 typo very low value 3.36, data sheet has 30.36 missing a 0 
dend.data$dend.meas[dend.data$plot == "F3" & dend.data$tag == 1 & dend.data$dend.num == 2 & 
    dend.data$date == "2001-07-30"] <- 30.36
# F5-99 2017 gap in dend.num 2, plenty overlap with dend.num 3, remove dend.num 2 > 2016 
dend.data <- dend.data[!(dend.data$plot == "F5" & dend.data$tag == 99 & dend.data$dend.num == 2 & 
    dend.data$year > 2016), ]
# H1-303 suppressed, dend.num 2 hasn't tightened in 5 years, dend.num 1 still good
dend.data <- dend.data[!(dend.data$plot == "H1" & dend.data$tag == 303 & dend.data$dend.num == 2), ]

# write corrected data for website ---------------------------------------------------------------------------
dend.data.98.21 <- dend.data %>% 
  mutate(date = as_date(paste0(year, jday), format = "%Y%j")) %>% 
  select(plot,tag, year, jday, date, dend.num, dend.meas)
#temporary corrections 
# # probably a faulty caliper on "2018-05-29" when REU's first helping
# dend.data.98.21$dend.meas[dend.data.98.21$plot == "C5" & dend.data.98.21$tag == 888 & dend.data$date == "2018-05-29"] <- 6.78
# dend.data.98.21$dend.meas[dend.data.98.21$plot == "C5" & dend.data.98.21$tag == 889 & dend.data$date == "2018-05-29"] <- 10.31
# dend.data.98.21$dend.meas[dend.data.98.21$plot == "D1" & dend.data.98.21$tag == 552 & dend.data$date == "2018-05-29"] <- 54.41
# dbh.data.93.20 <- dbh.data.93.20[!(dbh.data.93.20$plot == "D3" & dbh.data.93.20$tag == 604 & dbh.data.93.20$date == "2017-06-20"), ]
# dend.data.98.21 <- dend.data.98.21[!(dend.data.98.21$plot == "E1" & dend.data.98.21$tag == 678 &
# #     dend.data.98.21$year == 2017 & dend.data.98.21$dend.num == 1), ]
# # temp plots
# ggplot(filter(dend.data.98.21, plot == "C5", tag == 290), aes(x= date, y = dend.meas, color = as.factor(dend.num), group = dend.num)) + 
#   geom_line()
# ggplot(data = filter(dbh.data.93.20,  plot == "C5", tag == 290), aes(x= date, y = DBH), inherit.aes = FALSE) +
#   geom_point()
# filter(dend.data.98.21,  plot == "A1", tag == 1)

write.table(dend.data.98.21, "../data/ems.dend.all.98.21.txt", sep = "\t", quote = F, row.names = F)
dbh.data.93.21 <- bind_rows(tow.93.dbh, dbh.data) %>% 
  mutate(date = as_date(paste0(year, jday), format = "%Y%j")) %>% 
  select(plot,tag, year, jday, date, DBH) %>% 
  arrange(plot, tag, date)
write.table(dbh.data.93.21, "../data/ems.dbh.all.93.21.txt", sep = "\t", quote = F, row.names = F)

# DBH data reduction -----------------------------------------------------------------------------------------

# loop filters out ref DBH within the first 3 months of dendroband record
# if there are at least 2 ref DBH's for that band, as the band
# is most likely still loose in this time period

trees <- unique(dend.data[, c("plot", "tag")])

a <- length(dbh.data$DBH[!is.na(dbh.data$DBH)])
start <- Sys.time()

for (i in 1:nrow(trees)) {
  tree <- dend.data[dend.data$plot == trees$plot[i] & dend.data$tag == trees$tag[i], ]
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
# there are 398 early dbh records that will not be used

# Overlaping dendrometers data reduction, observation date filling -------------------------------------------

obs.dates <- unique(dend.data$date)
obs.trees <- unique(dend.data[ , c("plot", "tag")])
row.names(obs.trees) <- NULL
tmp2 <- dend.data[0, ]

for(i in 1:nrow(obs.trees)){
  tmp <- dend.data[dend.data$plot == obs.trees$plot[i] & dend.data$tag == obs.trees$tag[i], ] 
  # remove zero's used to fill empty observations, they will be refilled by carrying forward/back nearest value
  # must have some non-NA values to fill from, otherwise keep
  max.date <- max(tmp$date)
  min.date <- min(tmp$date)
  tree.dates <- data.frame(obs.trees[i, 1:2], date = obs.dates[obs.dates >= min.date & obs.dates <= max.date])
  band.count <- na.omit(unique(tmp$dend.num))
  if(length(band.count)>1){
    for(bnd in 1:(length(band.count)-1)){
      date.end <- max(tmp$date[tmp$dend.num == bnd], na.rm = F)
      date.start <- min(tmp$date[tmp$dend.num == bnd + 1], na.rm = F)
      if(date.end > date.start & (date.start - date.end) < -180){ 
        # delete first 30 days of new dend if overlap is > 180 days
        tmp <- tmp[!(tmp$dend.num == bnd + 1 & tmp$date < date.start + 90), ]
      }
    }
  }
  tmp <- full_join(tmp, tree.dates, by = c("plot", "tag", "date")) # full-join makes sure tree is present every date in its range
  tmp2<- rbind(tmp2, tmp)
}

nrow(tmp2) - nrow(dend.data)
str(tmp2)
tmp2 <- as.data.frame(arrange(tmp2, plot, tag, date, dend.num))

rows.added <- anti_join(tmp2, dend.data, by = c("plot", "tag", "dend.num","date")) %>% arrange(plot, tag, date, dend.num)
str(rows.added)
# 49 observations that were not in dend data
rows.removed <- anti_join(dend.data, tmp2, by = c("plot", "tag", "dend.num","date")) %>% arrange(plot, tag, date, dend.num)
str(rows.removed)
# 558 overlapping new dendrometer observations that were taken out of the dataset

# Interpolate missing dend.num and dend.meas values with zoo package -----------------------------------------

# NA's were either missed days because of broken dendrometers, or recording errors.
# if the gap is within a single dendrometer it is interpolated by date, 
# if the gap is at the end of a dendrometer it is because of switching to new dendrometers
# and the last measurement is carried forward
# this will not carry forward for dead trees, because dates weren't added after a tree's final observation
sum(is.na(tmp2$dend.meas))
sum(is.na(tmp2$dend.num))

# create median dend inc by species for filling NA's
dend.inc.spp <- dend.data %>% left_join(spp.data) %>% 
  group_by(plot, tag, spp, dend.num) %>% 
  mutate(dend.inc = replace(dend.meas, !is.na(dend.meas), c(0, diff(na.omit(dend.meas))))) %>% 
  group_by(year, spp, date) %>%
  summarize(dend.inc = median(dend.inc, na.rm=T))


dend.data <- tmp2 %>% arrange(plot, tag, date, dend.num) %>%
  group_by(plot, tag) %>%
  mutate(year = ifelse(is.na(year), year(date), year),
    jday = ifelse(is.na(jday), yday(date), jday),
    dend.num = na.locf(dend.num, na.rm=F), # carry forward dend.num to empty obs dates first
    dend.num = na.locf(dend.num, na.rm=F, fromLast = T),
    plottag = paste(plot, tag, sep = "-")) %>% #  then carry backwards to fill beginning of record
  left_join(spp.data) %>% 
  left_join(dend.inc.spp) %>% 
  group_by(plot, tag, dend.num) %>%
  mutate(  # old method of carry forward last measurement
    dend.meas = na.approx(dend.meas, date, na.rm = F),  # only fills internal gaps on a single dendrometer
    dend.meas = na.locf(dend.meas, na.rm = F), #If NA's at head or tail, fills with nearest observation
    dend.meas = na.locf(dend.meas, na.rm = F, fromLast = T)) %>% 
  # mutate(dend.meas.fill = na.locf(dend.meas, na.rm = F),
  #   dend.meas.fill.back = na.locf(dend.meas.fill, na.rm = F, fromLast = T),
  #   # if it's empty, add the spp average increment
  #   dend.meas = ifelse(is.na(dend.meas)& !is.na(dend.meas.fill), dend.meas.fill + dend.inc,
  #     ifelse(is.na(dend.meas)& !is.na(dend.meas.fill.back), dend.meas -)) %>% 
  select(colnames(dend.data.98.21)) %>%
  as.data.frame()


ggplot(dend.inc.spp, aes(x = date, y = dend.inc))+
  geom_line()+
  facet_wrap(~spp)+
  geom_hline(yintercept = 0)

anyNA(dend.data$dend.meas)
sum(is.na(dend.data$dend.meas))
anyNA(dend.data$dend.num)

# temp plots
# ggplot(filter(dend.data.98.21, plot == "C5", tag == 290), 
#   aes(x= date, y = dend.meas, color = as.factor(dend.num), group = dend.num)) + 
#   geom_line()
# ggplot(data = filter(dbh.data.93.20,  plot == "C5", tag == 290), aes(x= date, y = DBH), inherit.aes = FALSE) +
#   geom_point()
# filter(dend.data.98.21,  plot == "A1", tag == 1)


# Mortality and recruitment data -----------------------------------------------------------------------------

years <- as.character(list.files("../data/dendrometer", pattern = "\\d{4}"))

recruit.tow <- list()
dead.tow <- list()
for (year in years) {
  yr <- substr(year, 3, 4)
  fl <- paste("../data/dendrometer/", year, "/tow.", yr, ".mort.txt", sep = "")
  if (file.exists(fl)) {
    tmp <- read.table(file = fl, header = T)
    dead.tow[[year]] <- tmp
  }
  fl <- paste("../data/dendrometer/", year, "/tow.", yr, ".recruits.txt", sep = "")
  if (file.exists(fl)) {
    tmp <- read.table(file = fl, header = T)
    recruit.tow[[year]] <- tmp
  }
}
# remove trees in dendroband data after they are recorded as dead
for (year in years[-1]){ # don't include 1993
  dead.trees <- dead.tow[[year]]
  if (nrow(dead.trees)>0) {
    for (i in 1:nrow(dead.trees)) {
      dend.data <- dend.data[!(dend.data$plot == dead.trees$plot[i] & dend.data$tag == dead.trees$tag[i] & 
          dend.data$year > as.numeric(year)), ]
      dbh.data <- dbh.data[!(dbh.data$plot == dead.trees[i, 1] & dbh.data$tag == dead.trees[i, 2] & 
          dbh.data$year > as.numeric(year)), ]
    }
  }
}
# remove trees in dendroband data if they are present before recruitment year
for (year in names(recruit.tow)) { # don't include 1993
  rec.trees <- recruit.tow[[year]]
  if (nrow(rec.trees)>0) {
    for (i in 1:nrow(rec.trees)) {
      dend.data <- dend.data[!(dend.data$plot == rec.trees[i, 1] & dend.data$tag == rec.trees[i, 2] & 
          dend.data$year < as.numeric(year)),]
      dbh.data <- dbh.data[!(dbh.data$plot == rec.trees[i, 1] & dbh.data$tag == rec.trees[i, 2] & 
          dbh.data$year < as.numeric(year)), ]
    }
  }
}

# QA QC test plots -------------------------------------------------------------------------------------------

plot.dend <- filter(dend.data, plot == "H1", tag == 313)
ggplot(plot.dend, aes(x= date, y = dend.meas, color = as.factor(dend.num), group = dend.num)) +
  geom_line() +
  labs(title = paste(plot.dend$plot[1], plot.dend$tag[1], sep = "-"))
plot.dbh <- filter(dbh.data, plot == "F2", tag == 987)
ggplot(plot.dbh, aes(x= date, y = DBH), inherit.aes = FALSE) +
  geom_point()+
  labs(title = paste(plot.dbh$plot[1], plot.dbh$tag[1], sep = "-"))

# DBH calculations -------------------------------------------------------------------------------------------

# initialize loop with tree list and empty data.frame
trees <- as.data.frame(unique(dend.data[, c("plot", "tag")]))
any(duplicated(trees))
# dend.dates <- sort(unique(as.POSIXct(dend.data$date, format = "%Y-%m-%d", tz = "EST", origin = "1970-1-1")))
dend.dates <- sort(unique(dend.data$date))
spp.col <- as.character(apply(trees, 1, function(x) {
  spp.data$spp[spp.data$plot == as.character(x[1]) & spp.data$tag == as.numeric(x[2])]
}))
tmp <- data.frame(matrix(NA, ncol = length(dend.dates), nrow = nrow(trees)))
all.dbh <- data.frame(trees, spp.col, tmp)
names(all.dbh) <- c("plot", "tag", "spp", as.numeric(dend.dates))


start <- Sys.time()
pdf(file = "../median_dbh_plots.pdf", paper = "USr")

for (i in 1:nrow(trees)) {
  tree.name <- paste(trees[i,1], trees[i,2], sep = "-")
  dbh <- dbh.data[dbh.data$plot == trees[i, 1] & dbh.data$tag == trees[i, 2], ]
  dend <- dend.data[dend.data$plot == trees[i, 1] & dend.data$tag == trees[i, 2], ]
  x.vals <- sort(unique(dend$date))
  band.nums <- unique(dend$dend.num)
  all.estimates <- as.data.frame(matrix(NA, nrow = length(x.vals), ncol = nrow(dbh) * max(band.nums)))
  col.num <- 1
  # fill in missing data when bands switch
  if (length(band.nums) > 1) {
    for (bnd in 1:(length(band.nums) - 1)) {
      last.measure <- max(dend$date[dend$dend.num == bnd])
      first.measure.of.next <- min(dend$date[dend$dend.num == bnd + 1])
      if (last.measure < first.measure.of.next & as.numeric(first.measure.of.next) - as.numeric(last.measure) < 300) {
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
      band.measures <- dend$dend.meas[ind]
      band.dates <- dend$date[ind]
      if (any(dbh.in.rng)) {
        for (j in which(dbh.in.rng)) {
          closest <- which.min(abs(band.dates - dbh$date[j]))
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
    
    matplot(x.vals, all.estimates, main = tree.name, ylab = "DBH", xlab = "Date", xaxt = "n", type = "l", lwd = 2)
    lines(x.vals, median.estimate, lwd = 4)
    points(dbh$date, dbh$DBH, pch = 2, cex = 2)
    axis.Date(1, x.vals)
    
  } else {
    all.dbh[i, as.character(as.numeric(dbh$date))] <- dbh$DBH
  }
}
dev.off()
end <- Sys.time()
start - end

# Incorporate 1993 DBH tape measurements ---------------------------------------------------------------------

# I arbitratily chose the midpoint of 1993 for jday
names(tow.93.dbh) <- c("plot", "tag", "year", "jday", as_date("1993-182", format = "%Y-%j"))
# update dend.dates since it now has 1993 at beginning
dend.dates <- c(as_date("1993-182", format = "%Y-%j"), dend.dates)
all.dbh <- full_join(tow.93.dbh[, c(1, 2, 5)], all.dbh)
# 1993 data are at end of record. Need to resort the columns chronologically then reconvert to character column names
all.dbh <- all.dbh[, c("plot", "tag", "spp", sort(as.numeric(colnames(all.dbh[c(-1,-2,-4)]))))]
# create all.dbh.dates since they no longer correspond with dend.dates
dend.dates <- as_date(as.numeric(colnames(all.dbh[-1:-3])), origin = "1970-1-1")

# fix recruitments with missing data and extra data ----------------------------------------------------------
# this needs to go before back.calc or it will delete the filled data

years.rec <- names(recruit.tow)
for (year in years.rec) {
  rec.ind <- splus.is.element(all.dbh[, 1:2], recruit.tow[[year]][, 1:2])
  if(sum(rec.ind > 0)){
    year.ind <- 3 + which(year(dend.dates) == as.numeric(year))
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
}


# Back calculate DBH for large recruits ----------------------------------------------------------------------

# B4 726 was recruited in 2004, and C1 217, D1 266 & H1 204 were recruited in 2006 all with DBHs too large to have grown in since the
# previous recruitment surveys they were probably missed b/c they were near the plot border or b/c they looked dead but weren't we
# must back-calculate their DBHs through 1993 & insert them into the DBH & kgC data from previous years so as to be included in all
# annual summaries

#test back.calc
pl="D2"; tg = 288; spp = "hem"; start.year = 1993; end.year = 2002; min.size= 16; max.size= 30; transects= "SW"

back.calc <-function(pl, tg, spp, start.year, end.year, min.size, max.size, transects = "NONE"){
  dbh.subset <- all.dbh[,3+which(year(dend.dates) %in% start.year:end.year)]
  pik.years <- apply(dbh.subset, 1, function(x){!any(is.na(x))})
  pik.spp <- all.dbh$spp  ==  spp
  pik.size <- dbh.subset[ , ncol(dbh.subset)] >= min.size & dbh.subset[ , ncol(dbh.subset)] <= max.size
  dbh.sub <- all.dbh[pik.years & pik.spp & pik.size, ]
  
  if(transects  ==  "SW"){
    dbh.sub <- dbh.sub[grep("[A-D][1-5]", dbh.sub$plot), ] # look at just plots in the SW transect
  }
  
  ref.date <- which(!is.na(all.dbh[all.dbh$plot == pl & all.dbh$tag == tg, -(1:3)]))[1] + 3
  normalized.growth <- (dbh.sub[, -(1:3)] - dbh.sub[, ref.date]) / dbh.sub[ , ref.date] #normalized growth for similar trees
  target.tree <- all.dbh[all.dbh$plot == pl & all.dbh$tag == tg, ]
  first.dbh <- as.numeric(target.tree[ref.date])
  norm.target <- (target.tree[-(1:3)]-first.dbh) / first.dbh #normalized growth for target tree
  
  tmp <- apply(normalized.growth, 1, function(x){x-norm.target})
  tmp <- matrix(unlist(tmp), nrow = length(dend.dates), byrow = F)
  tmp <- abs(apply(tmp, 2, sum, na.rm = T))
  wts <- abs(tmp-max(tmp))/max(tmp) #weights to apply to similar trees based on how similar their growth rates were
  
  # matplot(as.numeric(dend.dates), t(normalized.growth))
  # lines(as.numeric(dend.dates), norm.target, lwd = 2)
  # lines(as.numeric(dend.dates), apply(normalized.growth, 2, weighted.mean, w = wts^2), col = 1, lty = 2, lwd = 2)
  
  modeled.tree <- apply(normalized.growth, 2, weighted.mean, w = wts^2, na.rm = T)
  # use model for time when there is no data for tree
  norm.target[is.na(norm.target)] <- modeled.tree[is.na(norm.target)]
  # convert from relative to
  target.tree <- norm.target * first.dbh + first.dbh 
  
  plot(dend.dates, target.tree, main = paste(pl, tg, sep = " "))
  all.dbh[all.dbh$plot == pl&all.dbh$tag == tg, -(1:3)] <<- target.tree
  # modeled.tree
}

back.calc("B4", 726, "ro", 1993, 2003, 30, 40, "SW")
back.calc("C1", 217, "hem", 1993, 2005, 25, 35)
back.calc("D1", 266, "wp", 1993, 2005, 40, 50)
# back.calc("H1", 204, "rm", 1993, 2005, 12, 16) # tree was dead when written as recruit; removed from data
# more trees added to back.calc in 2020 because of large recruitment size
back.calc("F4", 735, "ash", 1993, 1998, 20, 30)
back.calc("C3", 729, "yb", 1993, 1998, 17, 22)
back.calc("C2", 291, "rm", 1993, 2002, 12, 20, "SW") # odd case of tree with very little growth. maybe was thought dead prior to recruitment
back.calc("D2", 288, "hem", 1993, 2002, 15, 25)
# back.calc("D2", 698, "rm", 1993, 2002, 13, 14) # tree was dead when written as recruit; removed from data
back.calc("F3", 535, "hem", 1993, 2002, 11, 16)
back.calc("G5", 589, "hem", 1993, 2002, 11, 16)


# all of these need to be removed from the recruitment data
summary(recruit.tow)
# remove large recruits from list
recruit.tow[["1999"]] <- recruit.tow[["1999"]][!(paste0(recruit.tow[["1999"]]$plot,"-",recruit.tow[["1999"]]$tag) %in% 
    c("F4-735", "C3-729")), ]
recruit.tow[["2003"]] <- recruit.tow[["2003"]][!(paste0(recruit.tow[["2003"]]$plot,"-",recruit.tow[["2003"]]$tag) %in% 
    c("C2-291", "D2-288", "D2-698", "F3-535", "G5-589")), ]
# H1-204 and D2-698 also need to be removed from mortality data
dead.tow[["2005"]] <- dead.tow[["2005"]][!(paste0(dead.tow[["2005"]]$plot, "-", dead.tow[["2005"]]$tag) %in% "D2-698"), ]

# Combine recruit and mortality data for website -------------------------------------------------------------

ems.rec.99.21 <- bind_rows(recruit.tow, .id = "year") %>% 
  select(year, plot, tag) %>% 
  filter(!plot %in% c("G3", "H3", "H4", "A4", "A5", "B5")) %>% 
  left_join(spp.data)

ems.mort.98.21 <- bind_rows(dead.tow, .id = "year") %>% 
  select(year, plot, tag) %>% 
  filter(!plot %in% c("G3", "H3", "H4", "A4", "A5", "B5")) %>% 
  left_join(spp.data)

write.table(ems.rec.99.21, "../data/ems.rec.99.21.txt", sep = "\t", row.names = F, quote = F)
write.table(ems.mort.98.21, "../data/ems.mort.98.21.txt", sep = "\t", row.names = F, quote = F)

# Create long format to merge with HF069 archive file for trees ----------------------------------------------

all.dbh.long <- all.dbh %>% group_by(plot, tag) %>% 
  gather(date, dbh, c(4:ncol(all.dbh)), na.rm = T, convert = T) %>% 
  ungroup() %>% 
  mutate(date = as_date(date),
    year = year(date),
    jday = yday(date),
    plottag = paste(plot, tag, sep="-"),
    year = year(date),
    jday = yday(date), 
    site = "ems", 
    tree.type = "live", 
    dbh = round(dbh, 2), 
    kgc = round(calc_kgc(spp, dbh), 2),
    ba.cm2 = round((pi*(dbh/2)^2),3)) %>% # cm^2
  select(date, year, doy = jday, tree.type, site, plot, plottag, tag, species = spp, dbh, kgc, ba.cm2)

# test that data frame/ matrix version "calc.kgc" and vectorized "calc_kgc" function give same result
all.dbh.long.test <- calc.kgC("all.dbh.long", 9, 10)
mean(round(all.dbh.long.test$dbh, 2)- all.dbh.long.test$kgc)
# should == 0
rm(all.dbh.long.test)

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

write.csv(all.dbh.long, "../ems.all.dbh.long.21.csv", quote = F, row.names = F)

dead.tree.names.recent <-  unique(all.dbh.long$plottag[all.dbh.long$tree.type == "dead" & all.dbh.long$year >=2015])

trees.recently.dead <- all.dbh.long %>% group_by(plot, tag, year) %>%
  mutate(last = doy == max(doy),
    plottag = paste(plot, tag, sep = "-")) %>% 
  filter(tree.type == "dead", year >= 2015, last == T) %>% 
  select(plot, tag, plottag, species, year, dbh) %>% 
  arrange(year, plot, tag)

# write.csv(trees.recently.dead, "../ems.recently.dead.trees.21.csv", quote = F, row.names = F)

# Graphs to see how the median dbh function is working -------------------------------------------------------

# make temporary data sets for plotting recently dead trees
p <- all.dbh.long %>% filter(plottag %in% dead.tree.names.recent) %>% 
  group_by(plot, tag) %>% 
  mutate(date = as_date(date), dbh.class = floor(min(dbh)/5)*5)

p2 <- dbh.data %>% group_by(plot, tag) %>% 
  mutate(plottag = paste(plot, tag, sep="-"),
    dbh.class = floor(min(DBH)/5)*5) %>% 
  filter(plottag %in% dead.tree.names.recent)

# ptitle <- paste(p$plot, p$tag, sep = " ")[1]
ptitle <- "Recently Dead Tree DBH"
ggplot(p, aes(date, dbh, group = plottag)) + 
  geom_line(aes(color = species)) + 
  labs(title = ptitle) + 
  geom_point(data = p2, inherit.aes = F, color = "black", aes(date, DBH, group = plottag)) +
  facet_wrap(~dbh.class, scales = "free")

# other trees of interest that had overlapping bands or were back calculated
# filter(plot == 'A1', tag == 18) 
# filter(plot == 'A2', tag == 25) 
# filter(plot == 'A3', tag == 51)
# filter(plot == "C1", tag == 217)
# filter(plot == "B4", tag == 726)
# filter(plot == 'D1', tag == 537)
# filter(plot == 'D2', tag == 587) 
# filter(plot == 'D4', tag == 633)
# filter(plot == 'H1', tag == 207)

p <- all.dbh.long %>% filter(plot == 'H1', tag == 204)
p2 <- dbh.data %>% filter(plot == 'H1', tag == 204)

ptitle <- paste(p$plot, p$tag, sep = " ")[1]
ggplot(p, aes(date, dbh)) + geom_line() + labs(title = ptitle) + geom_point(data = p2, aes(date, DBH))

# calculate kgC from DBHs then convert to MgC/ha -------------------------------------------------------------

kgC <- calc.kgC("all.dbh", "spp", c(4:ncol(all.dbh)))
kgC.dates <- as_date(as.numeric(names(kgC[c(-1:-3)])))
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

matplot(kgC.dates, t(kgC.by.plot[, -1]), type = "l", ylim = c(0, max(kgC.by.plot[, -1])), ylab = "kgC", xlab = "Date", main = "Plot-level Biomass", xaxt = "n")
axis.Date(1, kgC.dates)

MgCha.by.plot <- kgC.by.plot
MgCha.by.plot[, -1] <- (MgCha.by.plot[-1]/1000)/(100 * pi/10000)
matplot(kgC.dates, t(MgCha.by.plot[, -1]), type = "l", ylim = c(0, max(MgCha.by.plot[, -1])), ylab = "MgC/ha", xlab = "Date", main = "Plot-level Biomass Density", xaxt = "n")
axis.Date(1, kgC.dates)


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

# calculate AGWI, AGWB, recruitment, and mortality -----------------------------------------------------------


years <- unique(year(kgC.dates))
# years <- years[1:21]
# inc abbreviates annual growth increment which is the annual difference in peak biomass
# inc.old uses the calculated DBH of the final measurement in the previous year as the baseline, 
# which may introduce noise into the data based on timing of the final measurement
inc <- NULL
inc.old <- NULL
inc.min <- NULL
rec <- NULL
mort <- NULL
AGWB <- NULL

for (y in 1:length(years)) {
  tmp <- kgC[, which(year(kgC.dates) == years[y]) + 3]
  if (years[y] == 1998) {
    tmp.last.year <- data.frame(v1 = kgC[, which(year(kgC.dates) == 1993) + 3])  #needs to remain data.frame class, so I duplicated the single vector
  } else if (years[y] > 1998) {
    tmp.last.year <- kgC[, which(year(kgC.dates) == years[y - 1]) + 3]
  } else if (years[y] == 1993) {
    tmp.last.year <- NULL
  }
  if (!is.null(tmp.last.year)) {
    # these original caclulations don't work because some NA's are gaps in data, 
    # and some dead trees were filled until end of the year it died
    # left in for reference only
    # absent.last.year <- apply(tmp.last.year, 1, function(x){all(is.na(x))})
    # present.this.year <- !apply(tmp, 1,function(x){all(is.na(x))}) 
    # present.last.year <- !apply(tmp.last.year, 1, function(x){any(is.na(x))}) 
    # absent.this.year <- apply(tmp, 1, function(x){any(is.na(x))})
    # This may shift some mortality one year later(see 2005), if it was recorded as dead on
    # the last recording in the year
    # recruited <- absent.last.year & present.this.year 
    # died <- present.last.year & absent.this.year
    
    died2 <- splus.is.element(kgC[, 1:2], dead.tow[[as.character(years[y])]])
    recruited2 <- splus.is.element(kgC[, 1:2], recruit.tow[[as.character(years[y])]])
    # if only one observation in given year (running script early) apply will give an error
    if(is.null(ncol(tmp))){
      max.tmp <- tmp
      min.tmp <- tmp
      max.tmp.old <- tmp
    } else {
      max.tmp <- apply(tmp, 1, max, na.rm = T)
      max.tmp[!is.finite(max.tmp)] <- NA
      min.tmp <- apply(tmp, 1, min, na.rm = T)
      tmp.colsums <- as.numeric(apply(tmp, 2, max, na.rm = T))
      max.tmp.old <- tmp[, which.max(tmp.colsums)]
    }
    max.tmp.last.year <- apply(tmp.last.year, 1, max, na.rm = T)
    max.tmp.last.year[!is.finite(max.tmp.last.year)] <- NA
    
    # hist(max.tmp-max.tmp.last.year, main = year)
    AGWB[as.character(years[y])] <- (sum(max.tmp[!died2], na.rm = T)/1000)/(100 * pi * 34/10000)
    inc[as.character(years[y])] <- (sum(max.tmp[!recruited2 & !died2] -
        max.tmp.last.year[!recruited2 & !died2], na.rm = T)/1000)/(100 * pi * 34/10000)
    inc.min[as.character(years[y])] <- (sum(max.tmp[!recruited2 & !died2] -
        min.tmp[!recruited2 & !died2], na.rm = T)/1000)/(100 * pi * 34/10000)
    inc.old[as.character(years[y])] <- (sum(max.tmp.old[!recruited2 & !died2] - 
        tmp.last.year[, ncol(tmp.last.year)][!recruited2 & !died2], na.rm = T)/1000)/(100 * pi * 34/10000)
    rec[as.character(years[y])] <- (sum(max.tmp[recruited2], na.rm = T)/1000)/(100 * pi * 34/10000)
    # if a tree died use the max from previous year when still alive
    # TGW 2020 Changed from max of current year in case tree grew or shrank in interim
    mort[as.character(years[y])] <- (sum(max.tmp.last.year[died2], na.rm = T)/1000)/(100 * pi * 34/10000)
    # this is left out, as it double counts the recruitment in calculations of net increment
    # if (any(recruited2)) {
    #   # recruits may grow after initial DBH measurement, and increment is calculated relative to first measurement
    #   recs.max <- max.tmp[recruited2]
    #   recs <- tmp[recruited2, ]
    #   rec.inc <- numeric()
    #   for (i in 1:nrow(recs)) {
    #     tmp.row <- recs[i, ][!is.na(recs[i, ])]
    #     rec.inc[i] <- ((recs.max[i] - tmp.row[1])/1000)/(100 * pi * 34/10000)
    #   }
    #   inc[as.character(years[y])] <- inc[as.character(years[y])] + sum(rec.inc)
    # }
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

# How much bigger are dead trees in 98 than their first measurement in 93?
id.died.98 <- splus.is.element(kgC[, 1:2], dead.tow[["1998"]])
kgc.93 <- kgC[, c(1, which(year(kgC.dates) == 1993)) + 3]
died98in93 <- apply(kgc.93[id.died.98,], 1, max, na.rm = T)
kgc.98 <- kgC[, c(1, which(year(kgC.dates) == 1998)) + 3]
died98in98 <- apply(kgc.98[id.died.98,], 1, max, na.rm = T)
# convert change in biomass to MG/ha
(sum(died98in98-died98in93)/1000)/(100 * pi * 34/10000)
# dead trees in 1998 were .088 MG/ha larger than in 1993, 
# this should be accounted for in carbon balance since they grew some before they died


inc["1998"]
# 7.827039 not annualized
rec["1998"] 
# 0.06988568 already annualized
mort["1998"]
# 1.961929 not annualized

# change growth increment, AGWB, and mortality, to be an an annual rate for 1993:1998

mort[as.character(1994:1998)] <- mort["1998"] / 5
inc["1998"] <- inc["1998"] / 5
# old code now simplified in for loop below
inc[as.character(1994:1997)] <- ((AGWB["1998"] - inc["1998"] + mort["1998"] - rec["1998"]) - AGWB["1993"])/4
# since dead trees grew some before found in 1998, they contributed some to AGWI in interim, AGWB cannot be linearly interpolated 
# factor mort and rec into each year to recalculate AGWB and AGWI estimate 

inc.tmp<-inc
for (y in 1997:1994){
  AGWB[as.character(y)] <- AGWB[as.character(y + 1)] - rec[as.character(y + 1)] + mort[as.character(y + 1)] - inc.tmp[as.character(y + 1)]
  inc.tmp[as.character(y)] <- (AGWB[as.character(y)]- AGWB["1993"])/ (y-1993)
}
inc.tmp <- inc.tmp[sort(names(inc.tmp))]
inc <- inc[sort(names(inc))]
cbind(inc, inc.tmp)
AGWB <- AGWB[sort(names(AGWB))]
net = diff(AGWB[])
mort <- mort[sort(names(mort))]*-1
cbind(apply(cbind(mort,rec, inc), 1, sum), net)
cbind(apply(cbind(mort,rec, inc.tmp), 1, sum), net)

# compare to method of using dbhtape measurement from beginning of 1998 as previous year's biomass
inc.old["1998"] <- inc.min["1998"]*1.25 #following adjustment for band settling seen in "tow.stats.calc.r"

inc.old[as.character(1994:1997)] <- ((AGWB["1998"] - inc.old["1998"] + mort["1998"]) - AGWB["1993"])/4
inc.old <- inc.old[sort(names(inc.old))]

# have not yet implemented the confidence intervals for this data yet. We should try and use the method Josh Benmergui worked out for
# his paper, rather than just reimplementing the old method (elg 2016)

# plot of biomass increments with new method
plot.data <- rbind(inc, rec, mort)[ , 4:length(inc)]
plot.data <- replace(plot.data, is.na(plot.data), 0)
par(mfrow = c(2, 1), mar = c(0, 5, 5, 2))

tmp <- barplot(plot.data[-3, ], yaxs = "i", ylim = c(0, 2.5), las = 2, names.arg = rep("", ncol(plot.data)), col = c(3, 4), main = "Median DBH AGWI (from peak AGWB previous year)")
# plotCI(tmp, plot.data[1, ]+plot.data[2, ], uiw = tow.ann.stats[4:21, 'pos.95CI'], add = T, type='n', gap = 0)
par(mar = c(5, 5, 0, 2), xpd = NA)
# lower = plot.data[3, ]-tow.ann.stats[4:22, 'neg.95CI'] upper = plot.data[3, ]+tow.ann.stats[4:22, 'neg.95CI']
barplot(plot.data[3, ], yaxs = "i", ylim = c(-2.5, 0), las = 2, col = 2, names.arg = c("1994:1997", as.character(years[-1])), ylab = "          Aboveground woody increment (MgC/ha)", 
  xpd = NA)
# plot.ci = T, ci.l = lower, ci.u = upper, xpd = NA, ylab='Aboveground woody increment (MgC/ha)')
legend("bottom", legend = c("growth", "recruitment", "mortality"), fill = c(3, 4, 2))


plot.data <- rbind(inc.old, rec, mort)[, 4:length(inc.old)]
plot.data <- replace(plot.data, is.na(plot.data), 0)
par(mfrow = c(2, 1), mar = c(0, 5, 5, 2))

tmp <- barplot(plot.data[-3, ], yaxs = "i", ylim = c(0, 2.5), las = 2, names.arg = rep("", ncol(plot.data)), col = c(3, 4), main = "Median DBH AGWI (from last observation of AGWB previous year)")
# plotCI(tmp, plot.data[1, ]+plot.data[2, ], uiw = tow.ann.stats[4:21, 'pos.95CI'], add = T, type='n', gap = 0)
par(mar = c(5, 5, 0, 2), xpd = NA)
# lower = plot.data[3, ]-tow.ann.stats[4:22, 'neg.95CI'] upper = plot.data[3, ]+tow.ann.stats[4:22, 'neg.95CI']
barplot(plot.data[3, ], yaxs = "i", ylim = c(-2.5, 0), las = 2, col = 2, names.arg = c("1994:1997", as.character(years[-1])), ylab = "       Aboveground woody increment (MgC/ha)", 
  xpd = NA)
# plot.ci = T, ci.l = lower, ci.u = upper, xpd = NA, ylab='Aboveground woody increment (MgC/ha)')
legend("bottom", legend = c("growth", "recruitment", "mortality"), fill = c(3, 4, 2))

# reset graping par
par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 0.1)
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
write.table(kgC, "../all.kgC.93.21.txt", sep = "\t", quote = F, row.names = F)
write.table(all.dbh, "../all.dbh.93.21.txt", sep = "\t", quote = F, row.names = F)

# Summarize AGWI table and Calculate ANPP --------------------------------------------------------------------

# initalise objects to fill in loop
inc["1993"] <- NA
inc <- inc[sort(names(inc))]
agwi <- data.frame(as.numeric(names(inc)), inc)
colnames(agwi) <- c("Year", "AGWI")
agwi <- data.frame(agwi, AGWB, recruitment = c(NA, rec), mortality = c(NA, mort))
rownames(agwi) <- NULL
anpp <- data.frame(year = 2000:max(year(dend.dates)), AGWI = NA, AGWI.ingrowth = NA, fine.litterfall = NA, ANPP = NA)
anpp$AGWI[anpp$year %in% agwi$Year] <- agwi[agwi$Year %in% anpp$year, "AGWI"]
# go through each year
years.rec <- names(recruit.tow)[-1] 
# years.rec <- years.rec[-length(years.rec)]
prev.recruit <- "1999"
for (year in years.rec) {
  # pick out new recruits tmp <-all.dbh[splus.is.element(all.dbh[, 1:2], recruit.tow[[year]][, 1:2]), c(1:3,
  # 3+which(dend.dates$year+1900 == as.numeric(year)))]
  tmp <- all.dbh[splus.is.element(all.dbh[, 1:2], recruit.tow[[year]][, 1:2]), ]
  year.cols <- 3 + which(year(dend.dates) == as.numeric(year))
  empty.ones <- apply(tmp[, year.cols], 1, function(x) {all(is.na(x))}) 
  #recruits that don't have any band readings
  if (sum(empty.ones) > 0) {
    tmp[empty.ones, year.cols[1]] <- apply(tmp[empty.ones, 3 + which(year(dend.dates) > as.numeric(year))], 1, min, na.rm = T)  #take the smallest band reading from after the year of recruitment
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
#as of November 2021, 2015 was most recent summary for leaf ANPP
lit.sum.ems <- read.table("../../litter/litter.stats.ems.98.15.txt", sep = "\t", header = T, stringsAsFactors = F)
# if need to re-run litter data, may have issues with setwd()
# source("../../litter/r.scripts/litter.12.tgw/lit.data.import.df.tgw.r")
# setwd() # reset working directory
# source("../../litter/r.scripts/litter.12.tgw/lit.sum.r")
# setwd(dir)

anpp[anpp$year %in% unique(lit.sum.ems$year), "fine.litterfall"] <- (lit.sum.ems[lit.sum.ems$year %in% anpp$year & lit.sum.ems$litter.type == "fine.litter", "Mg.c.ha"])

# calculate ANPP = AGWI +AGWI.ingrowth + fine.litterfall
anpp$ANPP <- apply(anpp[, 2:4], 1, sum)

agwi$ANPP <- NA
agwi$ANPP[agwi$Year %in% anpp$year] <- anpp$ANPP

anpp[ , 2:5] <- round(anpp[ , 2:5], 3)
agwi[ , 2:6] <- round(agwi[ , 2:6], 2)



write.csv(agwi, "../ems.agwi.21.csv", quote = F, row.names = F)
write.table(agwi, "../tow.ann.sum.93.21.txt", quote = F, row.names = F, sep = "\t")
write.csv(anpp, "../ems.anpp.21.csv", quote = F, row.names = F)
write.table(anpp, "../tow.anpp.93.21.txt", quote = F, row.names = F, sep = "\t")

source("run.all.tgw.r", echo = T)
setwd(dir)
tow.ann.sum.old <- read.csv("../tow.ann.sum.old.csv")
tow.ann.sum <- read.csv("../tow.ann.sum.new.csv")
tow.anpp.old <- read.csv("../tow.anpp.old.csv")
tow.anpp <- read.csv("../tow.anpp.new.csv")
colnames(tow.ann.sum.old)[1] <- "year"
colnames(tow.ann.sum)[1] <- "year"
colnames(tow.anpp.old)[1] <- "year"
colnames(tow.anpp)[1] <- "year"

# compare AGWI and ANPP new vs old methods ------------------------------------------------------------------- 

par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 0.1)
plot(tow.ann.sum$year, tow.ann.sum.old$AGWI, col = 2, type = "o", lty = 1, ylim = c(0.5, 4), 
  ylab = "AGWI (MgC/ha)", xlab = "Year")
lines(tow.ann.sum$year, tow.ann.sum$AGWI.new, col = 2, type = "o", lty = 2)
lines(agwi$Year[-1], inc.old, type = "o", lty = 1)
lines(agwi$Year, agwi$AGWI, type = "o", lty = 2, col = 3)

legend("topleft", legend = c("old.ref", "old.max", "new.ref", "new.max"), col = c(2, 2, 1, 3), lty = c(1, 2, 1, 2), pch = "o")

plot(c(2000:2015), tow.anpp.old$ANPP.MgC.ha[1:16], col = 2, type = "o", xlab = "Year", 
  ylab = "ANPP (MgC/ha/yr)", ylim = c(2.5, 5))
lines(anpp$year, anpp$ANPP, type = "o")
legend("topleft", legend = c("old.ref", "new.max"), col = c(2, 1), lty = c(1, 2, 1, 2), pch = "o")

# annual recruitment and mortality are now calculated in a standard way reported recruitment year are taken to be fact, even if after
# calculations they turn out to cross 10cm DBH on some other year. this is necessary because, otherwise a few trees start getting
# recruited in non-recruitment years, and this makes figuring out annual rates difficult. the alternative method for doing this would
# be to apply a universal backcalc like Josh Benmergui did to find the true recruitment date for all individuals. I'm not sure if
# this is actually better though, because we don't have good data on growth rates for trees under 10cm DBH. often this method results
# in trees recruiting many years earlier than reported, which seems unlikely, unless recruitment surveys are very bad. (elg 2016)

