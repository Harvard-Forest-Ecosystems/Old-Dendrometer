# use to read in and format harvest site DBH from 2017
library(dplyr)
library(ggplot2)
library(lubridate)
library(purrr)
setwd("C:/Users/tgw475/Desktop/Whitby HF Files/HF_BIOMETRY_WOFSY copy/trees")
source("r.scripts/calc.kgC.r")
source("r.scripts/tgw 2017/calc_kgc_tgw2.r")

cut.17.dbh <- read.table("data/DBH/2017/cut.17.dbh.txt", header = T, sep = "\t") %>% 
  mutate(plottag = paste0(plot,"-",tag))
# used wrong column from previous year for ref.dbh, updated below
cut.17.rcrt <- read.table("data/DBH/2017/cut.17.recruits.txt", header = T, sep = "\t") %>% 
  mutate(plottag = paste0(plot,"-",tag))
cut.17.mort <- read.table("data/DBH/2017/cut.17.mort.txt", header = T, sep = "\t") %>% 
  mutate(plottag = paste0(plot,"-",tag))

HF.archive.cut.trees <- read.csv("../../Online Datasets/Harvard Forest Archive/hf069-09-trees-18.csv") %>% 
  filter((site=="harv" & tree.type %in% c("live", "dead", "rcrt", "recruit"))) %>% 
  mutate(species = tolower(species),
    species = recode(species, "ac" = "chestnut", "ab" = "beech", "haw" = "ht", "eh" = "hem", "bc" = "cherry", "pb" = "wb"),
    tree.type = recode(tree.type, "rcrt" = "recruit"),
    date = as_date(date))
sort(unique(HF.archive.cut.trees$species))
head(HF.archive.cut.trees)

rcrt.plottag <- unique(cut.17.rcrt$plottag)
mort.plottag <- unique(cut.17.mort$plottag)

# reformat cut.17.dbh to match hf.archive
cut.17.dbh <- cut.17.dbh %>% 
  mutate(year = 2017,
    doy = 173, 
    date = as.Date(paste0(year, "-", doy), format = "%Y-%j", tz = "EST"),
    dbh = ifelse(!is.na(dbh), dbh, ref.dbh),
    tree.type = ifelse(plottag %in% rcrt.plottag, "recruit", 
      ifelse(plottag %in% mort.plottag, "dead", "live")),
    nindivs = NA,
    site = "harv") %>% 
  select("date", "year", "doy", "tree.type", "site", "plot", "nindivs", "tag", "plottag", "spp", "dbh")

cut.07.dbh.df <- as.data.frame(cut.07.dbh) %>% 
  mutate(plot = recode(plot, "Marsha" = "X1", "Greg" = "X2", "Peter" = "X3", "Jan" = "X4",
    "Bobby" = "X5", "Cindy" = "X6"),
    plottag = paste0(plot, "-", tag))

# use final measurement of 2007 as ref.dbh for 2017
cut.17.dbh <- left_join(cut.17.dbh, cut.07.dbh.df[, c("plottag", "jday.323")], by = "plottag") %>% 
  mutate(jday.323 = round(jday.323, 4)) %>% 
  select("date", "year", "doy", "tree.type", "site", "plot", "nindivs", "tag", "plottag", "spp", "ref.dbh" = "jday.323", "dbh")

cut.17.kgc <- cut.17.dbh %>% 
  mutate(ref.kgc = calc_kgc(spp, ref.dbh),
    kgc = calc_kgc(spp, dbh)) %>% 
  select(-contains("dbh"))

cut.07.kgc.df <- as.data.frame(cut.07.kgC) %>% 
  mutate(plot = recode(plot, "Marsha" = "X1", "Greg" = "X2", "Peter" = "X3", "Jan" = "X4",
    "Bobby" = "X5", "Cindy" = "X6"),
    plottag = paste0(plot, "-", tag))

# test that old matrix-based calc.kgc give same result as vector-based calc_kgc
cut.17.kgc.test <- cut.17.dbh[cut.17.dbh$tree.type == "live", c("plot", "tag", "plottag", "spp", "dbh", "ref.dbh")] %>% 
  left_join(cut.07.kgc.df[, c("plottag", "jday.323")], by = "plottag") %>% 
  select(plot, tag, plottag, spp, jday.323, ref.dbh, dbh)

cut.17.kgc.test <- calc.kgC("cut.17.kgc.test", 4, 6:7)
# methods agree within rounding error
rm(cut.17.kgc.test)

# separate out mort and recruit dbh ----------------------------------------------------------------
cut.17.mort.dbh <- filter(cut.17.dbh, tree.type == "dead")
cut.17.mort.kgc <- filter(cut.17.kgc, tree.type == "dead")
# remove dead from live dbh and kgc
cut.17.dbh <- filter(cut.17.dbh, tree.type == "live")
cut.17.kgc <- filter(cut.17.kgc, tree.type == "live")
# calculate recruit kgc
cut.17.rcrt.dbh <- cut.17.rcrt %>% 
  mutate(year = 2017,
  doy = 173, 
  date = as.Date(paste0(year, "-", doy), format = "%Y-%j", tz = "EST"),
  ref.dbh = NA,
  tree.type = "recruit", 
  nindivs = NA,
  site = "harv") %>% 
  select("date", "year", "doy", "tree.type", "site", "plot", "nindivs", "tag", "plottag", "spp", "dbh")
cut.17.rcrt.kgc <- cut.17.rcrt.dbh %>% 
  mutate(kgc = calc_kgc(spp, dbh),
    dbh = NULL)

cut.17.dbh.all <- bind_rows(cut.17.dbh, cut.17.mort.dbh) %>% 
  bind_rows(cut.17.rcrt.dbh)
cut.17.kgc.all <- bind_rows(cut.17.kgc, cut.17.mort.kgc) %>% 
  bind_rows(cut.17.rcrt.kgc)
sum(duplicated(cut.17.dbh.all$plottag))

# create cut.ann.sum.17 to append to tree site summary
# requires outputs from run.all.r to be in environment
cut.17.sum <- data.frame("year" = 2017, site = "harv", "AGWI" = 0, "AGWB" = 0, "recruit" = 0, "mort" = 0, ANPP = NA) #, "AGWI.alt" = 0, "mort.alt" = 0)
# only one obs per plot
cut.17.sum$AGWB <- sum(cut.17.kgc$kgc, na.rm = T)/1000/(15^2 * pi * 8) * 10000
# annual rate AGWI
cut.17.sum$AGWI <- (cut.17.sum$AGWB - (sum(cut.17.kgc$ref.kgc, na.rm = T)/1000/(15^2 * pi * 8) * 10000)) / (2017-2007)
# recruit and mort sums
cut.17.sum$recruit <- (sum(cut.17.rcrt.kgc$kgc, na.rm = T)/1000/(15^2 * pi * 8) * 10000) / (2017-2007) #were recruits not measured in 2007?
cut.17.sum$mort <- (sum(cut.17.mort.kgc$kgc, na.rm = T)/1000/(15^2 * pi * 8) * 10000) / (2017-2007)
# cut.17.sum$mort.alt <- (sum(cut.07.kgc.df[cut.07.kgc.df$plottag %in% mort.plottag, "jday.323"], na.rm = T)/1000/(15^2 * pi * 8) * 10000) / (2017-2007)
# cut.17.sum$AGWI.alt <- cut.17.sum$AGWI + (cut.17.sum$mort - ((sum(cut.17.mort.kgc$ref.kgc, na.rm = T)/1000/(15^2 * pi * 8) * 10000) / (2017-2007)))

write.table(cut.17.sum, "data/cut.17.sum.txt", row.names = F, sep = "\t")



summary(HF.archive.cut.trees)

colnames(cut.17.dbh.all)
str(cut.17.dbh.all)
cut.17.dbh.all <- rename(cut.17.dbh.all, "species" = "spp")

hf069.09.cut.trees <- bind_rows(HF.archive.cut.trees, cut.17.dbh.all[, colnames(HF.archive.cut.trees)])
str(hf069.09.cut.trees)
write.csv(hf069.09.cut.trees, "data/hf069.cut.trees.csv", row.names = F)

