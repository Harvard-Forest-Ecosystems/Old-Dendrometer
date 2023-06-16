library(tidyverse)


# Import qa/qc dataset from "tree.qaqc.test.R"
setwd(dir="C:/Users/tgw475/Desktop/Whitby HF Files/HF_BIOMETRY_WOFSY copy/trees/")
source("r.scripts/tgw 2017/new.dbh.fit.tgw.r")

# create simplified dataframe of linear model summaries observed DBH(cm) ~ year calculated in new.dbh.fit.tgw.r
dbh.date.tidy.slice <- dbh.date.tidy %>% 
  group_by(plt, tag) %>% 
  slice(1) %>% 
  select(plt, tag, slope, int, rsq)

# combine full dataset of dendroband measurements with linearmodel values
dend.dbh.calc <- dend.dbh %>% 
  left_join(dbh.date.tidy.slice) %>% 
  left_join(dbh.date.augment[,c(1,2,4:14)]) %>% 
  mutate(rel.date = decimal_date(date) - 1998) %>% 
  group_by(plt, tag, dend.num)

# remove trees on the year they were recorded as dead

# separate trees out on the year they were recruited 

# fill refdbh, refdend, refdbh.lm to begin calculating DBH's
dend.dbh.calc <- dend.dbh.calc %>% 
  mutate(refdend = ifelse(is.na(DBH), NA, dend.meas),
         refdbh = DBH,
         refdbh.lm = `.fitted`)
dend.dbh.calc <- dend.dbh.calc %>% 
  arrange(plt, tag, year, jday, dend.num) %>% 
  # first fill in chronological order
  fill(refdend, refdbh, refdbh.lm, .direction = "down") %>% 
  # then fill in reverse chronological order to get earliest measurements for each dendroband if no DBH was taken initially
  fill(refdend, refdbh, refdbh.lm, .direction = "up")

source("r.scripts/tgw 2017/dbh.fcn.tgw.r")

dend.dbh.calc <- ungroup(dend.dbh.calc) %>%
  mutate(dbhcalc = calc.dbh(dend.meas, refdend, refdbh),
         dbhcalc.lm = calc.dbh(dend.meas, refdend, refdbh.lm))

dend.dbh.med.calc <- dend.dbh %>%
  ungroup() %>%
  mutate(kgc = ifelse(is.na(dbh.med), NA, calc.kgc(spp, dbh.med)))

kgc.med.plot.year <- dend.dbh.med.calc %>% 
  select(plt,tag,dend.num,year,jday,date,kgc) %>% 
  group_by(plt,tag,year) %>% 
  filter(kgc == max(kgc)) %>% 
  group_by(plt, year) %>% 
  summarise(kgc = sum(kgc)) %>% 
  mutate(Mgc.ha = kgc /(100*pi) /1000 *10000)

kgc.med.year<- kgc.med.plot.year %>% 
  group_by(year) %>% 
  summarise_at(vars(kgc, Mgc.ha), funs(mean, se = sd(.)/sqrt(n())))

source("r.scripts/tgw 2017/calc.kgc.tgw2.r")

dend.dbh.calc <- dend.dbh.calc %>% 
  group_by(spp) %>% 
  mutate(kgc = calc.kgc(spp, dbhcalc),
         kgc.lm = calc.kgc(spp, dbhcalc.lm))

summary(dend.dbh.calc)
 