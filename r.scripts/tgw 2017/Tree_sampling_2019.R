# I'll use this script as an example of how to subset the tree data
# then take a random sample within each size class

# Load packages
# if not installed use install.packages("tidyverse")
library(tidyverse)

# set working directory so R knows where to open and save files from
# R uses forward slashes unlike windows
setwd("C:/Users/whitby/Google Drive/Whitby HF files/REU Mentor/2019/REU_2019_Dendro")

# read in tree dbh data
all.dbh <- read.csv("Data/ems.all.dbh.long.18.csv")
unique(all.dbh$tree.type)

# subset only trees from most recent year and are alive
dbh.18 <- all.dbh[all.dbh$tree.type == "live"& all.dbh$year == 2018, ]
# this has multiple observations for each tree, need only last observation
dbh.18 <- dbh.18[dbh.18$doy == max(dbh.18$doy), ]
# check for duplicates
any(duplicated(dbh.18$plottag))
length(unique(dbh.18$plottag))

# make a new column with the DBH class by 10cm (we can adjust this if necesary)
# '%/%' is integer division
dbh.18$dbh.class <- (dbh.18$dbh %/% 10) * 10
# a few trees were <10, round those up
dbh.18$dbh.class[dbh.18$dbh.class==0] <- 10
# change dbh.class to a factor for grouping later
dbh.18$dbh.class<- as.factor(dbh.18$dbh.class)
levels(dbh.18$dbh.class)

# using base R my first guess is just to write a loop
samp <- data.frame()
for(i in 1:length(unique(dbh.18$dbh.class))){
  tmp <- dbh.18[dbh.18$dbh.class == unique(dbh.18$dbh.class)[i], ]
  # take 10 random rows, unless there aren't 10 observations
  samp <- rbind(samp, tmp[sample(nrow(tmp), ifelse(nrow(tmp) <10, nrow(tmp), 10)), ])
}

# using dplyr and piping can simplify the code
dbh.18.new <- all.dbh %>% filter(year==2018, tree.type=="live") %>% 
  filter(doy == max(doy)) %>% 
  mutate(dbh.class = as.factor((dbh%/%10)*10),
         dbh.class = recode_factor(dbh.class, "0" = "10", "70" = "60")) %>% 
  # only two trees above 70 dbh, combine those with 60, make class 0 == 10
  group_by(dbh.class) %>% 
  sample_n(10) %>% 
  arrange(dbh.class, plottag)

# we'll need input from Neil to see if this is enough to cross date or come up with an age distribution
# for practice, try to replicate this sampling using 5 trees from 5cm size classes
# alternatively, read the help for dplyr::sample_frac and see if you can subsample a certain portion of each species
