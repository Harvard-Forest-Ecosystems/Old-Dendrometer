library(tidyverse)
setwd("C:/Users/tgw475/Desktop/Whitby HF Files/HF_BIOMETRY_WOFSY copy/trees")
agwi.nalocf.dbh <- read.csv("ems.agwi.18.na.locf.-dbh.csv")
agwi.nalocf <- read.csv("ems.agwi.18.na.locf.csv")
agwi.old <- read.csv("ems.agwi.18.old.csv")
agwi.old.dbh <- read.csv("ems.agwi.18.old.-dbh.csv")

agwi.compare <- bind_rows("nalocf-dbh" = agwi.nalocf.dbh, "nalocf" = agwi.nalocf, "old" = agwi.old, 
          "old-dbh"= agwi.old.dbh, .id = "process")

ggplot(agwi.compare, aes(Year, AGWI, group = process, color = process)) + 
  geom_point(size =1.5, alpha = .7, position = position_jitter(height = 0, width = .3))+
  geom_line()

ggplot(agwi.compare, aes(Year, AGWB, group = process, color = process)) + 
  geom_point(size =1.5, alpha = .7, position = position_jitter(height = 0, width = .3)) +
  geom_line()


