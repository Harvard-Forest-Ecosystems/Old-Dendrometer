rm(list = ls())
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)

# modifications/assumptions:
# use only max dbh for each year 
# filter out recruits for cumulative analysis
# keep size class constant for cumulative analysis

setwd('C:/Users/tgw475/Desktop/Whitby HF Files/HF_BIOMETRY_WOFSY copy/trees/')
all.dbh.kgC = read.csv('ems.all.dbh.long.csv')
all.dbh.kgC <- filter(all.dbh.kgC, tree.type != "recruit")


spp = "ro"
subset.spp = subset(all.dbh.kgC, species == spp) %>% group_by(plottag) %>% filter(year > 1997)

all.tags = droplevels(unique(subset.spp$plottag))
dead.tags = droplevels(unique(subset(subset.spp, tree.type == 'dead')$plottag)) #droplevels is very helpful to interpret whats going on since it only keeps T factors
subset.spp[,'mortality'] = NA
subset.spp$mortality <- ifelse(subset.spp$plottag %in% dead.tags, T, F) 
# for (ii in 1:length(dead.tags)){subset.spp$mortality[which(subset.spp$plottag == dead.tags[ii])] = T}
# for (ii in 1:nrow(subset.spp)){if(is.na(subset.spp$mortality[ii])){subset.spp$mortality[ii] = F}}

inc.list <- data.frame()
inc.list.ind <- data.frame()
spp.list.all <- data.frame()

# create dbh increments for live red maple trees
spp.inc <- subset.spp %>% 
  select(mortality, year, doy, plot, plottag, tag, dbh, KgC) %>%
  group_by(mortality, plot, tag, year) %>% 
  summarize(dbh = max(dbh), # use max dbh for each year to reduce noise
            KgC = max(KgC)) %>% 
  mutate(dbh.inc = replace(dbh, !is.na(dbh), c(0, diff(na.omit(dbh)))),
         KgC.inc = replace(KgC, !is.na(KgC), c(0, diff(na.omit(KgC)))))


# assign dbh classes and summarize by class
# assumes spp.inc is sorted by tag and date (which it is)
for(i in seq(from = 10, to = max(spp.inc$dbh, na.rm=T), by= 10)){
  spp.group.inc <- spp.inc %>% 
    group_by(mortality, plot, tag) %>% 
    mutate(init.dbh = first(dbh)) %>%
    filter(init.dbh >= i & init.dbh < i+10) %>%
    mutate(dbh.class = i) %>%
    select(mortality, plot, year, tag, dbh.inc, KgC.inc, dbh.class) %>% 
    group_by(dbh.class, mortality, year) %>% 
    summarize(dbh.inc.mean = mean(dbh.inc, na.rm=T),
              KgC.inc.mean = mean(KgC.inc, na.rm=T)) %>% 
    ungroup()
  inc.list <- bind_rows(inc.list, spp.group.inc)
  }

growth.increments <- inc.list %>% 
  group_by(mortality, dbh.class) %>% 
  mutate(cum.dbh.inc = cumsum(dbh.inc.mean),
         cum.KgC.inc = cumsum(dbh.inc.mean)) 

any(duplicated(growth.increments))

## same but not summarizing by class, instead keeping as individuals
for(i in seq(from = 10, to = max(spp.inc$dbh, na.rm=T), by= 10)){
  inc.ind <- spp.inc %>% group_by(mortality, plot, tag) %>% 
    mutate(init.dbh = first(dbh)) %>%
    ungroup() %>%
    group_by(mortality, plot, tag) %>% 
    filter(init.dbh >= i & init.dbh < i+10) %>%
    select(mortality, year, plot,tag, dbh.inc, KgC.inc) %>% 
    mutate(dbh.class = i) %>% 
    ungroup()
  inc.list.ind <- bind_rows(inc.list.ind, inc.ind )
}

ind.growth.increments <- inc.list.ind %>% 
  group_by(plot,tag) %>% 
  mutate(cum.dbh.inc = cumsum(dbh.inc),
         cum.KgC.inc = cumsum(KgC.inc)) %>%
  ungroup()
 
quant.table <- ind.growth.increments %>%
  group_by(mortality, dbh.class, year) %>% 
  summarize(quant.dbh.inc.low = quantile(cum.dbh.inc, probs = c(0), na.rm=T),
  quant.dbh.inc.high = quantile(cum.dbh.inc, probs = c(1), na.rm=T))
  

any(duplicated(ind.growth.increments))

pdf(paste('./',spp,'.yearly.size.class.comparison.const.class.pdf', sep = ''))
for(j in 1:4){ 
  plot1 <- quant.table %>% filter(dbh.class == 10*j) %>% ggplot() + 
    geom_ribbon(aes(x = year, ymin = quant.dbh.inc.low, ymax = quant.dbh.inc.high, color = mortality, fill = mortality), alpha = .3) + 
    labs(title = paste(spp," Biomass Increment, dbh class ", 10*j, sep = ""), x = "Date", y = "Cumulative Growth Incrment (Kg C)")
  print(plot1)
 
  # plot2 <- growth.increments %>% filter(dbh.class == 10*j) %>% ggplot() + 
  # geom_line(mapping = aes(x =  y = cum.dbh.inc, group = mortality, color = as.factor(mortality))) + 
  #   labs(title = paste("Red Maple Biomass Increment, dbh class ", 10*j, sep = ""), x = "Date", y = "Cumulative Growth Incrment (Kg C)", color = "DBH class")
  # print(plot2)
  
  plot3 <- ind.growth.increments %>% filter(dbh.class == 10*j) %>% ggplot() + 
    geom_line(aes(x = year, y = cum.KgC.inc, group = interaction(plot,tag), color = mortality)) + 
    labs(title = paste(spp," Biomass Increment, dbh class ", 10*j, sep = ""), x = "Date", y = "Cumulative Growth Incrment (Kg C)")
  print(plot3)
}
dev.off()

