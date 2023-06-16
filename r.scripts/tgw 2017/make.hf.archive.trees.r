# Coded Years need to be updated annually
# Also rename current ../../hf_archive/hf069-09-trees.csv file to include two digit year suffix -yy.csv
# need to run tree.figures.18.R to get tree.stats

library(tidyverse)
# Local directory for TGW 
  dir <- "G:/My Drive/Whitby HF files"
setwd(dir)
# Source hf harvest plot tree summary, which compiles all the harvest plot data
# source("HF_BIOMETRY_WOFSY copy/trees/r.scripts/cut.17.calc.r")
summary(hf069.09.cut.trees)
if(!exists("hf069.09.cut.trees")){
  hf069.09.cut.trees <- read.csv("HF_BIOMETRY_WOFSY copy/trees/data/hf069.cut.trees.csv", header = T)
  hf069.09.cut.trees$date <- as.Date(hf069.09.cut.trees$date)
}

all.dbh.long <- read.csv("HF_BIOMETRY_WOFSY copy/trees/ems.all.dbh.long.21.csv")
all.dbh.long$date <- as.Date(all.dbh.long$date)
str(all.dbh.long)
# find merged understory data from understory.trees.summry.R if not in workspace environment
if(!exists("hf.understory.trees")){
  hf.understory.trees <- read.csv("HF_BIOMETRY_WOFSY copy/trees/understory/sapling.all.dbh.04.17.csv", header = T)
  hf.understory.trees$date <- as.Date(hf.understory.trees$date)
}

hf069.09.trees.new <- bind_rows(all.dbh.long, hf069.09.cut.trees) %>% 
  bind_rows(hf.understory.trees) %>% 
  arrange(site, year, doy, tree.type, plot, tag) %>% 
  mutate(plottag = if_else(is.na(plottag) & !is.na(plot) & !is.na(tag), paste0(plot, "-", tag), as.character(plottag)),
    tag = signif(tag,1),
    tree.type= as.factor(tree.type),
    site = as.factor(site),
    plot = as.factor(plot),
    nindivs = ifelse(tree.type == "und" & is.na(nindivs), 1, nindivs)) %>% 
  select(date,	year,	doy,	tree.type,	site,	plot,	nindivs,	tag, plottag,	species,	dbh)
str(hf069.09.trees.new)
summary(hf069.09.trees.new[duplicated(hf069.09.trees.new), ])

write.csv(hf069.09.trees.new,"HF_BIOMETRY_WOFSY copy/hf_archive/hf069-09-trees.csv", quote = F, row.names = F)
write.csv(hf069.09.trees.new,"Online Datasets/Harvard Forest Archive/hf069-09-trees.csv", quote = F, row.names = F)

# make Ameriflux BADM format

tree.ann.sum <- read.table("HF_BIOMETRY_WOFSY copy/trees/tow.ann.sum.93.21.txt", sep="\t", header = T) %>% 
  mutate(site= "ems") %>% 
  select("year" = "Year", site, AGWI, AGWB, "recruit" = "recruitment", mortality, ANPP)
head(tree.ann.sum)
head(tree.stats)
tree.stats <- tree.stats %>% 
  mutate(sd = se * sqrt(n))
head(tree.stats)
ggplot(filter(tree.stats, group == "live"), aes(year, MgC.ha))+geom_line()+
geom_line(data = tree.ann.sum, aes(year, AGWB), color = "red", inherit.aes = F)
ggplot(filter(tree.stats, group == "agwi"), aes(year, MgC.ha))+geom_line()+
geom_line(data = tree.ann.sum, aes(year, AGWI), color = "red", inherit.aes = F)

# read in previous annual sum hf069-13 to get harvest plots
  # don't need to piece together harv data after 2018
  # hf069.13.2018 <- read.csv("HF_BIOMETRY_WOFSY copy/hf_archive/hf069-13-annual-summ-18.csv", header = T)
  # hf069.13.harv.2007 <- hf069.13.2018[hf069.13.2018$site == "harv", ]
  # hf069.13.harv.2017 <- read.table("HF_BIOMETRY_WOFSY copy/trees/data/cut.17.sum.txt", sep = "\t", header = T)
  # hf069.13.harv.2017[, 3:6] <- round(hf069.13.harv.2017[, 3:6], 2)

hf069.13.harv <- read.csv("HF_BIOMETRY_WOFSY copy/hf_archive/hf069-13-annual-summ-19.csv", header = T)
hf069.13.harv <- hf069.13.harv[hf069.13.harv$site == "harv", ]
# read in BADM biomass template

colnames(hf069.13.harv) <- colnames(tree.ann.sum)

hf069.13.new <- bind_rows(hf069.13.harv, tree.ann.sum)
write.csv(hf069.13.new,"HF_BIOMETRY_WOFSY copy/hf_archive/hf069-13-annual-summ.csv", quote = F, row.names = F)

hf069.12.und.sum <- read.csv("HF_BIOMETRY_WOFSY copy/trees/understory/sapling.sum.06.17.csv")
write.csv(hf069.12.und.sum,"HF_BIOMETRY_WOFSY copy/hf_archive/hf069-12-understory-summ.csv", quote = F, row.names = F)


BADM.cols <- read.csv("HF_BIOMETRY_WOFSY copy/BADM_AGBiomass_template_2014.csv")
grep(pattern = "TREE", unique(BADM.cols$Variable), value = T)

ag_biomass_long <- data.frame("AG_BIOMASS_DATE" = tree.ann.sum$Year,
  "AG_BIOMASS_TREE" = tree.ann.sum$AGWB, 
  "AG_BIOMASS_TREE_SPATIAL_REP_NUMBER" = 34, 
  "AG_BIOMASS_TREE_UNIT" = "megagram carbon / hectare", 
  "AG_BIOMASS_TREE_SPATIAL_VARIABILITY" = tree.stats$sd[tree.stats$group == "live"],
  "AG_BIOMASS_TREE_ORGAN" = "wood", 
  "AG_BIOMASS_APPROACH" = "In 34 10m radius plots, we dendrometer band each tree >=10 cm DBH, and scale to biomass using allometric equations.")

harv.tree.stats <- hf069.13.harv %>% group_by(year) %>% 
  select(year, AGWI, recruit, mortality) %>% 
  mutate(mortality = mortality*-1,
    net = AGWI + recruit + mortality) %>% 
  gather(key = "group", value = "MgC.ha", AGWI, recruit, mortality, net) %>% 
  mutate(group = factor(group, levels = c( "AGWI", "recruit", "mortality", "net"))) %>% 
  ungroup()

cols2 <- c("mortality" = "red", "recruit" = "blue", "AGWI" = "green4")
ggharv <- harv.tree.stats %>% filter(group %in% c("AGWI", "recruit", "mortality")) %>% 
  ggplot(aes(x = year, y = MgC.ha, group = group, color = group, fill = group))
ggharv + geom_col(alpha = 0.5, position = position_stack(reverse = T)) +
  scale_colour_manual(values = cols2) + scale_fill_manual(values = cols2) +
  theme_bw() +
  theme(legend.background = element_blank(), legend.title = element_blank(),
    legend.position = c(.75,.25), text = element_text(size = 18))+
  labs(title = "EMS Harvest Plots AGWI", x = "Year") + 
  scale_x_continuous(breaks = seq(1998, 2020 ,4)) +
  geom_hline(yintercept = 0) + 
  geom_point(data = filter(harv.tree.stats, group == "net"), 
    aes(x = year, y = MgC.ha), inherit.aes = F, color = "black") 
  
  
  