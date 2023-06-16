# Analysis of tree species demographics and temporal shifts in HF EMS plots
# library(vegan)
library(tidyverse)
# Local directory for TGW 
# dir <- "C:/Users/tgw475/Desktop/Whitby HF Files/HF_BIOMETRY_WOFSY copy/"
# dir <- "D:/Desktop/Google Drive/Whitby HF files/HF_BIOMETRY_WOFSY copy"
# setwd(dir)

if(!exists('all.dbh.long')){
  all.dbh.long <- read.csv('../ems.all.dbh.long.21.csv', header = T)
}

all.agwb.plot.spp.yr <- ungroup(all.dbh.long) %>%
  complete(tree.type, year, plot, species) %>% 
  # need to create zero values where there are no recruits or dead trees of every species in a plot to get plot average
  mutate(kgc = ifelse(is.na(kgc), 0, kgc)) %>% 
  group_by(tree.type, year, plot, species, tag) %>%
  summarize(kgc = max(kgc, na.rm = T)) %>% #within each tree-year, keep the max kgc (same as apply(x ,1, max))
  group_by(tree.type, year, plot, species) %>%
  summarize(abundance = sum(kgc > 0, na.rm = T), # get count of individuals/plot-species-year
    kgc.sum =  sum(kgc, na.rm = T)) %>% # get total kgc in each plot-species-year
  mutate(mgc.ha = (kgc.sum / 1000) / (10^2 * pi / 10000))  # convert to MgCha

sp.full.names <- c("ro" = "Red.Oak", "bo" = "Black.Oak", "ash" = "Ash", "bb" = "Black.Birch",
  "beech" = "Beech","cherry" = "Cherry", "chestnut" = "Chestnut", "gb" = "Gray.birch", "hem" = "Hemlock",
  "rm" = "Red.Maple", "rp" = "Red.Pine", "sm" = "Striped.Maple", "wb" = "Paper.Birch", "wp" = "White.Pine",
  "ws" = "Spruce", "yb" = "Yellow.Birch")

sp.conifer <- c("hem" = "Hemlock", "rp" = "Red.Pine", "wp" = "White.Pine", "ws" = "Spruce")

sp.decid <- sp.full.names[!sp.full.names %in% sp.conifer]

# use this dataset to show annual biomass or abundance trends by species
all.agwb.spp.yr <- ungroup(all.dbh.long) %>%
  complete(tree.type, year, species) %>% 
  # need to create zero values where there are no recruits or dead trees of every species in a plot to get plot average
  mutate(kgc = ifelse(is.na(kgc), 0, kgc),
    ba.cm2 = ifelse(is.na(ba.cm2), 0, ba.cm2)) %>% 
  group_by(tree.type, year, species, tag) %>%
  slice(which.max(kgc)) %>% #within each tree-year, keep the max kgc (same as apply(x ,1, max))
  group_by(tree.type, year, species) %>%
  summarize(kgc.sum =  sum(kgc, na.rm = T),# get total kgc in each species-year
    ba.cm2.sum =  sum(ba.cm2, na.rm = T), # get total basal area in each species-year
    abundance = sum(kgc > 0, na.rm = T)) %>% # get count of individuals/species-year
  mutate(stems.ha = abundance/ (34 * 10^2 * pi / 10000),
    mgc.ha = (kgc.sum / 1000) / (34 * 10^2 * pi / 10000),
    ba.m2.ha = (ba.cm2.sum / 10000) / (34 * 10^2 * pi / 10000),
    oak.not = ifelse(species == "ro", "Red Oak", "All Other Species"),
    oak.not = factor(oak.not, levels = c("Red Oak", "All Other Species")),# create binary oak/ not oak for plotting
    rel.stems.ha = (stems.ha / sum(stems.ha))*100, # put on percent scale
    rel.mgc.ha = (mgc.ha / sum(mgc.ha))*100,
    rel.ba.m2.ha = (ba.m2.ha / sum(ba.m2.ha))*100) %>% 
  ungroup() %>% 
  mutate(species.label = recode(species, !!!sp.full.names))

# come up with RO v not RO annual biomass
live.agwb.spp.mean <- all.agwb.spp.yr %>% 
  filter(tree.type == "live") %>% 
  group_by(species, oak.not) %>% 
  summarise_at(vars(kgc.sum, mgc.ha, ba.m2.ha, stems.ha), mean) %>% 
  arrange(desc(kgc.sum))

spp.order1 <- as.character(live.agwb.spp.mean$species)

spp.order2 <- as.character(live.agwb.spp.mean$species[order(live.agwb.spp.mean$stems.ha, decreasing = T)])

all.agwb.spp.yr <- all.agwb.spp.yr %>% 
  mutate(species = factor(species, levels = spp.order1),
  species.label = factor(species.label, levels = unname(sp.full.names[spp.order1])))


agwb.spp.long <- ungroup(all.agwb.spp.yr) %>% filter(tree.type != "dead") %>% 
  select(year, species, mgc.ha ) %>% 
  # mutate(MgC.sum = mgc.ha * (34 * 10^2 * pi / 10000)) %>% 
  group_by(year, species) %>% 
  summarise(mgc.ha = round(sum(mgc.ha, na.rm=T),2)) %>% 
  arrange(species, year)

agwb.spp.wide <- agwb.spp.long %>% 
  group_by(species) %>% 
  spread(year, mgc.ha)

# setwd(dir)
# write.table(agwb.spp.wide, "trees/data/agwb.mgcha.spp.19.txt", row.names = F, sep = "\t")
# write.table(agwb.spp.long, "trees/data/agwb.mgcha.spp.long.19.txt", row.names = F, sep = "\t")

# AGWB species stacked
filter(all.agwb.spp.yr, tree.type == "live") %>% 
  ggplot(aes(x = year, y = mgc.ha, group = species.label, color = species.label, fill = species.label)) +
  geom_col(alpha = 0.4, position =  position_stack(reverse = T)) +
  # labs(y = "Above-ground Woody Biomass (Mg C/ ha)", x = "Year", title = "EMS Tree Biomass by Species ") +
  labs(y = "AGWB (Mg C / ha)", x = "Year", title = "EMS Tree Biomass by Species ") + 
  scale_x_continuous(breaks = seq(1995, 2020, by = 5)) +
  scale_fill_discrete(breaks = rev(levels(all.agwb.spp.yr$species.label))) +
  scale_color_discrete(breaks = rev(levels(all.agwb.spp.yr$species.label))) +
  theme_bw() +
  theme(text = element_text(size = 16))

# AGWB species Oak v Others
max.other.yr <- filter(all.agwb.spp.yr, oak.not == "All Other Species", tree.type == "live") %>%
  group_by(year) %>% summarize(MgC.ha = sum(mgc.ha, na.rm=T))
h <- max(max.other.yr$MgC.ha)
filter(all.agwb.spp.yr, tree.type == "live") %>% 
  ggplot(aes(x = year, y = mgc.ha, group = species, color = species.label, fill = species.label)) +
  geom_col(alpha = 0.4, position =  position_stack(reverse = T), show.legend = T) +
  labs(y = "AGWB (Mg C/ ha)", x = "Year", title = "EMS Tree Biomass by Species ", color = "Species", fill = "Species") +
  facet_wrap(~oak.not) +
  scale_x_continuous(breaks = seq(1995, 2020, by = 5)) +
  scale_fill_discrete(breaks = rev(levels(all.agwb.spp.yr$species.label))) +
  scale_color_discrete(breaks = rev(levels(all.agwb.spp.yr$species.label))) +
  theme_bw() +
  geom_hline(yintercept = h, lty = 2) +
  theme(text = element_text(size = 18), axis.text.x = element_text(angle = 90))


# Basal area 

filter(all.agwb.spp.yr, tree.type == "live") %>% 
  ggplot(aes(x = year, y = ba.m2.ha, group = species, color = species, fill = species)) +
  geom_col(alpha = 0.4, position =  position_stack(reverse = T), show.legend = T) +
  labs(y = "Basal Area (m^2/ha)", x = "Year", title = "EMS Tree Species Basal Area") +
  scale_x_continuous(breaks = seq(1995, 2020, by = 5)) +
  # facet_wrap(~oak.not) +
  theme_bw() 

# Species basal area line
filter(all.agwb.spp.yr, tree.type == "live") %>% 
  ggplot(aes(x = year, y = ba.m2.ha , group = species, color = species, fill = species)) +
  geom_line(size = 1) +
  labs(y = "Basal Area (m^2/ha)", x = "Year", title = "EMS Tree Species Basal Area") +
  scale_x_continuous(breaks = seq(1995, 2020, by = 5)) +
  # facet_wrap(~oak.not) +
  theme_bw() 


# re-order species factors for stems.ha
all.agwb.spp.yr <- all.agwb.spp.yr %>% mutate(species = factor(species, levels = spp.order2),
  species.label = factor(species.label, levels = unname(sp.full.names[spp.order2])))


# Species Stems/ha
filter(all.agwb.spp.yr, tree.type == "live") %>% 
  ggplot(aes(x = year, y = stems.ha, group = species.label, color = species.label, fill = species.label)) +
  geom_col(alpha = 0.4, position =  position_stack(reverse = T), show.legend = T) +
  labs(y = "stems/ha", x = "Year", title = "EMS Tree Species Stems per Hectare", color = "Species", fill = "Species") +
  scale_x_continuous(breaks = seq(1995, 2020, by = 5)) +
  scale_fill_discrete(breaks = rev(levels(all.agwb.spp.yr$species.label))) +
  scale_color_discrete(breaks = rev(levels(all.agwb.spp.yr$species.label))) +
  # facet_wrap(~oak.not) +
  theme_bw() +
  theme(text = element_text(size = 16))

# Species relative stems/ha
filter(all.agwb.spp.yr, tree.type == "live") %>% 
  ggplot(aes(x = year, y = rel.stems.ha, group = species, color = species, fill = species)) +
  geom_line(size = 1) +
  labs(y = "Relative stems/ha", x = "Year", title = "EMS Tree Species Relative Stems per Hectare") +
  scale_x_continuous(breaks = seq(1995, 2020, by = 5)) +
  # facet_wrap(~oak.not) +
  theme_bw() 


##### summarize and plot based on conifer to deciduous #####
all.con.decid.yr <- # use this dataset to show annual biomass or abundance trends by species
  all.agwb.spp.yr <- ungroup(all.dbh.long) %>%
  complete(tree.type, year, species) %>% 
  # need to create zero values where there are no recruits or dead trees of every species in a plot to get plot average
  mutate(kgc = ifelse(is.na(kgc), 0, kgc),
    ba.cm2 = ifelse(is.na(ba.cm2), 0, ba.cm2),
    species.label = recode(species, !!!sp.full.names),
    c.d = ifelse(species.label %in% sp.conifer, 'conifer', 'deciduous')) %>% 
  group_by(tree.type, year, species, tag) %>%
  slice(which.max(kgc)) %>% #within each tree-year, keep the max kgc (same as apply(x ,1, max))
  group_by(tree.type, year, c.d) %>%
  summarize(kgc.sum =  sum(kgc, na.rm = T),# get total kgc in each species-year
    ba.cm2.sum =  sum(ba.cm2, na.rm = T), # get total basal area in each species-year
    abundance = sum(kgc > 0, na.rm = T)) %>% # get count of individuals/species-year
  mutate(stems.ha = abundance/ (34 * 10^2 * pi / 10000),
    mgc.ha = (kgc.sum / 1000) / (34 * 10^2 * pi / 10000),
    ba.m2.ha = (ba.cm2.sum / 10000) / (34 * 10^2 * pi / 10000),
    rel.stems.ha = (stems.ha / sum(stems.ha))*100, # put on percent scale
    rel.mgc.ha = (mgc.ha / sum(mgc.ha))*100,
    rel.ba.m2.ha = (ba.m2.ha / sum(ba.m2.ha))*100) %>% 
  mutate(across(kgc.sum:rel.ba.m2.ha, round, digits = 2)) %>% 
  ungroup() %>% 
  arrange(tree.type, c.d, year)

live.con.decid.yr <- all.con.decid.yr %>% filter(tree.type == 'live') %>% 
  select(-tree.type)

write.csv(live.con.decid.yr, '../data/conifer_deciduous_live_agwb_9321.csv', row.names = F, quote = F)



# #### Ordination Plots ------------------------------------------------------------------------------
# # use this dataset for plot-level species data in ordination plots based on abundance
# live.pyr.sp.a <- all.agwb.plot.spp.yr %>% 
#   group_by(tree.type, year, plot) %>% 
#   filter(tree.type != "dead") %>% 
#   unite(pyr, plot, year, sep = "-") %>% 
#   group_by(pyr, species) %>% 
#   summarize(abundance = sum(abundance)) %>% 
#   # group_by(pyr) %>% 
#   # mutate(rel.abund = round(abundance / sum(abundance), 3)) %>% 
#   select(pyr, species, abundance) %>% 
#   ungroup() %>% 
#   mutate(species = recode(species, !!!sp.full.names)) %>% 
#   spread(species, abundance, fill = 0, drop = F) %>% 
#   arrange(pyr)
# 
# # environmental covariates for plotting in ordination space
# live.pyr.env <- all.agwb.plot.spp.yr %>% 
#   group_by(tree.type, year, plot) %>% 
#   filter(tree.type != "dead") %>% 
#   group_by(year, plot) %>% 
#   summarize(stems.ha = sum(abundance, na.rm = T)/ (10^2* pi/ 10000),
#     biomass = sum(mgc.ha, na.rm = T))
# 
# # get LAI data
# source("lai/R.scripts/lai.calc.2017.tgw.R")
# setwd("../trees/r.scripts/")
# lai.pyr <- lai.ems.plots %>% 
#   group_by(plot, year) %>%
#   summarise(LAI.max = max(LAI, na.rm = T),
#     LAI.decid.max = max(LAI.decid, na.rm = T))
# 
# # get plot growth rates from "growth_increments_by_spp.R"
# source("tgw 2017/growth_increments_by_spp.R")
# # join lai and plot growth to env
# live.pyr.env <- live.pyr.env %>% 
#   left_join(lai.pyr) %>% 
#   left_join(tree.growth.pyr) %>% 
#   rename("growth.rate" = "MgC.ha.yr") %>% 
#   unite("pyr", plot, year, sep = "-") %>% 
#   arrange(pyr)
# 
# all.equal(live.pyr.env$pyr, live.pyr.sp.a$pyr)
# 
# tree.MDS.4 <- metaMDS(live.pyr.sp.a[,-1], k = 4, trymax = 25)
# # tree.MDS.3 <- metaMDS(live.pyr.sp.a[,-1], k = 3, trymax = 25) 
# stressplot(tree.MDS.4)
# tree.env.12 <- envfit(tree.MDS.4, live.pyr.env[-1], na.rm = T, choices = c(1,2))
# tree.env.13 <- envfit(tree.MDS.4, live.pyr.env[-1], na.rm = T, choices = c(1,3))
# 
# plot(tree.MDS.4, choices = c(1,2))
# plot(tree.env.12)
# 
# # extract scores for manual plotting in ggplot
# tree.MDS.4.scores <- scores(tree.MDS.4, choices = c(1:3))
# sp.scores <- scores(tree.MDS.4, display = "species", choices = c(1:3)) %>% 
#   as.data.frame() %>% 
#   rownames_to_column(var = "spp")
# # need to extract vector scores one pair at a time, or they will be scaled differently 
# # based on how well correlated they are with all three axes, not just the two you are plotting
# env.scores.12 <- scores(tree.env.12, display = "vectors") %>% 
#   as.data.frame() %>% 
#   rownames_to_column(var = "var") %>% 
#   mutate(r2  = tree.env.12$vectors$r, 
#     p = tree.env.12$vectors$pvals, 
#     col = as.numeric(p < 0.05))
# env.scores.13 <- scores(tree.env.13, display = "vectors") %>% 
#   as.data.frame() %>% 
#   rownames_to_column(var = "var") %>% 
#   mutate(r2  = tree.env.13$vectors$r, 
#     p = tree.env.13$vectors$pvals, 
#     col = as.numeric(p < 0.05))
# 
# mds.plot <- cbind(live.pyr.env, tree.MDS.4.scores) %>% 
#   separate(pyr, c("plot", "year"), sep = "-", remove = F) %>% 
#   as.data.frame()
# 
# range01 <- function(x){(x-min(x))/(max(x)-min(x))}
# 
# # plot NMDS ordination axis 1 and 2
# ggplot(mds.plot, aes(NMDS1, NMDS2, color = as.numeric(year), group = plot)) +
#   geom_text(aes(label = plot, alpha = range01(as.numeric(year))), size =3)+ 
#   scale_alpha_continuous(guide = 'none', range = c(0.5, 1)) +
#   scale_color_continuous(low = "orange", high = "blue") +
#   lims(x = c(-1, 1.5)) +
#   # geom_line(data = mds.plot, aes(NMDS1, NMDS3, group = plot), inherit.aes = F, show.legend = F) +
#   labs(color = "Year") +
#   coord_fixed() +
#   # environmental vectors
#   geom_segment(data = filter(env.scores.12, col == 1) ,aes(x = 0,xend = NMDS1,y = 0, yend = NMDS2 ),
#     arrow = arrow(length = unit(0.5, "cm")), inherit.aes=FALSE) +
#   geom_text(data = filter(env.scores.12, col == 1), aes(NMDS1, NMDS2, label = var), size=4, inherit.aes=FALSE) +
#   # species points
#   geom_text(data = sp.scores, aes(NMDS1, NMDS2, label = spp), col = "green4", size=5, inherit.aes=FALSE) +
#   theme_bw()
# 
# # plot NMDS ordination axis 1 and 3
# ggplot(mds.plot, aes(NMDS1, NMDS3, color = as.numeric(year), group = plot)) +
#   geom_text(aes(label = plot, alpha = range01(as.numeric(year))), size =3)+ 
#   scale_alpha_continuous(guide = 'none', range = c(0.5, 1)) +
#   scale_color_continuous(low = "orange", high = "blue") +
#   lims(x = c(-1, 1.5)) +
#   # geom_line(data = mds.plot, aes(NMDS1, NMDS3, group = plot), inherit.aes = F, show.legend = F) +
#   labs(color = "Year") +
#   coord_fixed() +
#   # environmental vectors
#   geom_segment(data = filter(env.scores.13, col == 1) ,aes(x = 0,xend = NMDS1,y = 0, yend = NMDS3 ),
#     arrow = arrow(length = unit(0.5, "cm")), inherit.aes=FALSE) +
#   geom_text(data = filter(env.scores.13, col == 1), aes(NMDS1, NMDS3, label = var), size=4, inherit.aes=FALSE) +
#   # species points
#   geom_text(data = sp.scores, aes(NMDS1, NMDS3, label = spp), col = "green4", size=5, inherit.aes=FALSE) +
#   theme_bw()
#   



