# ---------------------------------------------------------------------------------
#  create average growth increment curves for back-calc and gap filling between recruitment and dendroband installation
#  will have an avearage DBH increment

### you'll need to change object names to match your scripts

# inc.list <- data.frame()
# species.list <- data.frame()
# 
# # using alternative method for getting median DBH
# for(s in unique(dend.dbh$species)){
#   subset <- dend.dbh %>% filter(species == s & mort == 0) %>% 
#     group_by(plot, tag) %>% 
#     mutate(dbh.inc = replace(dbh.med, !is.na(dbh.med), c(0, diff(na.omit(dbh.med))))) %>% 
#     select(date, year, jday, plot, tag, dend.num, dbh.med, dbh.inc) %>%
#     unite(plottag, plot, tag) %>% # simplify tree ID
#     group_by(plottag, date) %>% 
#     summarize(dbh.inc = mean(dbh.inc),dbh.med = mean(dbh.med)) %>% #  when there are multiple overlapping bands use the mean value
#     ungroup()
#   a <-1
#   for(i in seq(from = 10, to = max(subset$dbh.med, na.rm=T), by= 10)){
#     inc <- subset %>% group_by(plottag) %>% 
#       filter(dbh.med >= i & dbh.med < i+10) %>%
#       select(date, plottag, dbh.inc) %>% 
#       group_by(date) %>% 
#       summarize(dbh.inc.mean = mean(dbh.inc, na.rm=T)) %>% 
#       mutate(species = s, 
#              dbh.class = i) %>% 
#       ungroup()
#     inc.list <- bind_rows(inc.list, inc)
#     a<-a+1
#   }
#   species.list <- bind_rows(species.list, inc.list)
#   inc.list <- data.frame()
# }
# 
# growth.increments <- bind_rows(species.list) %>% 
#   group_by(species, dbh.class) %>%
#   mutate(cum.inc = cumsum(dbh.inc.mean))
# 
# any(duplicated(growth.increments))
# 
# ggplot(growth.increments, aes(date, dbh.inc.mean, group = interaction(species, dbh.class), color = species)) + geom_line()
# growth.increments %>% ggplot(aes(date, cum.inc, group = dbh.class, color = as.factor(dbh.class))) + geom_line() +facet_wrap(~species)


# from here you can write a function that subsets the species and size-class of interest
# it would subtract the cum.inc value for the date the DBH value was taken
# then it would add the actual DBH to shift it to the proper size

# using the output from current version of run.all.new.format...



library(ggpubr)
library(ggplot2)
library(dplyr)
library(tidyr)

# read in tree data
# all.dbh.long <- read.csv("../ems.all.dbh.long.21.csv")
# create filter for trees that have died
dead.pt <- unique(all.dbh.long$plottag[all.dbh.long$tree.type == "dead"])

#species full names for plotting

sp.full.names <- c("ro" = "Red.Oak", "bo" = "Black.Oak", "ash" = "Ash", "bb" = "Black.Birch",
  "beech" = "Beech","cherry" = "Cherry", "chestnut" = "Chestnut", "gb" = "Gray.birch", "hem" = "Hemlock",
  "rm" = "Red.Maple", "rp" = "Red.Pine", "sm" = "Striped.Maple", "wb" = "Paper.Birch", "wp" = "White.Pine",
  "ws" = "Spruce", "yb" = "Yellow.Birch")

# yearly growth increment by indvidual tree --------------------------------------------------------
tree.growth.inc.yr <- all.dbh.long %>% 
  filter(year >= 1998) %>%
  mutate(species = recode(species, !!!sp.full.names)) %>% 
  group_by(year, plottag, species) %>% 
  summarize_at(vars(dbh, kgc), max, na.rm =T) %>% 
  group_by(plottag, species) %>% 
  mutate(dbh.inc = replace(dbh, !is.na(dbh), c(0, diff(na.omit(dbh)))),
    kgc.inc = replace(kgc, !is.na(kgc), c(0, diff(na.omit(kgc)))),
    dbh.class = (cut(min(dbh, na.rm = T), breaks = seq(from = 0, to = max(.$dbh, na.rm = T), by = 10), labels = F)-1)*10,
    dbh.class = ifelse(dbh.class == 0, 10, dbh.class),
    cum.dbh.inc = cumsum(dbh.inc),
    cum.kgc.inc = cumsum(kgc.inc)) %>% 
  select(plottag, species, everything())

unique(tree.growth.inc.yr$species)

tree.growth.inc.live.yr <- filter(tree.growth.inc.yr, !plottag %in% dead.pt)

# ggplot(tree.growth.inc, aes(year, dbh, group = interaction(species, dbh.class), color = species)) + geom_line()
tree.growth.inc.live.yr %>% ggplot(aes(year, cum.dbh.inc, group = plottag, color = as.factor(dbh.class))) + 
  geom_line() +
  facet_wrap(~species) +
  labs(y="Cumulative Tree Growth (cm DBH)", title = "Tree Growth Rates by Species and Size Class",
    x = "Year", color = "DBH class") +
  theme_bw()

# sub-season growth increment by species and size class --------------------------------------------
tree.growth.inc.mn <- all.dbh.long %>%
  filter(year >= 1998) %>%
  mutate(species = recode(species, !!!sp.full.names)) %>% 
  group_by(plottag) %>% 
  mutate(dbh.inc = replace(dbh, !is.na(dbh), c(0, diff(na.omit(dbh)))),
    kgc.inc = replace(kgc, !is.na(kgc), c(0, diff(na.omit(kgc)))),
    dbh.class = (cut(min(dbh, na.rm = T), breaks = seq(from = 0, to = max(.$dbh, na.rm = T), by = 10), labels = F)-1)*10,
    dbh.class = ifelse(dbh.class == 0, 10, dbh.class)) %>%  
  group_by(date, species, dbh.class) %>% 
  summarize_at(vars(dbh.inc, kgc.inc), mean, na.rm = T) %>% 
  group_by(species, dbh.class) %>% 
  mutate(date = as.Date(date),
    cum.dbh.inc = cumsum(dbh.inc),
    cum.kgc.inc = cumsum(kgc.inc))

tree.growth.inc.live.mn <- all.dbh.long %>%
  filter(year >= 1998, !plottag %in% dead.pt) %>%
  mutate(species = recode(species, !!!sp.full.names)) %>% 
  group_by(plottag) %>% 
  mutate(dbh.inc = replace(dbh, !is.na(dbh), c(0, diff(na.omit(dbh)))),
    kgc.inc = replace(kgc, !is.na(kgc), c(0, diff(na.omit(kgc)))),
    dbh.class = (cut(min(dbh, na.rm = T), breaks = seq(from = 0, to = max(.$dbh, na.rm = T), by = 10), labels = F)-1)*10,
    dbh.class = ifelse(dbh.class == 0, 10, dbh.class)) %>%  
  group_by(date, species, dbh.class) %>% 
  summarize_at(vars(dbh.inc, kgc.inc), mean, na.rm = T) %>% 
  group_by(species, dbh.class) %>% 
  mutate(date = as.Date(date),
    cum.dbh.inc = cumsum(dbh.inc),
    cum.kgc.inc = cumsum(kgc.inc))

tree.growth.inc.live.mn %>% ggplot(aes(date, cum.kgc.inc, group = interaction(species, dbh.class), color = as.factor(dbh.class))) + 
  geom_line() +facet_wrap(~species) + 
  labs(y="Cumulative Carbon Accumulation (Kg C)", x = "",title = "Biomass Accumulation Rates by Species and Size Class", 
    color = "DBH class") +
  theme_bw() #+ theme(legend.position = c(0.7, 0.1 ), legend.direction = "horizontal")

# Plot-level growth increment for ordination envfit ------------------------------------------------
tree.growth.pyr <- tree.growth.inc.yr %>% 
  filter(year > 1998) %>% # omit first year which is 0 in the cumulative calc
  separate(plottag, c("plot", "tag"), sep = "-") %>% 
  group_by(year, plot) %>% 
  summarize(MgC.ha.yr = (sum(kgc.inc)/ 1000) / (10^2 * pi / 10000)) %>% 
  arrange(plot, year) #%>% # convert to MgCha
  # mutate(MgC.ha.yr = ifelse(year == 1998, MgC.ha.yr/5, MgC.ha.yr)) # account for time gap

tree.growth.pyr %>% ggplot(aes(year, MgC.ha.yr, group = plot, color = plot)) + 
  geom_line() 


# Species level growth / year ------------------------------------------------
tree.growth.syr <- tree.growth.inc.yr %>% 
  filter(year > 1998) %>% # omit first year which is 0 in the cumulative calc
  separate(plottag, c("plot", "tag"), sep = "-") %>% 
  group_by(year, species) %>% 
  summarize(MgC.ha.yr = (sum(kgc.inc)/ 1000) / (34* 10^2 * pi / 10000)) %>% 
  arrange(species, year) %>% 
  mutate(species.label = factor(species, levels = sp.full.names[spp.order1]),
    oak.not = ifelse(species.label == "Red.Oak", 1, 0)) #%>% # convert to MgCha
# mutate(MgC.ha.yr = ifelse(year == 1998, MgC.ha.yr/5, MgC.ha.yr)) # account for time gap

gg.agwi1 <- tree.growth.syr %>% 
  ggplot(aes(year, MgC.ha.yr, group = species.label, color = species.label)) + 
  geom_line(size = 1.2) +
  labs(x="", y = "AGWI (Mg C/ ha)", color = "Species", title = "Tree species annual carbon accumulation") +
  scale_x_continuous(breaks = seq(2000, 2020, by = 5)) +
  theme_bw() +
  theme(plot.margin = margin(1,1,0,7),  axis.title.x = element_blank(), text = element_text(size = 16))
gg.agwi2 <- tree.growth.syr %>% 
  ggplot(aes(year, MgC.ha.yr, group = species.label, color = species.label)) + 
  geom_line(size = 1.2) +
  labs(x="Year",  y = "AGWI (Mg C/ ha)",color = "Species", title = "") +
  theme_bw() + 
  theme(plot.margin = margin(0,1,1,1), plot.title = element_blank(), text = element_text(size = 16)) +
  scale_x_continuous(breaks = seq(2000, 2020, by = 5)) +
  scale_y_continuous(breaks = seq(0, .2, by = .04), limits = c(0,.12))

ggarrange(gg.agwi1, gg.agwi2, common.legend = T, ncol = 1, nrow = 2, legend = "right")
