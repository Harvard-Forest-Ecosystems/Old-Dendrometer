# Updated Scripts to visualize the tree biomass data from the median DBH calculations
# requires run.all.new.format.nalocf.18.R

library(tidyverse)
library(lubridate)
library(zoo)

se <- function(x){
  sd(x)/sqrt(length(x))
}

full.years <- data.frame(year = 1993:2020)

# recreate total AGWB plot with dplyr ------------------------------------------------------------------------

agwb.stats <- all.dbh.long %>% 
  filter(!tree.type == "dead",
         !species %in% c("ws", "rp")) %>% 
  group_by(plot, date) %>%
  summarize_at(vars(KgC, BA), funs(sum), na.rm = T) %>%  
  mutate(MgC.ha = (KgC / 1000) / ((10^2 * pi) / 10000),
         BA.ha = BA / ((10^2 * pi) / 10000)) %>% 
  group_by(date) %>% 
  summarize_at(vars(MgC.ha, BA.ha), funs(mean, se, n())) %>%
  select(-BA.ha_n) %>% 
  rename(n = MgC.ha_n) %>% 
  mutate(MgC.ha_lci = MgC.ha_mean - qt(1 - (0.05 / 2), n - 1) * MgC.ha_se,
         MgC.ha_uci = MgC.ha_mean + qt(1 - (0.05 / 2), n - 1) * MgC.ha_se,
         BA.ha_lci = BA.ha_mean - qt(1 - (0.05 / 2), n - 1) * BA.ha_se,
         BA.ha_uci = BA.ha_mean + qt(1 - (0.05 / 2), n - 1) * BA.ha_se)


gg.agwb <- ggplot(agwb.stats, aes(x=date, y=MgC.ha_mean))
limits <- aes(ymax = MgC.ha_uci, ymin = MgC.ha_lci)
gg.agwb + geom_line(size = .5, color = "green4") + geom_ribbon(limits, alpha = 0.5, fill = "green4") +
  theme_light() + labs(x = "Date", y = "Above-Ground Woody Biomass (Mg C / ha)", title = "HF EMS Live Tree Biomass")

gg.agwb <- ggplot(agwb.stats, aes(x=date, y=BA.ha_mean))
limits <- aes(ymax = BA.ha_uci, ymin = BA.ha_lci)
gg.agwb + geom_line(size = .5, color = "olivedrab") + geom_ribbon(limits, alpha = 0.5, fill = "green4") +
  theme_light() + labs(x = "Date", y = "Basal Area (M^2 / ha)", title = "HF EMS Live Tree Basal Area")

# create annual summary of AGWB, AGWI, recruitment, and mortality
# ------------------------------------------------------------------------------------------------------------

agwb.ann.stats <- ungroup(all.dbh.long) %>% 
  filter(!species %in% c("ws", "rp")) %>% 
  #need to create zero values where there are no recruits or dead trees in a plot to get plot average
  complete(year, plot, tree.type) %>% 
  mutate(KgC = ifelse(is.na(KgC), 0, KgC)) %>% 
  group_by(tree.type, year, date, plot) %>%
  summarize_at(vars(KgC), funs(sum), na.rm = T) %>% 
  group_by(tree.type, year, plot) %>% 
  summarize(KgC = max(KgC, na.rm = T)) %>% 
  group_by(tree.type, year) %>% 
  mutate(MgC.ha = (KgC / 1000) / ((10^2 * pi) / 10000)) %>% 
  summarize_at(vars(MgC.ha), funs(mean, se, n())) %>%
  rename("MgC.ha" = "mean") %>% 
  mutate(MgC.ha = ifelse(MgC.ha == 0, NA, MgC.ha), 
         MgC.ha = ifelse(tree.type == "dead", (MgC.ha * -1), MgC.ha),
         se = ifelse(se == 0, NA, se),
         lci = MgC.ha - qt(1 - (0.05 / 2), n - 1) * se,
         uci = MgC.ha + qt(1 - (0.05 / 2), n - 1) * se) %>% 
  gather(key = "variable", value = "val", MgC.ha, se, n, lci, uci) %>% 
  unite(ID, tree.type, variable) %>% 
  spread(ID, val) %>% 
  right_join(full.years) %>% 
  select(year, live_MgC.ha, live_se, live_lci, live_uci, live_n, 
         recruit_MgC.ha, recruit_se, recruit_lci, recruit_uci, recruit_n,
         dead_MgC.ha, dead_se, dead_lci, dead_uci, dead_n)

#check to make sure that the values with plot mean and CI are comparable to a site-wide sum
agwb.ann.stats2 <- ungroup(all.dbh.long) %>% 
  filter(!species %in% c("ws", "rp")) %>% 
  group_by(tree.type, year, date, plot) %>%
  summarize_at(vars(KgC), funs(sum), na.rm = T) %>%  
  group_by(tree.type, year, plot) %>% 
  summarize(KgC = max(KgC, na.rm = T)) %>% 
  group_by(tree.type, year) %>% 
  summarize(MgC.ha = sum(KgC / 1000) / (34*(10^2 * pi) / 10000)) %>%
  spread(tree.type, MgC.ha) %>% 
  right_join(full.years)

# site-wide sum, and mean of plots give same MgC.ha
all.equal(agwb.ann.stats$live_MgC.ha[c(1, 6:26)], agwb.ann.stats2$live[c(1, 6:26)]) 

# Fill years with missing data
# ------------------------------------------------------------------------------------------------------------

# Divide recruitment to spread out evenly over previous missing years
agwb.ann.stats$recruit_n <- na.locf(agwb.ann.stats$recruit_n, na.rm = F,  fromLast = T)
agwb.ann.stats$recruit_n[1] <- NA
years <- agwb.ann.stats$year[!is.na(agwb.ann.stats$recruit_MgC.ha)]

for(i in 7:8){
  for (year in years) {
    agwb.ann.stats[agwb.ann.stats$year %in% ((prev.rec.year + 1):year), i] <-
      agwb.ann.stats[agwb.ann.stats$year == year, i]/(year - prev.rec.year)
    prev.rec.year <- year
  }
  prev.rec.year <- 1993
}
# fill in confidence intervals for recruitment
agwb.ann.stats <- agwb.ann.stats %>%
  mutate(recruit_lci = recruit_MgC.ha - qt(1 - (0.05 / 2), n - 1) * recruit_se,
         recruit_uci = recruit_MgC.ha + qt(1 - (0.05 / 2), n - 1) * recruit_se)

# fill dead_MgC.ha with linear interpolation, after making 1993 == 0
agwb.ann.stats[1, 12:15] <- 0
agwb.ann.stats[agwb.ann.stats$year %in% 1994:1998, 12:15] <- 
  agwb.ann.stats[agwb.ann.stats$year == 1998, 12:15]/5
agwb.ann.stats$dead_n <- na.locf(agwb.ann.stats$dead_n, na.rm = F,  fromLast = T)
agwb.ann.stats[1, 12:15] <- NA


# AGWI with dplyr
# ------------------------------------------------------------------------------------------------------------

agwi.ann.stats <- all.dbh.long %>% 
  # increment needs recruits to be included so the following year's increment is added
  filter(!tree.type == "dead",
         !species %in% c("ws", "rp")) %>%
  group_by(plot, tag, year) %>%
  #get max biomass for each tree in each year
  summarize(KgC = max(KgC, na.rm = T)) %>% 
  # growth increment calculated on a tree-by-tree basis, then summed by year
  mutate(KgC.inc = replace(KgC, !is.na(KgC), c(0, diff(na.omit(KgC))))) %>% 
  group_by(plot, year) %>%
  summarize(KgC.inc = sum(KgC.inc, na.rm = T)) %>%  
  mutate(agwi = (KgC.inc / 1000) / ((10^2 * pi) / 10000)) %>% 
  group_by(year) %>% 
  summarize(se = se(agwi),
            agwi_MgC.ha = mean(agwi),
            n = n()) %>%
  mutate(agwi_lci = agwi_MgC.ha - qt(1 - (0.05 / 2), n - 1) * se,
         agwi_uci = agwi_MgC.ha + qt(1 - (0.05 / 2), n - 1) * se) %>% 
  right_join(full.years) %>% 
  select(year, agwi_MgC.ha, agwi_se = se, agwi_lci, agwi_uci, agwi_n = n)

### make sure increment from 1993 to 1998 is only applied to live trees and recruit and mort are not muddled in the interpolation

# since the increment will only apply to trees that are alive in 1998, it cannot be interpolated all the way back to 1993
agwi.ann.stats[agwi.ann.stats$year == 1998, 2:3] <- 
  agwi.ann.stats[agwi.ann.stats$year == 1998, 2:3]/5
# If we assume all trees died at the end of 1997, we can find this live biomass and then interpolate to 1993
agwb.ann.stats$live_MgC.ha[agwb.ann.stats$year == 1997] <- 
  agwb.ann.stats$live_MgC.ha[agwb.ann.stats$year == 1998] - agwi.ann.stats$agwi_MgC.ha[agwi.ann.stats$year == 1998]
agwi.ann.stats[agwi.ann.stats$year %in% 1994:1997, "agwi_MgC.ha"] <- 
  (agwb.ann.stats$live_MgC.ha[agwb.ann.stats$year == 1997] - agwb.ann.stats$live_MgC.ha[agwb.ann.stats$year == 1993]) /4
# approximate standard error by evenly distributing between 1994:1997
agwi.ann.stats[agwi.ann.stats$year %in% 1994:1997, "agwi_se"] <- agwi.ann.stats$agwi_se[agwi.ann.stats$year == 1998] *((4 /5) /4)
agwi.ann.stats <- agwi.ann.stats %>% 
  mutate(agwi_lci = agwi_MgC.ha - qt(1 - (0.05 / 2), n - 1) * agwi_se,
         agwi_uci = agwi_MgC.ha + qt(1 - (0.05 / 2), n - 1) * agwi_se, 
         agwi_n = 34)
agwi.ann.stats[1, 2:6] <- NA # change zero to NA

# fill live_MgC.ha with linear interpolation after filling 1997 biomass with live increment
agwb.ann.stats[, 2:6] <- na.approx(agwb.ann.stats[, 2:6])

# # create agwb.inc to show as net carbon uptake in live trees
# agwb.ann.stats <- agwb.ann.stats %>% 
#   mutate(inc2_MgC.ha = c(0, diff(live_MgC.ha)),
#          net_MgC.ha = pmap_dbl(list(inc2_MgC.ha, recruit_MgC.ha, dead_MgC.ha), sum, na.rm=T))

tree.stats <- right_join(agwi.ann.stats, agwb.ann.stats) %>% 
  mutate(net_MgC.ha = pmap_dbl(list(agwi_MgC.ha, dead_MgC.ha), sum, na.rm=T)) %>% 
  gather(key = "id", value = "value", -year) %>% 
  separate(id, c("group", "var"), sep = "_") %>% 
  spread(var, value, convert = T) %>%
  select(year, group, MgC.ha, se, lci, uci, n) %>% 
  mutate(group = ifelse(group == "dead", "mortality", group),
         group = factor(group, levels = c("live", "agwi","recruit", "mortality", "net")),
         lci = MgC.ha - qt(1 - (0.05 / 2), n - 1) * se,
         uci = MgC.ha + qt(1 - (0.05 / 2), n - 1) * se) %>% 
  arrange(group, year)
# make net inc in 1993 NA
tree.stats$MgC.ha[tree.stats$group =="net" & tree.stats$year  == 1993] <- NA

# for plotting recruits error bars on stacked plot, need to add the mean of live biomass to CI
tree.stats$lci[which(tree.stats$group == "recruit")] <-  apply(cbind(tree.stats$lci[which(tree.stats$group == "recruit")],
                                                                     tree.stats$MgC.ha[which(tree.stats$group == "agwi")]),
                                                               1, sum, na.rm = T)
tree.stats$uci[which(tree.stats$group == "recruit")] <-  apply(cbind(tree.stats$uci[which(tree.stats$group == "recruit")],
                                                                     tree.stats$MgC.ha[which(tree.stats$group == "agwi")]),
                                                               1, sum, na.rm = T)

# sum the components of annual agwb increment to get net change
tmp <- tree.stats %>% 
  filter(group %in% c("agwi", "recruit", "mortality")) %>% 
  group_by(year) %>% 
  summarize(net.inc_Mg.C.ha = sum(MgC.ha, na.rm = T))

plot(tmp, type = "l")

# make new plot of annual carbon balance
gg1 <- tree.stats %>% filter(group %in% c("agwi", "recruit", "mortality")) %>% 
  ggplot(aes(x = year, y = MgC.ha, group = group, color = group, fill = group, ymin = lci, ymax = uci))
cols <- c("mortality" = "red", "recruit" = "blue", "agwi" = "green4")

# with frequentist standard error bars
gg1 + geom_col(alpha = 0.5, position = position_stack(reverse = T)) +
  geom_errorbar(aes(ymin = lci, ymax = uci, group = group, color = group), position = position_dodge2(width = 1, padding = 0)) +
  scale_colour_manual(values = cols) + scale_fill_manual(values = cols) +
  theme_light() +
  labs(title = "HF-EMS Annual Above Ground Woody Carbon Balance") + 
  scale_x_continuous(breaks = seq(1994, 2018 ,6), limits = c(1993,2020)) +
  geom_hline(yintercept = 0) +
  geom_point(data = filter(tree.stats, group == "net"), aes(x = year, y = MgC.ha), inherit.aes = F)

# without error bars
gg1 + geom_col(alpha = 0.5, position = position_stack(reverse = T)) +
  scale_colour_manual(values = cols) + scale_fill_manual(values = cols) +
  theme_light() +
  labs(title = "HF-EMS Annual Above Ground Woody Carbon Balance") + 
  scale_x_continuous(breaks = seq(1994, 2018 ,6), limits = c(1993,2020)) +
  geom_hline(yintercept = 0) + 
  geom_point(data = filter(tree.stats, group == "net"), aes(x = year, y = MgC.ha), inherit.aes = F)



