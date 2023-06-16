# Updated Scripts to visualize the tree biomass data from the median DBH calculations
# requires run.all.new.format.nalocf.19.R

library(tidyverse)
library(lubridate)
library(zoo)
library(boot)
# setwd("C:/Users/tgw475/Desktop/Whitby HF Files/HF_BIOMETRY_WOFSY copy/trees/r.scripts/")

se <- function(x){
  sd(x, na.rm = T)/sqrt(length(na.omit(x)))
}

full.years <- data.frame(year = 1993:2021)

# # view most recent years' trees for QA/QC
# pdf(file = "DBH_by_plot_2021.pdf", paper= "USr", width = 9)
# for(p in unique(all.dbh.long$plot)){
#   print(
#     ggplot(filter(all.dbh.long, year == max(year), plot == p), aes(doy, dbh, color = plottag))+
#     geom_line()+
#     # geom_label(aes(label = plottag), position = position_dodge(0.5))+
#     theme_bw()
#   )
# }
# dev.off()

# test ID column of recruit and dead
# select "rcrt" & "live" category by tree id & look at min and max and look at order of dates
rcrt.chk <- all.dbh.long %>% 
  filter(tree.type != "dead") %>% 
  group_by(plottag) %>% 
  slice(which.min(date)) %>% 
  filter(year > 1993 & tree.type == "live")

dead.chk <- all.dbh.long %>% 
  filter(tree.type != "recruit") %>% 
  group_by(plottag) %>% 
  slice(which.max(date)) %>% 
  filter(year < 2020 & tree.type == "live")

dead.chk$plottag
# need to add these to respective years tow.yy.mort.txt
# [1] "C4-475" "E4-858" "G5-91"  
# once corrected this should be 0 rows

missing.dead <- filter(all.dbh.long, plottag %in% dead.chk$plottag) %>% 
  arrange(plottag, date)

ggplot(missing.dead, aes(date, dbh, color = plottag)) + 
  geom_line() +
  theme_bw()


# recreate total AGWB plot with dplyr ------------------------------------------------------------------------

agwb.stats <- all.dbh.long %>% 
  filter(!tree.type == "dead") %>% 
  group_by(plot, date) %>%
  summarize(across(c(kgc, ba.cm2), ~sum(.x, na.rm = T))) %>%  
  mutate(MgC.ha = (kgc / 1000) / ((10^2 * pi) / 10000),
    ba.cm2.ha = ba.cm2 / ((10^2 * pi) / 10000)) %>% 
  group_by(date) %>% 
  summarize(across(.cols = c(MgC.ha, ba.cm2.ha), 
    .fns = list(mean = ~mean(.x, na.rm =T), se = se, n = ~n()))) %>% 
select(-ba.cm2.ha_n) %>%
  rename(n = MgC.ha_n) %>%
  mutate(MgC.ha_lci = MgC.ha_mean - qt(1 - (0.05 / 2), n - 1) * MgC.ha_se,
    MgC.ha_uci = MgC.ha_mean + qt(1 - (0.05 / 2), n - 1) * MgC.ha_se,
    ba.cm2.ha_lci = ba.cm2.ha_mean - qt(1 - (0.05 / 2), n - 1) * ba.cm2.ha_se,
    ba.cm2.ha_uci = ba.cm2.ha_mean + qt(1 - (0.05 / 2), n - 1) * ba.cm2.ha_se,
    date = as.Date(date))
str(agwb.stats)

gg.agwb <- ggplot(agwb.stats, aes(x=date, y=MgC.ha_mean))
limits <- aes(ymax = MgC.ha_uci, ymin = MgC.ha_lci)
gg.agwb + geom_line(size = .5, color = "green4") + geom_ribbon(limits, alpha = 0.5, fill = "green4") +
  theme_bw() + labs(x = "", y = "Above-Ground Woody Biomass (Mg C/ ha)", title = "EMS Tree Carbon Accumulation") +
  theme(text = element_text(size = 16)) +
  scale_x_date(date_breaks = "4 years", limits = as_date(c("1993-01-01","2021-01-01")), date_labels = "%Y")

gg.agwb <- ggplot(agwb.stats, aes(x=date, y=ba.cm2.ha_mean))
limits <- aes(ymax = ba.cm2.ha_uci, ymin = ba.cm2.ha_lci)
gg.agwb + geom_line(size = .5, color = "olivedrab") + geom_ribbon(limits, alpha = 0.5, fill = "green4") +
  theme_bw() + labs(x = "Date", y = "Plot Average Basal Area (cm^2 / ha)", title = "HF EMS Live Tree Basal Area") +
  theme(text = element_text(size = 16)) +
  scale_x_date(date_breaks = "4 years", limits = as_date(c("1993-01-01","2021-01-01")), date_labels = "%Y")

# create annual summary of AGWB, AGWI, recruitment, and mortality
# ------------------------------------------------------------------------------------------------------------

agwb.ann.stats <- ungroup(all.dbh.long) %>% 
  complete(tree.type, year, plot) %>% 
  # need to create zero values where there are no recruits or dead trees in a plot to get plot average
  mutate(kgc = ifelse(is.na(kgc), 0, kgc)) %>% 
  group_by(tree.type, year, plot, tag) %>%
  slice(which.max(kgc)) %>% #within each tree-year, keep the max kgc (same as apply(x ,1, max))
  group_by(tree.type, year, plot) %>%
  summarize_at(vars(kgc), funs(sum), na.rm = T) %>% # get total kgc in each plot in each date
  mutate(MgC.ha = (kgc / 1000) / (10^2 * pi / 10000)) %>% # convert to MgCha
  group_by(tree.type, year) %>%
  summarize(across(MgC.ha, list(mean = mean, se = se, n = ~n()), .names = "{fn}")) %>% # get plot mean at year date
  rename("MgC.ha" = "mean") %>% 
  mutate(MgC.ha = ifelse(MgC.ha == 0, NA, MgC.ha), 
    MgC.ha = ifelse(tree.type == "dead", (MgC.ha * -1), MgC.ha),
    se = ifelse(se == 0, NA, se),
    lci = MgC.ha - qt(1 - (0.05 / 2), n - 1) * se,
    uci = MgC.ha + qt(1 - (0.05 / 2), n - 1) * se) %>% 
  pivot_longer(c(MgC.ha, se, n, lci, uci), names_to = "variable", values_to = "val") %>% 
  unite(ID, tree.type, variable) %>% 
  pivot_wider(names_from = ID, values_from = val) %>% 
  right_join(full.years) %>% 
  arrange(year) %>% 
  select(year, live_MgC.ha, live_se, live_lci, live_uci, live_n, recruit_MgC.ha, recruit_se, recruit_lci, recruit_uci, recruit_n,
    dead_MgC.ha, dead_se, dead_lci, dead_uci, dead_n)

#check to make sure that the values with plot mean and CI are comparable to a site-wide sum of individual trees
agwb.ann.stats2 <- ungroup(all.dbh.long) %>% 
  group_by(tree.type, plot, tag, year, date) %>% 
  mutate(MgC.ha = (kgc / 1000) / (34*(10^2 * pi) / 10000)) %>% # convert to MgCha
  group_by(tree.type, year, date) %>% 
  summarize(MgC.ha = sum(MgC.ha)) %>%
  group_by(tree.type, year) %>% 
  summarize(MgC.ha = max(MgC.ha)) %>%
  mutate(MgC.ha = ifelse(MgC.ha == 0, NA, MgC.ha), 
    MgC.ha = ifelse(tree.type == "dead", (MgC.ha * -1), MgC.ha)) %>% 
  spread(tree.type, MgC.ha) %>% 
  right_join(full.years) %>% 
  arrange(year)

# site-wide sum, and mean of plots give effectively same MgC.ha

plot(agwb.ann.stats$year[c(1, 6:29)], (agwb.ann.stats$live_MgC.ha[c(1, 6:29)] - agwb.ann.stats2$live[c(1, 6:29)]), 
  xlab = "year", ylab = "Plot-level Mean AGWB - Sitewide Mean AGWB")
plot(agwb.ann.stats$year[c(1, 6:29)], agwb.ann.stats$live_MgC.ha[c(1, 6:29)])
lines(agwb.ann.stats$year[c(1, 6:29)], agwb.ann.stats2$live[c(1, 6:29)])
lines(names(AGWB), AGWB, col = "red", lty = 2)
# repeat for dead trees
plot(agwb.ann.stats$year[c(1, 6:29)], agwb.ann.stats$dead_MgC.ha[c(1, 6:29)])
lines(agwb.ann.stats$year[c(1, 6:29)], agwb.ann.stats2$dead[c(1, 6:29)])
lines(agwi$Year, agwi$mortality, col = "red", lty = 2)
# repeat for recruit
plot(agwb.ann.stats$year[c(1, 6:29)], agwb.ann.stats$recruit_MgC.ha[c(1, 6:29)], ylim = c(0,.3))
points(agwb.ann.stats$year[c(1, 6:29)], agwb.ann.stats2$recruit[c(1, 6:29)], col = "blue", pch = 2)
lines(agwi$Year, agwi$recruitment, col = "red", lty = 2)


# Fill recruit years with missing data
# ------------------------------------------------------------------------------------------------------------

# Divide recruitment to spread out evenly over previous missing years
agwb.ann.stats$recruit_n <- na.locf(agwb.ann.stats$recruit_n, na.rm = F,  fromLast = T)
agwb.ann.stats$recruit_n[1] <- NA
years <- agwb.ann.stats$year[!is.na(agwb.ann.stats$recruit_MgC.ha)]
prev.rec.year <- 1993

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
  mutate(recruit_lci = recruit_MgC.ha - qt(1 - (0.05 / 2), recruit_n - 1) * recruit_se,
    recruit_uci = recruit_MgC.ha + qt(1 - (0.05 / 2), recruit_n - 1) * recruit_se)

# fill dead_MgC.ha with linear interpolation, after making 1993 == 0
agwb.ann.stats[1, 12:15] <- 0
agwb.ann.stats[agwb.ann.stats$year %in% 1994:1998, 12:15] <- 
  agwb.ann.stats[agwb.ann.stats$year == 1998, 12:15]/5
agwb.ann.stats$dead_n <- na.locf(agwb.ann.stats$dead_n, na.rm = F,  fromLast = T)
agwb.ann.stats[1, 12:16] <- NA

# Create AGWI with dplyr -------------------------------------------------------------------------------------

agwi.ann.stats <- all.dbh.long %>% 
  # increment needs recruits to be included so the following year's increment is added
  filter(!tree.type == "dead") %>% 
  group_by(plot, tag, year) %>%
  #get max biomass for each tree in each year
  summarize(kgc = max(kgc, na.rm = T)) %>% 
  # growth increment calculated on a tree-by-tree basis, then summed by year
  mutate(kgc.inc = replace(kgc, !is.na(kgc), c(0, diff(na.omit(kgc))))) %>% 
  group_by(plot, year) %>%
  summarize(kgc.inc = sum(kgc.inc, na.rm = T)) %>%  
  mutate(agwi = (kgc.inc / 1000) / ((10^2 * pi) / 10000)) %>% 
  group_by(year) %>% 
  summarize(se = se(agwi),
    agwi_MgC.ha = mean(agwi),
    n = n()) %>%
  mutate(agwi_lci = agwi_MgC.ha - qt(1 - (0.05 / 2), n - 1) * se,
    agwi_uci = agwi_MgC.ha + qt(1 - (0.05 / 2), n - 1) * se) %>% 
  right_join(full.years) %>% 
  arrange(year) %>% 
  select(year, agwi_MgC.ha, agwi_se = se, agwi_lci, agwi_uci, agwi_n = n)

agwi.ingrowth <- all.dbh.long %>% 
  # increment needs recruits to be included so the following year's increment is added
  filter(tree.type == "recruit") %>% 
  group_by(plot, tag, species, year) %>%
  #get max biomass for each tree in each year
  summarize(kgc = max(kgc, na.rm = T),
    dbh = max(dbh, na.rm = T)) %>% 
  # growth increment calculated on a tree-by-tree basis, then summed by year
  mutate(kgc.10 = round(calc_kgc(species, 10), 2),
    kgc.ingr = kgc - kgc.10) %>% 
  group_by(plot, year) %>%
  summarize(kgc.ingr = sum(kgc.ingr, na.rm = T)) %>%  
  mutate(agwi.ingr = (kgc.ingr / 1000) / ((10^2 * pi) / 10000)) %>% 
  group_by(year) %>% 
  summarize(se = se(agwi.ingr),
    agwi.ingr_MgC.ha = mean(agwi.ingr),
    n = n()) %>%
  mutate(agwi.ingr_lci = agwi.ingr_MgC.ha - qt(1 - (0.05 / 2), n - 1) * se,
    agwi.ingr_uci = agwi.ingr_MgC.ha + qt(1 - (0.05 / 2), n - 1) * se) %>% 
  right_join(full.years) %>% 
  arrange(year) %>% 
  select(year, agwi.ingr_MgC.ha, agwi.ingr_se = se, agwi.ingr_lci, agwi.ingr_uci, agwi.ingr_n = n)

agwi.ingrowth$agwi.ingr_n <- na.locf(agwi.ingrowth$agwi.ingr_n, na.rm = F,  fromLast = T)
agwi.ingrowth$agwi.ingr_n[1] <- NA
years <- agwi.ingrowth$year[!is.na(agwi.ingrowth$agwi.ingr_MgC.ha)]
prev.rec.year <- 1993

for(i in 2:3){
  for (year in years) {
    agwi.ingrowth[agwi.ingrowth$year %in% ((prev.rec.year + 1):year), i] <- 
      agwi.ingrowth[agwi.ingrowth$year == year, i]/(year - prev.rec.year)
    prev.rec.year <- year
  }
  prev.rec.year <- 1993
}
# fill in confidence intervals for recruitment
agwi.ingrowth <- agwi.ingrowth %>% 
  mutate(agwi.ingr_lci = agwi.ingr_MgC.ha - qt(1 - (0.05 / 2), agwi.ingr_n - 1) * agwi.ingr_se,
    agwi.ingr_uci = agwi.ingr_MgC.ha + qt(1 - (0.05 / 2), agwi.ingr_n - 1) * agwi.ingr_se)



agwi.ann.plot.stats <- all.dbh.long %>% 
  # increment needs recruits to be included so the following year's increment is added
  filter(!tree.type == "dead") %>% 
  group_by(plot, tag, year) %>%
  #get max biomass for each tree in each year
  summarize(kgc = max(kgc, na.rm = T)) %>% 
  # growth increment calculated on a tree-by-tree ba.cm2sis, then summed by year
  mutate(kgc.inc = replace(kgc, !is.na(kgc), c(0, diff(na.omit(kgc))))) %>% 
  group_by(plot, year) %>%
  summarize(kgc.inc = sum(kgc.inc, na.rm = T)) %>%  
  mutate(agwi = (kgc.inc / 1000) / ((10^2 * pi) / 10000)) %>% 
  group_by(year) %>% 
  summarize(se = se(agwi),
    agwi_MgC.ha = mean(agwi),
    n = n()) %>%
  mutate(agwi_lci = agwi_MgC.ha - qt(1 - (0.05 / 2), n - 1) * se,
    agwi_uci = agwi_MgC.ha + qt(1 - (0.05 / 2), n - 1) * se) %>% 
  right_join(full.years) %>% 
  arrange(year) %>% 
  select(year, agwi_MgC.ha, agwi_se = se, agwi_lci, agwi_uci, agwi_n = n)


# Fill mortality years with missing data
# ------------------------------------------------------------------------------------------------------------
### make sure increment from 1993 to 1998 is only applied to live trees and recruit and mort are not muddled in the interpolation

# code example from run.all.new.format.na.locf.19.r
  # mort[as.character(1994:1998)] <- mort["1998"] / 5
  # inc["1998"] <- inc["1998"] / 5
  # inc[as.character(1994:1997)] <- ((AGWB["1998"] - inc["1998"] + mort["1998"] - rec["1998"]) - AGWB["1993"])/4
  # # since dead trees grew some before found in 1998, they contributed some to AGWI in interim, AGWB cannot be linearly interpolated 
  # # factor mort into each to recalculate AGWB and AGWI estimate 
  # for (y in 1997:1994){
  #   AGWB[as.character(y)] <- AGWB[as.character(y + 1)] - rec[as.character(y + 1)] + mort[as.character(y + 1)] - inc[as.character(y + 1)]
  # }

# since the increment will only apply to trees that are alive in 1998, it cannot be interpolated all the way back to 1993
agwi.ann.stats[agwi.ann.stats$year == 1998, 2:3] <- 
  agwi.ann.stats[agwi.ann.stats$year == 1998, 2:3]/5
# If we assume all trees died at the end of 1997, we can find this live biomass and then interpolate to 1993
agwb.ann.stats$live_MgC.ha[agwb.ann.stats$year == 1997] <- 
  agwb.ann.stats$live_MgC.ha[agwb.ann.stats$year == 1998] - agwi.ann.stats$agwi_MgC.ha[agwi.ann.stats$year == 1998]
agwi.ann.stats[agwi.ann.stats$year %in% 1994:1997, "agwi_MgC.ha"] <- 
  (agwb.ann.stats$live_MgC.ha[agwb.ann.stats$year == 1997] - agwb.ann.stats$live_MgC.ha[agwb.ann.stats$year == 1993]) /4
# approximate standard error by evenly distributing between 1994:1997
agwi.ann.stats[agwi.ann.stats$year %in% 1994:1997, "agwi_se"] <- NA
agwi.ann.stats <- agwi.ann.stats %>% 
  mutate(agwi_lci = agwi_MgC.ha - qt(1 - (0.05 / 2), agwi_n - 1) * agwi_se,
    agwi_uci = agwi_MgC.ha + qt(1 - (0.05 / 2), agwi_n - 1) * agwi_se, 
    agwi_n = 34)
agwi.ann.stats[1, 2:6] <- NA # change zero to NA

# fill live_MgC.ha with linear interpolation after filling 1997 biomass with live increment
agwb.ann.stats[, 2:6] <- na.approx(agwb.ann.stats[, 2:6])

# Combine AGWB and AGWI into one dataframe -------------------------------------------------------------------

tree.stats <- right_join(agwi.ann.stats, agwb.ann.stats) %>% 
  left_join(agwi.ingrowth) %>% 
  mutate(net_MgC.ha = pmap_dbl(list(agwi_MgC.ha, recruit_MgC.ha, dead_MgC.ha), sum, na.rm=T)) %>% 
  pivot_longer(-year, names_to = "id", values_to = "value") %>% 
  separate(id, c("group", "var"), sep = "_") %>% 
  pivot_wider(names_from = var, values_from = value) %>%
  select(year, group, MgC.ha, se, lci, uci, n) %>% 
  mutate(group = ifelse(group == "dead", "mortality", group),
    group = factor(group, levels = c("live", "agwi", "recruit", "mortality", "net", "agwi.ingr")),
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

# Calculate annual average for carbon budget
# ------------------------------------------------------------------------------------------------------------ 

tree.budget.ann.avg <- tree.stats %>% 
  filter(year %in% 1998:2021) %>% 
  group_by(group) %>% 
  summarize(MgC.ha_se = se(MgC.ha),
    MgC.ha_mean = mean(MgC.ha),
    n = n()) %>% 
  mutate(MgC.ha_ci = qt(1 - (0.05 / 2), n - 1) * MgC.ha_se) %>% 
  select(group, MgC.ha_mean, MgC.ha_ci, n)

write.table(tree.budget.ann.avg, "../ems.agwb.budget.summary.2021.txt", sep = "\t", quote = F, row.names = F)
write.table(tree.stats, "../tree.stats.98.21.txt", sep = "\t", quote = F, row.names = F )

# Bootsrapped mean annual C balance for CI -------------------------------------------------------------------

samplemean <- function(x, d) {
  return(mean(x[d]))
}

boot.agwi <- boot(agwi.ann.stats$agwi_MgC.ha[agwi.ann.stats$year %in% c(1998:2020)], samplemean, 100)
boot.mort <- boot(agwb.ann.stats$dead_MgC.ha[agwb.ann.stats$year %in% c(1998:2020)], samplemean, 100)
saveRDS(boot.mort$t, "../boot.mort")
boot.rec <- boot(agwb.ann.stats$recruit_MgC.ha[agwb.ann.stats$year %in% c(1998:2020)], samplemean, 100)
boot.df <- data.frame(agwi = boot.agwi$t, mort = boot.mort$t, rec = boot.rec$t, bg.agwi = .2*boot.agwi$t,
  bg.mort = .2*boot.mort$t, bg.rec = .2*boot.rec$t)
boot.df$live.sum <- apply(boot.df, 1, sum)
live.ci <- qt(1-(0.05/2), 100 - 1) * se(boot.df$live.sum)
saveRDS(boot.df, "../boot.live.df")

# make new plots of annual carbon balance --------------------------------------------------------------------

gg1 <- tree.stats %>% filter(group %in% c("agwi", "recruit", "mortality"), year > 1997) %>% 
  ggplot(aes(x = year, y = MgC.ha, ymin = lci, ymax = uci, col = group, fill = group))
cols <- c("mortality" = "red", "recruit" = "blue", "agwi" ="green4", "net" ="black")
fills <- c("mortality" = "red", "recruit" = "blue", "agwi" = "green4", "net" = NA)
shapes <- c("mortality" = NA, "recruit" = NA, "agwi" = NA, "net" = 19)

lm.agwi <- lm(MgC.ha~year, data = filter(tree.stats, group %in% c("agwi"), year > 1997))
summary(lm.agwi)
lm.agwi.09 <- lm(MgC.ha~year, data = filter(tree.stats, group %in% c("agwi"), year > 1997, year < 2010))
summary(lm.agwi.09)


# function for lm results in plots
lm_text <- function(model) {
  paste("y =", signif(model$coef[[1]], 3), " +", signif(model$coef[[2]], 3), "x", "\n",
    " P =", signif(summary(model)$coef[2,4], 3), "Adj R^{2} = ", signif(summary(model)$adj.r.squared, 3))
}  
txt <- lm_text(lm.agwi)

# with frequentist standard error bars
gg1 + geom_col(alpha = 0.5, position = position_stack(reverse = T)) +
  geom_errorbar(aes(ymin = lci, ymax = uci), 
    position = position_dodge2(width = 1, padding = 0), show.legend = F) +
  labs(title = "HF-EMS Annual Above Ground Woody Carbon balance") + 
  scale_x_continuous(breaks = seq(1998, 2020 ,4)) +
  geom_hline(yintercept = 0) +
  geom_point(data = filter(tree.stats, group == "net", year > 1997),
    aes(x = year, y = MgC.ha), inherit.aes = T) +
  scale_colour_manual(values = cols) + scale_fill_manual(values = fills) +
  scale_shape_manual(values = shapes) +
  theme(legend.position = c(0.5, 0.2), legend.background = element_blank(), legend.title = element_blank(),
    panel.background = element_rect(fill="white"), panel.grid = element_line(colour="grey80"),
    text = element_text(size = 16))

# without error bars
cols <- c("mortality" = NA, "recruit" = NA, "agwi" = NA, "net" ="black")
gg1.1 <- tree.stats %>% filter(group %in% c("agwi", "recruit", "mortality"), year > 1997) %>% 
  ggplot(aes(x = year, y = MgC.ha, ymin = lci, ymax = uci))
gg1.1 + geom_col(alpha = 0.5, position = position_stack(reverse = T), aes(fill = group)) +
  labs(x = "", y = "MgC / ha") + 
  scale_x_continuous(breaks = seq(1995, 2020, 5)) +
  geom_hline(yintercept = 0) + 
  # geom_smooth(data = filter(tree.stats, group == "agwi", year > 1997), 
  #   aes(year, MgC.ha), method = "lm", formula = y~x, inherit.aes = F, color = "green4", se = F )+
  geom_point(data = filter(tree.stats, group == "net", year > 1997),
    aes(x = year, y = MgC.ha, color = "net"), inherit.aes = F) +
  scale_colour_manual(values = c("net" = "black")) + scale_fill_manual(values = fills) +
  theme_bw()+
  theme(legend.background = element_blank(), legend.title = element_blank(),
    legend.position = c(.75,.25), text = element_text(size = 18))+
  labs(title = "HF-EMS Above Ground Woody Carbon Balance") + 
  geom_hline(yintercept = 0) #+
  # annotate("text", label = txt, x = 2003, y = -4.3)

txt <- lm_text(lm.agwi.09)

# without error bars < 2010
cols <- c("mortality" = NA, "recruit" = NA, "agwi" = NA, "net" ="black")
gg1.2 <- tree.stats %>% filter(group %in% c("agwi", "recruit", "mortality"), year > 1997, year < 2010) %>% 
  ggplot(aes(x = year, y = MgC.ha, ymin = lci, ymax = uci))
gg1.2 + geom_col(alpha = 0.5, position = position_stack(reverse = T), aes(fill = group)) +
  labs(title = "Annual above-ground woody carbon balance", x = "", y = "MgC / ha") + 
  scale_x_continuous(breaks = seq(1998, 2020, 4)) +
  geom_hline(yintercept = 0) + 
  geom_smooth(data = filter(tree.stats, group == "agwi", year > 1997, year < 2010), 
    aes(year, MgC.ha), method = "lm", formula = y~x, inherit.aes = F, color = "green4", se = F ) +
  geom_point(data = filter(tree.stats, group == "net", year > 1997, year < 2010),
    aes(x = year, y = MgC.ha, color = "net"), inherit.aes = F) +
  scale_colour_manual(values = c("net" = "black")) + scale_fill_manual(values = fills) +
  theme_bw()+
  theme(legend.background = element_blank(), legend.title = element_blank(),
    legend.position = c(.75,.25), text = element_text(size = 16))+
  annotate("text", label = txt, x = 2004, y = -1.5)


# plot just AGWI
gg1.3 <- tree.stats %>% filter(group == "agwi", year > 1997) %>% 
  ggplot(aes(x = year, y = MgC.ha, fill = group, ymin = lci, ymax = uci))
# with frequentist standard error bars
gg1.3 + geom_col(alpha = 0.6, position = position_stack(reverse = T), color = "green4", fill = "green4") +
  geom_errorbar(aes(ymin = lci, ymax = uci), 
    position = position_dodge2(width = 1, padding = 0), show.legend = F) +
  geom_smooth(data = filter(tree.stats, group == "agwi", year > 1997, year < 2010), 
    aes(year, MgC.ha), method = "lm", formula = y~x, inherit.aes = F, color = "green4", se = F )+
  geom_smooth(data = filter(tree.stats, group == "agwi", year > 1997), 
    aes(year, MgC.ha), method = "lm", formula = y~x, inherit.aes = F, color = "black", se = F )+
  # labs(title = "HF-EMS Annual Above Ground Woody Increment") + 
  scale_x_continuous(breaks = seq(1998, 2021 ,4)) +
  geom_hline(yintercept = 0) +
  theme_bw()+
  # scale_colour_manual(values = cols) + scale_fill_manual(values = cols) +
  theme(legend.background = element_blank(), legend.title = element_blank())


# Use ggplot on old data ------------------------------------------

# tree.sum.long <- agwi %>% group_by(Year) %>% 
#   select(Year, AGWI, recruitment, mortality) %>% 
#   mutate(net = AGWI + recruitment + mortality) %>% 
#   gather(key = "group", value = "MgC.ha", AGWI, recruitment, mortality, net) %>% 
#   mutate(group = factor(group, levels = c( "AGWI", "recruitment", "mortality", "net"))) %>% 
#   ungroup()
# cols2 <- c("mortality" = "red", "recruitment" = "blue", "AGWI" = "green4")
# gg2 <- tree.sum.long %>% filter(group %in% c("AGWI", "recruitment", "mortality"), Year > 1997) %>% 
#   ggplot(aes(x = Year, y = MgC.ha, group = group, color = group, fill = group))
# gg2 + geom_col(alpha = 0.5, position = position_stack(reverse = T)) +
#   scale_colour_manual(values = cols2) + scale_fill_manual(values = cols2) +
#   theme_bw() +
#   theme(legend.background = element_blank(), legend.title = element_blank(),
#     legend.position = c(.75,.25), text = element_text(size = 16))+
#   labs(title = "HF-EMS Annual Above Ground Woody Carbon balance") + 
#   scale_x_continuous(breaks = seq(1998, 2020 ,4)) +
#   geom_hline(yintercept = 0) + 
#   geom_point(data = filter(tree.sum.long, group == "net", Year > 1997), 
#     aes(x = Year, y = MgC.ha), inherit.aes = F, color = "black") 

#idendtical except that AGWI in 1994:1997 and is slightly lower in dplyr since AGWB was is a plot-level mean, not a site-wide sum

