# canopy height data summary from 2018
# need to get domincance category for trees
library(readxl)
library(tidyverse)
reu18comp <-read_xlsx("C:/Users/tgw475/Desktop/Whitby HF Files/REU Mentor/2018/REU2018_Tree_Mortality/Data Entry/EMS2018 Plot Mortality Data.xlsx", na = "NA")
tree.hts <- reu18comp %>% select(plot.f, plot.c, tag.c, spp.c, DBH.c, BA.c, height.c) %>% 
  mutate_if(is.numeric, round, digits = 1) %>% 
  mutate(spp.c = tolower(spp.c),
    spp.c = recode(spp.c, "be" = "beech", "bc" = "cherry"),
    # plot.c = ifelse(is.na(plot.c), plot.f, plot.c),
    plot.tag = paste(plot.c, tag.c, sep = "-")) %>% 
  filter(!is.na(spp.c), !duplicated(plot.tag), as.numeric(tag.c) < 1000)

tree.hts[, 5:7] <- apply(tree.hts[,5:7], 2, round, digits = 1)

tree.ht.plot.sum <- group_by(tree.hts, plot.c) %>% 
  summarize(ht.mean = mean(height.c),
    ht.md = median(height.c),
    ht.sd = sd(height.c),
    ht.ci = sd(height.c)/sqrt(n()),
    n = n())

ggplot(tree.hts, aes(x = plot.c, y = height.c, group = plot.c)) +
  geom_boxplot(aes(fill = plot.c), show.legend = F) + 
  geom_point(aes(fill = NULL), shape = 1, show.legend = F) +
  ylim(0,35) +
  labs(title = "EMS Canopy Heights from 2018 competition index study",
    x = "plot", y = "canopy height (m)")+
  theme_bw()
  

tree.ht.mean <- ungroup(tree.hts) %>% 
  summarize(ht.mean = mean(height.c),
    ht.sd = sd(height.c),
    ht.n = n())

tree.ht.mean

write.csv(tree.hts[-1], file = "trees/canopy_heights_18.csv", row.names = F)
tree.hts.dom <- read.csv(file = "trees/canopy_heights_dom_18.csv", header = T) %>% 
  arrange(plot.tag)

ggplot(filter(tree.hts.dom, dominance != "suppressed"), aes(x = "", y =height.c)) +
  geom_boxplot(show.legend = F, fill = "blue", alpha = 0.5) + 
  geom_point(aes(fill = NULL), shape = 1, show.legend = F, position = "jitter") +
  # ylim(0,35) +
  labs(title = "EMS Canopy Heights from 2018 competition index study \nno suppressed trees",
     x = "", y = "canopy height (m)")+
  theme_bw()


tree.canopy.mean <- ungroup(tree.hts.dom) %>% 
  filter(dominance != "suppressed") %>% 
  summarize(ht.mean = mean(height.c),
    ht.sd = sd(height.c),
    ht.n = n())

tree.canopy.mean
