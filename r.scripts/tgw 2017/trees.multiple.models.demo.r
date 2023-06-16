# will use dplyr for piping function and data wrangling,
# tidyr to nest and unnest dataframes,
# and purr packages to apply functions within nested data frames (similar to apply family of functions)
# all of these are within the tidyverse umbrella
library(tidyverse)
library(lubridate)
# broom is responsible for simpliflying and extracting info the nested data frame that contains the tree-specific models
library(broom)


setwd("C:/Users/tgw475/Desktop/Whitby HF Files/HF_BIOMETRY_WOFSY copy/trees")

tree.data <- read.csv("ems.all.dbh.long.csv") %>% 
  # date was read in as a factor, change to lubriate compatible date object
  mutate(date = as_date(date)) %>% 
  # 1993 was before dendrometers were installed, we should remove it as it messes up linear trends
  filter(!year == 1993)

# make a nested dataframe, so each row is a unique tree, and its remaining data is a list in column 2
tree.data.nest <- tree.data %>% group_by(plottag) %>% 
  # need at least 3 points to fit line, or model will throw an error
  filter(n() >= 3) %>% 
  # create relative date to serve as x axis for model
  mutate(rel.date = decimal_date(date) - decimal_date(min(date))) %>% 
  # nest will condense all data that isn't a grouping vaiable to a list column called "data"
  nest()

# view one element in nested data frame, str(tree.data.nest) gives messy output
tree.data.nest$data[[1]]

# create a function that can be applied to the "data" column using purr:map
tree_mod1 <- function(df){
  return(lm(dbh ~ rel.date, data = df, na.action = na.omit))
}

# apply model, and extract informative parameters from model using broom::tidy, glance, augment
tree.data.fit <- tree.data.nest %>%
  mutate(mod = map(data, tree_mod1),
         tidy = map(mod, tidy),
         glance = map(mod, glance),
         augment = map(mod, augment),
         rsq = map_dbl(glance, "r.squared") #map_dbl is just a specific instance that returns a number instead of a list
  )

# create tidy dataframe of the model parameters
tree.data.tidy <- tree.data.fit %>% unnest(tidy) %>%
  # keep relevant columns only 
  select(plottag, rsq, term, estimate) %>%
  # term is a grouping variable that has both slope and intercept in it, separate this into two columns
  spread(term, estimate) %>%
  # give better names 
  rename(slope = `rel.date`, int = `(Intercept)`) %>%
  # optional:  merge back to original dataframe: left_join will find the matching column in DF #1 and append all the matching data from DF #2
  # left_join(tree.data.fit[ , 1:2]) %>%
  unnest()

rsq.date.hist <- tree.data.tidy %>% ggplot(aes(x=rsq))
rsq.date.hist + geom_histogram(binwidth = 0.025) +
  xlab("Tree-specific Linear Model R^2")


slope.date.hist <- tree.data.tidy %>% ggplot(aes(x=slope, colour = slope))
slope.date.hist + geom_histogram(binwidth = 0.05) +
  xlab("Tree-specific Linear Model Slope")

# we should be able to do this on the live and dead subsets separately with more complex models
# we may get clearer results if we only use the max value in each year instead of every observation
# hopefully then we could see differences between live and dead in how well different models fit, 
# or compare the values of the parameter coefficients within a given model type
