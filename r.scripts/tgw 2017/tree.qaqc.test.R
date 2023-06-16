library(tidyverse)
library(stringr)


setwd(dir="C:/Users/tgw475/Desktop/Whitby HF Files/HF_BIOMETRY_WOFSY copy/trees/")

years <- list.files(path = "C:/Users/tgw475/Desktop/Whitby HF Files/HF_BIOMETRY_WOFSY copy/trees/data/dendrometer", recursive = F)
dirs <- list.dirs(path = "C:/Users/tgw475/Desktop/Whitby HF Files/HF_BIOMETRY_WOFSY copy/trees/data/dendrometer", recursive = F)
names <- as.character("NA")

for (i in 1:length(dirs)){
  f.dir <- paste(dirs[i], "/", list.files(path = dirs[i], pattern="tow.\\d{2}.ref.txt"), sep = "")
  if(file.exists(f.dir)){
    tmp <- read.table(file = f.dir, header = T, row.names = NULL, as.is = T, sep = "\t")
    # clean up colnames for each ...ref.txt so each year is consistent and each colname ends in ".ddd" for making jday column
    jdays <- as.integer(unique(na.omit(str_extract(colnames(tmp), "\\d+$"))))
    colnames(tmp) <- sub(pattern = "\\.tape", replacement = "", x= colnames(tmp))
    colnames(tmp) <- sub(pattern = "dbh$", replacement = paste("dbh.", min(jdays), sep=""), x= colnames(tmp))
    colnames(tmp) <- sub(pattern = "dend$", replacement = paste("dend.", min(jdays), sep=""), x= colnames(tmp))
    colnames(tmp) <- sub(pattern = "ref\\.", replacement = "ref", x= colnames(tmp))
    colnames(tmp) <- sub(pattern = "jday", replacement = "dend", x= colnames(tmp))
    colnames(tmp) <- sub(pattern = "doy", replacement = "dend", x= colnames(tmp))
    tmp2 <- tmp %>% gather("obs", "meas", -plot,-tag,-spp, na.rm=T) %>%
      separate(obs, c("var", "jday"), sep = "\\.") %>%
      spread(var, meas) %>% group_by(plot, tag) 
    tmp2$jday <- as.numeric(tmp2$jday)
    tmp2$year <- as.numeric(years[i])
    tmp2$date <- strptime(paste(tmp2$year, tmp2$jday, sep = ""), format = "%Y%j", tz = "EST")
    tmp2$date <- as.character(tmp2$date) #grouped df soes not support POSIXlt format
    tmp2 <- arrange(tmp2, plot, tag, jday)
    yr <- years[i]
    name <- paste("tow", yr,".df", sep = "")
    names[i] <- name
    assign(name, tmp2)
  }
}

# create long dataset
badplots = c("A4", "A5", "B5","G3", "H3", "H4")
towall.df <- rbind(tow1998.df, tow1999.df, tow2000.df, tow2001.df, tow2002.df, tow2003.df, tow2004.df, tow2005.df, tow2006.df,
                    tow2007.df, tow2008.df, tow2009.df, tow2010.df, tow2011.df, tow2012.df, tow2013.df, tow2014.df, tow2015.df, tow2016.df)
towall.df <- towall.df %>% filter(!plot %in% badplots) %>% group_by(plot, tag, year) %>% arrange(year, jday, plot, tag)
# reorder columns
towall.df <- select(towall.df, plot, tag, spp, date, year, jday, dend, refdend, refdbh)
# adjust refdbh and refdend for 2001.126 to actual collection date of 2000.278
for(i in 1:nrow(towall.df)){
  if(towall.df$date[i] == "2001-05-06"){
    p <- towall.df$plot[i]
    t <- towall.df$tag[i]
    towall.df$refdend[towall.df$date == "2000-10-04" & towall.df$plot == p & towall.df$tag == t] <- towall.df$refdend[i]
    towall.df$refdbh[towall.df$date == "2000-10-04" & towall.df$plot == p & towall.df$tag == t] <- towall.df$refdbh[i]
    towall.df$refdend[i] <- NA
    towall.df$refdbh[i] <- NA
  }
}

# ------------------------------------------------------------------------------------------------------------
# clean data such that "refdbh" and "refdend" values are only times when a new DBH tape measurement was taken,
# not when they were calculated from the final measurement of the previous year
 
# create a logical column to indicate if the "refdbh" at the first measure of the year 
# is calculated from the previous year's final measurement or a new measurement
first <- towall.df %>% filter(!year == 1998, jday == min(jday))
last <- towall.df %>% filter(!year %in% c(2016, 2000), jday == max(jday))
last$year <- last$year + 1
first.last <- left_join(first, last, by = c("plot", "tag", "year"))
first.last$refcheck <- F
for(i in 1:nrow(first.last)){
  if(first.last$year[i] %in% c(2001:2003, 2006)){
    first.last$refcheck[i] <- F
  } else if (is.na(first.last$dend.x[i])| is.na(first.last$refdend.x[i]) | is.na(first.last$dend.y[i]) == T){
    first.last$refcheck[i] <- F
  # } else if(first.last$dend.x[i] == first.last$refdend.x[i]){
  #    first.last$refcheck[i] <- F
  } else if (first.last$dend.y[i] == first.last$refdend.x[i] & first.last$dend.x[i] != 0){
    first.last$refcheck[i] <- T
  } else {
    first.last$refcheck[i] <- F
  }
}

gg.TF <- ggplot(first.last, aes(x=year, fill = refcheck))
gg.TF + geom_histogram(bins = length(unique(first.last$year))) + 
labs(caption = "False = when refdend & refdbh are new measurements \n 
     True = when refdend and refdbh are calculated from last year's final value")

# join the refcheck column to tow.all, and remove extrapolated refdend and refdbh values
towall.df.chk <- towall.df %>% 
  left_join(select(first.last, plot, year, jday = jday.x, refcheck), by = c("plot", "tag", "year", "jday"))
# replace NA's in refcheck with FALSE
towall.df.chk<- mutate(towall.df.chk, refcheck = if_else(is.na(refcheck), F, refcheck))
# 1998 refdend and refdbh were taken up to 1 month apart, but are still included given the low resolution of DBH measurements
for (i in 1:nrow(towall.df.chk)){
  if(towall.df.chk$year[i] == 1998 & towall.df$jday[i] == 146){
    towall.df.chk$refcheck[i] <- T
  }
}
# replace zero calues for dend and refdend with NA
towall.df.chk <- mutate(towall.df.chk, dend = ifelse(dend == 0, NA, dend))
towall.df.chk <- mutate(towall.df.chk, refdend = ifelse(refdend == 0, NA, refdend))

# remove "TRUE" refcheck, aka calculated, values of refdbh and refdend
for (i in 1:nrow(towall.df.chk)){
  if (towall.df.chk$refcheck[i] == T){
    towall.df.chk$refdend[i] <- NA
    towall.df.chk$refdbh[i] <- NA
  }
}

dend.data<-read.table("data/ems.dend.all.txt", stringsAsFactors=F,header=T) %>% 
  mutate(dend.meas = ifelse(dend.meas == 0, NA, dend.meas))
dbh.data<-read.table("data/ems.dbh.all.txt", stringsAsFactors=,header=T) %>% 
  filter(!(year == 1998 & jday == 100))
spp.data<-read.table("data/ems.spp.all.txt", stringsAsFactors=F,header=T)
#adjust jdays to match nearest following date where dendrobands were read, to facilitate joining datasets

dbh.data <- dbh.data %>% ungroup() %>% 
  mutate(jday = ifelse(jday == 119 & year == 2003, 117, jday))
dbh.data <- mutate(dbh.data, jday = ifelse(jday == 104 & year == 2000, 94, jday)) 
dbh.data <- dbh.data[!duplicated(dbh.data),]
dbh.data <- filter(dbh.data, !(year == 2001 & jday == 126))

towall.df.join <- full_join(towall.df.chk, dbh.data, by = c("year", "jday", "plot", "tag"))
towall.df.join <- full_join(towall.df.join, dend.data, by = c("year", "jday", "plot", "tag")) %>% 
  group_by(plot, tag, year, jday) %>% 
  arrange(plot, tag, year, jday) %>% 
  mutate(rep = max(n()))
  
#Exception # 1 -------------------------------------------------------------------------------
#For trees with two bands simultaneously in 2012 and on, make old format match new format
for(i in which(towall.df.join$rep > 1)){
  if(towall.df.join$year[i] > 2012){
    if(is.na(towall.df.join$refdend[i]) & is.na(towall.df.join$refdbh[i]) & is.na(towall.df.join$DBH[i])){
        towall.df.join$dend[i] <- towall.df.join$dend.meas[i]
    }
  }
}

# Exception # 2 -------------------------------------------------------------------------------------------
# Accept new "DBH" as "refdbh" in years after 2012, where dendrometer measurements agree, and refdend and refdbh were empty
# because these are by default in the new format made by ELG
towall.df.join <- mutate(towall.df.join, refdbh = ifelse(year > 2012,
                                                         ifelse(dend == dend.meas, 
                                                                 ifelse(is.na(refdbh),
                                                                        ifelse(is.na(refdend), DBH,refdbh),refdbh),refdbh),refdbh))
towall.df.join <- ungroup(towall.df.join) %>% mutate(id = rownames(towall.df.join)) %>% select(15, 1:14)

# Manual review done with paper datasheets and any changes recorded in .csv files in excel
# first file created had over 5000 entires before exceptions #1 and #2
meas.mismatch.old <- read.csv( file = "old.new.dbh.mismatch.csv")


# Create new dataframes for instances that differ between new and old format
dbh.mismatch <- filter(towall.df.join, DBH != refdbh | !is.na(DBH) & is.na(refdbh) | !is.na(refdbh) & is.na(DBH)) %>% 
  group_by(plot, tag, year, jday, dend.num)
dend.mismatch <- filter(towall.df.join, dend != dend.meas | !is.na(dend) & is.na(dend.meas) | !is.na(dend.meas) & is.na(dend)) %>% 
  group_by(plot, tag, year, jday, dend.num)
meas.mismatch <- full_join(dbh.mismatch, dend.mismatch) %>% 
  arrange(plot, tag,year, jday, dend.num)
write.csv(meas.mismatch, file = "old.new.dbh.mismatch2.csv")


# reconcile versions after exceptions #1 and #2 (see above) were implemented.
meas.mismatch3 <- read.csv(file="old.new.dbh.mismatch3.csv")
meas.mismatch.join <- left_join(meas.mismatch, meas.mismatch3) %>% 
  group_by(plot, tag,year, jday, dend.num) %>% 
  arrange(plot, tag, year, jday, dend.num) 
write.csv(meas.mismatch.join, file = "mismatch.join.csv")

# After manually incorporating edits informed by "mismatch.join" file, read in clean datasets and merge
dend.data.2<-read.csv("ems.dend.all.tgw.csv", stringsAsFactors=F, header=T) %>% 
  mutate(dend.meas = ifelse(dend.meas == 0, NA, dend.meas))
dbh.data.2<- read.csv("ems.dbh.all.tgw.csv", stringsAsFactors=F, header=T) %>% 
  mutate(jday= ifelse((year == 1998 & jday == 100), 146, jday))
dend.dbh.join<- full_join(dend.data.2, dbh.data.2,  by = c("year", "jday", "plot", "tag")) %>% 
  group_by(plot, tag, year, jday) %>% 
  arrange(plot, tag, year, jday) %>% 
  left_join(spp.data) %>% 
  select(year, jday, plot, tag, spp, dend.num, dend.meas, DBH)
write.csv(dend.dbh.join, file = "ems.dend.dbh.all.csv", row.names = F)
