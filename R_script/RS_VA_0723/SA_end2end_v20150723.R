##########
##########
# 20150723
# Input - Output
##########
##########

# call packages
library(dplyr)


library(ggplot2)
library(gridExtra)
library(knitr)
##########
# load data
# !!! to be replaced by: csv -> data.frame  
load("VIP2.RData")

# get history for: CC,FF,NP,PC
# from 2015-1-1 to 2015-5-28
trx <- tbl_df(VIP2 %>% 
  filter(ID %in% c("CC", "FF", "NP", "PC")) %>%
  filter(Date > as.Date("2014-12-31") & Date < as.Date("2015-5-29")))

trx <- trx[, -c(9:13)]

# viz for understanding
# to be removed
# trx %>% group_by(ID) %>% summarise( Dmin = min(Date), Dmax = max(Date), n = n())
# ggplot(trx) + geom_point(aes(Date, TimeSor)) + facet_wrap(~ID)

# construct train & test set
train <- trx %>% filter(Date < as.Date("2015-5-1"))
test <- trx %>% filter(Date >= as.Date("2015-5-1"))

rm(centers1, centers2, cl1, cl2, gg1, gg2, within.ss, t.kmeans)


ID.list <- trx %>% group_by(ID) %>% summarise()

train_decompose <- decompose(trx)  
head(train_decompose)
ggplot(train_decompose) +geom_point(aes(Date, TimeSor)) + facet_wrap(~ID) + xlim(as.Date("2015-1-1"), as.Date("2015-5-28"))
ggplot(train) +geom_point(aes(Date, TimeSor)) + facet_wrap(~ID) + xlim(as.Date("2015-1-1"), as.Date("2015-5-28"))
  
  
  
