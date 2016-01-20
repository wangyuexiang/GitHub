library(dplyr)
library(ggplot2)
library(zoo)

###
# 20150813: extract data in 2014
trx.2014 <- trx.5000 %>% filter(Year == 2014)

trx.2014 <- trx.2014 %>%
  mutate(
    D = substr(SHoro, 1, 10),
    HS = substr(SHoro, 12, 16),
    HE = substr(EHoro, 12, 16)) %>%
  select(ID, Label, Entr, Sor, D, HE, HS)
  
trx.2014 <- trx.2014 %>%
  mutate(  
    Date = as.Date(D),
    HH = substr(HE, 1, 2), MM = substr(HE, 4,5),
    TimeEntr = as.numeric(HH) + as.numeric(MM) / 60) %>%
  select(ID, Label, Entr, Sor, Date, TimeEntr, HS)
    
trx.2014 <- trx.2014 %>%
  mutate(  
    DOW = as.POSIXlt(Date)$wday,
    WOY = as.numeric(format(Date+3, "%U")),
    HH = substr(HS, 1, 2), MM = substr(HS, 4,5),
    TimeSor = as.numeric(HH) + as.numeric(MM) / 60) %>%
  select(ID, Label, Entr, Sor, Date, DOW, WOY, TimeEntr, TimeSor)    

trx.2014[trx.2014$Entr== "00000000", ]$TimeEntr <- 0    
trx.2014$Voie <- 0    

save(trx.2014, file = "trx.2014.RData")    




trx <- trx.5000
##########
### prepare trx
##########
trx <- trx %>%
  mutate(
    D = substr(SHoro, 1, 10),
    HS = substr(SHoro, 12, 16),
    HE = substr(EHoro, 12, 16)) %>%
  select(ID, Label, Entr, Sor, D, HE, HS)

trx <- trx %>%
  mutate(  
    Date = as.Date(D),
    HH = substr(HE, 1, 2), MM = substr(HE, 4,5),
    TimeEntr = as.numeric(HH) + as.numeric(MM) / 60) %>%
  select(ID, Label, Entr, Sor, Date, TimeEntr, HS)

trx <- trx %>%
  mutate(  
    DOW = as.POSIXlt(Date)$wday,
    WOY = as.numeric(format(Date+3, "%U")),
    HH = substr(HS, 1, 2), MM = substr(HS, 4,5),
    TimeSor = as.numeric(HH) + as.numeric(MM) / 60) %>%
  select(ID, Label, Entr, Sor, Date, DOW, WOY, TimeEntr, TimeSor)    

trx[trx$Entr== "00000000", ]$TimeEntr <- 0    
trx$Voie <- 0    

##########
### create trx.ID
##########
trx.ID <- trx %>%
  group_by(Label) %>%
  summarise(Dmin = min(Date),
            Dmax = max(Date),
            noPsg = n())

trx.ID <- trx.ID %>% arrange(Dmax)
trx.ID$ord <- 1
for(i in 2:nrow(trx.ID)) trx.ID$ord[i] <- i
trx.ID <- trx.ID %>% arrange(Label)

##########
### viw
##########
ggplot(trx.ID) + geom_bar(aes(noPsg), binwidth = 100)
ggplot(trx.ID) + geom_segment(aes(x=Dmin, xend=Dmax, y=ord, yend=ord)) 
  
ggplot(trx.ID %>% filter(Dmin > as.Date("2011/1/20"))) + 
  geom_bar(aes(Dmin), binwidth = 30) 
