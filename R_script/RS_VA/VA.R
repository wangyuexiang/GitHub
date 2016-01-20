library(dplyr)
library(ggplot2)
library(knitr)

# load("duree.RData")
# load("Esc2.RData")
# load("VIP.RData")
# rm(ASF, Escotis2, Esc)

gare <- read.table("garesLatLng.csv", sep =",", header = T)
geo <- gare[, c("Cde", "Lng", "Lat")]

VIP <- ASF[, c("Nom", "Entr", "Sor", "Date", "DOW", "WOY","ETime", "STime")]
names(VIP)[1] <- "ID"

VIP[is.na(VIP)] <- 0
VIP[VIP$ETime == 0, ]$ETime <- VIP[VIP$ETime == 0, ]$STime - .5
names(VIP)[7:8] <- c("TimeEntr", "TimeSor")

Esc2 <- Esc[ c("ID", "Entr", "Sor", "Date", "DOW", "WOY", "TimeEntr", "TimeSor")]

VIP <- tbl_df(VIP)
Esc2 <- tbl_df(Esc2)
duree1 <- VIP %>% group_by(ID) %>% summarise( Dmin = min(Date), Dmax = max(Date), dif = Dmax - Dmin)
duree2 <- Esc2 %>% group_by(ID) %>% summarise( Dmin = min(Date), Dmax = max(Date), dif = Dmax - Dmin)
duree.double <- rbind(duree1, duree2)
ggplot(duree) + geom_segment(aes(x=Dmin, xend= Dmax, y = ID, yend = ID, size = 3, col = ID))
                             
VIP$Source <-"ASF"
Esc2$Source <-"ESCOTAS"

VIP2 <- rbind(VIP, Esc2)

names(geo) <- c("Entr", "Elng", "Elat")
VIP2 <- merge(x=VIP2, y=geo, all.x=T)
names(geo) <- c("Sor", "Slng", "Slat")
VIP2 <- merge(x=VIP2, y=geo, all.x=T)


duree.double$Source <- "ASF"
duree.double$Source[13:17] <- "ESCOTA"


##########
##########
load("ID_OD.RData")
ID_OD <- ID_OD[, c("Nom", "Entr", "Sor", "Date", "DOW", "WOY", "Time", "Elng", "Elat", "Slng", "Slat")]


##########
##########
result <- read.table("result.csv", header = T, sep =";")
NPFF <- VIP2 %>% filter(ID == "NP" | ID =="FF")
NPFF$result <- 0

for (i in 1:nrow(result)){
  NPFF[NPFF$ID     == result$Nom[i] &
         NPFF$Entr == result$Entr[i]&
         NPFF$Sor  == result$Sor[i]&
         NPFF$DOW  == result$DOW[i]&
         NPFF$TimeSor >= result$Tmin[i]&
         NPFF$TimeSor <= result$Tmax[i],
         "result"]<-1
}

ggplot(NPFF) +
  geom_point(aes(TimeEntr, Elat, col = "Elat", shape = "Elat", alpha = .1)) +
  geom_point(aes(TimeSor, Slat, col = "Slat", shape = "Slat", alpha = .1)) +
  geom_segment(aes(x=TimeEntr, xend=TimeSor, y=Elat, yend=Slat, alpha =.3, col = as.factor(result))) +
  facet_wrap(~ID) + ylab("Lat") + ggtitle("Time~Lat")


# temp <- NPFF[, c("ID", "Entr", "Sor", "Date", "DOW", "WOY", "TimeEntr", "TimeSor")]
# write.table(temp, file='NPFF.csv',row.names=FALSE,quote=FALSE,sep=";")
##########
##########
##########
##########


ggplot(VIP2 %>% filter(ID == "JA")) + geom_point(aes(Date, TimeSor)) +facet_wrap(~Source)
ggplot(VIP2 %>% filter(ID == "JA")) + geom_point(aes(Date, TimeSor, col = Source, alpha = .5))
temp <- VIP2 %>% filter(ID == "JA")
temp %>% group_by(Source) %>% summarise(Dmin = min(Date), Dmax = max(Date))

temp1 <- temp
temp1$S<-substr(temp1$Sor,4,5)
count(temp1,S)


ggplot(temp1[temp1$S=="04",]) + geom_point(aes(Date, TimeSor, col = Source, alpha = .5))

##########
##########

ggplot(VIP2 %>% filter(ID=="CC")) + 
  geom_point(aes(Date, TimeSor, shape = "Sor", col = "Sor")) +
  geom_point(aes(Date, TimeEntr, shape = "Entr", col = "Entr")) +
  geom_segment(aes(x=Date, xend=Date, y=TimeEntr, yend=TimeSor, alpa = .2))

#CC <- VIP2 %>% filter(ID == "CC" )
CC <- VIP2 %>% filter(ID == "CC" & Date>as.Date("2015-1-1"))
CC <- CC %>% arrange(Date, TimeSor)

Sor<-count(CC,Sor)
Entr<-count(CC,Entr)
Sor <- Sor %>% arrange(desc(n))
Entr <- Entr %>% arrange(desc(n))
names(Sor) <- c("Cde", "nSor")
names(Entr) <- c("Cde", "nEntr")

CC.gare<- full_join(Sor,Entr)
CC.hot <- CC %>% filter(Entr %in% CC.gare$Cde[1:4] & Sor %in% CC.gare$Cde[1:4])

count(CC.hot, DOW)
count(CC.hot, WOY)


##########
##########

trx <- tbl_df(ID_OD)
trx$Time <- trx$Time + 2
names(trx)[c(1,7)] <- c("ID", "TimeSor")
trx$TimeEntr <- trx$TimeSor
trx <- trx[, c(1:6,12,7:11)]

trx <- rbind(trx, VIP2[,-9] %>% filter(ID %in% c("CC", "JA", "MJ", "MR", "VM")))

trx %>% group_by(ID) %>% summarise(Dmin = min(Date), Dmax = max(Date))
trx <- trx %>% filter(Date > as.Date("2014-12-31") & Date < as.Date("2015-5-29"))
##########

matin <- trx %>%
  select(ID, Entr, Sor, Date, DOW, TimeSor) %>%
  filter(TimeSor < 12 ) %>%
  group_by(ID, Entr, Sor, DOW) %>%
  summarise(nDOW = n())

aprem <- trx %>%
  select(ID, Entr, Sor, Date, DOW, TimeSor) %>%
  filter(TimeSor >= 12 ) %>%
  group_by(ID, Entr, Sor, DOW) %>%
  summarise(nDOW = n())

T.matin <- trx %>%
  select(ID, Entr, Sor, Date, DOW, TimeSor) %>%
  filter(TimeSor < 12 ) %>%
  group_by(ID, Entr, Sor, DOW) %>%
  summarise(SD = sd(TimeSor), T = mean(TimeSor),Tmin = T -SD, Tmax = T + SD)

T.aprem <- trx %>%
  select(ID, Entr, Sor, Date, DOW, TimeSor) %>%
  filter(TimeSor >= 12 ) %>%
  group_by(ID, Entr, Sor, DOW) %>%
  summarise(SD = sd(TimeSor), T = mean(TimeSor),Tmin = T -SD, Tmax = T + SD)


T.matin <- inner_join(T.matin, matin)
T.aprem <- inner_join(T.aprem, aprem)

T <- rbind(T.matin, T.aprem)

T.result <- T%>% filter(nDOW> 6)

trx$result <- 0

for (i in 1:nrow(T.result)){
  trx[trx$ID     == T.result$ID[i] &
        trx$Entr == T.result$Entr[i]&
        trx$Sor  == T.result$Sor[i]&
        trx$DOW  == T.result$DOW[i]&
        trx$TimeSor >= T.result$Tmin[i]&
        trx$TimeSor <= T.result$Tmax[i],
      "result"]<-1
}


ggplot(trx) +
  geom_point(aes(Date, TimeEntr, shape = "TimeEntr", col = as.factor(result))) +
  geom_point(aes(Date, TimeSor, shape = "TimeSor", col = as.factor(result))) +
  geom_segment(aes(x=Date, xend=Date, y=TimeEntr, yend=TimeSor)) +
  facet_wrap(~ID)


ggplot(trx) +
  geom_point(aes(TimeEntr, Elat, col = "Elat", shape = "Elat", alpha = .1)) +
  geom_point(aes(TimeSor, Slat, col = "Slat", shape = "Slat", alpha = .1)) +
  geom_segment(aes(x=TimeEntr, xend=TimeSor, y=Elat, yend=Slat, alpha =.3, col = as.factor(result))) +
  facet_wrap(~ID) + ylab("Lat") + ggtitle("Time~Lat")

##########

temp1 <- trx %>% filter(ID %in% c("CC", "FF", "NP", "PC"))
temp2 <- VIP2 %>% filter(ID %in% c("CC", "FF", "NP", "PC"))
temp2 <-tbl_df(temp2)
temp2 <- temp2[, -9]

temp1$Ind <- 1
temp2$Ind <- 2
temp <- rbind(temp1, temp2)
temp$Ind <- as.factor(temp$Ind)

temp %>%
  filter(Date < as.Date("2015-1-1")) %>%
  group_by(ID, Ind) %>% summarise( Dmin = min(Date), Dmax = max(Date), n = n())

temp.2014 <- temp %>%
  filter(Date < as.Date("2015-1-1"))

ggplot(temp.2014) + geom_point(aes(Date, TimeSor, col = as.factor(Ind), shape = as.factor(Ind), alpha = .2)) + facet_wrap(~ID)
