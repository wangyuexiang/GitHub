#library(plyr)
library(dplyr)
library(ggplot2)
library(knitr)
library(gridExtra)


##########
# 20150715
##########
g1 <- ggplot(ID1) + 
  geom_point(aes(Date, Elng, col = "Elng", shape = "Elng", alpha = .5)) +
  geom_point(aes(Date, Slng, col = "Slng", shape = "Slng", alpha = .5))
g2 <- ggplot(ID1) + 
  geom_point(aes(Date, Elng, col = "Elat", shape = "Elat", alpha = .5)) +
  geom_point(aes(Date, Slng, col = "Slat", shape = "Slat", alpha = .5))


ID2 <- trx.2014 %>% filter(Label == 2)
g1 <- ggplot(ID2) + 
  geom_point(aes(Date, Elng, col = "Elng", shape = "Elng", alpha = .5)) +
  geom_point(aes(Date, Slng, col = "Slng", shape = "Slng", alpha = .5))+
  geom_segment(aes(x = Date, xend = Date, y = Elng, yend = Slng, alpha = .1))
g2 <- ggplot(ID2) + 
  geom_point(aes(Date, Elng, col = "Elat", shape = "Elat", alpha = .5)) +
  geom_point(aes(Date, Slng, col = "Slat", shape = "Slat", alpha = .5))+
  geom_segment(aes(x = Date, xend = Date, y = Elng, yend = Slng, alpha = .1))
grid.arrange(g1, g2, ncol = 1)


##########
# 20150703
##########
ID1<-trx.2014 %>% filter(Label == 1)
EA1 <- ID1 %>% filter(Sor == 25006002)
EA2 <- ID1 %>% filter(Entr == 25004220)

AE2 <- ID1 %>% filter(Entr == 25006002)
AE1 <- ID1 %>% filter(Sor == 25004220)

EA1$N<-1
EA2$N<-2
AE1$N<-3
AE2$N<-4
EA <- rbind(EA1,EA2,AE1,AE2)

ggplot(EA) +  geom_bar(aes(Date) , binwidth = 1) + facet_wrap(~N)
ggplot(EA) +  geom_point(aes(Date, Time, col = as.factor(N)))


tEA<- EA %>% 
  group_by(ID, Date, N) %>%
  summarise(count = n()) %>%
  group_by(ID, N, count) %>%
  summarise(count1 = n())

ggplot(tEA) + geom_point(aes(count,count1, col = as.factor(N)))


##########
# 20150703
##########
gare <- read.table("garesLatLng.csv", sep= ";", head = T)
load("trx.5000.RData")

OD <- trx.5000 %>%
  select(Entr, Sor) %>%
  unique()

gare$Cde <- as.character(gare$Cde)
Entr <- gare
names(Entr) <- c("Esoc","Elib","ER","Entr","Esens","Epk","ELat","ELng")
Sor <- gare
names(Sor) <- c("Ssoc","Slib","SR","Sor","Ssens","Spk","SLat","SLng")

OD <- OD %>%
  left_join(Entr, by = "Entr") %>%
  left_join(Sor, by = "Sor")

nrow(OD)
#[1] 9140
nrow(OD %>% filter(ER == SR))
#[1] 1937
nrow(OD %>% filter(Esoc == Ssoc))
#[1] 3391

tOD <- OD %>%
  filter(Esoc %in% c(4:6) | Entr == "00000000") %>%
  filter(Ssoc %in% c(4:6)) 

ggplot(tOD %>% filter(ER == SR)) + 
  geom_segment(aes(x = ELng, y = ELat, xend = SLng, yend = SLat)) +
  geom_segment(data = tOD %>% filter(ER != SR), aes(x = ELng, y = ELat, xend = SLng, yend = SLat, col = "red")) +
  geom_point(data=gare, aes(Lng,Lat,col=Autoroute)) +
  ggtitle()

ggplot(gare[gare$Societe != 5 & gare$Lng > 2.3, ]) +
  geom_point(aes(Lng, Lat, col = Autoroute))

##########
JF <- read.table("JF_2014-2015-2016.csv", sep = ",", head = T)
JF <- JF[JF$Year == 2014 & JF$Month <9,]
count(JF[JF$JF == 0, ], DOW)

trx.5000 <- trx.5000 %>% mutate(OD = paste0(Entr,"-",Sor))
trx.5000 <- trx.5000 %>% mutate(Date = as.Date(paste0(Year,"-",Month,"-",Day)))
trx.5000$Dat_Sor <- NULL
trx.5000 <- trx.5000 %>% mutate(DOW = as.POSIXlt(Date)$wday)
trx.5000 <- trx.5000 %>% mutate(WOY = as.numeric(format(Date+3, "%U")))

save(trx.5000, file = "trx.5000.v2.RData")
rm(trx.5000)


ID.ref<- trx.5000 %>%
  group_by(Label) %>%
  summarise(total = n(),
            n.Entr = n_distinct(Entr), 
            n.Sor = n_distinct(Sor),
            n.OD = n_distinct(OD),
            debut = min(Date),
            fin = max(Date))

##########
# 2014
##########
trx.2014 <- trx.5000 %>% filter(Year == 2014)
ID1 <- trx.2014 %>% filter(Label == 1)

ID.OD.ref <- trx.5000 %>%
  filter(Year == 2014) %>%
  group_by(Label, Entr, Sor) %>%
  summarise(noPsg = n(), T_mean = mean(Time), T_sd = sd(Time)) %>%
  # T_sd == NaN, if & only if noPsg == 1
  group_by(Label) %>%
  arrange(desc(noPsg)) 

temp <-   trx.5000 %>%
  filter(Year == 2014) %>%
  group_by(Label, Entr, Sor, Date) %>%
  summarise(noJour = n()) %>%
  group_by(Label, Entr, Sor) %>%
  summarise(nJ = n(), noPsg =sum(noJour), nJ_mean = mean(noJour), nJ_sd = sd(noJour), nJ_min = min(noJour), nJ_max = max(noJour)) %>%
  group_by(Label) %>%
  arrange(desc(noPsg))

# nrow(temp[temp$noPsg==1,])
# sum(is.na(temp$nJ_sd))
ggplot(temp)+geom_bar(aes(nJ),binwidth=10)
ggplot(temp[temp$nJ>=10,])+geom_bar(aes(nJ),binwidth=10)
count(temp[temp$nJ<10,],nJ)

ID.OD.ref <- inner_join(ID.OD.ref,temp,by=c("Label","Entr","Sor","noPsg"))

ggplot(ID.OD.ref)+geom_bar(aes(nJ_mean),binwidth=.1)
ggplot(ID.OD.ref)+geom_bar(aes(nJ_sd),binwidth=.1)
ggplot(ID.OD.ref[ID.OD.ref$nJ_sd>0,])+geom_bar(aes(nJ_sd),binwidth=.1)

count(ID.OD.ref[ID.OD.ref$nJ_max==ID.OD.ref$nJ_min,],nJ_min)


temp1 <-   trx.5000 %>%
  filter(Year == 2014) %>%
  group_by(Label, Entr, Sor, Date) %>%
  summarise(noJour = n())

# t1 <- temp1 %>% filter(noJour == 1) %>% group_by(Label, Entr, Sor) %>% summarise(n1 = n())
# t2 <- temp1 %>% filter(noJour == 2) %>% group_by(Label, Entr, Sor) %>% summarise(n2 = n())
# t3 <- temp1 %>% filter(noJour > 2) %>% group_by(Label, Entr, Sor) %>% summarise(n3 = n())
rm(t1,t2,t3)
t <- full_join(t1, t2, by = c("Label", "Entr", "Sor")) %>%
  full_join(t3, by = c("Label", "Entr", "Sor"))
t[is.na(t)] <- 0

temp <- inner_join(temp, t, by = c("Label", "Entr", "Sor"))
ID.OD.ref <- inner_join(ID.OD.ref, t, by = c("Label", "Entr", "Sor"))

nrow(temp)
# 289573
nrow(temp %>% filter(noPsg == 1))
# 151360
count(temp %>% filter(noPsg < 10),noPsg)

nrow(temp %>% filter(nJ_max == 1))
# 264884
nrow(temp %>% filter(nJ_min > 1))
# 4631


t1 <- ID.OD.ref %>% 
  filter(nJ_max + nJ_sd < 2) %>%
  filter(n1 > 50)
sum(t1$noPsg)
# 279557

i <- 20
test <- trx.2014 %>%
  filter(Label == t1$Label[i] &
         Entr  == t1$Entr[i] &
         Sor   == t1$Sor[i] )
ggplot(test) + geom_point(aes(Date,Time)) + facet_wrap(~DOW,ncol=1) + ylim(0, 25)
ggplot(test) + geom_boxplot(aes(x=DOW,y=Time)) + facet_wrap(~DOW,ncol=7) + ylim(0, 25)
##########



t2 <- ID.OD.ref %>% 
#  filter(nJ_max + nJ_sd < 2) %>%
  filter(n2 > 50)
sum(t2$noPsg)
# 279557

i <- 8
test <- trx.2014 %>%
  filter(Label == t2$Label[i] &
           Entr  == t2$Entr[i] &
           Sor   == t2$Sor[i] )

ggplot(test) + geom_point(aes(Date,Time)) + ylim(0, 25)
ggplot(test) + geom_point(aes(Date,Time)) + facet_wrap(~DOW,ncol=1) + ylim(0, 25)
ggplot(test) + geom_boxplot(aes(x=DOW,y=Time)) + facet_wrap(~DOW,ncol=7) + ylim(0, 25)
##########
# special
# 1 25004213 25004220   443
# 3 25004213 25004220 
# 4 00000000 25006012









temp <- top3 %>%
  group_by(Label) %>%
  summarise(no = n()) %>%
  filter(no == 2) %>%
  select(Label) %>%
  inner_join(trx.5000[trx.5000$Year == 2014,], by ="Label") %>%
  group_by(Label, Entr, Sor) %>%
  summarise(count = n())


# ID.2014 <- trx %>%
#   select(ID,Year) %>%
#   filter(Year == 2014) %>%
#   distinct %>%
#   select(ID)
# 46849
  
# trx.2014 <- inner_join(trx, ID.2014, by = "ID")
# 19405744

# ID_OD_2014 <- trx %>%
#   select(ID,Year) %>%
#   filter(Year == 2014) %>%
#   distinct %>%
#   select(ID) %>%
#   inner_join(trx,by="ID")
# 19405744

#nrow(distinct(select(trx,ID)))
# 57362
# trx <- mutate(trx,
#   Time = Hour + Minute / 60,
#   Date = as.Date(paste0(Year,"-",Month,"-",Day))
# )

##########
# 20150706
##########
gares_geo<-gare[,c(1:4,6,8,7)]

geo<-gares_geo[,c(4,6,7)]
gare_SO <- read.table("gare_SO.csv", head = T, sep =",")

names(geo)<-c("Entr","Elng","Elat")
trx.2014 <- left_join(x=trx.2014,y=geo,by="Entr")
names(geo)<-c("Sor","Slng","Slat")
trx.2014 <- left_join(x=trx.2014,y=geo,by="Sor")

trx.2014$Entr <- as.factor(trx.2014$Entr)
trx.2014$Sor <- as.factor(trx.2014$Sor)

##########
# 20150707
##########
ID_1_5 <- trx.2014 %>% filter(Label < 6)
save(ID_1_5, file = "ID_1_5.RData")

ID2 <- trx.2014 %>% filter(Label == 2)

ID12 <- rbind(ID1,ID2)
