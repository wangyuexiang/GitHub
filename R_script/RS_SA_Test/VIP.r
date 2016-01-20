library(party)
library(dplyr)
library(ggplot2)
library(scales)
library(knitr)
library(sm)

##########
# 20150716
##########
NPFF <- rbind(NP,FF)
##########
##########
NP.a <- NP %>% 
  filter(Entr == 25004220 & Sor == 25004211) %>%
  select(Date, DOW, WOY, Hour, Minute, Time)

ggplot(NP.a) + geom_point(aes(Date, Time))
ggplot(NP.a) + geom_boxplot(aes(factor(DOW), y = Time)) 

NP.a <- NP.a %>% group_by(DOW) %>% summarise(n = n(), T = mean(Time), SD = sd(Time), Tmin = T -SD, Tmax = T + SD)
NP.a <- NP.a %>% inner_join(t_D)
NP.a$Prob <- NP.a$n / NP.a$Total
##########
##########
NP.r <- NP %>% 
  filter(Entr == 25004211 & Sor == 25004220) %>%
  select(Date, DOW, WOY, Hour, Minute, Time)

ggplot(NP.r) + geom_point(aes(Date, Time))
ggplot(NP.r) + geom_boxplot(aes(factor(DOW), y = Time)) 

NP.r <- NP.r %>% group_by(DOW) %>% summarise(n = n(), T = mean(Time), SD = sd(Time), Tmin = T -SD, Tmax = T + SD)
NP.r <- NP.r %>% inner_join(t_D)
NP.r$Prob <- NP.r$n / NP.r$Total

##########
##########

trx <- ID_OD
trx <- tbl_df(trx)
trx <- trx %>% filter(Nom %in% c("FF","NP"))
trx$Hour <- trx$Hour + 2
trx$Time <- trx$Time + 2

matin <- trx %>%
  select(Nom, Entr, Sor, Date, DOW, Time) %>%
  filter(Time < 12 ) %>%
  group_by(Nom, Entr, Sor, DOW) %>%
  summarise(nDOW = n())

aprem <- trx %>%
  select(Nom, Entr, Sor, Date, DOW, Time) %>%
  filter(Time >= 12 ) %>%
  group_by(Nom, Entr, Sor, DOW) %>%
  summarise(nDOW = n())

T.matin <- trx %>%
  select(Nom, Entr, Sor, Date, DOW, Time) %>%
  filter(Time < 12 ) %>%
  group_by(Nom, Entr, Sor, DOW) %>%
  summarise(SD = sd(Time), T = mean(Time),Tmin = T -SD, Tmax = T + SD)

T.aprem <- trx %>%
  select(Nom, Entr, Sor, Date, DOW, Time) %>%
  filter(Time >= 12 ) %>%
  group_by(Nom, Entr, Sor, DOW) %>%
  summarise(SD = sd(Time), T = mean(Time),Tmin = T -SD, Tmax = T + SD)


T.matin <- inner_join(T.matin, matin)
T.aprem <- inner_join(T.aprem, aprem)

T <- rbind(T.matin, T.aprem)
T <- inner_join(T, t_D)
T$Per <- T$nDOW / T$Total

write.table(T, file = "T.csv", sep=";", row.names = F)
##########
##########

t1 <- NP2_par_troncons[NP2_par_troncons$Sens == 1,]
t2 <- NP2_par_troncons[NP2_par_troncons$Sens == 2,]
names(t2)[3:4]<-c("Sor","Entr")
t <- rbind(t1,t2)
t$Date <- as.Date(paste0(t$Year, "-", t$Month, "-", t$Day))
t <- t[, c("Entr", "Sor", "Date", "DOW", "Time")]
t$Nom <- "NP"
NPFF <- t

t1 <- FF2_par_troncons[FF2_par_troncons$Sens == 1,]
t2 <- FF2_par_troncons[FF2_par_troncons$Sens == 2,]
names(t2)[3:4]<-c("Sor","Entr")
t <- rbind(t1,t2)
t$Date <- as.Date(paste0(t$Year, "-", t$Month, "-", t$Day))
t <- t[, c("Entr", "Sor", "Date", "DOW", "Time")]
t$Nom <- "FF"

NPFF <- rbind(NPFF, t)
NPFF$Time <- as.numeric(NPFF$Time) + 2
NPFF <- tbl_df(NPFF)

matin <- NPFF %>%
  filter(Time < 12 ) %>%
  group_by(Nom, Entr, Sor, DOW) %>%
  summarise(nDOW = n())

aprem <- NPFF %>%
  filter(Time >= 12 ) %>%
  group_by(Nom, Entr, Sor, DOW) %>%
  summarise(nDOW = n())

T.matin <- NPFF %>%
  filter(Time < 12 ) %>%
  group_by(Nom, Entr, Sor, DOW) %>%
  summarise(SD = sd(Time), T = mean(Time),Tmin = T -SD, Tmax = T + SD)

T.aprem <- NPFF %>%
  filter(Time >= 12 ) %>%
  group_by(Nom, Entr, Sor, DOW) %>%
  summarise(SD = sd(Time), T = mean(Time),Tmin = T -SD, Tmax = T + SD)


T.matin <- inner_join(T.matin, matin)
T.aprem <- inner_join(T.aprem, aprem)

TT <- rbind(T.matin, T.aprem)
TT$DOW <- as.numeric(TT$DOW)
TT <- inner_join(TT, t_D)
TT$Per <- TT$nDOW / TT$Total
write.table(TT, file = "TT.csv", sep=";", row.names = F)




##########
# 20150623
##########
OD[OD$freq==2,]
ggplot(NP)+
  geom_segment(aes(x=Elng, y=Elat,xend=Slng, yend=Slat,col=as.factor(noPsg)),
               alpha=.4,lwd=1
               )+
  geom_point(data=Sor,aes(Lng, Lat,size=freq), col="Red")+
  geom_point(data=gares_geo,aes(Lng,Lat),size=3,alpha=.2,shape=3)+
  geom_point(data=gare_SO,aes(Lng, Lat, color="S.O."), size=3, shape=3)+
  ylim(c(42,46.5))+xlim(c(-1.5,7.5))+
  theme(legend.position=c(0,.5),plot.title = element_text(size=20, face="bold")) +
  ggtitle("Sor & OD")
##########
j<-3
tOD<-subset(NP,NP$Entr==OD$Entr[j] & NP$Sor==OD$Sor[j])

plot(count(tOD,"Date")$freq)
boxplot(count(tOD,"Date")$freq)


ggplot(tOD)+geom_bar(aes(Date,fill=as.factor(DOW)),binwidth=1)+
  geom_bar(data=JF[JF$Date<=d2 & JF$JF==1,],aes(Date),binwidth=1)

ggplot(tOD,aes(Date,Time,color=as.factor(DOW)))+geom_point()

count(tOD,"DOW")
count(tOD,c("Year","WOY"))

ggplot(tOD,aes(DOW))+geom_bar(binwidth=1)
ggplot(tOD,aes(WOY,fill=as.factor(Year)))+geom_bar(binwidth=1,position="dodge")
##########
PC<-ID_OD[ID_OD$Nom=="PC",]
OD_PC<-count(PC,c("Entr","Sor"))
OD_PC<-OD_PC[order(OD_PC$freq,decreasing=T),]
j<-1
tOD<-subset(PC,PC$Entr==OD_PC$Entr[j] & PC$Sor==OD_PC$Sor[j])
ggplot(tOD,aes(Date,Hour))+geom_point()
ggplot(tOD,aes(Date,Time))+geom_point()

ggplot(tOD, aes(Date, Time)) + geom_point()
ggplot(tOD[tOD$DOW == 1, ],aes(Date,Time)) + geom_point()

ggplot(tOD[tOD$DOW == 1, ],aes(x=1,y=Time)) + geom_boxplot()

ggplot(tOD,aes(Date,Time,color=as.factor(DOW)))+geom_point()

ggplot(tOD[tOD$Time<12,],aes(factor(DOW),y=Time))+geom_boxplot()+ylim(c(0,25))
ggplot(tOD[tOD$Time>=12,],aes(factor(DOW),y=Time))+geom_boxplot()+ylim(c(0,25))
##########
FF<-ID_OD[ID_OD$Nom=="FF",]
OD_FF<-count(FF,c("Entr","Sor"))
OD_FF<-OD_FF[order(OD_FF$freq,decreasing=T),]
j<-1
tOD<-subset(FF,FF$Entr==OD_FF$Entr[j] & FF$Sor==OD_FF$Sor[j])
ggplot(tOD,aes(Date,Hour))+geom_point()
ggplot(tOD,aes(Date,Time,color=as.factor(DOW)))+geom_point()
t2<-ggplot(tOD[tOD$Time<12,],aes(factor(DOW),y=Time))+geom_boxplot()+ylim(c(0,25))
t1<-ggplot(tOD[tOD$Time>=12,],aes(factor(DOW),y=Time))+geom_boxplot()+ylim(c(0,25))
grid.arrange(t1,t2,ncol=1)

ggplot(tOD, aes(Date, Time)) + geom_point()

##########
# 20150618
##########
ID_OD$Time<-ID_OD$Hour+ID_OD$Minute/60
##########
# 20150617
##########
gares<-read.table("garesLatLng.csv",head=TRUE,sep=";")
gares_geo<-gares[,c(1:4,6,8,7)]
ggplot(gares_geo,aes(Lng,Lat,color=as.factor(Societe)))+geom_point()

geo<-gares_geo[,c(4,6,7)]
geo<-geo[geo$Cde>25000000,]

names(geo)<-c("Sor","Slng","Slat")
ID_OD<-merge(x=ID_OD,y=geo,by="Sor",all.x=TRUE)
names(geo)<-c("Entr","Elng","Elat")
ID_OD<-merge(x=ID_OD,y=geo,by="Entr",all.x=TRUE)

ID_OD[ID_OD$Entr==0,]$Elng<-ID_OD[ID_OD$Entr==0,]$Slng
ID_OD[ID_OD$Entr==0,]$Elat<-ID_OD[ID_OD$Entr==0,]$Slat

temp<-NP[,18:21]
temp1<-temp[,1:2]
temp2<-temp[,3:4]

cl <- kmeans(temp1,5)
cl$cluster
plot(temp1$Slng,temp1$Slat,col=cl$cluster)
points(cl$centers, pch=16)
##########
# 20150610
##########
d1<-as.Date("2014-1-1")
d2<-as.Date("2014-12-31")
t<-In_365[In_365$Date<=d2,]
head(t)

ggplot(t,aes(WOY,DOW,fill=as.factor(DOW)))+geom_tile()
ggplot(t,aes(WOY,DOW,fill=as.factor(JF)))+geom_tile()
ggplot(t,aes(WOY,DOW,fill=as.factor(VS)))+geom_tile()

ggplot(t,aes(Hour,DOW,fill=as.factor(DOW)))+geom_tile()


ggplot(result,aes(Per))+geom_density()
##########
ggplot(result[,],aes(Per))+geom_density()
ggplot(result[result$Per>.6,],aes(Per))+geom_density()
nrow(result[result$Per>.6,])
##########
nrow(temp1[temp1$freq>10,])
nrow(temp2[temp2$freq>10,])
ggplot(temp2[temp2$freq>10,],aes(freq))+geom_density()
temp4<-count(temp,c("Nom","Entr","Sor","Date"))
temp4<-temp4[order(temp4$freq,decreasing=TRUE),]
##########
ggplot(temp2,aes(DOW,Hour))+geom_tile()
temp2<-temp[temp$Entr==25004220 & temp$Sor==25004211,]
temp3<-temp[temp$Entr==25004211 & temp$Sor==25004220,]
p1<-ggplot(temp2,aes(Hour,DOW))+geom_tile(binwidth=1)+xlim(c(0,25))+ylim(c(0,7))
p2<-ggplot(temp3,aes(Hour,DOW))+geom_tile(binwidth=1)+xlim(c(0,25))+ylim(c(0,7))
grid.arrange(p1,p2,ncol=2)

d1<-as.Date("2014-1-1")
d2<-as.Date("2014-12-31")
t<-In_365[In_365$Date<=d2,]
##########
gare_SO<-read.table("gare_SO.csv",sep=",",head=T)
