##########
##########
# ID2
##########
##########

ID2<-Test_set[1,1]

ID2_OD<-subset(Data,Data$ID==ID2)
ID2_2014<-subset(ID2_OD,ID2_OD$Year==2014)

par(mfrow=c(2,2))

ID2_OD_freq<-count(ID2_OD,c("Entr","Sor","Year"))
ID2_OD_freq<-ID2_OD_freq[order(ID2_OD_freq$freq,decreasing=TRUE),]
##########
# ts
##########
library(forecast)

ID2_noPassage<-count(ID2_OD,"Date")
temp<-ID2_noPassage

ggplot(temp, aes(Date, freq)) + 
  geom_line() +
  scale_x_date() + 
  xlab("") + 
  ylab("Daily Passages") +
  xlim(c(as.Date("2014-1-1"),as.Date("2014-8-31")))

plot(temp,xlim=c(as.Date("2014-1-1"),as.Date("2014-8-31")),type="n")
lines(temp,xlim=c(as.Date("2014-1-1"),as.Date("2014-8-31")))


temp1<-ts(temp$freq,frequency=7)
fit<-ets(temp1)

##########
# freq of gare
##########
#ID2_gare_freq<-temp


Entr<-substr(ID2_OD$Entr,4,8)
Sor<-substr(ID2_OD$Sor,4,8)

Entr<-as.integer(Entr)
Sor<-as.integer(Sor)
ID2_gare<-data.frame(Entr,Sor)

ggplot(ID2_gare,aes(x=Sor))+
  geom_bar(aes(y = (..count..)/sum(..count..)),binwidth=100) +
#  geom_bar() +
  scale_y_continuous(labels = percent) +
  xlim(c(4000,6100))

ggplot(temp, aes(x = Entr)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)),binwidth=1) +
  scale_y_continuous(labels = percent) +
  xlim(c(4200,4250))

ggplot(temp, aes(x = Entr)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)),binwidth=10000) +
  scale_y_continuous(labels = percent) 


n<-nrow(ID2_gare)

temp<-ID2_gare
temp<-temp[temp$Sor>4000 & temp$Sor<6200,]

temp1<-temp[temp<4300]
temp2<-temp[temp>=6000]
temp2<-temp2-2000+100
temp<-c(temp1,temp2)
temp<-as.data.frame(temp)

qplot(temp,data=temp,binwidth=.5)+geom_bar()+xlim(c(4210,4215))
qplot(temp,data=temp,binwidth=.5)+geom_bar()+xlim(c(4210,4215))

##########
# temp_freq
##########
temp<-ID2_OD_freq
temp$OD<-paste0(temp$Entr," ",temp$Sor)
temp$Entr<-NULL
temp$Sor<-NULL

for (i in 2010:2014){
  assign(paste0("temp_",i),subset(temp,temp$Year==i))
}

names(temp_2014)[2]<-"no_2014"
names(temp_2013)[2]<-"no_2013"
names(temp_2012)[2]<-"no_2012"
names(temp_2011)[2]<-"no_2011"
names(temp_2010)[2]<-"no_2010"

temp_2014$Year<-NULL
temp_2013$Year<-NULL
temp_2012$Year<-NULL
temp_2011$Year<-NULL
temp_2010$Year<-NULL

temp_freq<-merge(temp_2014,temp_2013,by="OD",all=TRUE)
temp_freq<-merge(temp_freq,temp_2012,by="OD",all=TRUE)
temp_freq<-merge(temp_freq,temp_2011,by="OD",all=TRUE)

rm(temp_2014,temp_2013,temp_2012,temp_2011,temp_2010)

temp_freq[is.na(temp_freq)]<-0
temp_freq<-temp_freq[order(temp_freq$no_2014,decreasing=TRUE),]


qplot(no_2014,data=temp_freq[temp_freq$no_2014!=0,],geom="histogram",binwidth=50) + 
  xlab("Number of Passage in 2014") + 
  ylab("Number of OD")+
  ggtitle("Distribution of OD by Nb of Passage in 2014")

##########
# temp_key
##########
temp<-ID2_OD
Date<-seq(as.Date("2014-1-1"),as.Date("2014-8-31"),"day")
temp_key<-as.data.frame(Date)
temp_key$OD1<-0
temp_key$OD2<-0
temp_key$OD3<-0
temp_key$OD4<-0
temp_key$OD5<-0
temp_key$OD6<-0

for (i in 1:nrow(temp)){
  if (temp$Entr[i]==25004213 & temp$Sor[i]==25004220){
    temp_key$OD1[temp_key$Date==temp$Date[i]]<-temp_key$OD1[temp_key$Date==temp$Date[i]]+1
  }
  else if(temp$Entr[i]==25004211 & temp$Sor[i]==25004213){
    temp_key$OD2[temp_key$Date==temp$Date[i]]<-temp_key$OD2[temp_key$Date==temp$Date[i]]+1
  }
  else if(temp$Entr[i]==25004213 & temp$Sor[i]==25004211){
    temp_key$OD3[temp_key$Date==temp$Date[i]]<-temp_key$OD3[temp_key$Date==temp$Date[i]]+1
  }
  else if(temp$Entr[i]==25004220 & temp$Sor[i]==25004213){
    temp_key$OD4[temp_key$Date==temp$Date[i]]<-temp_key$OD4[temp_key$Date==temp$Date[i]]+1
  }
  else if(temp$Entr[i]==25004266 & temp$Sor[i]==25004213){
    temp_key$OD5[temp_key$Date==temp$Date[i]]<-temp_key$OD5[temp_key$Date==temp$Date[i]]+1
  }
  else if(temp$Entr[i]==25004213 & temp$Sor[i]==25004266){
    temp_key$OD6[temp_key$Date==temp$Date[i]]<-temp_key$OD5[temp_key$Date==temp$Date[i]]+1
  }
}

datebreaks <- seq(as.Date("2014-1-1"), as.Date("2014-12-31"), by="1 month")
datebreaks_week <- seq(as.Date("2013-12-30"), as.Date("2014-12-31"), by="1 week")

d1<-as.Date("2014-6-1")
d2<-as.Date("2014-6-30")
temp_noPassage<-temp_key[temp_key$Date>d1 & temp_key$Date<d2,]
temp_date<-datebreaks_week[datebreaks_week>d1 &datebreaks_week<d2]

ggplot(temp_noPassage,aes(Date))+
  geom_line(aes(y=OD1, color ="OD1")) +
  geom_line(aes(y=OD2, color ="OD2")) +
  geom_line(aes(y=OD3, color ="OD3")) +
  geom_line(aes(y=OD4, color ="OD4")) +
  geom_line(aes(y=OD5, color ="OD5")) +
  geom_line(aes(y=OD6, color ="OD6")) +
  xlab("") + ylab("# of Daily Passages") +
  scale_x_date(breaks=datebreaks) +
  geom_vline(xintercept = as.numeric(temp_date), color="black")




##########
# 
##########
d1<-as.Date("2014-7-8")
d2<-as.Date("2014-7-10")

# temp<-subset(trajets,trajets$ID==ID2)
# 
# names(temp)<-c("ID","Entr","Dat_Entr","Sor","Dat_Sor","Tar_TCC","KMS","Cl")
# temp$Year<-as.integer(substr(temp$Dat_Sor,1,4))
# temp$Month<-as.integer(substr(temp$Dat_Sor,6,7))
# temp$Day<-as.integer(substr(temp$Dat_Sor,9,10))
# temp$Date<-as.Date(substr(temp$Dat_Sor,1,10))
# temp$Hour<-as.integer(substr(temp$Dat_Sor,12,13))
# temp$Minute<-as.integer(substr(temp$Dat_Sor,15,16))

temp<-temp_min
u<-c(2,4,13:ncol(temp))
temp1<-temp[temp$Date>d1 & temp$Date<d2,u]
temp1<-temp1[order(temp1$Hour*100+temp1$Minute),]



##########
# plot general
##########
temp<-ID2_OD

qplot(Year,data=temp,geom="histogram",binwidth=.5)

datebreaks <- seq(as.Date("2010-1-1"), as.Date("2015-3-31"), by="3 month")
datebreaks_week <- seq(as.Date("2009-12-28"), as.Date("2015-3-31"), by="1 week")
d1<-as.Date("2014-7-7")
d2<-as.Date("2014-7-9")
temp_noPassage<-temp[temp$Date>d1 & temp$Date<d2,]
temp_date<-datebreaks_week[datebreaks_week>d1 &datebreaks_week<d2]

# General Plot
qplot(Month,data=temp_noPassage,geom="histogram",binwidth=1,xlim=c(1,12)) + ylab("Number of Passage") + ggtitle("Nb_Psge by Month")
qplot(Day,data=temp_noPassage,geom="histogram",binwidth=1,xlim=c(1,31))+ ylab("Number of Passage") + ggtitle("Nb_Psge by Day")
qplot(dow,data=temp_noPassage,geom="histogram",binwidth=1,xlim=c(0,7))+ ylab("Number of Passage") + ggtitle("Nb_Psge by DOW")
qplot(woy,data=temp_noPassage,geom="histogram",binwidth=1,xlim=c(0,53))+ ylab("Number of Passage") + ggtitle("Nb_Psge by WOY")
qplot(Hour,data=temp_noPassage,geom="histogram",binwidth=1,xlim=c(0,25))+ ylab("Number of Passage") + ggtitle("Nb_Psge by Hour")

qplot(Date,data=temp_noPassage,geom="histogram",binwidth=1) + 
  ylab("Number of Passage") + ggtitle("Daily Passage") +
  geom_vline(xintercept = as.numeric(temp_date), color="black")  
qplot(KMS,data=temp_noPassage,geom="histogram",binwidth=10)  + 
  ylab("Number of Passage")  + 
  ggtitle("Nb_Psge by KMS") +
  xlim(c(0,100))
qplot(Tarif,data=temp_noPassage,geom="histogram",binwidth=1)  + 
  ylab("Number of Passage")  + 
  ggtitle("Nb_Psge by Tarif")



require(gridExtra)
p1<-qplot(Month,data=temp_noPassage,geom="histogram",binwidth=1,xlim=c(1,12)) + ylab("Number of Passage") + ggtitle("Nb_Psge by Month")
p2<-qplot(Day,data=temp_noPassage,geom="histogram",binwidth=1,xlim=c(1,31))+ ylab("Number of Passage") + ggtitle("Nb_Psge by Day")
grid.arrange(p1, p2, ncol=2)