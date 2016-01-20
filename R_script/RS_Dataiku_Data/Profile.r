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
# freq of gare
##########
#ID2_gare_freq<-temp
require(scales)

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


ID2_gare<-temp
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
# plot general
##########
temp<-ID2_OD
t<-"ID2"

qplot(Year,data=temp,geom="histogram",binwidth=.5, xlim=c(2010,2015)) + ylab("Number of Passage") + ggtitle("Nb_Psge by Year")
ggsave(file=paste0(t,"-Nb of Passage by Year.png"))


datebreaks <- seq(as.Date("2010-1-1"), as.Date("2015-3-31"), by="3 month")
datebreaks_week <- seq(as.Date("2009-12-28"), as.Date("2015-3-31"), by="1 week")
d1<-as.Date("2014-1-1")
d2<-as.Date("2014-8-31")
temp_noPassage<-temp[temp$Date>d1 & temp$Date<d2,]
temp_date<-datebreaks_week[datebreaks_week>d1 &datebreaks_week<d2]

# General Plot
qplot(Month,data=temp_noPassage,geom="histogram",binwidth=1,xlim=c(1,9)) + ylab("Number of Passage") + ggtitle("Nb_Psge by Month in 2014")
	ggsave(file=paste0(t,"-Nb of Passage by Month in 2014 .png"))
qplot(Day,data=temp_noPassage,geom="histogram",binwidth=1,xlim=c(1,31))+ ylab("Number of Passage") + ggtitle("Nb_Psge by Day in 2014")
	ggsave(file=paste0(t,"-Nb of Passage by Day in 2014.png"))
qplot(dow,data=temp_noPassage,geom="histogram",binwidth=1,xlim=c(0,7))+ ylab("Number of Passage") + ggtitle("Nb_Psge by DOW in 2014")
	ggsave(file=paste0(t,"-Nb of Passage by DOW in 2014.png"))
qplot(woy,data=temp_noPassage,geom="histogram",binwidth=1,xlim=c(0,53))+ ylab("Number of Passage") + ggtitle("Nb_Psge by WOY in 2014")
	ggsave(file=paste0(t,"-Nb of Passage by WOY in 2014.png"))
qplot(Hour,data=temp_noPassage,geom="histogram",binwidth=1,xlim=c(0,25))+ ylab("Number of Passage") + ggtitle("Nb_Psge by Hour in 2014")
	ggsave(file=paste0(t,"-Nb of Passage by Hour in 2014.png"))

qplot(Date,data=temp_noPassage,geom="histogram",binwidth=1) + 
  ylab("Number of Passage") + ggtitle("Daily Passage") +
  geom_vline(xintercept = as.numeric(temp_date), color="black")  
	ggsave(file=paste0(t,"-Daily Passage in 2014.png"))
  
qplot(KMS,data=temp_noPassage,geom="histogram",binwidth=10)  + 
  ylab("Number of Passage")  + 
  ggtitle("Nb_Psge by KMS") +
  xlim(c(0,100))
  	ggsave(file=paste0(t,"-Nb of Passage by KMS in 2014.png"))

qplot(Tarif,data=temp_noPassage,geom="histogram",binwidth=1)  + 
  ylab("Number of Passage")  + 
  ggtitle("Nb_Psge by Tarif")
	ggsave(file=paste0(t,"-Nb of Passage by Tarif in 2014.png"))

	
	
	
	
	
	
	
	
	
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

