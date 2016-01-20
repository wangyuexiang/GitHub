##########
##########
#VIP FF
##########
##########
ID_FF<-ID_ref[4,3]
trajets_FF<-subset(ID_OD_all,ID_OD_all$ID==ID_FF)
hot_ref_FF<-count(trajets_FF,c("Entr","Sor"))
hot_ref_FF<-hot_ref_FF[order(hot_ref_FF$freq,decreasing=TRUE),]

noPassage_FF<-count(trajets_FF,"Date")

Date<-seq(as.Date("2014-1-1"),as.Date("2015-3-31"),"day")

noPassage_FF<-as.data.frame(Date)
noPassage_FF$OD1<-0
noPassage_FF$OD2<-0
noPassage_FF$OD3<-0
noPassage_FF$OD4<-0
noPassage_FF$OD5<-0

for (i in 1:nrow(trajets_FF)){
  if (trajets_FF$Entr[i]==0 & trajets_FF$Sor[i]==10){
    noPassage_FF$OD1[noPassage_FF$Date==trajets_FF$Date[i]]<-noPassage_FF$OD1[noPassage_FF$Date==trajets_FF$Date[i]]+1
  }
  else if(trajets_FF$Entr[i]==49 & trajets_FF$Sor[i]==2){
    noPassage_FF$OD2[noPassage_FF$Date==trajets_FF$Date[i]]<-noPassage_FF$OD2[noPassage_FF$Date==trajets_FF$Date[i]]+1
  }
  else if(trajets_FF$Entr[i]==220 & trajets_FF$Sor[i]==211){
    noPassage_FF$OD3[noPassage_FF$Date==trajets_FF$Date[i]]<-noPassage_FF$OD3[noPassage_FF$Date==trajets_FF$Date[i]]+1
  }
  else if(trajets_FF$Entr[i]==211 & trajets_FF$Sor[i]==220){
    noPassage_FF$OD4[noPassage_FF$Date==trajets_FF$Date[i]]<-noPassage_FF$OD4[noPassage_FF$Date==trajets_FF$Date[i]]+1
  }
  else if(trajets_FF$Entr[i]==2 & trajets_FF$Sor[i]==49){
    noPassage_FF$OD5[noPassage_FF$Date==trajets_FF$Date[i]]<-noPassage_FF$OD5[noPassage_FF$Date==trajets_FF$Date[i]]+1
  }
}

datebreaks <- seq(as.Date("2014-1-1"), as.Date("2015-3-31"), by="1 month")

datebreaks_week <- seq(as.Date("2013-12-30"), as.Date("2015-3-31"), by="1 week")
d1<-as.Date("2015-1-1")
d2<-as.Date("2015-2-27")

temp_noPassage<-noPassage_FF[noPassage_FF$Date>d1 & noPassage_FF$Date<d2,]
temp_date<-datebreaks_week[datebreaks_week>d1 &datebreaks_week<d2]

ggplot(temp_noPassage,aes(Date))+
  geom_line(aes(y=OD1, color ="0-10")) +
  geom_line(aes(y=OD2, color ="49-2")) +
  geom_line(aes(y=OD3, color ="220-211")) +
  geom_line(aes(y=OD4, color ="211-220")) +
  geom_line(aes(y=OD5, color ="2-49")) +
  xlab("") + ylab("# of Daily Passages") +
  scale_x_date(breaks=datebreaks) +
  geom_vline(xintercept = as.numeric(temp_date), color="black")

##########
ID_OD_FF<-subset(ID_OD,ID_OD$ID==ID_FF)
t1<-ID_OD_FF
t2<-gares

t3<-merge(x=t1,y=t2,by="Sor",all.x=TRUE)
write.table(t3[!is.na(t3$Lib),],file="heatmap_FF.csv",sep=",")

##########
ggplot(ID_OD_FF)+geom_bar(aes(Date),binwidth=1)+scale_x_date()

ID_OD_ref_FF<-count(ID_OD_FF,c("Entr","Sor"))
ID_OD_ref_FF<-ID_OD_ref_FF[order(ID_OD_ref_FF$freq,decreasing=TRUE),]

ID_OD_ref_FF[1:20,]


ggplot(ID_OD_FF[ID_OD_FF$Sor==25006010,])+geom_bar(aes(DOW))+ggtitle("Sor=25006010")
ggplot(ID_OD_FF[ID_OD_FF$Entr==25006049 & ID_OD_FF$Sor==25006002,])+geom_bar(aes(DOW))+ggtitle("Entr=25006049,Sor=25006002")
ggplot(ID_OD_FF[ID_OD_FF$Entr==25006002 & ID_OD_FF$Sor==25006049,])+geom_bar(aes(DOW))+ggtitle("Entr=25006002,Sor=25006049")
ggplot(ID_OD_FF[ID_OD_FF$Entr==25004220 & ID_OD_FF$Sor==25004211,])+geom_bar(aes(DOW))+ggtitle("Entr=25004220,Sor=25004211")
ggplot(ID_OD_FF[ID_OD_FF$Entr==25004211 & ID_OD_FF$Sor==25004220,])+geom_bar(aes(DOW))+ggtitle("Entr=25004211,Sor=25004220")

d1<-as.Date("2015-3-1")
d2<-as.Date("2015-5-10")

ggplot(ID_OD_FF)+geom_bar(aes(Date),binwidth=1)+scale_x_date()+ggtitle("FF") +
  xlim(c(d1,d2))
temp<-ID_OD_FF[ID_OD_FF$Date<=d2 & ID_OD_FF$Date>=d1,]

ggplot(temp[temp$Sor==25006010 ,])+geom_bar(aes(DOW))+ggtitle("Sor=25006010") 
ggplot(temp[temp$Entr==25006049 & temp$Sor==25006002,])+geom_bar(aes(DOW))+ggtitle("Entr=25006049,Sor=25006002")
ggplot(temp[temp$Entr==25006002 & temp$Sor==25006049,])+geom_bar(aes(DOW))+ggtitle("Entr=25006002,Sor=25006049")
ggplot(temp[temp$Entr==25004220 & temp$Sor==25004211,])+geom_bar(aes(DOW))+ggtitle("Entr=25004220,Sor=25004211")
ggplot(temp[temp$Entr==25004211 & temp$Sor==25004220,])+geom_bar(aes(DOW))+ggtitle("Entr=25004211,Sor=25004220")
##########
d1<-as.Date("2015-3-1")
d2<-as.Date("2015-05-10")

test <- seq(d1,d2, by="1 day")
test<-as.data.frame(test)
names(test)<-"Date"
test$DOW<-as.POSIXlt(test$Date)$wday
test$WOY<-as.integer(strftime(as.POSIXlt(test$Date),format="%W"))

nrow(test); count(test,"DOW")
temp<-ID_OD_FF[ID_OD_FF$Date<=d2 & ID_OD_FF$Date>=d1,]

