##########
##########
# ID3
##########
##########

ID3<-Test_set[1000,1]

ID3_OD<-subset(Data,Data$ID==ID3)
ID3_OD_freq<-count(ID3_OD,c("Entr","Sor","Year"))
ID3_OD_freq<-ID3_OD_freq[order(ID3_OD_freq$freq,decreasing=TRUE),]

ID3_noPassage<-count(ID3_OD,"Date")
plot(ID3_noPassage,type="n")
lines(ID3_noPassage)



plot(Test_set$freq_2014)
abline(h=500)
abline(h=300)

##########
# ID3_gare
##########
Entr<-substr(ID3_OD$Entr,4,8)
Sor<-substr(ID3_OD$Sor,4,8)

Entr<-as.integer(Entr)
Sor<-as.integer(Sor)
ID3_gare<-data.frame(Entr,Sor)

ID3_gare$OD<-paste(ID3_gare$Entr,ID3_gare$Sor)
temp<-count(ID3_gare,"OD")
temp<-temp[order(temp$freq,decreasing=TRUE),]

plot(temp$freq,type="n")
lines(temp$freq)

abline(v=5)
abline(v=15)
abline(h=1,col="red")
abline(h=3,col="red")


ggplot(ID3_gare,aes(x=Sor))+  geom_bar(binwidth=100) +
  xlim(c(4000,6100))
ggplot(ID3_gare,aes(x=Sor))+  geom_bar(aes(y = (..count..)/sum(..count..)),binwidth=.5) +  scale_y_continuous(labels = percent) +
  xlim(c(4200,4230))
ggplot(ID3_gare,aes(x=Sor))+    geom_bar(aes(y = (..count..)/sum(..count..)),binwidth=.5) +     scale_y_continuous(labels = percent) +
  xlim(c(6000,6075))



temp<-ID3_gare
temp<-temp[temp$Sor>4000 & temp$Sor<6200,]

temp1<-temp[temp<4300]
temp2<-temp[temp>=6000]
temp2<-temp2-2000
temp<-c(temp1,temp2)
temp<-as.data.frame(temp)

qplot(temp,data=temp,binwidth=.5)+geom_bar()+xlim(c(4200,4230))


##########
# plot general
##########
temp<-ID3_OD

qplot(Year,data=temp,geom="histogram",binwidth=.5)

datebreaks <- seq(as.Date("2010-1-1"), as.Date("2015-3-31"), by="3 month")
datebreaks_week <- seq(as.Date("2009-12-28"), as.Date("2015-3-31"), by="1 week")
d1<-as.Date("2014-1-1")
d2<-as.Date("2014-8-31")
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