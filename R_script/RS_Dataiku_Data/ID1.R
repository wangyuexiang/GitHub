##########
##########
# ID1
##########
##########
ID1<-t1[t1$freq>8,][1,1]
ID1_trajets<-subset(ID_OD,ID_OD$ID==ID1)

Date<-seq(as.Date("2011-1-1"),as.Date("2015-4-30"),"day")
D365<-data.frame(Date)
D365$Year<-as.numeric(format(D365$Date, "%Y"))
D365$Month<-as.numeric(format(D365$Date, "%m"))
D365$Day<-as.numeric(format(D365$Date, "%d"))

D365$dow<-as.POSIXlt(D365$Date)$wday
D365$dow[D365$dow==0]<-7

for (i in 2011:2015){
  assign(paste0("Y",i),subset(D365,D365$Year==i))
}

rm(Date,i)

ID1_no_passage<-count(ID1_trajets,c("Year","Month","Day"))

ID1_365<-D365
ID1_365$no_passage<-0

for (i in 1:nrow(ID1_365)){
  for (j in 1:nrow(ID1_no_passage)){
    if(ID1_365$Year[i]==ID1_no_passage$Year[j] & ID1_365$Month[i] == ID1_no_passage$Month[j] & ID1_365$Day[i]== ID1_no_passage$Day[j]){
      ID1_365$no_passage[i]<-ID1_no_passage$freq[j]
      break
    }
  }
}


for (i in 2011:2015){
  assign(paste0("ID1_Y",i),subset(ID1_365,ID1_365$Year==i))
}

datebreaks <- seq(as.Date("2011-1-1"), as.Date("2015-05-01"), by="1 month")
ggplot(ID1_Y2011, aes(Date, no_passage)) + 
  geom_line() + xlab("") + ylab("# of Daily Passages") +
  scale_x_date(breaks=datebreaks)
ggplot(ID1_Y2012, aes(Date, no_passage)) + 
  geom_line() + xlab("") + ylab("# of Daily Passages") +
  scale_x_date(breaks=datebreaks)
ggplot(ID1_Y2013, aes(Date, no_passage)) + 
  geom_line() + xlab("") + ylab("# of Daily Passages") +
  scale_x_date(breaks=datebreaks)
ggplot(ID1_Y2014, aes(Date, no_passage)) + 
  geom_line() + xlab("") + ylab("# of Daily Passages") +
  scale_x_date(breaks=datebreaks)


##########
##########
# 2015-05-18
##########
##########
ID1_OD<-ID1_trajets

ID1_OD$Dat_Entr<-NULL
ID1_OD$Dat_Sor<-NULL
ID1_OD$Tar_TCC<-NULL
#ID1_OD$KMS<-NULL
ID1_OD$Cl<-NULL
ID1_OD$Date<-as.Date(paste0(ID1_OD$Year,"-",ID1_OD$Month,"-",ID1_OD$Day))
ID1_OD$dow<-as.POSIXlt(ID1_OD$Date)$wday
ID1_OD$woy<-as.numeric( format(ID1_OD$Date+3, "%U"))


#ID1_OD_freq<-count(ID1_OD,c("Entr","Sor"))
ID1_OD_freq<-count(ID1_OD,c("Entr","Sor","KMS","Year"))

ID1_OD_freq<-ID1_OD_freq[order(ID1_OD_freq$freq,decreasing=TRUE),]




ID1_OD_freq$OD<-paste(ID1_OD_freq$Entr,ID1_OD_freq$Sor)
ID1_OD_freq$Entr<-NULL
ID1_OD_freq$Sor<-NULL


temp_2014<-ID1_OD_freq[ID1_OD_freq$Year==2014,]
temp_2013<-ID1_OD_freq[ID1_OD_freq$Year==2013,]
temp_2012<-ID1_OD_freq[ID1_OD_freq$Year==2012,]
temp_2011<-ID1_OD_freq[ID1_OD_freq$Year==2011,]
temp_2010<-ID1_OD_freq[ID1_OD_freq$Year==2010,]

names(temp_2014)[3]<-"no_2014"
names(temp_2013)[3]<-"no_2013"
names(temp_2012)[3]<-"no_2012"
names(temp_2011)[3]<-"no_2011"
names(temp_2010)[3]<-"no_2010"

temp_2014$Year<-NULL
temp_2013$Year<-NULL
temp_2012$Year<-NULL
temp_2011$Year<-NULL
temp_2010$Year<-NULL

ID1_noPassage<-merge(temp_2014,temp_2013,by="OD",all=TRUE)
ID1_noPassage<-merge(ID1_noPassage,temp_2012,by="OD",all=TRUE)
ID1_noPassage<-merge(ID1_noPassage,temp_2011,by="OD",all=TRUE)

ID1_noPassage[is.na(ID1_noPassage)]<-0

ID1_noPassage<-merge(temp_2014,temp_2013,by="OD",all=TRUE)
ID1_noPassage<-merge(ID1_noPassage,temp_2012,by="OD",all=TRUE)
ID1_noPassage<-merge(ID1_noPassage,temp_2011,by="OD",all=TRUE)
names(ID1_noPassage)[c(2,4,6,8)]<-c("t1","t2","t3","t4")

for (i in 1:179){
  if(ID1_noPassage$t1[i]!=0){
    ID1_noPassage$KMS[i]<-ID1_noPassage$t1[i]
  }
  else if(ID1_noPassage$t2[i]!=0){
    ID1_noPassage$KMS[i]<-ID1_noPassage$t2[i]
  }
  else if(ID1_noPassage$t3[i]!=0){
    ID1_noPassage$KMS[i]<-ID1_noPassage$t3[i]
  }
  else if(ID1_noPassage$t4[i]!=0){
    ID1_noPassage$KMS[i]<-ID1_noPassage$t4[i]
  }
}

ID1_noPassage$t1<-NULL
ID1_noPassage$t2<-NULL
ID1_noPassage$t3<-NULL
ID1_noPassage$t4<-NULL

ID1_noPassage<-ID1_noPassage[order(ID1_noPassage$no_2014,decreasing=TRUE),]



ID1_noPassage[1:30,]



par(mfrow=c(3,4))

temp<-ID1_OD[
  ID1_OD$Year== 2014 
  & ID1_OD$Entr=="00000000" 
  & ID1_OD$Sor=="25006012" ,]
hist(temp$Month,breaks=8)
hist(temp$Day,breaks=31)
hist(temp$dow,breaks=7)
hist(temp$Hour,breaks=24)

temp<-ID1_OD[
  ID1_OD$Year== 2013 
  & ID1_OD$Entr=="00000000" 
  & ID1_OD$Sor=="25006012" ,]
hist(temp$Month,breaks=8)
hist(temp$Day,breaks=31)
hist(temp$dow,breaks=7)
hist(temp$Hour,breaks=24)

temp<-ID1_OD[
  ID1_OD$Year== 2012 
  & ID1_OD$Entr=="00000000" 
  & ID1_OD$Sor=="25006012" ,]
hist(temp$Month,breaks=8)
hist(temp$Day,breaks=31)
hist(temp$dow,breaks=7)
hist(temp$Hour,breaks=24)

count(ID1_OD[ID1_OD$Entr=="00000000" 
             & ID1_OD$Sor=="25006012" ,], "Year")
