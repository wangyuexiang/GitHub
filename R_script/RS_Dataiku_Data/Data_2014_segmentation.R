##########
##########
# Data, Data_2014, Segmentation
##########
##########
Data<-ID_OD

Data$Dat_Entr<-NULL
Data$Dat_Sor<-NULL
#Data$Tar_TCC<-NULL
#Data$KMS<-NULL
Data$Cl<-NULL

Data$Date<-as.Date(paste0(Data$Year,"-",Data$Month,"-",Data$Day))
Data$ID<-as.character(Data$ID)

Data$dow<-as.POSIXlt(Data$Date)$wday
Data$woy<-as.numeric( format(Data$Date+3, "%U"))

Data_2014<-Data[Data$Date>=as.Date("2014-1-1"),]
#Data_2014<-Data[Data$Year==2014,]

ID_OD_Year_freq<-count(Data,c("ID","Entr","Sor","Year"))
names(ID_OD_Year_freq)[5]<-"no"

#Data_avec_parking<-Data

##########
##########
# #_Trx/Year
##########
##########

freq_2014<-count(ID_OD_Year_freq[ID_OD_Year_freq$Year==2014,],"no")
no_2014<-count(Data[Data$Year==2014,],"ID")
no_2013<-count(Data[Data$Year==2013,],"ID")
no_2012<-count(Data[Data$Year==2012,],"ID")
no_2011<-count(Data[Data$Year==2011,],"ID")
no_2010<-count(Data[Data$Year==2010,],"ID")

names(no_2014)[2]<-"freq_2014"
names(no_2013)[2]<-"freq_2013"
names(no_2012)[2]<-"freq_2012"
names(no_2011)[2]<-"freq_2011"
names(no_2010)[2]<-"freq_2010"

noPassage<-merge(no_2014,no_2013,by="ID",all = TRUE)
noPassage<-merge(noPassage,no_2012,by="ID",all = TRUE)
noPassage<-merge(noPassage,no_2011,by="ID",all = TRUE)
noPassage<-merge(noPassage,no_2010,by="ID",all = TRUE)

noPassage_NA<-noPassage

noPassage[is.na(noPassage)]<-0
noPassage$Total<-noPassage$freq_2014+
  noPassage$freq_2013+
  noPassage$freq_2012+
  noPassage$freq_2011+
  noPassage$freq_2010

##########
##########
# #_Trx/dow
##########
##########
temp<-count(Data_2014,c("ID","dow"))

no_0<-temp[temp$dow==0,c(1,3)]
no_1<-temp[temp$dow==1,c(1,3)]
no_2<-temp[temp$dow==2,c(1,3)]
no_3<-temp[temp$dow==3,c(1,3)]
no_4<-temp[temp$dow==4,c(1,3)]
no_5<-temp[temp$dow==5,c(1,3)]
no_6<-temp[temp$dow==6,c(1,3)]

names(no_0)[2]<-"no_0"
names(no_1)[2]<-"no_1"
names(no_2)[2]<-"no_2"
names(no_3)[2]<-"no_3"
names(no_4)[2]<-"no_4"
names(no_5)[2]<-"no_5"
names(no_6)[2]<-"no_6"

noPassage<-merge(noPassage,no_0,by="ID",all = TRUE)
noPassage<-merge(noPassage,no_1,by="ID",all = TRUE)
noPassage<-merge(noPassage,no_2,by="ID",all = TRUE)
noPassage<-merge(noPassage,no_3,by="ID",all = TRUE)
noPassage<-merge(noPassage,no_4,by="ID",all = TRUE)
noPassage<-merge(noPassage,no_5,by="ID",all = TRUE)
noPassage<-merge(noPassage,no_6,by="ID",all = TRUE)

# noPassage$T2014<-noPassage$no_0+
#   noPassage$no_1+
#   noPassage$no_2+
#   noPassage$no_3+
#   noPassage$no_4+
#   noPassage$no_5+
#   noPassage$no_6


##########
##########
# #_Trx/ID_OD
##########
##########
temp<-count(Data_2014,c("ID","Entr","Sor"))
names(temp)[4]<-"no"
Data_2014_freq<-temp

temp<-count(Data_2014_freq[Data_2014_freq$no>100,],"ID")
names(temp)[2]<-"OD_100"
noPassage<-merge(noPassage,temp,by="ID",all=TRUE)

temp<-count(Data_2014_freq[Data_2014_freq$no>50 & Data_2014_freq$no<=100,],"ID")
names(temp)[2]<-"OD_50"
noPassage<-merge(noPassage,temp,by="ID",all=TRUE)

temp<-count(Data_2014_freq[Data_2014_freq$no>10 & Data_2014_freq$no<=50,],"ID")
names(temp)[2]<-"OD_10"
noPassage<-merge(noPassage,temp,by="ID",all=TRUE)

temp<-count(Data_2014_freq[Data_2014_freq$no>2 & Data_2014_freq$no<=10,],"ID")
names(temp)[2]<-"OD_3"
noPassage<-merge(noPassage,temp,by="ID",all=TRUE)

temp<-count(Data_2014_freq[Data_2014_freq$no<=2,],"ID")
names(temp)[2]<-"OD_1"
noPassage<-merge(noPassage,temp,by="ID",all=TRUE)

temp<-Data_2014_freq[Data_2014_freq$no>100,]
temp1<-aggregate(temp[,4],temp["ID"],FUN=sum)
names(temp1)[2]<-"Trx_100"
noPassage<-merge(noPassage,temp1,by="ID",all=TRUE)

temp<-Data_2014_freq[Data_2014_freq$no>50 & Data_2014_freq$no<=100,]
temp1<-aggregate(temp[,4],temp["ID"],FUN=sum)
names(temp1)[2]<-"Trx_50"
noPassage<-merge(noPassage,temp1,by="ID",all=TRUE)

noPassage[is.na(noPassage)]<-0
noPassage<-noPassage[order(noPassage$freq_2014,decreasing=TRUE),]
