##########
##########
#General Model
##########
##########

#install packages
#install.packages('randomForest')
#install.packages('party')
library(randomForest)
library(e1071)

library(party)
library(plyr)
library(ggplot2)
library(scales)
library(knitr)
library(sm)
##########
##########
#prepare transaction history
##########
##########
#get original data
#trajets<-read.csv("TransactionsEchantillon_20150324.csv",sep=",",header=TRUE)
trajets<-read.csv("Histo_Escota&ASF.csv",sep=";",header=TRUE)
trajets[,2]<-sapply(trajets[,2],as.character)

#construct data.frame
ID<-factor(trajets$Num_MP)
Entr<-factor(trajets$Code_Gare_Entree)
Sor<-factor(trajets$Code_Gare_Sortie)

Year<-as.factor(substr(trajets[,1],7,10))
Month<-as.factor(substr(trajets[,1],4,5))
Day<-as.factor(substr(trajets[,1],1,2))

Hour<-as.factor(substr(trajets$Horodate_Sortie,12,13))
Minute<-as.factor(substr(trajets$Horodate_Sortie,15,16))

#ID_OD with necessary info
ID_OD<-data.frame(ID,Entr,Sor,Year,Month,Day,Hour,Minute)

#ID_OD$Year<-as.integer(ID_OD$Year)
ID_OD$Month<-as.integer(ID_OD$Month)
ID_OD$Day<-as.integer(ID_OD$Day)
ID_OD$Hour<-as.integer(ID_OD$Hour)
ID_OD$Minute<-as.integer(ID_OD$Minute)

ID_OD$Date<-as.Date(paste0(Year,"-",Month,"-",Day))
ID_OD$DOW<-as.POSIXlt(ID_OD$Date)$wday
ID_OD$WOY<-as.integer(strftime(as.POSIXlt(ID_OD$Date),format="%W"))

ID_OD$ID[ID_OD$ID==25004903597500004]<-25004903597500000
ggplot(ID_OD)+geom_bar(aes(x=factor(ID)))
count(ID_OD,"ID")

ID_OD$Entr<-as.character(ID_OD$Entr)
ID_OD$Entr[is.na(ID_OD$Entr)]<-0

#get ID reference
ID_ref<-read.csv("SupportsTestBigData.csv",header = TRUE,sep =",")
ID_ref[,3]<-sapply(ID_ref[,3],as.character)
ID_ref<-ID_ref[order(ID_ref$IDT_SUPO_ABNE),]

ID_ref$label<-c("VIP4","VIP1","VIP2","VIP3","VIP6","VIP5","VIP7","VIP8")
names(ID_ref)[3]<-"ID"
ID_ref<-merge(ID_ref,count(ID_OD,"ID"),by="ID")

ID_ref$Nom<-c("MD","NP","PC","FF","LF","MC","JP","FC")
temp<-ID_ref[,c(1,7)]
ID_OD<-merge(ID_OD,temp,by="ID")

##########
ID_OD_freq<-count(ID_OD,c("Nom","Entr","Sor"))
names(ID_OD_freq)[4]<-"noPsg"

no<-count(ID_OD,"Nom")
names(no)[2]<-"totalPsg"
ID_OD_freq<-merge(x=ID_OD_freq,y=no,by="Nom",all.x=TRUE)
ID_OD_freq$perPsg<-ID_OD_freq$noPsg/ID_OD_freq$totalPsg

ID_OD<-merge(x=ID_OD,y=ID_OD_freq,by=c("Nom","Entr","Sor"),all.x=TRUE)

##########
temp<-read.table("JF_2014_2015.csv")
names(temp)<-"Date"
temp$Date<-as.Date(temp$Date)
temp$JF<-1

JF<-data.frame(seq(as.Date("2014-1-1"),as.Date("2016-12-31"),"1 day"))
names(JF)<-"Date"
JF$Year<-as.numeric(format(JF$Date, "%Y"))
JF$Month<-as.numeric(format(JF$Date, "%m"))
JF$Day<-as.numeric(format(JF$Date, "%d"))

JF$DOW<-as.POSIXlt(JF$Date)$wday
JF$WOY<-as.numeric( format(JF$Date+3, "%U"))

JF<-merge(x=JF,y=temp,by="Date",all.x=TRUE)
JF[is.na(JF)]<-0
write.table(JF,"JF_2014-2015-2016.csv",sep=",",row.name=FALSE,quote=FALSE)


##########
##########
#split data into train/test set
##########
##########
#75% of sample size
smp_size<-floor(.75*nrow(hot_In_365))

#set seed (reproductible)
set.seed(0422)
train_ind<-sample(seq_len(nrow(hot_In_365)),size=smp_size)
#construct train/test set
train<-hot_In_365[train_ind,]
test<-hot_In_365[-train_ind,]

##########
##########
#Random Forest
##########
##########

fit<-randomForest(as.factor(Passage)~.,data=train,importance=TRUE,ntree=2000)
fit.rf<-randomForest(as.factor(Passage)~ Year+Month+Day+woy+dow+JF+VS+ID, data=train,importance=TRUE,ntree=2000)
pred.rf<-predict(fit.rf,test)
varImpPlot(fit.rf)

##########
##########
#SVM
##########
##########
fit.svm<-svm(as.factor(Passage)~.,data=train,cost=100,gamma=1)
pred.svm<-predict(fit.svm,test)

##########
##########
#cforest
##########
##########
fit.cf <- cforest(as.factor(Passage) ~ .,data = train, controls=cforest_unbiased(ntree=2000, mtry=3))

##########
##########
#get #_passage/jr
##########
##########
#get no_passage/jr
ID_no_passage<-count(ID_OD_all,c("ID","Year","Month","Day"))

#initiate no_passage_In_365
no_passage_In_365<-In_365
no_passage_In_365$no_passage<-0
temp_all<-subset(ID_no_passage,ID_no_passage$ID==ID_ref[1,3])
no_passage_In_365$ID<-ID_ref[1,3]

for (i in 1:nrow(no_passage_In_365)){
  for (j in 1:nrow(temp_all)){
    if(no_passage_In_365$Year[i]==temp_all$Year[j] & no_passage_In_365$Month[i]==temp_all$Month[j] & no_passage_In_365$Day[i]==temp_all$Day[j]){
      no_passage_In_365$no_passage[i]<-temp_all$freq[j]
      break
    }
  }
}

#complete no_passage_In_365
for (i in 2:nrow(ID_ref)){
  temp_In_365<-In_365
  temp_In_365$no_passage<-0
  temp_all<-subset(ID_no_passage,ID_no_passage$ID==ID_ref[i,3])
  temp_In_365$ID<-ID_ref[i,3]
  
  for (j in 1: nrow(temp_In_365)){
    for (k in 1 : nrow(temp_all)){
      if(temp_In_365$Year[j] ==temp_all$Year[k] & temp_In_365$Month[j]==temp_all$Month[k] & temp_In_365$Day[j]  ==temp_all$Day[k]) {
        temp_In_365$no_passage[j]<-temp_all$freq[k]
        break
      }
    }
  }
  no_passage_In_365<-rbind(no_passage_In_365,temp_In_365)
}  
#prepare
no_passage_In_365$Date<-as.Date(paste0(as.character(no_passage_In_365$Year),"-",as.character(no_passage_In_365$Month),"-",as.character(no_passage_In_365$Day)))
count(no_passage_In_365,"no_passage")

datebreaks <- seq(as.Date("2014-01-01"), as.Date("2015-06-01"), by="2 month")
#plot each VIP's passage
for (i in 1:nrow(ID_ref)){
  temp_all<-subset(no_passage_In_365,no_passage_In_365$ID==ID_ref[i,3])
  ggplot(temp_all, aes(Date, no_passage)) + geom_line() + 
    xlab(paste0("History_of_",ID_ref$label[i],"-",ID_ref$NOM_TIT[i])) + 
    ylab("# of Daily Passages")+scale_x_date(breaks=datebreaks) +
    scale_y_continuous(limits=c(0,5))
  ggsave(file=paste0("History_of_",ID_ref$label[i],"-",ID_ref$NOM_TIT[i],".png"))
}
datebreaks <- seq(as.Date("2014-12-22"), as.Date("2015-04-06"), by="1 week")
#plot each VIP's passage in the last 90 days 
for (i in 1:nrow(ID_ref)){
  temp_all<-subset(no_passage_In_365,no_passage_In_365$ID==ID_ref[i,3])
  n<-nrow(temp_all)
  ggplot(temp_all[(n-90):n,], aes(Date, no_passage)) + geom_line() + 
    xlab(paste0("History_of_",ID_ref$label[i],"-",ID_ref$NOM_TIT[i])) + 
    ylab("# of Daily Passages")+scale_x_date(breaks=datebreaks) +
    scale_y_continuous(limits=c(0,5))
  ggsave(file=paste0("History_of_",ID_ref$label[i],"-",ID_ref$NOM_TIT[i],"_last_30_days.png"))
}


