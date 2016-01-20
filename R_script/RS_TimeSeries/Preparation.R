##########
##########
# Preparation
##########
##########

library(plyr)
library(ggplot2)
library(scales)
library(gridExtra)
library(knitr)
library(sm)


List<-List[order(List$freq_2014,decreasing=TRUE),]
List$Semaine<-List$no_1+List$no_2+List$no_3+List$no_4+List$no_5
List$SemaineTaux<-List$Semaine/List$freq_2014

ID_OD_freq<-count(ID_OD,c("ID","Entr","Sor"))
names(ID_OD_freq)[4]<-"noPsg"
noPsg_freq<-count(ID_OD_freq,"noPsg")
noPsg_freq$Trx<-noPsg_freq$noPsg*noPsg_freq$freq

##########
no<-count(ID_OD,"ID")
names(no)[2]<-"totalPsg"
ID_OD_freq<-merge(x=ID_OD_freq,y=no,by="ID",all.x=TRUE)
ID_OD_freq$perPsg<-ID_OD_freq$noPsg/ID_OD_freq$totalPsg

ID_OD<-merge(x=ID_OD,y=ID_OD_freq,by=c("ID","Entr","Sor"),all.x=TRUE)
##########
ID_OD_2014<-ID_OD[ID_OD$Year==2014,]

ID_OD_2014$noPsg<-NULL
ID_OD_2014$totalPsg<-NULL
ID_OD_2014$perPsg<-NULL  

ID_OD_freq_2014<-count(ID_OD_2014,c("ID","Entr","Sor"))
names(ID_OD_freq_2014)[4]<-"noPsg"

no_2014<-count(ID_OD_2014,"ID")
names(no_2014)[2]<-"totalPsg"
ID_OD_freq_2014<-merge(x=ID_OD_freq_2014,y=no_2014,by="ID",all.x=TRUE)
ID_OD_freq_2014$perPsg<-ID_OD_freq_2014$noPsg/ID_OD_freq_2014$totalPsg

ID_OD_2014<-merge(x=ID_OD_2014,y=ID_OD_freq_2014,by=c("ID","Entr","Sor"),all.x=TRUE)
rm(no_2014)

##########
List$Label<-seq(1:5000)
temp<-List[,c(1,25)]
ID_OD_2014<-merge(ID_OD_2014,temp,by="ID")

ID_OD<-merge(ID_OD,temp,by="ID")
##########
names(ID_OD)[12:13]<-c("DOW","WOY")
names(ID_OD_2014)[12:13]<-c("DOW","WOY")
##########
JF<-read.table("JF_2014-2015-2016.csv",sep=",",head=TRUE)
JF$Date<-as.Date(JF$Date)
##########
gares_temp<-gares
gares<-read.table("garesLatLng.csv",head=TRUE,sep=";")

#SNCF<-read.table("SNCF.csv",head=TRUE,sep=";")
##########
d1<-as.Date("2014-1-1")
d2<-as.Date("2014-8-31")
t<-JF[JF$Date<=d2,]

t_DOW<-count(t,"DOW")
names(t_DOW)[2]<-"Total"
##########
#ID_OD_freq_2014_ALL<-count(ID_OD_2014,c("Label","Entr","Sor","Tarif","KMS","noPsg","totalPsg","perPsg"))
#ID_OD_freq_2014_Part<-count(ID_OD_2014,c("Label","Entr","Sor","KMS","noPsg","totalPsg","perPsg"))
#rm(ID_OD_freq_2014_ALL,ID_OD_freq_2014_Part)
ID_OD_freq_2014<-count(ID_OD_2014,c("Label","Entr","Sor"))
ID_OD_freq_2014<-ID_OD_freq_2014[order(ID_OD_freq_2014$freq,decreasing=TRUE),]
##########
ID_OD_2014_DH<-count(ID_OD_2014,c("Label","Entr","Sor","DOW","Hour"))
ID_OD_2014_DH<-ID_OD_2014_DH[order(ID_OD_2014_DH$freq,decreasing=TRUE),]
ID_OD_2014_DH_fake<-ID_OD_2014_DH

result_fake<-merge(ID_OD_2014_DH_fake,t_DOW,by="DOW")
result_fake$Per<-result_fake$freq/result_fake$Total
result_fake<-result_fake[order(result_fake$Per,decreasing=TRUE),]
nrow(result_fake[result_fake$Per>=.1,])
##########
ID_OD_2014_Date_DH<-count(ID_OD_2014,c("Label","Entr","Sor","Date","DOW","Hour","totalPsg"))
temp<-ID_OD_2014_Date_DH
temp$freq<-NULL
ID_OD_2014_DH<-count(temp,c("Label","Entr","Sor","DOW","Hour","totalPsg"))
ID_OD_2014_DH<-ID_OD_2014_DH[order(ID_OD_2014_DH$freq,decreasing=TRUE),]

result<-merge(ID_OD_2014_DH,t_DOW,by="DOW")
result$Per<-result$freq/result$Total
result<-result[order(result$Per,decreasing=TRUE),]
nrow(result[result$Per>=.1,])
##########
temp<-result[,c(1:5,9)]
temp2<-merge(ID_OD_2014,temp,by=c("Label","Entr","Sor","DOW","Hour"))

ggplot(temp2,aes(Hour))+geom_bar(binwidth=1)

temp2$Predicted<-0
temp2$Predicted[temp2$Per>=.5]<-.5
temp2$Predicted[temp2$Per< .5 & temp2$Per>=.3]<-.3
temp2$Predicted[temp2$Per< .3 & temp2$Per>=.1]<-.1
ggplot(temp2,aes(Hour,fill=as.factor(Predicted)))+geom_bar(binwidth=1)

ID_OD_2014<-temp2
##########

##########
List_old<-List

List$freq_2013<-NULL
List$freq_2012<-NULL
List$freq_2011<-NULL
List$freq_2010<-NULL
List$Total<-NULL
names(List)[3]<-"totalPsg"

t_5<-temp[temp$Predicted==.5,c(1,3)]
t_3<-temp[temp$Predicted==.3,c(1,3)]
t_1<-temp[temp$Predicted==.1,c(1,3)]
t_0<-temp[temp$Predicted==.0,c(1,3)]

names(t_5)[2]<-"noP.5"
names(t_3)[2]<-"noP.3"
names(t_1)[2]<-"noP.1"
names(t_0)[2]<-"noP.0"

# List<-merge(x=List,y=t_5,by="Label",all.x=TRUE)
# List<-merge(x=List,y=t_3,by="Label",all.x=TRUE)
# List<-merge(x=List,y=t_1,by="Label",all.x=TRUE)
# List<-merge(x=List,y=t_0,by="Label",all.x=TRUE)

List[is.na(List)]<-0

List$Pred.5<-format(round(List$noP.5/List$totalPsg,3), nsmall=3)
List$Pred.3<-format(round(List$noP.3/List$totalPsg,3), nsmall=3)
List$Pred.1<-format(round(List$noP.1/List$totalPsg,3), nsmall=3)
List$Pred.0<-format(round(List$noP.0/List$totalPsg,3), nsmall=3)

List$Pred.5<-as.numeric(List$Pred.5)
List$Pred.3<-as.numeric(List$Pred.3)
List$Pred.1<-as.numeric(List$Pred.1)
List$Pred.0<-as.numeric(List$Pred.0)

ggplot(List[List$Label<100,],aes(x=Label))+
  geom_line(aes(y=Pred.5,color="P.5")) +
  geom_line(aes(y=Pred.3,color="P.3")) +
  geom_line(aes(y=Pred.1,color="P.1")) +
  geom_line(aes(y=Pred.0,color="P.0")) 


ggplot(List,aes(Pred.5))+geom_bar(binwidth=.1)
ggplot(List,aes(Pred.3))+geom_bar(binwidth=.1)
ggplot(List,aes(Pred.1))+geom_bar(binwidth=.1)
ggplot(List,aes(Pred.0))+geom_bar(binwidth=.1)


nrow(List[List$Pred.5>.5,])

nrow(temp2)
temp<-count(ID_OD_2014,"Predicted")
temp$Percentage<-temp$freq/1953411
##########
# 20150618
##########
gares_geo<-gares[,c(1:4,6,8,7)]

geo<-gares_geo[,c(4,6,7)]
geo<-geo[geo$Cde>25000000,]

names(geo)<-c("Entr","Elng","Elat")
ID_OD_2014<-merge(x=ID_OD_2014,y=geo,by="Entr",all.x=TRUE)
names(geo)<-c("Sor","Slng","Slat")
ID_OD_2014<-merge(x=ID_OD_2014,y=geo,by="Sor",all.x=TRUE)

Sor_SO<-unique(ID_OD_2014[ID_OD_2014$Entr=="00000000",]$Sor)
Sor_SO<-data.frame(Sor_SO)

gare_SO<-gares[gares$Cde %in% Sor_SO$Sor_SO,]
write.table(gare_SO,file="gare_SO.csv",sep=",",row.name=FALSE,quote=FALSE)

##########
# 20150706
##########
ID_2014_Date_DH<-count(ID_OD_2014,c("Label","Date","DOW","Hour","totalPsg"))
temp<-ID_2014_Date_DH
temp$freq<-NULL
ID_2014_DH<-count(temp,c("Label","DOW","Hour","totalPsg"))
ID_2014_DH<-ID_2014_DH[order(ID_2014_DH$freq,decreasing=TRUE),]

result<-merge(ID_2014_DH,t_DOW,by="DOW")
result$Per<-result$freq/result$Total
result<-result[order(result$Per,decreasing=TRUE),]
nrow(result[result$Per>=.1,])

names(result)[7] <- "PD"

temp<-result[,c(1:3,7)]
temp2<-merge(ID_OD_2014,temp,by=c("Label","DOW","Hour"))
ID_2014 <- temp2
rm(temp2)

ggplot(ID_2014,aes(Hour))+geom_bar(binwidth=1)


ID_2014$PDF <- NA
ID_2014[ID_2014$PD > .5, ]$PDF <- .5
ID_2014[ID_2014$PD > .3 & ID_2014$PD <= .5, ]$PDF <- .3
ID_2014[ID_2014$PD > .1 & ID_2014$PD <= .3, ]$PDF <- .1
ID_2014[ID_2014$PD <= .1, ]$PDF <- 0


# ggplot(ID_2014) + geom_bar(aes(PDF))
# ggplot(ID_2014) + geom_bar(aes(PD))


ggplot(ID_2014) + 
  geom_density(aes(PD, col = "PD")) +
  geom_density(aes(Per, col = "Per"))

PDF <- count(ID_2014,"PDF")
PDF$Pourcentage <- PDF$freq / 1953411

temp <- ID_2014[ID_2014$Label == 1, ]
ggplot(temp) +  
  geom_segment(aes(x = Elng, xend = Slng, y = Elat, yend = Slat,
                   #size = totalPsg,
                   col = as.factor(PDF)),
               alpha = .5)
