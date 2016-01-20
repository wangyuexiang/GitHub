##########
# Test
##########
library(plyr)
library(ggplot2)
library(gridExtra)
library(knitr)
library(scales)

#library(party)
#library(sm)
#library(hexbin)
##########
##########
t1<-ID_ref$Nom
rm(MD,NP,PC,FF,LF,MC,JP,FC)
rm(NP_2014)
##########

ID_OD$ML<-0
ID_OD$ML[ID_OD$noPsg>50]<-1

ML_sans_J1<-read.table("sans_J1.csv",sep=",",header=TRUE)
ML_avec_J1<-read.table("avec_J1.csv",sep=",",header=TRUE)

ML_sans_J1$Date<-as.Date(paste0(ML_sans_J1$Year,"-",ML_sans_J1$Month,"-",ML_sans_J1$Day))
ML_sans_J1$DOW<-
##########
d1<-as.Date("2014-1-1")
d2<-as.Date("2015-5-10")
#t<-In_365[In_365$Date<=d2,]
t<-JF[JF$Date<=d2,]


t_DOW<-count(t,"DOW")
names(t_DOW)[2]<-"Total"

t <- t[t$JF == 0, ]
t_D <- count(t, DOW) 
names(t_D)[2]<-"Total"
##########
ID_OD_freq<-ID_OD_freq[order(ID_OD_freq$noPsg,decreasing=TRUE),]
ID_OD_DH_fake<-count(ID_OD,c("Nom","Entr","Sor","DOW","Hour"))
ID_OD_DH_fake<-ID_OD_DH_fake[order(ID_OD_DH_fake$freq,decreasing=TRUE),]

ID_OD_Date_DH<-count(ID_OD,c("Nom","Entr","Sor","Date","DOW","Hour"))
temp<-ID_OD_Date_DH
temp$freq<-NULL
ID_OD_DH<-count(temp,c("Nom","Entr","Sor","DOW","Hour"))
ID_OD_DH<-ID_OD_DH[order(ID_OD_DH$freq,decreasing=TRUE),]
##########
result<-merge(ID_OD_DH,t_DOW,by="DOW")
result$Per<-result$freq/result$Total
result<-result[order(result$Per,decreasing=TRUE),]

##########
temp<-result[,c(1:5,8)]
temp2<-merge(ID_OD,temp,by=c("Nom","Entr","Sor","DOW","Hour"))
ggplot(ID_OD,aes(Hour))+geom_bar(binwidth=1)
#ggplot(temp2,aes(Hour,fill=as.factor(Per)))+geom_bar(binwidth=1)

temp2$Predicted<-0
temp2$Predicted[temp2$Per>=.5]<-.5
temp2$Predicted[temp2$Per< .5 & temp2$Per>=.3]<-.3
temp2$Predicted[temp2$Per< .3 & temp2$Per>=.1]<-.1
ID_OD<-temp2

ggplot(temp2,aes(Hour,fill=as.factor(Predicted)))+geom_bar(binwidth=1)
ggplot(temp2,aes(Hour,fill=as.factor(Predicted)))+geom_bar(binwidth=1)+facet_wrap(~Nom, ncol=2)

temp3<-temp2[temp2$Nom=="NP",]
ggplot(temp3,aes(WOY,DOW,fill=as.factor(Predicted)))+geom_tile()
ggplot(temp3[temp3$noPsg>10,],aes(WOY,DOW,fill=as.factor(Predicted)))+geom_tile()+facet_wrap(~Sor,ncol=2)
ggplot(temp3[temp3$noPsg>10,],aes(Hour,DOW,fill=as.factor(Predicted)))+geom_tile()+facet_wrap(~Sor,ncol=2)






##########
temp<-count(ID_OD[ID_OD$Hour %in% periodAller & ID_OD$DOW %in% jrOuvert,],c("Nom","Entr","Sor"))
temp[temp$freq>seuil,]
temp<-temp[order(temp$freq,decreasing=TRUE),]
##########


temp<-PC[PC$Year==2014,]

temp1<-count(temp,c("Entr","Sor"))
temp1<-temp1[order(temp1$freq,decreasing=TRUE),]
temp2<-count(temp,c("Entr","Sor","Date"))
temp2<-temp2[order(temp2$freq,decreasing=TRUE),]
temp3<-count(temp,c("Sor","Date"))
temp3<-temp3[order(temp3$freq,decreasing=TRUE),]

head(temp1)

temp<-ID_OD[ID_OD$Year==2014,]
temp1<-count(temp,c("Nom","Entr","Sor"))
temp1<-temp1[order(temp1$freq,decreasing=TRUE),]

ggplot(NP[NP$Year==2014 & NP$noPsg>10,],aes(x=WOY,y=DOW))+geom_tile()+facet_wrap(~Sor)




















ggplot(ID_OD,aes(Hour,fill=(as.factor(DOW)))) +  geom_bar(binwidth=1) +  ggtitle("VIPs: #/ (H) ") +  facet_wrap(~Nom, ncol=2)
ggplot(ID_OD,aes(DOW,Hour,color=Sor))+geom_point(alpha=0.04)+facet_wrap(~Nom)


seuil=10
i=4
nrow(ID_OD[ID_OD$Nom==ID_ref$Nom[i],])
nrow(ID_OD[ID_OD$Nom==ID_ref$Nom[i] & ID_OD$Hour %in% periodAller & ID_OD$DOW %in% jrOuvert,])
count(ID_OD[ID_OD$Nom==ID_ref$Nom[i] & ID_OD$noPsg>seuil & ID_OD$Hour %in% periodAller & ID_OD$DOW %in% jrOuvert,],c("Nom","Entr","Sor"))
count(ID_OD[ID_OD$Nom==ID_ref$Nom[i] & ID_OD$noPsg>seuil & ID_OD$Hour %in% periodRetour & ID_OD$DOW %in% jrOuvert,],c("Nom","Entr","Sor"))

##########
# FF
i<-4
temp_Aller<-ID_OD[ID_OD$Nom==ID_ref$Nom[i] & ID_OD$Entr==25004220 & ID_OD$Sor==25004211,]
p1<-ggplot(temp_Aller,aes(Hour,fill=as.factor(DOW)))+geom_bar(binwidth=1)
p2<-ggplot(temp_Aller[temp_Aller$Year==2014 & temp_Aller$DOW %in% jrOuvert,],aes(WOY,fill=as.factor(DOW)))+geom_bar(binwidth=1)
grid.arrange(p1, p2, ncol=1,main =  paste0(temp_Aller[1,15],":",temp_Aller[1,2],"-",temp_Aller[1,3]))

temp_Retour<-ID_OD[ID_OD$Nom==ID_ref$Nom[i] & ID_OD$Entr==25004211 & ID_OD$Sor==25004220,]
p1<-ggplot(temp_Retour,aes(Hour,fill=as.factor(DOW)))+geom_bar(binwidth=1)
p2<-ggplot(temp_Retour[temp_Retour$Year==2014 & temp_Retour$DOW %in% jrOuvert,],aes(WOY,fill=as.factor(DOW)))+geom_bar(binwidth=1)
grid.arrange(p1, p2, ncol=1,main = paste0(temp_Retour[1,15],":",temp_Retour[1,2],"-",temp_Retour[1,3]))

temp_Aller<-ID_OD[ID_OD$Nom==ID_ref$Nom[i] & ID_OD$Entr==25006049 & ID_OD$Sor==25006002,]
temp_Retour<-ID_OD[ID_OD$Nom==ID_ref$Nom[i] & ID_OD$Entr==25006002 & ID_OD$Sor==25006049,]
# S.O.
temp_Aller<-ID_OD[ID_OD$Nom==ID_ref$Nom[i] & ID_OD$Sor==25006010 & ID_OD$Hour<12,]
temp_Retour<-ID_OD[ID_OD$Nom==ID_ref$Nom[i] & ID_OD$Sor==25006010 & ID_OD$Hour>12,]

##########
# MD _ 2015
i<-1
temp_Aller<-ID_OD[ID_OD$Nom==ID_ref$Nom[i] & ID_OD$Entr==25004209 & ID_OD$Sor==25004211,]
temp_Retour<-ID_OD[ID_OD$Nom==ID_ref$Nom[i] & ID_OD$Entr==25004211 & ID_OD$Sor==25004209,]

temp_Aller<-ID_OD[ID_OD$Nom==ID_ref$Nom[i] & ID_OD$Entr==25004209 & ID_OD$Sor==25004211,]
p1<-ggplot(temp_Aller,aes(Hour,fill=as.factor(DOW)))+geom_bar(binwidth=1)
p2<-ggplot(temp_Aller[temp_Aller$Year==2015 & temp_Aller$DOW %in% jrOuvert,],aes(WOY,fill=as.factor(DOW)))+geom_bar(binwidth=1)
grid.arrange(p1, p2, ncol=1,main =  paste0(temp_Aller[1,15],":",temp_Aller[1,2],"-",temp_Aller[1,3]))

temp_Retour<-ID_OD[ID_OD$Nom==ID_ref$Nom[i] & ID_OD$Entr==25004211 & ID_OD$Sor==25004209,]
p1<-ggplot(temp_Retour,aes(Hour,fill=as.factor(DOW)))+geom_bar(binwidth=1)
p2<-ggplot(temp_Retour[temp_Retour$Year==2015 & temp_Retour$DOW %in% jrOuvert,],aes(WOY,fill=as.factor(DOW)))+geom_bar(binwidth=1)
grid.arrange(p1, p2, ncol=1,main = paste0(temp_Retour[1,15],":",temp_Retour[1,2],"-",temp_Retour[1,3]))

##########
# NP + PC
i<-2
temp_AR<-ID_OD[ID_OD$Nom==ID_ref$Nom[i] & 
                 ((ID_OD$Entr==25004220 & ID_OD$Sor==25004211)|
                    (ID_OD$Entr==25004211 & ID_OD$Sor==25004220)),]
ggplot(temp_AR,aes(Hour,fill=as.factor(DOW)))+geom_bar(binwidth=1)  
ggplot(temp_AR[temp_AR$Year==2014 & temp_AR$DOW %in% jrOuvert,],aes(WOY,fill=as.factor(DOW)))+geom_bar(binwidth=1)

##########
# PC
# S.O.
i=3
temp_Aller<-ID_OD[ID_OD$Nom==ID_ref$Nom[i] & ID_OD$Sor==25006001 & ID_OD$Hour<12,]
temp_Retour<-ID_OD[ID_OD$Nom==ID_ref$Nom[i] & ID_OD$Sor==25006001 & ID_OD$Hour>12,]

##########
# MC
i=6
temp_Aller<-ID_OD[ID_OD$Nom==ID_ref$Nom[i] & ID_OD$Entr==25004220 & ID_OD$Sor==25004211,]
temp_Retour<-ID_OD[ID_OD$Nom==ID_ref$Nom[i] & ID_OD$Entr==25004211 & ID_OD$Sor==25004220,]


##########
# MC
temp_Aller<-ID_OD[ID_OD$Nom==ID_ref$Nom[i] & ID_OD$Entr==25006002 & ID_OD$Sor==25006009,]
temp_Retour<-ID_OD[ID_OD$Nom==ID_ref$Nom[i] & ID_OD$Entr==25006009 & ID_OD$Sor==25006002,]

##########
# MD
i<-7
temp_Aller<-ID_OD[ID_OD$Nom==ID_ref$Nom[i] & ID_OD$Entr==25006042 & ID_OD$Sor==25006009,]
temp_Retour<-ID_OD[ID_OD$Nom==ID_ref$Nom[i] & ID_OD$Entr==25006009 & ID_OD$Sor==25006042,]

##########
##########
# LF
i<-5
temp_Aller<-ID_OD[ID_OD$Nom==ID_ref$Nom[i] & ID_OD$Entr==25006009 & ID_OD$Sor==25006009,]
