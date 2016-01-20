library(ggplot2)
library(knitr)
library(plyr)
library(scales)
library(gridExtra)
library(reshape2)

library(ggmap)

#library(sm)
#library(hexbin)
##########

seuil=10
pAtemp<-c(6:8)
pRtemp<-c(16:19)
jOtemp<-c(1:5)

# rm(tA_Entr,tA_Sor,tRetour_freq)
# rm(tR_Entr,tR_Sor,tAller_freq)
# rm(temp_Aller,temp_Retour)
# rm(j,k)
# rm(ID4_OD,ID_OD_freq_test)
# rm(noAller,noMilieu,noRetour)
# rm(ID2_OD,ID2_OD_freq,ID3_OD,ID3_OD_freq)
# rm(seuil,useful)
# rm(ID2,ID3,ID4)
# rm(periodAller,periodMilieu,periodRetour)
# rm(p1,p2,p3,p4,p5,p6,p7,p8)
##########
List[seq(1,5000,by=100),c(2,25)]

##########
# dist
##########
temp<-count(ID_OD,c("Entr","Sor","KMS"))
temp<-temp[order(temp$freq,decreasing=TRUE),]
head(temp)

gare_dist<-temp
# get society code
gare_dist$Soc<-substr(gare_dist$Sor,4,5)

gare_dist<-gare_dist[gare_dist$Soc %in% c("04","05","06"),]
gare_dist$Entr<-as.numeric(as.character(gare_dist$Entr))
gare_dist$Sor<-as.numeric(as.character(gare_dist$Sor))


nrow(gare_dist[gare_dist$Entr<=25004220 & gare_dist$Entr>=25004210 & gare_dist$Sor <=25004220 & gare_dist$Sor >=25004210 ,])

temp<-
  gare_dist[gare_dist$Entr<25004300 &
          gare_dist$Entr>25004200 &
          gare_dist$Sor <25004300 &
          gare_dist$Sor >25004200 ,
             ]

temp$KMS<-abs(temp$KMS)
temp<-temp[temp$KMS>0,]
temp<-temp[,1:4]
temp<-count(temp,c("Entr","Sor","KMS"))
temp1<-temp[,1:3]
ggplot(temp1,aes(KMS))+geom_bar()
t1<-dcast(temp1, Entr~Sor, value.var="KMS")

##########
ggplot(ID_OD_2014,aes(Hour,fill=as.factor(Predicted)))+geom_bar(binwidth=1)

temp1<-count(ID_OD_2014[ID_OD_2014$Per>.3,],"Label")

i<-List[List$Pred.5>.8,]$Label
temp_i<-temp2[temp2$Label %in% i,]
test<-List[List$OD_100==2&List$P_Trx_100>.8,]
##########
# segmentation
##########
c(2,8:25)

head(List[List$OD_100==2 & List$OD_50==0,c(2,8:25)])
head(List[List$OD_100==2 & List$P_Trx_100>.5,c(2,8:25)])

nrow(List[List$OD_100==2 & List$OD_50==0,c(2,8:25)])
nrow(List[List$OD_100==2 & List$P_Trx_100>.5,c(2,8:25)])

nrow(List[List$OD_100==2 & List$P_Trx_100<=.5,c(2,8:25)])
nrow(List[List$OD_100==2 & List$OD_50 != 0 & List$P_Trx_100>.5 ,c(2,8:25)])

temp<-List[List$OD_100==2 & List$OD_50==0,c(2,8:25)]
temp<-List[List$OD_100==2 & List$P_Trx_100>.5,c(2,8:25)]

temp<-List[List$OD_100==2 & List$P_Trx_100<=.5,c(2,8:25)]
temp<-List[List$OD_100==2 & List$OD_50 != 0 & List$P_Trx_100>.5 ,c(2,8:25)]

i= 1
temp<-ID_OD_2014[ID_OD_2014$Label==i,]
temp1<-count(temp,c("Entr","Sor"))
p1<-ggplot(temp,aes(Hour))+geom_density()
p2<-ggplot(temp,aes(x=Hour,fill=as.factor(DOW)))+geom_bar(aes(y = (..count..)/sum(..count..)),binwidth=1)+guides(fill=FALSE)+xlim(c(0,24))+ylab("")
p3<-ggplot(temp,aes(DOW))+geom_density()
p4<-ggplot(temp,aes(DOW,fill=as.factor(Hour)))+geom_bar(aes(y = (..count..)/sum(..count..)),binwidth=1)+guides(fill=FALSE)+ylab("")
p5<-ggplot(temp,aes(WOY))+geom_density()
#p6<-ggplot(temp,aes(WOY),fill=as.factor(DOW))+geom_bar(aes(y = (..count..)/sum(..count..)),binwidth=1)+ylab("")
p6<-ggplot(temp,aes(KMS))+geom_density()
p7<-ggplot(temp,aes(perPsg))+geom_density()
p8<-ggplot(temp,aes(perPsg,fill=as.factor(DOW)))+geom_bar(aes(y = (..count..)/sum(..count..)),binwidth=.02)+guides(fill=FALSE)+ylab("")
grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8, ncol=2)

#grid.arrange(p1, p2,p3,p4, ncol=1)

ggplot(temp,aes(DOW,Hour,color=Sor))+geom_point(alpha=.1)
ggplot(temp,aes(DOW,Hour,color=noPsg))+geom_point(alpha=.1)

count(temp[temp$noPsg>50,],"noPsg")

ggplot(temp[temp$noPsg==352,],aes(dow,Hour))+geom_point(alpha=.1)
ggplot(temp[temp$noPsg==329,],aes(dow,Hour))+geom_point(alpha=.1)
ggplot(temp[temp$noPsg==342,],aes(dow,Hour))+geom_point(alpha=.1)

temp1<-temp1[order(temp1$freq,decreasing=TRUE),]
temp2<-count(temp,c("Entr","Sor","Date"))
temp2<-temp2[order(temp2$freq,decreasing=TRUE),]
temp3<-count(temp,c("Sor","Date"))
temp3<-temp3[order(temp3$freq,decreasing=TRUE),]

ggplot(temp,aes(Date))+geom_bar(binwidth=1)+scale_x_date()

head(temp1)

ggplot(temp,aes(dow,Hour))+geom_point(alpha=.1)

qplot(temp$Sor)
qplot(temp3$freq)
qplot(temp3[temp3$freq>2,]$Sor)

temp4<-temp[temp$Entr== 25004213 & temp$Sor==25004220,]
ggplot(temp4,aes(woy,dow))+geom_tile()

ggplot(temp[temp$Entr== 25004210 & temp$Sor==25004218,],aes(Date,fill=as.factor(Hour)))+geom_bar(binwidth=1)+scale_x_date()
ggplot(temp[temp$Month==1 & ((temp$Entr== 25004210 & temp$Sor==25004218)|
ggplot(temp[ ((temp$Entr== 25004210 & temp$Sor==25004218)|
           (temp$Entr== 25004218 & temp$Sor==25004210)),],aes(Date,fill=as.factor(Sor)))+geom_bar(binwidth=1)
                             

ggplot(temp[temp$Entr== 25004213 & temp$Sor==25004211,],aes(Hour,fill=as.factor(dow)))+geom_bar(binwidth=1)
ggplot(temp[temp$Entr== 25004211 & temp$Sor==25004213,],aes(Hour,fill=as.factor(dow)))+geom_bar(binwidth=1)




i<-1:20

test<-List[List$OD_100==2&List$P_Trx_100>.8,]
test[601:640,c(2,15:25)]

i<-test$Label[601:640]
i<-test$Label[1:40]

temp<-ID_OD_2014[ID_OD_2014$Label %in% i,]
ggplot(temp,aes(Hour))+geom_bar(binwidth=1)+facet_wrap(~Label) +  xlim(0,25)

ggplot(temp,aes(dow,Hour))+geom_point(alpha=.1)+facet_wrap(~Label)

ggplot(temp,aes(KMS))+geom_bar(binwidth=5)+facet_wrap(~Label) +  xlim(0,80)
ggplot(temp,aes(KMS))+geom_density()+facet_wrap(~Label) +  xlim(0,80)





ggplot(temp[(temp$Entr== 25004211 & temp$Sor==25004210) |
              (temp$Entr== 25004210 & temp$Sor==25004211) ,]) +geom_bar(aes(Hour,fill=as.factor(dow)),binwidth=1) + xlim(c(0,24))
ggplot(temp[temp$Entr== 25004211 & temp$Sor==25004210,]) +
            geom_bar(aes(woy,fill=as.factor(dow)),binwidth=1) + xlim(c(0,52))

ggplot(List[seq(1,5000,by=100),],aes(x=Label)) +
         geom_line(aes(y=no_0,color="0"))+
         geom_line(aes(y=no_1,color="1"))+
         geom_line(aes(y=no_2,color="2"))+
         geom_line(aes(y=no_3,color="3"))+
         geom_line(aes(y=no_4,color="4"))+
         geom_line(aes(y=no_5,color="5"))+
         geom_line(aes(y=no_6,color="6"))






































##########
tAller_freq<-count(temp[temp$noPsg>seuil & temp$Hour %in% pAtemp & temp$dow %in% jOtemp,],c("Entr","Sor"))
tAller_freq<-tAller_freq[order(tAller_freq$freq,decreasing=TRUE),]
head(tAller_freq)

tRetour_freq<-count(temp[temp$noPsg>seuil & temp$Hour %in% pRtemp & temp$dow %in% jOtemp,],c("Entr","Sor"))
tRetour_freq<-tRetour_freq[order(tRetour_freq$freq,decreasing=TRUE),]
head(tRetour_freq)
##########
tA_Entr<-count(temp[temp$noPsg>seuil & temp$Hour %in% pAtemp & temp$dow %in% jOtemp,],c("Entr"))
tA_Entr<-tA_Entr[order(tA_Entr$freq,decreasing=TRUE),]
head(tA_Entr)

tA_Sor<-count(temp[temp$noPsg>seuil & temp$Hour %in% pAtemp & temp$dow %in% jOtemp,],c("Sor"))
tA_Sor<-tA_Sor[order(tA_Sor$freq,decreasing=TRUE),]
head(tA_Sor)

##########
temp_Aller<-temp[temp$noPsg>seuil & temp$Hour %in% pAtemp & temp$dow %in% jOtemp,]
p1<-ggplot(temp_Aller,aes(Hour,fill=as.factor(dow)))+geom_bar(binwidth=1)
p2<-ggplot(temp_Aller,aes(woy,fill=as.factor(dow)))+geom_bar(binwidth=1)
grid.arrange(p1, p2, ncol=2,main =  paste0("Label_",i,":Aller"))

p1<-ggplot(temp_Aller,aes(Hour,fill=as.factor(Entr)))+geom_bar(binwidth=1)
p2<-ggplot(temp_Aller,aes(Hour,fill=as.factor(Sor)))+geom_bar(binwidth=1)
grid.arrange(p1, p2, ncol=2,main =  paste0("Label_",i,":Aller"))

##########
tR_Entr<-count(temp[temp$noPsg>seuil & temp$Hour %in% pRtemp & temp$dow %in% jOtemp,],c("Entr"))
tR_Entr<-tR_Entr[order(tR_Entr$freq,decreasing=TRUE),]
head(tR_Entr)

tR_Sor<-count(temp[temp$noPsg>seuil & temp$Hour %in% pRtemp & temp$dow %in% jOtemp,],c("Sor"))
tR_Sor<-tR_Sor[order(tR_Sor$freq,decreasing=TRUE),]
head(tR_Sor)

##########
temp_Retour<-temp[temp$noPsg>seuil & temp$Hour %in% pRtemp & temp$dow %in% jOtemp,]
p1<-ggplot(temp_Retour,aes(Hour,fill=as.factor(dow)))+geom_bar(binwidth=1)
p2<-ggplot(temp_Retour,aes(woy,fill=as.factor(dow)))+geom_bar(binwidth=1)
grid.arrange(p1, p2, ncol=2,main =  paste0("Label_",i,":Retour"))

p1<-ggplot(temp_Retour,aes(Hour,fill=as.factor(Entr)))+geom_bar(binwidth=1)
p2<-ggplot(temp_Retour,aes(Hour,fill=as.factor(Sor)))+geom_bar(binwidth=1)
grid.arrange(p1, p2, ncol=2,main =  paste0("Label_",i,":Retour"))

##########
ID2<-List[1,1]
ID3<-List[1000,1]
ID4<-List$ID[List$freq_2014==2020]

##########
temp<-ID_OD_freq_2014[ID_OD_freq_2014$noPsg>500,]

ggplot(temp)+geom_bar(aes(noPsg))
temp<-(ID_OD[ID_OD$noPsg==863,])
# > print(unique(temp$ID),digits=16)
# [1] 25004000411700012 25004086206800000
temp1<-temp[temp$ID==25004000411700012,]

##########
ID_test<-List[1015:1020,1]
ID_OD_test<-ID_OD_2014[ID_OD_2014$ID %in% ID_test,] 

sm.density.compare(ID_OD_test$perPsg,ID_OD_test$ID)
sm.density.compare(ID_OD_test$noPsg,ID_OD_test$ID)

ggplot(ID_OD_test,aes(noPsg))+geom_bar()

ggplot(ID_OD_test,aes(Hour,fill=as.factor(dow))) +
  geom_bar(binwidth=1) + 
  facet_wrap(~Label)

ggplot(ID_OD_test,aes(woy,fill=as.factor(dow))) +
  geom_bar(binwidth=1) + 
  facet_wrap(~Label)


