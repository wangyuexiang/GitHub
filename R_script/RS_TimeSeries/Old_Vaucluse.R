##########
##########
# Old code: Vaucluse
##########
##########

useful<-c(1,2,8:15,ncol(List))
List[1:20, useful]

m<-ggplot(noPsg_freq,aes(x=noPsg)) +
  geom_point(aes(y=freq,color="no_Psg_de_OD")) +
  geom_point(aes(y=Trx,color="no_Trx_total"))


##########
hist(ID_OD_freq$noPsg)
hist(ID_OD_freq$noPsg,breaks=c(0,10,100,4000))

qplot(SemaineTaux,data=List)+geom_bar()
boxplot(List$SemaineTaux)

plot(List$SemaineTaux)
hist(List$SemaineTaux)
ggplot(List,aes(SemaineTaux))+geom_bar()
ggplot(List,aes(SemaineTaux))+geom_density()

##########

ID3_OD_freq<-count(ID3_OD,c("ID","Entr","Sor"))
names(ID3_OD_freq)[4]<-"noPsg"
ID3_OD_freq<-ID3_OD_freq[order(ID3_OD_freq$noPsg,decreasing=TRUE),]

ID3_noPsg_freq<-count(ID3_OD_freq,c("ID","noPsg"))
ID3_noPsg_freq$no<-ID3_noPsg_freq$noPsg*ID3_noPsg_freq$freq

ggplot(ID3_noPsg_freq,aes(x=noPsg)) +
  geom_point(aes(y=freq,color="no_Psg_de_OD")) +
  geom_point(aes(y=no,color="no_Trx_total"))

plot(ID3_OD_freq$noPsg)
hist(ID3_OD_freq$noPsg)

ggplot(ID_OD,aes(factor(Year)))+geom_bar()

temp1<-as.Date("2014-8-1")
temp<-ID3_OD[ID3_OD$Month==8 & ID3_OD$Year==2014,]
ggplot(temp,aes(Date,fill=factor(SorSim)))+ geom_bar() + scale_x_date()



##########
ID2_OD<-read.table("ID2_OD.csv",sep=",")
ID3_OD<-subset(ID_OD,ID_OD$ID==ID3)

ID2_OD$SorSim<-as.integer(substr(ID2_OD$Sor,4,8))
ID3_OD$SorSim<-as.integer(substr(ID3_OD$Sor,4,8))

ID2_OD$EntrSim<-as.integer(substr(ID2_OD$Entr,4,8))
ID3_OD$EntrSim<-as.integer(substr(ID3_OD$Entr,4,8))

ggplot(ID2_OD,aes( SorSim),binwidth=1) +   
  #  geom_histogram() + 
  geom_density() +
  xlim(c(4200,4250))

ggplot(ID3_OD,aes(EntrSim, SorSim)) +   geom_point() +ggtitle("ID3 Entr-Sor")

ggplot(ID3_OD) +   
  geom_density(aes( SorSim,color="Sor",alpha=.5)) + 
  geom_density(aes( EntrSim,color="Entr"),alpha=.5) + 
  xlim(c(4000,6000))

##########
temp<-ID_OD[1:1000,]
temp<-ID3_OD
ggplot(temp,aes(Entr,Sor))  + geom_point() + geom_rug(col="darkred",alpha=.1)
ggplot(temp,aes(Entr,Sor))  + geom_point() + geom_rug(col="darkred")

temp<-count(ID3_OD,"Sor")
n<-nrow(ID3_OD)
temp$taux<-temp$freq/n
temp<-temp[order(temp$taux,decreasing=TRUE),]

temp<-count(ID2_OD,"Sor")
n<-nrow(ID2_OD)
temp$taux<-temp$freq/n
temp<-temp[order(temp$taux,decreasing=TRUE),]

temp<-List
temp<-temp[order(temp$P_Trx_100),]

ncol(temp)
useful<-c(1,2,8:14,22,24)

##########
ID4<-List$ID[List$freq_2014==2020]
temp<-subset(ID_OD_freq,ID_OD_freq$ID==ID4)
temp<-temp[order(temp$noPsg,decreasing=TRUE),]

ID4_OD<-subset(ID_OD,ID_OD$ID==ID4)        
ID4_OD_freq<-count(ID3_OD,c("ID","Entr","Sor"))

ID4_OD$SorSim<-as.integer(substr(ID4_OD$Sor,4,8))
ID4_OD$EntrSim<-as.integer(substr(ID4_OD$Entr,4,8))

ggplot(ID4_OD,aes(SorSim))+geom_bar()+xlim(c(4000,9000))

ggplot(ID4_OD,aes(SorSim))+geom_bar(binwidth=1)+xlim(c(4200,4230))
ggplot(ID4_OD,aes(SorSim))+geom_bar()+xlim(c(6000,6200))
ggplot(ID4_OD,aes(SorSim))+geom_bar()+xlim(c(7000,8500))


##########
ggplot(data=temp1,aes(x=noPsg)) +
  geom_line(aes(y=freq,col="no_OD")) +
  geom_line(aes(y=noTrx,col="no_Trx")) 

ID4_OD<-merge(ID4_OD,temp,by=c("ID","Entr","Sor"))
ggplot(data=ID4_OD, aes(x=noPsg)) + 
  geom_bar(breaks = c(0,1, 5, 10, 50,100,180))


ID3_OD<-merge(ID3_OD,ID3_OD_freq,by=c("ID","Entr","Sor"))
ggplot(data=ID3_OD, aes(x=noPsg)) + 
  geom_bar(breaks = c(0,1, 5, 10, 50,100,180))

ID2_OD_freq<-subset(ID_OD_freq,ID_OD_freq$ID==ID2)
ID2_OD<-merge(ID2_OD,ID2_OD_freq,by=c("ID","Entr","Sor"))
ggplot(data=ID2_OD, aes(x=noPsg)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)),breaks = c(0,1, 5, 10, 50,100,180)) 


ggplot(data=noPsg_freq,aes(x=noPsg)) +
  geom_bar(breaks = c(0,1, 5, 10, 50,100,180)) +
  geom_line(aes(y=Trx,col="no_Trx")) 

ggplot(ID3_OD,aes(KMS))+geom_bar(aes(y=(..count..)/sum(..count..)))+ scale_y_continuous(labels = percent_format())


##########
t1<-ID3_OD
t2<-gares
t2$SorSim<-paste0(t2$Societe,t2$Cde)

t3<-merge(x=t1,y=t2,by="SorSim",all.x=TRUE)
write.table(t3[!is.na(t3$Lib),],file="heatmap_ID3.csv",sep=",")

##########
t1<-ID2_OD
t2<-gares
t2$SorSim<-paste0(t2$Societe,t2$Cde)

t3<-merge(x=t1,y=t2,by="SorSim",all.x=TRUE)
sum(!is.na(t3$Lib))

write.table(t3[!is.na(t3$Lib),23:24],file="heatmap_ID2.csv",sep=",")

##########
ID2_OD_freq<-ID2_OD_freq[order(ID2_OD_freq$noPsg,decreasing=TRUE),]
n<-nrow(ID2_OD)
ID2_OD_freq$perPsg<-ID2_OD_freq$noPsg/n

head(ID2_OD_freq)
ggplot(data=ID2_OD_freq,aes(x=perPsg)) +
  geom_bar(breaks = c(0,.001,.01 , .05,1)) +
  geom_line(aes(y=Trx,col="no_Trx")

##########
# ID_test<-List[1000:1005,1]
# ID_OD_test<-ID_OD[ID_OD$ID %in% ID_test,] 
# ID_OD_freq_test<-ID_OD_freq[ID_OD_freq$ID %in% ID_test,] 
# 
# no_test<-count(ID_OD_test,"ID")
# names(no_test)[2]<-"totalPsg"
# ID_OD_freq_test<-merge(x=ID_OD_freq_test,y=no_test,by="ID",all.x=TRUE)
# ID_OD_freq_test$perPsg<-ID_OD_freq_test$noPsg/ID_OD_freq_test$totalPsg
# 
# ID_OD_test<-merge(x=ID_OD_test,y=ID_OD_freq_test,by=c("ID","Entr","Sor"),all.x=TRUE)
# 
# # d1<-density(ID_OD_test[ID_OD_test$ID==ID_test[1],]$perPsg)
# # d2<-density(ID_OD_test[ID_OD_test$ID==ID_test[2],]$perPsg)
# # d3<-density(ID_OD_test[ID_OD_test$ID==ID_test[3],]$perPsg)
# # d4<-density(ID_OD_test[ID_OD_test$ID==ID_test[4],]$perPsg)
# # d5<-density(ID_OD_test[ID_OD_test$ID==ID_test[5],]$perPsg)
# 
# sm.density.compare(ID_OD_test$perPsg,ID_OD_test$ID)
##########

##########
ID_test<-List[1015:1020,1]
ID_OD_test<-ID_OD[ID_OD$ID %in% ID_test,] 

sm.density.compare(ID_OD_test$perPsg,ID_OD_test$ID)
sm.density.compare(ID_OD_test$noPsg,ID_OD_test$ID)

ggplot(ID_OD_test,aes(noPsg))+geom_bar()
ggplot(ID_OD_test,aes(noPsg,fill=Year))+geom_bar(position="dodge")
ggplot(ID_OD_test,aes(perPsg,fill=ID))+geom_bar(position="dodge")
##########
ID_test<-List[1015:1020,1]
ID_OD_test<-ID_OD_2014[ID_OD_2014$ID %in% ID_test,] 

sm.density.compare(ID_OD_test$perPsg,ID_OD_test$ID)
sm.density.compare(ID_OD_test$noPsg,ID_OD_test$ID)

ggplot(ID_OD_test,aes(noPsg))+geom_bar()
ggplot(ID_OD_test,aes(noPsg,fill=Year))+geom_bar(position="dodge")
ggplot(ID_OD_test,aes(perPsg,fill=as.factor(ID)))+geom_bar(position="dodge")

ggplot(ID_OD_test,aes(Hour,fill=as.factor(dow)))+geom_bar(binwidth=1)
ggplot(ID_OD_2014,aes(Hour,fill=as.factor(dow)))+geom_bar(binwidth=1)


# sm.density.compare(ID_OD$perPsg,ID_OD$ID)
plot(density(ID_OD$KMS))
plot(density(ID_OD$noPsg))
plot(density(ID_OD$perPsg))

ggplot(ID_OD)+geom_density(aes(perPsg))
ggplot(ID_OD)+geom_bar(aes(noPsg))
ggplot(ID_OD[ID_OD$Year==2014,])+geom_bar(aes(noPsg))
ggplot(ID_OD[ID_OD$Year==2014,])+geom_bar(aes(noPsg),breaks=c(0,1,3,10,50,100,500,3000))

nrow(ID_OD[ID_OD$Year==2014 & ID_OD$noPsg<3,]);
nrow(ID_OD[ID_OD$Year==2014 & ID_OD$noPsg<10,]);
nrow(ID_OD[ID_OD$Year==2014 & ID_OD$noPsg<50,]);
nrow(ID_OD[ID_OD$Year==2014 & ID_OD$noPsg<100,]);
nrow(ID_OD[ID_OD$Year==2014 & ID_OD$noPsg<500,])
##########
nrow(ID_OD_2014[ ID_OD_2014$noPsg<3,]);
nrow(ID_OD_2014[ ID_OD_2014$noPsg<10,]);
nrow(ID_OD_2014[ ID_OD_2014$noPsg<50,]);
nrow(ID_OD_2014[ ID_OD_2014$noPsg<100,]);
nrow(ID_OD_2014[ ID_OD_2014$noPsg<500,])
nrow(ID_OD_2014[])

##########
temp<-ID_OD_freq[ID_OD_freq$noPsg>500,]
ggplot(temp)+geom_bar(aes(noPsg))
temp<-(ID_OD[ID_OD$noPsg==863,])
# > print(unique(temp$ID),digits=16)
# [1] 25004000411700012 25004086206800000
temp1<-temp[temp$ID==25004000411700012,]

##########
# 20150616
##########
gare_dist$Entr<-as.character(gare_dist$Entr)
gare_dist$Sor<-as.character(gare_dist$Sor)

gare_dist$Entr<-as.numeric(gare_dist$Entr)
gare_dist$Sor<-as.numeric(gare_dist$Sor)
  
ggplot(gare_dist,aes(Entr,Sor))+geom_point()+
  xlim(c(25004200,25004300)) +
  ylim(c(25004200,25004300)) 

ggplot(gare_dist,aes(Entr,Sor))+geom_point()+
  xlim(c(25004100,25004300)) +
  ylim(c(25004100,25004300)) 

ggplot(gare_dist,aes(Entr,Sor))+geom_point()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

ggplot(gare_dist,aes(KMS,freq))+geom_point()
ggplot(gare_dist,aes(Soc))+geom_bar()

# temp_Entr<-temp1[temp1$Entr<=temp1$Sor,]
# temp_Sor<-temp1[temp1$Entr>temp1$Sor,]

ggplot(melt(t1), aes(Entr, fill=value)) + geom_raster()
image(t(m[nrow(m):1,] ), axes=FALSE, zlim=c(-4,4), col=rainbow(21))

##########
# 20150617
##########
```{r, echo=FALSE,cache=TRUE,fig.width= 15, fig.height=12}
temp<-ID1
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
```


```{r, echo=FALSE,cache=TRUE,fig.width= 15, fig.height=12}
temp<-ID1_zone
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
```
