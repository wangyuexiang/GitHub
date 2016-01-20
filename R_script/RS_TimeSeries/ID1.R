##########
# ID1
##########

##########
# 20190623
##########
head(ID1)

OD<-subset(ID1,ID1$Entr==25004213 & ID1$Sor==25004220)

plot(count(OD,"Date")$freq)
boxplot(count(OD,"Date")$freq)
ggplot(OD,aes(Date))+geom_bar(binwidth=1)
ggplot(OD,aes(Date,Hour))+geom_point()

count(OD,"DOW")
count(OD,c("Year","WOY"))

ggplot(OD,aes(DOW))+geom_bar(binwidth=1)
ggplot(OD,aes(WOY,fill=as.factor(Year)))+geom_bar(binwidth=1,position="dodge")

t2<-ggplot(OD[OD$Hour<12,],aes(factor(DOW),y=Hour))+geom_boxplot()+ylim(c(0,25))
t1<-ggplot(OD[OD$Hour>=12,],aes(factor(DOW),y=Hour))+geom_boxplot()+ylim(c(0,25))
grid.arrange(t1,t2,ncol=1)

tH<-OD[OD$DOW==1,]
ggplot(tH,aes(x=DOW,y=Hour))+geom_boxplot()
ggplot(tH,aes(DOW,Hour))+geom_point(position='dodge',alpha=.3)

ggplot(tH,aes(DOW,Hour))+geom_point(position=position_jitter(width=0.05))

ggplot(OD,aes(Date,Hour))+geom_point()
ggplot(OD,aes(DOW,Hour))+geom_point(position=position_jitter(width=0.05))

##########
# 20190617
##########
gares_geo<-gares[,c(1:4,6,8,7)]

ggplot(gares_geo,aes(Lng,Lat,color=as.factor(Societe)))+geom_point()
#ggplot(gares_geo,aes(Lng,Lat,color=PK))+geom_point()
ggplot(gares_geo,aes(Lng,Lat,color=as.factor(Autoroute)))+geom_point()
ggplot(gares_geo[gares_geo$Societe==4,],aes(Lng,Lat,color=as.factor(Autoroute)))+geom_point()

##########
names(Sor)[1]<-"Cde"
names(Entr)[1]<-"Cde"

Sor<-merge(x=Sor,y=gares_geo,by="Cde",all.x=T)

ggplot(na.omit(Sor),aes(Lng,Lat,color=as.factor(Societe)))+geom_point()
ggplot(na.omit(Sor),aes(Lng,Lat,color=freq))+geom_point()

##########
geo<-gares_geo[,c(4,6,7)]
geo<-geo[geo$Cde>25000000,]

names(geo)<-c("Sor","Slng","Slat")
ID1<-merge(x=ID1,y=geo,by="Sor",all.x=TRUE)
names(geo)<-c("Entr","Elng","Elat")
ID1<-merge(x=ID1,y=geo,by="Entr",all.x=TRUE)

# xquiet<-scale_x_continuous("", breaks=NULL)
# yquiet<-scale_y_continuous("", breaks=NULL)
# quiet<-list(xquiet, yquiet)
# 
# n<-nrow(ID1)
# ggplot(ID1)+geom_segment(aes(x=Elng, y=Elat,xend=Slng, yend=Slat,alpha=noPsg), col="white",lwd=2)+
#   scale_alpha_continuous(range = c(0.03, 0.3))+
#   geom_point(data=gares_geo,aes(Lng,Lat,color=as.factor(Societe)),size=3)+
#   theme(panel.background = element_rect(fill='black',colour='black'))+quiet+coord_equal()
##########
# 20190616
##########
#ID1<-ID_OD_2014[ID_OD_2014$Label==1,]
ggplot(ID1,aes(Sor))+geom_bar()

Sor<-count(ID1,"Sor")
Sor<-Sor[order(Sor$freq,decreasing=TRUE),]
Entr<-count(ID1,"Entr")
Entr<-Entr[order(Entr$freq,decreasing=TRUE),]
OD<-count(ID1,c("Entr","Sor"))
OD<-OD[order(OD$freq,decreasing=TRUE),]

zone<-c(25004210:25004220)
# nrow(ID1)
# nrow(ID1[ID1$Sor %in% zone,])
# nrow(ID1[ID1$Entr %in% zone & ID1$Sor %in% zone,])
ID1$zone<-0
ID1$zone[ID1$Entr %in% zone & ID1$Sor %in% zone]<-1
##########
# ggplot(ID1,aes(Hour,fill=as.factor(zone)))+geom_bar(binwidth=1)+guides(fill=FALSE)
# ggplot(ID1,aes(WOY,DOW,fill=as.factor(zone)))+geom_tile()
# ggplot(ID1,aes(WOY,Hour,fill=as.factor(zone)))+geom_tile()
# ggplot(ID1,aes(WOY,Hour))+geom_tile()
# ggplot(ID1,aes(Date,fill=as.factor(zone)))+geom_bar(binwidth=1)
ID1_zone<-ID1[ID1$zone==1,]
# ggplot(ID1_zone,aes(Date))+geom_bar(binwidth=1)
# ggplot(ID1_zone,aes(WOY,Hour))+geom_tile()
##########
ID1_Date_DH<-count(ID1,c("zone","Date","DOW","Hour"))
temp<-ID1_Date_DH
temp$freq<-NULL
ID1_DH<-count(temp,c("zone","DOW","Hour"))
ID1_DH<-ID1_DH[order(ID1_DH$freq,decreasing=TRUE),]

ID1_result<-merge(ID1_DH,t_DOW,by="DOW")
ID1_result$Per<-ID1_result$freq/ID1_result$Total
ID1_result<-ID1_result[order(ID1_result$Per,decreasing=TRUE),]
#nrow(ID1_result[ID1_result$Per>=.1,])

ID1_result<-ID1_result[ID1_result$zone==1,]
names(ID1_result)[6]<-"Per_Z"
#ggplot(ID1_result,aes(Per))+geom_bar()

##########
temp<-ID1_result[,c(1:3,6)]
temp2<-merge(x=ID1,y=temp,by=c("zone","DOW","Hour"),all.x=TRUE)

temp2$Pred_Z<-0
temp2$Pred_Z[temp2$Per_Z>=.5]<-.5
temp2$Pred_Z[temp2$Per_Z< .5 & temp2$Per_Z>=.3]<-.3
temp2$Pred_Z[temp2$Per_Z< .3 & temp2$Per_Z>=.1]<-.1

temp2[is.na(temp2)]<-0
ID1<-temp2
#ggplot(ID1,aes(WOY,Hour,fill=as.factor(Pred_Z)))+geom_tile()
#ggplot(ID1,aes(WOY,DOW,fill=as.factor(Pred_Z)))+geom_tile()
#ggplot(ID1,aes(Date,DOW,fill=as.factor(Pred_Z)))+geom_tile()

# ggplot(ID1,aes(Hour,fill=as.factor(Pred_Z)))+geom_bar(binwidth=1)
# ggplot(ID1,aes(Hour,fill=as.factor(Predicted)))+geom_bar(binwidth=1)
# ggplot(ID1,aes(Hour,fill=as.factor(zone)))+geom_bar(binwidth=1)
# ggplot(ID1,aes(Hour,DOW,fill=as.factor(Pred_Z)))+geom_tile()
##########
count(ID1,c("Predicted","Pred_Z","zone"))
# nrow(ID1[ID1$Per>.5,])
# nrow(ID1[ID1$Per_Z>.5,])
# nrow(ID1[ID1$Per_Z>.5 | ID1$Per>.5,])

