library(party)
library(plyr)
library(ggplot2)
library(scales)
library(knitr)
library(sm)
library(gridExtra)
##########


d1<-FF
d2<-JF

DOW.H <- count(d1, c("Entr", "Sor", "DOW", "Hour"))

for (i in 1:row(DOW.H)){
  temp.OD <- subset(d1,
                    d1$Entr == DOW.H$Entr[i] &
                      d1$Sor  == DOW.H$Sor[i] &
                      d1$DOW  == DOW.H$DOW[i] &
                      d1$Hour == DOW.H$Hour[i]
  )
  temp.other <- temp.OD[1, -c(5:8)]
  temp.DH <- subset(d2, d2$DOW == DOW.H$DOW[i])
  temp.DH$Passage <- 0
  temp.DH <- merge(temp.DH, temp.other, by = "DOW")
  
  for (j in 1:nrow(temp.DH)){
    for (k in 1:nrow(temp.OD)){
      if (temp.DH$Year[j]  == temp.OD$Year[k]  &
            temp.DH$Month[j] == temp.OD$Month[k] &
            temp.DH$Day[j]   == temp.OD$Day[k]) {
        temp.DH$Passage[j] <- 1
      }
    }
  }
  
  if (i == 1) {
    res <- temp.DH
  } else {
    res <- rbind(res, temp.DH)
  }
}


# prepare
NP<-na.omit(NP)
NP1<-NP[,c(1,2,4,5,7:12,13:22)]
NP2<-NP[,c(4,13,15,16,18:22)]
NP3<-NP[,c(4,18:22)]

##########
# plot to decide Nb of Clusters
wssplot<-function(data,nc=15,seed=1234){
  wss<-(nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i]<-sum(kmeans(data,centers=i)$withinss)
  }
  plot(1:nc,wss,type="b",xlab="Number of Clusters",ylab="Within groups sum of squares")
}
##########
# scale & plot
df<-scale(NP3)
wssplot(df)

##########
# NbClust
library(NbClust)
set.seed(1234)
nc <-NbClust(df,min.nc=2,max.nc=20,method="kmeans")
table(nc$Best.n[1,])
par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]),
          xlab="Numer of Clusters", ylab="Number of Criteria",
          main="Number of Clusters Chosen by 6 Criteria")

##########
# fit.km
set.seed(1234)
fit.km<-kmeans(df,5,nstart=25)
fit.km$size
fit.km$centers

Kresult<-aggregate(NP3,by=list(cluster=fit.km$cluster),mean)
Kresult

##########
# viz

Kres<-cbind(NP,fit.km$cluster)
names(Kres)[23]<-"Cluster"
ggplot(Kres,aes(Hour,fill=as.factor(Cluster)))+geom_bar(binwidth=1)
ggplot(Kres,aes(DOW,fill=as.factor(Cluster)))+geom_bar(binwidth=1)
ggplot(Kres,aes(WOY,fill=as.factor(Cluster)))+geom_bar(binwidth=1)


print(fit.km)

par(mfrow=c(1,1))
pairs(df,col=fit.km$cluster)



par(mfrow=c(2,2))
#plot(df[,c(1,5)],col = fit.km$cluster)
plot(df[,c(2,3)],col = fit.km$cluster)
plot(df[,c(4,5)],col = fit.km$cluster)
plot(df[,c(1,4)],col = fit.km$cluster)
plot(df[,c(1,6)],col = fit.km$cluster)


# points(fit.km$centers, col = 1:2, pch = 8)

# Sor<-count(NP,"Sor")
# Entr<-count(NP,"Entr")
# OD<-count(NP,c("Entr","Sor"))
# OD<-OD[order(OD$freq,decreasing=TRUE),] 
# 
# names(Sor)[1]<-"Cde"
# names(Entr)[1]<-"Cde"
# Sor<-merge(x=Sor,gares_geo,by="Cde")
# Entr<-merge(Entr,gares_geo,by="Cde")
# Sor<-Sor[order(Sor$freq,decreasing=TRUE),]
# Entr<-Entr[order(Entr$freq,decreasing=TRUE),]

ggplot(Kres)+
  geom_segment(aes(x=Elng, y=Elat,xend=Slng, yend=Slat,color=as.factor(Cluster)),lwd=1,arrow=arrow(length = unit(0.1,"cm")))+
  geom_point(data=Sor,aes(Lng, Lat,size=freq), col="Red")+
  geom_point(data=gares_geo,aes(Lng,Lat),size=3,alpha=.2,shape=3)+
  geom_point(data=gare_SO,aes(Lng, Lat, color="S.O."), size=3, shape=3)+
  ylim(c(42,46.5))+xlim(c(-1.5,7.5))+
  theme(legend.position=c(.95,.9),plot.title = element_text(size=20, face="bold")) +
  ggtitle("Sor & OD")

##########
C3<-Kres[Kres$Cluster==3,]

count(Kres[Kres$Cluster==1,],c('Entr','Sor'))
count(Kres[Kres$Cluster==4,],c('Entr','Sor'))
count(Kres[Kres$Cluster==5,],c('Entr','Sor'))

centers<-as.data.frame(fit.km$centers)

ggplot(data=Kres, aes(x=Slng, y=Slat, color=as.factor(Cluster) )) + 
  geom_point()
# +
#   geom_point(data=centers, aes(x=Slng,y=Slat,color='center')) +
#   geom_point(data=centers, aes(x=Slng,y=Slat,color='center'), size=52, alpha=.3,show_guide=FALSE)

##########
set.seed(1234)
clustergram(df, k.range = 2:40, line.width = 0.004) # notice how I am using line.width.  Play with it on your problem, according to the scale of Y.
