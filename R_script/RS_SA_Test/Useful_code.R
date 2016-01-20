##########
##########
# Useful Codes
##########
##########



##########
# compare density
##########
sm.density.compare(ID_OD$perPsg,ID_OD$ID)
legend("topright",legend=levels(ID_OD$ID),fill=2+(0:nlevels(ID_OD$ID)))


##########
par(mfrow=c(2,1))
plot(density(ID_OD$noPsg))
plot(density(ID_OD$perPsg))

ggplot(ID_OD)+geom_bar(aes(noPsg))

ggplot(ID_OD,aes(noPsg,fill=as.factor(DOW)))+geom_bar()
ggplot(ID_OD,aes(DOW,fill=as.factor(Hour)))+geom_bar()
ggplot(ID_OD,aes(Hour,fill=as.factor(DOW)))+geom_bar()

##########
# viz in GoogleMap
##########
gares<-read.table("garesLatLng.csv",head=TRUE,sep=";")
t1<-ID_OD
t2<-gares

t3<-merge(x=t1,y=t2,by="Sor",all.x=TRUE)
write.table(t3[!is.na(t3$Lib),],file="heatmap_VIPs.csv",sep=",")

nrow(t3[is.na(t3$Lib),])