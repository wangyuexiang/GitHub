##########
##########
# Old Code: VIPs
##########
##########

##########
##########
#prepare transaction history
##########
##########
#get original data
#trajets_all<-read.csv("TransactionsEchantillon_20150324.csv",sep=",",header=TRUE)
trajets_all<-read.csv("Histo_Escota&ASF.csv",sep=";",header=TRUE)
trajets_all[,1]<-sapply(trajets_all[,1],as.character)

#get Year,Month,Day,Hour,Minute
trajets_all$year<-substr(trajets_all[,11],1,4)
trajets_all$month<-substr(trajets_all[,11],6,7)
trajets_all$day<-substr(trajets_all[,11],9,10)
trajets_all$minute<-substr(trajets_all[,11],15,16)

#construct data.frame
ID<-factor(trajets_all$NUM_MOY_PAIM)
Entr<-factor(trajets_all$COD_GARE_ENTR)
Sor<-factor(trajets_all$COD_GARE_SOR)
Year<-factor(trajets_all$year)
Month<-factor(trajets_all$month)
Day<-factor(trajets_all$day)
DOW<-factor(trajets_all$dow)
Hour<-trajets_all$hour
Minute<-trajets_all$minute

#ID_OD with necessary info
ID_OD_all<-data.frame(ID,Entr,Sor,Year,Month,Day,DOW,Hour,Minute)
ID_OD_all$Year<-as.integer(trajets_all$year)
ID_OD_all$Month<-as.integer(ID_OD_all$Month)
ID_OD_all$Day<-as.integer(ID_OD_all$Day)
ID_OD_all$Date<-as.Date(substr(trajets_all[,11],1,10))

##########
##########
#prepare frequent ID_OD history
##########
##########
# get all requent ID_OD with the help of parameter freq_seuil
# freq seuil
freq_seuil<-50
#no_trajets/ID_OD
ID_OD_freq<-count(ID_OD_all,c("ID","Entr","Sor"))
# ID_OD with no. of pass over freq_seuil
ID_OD_hot_ref<-subset(ID_OD_freq,ID_OD_freq$freq>freq_seuil)

#get all passage of the frequent ID_OD
ID_OD_hot_all<-ID_OD_all[ID_OD_all$ID==ID_OD_hot_ref[1,1] & ID_OD_all$Entr==ID_OD_hot_ref[1,2] &ID_OD_all$Sor==ID_OD_hot_ref[1,3], ]
for (i in 2:nrow(ID_OD_hot_ref)){
  temp<-ID_OD_all[ID_OD_all$ID==ID_OD_hot_ref[i,1] & ID_OD_all$Entr==ID_OD_hot_ref[i,2] &ID_OD_all$Sor==ID_OD_hot_ref[i,3], ]
  ID_OD_hot_all<-rbind(ID_OD_hot_all,temp)
}

##########
sm.density.compare(ID_OD$noPsg,ID_OD$ID)
legend("topright",legend=levels(ID_OD$ID),fill=2+(0:nlevels(ID_OD$ID)))

##########
ID_PC<-ID_ref[3,3]
ID_LF<-ID_ref[5,3]
ID_MC<-ID_ref[6,3]
ID_JP<-ID_ref[7,3]
ID_FC<-ID_ref[8,3]

ID_OD_PC<-subset(ID_OD,ID_OD$ID==ID_PC)
t1<-ID_OD_PC
t3<-merge(x=t1,y=t2,by="Sor",all.x=TRUE)
write.table(t3[!is.na(t3$Lib),],file="heatmap_PC.csv",sep=",")

ID_OD_LF<-subset(ID_OD,ID_OD$ID==ID_LF)
t1<-ID_OD_LF
t3<-merge(x=t1,y=t2,by="Sor",all.x=TRUE)
write.table(t3[!is.na(t3$Lib),],file="heatmap_LF.csv",sep=",")

ID_OD_MC<-subset(ID_OD,ID_OD$ID==ID_MC)
t1<-ID_OD_MC
t3<-merge(x=t1,y=t2,by="Sor",all.x=TRUE)
write.table(t3[!is.na(t3$Lib),],file="heatmap_MC.csv",sep=",")

ID_OD_JP<-subset(ID_OD,ID_OD$ID==ID_JP)
t1<-ID_OD_JP
t3<-merge(x=t1,y=t2,by="Sor",all.x=TRUE)
write.table(t3[!is.na(t3$Lib),],file="heatmap_JP.csv",sep=",")

ID_OD_FC<-subset(ID_OD,ID_OD$ID==ID_FC)
t1<-ID_OD_FC
t3<-merge(x=t1,y=t2,by="Sor",all.x=TRUE)
write.table(t3[!is.na(t3$Lib),],file="heatmap_FC.csv",sep=",")

##########
d1<-as.Date("2014-1-1")
d2<-as.Date("2015-5-10")

ID_OD_MD<-subset(ID_OD,ID_OD$ID==ID_MD)
ID_OD_NP<-subset(ID_OD,ID_OD$ID==ID_NP)
ID_OD_PC<-subset(ID_OD,ID_OD$ID==ID_PC)
ID_OD_FF<-subset(ID_OD,ID_OD$ID==ID_FF)
ID_OD_LF<-subset(ID_OD,ID_OD$ID==ID_LF)
ID_OD_MC<-subset(ID_OD,ID_OD$ID==ID_MC)
ID_OD_JP<-subset(ID_OD,ID_OD$ID==ID_JP)
ID_OD_FC<-subset(ID_OD,ID_OD$ID==ID_FC)

ID_OD_freq_MD<-count(ID_OD_MD,c("ID","Entr","Sor"))
ID_OD_freq_NP<-count(ID_OD_NP,c("ID","Entr","Sor"))
ID_OD_freq_PC<-count(ID_OD_PC,c("ID","Entr","Sor"))
ID_OD_freq_FF<-count(ID_OD_FF,c("ID","Entr","Sor"))
ID_OD_freq_LF<-count(ID_OD_LF,c("ID","Entr","Sor"))
ID_OD_freq_MC<-count(ID_OD_MC,c("ID","Entr","Sor"))
ID_OD_freq_JP<-count(ID_OD_JP,c("ID","Entr","Sor"))
ID_OD_freq_FC<-count(ID_OD_FC,c("ID","Entr","Sor"))

ID_OD_freq_MD<-ID_OD_freq_MD[order(ID_OD_freq_MD$freq,decreasing=TRUE),]
ID_OD_freq_NP<-ID_OD_freq_NP[order(ID_OD_freq_NP$freq,decreasing=TRUE),]
ID_OD_freq_PC<-ID_OD_freq_PC[order(ID_OD_freq_PC$freq,decreasing=TRUE),]
ID_OD_freq_FF<-ID_OD_freq_FF[order(ID_OD_freq_FF$freq,decreasing=TRUE),]
ID_OD_freq_LF<-ID_OD_freq_LF[order(ID_OD_freq_LF$freq,decreasing=TRUE),]
ID_OD_freq_MC<-ID_OD_freq_MC[order(ID_OD_freq_MC$freq,decreasing=TRUE),]
ID_OD_freq_JP<-ID_OD_freq_JP[order(ID_OD_freq_JP$freq,decreasing=TRUE),]
ID_OD_freq_FC<-ID_OD_freq_FC[order(ID_OD_freq_FC$freq,decreasing=TRUE),]

ID_OD_freq_MD$per<-ID_OD_freq_MD$freq/nrow(ID_OD_MD)
ID_OD_freq_NP$per<-ID_OD_freq_NP$freq/nrow(ID_OD_NP)
ID_OD_freq_PC$per<-ID_OD_freq_PC$freq/nrow(ID_OD_PC)
ID_OD_freq_FF$per<-ID_OD_freq_FF$freq/nrow(ID_OD_FF)
ID_OD_freq_LF$per<-ID_OD_freq_LF$freq/nrow(ID_OD_LF)
ID_OD_freq_MC$per<-ID_OD_freq_MC$freq/nrow(ID_OD_MC)
ID_OD_freq_JP$per<-ID_OD_freq_JP$freq/nrow(ID_OD_JP)
ID_OD_freq_FC$per<-ID_OD_freq_FC$freq/nrow(ID_OD_FC)

head(ID_OD_freq_MD)
head(ID_OD_freq_NP)
head(ID_OD_freq_PC)
head(ID_OD_freq_FF)
head(ID_OD_freq_LF)
head(ID_OD_freq_MC)
head(ID_OD_freq_JP)
head(ID_OD_freq_FC)

ID<-ID_ref[,c(1,3,5,6)]
ID$freq_DT<-NA

ID$freq_DT[1]<-sum(ID_OD_freq_MD$freq[1:2])
ID$freq_DT[2]<-sum(ID_OD_freq_NP$freq[1:2])
ID$freq_DT[3]<-sum(ID_OD_freq_PC$freq[1:3])
ID$freq_DT[4]<-sum(ID_OD_freq_FF$freq[1:5])
ID$freq_DT[5]<-sum(ID_OD_freq_LF$freq[1:3])
ID$freq_DT[6]<-sum(ID_OD_freq_MC$freq[1:4])
ID$freq_DT[7]<-sum(ID_OD_freq_JP$freq[1:4])
ID$freq_DT[8]<-sum(ID_OD_freq_FC$freq[1:1])

ID$per_DT<-ID$freq_DT/ID$freq

ID_OD_freq_LF[1:10,]
temp<-subset(ID_OD_LF,ID_OD_LF$Sor==25006001)
ggplot(temp)+geom_bar(aes(Date),binwidth=1)+scale_x_date()
ggplot(temp)+geom_bar(aes(DOW),binwidth=1)+xlim(c(0,7))
ggplot(temp)+geom_bar(aes(Hour),binwidth=1)+xlim(c(0,25))

##########
no_FF<-as.data.frame(hot_ref_FF$freq)
names(no_FF)<-"no"
no_FF<-count(no_FF,"no")

no_NP<-as.data.frame(hot_ref_NP$freq)
names(no_NP)<-"no"
no_NP<-count(no_NP,"no")

no_MD<-as.data.frame(hot_ref_MD$freq)
names(no_MD)<-"no"
no_MD<-count(no_MD,"no")

##########
##########
#Tester sur ID1
##########
##########
# ID1<-ID_OD_hot_ref[3,1]
# trajet_ID1<-subset(ID_OD_all,ID_OD_all$ID==ID1)
# ID1_In_365<-hot_In_365[hot_In_365$ID==ID1,]

# ID1_365<-ID1_In_365
# ID1_365$Date<-ISOdate(ID1_365$Year,ID1_365$Month,ID1_365$Day)


# ID1_no_passage<-subset(ID_no_passage,ID_no_passage$ID==ID1)
# ID1_no_passage$Date<-ISOdate(ID1_no_passage$Year,ID1_no_passage$Month,ID1_no_passage$Day)
# ggplot(ID1_no_passage, aes(Date, freq)) + geom_line() + xlab("") + ylabylab("# of Daily Passages")
# 
# ID1_no_passage_In_365<-In_365
# ID1_no_passage_In_365$no_passage<-0
# for (i in 1:nrow(ID1_no_passage_In_365)){
#   for (j in 1:nrow(ID1_no_passage)){
#     if(ID1_no_passage_In_365$Year[i]==ID1_no_passage$Year[j] & ID1_no_passage_In_365$Month[i] == ID1_no_passage$Month[j] & ID1_no_passage_In_365$Day[i]== ID1_no_passage$Day[j]){
#       ID1_no_passage_In_365$no_passage[i]<-ID1_no_passage$freq[j]
#       break
#     }
#   }
# }
# 
# ID1_no_passage_In_365$Date<-ISOdate(ID1_no_passage_In_365$Year,ID1_no_passage_In_365$Month,ID1_no_passage_In_365$Day)
# ID1_no_passage_In_365$Date<-as.Date(paste0(as.character(ID1_no_passage_In_365$Year),"-",as.character(ID1_no_passage_In_365$Month),"-",as.character(ID1_no_passage_In_365$Day)))
# 
# datebreaks <- seq(as.Date("2014-01-01"), as.Date("2015-06-01"), by="2 month")
# ggplot(ID1_no_passage_In_365, aes(Date, no_passage)) + 
#   geom_line() + xlab("") + ylab("# of Daily Passages") +
#   scale_x_date(breaks=datebreaks)

##########
##########
#get the In_365 View of frequent ID_OD (add "non_passage")
##########
##########
#prepare In_365
In_365[is.na(In_365)]<-0
# In_365$Passage<-0
#In_365$Passage<-NULL
In_365$Date<-as.Date(paste0(In_365$Year,"-",In_365$Month,"-",In_365$Day))
names(In_365)[4:5]<-c("WOY","DOW")
In_365$DOW<-as.POSIXlt(In_365$Date)$wday

##########

#initiate hot_In_365
hot_In_365<-In_365
temp_all<-subset(ID_OD_all, ID_OD_all$ID==ID_OD_hot_ref[1,1] & ID_OD_all$Entr==ID_OD_hot_ref[1,2]& ID_OD_all$Sor==ID_OD_hot_ref[1,3])

hot_In_365$ID<-ID_OD_hot_ref[1,1]
hot_In_365$Entr<-ID_OD_hot_ref[1,2]
hot_In_365$Sor<-ID_OD_hot_ref[1,3]

for (i in 1: nrow(hot_In_365)){
  for (j in 1 : nrow(temp_all)){
    if(hot_In_365$Year[i] ==temp_all$Year[j] & hot_In_365$Month[i]==temp_all$Month[j] & hot_In_365$Day[i]  ==temp_all$Day[j]) {
      hot_In_365$Passage[i]<-1
      break
    }
  }
}
hot_In_365$PassageHier<-c(0,hot_In_365$Passage[-nrow(hot_In_365)])

#complete hot_In_365
for (i in 2:nrow(ID_OD_hot_ref)){
  temp_In_365<-In_365
  temp_all<-subset(ID_OD_all, ID_OD_all$ID==ID_OD_hot_ref[i,1] & ID_OD_all$Entr==ID_OD_hot_ref[i,2]& ID_OD_all$Sor==ID_OD_hot_ref[i,3])
  
  temp_In_365$ID<-ID_OD_hot_ref[i,1]
  temp_In_365$Entr<-ID_OD_hot_ref[i,2]
  temp_In_365$Sor<-ID_OD_hot_ref[i,3]
  for (j in 1: nrow(temp_In_365)){
    for (k in 1 : nrow(temp_all)){
      if(temp_In_365$Year[j] ==temp_all$Year[k] & temp_In_365$Month[j]==temp_all$Month[k] & temp_In_365$Day[j]  ==temp_all$Day[k]) {
        temp_In_365$Passage[j]<-1
        break
      }
    }
  }
  temp_In_365$PassageHier<-c(0,temp_In_365$Passage[-nrow(temp_In_365)])
  hot_In_365<-rbind(hot_In_365,temp_In_365)
}  

##########
##########
#remove temperal v.a.
##########
##########
rm(temp,temp_In_365,temp_all)
rm(DOW,Day,Entr,Hour,ID,Minute,Month,Sor,Year)
rm(i,j,k)

##########
##########
# Test
##########
##########
# rm(fit.rf,fit.svm,smp_size,train_ind,train,trajets)
# rm(pred.rf,pred.svm)
# rm(ID_OD_all,ID_OD_hot_ref)
##########
# count(ID_OD[ID_OD$Nom==ID_ref$Nom[i] & ID_OD$noPsg>seuil & ID_OD$Hour %in% periodAller & ID_OD$DOW %in% jrOuvert,],c("Nom","Entr","Sor"))
# count(ID_OD[ID_OD$Nom==ID_ref$Nom[i] & ID_OD$noPsg>seuil & ID_OD$Hour %in% periodRetour & ID_OD$DOW %in% jrOuvert,],c("Nom","Entr","Sor"))
# temp<-count(ID_OD[ ID_OD$noPsg>seuil & 
#                (ID_OD$Hour %in% periodAller | ID_OD$Hour %in% periodRetour) & 
#                ID_OD$DOW %in% jrOuvert,],c("Nom","Entr","Sor"))
# temp<-count(ID_OD[ID_OD$Hour %in% periodRetour & ID_OD$DOW %in% jrOuvert,],c("Nom","Entr","Sor"))


##########
# result, ID_OD_DH
##########
temp<-ID_OD[ID_OD$Year==2014,]
temp1<-count(temp,c("Nom","Entr","Sor"))
temp1<-temp1[order(temp1$freq,decreasing=TRUE),]
temp2<-count(temp,c("Nom","Entr","Sor","DOW","Hour"))
temp2<-temp2[order(temp2$freq,decreasing=TRUE),]

result<-merge(temp2,t_DOW,by="DOW")

result$Per<-result$freq/result$Total
result<-result[order(result$Per,decreasing=TRUE),]

temp<-ID_OD[ID_OD$Nom=="FF",]
temp<-ID_OD[ID_OD$Nom=="FF" &  ID_OD$noPsg>40,]
temp<-ID_OD[ID_OD$Nom=="FF" &  ID_OD$noPsg>50,]
temp<-ID_OD[ID_OD$Nom=="FF" &  ID_OD$noPsg==100,]
ggplot(temp,aes(DOW,Hour,color=Sor))+geom_point(alpha=0.1)
ggplot(temp,aes(DOW,Hour))+geom_point(alpha=.3)
#ggplot(temp,aes(DOW,Hour))+stat_binhex()
ggplot(temp,aes(Date))+geom_bar(binwidth=1)

##########
# NP, FF
##########
temp<-ID_OD[ID_OD$Nom=="NP" ,]
count(ID_OD[ID_OD$Nom=="NP" & ID_OD$noPsg>20,],c("Entr","Sor"))
temp<-ID_OD[ID_OD$Nom=="NP" &  ID_OD$noPsg>260,]
ggplot(temp,aes(Hour))+geom_density()+facet_wrap(~Sor)

##########
```{r,echo=FALSE,cache=TRUE,fig.width=10,fig.height=10}
temp1<-NP[NP$Year==2014,]
temp2<-NP[NP$Year==2015,]

p1<-ggplot(temp1,aes(Hour))+geom_density()
p3<-ggplot(temp1,aes(DOW))+geom_density()
p5<-ggplot(temp1,aes(WOY))+geom_density()
p7<-ggplot(temp1,aes(perPsg))+geom_density()

p2<-ggplot(temp2,aes(Hour))+geom_density()
p4<-ggplot(temp2,aes(DOW))+geom_density()
p6<-ggplot(temp2,aes(WOY))+geom_density()
p8<-ggplot(temp2,aes(perPsg))+geom_density()

grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8, ncol=2)
```