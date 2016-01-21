##########
##########
#install package
##########
##########
library(randomForest)
library(party)
library(plyr)
library(e1071)
library(ggplot2)
library(scales)
library(gridExtra)

##########
##########
#get original data
##########
##########

#get gare ref
gare<-read.csv("Gares.csv",header = TRUE,sep =";")
names(gare)<-c("Cod","Lib")
#extract Country/Societe/Cde
gare$Country<-substr(gare$Cod,1,3)
gare$Societe<-substr(gare$Cod,4,5)
gare$Cde<-substr(gare$Cod,6,8)

#get user
user<-read.csv("Supports_84.csv",header = TRUE,sep =";")
#make ID as character
user$IDT_CLI_ABNE<-as.character(user$IDT_CLI_ABNE)
user$IDT_SUPO_ABNE<-as.character(user$IDT_SUPO_ABNE)

#get# of user
ID_sim<-as.integer(substr(user$IDT_CLI_ABNE,6,12))
ID_sim<-unique(as.data.frame(ID_sim))
nrow(ID_sim)

#get trajets historiques
t1<-proc.time()
trajets<-read.csv("Trajets_84.csv",header = TRUE,sep =";")
t2<-proc.time()
t2-t1
#utilisateur     systeme      ecoule
#469.67        9.21      513.79 

##########
# 20150624
##########
temp <- trajets
names(temp) <- c("ID","Entr","Dat_Entr","Sor","Dat_Sor","Tar_TCC","KMS","Cl")
temp$Year<-as.integer(substr(temp$Dat_Sor,1,4))
temp$Month<-as.integer(substr(temp$Dat_Sor,6,7))
temp$Day<-as.integer(substr(temp$Dat_Sor,9,10))
temp$Hour<-as.integer(substr(temp$Dat_Sor,12,13))
temp$Minute<-as.integer(substr(temp$Dat_Sor,15,16))

trx <- tbl_df(temp)
rm(temp)
trx <- select(trx, -Cl)
save(trx,file="trx.Vaucluse.RData")
##########

ID_OD<-trajets
names(ID_OD)<-c("ID","Entr","Dat_Entr","Sor","Dat_Sor","Tar_TCC","KMS","Cl")

tar<-count(ID_OD,"Tar_TCC")
tar<-tar[order(tar$freq,decreasing=TRUE),]

kms<-count(ID_OD,"KMS")
kms<-kms[order(kms$freq,decreasing=TRUE),]

#extract data information
ID_OD$Year<-as.integer(substr(ID_OD$Dat_Sor,1,4))
ID_OD$Month<-as.integer(substr(ID_OD$Dat_Sor,6,7))
ID_OD$Day<-as.integer(substr(ID_OD$Dat_Sor,9,10))
ID_OD$Hour<-as.integer(substr(ID_OD$Dat_Sor,12,13))
ID_OD$Date<-as.Date(substr(ID_OD$Dat_Sor,1,10))

ID_OD$ID<-as.character(ID_OD$ID)

#get ID_OD_freq
t1<-proc.time()
ID_OD_freq<-count(ID_OD,c("ID","Entr","Sor"))
t2<-proc.time()
t2-t1
#utilisateur     systeme      ecoule
#169.88        4.08      187.30 

##########
##########
# ID_OD_freq
##########
##########
freq_seuil<-50
ID_OD_hot_ref<-subset(ID_OD_freq,ID_OD_freq$freq>freq_seuil)

ID_OD_hot_500<-ID_OD_hot_ref[ID_OD_hot_ref$freq>500,]
ID_OD_hot_200<-ID_OD_hot_ref[ID_OD_hot_ref$freq>200,]
ID_OD_hot_100<-ID_OD_hot_ref[ID_OD_hot_ref$freq>100,]

ID500<-ID_OD_hot_500[,-4]
ID200<-ID_OD_hot_200[,-4]
ID100<-ID_OD_hot_100[,-4]

t1<-count(ID500,"ID")
t2<-count(ID200,"ID")
t3<-count(ID100,"ID")

nrow(t3[t3$freq==1,])
nrow(t3[t3$freq==2,])
nrow(t3[t3$freq==3,])
nrow(t3[t3$freq==4,])
nrow(t3[t3$freq==5,])
nrow(t3[t3$freq>5,])

nrow(t2[t2$freq==1,])
nrow(t2[t2$freq==2,])
nrow(t2[t2$freq==3,])
nrow(t2[t2$freq==4,])
nrow(t2[t2$freq==5,])
nrow(t2[t2$freq>5,])

nrow(t1[t1$freq==1,])
nrow(t1[t1$freq==2,])
nrow(t1[t1$freq==3,])
nrow(t1[t1$freq==4,])
nrow(t1[t1$freq==5,])
nrow(t1[t1$freq>5,])

qplot(freq,data=t3,geom="histogram",binwidth=1)
