

datebreaks <- seq(as.Date("2011-7-1"), as.Date("2014-09-01"), by="6 month")
ggplot(temp, aes(Date, freq)) + 
  geom_line() + xlab("") + ylab("# of Trx") +
  scale_x_date(breaks=datebreaks)

##########
##########
# sample
##########
##########
ID_temp<-unique(as.character(Data_2014$ID))
set.seed(1205);ID<-sample(ID_temp,10)
ID_sample<-as.data.frame(ID)

t1<-proc.time()
for (i in 1:10){
  assign(paste0("sample_",i),subset(Data_2014,Data_2014$ID==ID_sample$ID[i]))
}
t2<-proc.time()
t2-t1

sample_1$dow<-as.POSIXlt(sample_1$Date)$wday
temp<-count(sample_1,"dow")
qplot(temp$dow,temp$freq,ylim=c(0,100))
sample_1_freq<-count(sample_1,c("Entr","Sor"))
sample_1_freq<-sample_1_freq[order(sample_1_freq$freq,decreasing=TRUE),]
sample_1_freq


sample_8$dow<-as.POSIXlt(sample_8$Date)$wday
temp<-count(sample_8,"dow")
qplot(temp$dow,temp$freq,ylim=c(0,100))
sample_8_freq<-count(sample_8,c("Entr","Sor"))
sample_8_freq<-sample_8_freq[order(sample_8_freq$freq,decreasing=TRUE),]
sample_8_freq

sample_5$dow<-as.POSIXlt(sample_5$Date)$wday
temp<-count(sample_5,"dow")
qplot(temp$dow,temp$freq,ylim=c(0,100))
sample_5_freq<-count(sample_5,c("Entr","Sor"))
sample_5_freq<-sample_5_freq[order(sample_5_freq$freq,decreasing=TRUE),]
sample_5_freq

sample_4$dow<-as.POSIXlt(sample_4$Date)$wday
temp<-count(sample_4,"dow")
qplot(temp$dow,temp$freq,ylim=c(0,100))
sample_4_freq<-count(sample_4,c("Entr","Sor"))
sample_4_freq<-sample_4_freq[order(sample_4_freq$freq,decreasing=TRUE),]
sample_4_freq

sample_6$dow<-as.POSIXlt(sample_6$Date)$wday
temp<-count(sample_6,"dow")
qplot(temp$dow,temp$freq,ylim=c(0,100))
sample_6_freq<-count(sample_6,c("Entr","Sor"))
sample_6_freq<-sample_6_freq[order(sample_6_freq$freq,decreasing=TRUE),]
sample_6_freq



count(sample_5[sample_5$Entr==25004211 & sample_5$Sor==25004209,],"Month")
count(sample_5[sample_5$Entr==25004209 & sample_5$Sor==25004211,],"Month")

count(sample_1[sample_1$Entr==25004210 & sample_1$Sor==25004218,],"Month")
count(sample_1[sample_1$Entr==25004210 & sample_1$Sor==25004209,],"Month")


sample_8[sample_8$Entr==25004217 & sample_8$Sor==25004220,]
count(sample_8[sample_8$Entr==25004217 & sample_8$Sor==25004220,],"Month")
count(sample_8[sample_8$Entr==25004217 & sample_8$Sor==25004220,],"dow")


ID_OD_freq_sample_1<-count(ID_OD_sample_1,c("ID","Entr","Sor"))
ID_OD_freq_sample_2<-count(ID_OD_sample_2,c("ID","Entr","Sor"))
ID_OD_freq_sample_3<-count(ID_OD_sample_3,c("ID","Entr","Sor"))
ID_OD_freq_sample_4<-count(ID_OD_sample_4,c("ID","Entr","Sor"))
ID_OD_freq_sample_5<-count(ID_OD_sample_5,c("ID","Entr","Sor"))
ID_OD_freq_sample_6<-count(ID_OD_sample_6,c("ID","Entr","Sor"))
ID_OD_freq_sample_7<-count(ID_OD_sample_7,c("ID","Entr","Sor"))
ID_OD_freq_sample_8<-count(ID_OD_sample_8,c("ID","Entr","Sor"))
ID_OD_freq_sample_9<-count(ID_OD_sample_9,c("ID","Entr","Sor"))

ID_OD_freq_sample_1<-ID_OD_freq_sample_1[order(ID_OD_freq_sample_1$freq,decreasing=TRUE),]
ID_OD_freq_sample_2<-ID_OD_freq_sample_2[order(ID_OD_freq_sample_2$freq,decreasing=TRUE),]
ID_OD_freq_sample_3<-ID_OD_freq_sample_3[order(ID_OD_freq_sample_3$freq,decreasing=TRUE),]
ID_OD_freq_sample_4<-ID_OD_freq_sample_4[order(ID_OD_freq_sample_4$freq,decreasing=TRUE),]
ID_OD_freq_sample_5<-ID_OD_freq_sample_5[order(ID_OD_freq_sample_5$freq,decreasing=TRUE),]
ID_OD_freq_sample_6<-ID_OD_freq_sample_6[order(ID_OD_freq_sample_6$freq,decreasing=TRUE),]
ID_OD_freq_sample_7<-ID_OD_freq_sample_7[order(ID_OD_freq_sample_7$freq,decreasing=TRUE),]
ID_OD_freq_sample_8<-ID_OD_freq_sample_8[order(ID_OD_freq_sample_8$freq,decreasing=TRUE),]
ID_OD_freq_sample_9<-ID_OD_freq_sample_9[order(ID_OD_freq_sample_9$freq,decreasing=TRUE),]

par(mfrow=c(3,3)) 
hist(ID_OD_freq_sample_1$freq)
hist(ID_OD_freq_sample_2$freq)
hist(ID_OD_freq_sample_3$freq)
hist(ID_OD_freq_sample_4$freq)
hist(ID_OD_freq_sample_5$freq)
hist(ID_OD_freq_sample_6$freq)
hist(ID_OD_freq_sample_7$freq)
hist(ID_OD_freq_sample_8$freq)
hist(ID_OD_freq_sample_9$freq)
