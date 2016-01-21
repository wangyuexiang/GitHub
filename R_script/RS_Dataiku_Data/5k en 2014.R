##########
##########
# 5k avec le #_Trx le plus grand en 2014
##########
##########

Test_set<-noPassage[1:5000,]

sum(Test_set$freq_2014);sum(Test_set$Total);sum(Test_set[,8:14]);sum(Test_set[,15:19]);sum(Test_set[,15:16])

Test_set$P_Trx_100<-round(Test_set$Trx_100/Test_set$freq_2014,digits=2)

useful<-c(1,2,8:ncol(Test_set))
n<-20
Test_set[1:20,useful]

plot(Test_set$freq_2014)
abline(h=500)
abline(h=200)

t1<-proc.time()
Test_ID_OD<-merge(x=Test_set,y=Data,by="ID",all.x=TRUE)
t2<-proc.time()
t2-t1

Test_set[sample(1:nrow(Test_set),size=20),useful]

plot.new()
plot(range(5),range(80),xlim=c(0,8),ylim=c(10,120),type='n')
colors<-rainbow(10)
for(i in 1:10 ){
  lines(1:7,as.integer(Test_set[812+i,8:14]),col=colors[i])
}

temp<-Data[Data$ID==25004056533700000,]
temp1<-count(temp,c("Entr","Sor"))
temp1<-temp1[order(temp1$freq,decreasing=TRUE),]

temp[ temp$Sor==25006012,]
temp[temp$Entr==25004218 & temp$Sor==25004220,]
temp2<-temp[temp$Entr==25004218 & temp$Sor==25004220,]
count(temp2,c("Year","Month"))

temp2<-temp[temp$Entr==25004220 & temp$Sor==25004218,]
count(temp2,c("Year","Month"))

count(temp[temp$Entr==00000000 & temp$Sor==25006012,],"Month")

temp<-Data_2014[Data_2014$ID=="25004170437700000",]
count(temp,c("Entr","Sor"))

write.table(noPassage,file="user.csv",sep=",")

qplot(freq_2014,data=Test_set,binwidth=5,geom="histogram",xlim=c(1,1000))

nrow(Test_set[Test_set$freq_2014>500,])/5000
nrow(Test_set[Test_set$freq_2014<=500 & Test_set$freq_2014>400,])/5000
nrow(Test_set[Test_set$freq_2014<=400 & Test_set$freq_2014>300,])/5000
nrow(Test_set[Test_set$freq_2014<=300 & Test_set$freq_2014>200,])/5000
nrow(Test_set[Test_set$freq_2014<=250 & Test_set$freq_2014>200,])/5000

qplot(freq_2014,data=Test_set,binwidth=50,geom="histogram",xlim=c(1,1000))
qplot(freq_2014,data=Test_set,binwidth=50,geom="histogram")
      