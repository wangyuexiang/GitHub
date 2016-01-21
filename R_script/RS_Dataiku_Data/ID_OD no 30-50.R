##########
##########
# ID_OD no 30-50
##########
##########

par(mfrow=c(2,2))
as.numeric( format(as.Date("2014-8-31")+3, "%U"))



temp1<-Data_2014_freq[Data_2014_freq$no<50 & Data_2014_freq$no>=30 & Data_2014_freq$Entr!=25053001,]


temp<-Data_2014[Data_2014$ID== 25004000251400008 
                & Data_2014$Entr==25053001 
                & Data_2014$Sor==25053178,]

temp<-Data_2014[Data_2014$ID== 25004000248500040 
                & Data_2014$Entr==25011080 
                & Data_2014$Sor==25011081,]

temp<-Data_2014[Data_2014$ID== 25004000428900024 
                & Data_2014$Entr==25004209 
                & Data_2014$Sor==25004211,]
hist(temp$Month,breaks=8)
hist(temp$Day,breaks=31)
hist(temp$dow,breaks=7)
hist(temp$Hour,breaks=24)

##########
temp<-Data_2014[Data_2014$ID== 25004000248500040 
                & Data_2014$Entr==25011080 
                & Data_2014$Sor==25011081,]

temp<-Data_2014[Data_2014$ID== 25004000428900024 
                & Data_2014$Entr==25004209 
                & Data_2014$Sor==25004211,]
hist(temp$Month,breaks=8)
hist(temp$Day,breaks=31)
hist(temp$dow,breaks=7)
hist(temp$Hour,breaks=24)


##########
##########
# 25004000428900024
##########
##########

par(mfrow=c(2,4))

temp<-Data_2014_freq[
  Data_2014_freq$ID== 25004000428900024 
  & (Data_2014_freq$Entr==25004211 | Data_2014_freq$Sor==25004211),]
##########
temp<-Data_2014[
  Data_2014$ID== 25004000428900024 
  & (Data_2014$Entr==25004211 ),]
hist(temp$Month,breaks=8)
hist(temp$Day,breaks=31)
hist(temp$dow,breaks=7)
hist(temp$Hour,breaks=24)

temp<-Data_2014[
  Data_2014$ID== 25004000428900024 
  & (Data_2014$Sor==25004211 ),]
hist(temp$Month,breaks=8)
hist(temp$Day,breaks=31)
hist(temp$dow,breaks=7)
hist(temp$Hour,breaks=24)
##########




temp<-Data_2014[
  Data_2014$ID== 25004001118600008 
  & (Data_2014$Entr==25004225 
  & Data_2014$Sor==25004222 
  ),]
hist(temp$Month,breaks=8)
hist(temp$Day,breaks=31)
hist(temp$dow,breaks=7)
hist(temp$Hour,breaks=24)

temp<-Data_2014[
  Data_2014$ID== 25004001118600008 
  & (Data_2014$Entr==25004222 
  & Data_2014$Sor==25004225 
  ),]
hist(temp$Month,breaks=8)
hist(temp$Day,breaks=31)
hist(temp$dow,breaks=7)
hist(temp$Hour,breaks=24)
##########

temp2<-Data[Data$ID==25004001118600008,]

par(mfrow=c(3,4))

temp<-temp2[
  temp2$Year== 2014 
  & temp2$Entr==25004225 
  & temp2$Sor==25004222 ,]
hist(temp$Month,breaks=8)
hist(temp$Day,breaks=31)
hist(temp$dow,breaks=7)
hist(temp$Hour,breaks=24)

temp<-temp2[
  temp2$Year== 2013 
  & temp2$Entr==25004225 
  & temp2$Sor==25004222 ,]
hist(temp$Month,breaks=8)
hist(temp$Day,breaks=31)
hist(temp$dow,breaks=7)
hist(temp$Hour,breaks=24)

temp<-temp2[
  temp2$Year== 2012 
  & temp2$Entr==25004225 
  & temp2$Sor==25004222 ,]
hist(temp$Month,breaks=8)
hist(temp$Day,breaks=31)
hist(temp$dow,breaks=7)
hist(temp$Hour,breaks=24)

count(temp2[temp2$Entr==25004225 
            & temp2$Sor==25004222 ,], "Year")
