
# remove unuseful data set after Rmd
rm(centers1, centers2, cl1, cl2, gg1, gg2, within.ss, t.kmeans)
rm(centers, T.aprem, T.matin)

##########
# viz: test result
ggplot(test_decompose) + geom_point(aes(Date, TimeSor, col = as.factor(result))) + facet_wrap(~ID) + ggtitle("Test Result")
ggplot(train_decompose) + geom_point(aes(Date, TimeSor, col = as.factor(result))) + facet_wrap(~ID) + ggtitle("Test Result")

temp <- train_decompose
temp$result <- 0
for (i in 1:nrow(result)){
  temp[
    temp$ID   == result$ID[i] &
      temp$Entr == result$Entr[i]&
      temp$Sor  == result$Sor[i]&
      temp$DOW  == result$DOW[i]&
      temp$TimeSor >= result$Tmin[i]&
      temp$TimeSor <= result$Tmax[i],
    "result"]<-1
}
##########
# viz: train result
ggplot(temp) + geom_point(aes(Date, TimeSor, col = as.factor(result))) + facet_wrap(~ID) + ggtitle("Train Result")
ggplot(VIP2) + geom_point(aes(Date, TimeSor)) + facet_wrap(~ID) + ggtitle("VIP2")


##########
# viz: result
ggplot(result) + 
  geom_point(aes(DOW, Tmin, col = "Tmin")) +
  geom_point(aes(DOW, Tmax, col = "Tmax")) +
  geom_segment(aes(x=DOW, xend=DOW, y=Tmin, yend=Tmax)) +
  facet_wrap(~ID) + ggtitle("Result: Time Interval by DOW")









##########
# get Ind3

test.period <- data.frame(Date = seq(as.Date("2015-5-1"), as.Date("2015-5-28"), "day"))
test.period$DOW <- as.POSIXlt(test.period$Date)$wday

result$DOW <- as.numeric(result$DOW)
temp <- inner_join(test.period, result[,-(5:6)], by="DOW")
temp$Mark <- 0
temp$DOW <- as.character(temp$DOW)
# for(i in 1:nrow(test_decompose)){
#   temp[
#     temp$ID   == test_decompose$ID[i] &&
#     temp$Entr == test_decompose$Entr[i &&
#     temp$Sor  == test_decompose$Sor[i]&&
#     temp$DOW  == test_decompose$DOW[i]&&
#     temp$Tmin <= test_decompose$TimeSor[i]&&
#     temp$Tmax >= test_decompose$TimeSor[i],
#   
#     "Mark"]<-1
# }
# count(temp,Mark)


for(i in 1:nrow(temp)){
  for(j in 1:nrow(test_decompose)){
    if(temp$ID[i]   == test_decompose$ID[j] &
       temp$Entr[i] == test_decompose$Entr[j] &
       temp$Sor[i]  == test_decompose$Sor[j] &
       temp$Date[i] == test_decompose$Date[j] &
       temp$TimeSor[i] >= test_decompose$Tmin[j] &
       temp$TimeSor[i] <= test_decompose$Tmax[j]) {
      temp$Mark[i] <- temp$Mark[i] + 1
      break # end for loop
    } # next trx in real
  } # next trx predicted
}


temp.ind <- temp %>%
  filter(Mark == 0)
group_by(ID) %>%
  summarize(Fpos = n())


