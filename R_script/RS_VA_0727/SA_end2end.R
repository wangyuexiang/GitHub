##########
##########
# 20150723
# Input - Output
##########
##########
# call packages
library(dplyr)
library(ggplot2)
library(gridExtra)
library(knitr)
##########

##########
# data preparation
##########
# load data
# !!! to be replaced by: csv -> data.frame  
# load("VIP2.RData")

# get history for: CC,FF,NP,PC
# from 2015-1-1 to 2015-5-28
trx <- tbl_df(VIP2 %>% 
  filter(ID %in% c("CC", "FF", "NP", "PC")) %>%
  filter(Date > as.Date("2014-12-31") & Date < as.Date("2015-5-29")))
# remove Lng & Lat
trx <- trx[, -c(9:13)]

# construct train & test set
train <- trx %>% filter(Date < as.Date("2015-5-1"))
test <- trx %>% filter(Date >= as.Date("2015-5-1"))

# get the ID list
ID.list <- trx %>% group_by(ID) %>% summarise()

##########
##########
# model0: ID_OD
##########
matin <- train %>%
  select(ID, Entr, Sor, Date, DOW, TimeSor) %>%
  filter(TimeSor < 12 ) %>%
  group_by(ID, Entr, Sor, DOW) %>%
  summarise(nDOW = n())

aprem <- train %>%
  select(ID, Entr, Sor, Date, DOW, TimeSor) %>%
  filter(TimeSor >= 12 ) %>%
  group_by(ID, Entr, Sor, DOW) %>%
  summarise(nDOW = n())

T.matin <- train %>%
  select(ID, Entr, Sor, Date, DOW, TimeSor) %>%
  filter(TimeSor < 12 ) %>%
  group_by(ID, Entr, Sor, DOW) %>%
  summarise(SD = sd(TimeSor), T = mean(TimeSor),Tmin = T -SD, Tmax = T + SD)

T.aprem <- train %>%
  select(ID, Entr, Sor, Date, DOW, TimeSor) %>%
  filter(TimeSor >= 12 ) %>%
  group_by(ID, Entr, Sor, DOW) %>%
  summarise(SD = sd(TimeSor), T = mean(TimeSor),Tmin = T -SD, Tmax = T + SD)

T.matin <- inner_join(T.matin, matin)
T.aprem <- inner_join(T.aprem, aprem)
T <- rbind(T.matin, T.aprem)

# ??? parameter: 5
T <- T %>% filter(nDOW > 5)
result.model0 <- T[, c(1:4, 7:8)]
result.model0$Model<-0
##########
# evalutaion model0
##########
test0 <- test

test0$result <- 0
for (i in 1:nrow(result.model0)){
  test0[
    test0$ID   == result.model0$ID[i] &
      test0$Entr == result.model0$Entr[i]&
      test0$Sor  == result.model0$Sor[i]&
      test0$DOW  == result.model0$DOW[i]&
      test0$TimeSor >= result.model0$Tmin[i]&
      test0$TimeSor <= result.model0$Tmax[i],
    "result"]<-1
}

# get indicators
test.result.model0 <- test0 %>%
  group_by(ID) %>%
  summarise(Tpos = sum(result[result == 1]), Fneg = n() - Tpos, Ind1 = Tpos/(Tpos+Fneg), Ind2 = Fneg/(Tpos+Fneg) )

# get the final indicator
test.result.model0$Ind <- test.result.model0$Ind1 - test.result.model0$Ind2 / 15
test.result.model0 <- test.result.model0[, c(1, ncol(test.result.model0))]
test.result.model0$Model <- 0
##########
##########
# model1: Time - Space
##########
# call the function to get the troncons
train_decompose <- decompose(train)  

# clustering TimeSor
result <- data.frame(ID="", Entr=0, Sor=0, DOW=0, SD=0, T=0, Tmin=0, Tmax=0, n=0)
for (i in 1:nrow(ID.list)){
  for (j in 0:6){
    temp <- train_decompose %>% filter(ID == ID.list$ID[i] & DOW == j)
    
    # base to be verified
    if(nrow(temp) >= 10) {
      # if not many passages, we will not cluster
      # decide nb of cluster
      n.cluster <- 3
      set.seed(1234)
      temp.kmeans <-   kmeans(temp[, "TimeSor"], centers = n.cluster)
      temp$cluster <- temp.kmeans$cluster
      T <- temp %>%
        group_by(ID, Entr, Sor, DOW, cluster) %>%
        summarise(SD = sd(TimeSor), T = mean(TimeSor),Tmin = T -SD, Tmax = T + SD, n = n())
      T <- T %>% filter(n>1)
      T$cluster <- NULL
      result <- rbind(result, T)
      }
    }
}
result <- result[-1,]
# ??? parameter: 5
result <- result %>% filter(n > 5)
result.model1 <- result[,-c(5:6,9)]
result.model1$Model <- 1
##########
# evalutaion model1
##########
test_decompose <- decompose(test)

test_decompose$result <- 0
for (i in 1:nrow(result)){
  test_decompose[
    test_decompose$ID   == result$ID[i] &
    test_decompose$Entr == result$Entr[i]&
    test_decompose$Sor  == result$Sor[i]&
    test_decompose$DOW  == result$DOW[i]&
    test_decompose$TimeSor >= result$Tmin[i]&
    test_decompose$TimeSor <= result$Tmax[i],
    "result"]<-1
}

# get indicators
test_result <- test_decompose %>%
  group_by(ID) %>%
  summarise(Tpos = sum(result[result == 1]), Fneg = n() - Tpos, Ind1 = Tpos/(Tpos+Fneg), Ind2 = Fneg/(Tpos+Fneg) )

# get the final indicator
test_result$Ind <- test_result$Ind1 - test_result$Ind2 / 15
test.result.model1 <- test_result[, c(1, ncol(test_result))]
test.result.model1$Model <- 1


##########
# compare model results
##########
Ind <- rbind(test.result.model0, test.result.model1)
Ind <- Ind %>% group_by(ID) %>% summarise(Model = sum(Model[Ind == max(Ind)]))

result <- rbind(result.model0, result.model1)
result <- inner_join(result, Ind)
result$Model <- NULL

write.table(result, file="result.csv", sep = ";", row.names = F, quote = F)
