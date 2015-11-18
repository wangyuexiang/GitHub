library(dplyr)
library(cluster)
library(zoo)

##########
##########
### data preparation

# predefined parameters
train.start <- as.Date("2015-5-1")
test.start <- as.Date("2015-8-1")
test.end <- as.Date("2015-8-31")
train.period <- data.frame(Date = seq(train.start, test.start, "day"))
train.period$DOW <- as.POSIXlt(train.period$Date)$wday
test.period <- data.frame(Date = seq(test.start, test.end, "day"))
test.period$DOW <- as.POSIXlt(test.period$Date)$wday

param.days <- GetNumberDays(train.period)

# !!! get running time
start.time <- Sys.time()

trx <- inner_join(transaction2, Grid) %>% filter(Date >= train.start & Date < test.end)

trx <- trx %>% mutate(OD = paste0(Row,"-",Col))

# construct train & test set
train <- trx %>% filter(Date < test.start)
test <- trx %>% filter(Date >= test.start)

# get the ID list
ID.list <- trx %>% group_by(ID) %>% summarise()

# !!! get running time
end.time.preparation <- Sys.time()

##########
##########
### hour heatmap
##########
trx.time <- transaction2 %>%
  mutate(H = round(TimeSor, digits = 0),
         H_2 = H - H %% 2
         )




### no Good result

########
########
#Find the number max of cluster by the density of TimeSor over 24h
########
# For each person
# NbClusterMax <- getNbClusterMax( train )
### ??? trx or train
models.units <- getModel.units( trx )
# train <- inverse.after.SO(train)
# test <- inverse.after.SO(test)

#Model 0 (decades) : BENCHMARK
result.model.decade.0 <- Model.for.a.decade(train,0,models.units)
# !!! get running time
end.time.model.0 <- Sys.time()
#Model 1 (decades)	Time-Space
result.model.decade.1 <- Model.for.a.decade(train,1,models.units)
#Model 2 (decades) OD -> Space -> Time
result.model.decade.2 <- Model.for.a.decade(train,2,models.units)

##########
# evaluation
##########
####################
####################
### evalutaion model.00
test.model.0 <- GetResult(test, result.model.decade.0)
test.model.0$ModelDecade <- 0
ind.model.0 <- GetInd(test.model.0, result.model.decade.0)
ind.model.0$ModelDecade <- 0
### evalutaion model.01
test.model.1 <- GetResult(test, result.model.decade.1)
test.model.1$ModelDecade <- 1
ind.model.1 <- GetInd(test.model.1, result.model.decade.1)
ind.model.1$ModelDecade <- 1
### evalutaion model.02
test.model.2 <- GetResult(test, result.model.decade.2)
test.model.2$ModelDecade <- 2
ind.model.2 <- GetInd(test.model.2, result.model.decade.2)
ind.model.2$ModelDecade <- 2

##########
# compare model results
##########
Ind <- rbind(ind.model.0, ind.model.1, ind.model.2)
Ind <- inner_join ( Ind, models.units)

Ind.final <- Ind %>% 
  group_by(ID) %>% 
  summarise( Model = ModelDecade[Ind == max(Ind)][1]*10 + model[Ind == max(Ind)][1])

result <- rbind(result.model.decade.0, result.model.decade.1, result.model.decade.2)

result.final <- inner_join(result, Ind.final)
# write.table(result.final, file="result.csv", sep = ";", row.names = F, quote = F)

result.final$Model <- NULL

result.final <- result.final %>%
  arrange (ID, desc(noPsg), Tmin) %>%
  filter(noPsg > 5)
# write.table(result.final.treated, file="result.treated.csv", sep = ";", row.names = F, quote = F)

### other action
# temp1 <- ref %>% transmute(ID = as.character(Nom), N.badge = paste0(Ste,N.badge), EVA)
temp <- result.final %>% 
  arrange (ID, desc(noPsg), Tmin) %>%
  filter((Tmax - Tmin) <= 1.5)

ID.list <- trx %>% group_by(ID,Badge,EVA) %>% summarise() 
temp <- inner_join(ID.list, temp)

# output: csv file
write.table(temp, file="result.treated.csv", sep = ";", row.names = F, quote = F)

# !!! get running time
end.time <- Sys.time()

time.taken.preparation <- end.time.preparation - start.time
time.taken.model.0 <- end.time.model.0 -  end.time.preparation
time.taken <- end.time - end.time.model.0
time.taken.preparation 
time.taken.model.0     
time.taken  
# 11.54938 mins for Time-Space Model
# 1.286145 hours for geographic one