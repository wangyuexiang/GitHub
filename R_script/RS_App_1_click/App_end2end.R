

##########
##########
### call packages
library(dplyr)
library(cluster)
library(ggplot2)
library(zoo)
library(gridExtra)
library(knitr)

##########
##########
### data preparation

# Input (Pre-model):
#	trx.app:		n_client n_badge Entr Sor Date DOW WOY TimeEntr TimeSor result
# 		train
#		test
#	ID.list:	ID

# predefined parameters
train.start <- as.Date("2015-1-1")
test.start <- as.Date("2015-6-1")
test.end <- as.Date("2015-6-30")
test.period <- data.frame(Date = seq(test.start, test.end, "day"))
test.period$DOW <- as.POSIXlt(test.period$Date)$wday
# test.period$DOW <- as.character(test.period$DOW)

# !!! get running time
start.time <- Sys.time()

# add SensEntr, SensSor
app.trx <- app.trx %>% left_join(gares.sens)

# combine OD to create trajet
app.trx <- Sens(app.trx)
# app.trx <- SO(app.trx)

# get trx
trx <- app.trx %>% filter(Date >= train.start & Date < test.end)

# construct train & test set
train <- trx %>% filter(Date < test.start)
test <- trx %>% filter(Date >= test.start)

# get the ID list
ID.list <- trx %>% group_by(ID) %>% summarise()

# !!! get running time
end.time.preparation <- Sys.time()


########
#Find the number max of cluster by the density of TimeSor over 24h
# For each person
models.units <- getModel.units( train )
train <- inverse.after.SO(train)
test <- inverse.after.SO(test)

# !!! get running time
t2 <- Sys.time()
#Model 0 (decades) : BENCHMARK
result.model.decade.0 <- Model.for.a.decade(train,0,models.units)
# !!! get running time
t3 <- Sys.time()
#Model 1 (decades)	Time-Space
result.model.decade.1 <- Model.for.a.decade(train,1,models.units)
# !!! get running time
t4 <- Sys.time()
#Model 2 (decades) OD -> Space -> Time
result.model.decade.2 <- Model.for.a.decade(train,2,models.units)
# !!! get running time
t5 <- Sys.time()

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

# output: csv file
result.final$Model <- NULL
### other action

# !!! get running time
end.time <- Sys.time()

temp <- gares %>% transmute(Entr = Cde, GEntr = Lib) 
temp1 <- left_join(result.final, temp)
temp <- gares %>% transmute(Sor = Cde, GSor = Lib) 
temp2 <- left_join(temp1, temp)

result.final.treated <- temp2 %>%
  arrange (as.numeric(ID), desc(noPsg), DOW, Tmin) %>%
  filter(Entr != Sor & noPsg > 10 & (Tmax-Tmin) < 1)

write.table(result.final.treated, file="result.treated.app.csv", sep = ";", row.names = F, quote = F)

# temp <- result.final %>% arrange(as.numeric(ID), desc(noPsg), DOW, Tmin)

temp <- inner_join(result, Ind.final) %>% select(ID,Entr,Sor,Sens,DOW,Tmin,Tmax,Model,noPsg)

temp <- temp %>% filter(noPsg > 10)
temp2 <- temp %>% filter(Model %in% c(2,12,22))
temp1 <- temp %>% filter(Model %in% c(1,11,21)) 
temp3 <- temp1 %>% group_by(ID,Entr,Sor,Sens,Tmin,Tmax,Model,noPsg) %>% summarise(DOW = min(DOW))

temp4 <- rbind(temp2,temp3) %>% arrange(as.numeric(ID), desc(noPsg), Tmin)
write.table(temp4, file="result.csv", sep = ";", row.names = F, quote = F)

##########
##########
### other action
T.matin <- trx %>%
  filter(TimeSor < 12 ) %>%
  group_by(ID, Entr, Sor, Sens) %>%
  summarise(noPsg = n(), T = mean(TimeSor), SD = sd(TimeSor), Tmin = T -SD, Tmax = T + SD)

T.aprem <- trx %>%
  filter(TimeSor < 12 ) %>%
  group_by(ID, Entr, Sor, Sens) %>%
  summarise(noPsg = n(), T = mean(TimeSor), SD = sd(TimeSor), Tmin = T -SD, Tmax = T + SD)

T <- rbind(T.matin, T.aprem)

T %>% filter(noPsg > param.min.noPsg & SD < 1) # param.min.noPsg = 5

temp <- count(T, ID)
ggplot(temp) + geom_bar(aes(n), binwidth = 1)

write.table(trx.after.ID %>% arrange(as.numeric(ID)), file="trx.after.ID.csv", sep = ";", row.names = F, quote = F)
