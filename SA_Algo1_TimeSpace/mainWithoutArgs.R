### load package
library(dplyr)
library(cluster)

InputFileName <- "App.csv"
OutputFileName <- "Output.csv"

start <- as.Date("2015-5-1")
end <- as.Date("2015-8-31")

### input 
input <- read.table(InputFileName, header = T, sep = ";") %>% tbl_df 

### load function
source('DataPreparation.R', encoding = 'UTF-8')
source('Functions.R', encoding = 'UTF-8')

####################
####################

### prepare period
train.period <- data.frame(Date = seq(start, end - 30, "day"))
train.period$DOW <- as.POSIXlt(train.period$Date)$wday
test.period <- data.frame(Date = seq(end - 30, end, "day"))
test.period$DOW <- as.POSIXlt(test.period$Date)$wday

### train & test
train <- output %>% filter(Date < end - 30)
test <- output %>% filter(Date >= end - 30)
ID.list <- output %>% group_by(ID) %>% summarise()

### model & eval
models.units <- getModel.units( output )

#Model 0 (decades) : BENCHMARK
result.model.decade.0 <- Model.for.a.decade(train,0,models.units)
### evalutaion model.00
test.model.0 <- GetResult(test, result.model.decade.0)
if(nrow(test.model.0) > 0) test.model.0$ModelDecade <- 0
ind.model.0 <- GetInd(test.model.0, result.model.decade.0)
if(nrow(ind.model.0) > 0)  ind.model.0$ModelDecade <- 0

#Model 1 (decades)	Time-Space
result.model.decade.1 <- Model.for.a.decade(train,1,models.units)
### evalutaion model.01
test.model.1 <- GetResult(test, result.model.decade.1)
if(nrow(test.model.1) > 0) test.model.1$ModelDecade <- 1
ind.model.1 <- GetInd(test.model.1, result.model.decade.1)
if(nrow(ind.model.1) > 0)  ind.model.1$ModelDecade <- 1

#Model 2 (decades) OD -> Space -> Time
result.model.decade.2 <- Model.for.a.decade(train,2,models.units)
### evalutaion model.02
test.model.2 <- GetResult(test, result.model.decade.2)
if(nrow(test.model.2) > 0) test.model.2$ModelDecade <- 2
ind.model.2 <- GetInd(test.model.2, result.model.decade.2)
if(nrow(ind.model.2) > 0)  ind.model.2$ModelDecade <- 2

### compare model results
Ind <- rbind(ind.model.0, ind.model.1, ind.model.2)
Ind <- inner_join ( Ind, models.units)
Ind.final <- Ind %>% 
  group_by(ID) %>% 
  summarise( Model = ModelDecade[Ind == max(Ind)][1]*10 + model[Ind == max(Ind)][1])

result <- rbind(result.model.decade.0, result.model.decade.1, result.model.decade.2)

rm(result.model.decade.0, test.model.0, ind.model.0,
   result.model.decade.1, test.model.1, ind.model.1,
   result.model.decade.2, test.model.2, ind.model.2)

result.final <- inner_join(result, Ind.final) %>%
  arrange (ID, desc(noPsg), Tmin) %>%
  select(-Model) %>%
  mutate(ID = as.character(as.numeric(ID))) %>%
  filter(noPsg > 5)

rm(Ind, Ind.final, models.units)
rm(ID.list,
   test,train,
   test.period,train.period)

### output
write.table(result.final, OutputFileName,sep=";",row.name=FALSE,quote=FALSE)
