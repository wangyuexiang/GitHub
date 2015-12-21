####################
# Alog1
####################
# Run Algo1_TimeSpace
#
# Input: 
#   Args (from Parameter/Param_Algo2_main.csv :
#     filename.Input
#     day.start
#     day.end
#   Input:
#     transaction history: Input/filename.Input
# Output:
#   result of model Time Space: Reference/Algo1_ + Input + V + time
####################

##########
### Step 0: Prepare
##########
# load package
library(dplyr)
library(cluster)

# define Repository to get Parameters
ParamRepo <- NA
Args <- commandArgs(trailingOnly = TRUE)
ParamRepo <- Args[1]

if(is.na(ParamRepo)){
  # get arguments 
  args <- read.table("Parameters/Param_Algo1.csv",sep = ";", header=TRUE) 
} else {
  # get arguments 
  args <- read.table(paste0(ParamRepo,"/Param_Algo1.csv"),sep = ";", header=TRUE) 
}

filename.Input <- as.character(args[1,1])
day.start <- as.Date(as.character(args[1,2]))
day.end <- as.Date(as.character(args[1,3]))

rm(args)
rm(Args)

# get Reference data from Reference/
# get sens
sens = read.table("Reference/Ref_sens.csv",sep = ";", header=TRUE)

# input 
input <- read.table(paste0("Input/",filename.Input), header = T, sep = ";") %>% tbl_df 

# load function
source('Algo1_Functions.R', encoding = 'UTF-8')
source('Algo1_DataPreparation.R', encoding = 'UTF-8')

# prepare period
train.period <- data.frame(Date = seq(day.start, day.end - 30, "day"))
train.period$DOW <- as.POSIXlt(train.period$Date)$wday
test.period <- data.frame(Date = seq(day.end - 30, day.end, "day"))
test.period$DOW <- as.POSIXlt(test.period$Date)$wday

# divide the hisory into: train & test
train <- output %>% filter(Date < day.end - 30)
test <- output %>% filter(Date >= day.end - 30)
ID.list <- output %>% group_by(ID) %>% summarise()

##########
### Step 1: train & evaluate model
##########
# find first digit for Model
models.units <- getModel.units( output )

# Model 0 (decades) : BENCHMARK
result.model.decade.0 <- Model.for.a.decade(train,0,models.units)
### evalutaion model.00
test.model.0 <- GetResult(test, result.model.decade.0)
if(nrow(test.model.0) > 0) test.model.0$ModelDecade <- 0
ind.model.0 <- GetInd(test.model.0, result.model.decade.0)
if(nrow(ind.model.0) > 0)  ind.model.0$ModelDecade <- 0

# Model 1 (decades)	Time-Space
result.model.decade.1 <- Model.for.a.decade(train,1,models.units)
### evalutaion model.01
test.model.1 <- GetResult(test, result.model.decade.1)
if(nrow(test.model.1) > 0) test.model.1$ModelDecade <- 1
ind.model.1 <- GetInd(test.model.1, result.model.decade.1)
if(nrow(ind.model.1) > 0)  ind.model.1$ModelDecade <- 1

# Model 2 (decades) OD -> Space -> Time
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

result.TS <- inner_join(result, Ind.final) %>%
  arrange (ID, desc(noPsg), Tmin) %>%
  select(-Model) %>%
  mutate(ID = as.character(as.numeric(ID))) %>%
  filter(noPsg > 5) %>%
  distinct

rm(Ind, Ind.final, models.units)
rm(ID.list,
   test,train,
   test.period,train.period)

##########
### Step 2: Output in Output/
##########
inputName <-  read.table(text = filename.Input, sep=".")$V1 %>% as.character
time <- Sys.time() %>% format(format = "%Y%m%d_%H%M")

write.table(result.TS, 
            paste0("Output/Algo1_",inputName,"_V",time,".csv"),
            sep=";",row.name=FALSE,quote=FALSE)

rm(inputName,time)

rm(list = ls())
