####################
# Alog
####################
# Run Algo1_TimeSpace
# Get all Trx not in result.TS
# Run Algo2_ZoneWindow over these Trx 
#
# Input: 
#     filename.Input 
#     ParamRepo
#   Args (from Parameter/Param_common.csv :
#			limit.Algo1.noPsg
#     limit.Algo2.GridPer
#     limit.Algo2.ActiveDay
#     day.start
#     day.end
#   Input:
#     transaction history: Input/filename.Input
# Output:
#   result of model Time Space: 
#			without filtrage & with noPsg: Output/Algo1_ + (Input) + V + (time).log
#			with filtrage & without noPsg: Output/Algo1_ + (Input) + V + (time).csv
#   result of model Zone Window: 
#     reference Zone-Grid: Output/Algo2_ + (Input) + V + (time) + Zone.csv
#     without filtrage & with noPsg: Output/Algo2_ + (Input) + V + (time) + Window.log
#     with filtrage & without noPsg for day:   Output/Algo2_ + (Input) + V + (time) + Day.csv
#     															for night: Output/Algo2_ + (Input) + V + (time) + Night.csv
####################

##########
### Step 0.1: General Prepare 
##########
# load package
.libPaths("RPackages")
library(dplyr)
library(cluster)

# define repository to get Parameters
ParamRepo <- NA
# define input filename to get History of transactions
filename.Input <- NA
Args <- commandArgs(trailingOnly = TRUE)

# get these two parameters from script
if(length(Args) > 1){
  filename.Input <- Args[1]
  ParamRepo <- Args[2]
} else if (length(grep(".csv",Args[1])) > 0){
  filename.Input <- Args[1]
} else{
  filename.Input <- "VIP_v20151210.csv"
  ParamRepo <- Args[1]
}

# get the other parameters from the specified repository
if(is.na(ParamRepo)){
  # get arguments 
  args <- read.table("Parameters/Param_common.csv",sep = ";", header=TRUE) 
  # args <- read.table("Parameters.Ete2015/Param_common.csv",sep = ";", header=TRUE) 
} else {
  # get arguments 
  args <- read.table(paste0(ParamRepo,"/Param_common.csv"),sep = ";", header=TRUE) 
}

# limit args: under which the OD or Zone are considered unimportant
limit.Algo1.noPsg <- args$limit.Algo1.noPsg[1]
limit.Algo2.GridPer <- args$limit.Algo2.GridPer[1]
limit.Algo2.ActiveDay <- args$limit.Algo2.ActiveDay[1]
limit.Algo2.Day <- args$limit.Algo2.Day[1]				# by default = 5
limit.Algo2.Night <- args$limit.Algo2.Night[1] 		# by default = 3
Algo2.DayStart <- args$Algo2.DayStart[1] 					# by default = 6
Algo2.DayEnd <- args$Algo2.DayEnd[1]              # by default = 22
# limit.WindowFreq <- 5
day.start <- as.Date(as.character(args$day.start[1]))
day.end <- as.Date(as.character(args$day.end[1]))
# filter input file: decide whether or not ID with less trx will be removed
filter <- args$filter[1]
logFile <- args$logFile[1]

rm(args)
rm(Args)

if(day.start >= day.end) stop("La date debut est apres la date fin!")

# get Reference data from Reference/
# get sens
sens = read.table("Reference/Ref_sens.csv",sep = ";", header=TRUE)

# input 
input <- read.table(paste0("Input/",filename.Input), header = T, sep = ";") %>% tbl_df 

# load function
source('Algo1_Functions.R', encoding = 'UTF-8')
source('Algo2_Functions.R', encoding = 'UTF-8')

##########
### Step 0.2: Prepare input file
##########
#   create ID from Client + Porteur
#   correct Date type
#   correct Sens
trx <- input %>%
  mutate(
    ID = as.character(Client * 100000 + Porteur),
    DateSor = as.Date(as.character(DateSor)),
    Sens = ifelse(Entr == 0,
                  ifelse(VoieSor <=20, 1,2),
                  0)
    ) 

# construct segmentation for a summary of each client with TotalNoPsg, TotalNoActiveDay
segmentation <- trx %>% group_by(ID) %>%
  summarize(TotalNoPsg= n(),
            TotalNoActiveDay = n_distinct(DateSor),
            TotalNoPsgPerActiveDay = TotalNoPsg / TotalNoActiveDay)
	
trx <- trx %>% filter(DateSor > day.start & DateSor <= day.end)

inputName <-  read.table(text = filename.Input, sep=".")$V1 %>% as.character
if (nrow(trx) == 0) {
  print(paste0("Aucun trajets sur la periode du ", day.start, " au ", day.end, " !"))
  
  result.TS <- data.frame(
    ID = character(),
    OD = character(),
    DOW = integer(),
    Tmin = double(),
    Tmax = double(),
    noPsg = double()
    )
  
  result.TS.before <- result.TS
  
  result.ZW.Zone <- data.frame(
    ID = character(),
    Zone = double(),
    Grid = character()
    )
  
  result.ZW.Day <- data.frame(
    ID = character(),
    Zone = double()
  )
  
  result.ZW.Night <- result.ZW.Day
  
  result.ZW.Frequency <- data.frame(
    ID = character(),
    Zone = double(),
    Day = integer(),
    Night = integer()
  )
  
  segmentation <- data.frame(
    ID = character(),
    TotalNoPsg = integer(),
    TotalNoActiveDay = integer(),
    TotalNoPsgPerActiveDay = double(),
    NoPsgInPeriod = double(),
    NoActiveDayInPeriod = double(),
    NoPsgPerActiveDayInPeriod = double(),
    NoOD10 = double(),
    Algo1_NoOD = integer(),
    Algo2_NoZone = integer()
  )
} else {

# add NoPsgInPeriod, NoActiveDayInPeriod to segmentation
t <- trx %>% group_by(ID) %>% 
  summarise(NoPsgInPeriod = n(),
            NoActiveDayInPeriod = n_distinct(DateSor),
            NoPsgPerActiveDayInPeriod = NoPsgInPeriod / NoActiveDayInPeriod)

segmentation <- left_join(segmentation, t)
# add NoOD10 to segmentation
t <- trx %>% group_by(ID, Entr, Sor, Sens) %>% summarise(noPsg = n())
# TODO: replace 10 with parameter modifiable or proportional to Period
t1 <- t %>% filter(noPsg >= 10) %>% group_by(ID) %>% summarise(NoOD10 = n())
segmentation <- left_join(segmentation, t1)
rm(t,t1)

# filter
if(filter == TRUE) source('Algo1_DataPreparation.R', encoding = 'UTF-8')
if(nrow(trx) == 0) stop("Aucun trajet apres le filtrage, mettez le parametre filter = FALSE!")

# add sens & create OD
trx <- trx %>% mutate(VoieSor = ifelse(Entr == 0, VoieSor, 0))
trx <- trx %>% left_join(sens)
trx <- trx %>% mutate(SensEntr = ifelse(is.na(SensEntr), 0, SensEntr),
                      SensSor = ifelse(is.na(SensSor), 0, SensSor))
rm(sens)
trx <- trx %>% mutate(OD = paste0(Entr,"-",Sor,"-",SensEntr,"-",SensSor))

output <- trx
rm(trx)

if( (day.end - day.start) <= 30 ) stop("La periode est trop courte!")

# prepare period
train.period <- data.frame(DateSor = seq(day.start, day.end - 30, "day"))
train.period$DOW <- as.POSIXlt(train.period$DateSor)$wday
test.period <- data.frame(DateSor = seq(day.end - 30, day.end, "day"))
test.period$DOW <- as.POSIXlt(test.period$DateSor)$wday

# divide the hisory into: train & test
train <- output %>% filter(DateSor < day.end - 30)
test <- output %>% filter(DateSor >= day.end - 30)
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

result.TS.before <- inner_join(result, Ind.final) %>%
  arrange (ID, desc(noPsg), Tmin) %>%
  select(-Model) %>%
  mutate(ID = as.character(as.numeric(ID))) %>%
	distinct

result.TS <- result.TS.before %>% filter(noPsg > limit.Algo1.noPsg)

rm(Ind, Ind.final, models.units)
rm(ID.list,
   test,train,
   test.period,train.period)

# add Algo1_NoOD (number of OD in result.TS) to segmentation
t <- result.TS %>% group_by(ID) %>% summarise(Algo1_NoOD = n_distinct(OD))
segmentation <- left_join(segmentation, t)

##########
### Step 2: Prepare for Algo2 - Get Trx not in Result.TS
##########
t <- input %>%
  mutate(
    ID = as.character(Client * 100000 + Porteur),
    DateSor = as.Date(as.character(DateSor)),
    Sens = ifelse(Entr == 0,
                  ifelse(VoieSor <=20, 1,2),
                  0)) %>%
  filter(DateSor >= day.start & DateSor <= day.end)

# find trx in result.TS
if(nrow(result.TS) > 0){
  t1 <- result.TS %>% transmute(OD =as.character(OD))
  t2 <- read.table(text = t1$OD,sep="-") %>% tbl_df %>%
    transmute(Entr = V1, Sor = V2)
  t1 <- cbind(result.TS,t2) %>% tbl_df
  
  t3 <- inner_join(t,t1)
  t4 <- t3 %>% filter(TimeSor <= Tmax & TimeSor >= Tmin)
  t5 <- t4 %>% select(Ste: Sens) %>% distinct
  
  t5$TS <- TRUE
  trx <- left_join(t,t5) %>% filter(is.na(TS)) %>% select(-TS)
  rm(t1,t2,t3,t4,t5)
} else {trx <- t}

# get Reference data from Reference/
# run Algo2_makeGrid to have "Ref_ODtoGrid.csv" and "Ref_GridLimit.csv" 
# get ODtoGrid
ODtoGrid <- read.table("Reference/Ref_ODtoGrid.csv", header = T, sep = ";") %>% 
  tbl_df %>%
  mutate(Grid = as.character(Grid))
# get GridLimit
GridLimit <- read.table("Reference/Ref_GridLimit.csv", header = T, sep = ";") %>% 
  tbl_df %>%
  mutate(Grid = as.character(Grid))

##########
### Step 3: find Zone frequently visited
##########
# transform Entr-Sor to Grid
trxGrid <- trx %>% inner_join(ODtoGrid)

# find number of active day for each ID
t1 <- trxGrid %>%
  group_by(ID) %>%
  summarise(ActiveDay = n_distinct(DateSor))
# find number of active day for each ID,Grid
t2 <- trxGrid %>%
  group_by(ID, Grid) %>%
  summarise(Day = n_distinct(DateSor))

# get ID,Grid with (Grid frequently visited)
#   - ActiveDay >= limit.Algo2.ActiveDay 
#   - Per >= limit.Algo2.GridPer
trxGridActive <- inner_join(t1,t2) %>% 
    mutate(Per = Day / ActiveDay) %>%
    filter(ActiveDay >= limit.Algo2.ActiveDay,
           Per >= limit.Algo2.GridPer)

rm(t1,t2)


##########
### Step 4: connect Zone  
##########
# connect Grid to make Zone
t <- inner_join(trxGridActive,
                GridLimit %>% select(Grid, Row, Col)) %>%
  ZoneLabel

result.ZW.Zone <- t %>% select(ID, Zone, Grid)

# add Algo2_NoZone (number of Zone in result.ZW.Zone) to segmentation
t1 <- result.ZW.Zone %>% group_by(ID) %>% summarise(Algo2_NoZone = max(Zone))
segmentation <- left_join(segmentation, t1)

##########
### Step 5: get Hour-heatmap for OneZone
##########
# get all trx with Entr_Sor info passing these gridActive
t <- inner_join(t %>% select(ID, Grid, Zone),
                trxGrid) %>%
								select(-Grid) %>% ungroup %>% distinct

# add H
t <- t %>% mutate(H = round(TimeSor, digits = 0))
# add Time: Day/Night
if(nrow(t) > 0){
  t$Time <- "Night"
  if(nrow(t %>% filter(H >= Algo2.DayStart & H <= Algo2.DayEnd)) > 0) t[t$H >= Algo2.DayStart & t$H <= Algo2.DayEnd,]$Time <- "Day"
}

# get result
t1 <- t %>%
	group_by(ID, Zone) %>%
	summarise(Day = sum(Time == "Day"),
						Night = sum(Time == "Night")
	)
result.ZW.Frequency <- t1

# get result with limit.Algo2.Day/Night
result.ZW.Day <- t1 %>% filter(Day > limit.Algo2.Day) %>% select(ID, Zone) 
result.ZW.Night <- t1 %>% filter(Night > limit.Algo2.Night) %>% select(ID, Zone) 

rm(t,t1,ODtoGrid,GridLimit)
}

##########
### Step 6: Output in Output/
##########
inputName <-  read.table(text = filename.Input, sep=".")$V1 %>% as.character
time <- Sys.time() %>% format(format = "%Y%m%d_%H%M")

# save result of Algo1_TS
write.table(result.TS,        paste0("Output/Algo1_TS_",inputName,"_V",time,".csv"),sep=";",row.name=FALSE,quote=FALSE)
# save result of Algo2_ZW
write.table(result.ZW.Zone,      paste0("Output/Algo2_ZW_",inputName,"_V",time,"_Zone.csv"),sep=";",row.name=FALSE,quote=FALSE)
write.table(result.ZW.Day,       paste0("Output/Algo2_ZW_",inputName,"_V",time,"_Day.csv"),sep=";",row.name=FALSE,quote=FALSE)
write.table(result.ZW.Night,     paste0("Output/Algo2_ZW_",inputName,"_V",time,"_Night.csv"),sep=";",row.name=FALSE,quote=FALSE)

# save inter-step result in .log files depending on the parameter logFile
if (logFile){
	write.table(result.TS.before, paste0("Output/Algo1_TS_",inputName,"_V",time,".log"),sep=";",row.name=FALSE,quote=FALSE)
	write.table(result.ZW.Frequency, paste0("Output/Algo2_ZW_",inputName,"_V",time,"_Frequence.log"),sep=";",row.name=FALSE,quote=FALSE)
}

# save summary of Result
segmentation[is.na(segmentation)] <- 0
write.table(segmentation, paste0("Output/Algo_Summary_",inputName,"_V",time,".csv"),sep=";",row.name=FALSE,quote=FALSE)

rm(inputName,time)
