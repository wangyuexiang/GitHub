####################
# Alog
####################
# Run Algo1_TimeSpace
# Get all Trx not in result.TS
# Run Algo2_ZoneWindow over these Trx 
#
# Input: 
#   Args (from Parameter/Param_Algo2_main.csv :
#     filename.Input: 
#			limit.Algo1.noPsg
#     limit.Algo2.ZonePer
#     limit.Algo2.ActiveDay
#     day.start
#     day.end
#   Input:
#     transaction history: Input/filename.Input
# Output:
#   result of model Time Space: Reference/Algo1_ + Input + V + time
#   result of model Zone Window: 
#       Reference/Algo2_ + Input + V + time + Zone
#       Reference/Algo2_ + Input + V + time + Window
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
  args <- read.table("Parameters/Param_Algo.csv",sep = ";", header=TRUE) 
} else {
  # get arguments 
  args <- read.table(paste0(ParamRepo,"/Param_Algo.csv"),sep = ";", header=TRUE) 
}

# attribute args
limit.Algo1.noPsg <- args$limit.Algo1.noPsg[1]
limit.Algo2.GridPer <- args$limit.Algo2.GridPer[1]
limit.Algo2.ActiveDay <- args$limit.Algo2.ActiveDay[1]
# limit.WindowFreq <- 5
day.start <- as.Date(as.character(args$day.start[1]))
day.end <- as.Date(as.character(args$day.end[1]))
# filter input file
filter <- args$filter[1]

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

##########
### Step 0.2: Prepare input file
##########
#   create ID from Badge + Porteur
#   correct Date type
#   correct Sens
trx <- input %>%
  mutate(
    ID = as.character(Badge * 100000 + Porteur),
    Date = as.Date(as.character(Date)),
    Sens = ifelse(Entr == 0,
                  ifelse(Voie <=20, 1,2),
                  0)
    ) 

# construct segmentation for a summary of each client with TotalNoPsg, TotalNoActiveDay
segmentation <- trx %>% group_by(ID) %>%
  summarize(TotalNoPsg= n(),
            TotalNoActiveDay = n_distinct(Date),
            TotalNoPsgPerActiveDay = TotalNoPsg / TotalNoActiveDay)
	
trx <- trx %>% filter(Date > day.start & Date <= day.end)

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
  
  result.ZW <- data.frame(
    ID = character(),
    DOW = integer(),
    H = double(),
    noPsg = integer()
    )
  
  trxZoneActive <- data.frame(
    ID = character(),
    ActiveDay = integer(),
    Grid = character(),
    Day = integer(),
    Per = double()
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
    NoODTS = double()
  )
} else {

# add NoPsgInPeriod, NoActiveDayInPeriod to segmentation
t <- trx %>% group_by(ID) %>% 
  summarise(NoPsgInPeriod = n(),
            NoActiveDayInPeriod = n_distinct(Date),
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
trx <- trx %>% mutate(Voie = ifelse(Entr == 0, Voie, 0))
trx <- trx %>% left_join(sens)
trx <- trx %>% mutate(SensEntr = ifelse(is.na(SensEntr), 0, SensEntr),
                      SensSor = ifelse(is.na(SensSor), 0, SensSor))
rm(sens)
trx <- trx %>% mutate(OD = paste0(Entr,"-",Sor,"-",SensEntr,"-",SensSor))

output <- trx
rm(trx)

if( (day.end - day.start) <= 30 ) stop("La periode est trop courte!")

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
  filter(noPsg > limit.Algo1.noPsg) %>%
  distinct

rm(Ind, Ind.final, models.units)
rm(ID.list,
   test,train,
   test.period,train.period)

# add NoODTS (number of OD in result.TS) to segmentation
t <- result.TS %>% group_by(ID) %>% summarise(NoODTS = n_distinct(OD))
segmentation <- left_join(segmentation, t)

##########
### Step 2: Prepare for Algo2 - Get Trx not in Result.TS
##########
t <- input %>%
  mutate(
    ID = as.character(Badge * 100000 + Porteur),
    Date = as.Date(as.character(Date)),
    Sens = ifelse(Entr == 0,
                  ifelse(Voie <=20, 1,2),
                  0)) %>%
  filter(Date >= day.start & Date <= day.end)

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
  trx.Algo2 <- left_join(t,t5) %>% filter(is.na(TS)) %>% select(-TS)
  rm(t1,t2,t3,t4,t5)
  
  trx <- trx.Algo2
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
  summarise(ActiveDay = n_distinct(Date))
# find number of active day for each ID,Grid
t2 <- trxGrid %>%
  group_by(ID, Grid) %>%
  summarise(Day = n_distinct(Date))

# get ID,Grid with (Grid frequently visited)
#   - ActiveDay >= limit.Algo2.ActiveDay 
#   - Per >= limit.Algo2.GridPer
trxGridActive <- inner_join(t1,t2) %>% 
  mutate(Per = Day / ActiveDay) %>%
  filter(ActiveDay >= limit.Algo2.ActiveDay,
         Per >= limit.Algo2.GridPer)
rm(t1,t2)

##########
### Step 4: find ID with only one big zone  
##########
# get Grid detailed info for trxGridActive
t <- inner_join(trxGridActive, GridLimit)
# group them by ID
t <- t %>% group_by(ID)

# Get 4 points
#   NW  NE (NorthWest, NorthEast)
#   SW  SE (SouthWest, SouthEast)
t1 <- t %>% arrange(Row, Col)             %>% select(Row, Col)  %>% rename(R_NW = Row, C_NW = Col) %>% slice(1) %>% ungroup
t2 <- t %>% arrange(Row, desc(Col))       %>% select(Row, Col) %>% rename(R_NE = Row, C_NE = Col) %>% slice(1) %>% ungroup
t3 <- t %>% arrange(desc(Row), Col)       %>% select(Row, Col) %>% rename(R_SW = Row, C_SW = Col) %>% slice(1) %>% ungroup
t4 <- t %>% arrange(desc(Row), desc(Col)) %>% select(Row, Col) %>% rename(R_SE = Row, C_SE = Col) %>% slice(1) %>% ungroup

temp <- inner_join(t1, t2) %>% inner_join(t3) %>% inner_join(t4)
rm(t1,t2,t3,t4)

# Compare to get One_Zone
#   C_NW & C_SW
#   C_NE & C_SE
temp <- temp %>% mutate(Left = (C_NW == C_SW),
                        Right = (C_NE == C_SE),
                        OneZone = Left && Right)

##########
### Step 5: get Hour-heatmap for OneZone
##########
# get ID with only one zone
t <- temp %>%
  filter(OneZone == TRUE) %>%
  select(ID)
# get all grids for these ID
t <- inner_join(t,trxGridActive) %>% select(ID,Zone) %>% ungroup %>% distinct
# Get all trx passing these grids for these ID
t <- inner_join(t,trxZone)
# transform back from Zone to Entr_Sor
t <- t %>% select(-Zone) %>% ungroup %>% distinct

# get time window
trxZoneActiveH <- t %>%
  mutate(H = round(TimeSor, digits = 0),
         H_2 = H - H %% 2
  ) 

# get time window frequency >= limit.WindowFreq
result.ZW <- trxZoneActiveH %>% 
  group_by(ID,DOW,H) %>% 
  summarise(noPsg = n()) 
# %>%
#   filter(freq >= limit.WindowFreq)
rm(temp,ODtoGrid,GridLimit)
}

##########
### Step 6: Output in Output/
##########
inputName <-  read.table(text = filename.Input, sep=".")$V1 %>% as.character
time <- Sys.time() %>% format(format = "%Y%m%d_%H%M")

write.table(result.TS, 
            paste0("Output/Algo_",inputName,"_V",time,".csv"),
            sep=";",row.name=FALSE,quote=FALSE)
write.table(result.ZW, paste0("Output/Algo_",inputName,"_V",time,"_Window.csv"),sep=";",row.name=FALSE,quote=FALSE)
write.table(trxGridActive, paste0("Output/Algo_",inputName,"_V",time,"_Zone.csv"),sep=";",row.name=FALSE,quote=FALSE)

segmentation[is.na(segmentation)] <- 0
write.table(segmentation, paste0("Output/Algo_",inputName,"_V",time,"_Segmentation.csv"),sep=";",row.name=FALSE,quote=FALSE)

rm(inputName,time)
