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
#     limit.ZonePer
#     limit.ActiveDay
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
### Step 0: Prepare
##########
print("Log: Step 0")
# load package
library(dplyr)
library(cluster)

# define Repository to get Parameters
ParamRepo <- NA
Args <- commandArgs(trailingOnly = TRUE)
ParamRepo <- Args[1]

if(is.na(ParamRepo)){
  # get arguments 
  args <- read.table("Parameters/Param_Algo.csv",sep = ";", header=TRUE) 
} else {
  # get arguments 
  args <- read.table(paste0(ParamRepo,"/Param_Algo.csv"),sep = ";", header=TRUE) 
}

# attribute args
filename.Input <- as.character(args[1,1])
limit.ZonePer <- args[1,2]
limit.ActiveDay <- args[1,3]
# limit.WindowFreq <- 5
day.start <- as.Date(as.character(args[1,4]))
day.end <- as.Date(as.character(args[1,5]))

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
print("Log: Step 1")
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
### Step 2: Prepare for Algo2 - Get Trx not in Result.TS
##########
print("Log: Step 2")
t <- input %>%
  mutate(
    ID = as.character(Badge * 100000 + Porteur),
    Date = as.Date(as.character(Date)),
    Sens = ifelse(Entr == 0,
                  ifelse(Voie <=20, 1,2),
                  0)) %>%
  filter(Date >= day.start & Date <= day.end)

# find trx in result.TS
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

# get Reference data from Reference/
# run Algo2_makeGrid to have "Ref_ODtoGrid.csv" and "Ref_GridLimit.csv" 
# get ODtoGrid
ODtoGrid <- read.table("Reference/Ref_ODtoGrid.csv", header = T, sep = ";") %>% 
  tbl_df %>%
  mutate(Zone = as.character(Zone))
# get GridLimit
GridLimit <- read.table("Reference/Ref_GridLimit.csv", header = T, sep = ";") %>% 
  tbl_df %>%
  mutate(Zone = as.character(Zone))

##########
### Step 3: find Zone frequently visited
##########
print("Log: Step 3")
# transform Entr-Sor to Zone
trxZone <- trx %>% inner_join(ODtoGrid)

# find number of active day for each ID
t1 <- trxZone %>%
  group_by(ID) %>%
  summarise(ActiveDay = n_distinct(Date))
# find number of active day for each ID,Zone
t2 <- trxZone %>%
  group_by(ID, Zone) %>%
  summarise(Day = n_distinct(Date))

# get ID,Zone with (Zone frequently visited)
#   - ActiveDay >= limit.ActiveDay 
#   - Per >= limit.ZonePer
trxZoneActive <- inner_join(t1,t2) %>% 
  mutate(Per = Day / ActiveDay) %>%
  filter(ActiveDay >= limit.ActiveDay,
         Per >= limit.ZonePer)
rm(t1,t2)

##########
### Step 4: find ID with only one big zone  
##########
print("Log: Step 4")
# get Grid detailed info for trxZoneActive
t <- inner_join(trxZoneActive, GridLimit)
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
### Step 5: get Hourheatmap for OneZone
##########
print("Log: Step 5")
# get ID with only one zone
t <- temp %>%
  filter(OneZone == TRUE) %>%
  select(ID)
# get all grids for these ID
t <- inner_join(t,trxZoneActive) %>% select(ID,Zone) %>% ungroup %>% distinct
# Get all trx passing these grids for these ID
t <- inner_join(t,trxZone)
# transform back from Zone to Entr_Sor
t <- t %>% select(-Zone) %>% ungroup %>% distinct

# get time window
trxZoneActiveH <- t %>%
  mutate(H = round(TimeSor, digits = 0),
         H_2 = H - H %% 2
  ) 

print("Step 5.1")

# get time window frequency >= limit.WindowFreq
result.ZW <- trxZoneActiveH %>% 
  group_by(ID,DOW,H) %>% 
  summarise(freq = n()) 
# %>%
#   filter(freq >= limit.WindowFreq)
rm(temp,ODtoGrid,GridLimit)

print("Step 6")

##########
### Step 6: Output in Output/
##########
print("Log: Step 6")
inputName <-  read.table(text = filename.Input, sep=".")$V1 %>% as.character
time <- Sys.time() %>% format(format = "%Y%m%d_%H%M")

write.table(result.TS, 
            paste0("Output/Algo_",inputName,"_V",time,".csv"),
            sep=";",row.name=FALSE,quote=FALSE)
write.table(result.ZW, paste0("Output/Algo_",inputName,"_V",time,"_Window.csv"),sep=";",row.name=FALSE,quote=FALSE)
write.table(trxZoneActive, paste0("Output/Algo_",inputName,"_V",time,"_Zone.csv"),sep=";",row.name=FALSE,quote=FALSE)

rm(inputName,time)
