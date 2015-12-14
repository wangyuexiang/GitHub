####################
# Alog2
####################
# Run Algo2_ZoneWindow
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
#   result of model Zone Window: 
#       Reference/Algo2_ + Input + V + time + Zone
#       Reference/Algo2_ + Input + V + time + Window
####################

##########
### Step 0: Prepare
##########
# load package
library(dplyr)

# define Repository to get Parameters
ParamRepo <- NA
Args <- commandArgs(trailingOnly = TRUE)
ParamRepo <- Args[1]

if(is.na(ParamRepo)){
  # get arguments 
  args <- read.table("Parameters/Param_Algo2.csv",sep = ";", header=TRUE) 
} else {
  # get arguments 
  args <- read.table(paste0(ParamRepo,"/Param_Algo2.csv"),sep = ";", header=TRUE) 
}

filename.Input <- as.character(args[1,1])
limit.ZonePer <- args[1,2]
limit.ActiveDay <- args[1,3]
# limit.WindowFreq <- 5
day.start <- as.Date(as.character(args[1,4]))
day.end <- as.Date(as.character(args[1,5]))

rm(args)
rm(Args)

# input 
input <- read.table(paste0("Input/",filename.Input), header = T, sep = ";") %>% tbl_df 

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
### Step 1: add ID & get only the trx in the period
##########
trx <- input %>%
  mutate(
    ID = as.character(Badge * 100000 + Porteur),
    Date = as.Date(as.character(Date)),
    Sens = ifelse(Entr == 0,
                  ifelse(Voie <=20, 1,2),
                  0)) %>%
  filter(Date >= day.start & Date <= day.end)

##########
### Step 2: find Zone frequently visited
##########
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
### Step 3: find ID with only one big zone  
##########
# get Grid detailed info for trxZoneActive
t <- inner_join(trxZoneActive, GridLimit)
# group them by ID
t <- t %>% group_by(ID)

# Get 4 points
#   NW  NE (NorthWest, NorthEast)
#   SW  SE (SouthWest, SouthEast)
t1 <- t %>% arrange(Row, Col)             %>% select(Row, Col) %>% slice(1) %>% rename(R_NW = Row, C_NW = Col)
t2 <- t %>% arrange(Row, desc(Col))       %>% select(Row, Col) %>% slice(1) %>% rename(R_NE = Row, C_NE = Col)
t3 <- t %>% arrange(desc(Row), Col)       %>% select(Row, Col) %>% slice(1) %>% rename(R_SW = Row, C_SW = Col)
t4 <- t %>% arrange(desc(Row), desc(Col)) %>% select(Row, Col) %>% slice(1) %>% rename(R_SE = Row, C_SE = Col)

temp <- inner_join(t1, t2) %>% inner_join(t3) %>% inner_join(t4)
rm(t1,t2,t3,t4)

# Compare to get One_Zone
#   C_NW & C_SW
#   C_NE & C_SE
temp <- temp %>% mutate(Left = (C_NW == C_SW),
                        Right = (C_NE == C_SE),
                        OneZone = Left && Right)

##########
### Step 4: get Hourheatmap for OneZone
##########
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

# get time window frequency >= limit.WindowFreq
result.ZW <- trxZoneActiveH %>% 
  group_by(ID,DOW,H) %>% 
  summarise(freq = n()) 
# %>%
#   filter(freq >= limit.WindowFreq)
rm(temp,ODtoGrid,GridLimit)

##########
### Step 5: Output in Output/
##########
inputName <-  read.table(text = filename.Input, sep=".")$V1 %>% as.character
time <- Sys.time() %>% format(format = "%Y%m%d_%H%M")

write.table(result.ZW, paste0("Output/Algo2_",inputName,"_V",time,"_Window.csv"),sep=";",row.name=FALSE,quote=FALSE)
write.table(trxZoneActive, paste0("Output/Algo2_",inputName,"_V",time,"_Zone.csv"),sep=";",row.name=FALSE,quote=FALSE)
rm(inputName,time)