### load package
library(dplyr)

filename.Input <- "VIP.csv"
filename.Output <- "Output.csv"

limit.ZonePer <- .5
limit.ActiveyDay <- 5
# limit.WindowFreq <- 5

day.start <- as.Date("2015-5-1")
day.end <- as.Date("2015-8-31")


### input 
input <- read.table(filename.Input, header = T, sep = ";") %>% tbl_df 

### load function
# source('DataPreparation.R', encoding = 'UTF-8')
# source('Functions.R', encoding = 'UTF-8')

####################
####################

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
  filter(Date >= day.start,
         Date <= day.end)

##########
### Step 2: find Zone frequently visited
##########
# get ODtoGrid
ODtoGrid <- read.table("Ref_ODtoGrid.csv", header = T, sep = ";") %>% tbl_df
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
  filter(ActiveDay >= limit.ActiveyDay,
         Per >= limit.ZonePer)
rm(t1,t2)

##########
### Step 3: find ID with only one big zone  
##########
# get GridLimit
GridLimit <- read.table("Ref_GridLimit.csv", header = T, sep = ";") %>% tbl_df 
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
result <- trxZoneActiveH %>% 
  group_by(ID,DOW,H) %>% 
  summarise(freq = n()) 
# %>%
#   filter(freq >= limit.WindowFreq)
rm(temp,t1,t2,ODtoGrid,GridLimit)

##########
### output
##########
write.table(result, filename.Output,sep=";",row.name=FALSE,quote=FALSE)
