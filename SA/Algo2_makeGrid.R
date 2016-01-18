####################
# Alog2_makeGrid
####################
# Create the grid system for Algo2_ZoneWindow
#
# Input: 
#   Args (from Parameter/Param_makeGrid.csv :
#     gridStep: unit in degree, width & height of each grid rectangular
#     zoneStep: unit in degree, extension for Entr or Sor with no Lng&Lat, ex. Systeme Ouvert
#   Reference:
#     gares(Reference/Ref_gares.csv): code, latitude et longitude des gares 
#     sens: Reference/Ref_sens.csv
# Output:
#   Reference/Ref_GridLimit
#   Reference/Ref_ODtoGrid
####################

##########
### Step 0: create Grid
##########
# import libraries
library(dplyr)

# get Parameter from Parameter/Param_makeGrid
args <- read.table("Parameters/Param_Algo2_makeGrid.csv",sep = ";", header=TRUE) 
gridStep <- args[1,1] 
zoneStep <- args[1,2]  
rm(args)

# get Reference data from Reference/
## TODO: update Ref_gares.csv, many missing Lng & Lat, wrong Code
gares <- read.table("Reference/Ref_gares.csv", sep = ";", header = TRUE, quote = "") %>%
  tbl_df %>%
  transmute(Ste = Societe,
            Cde,Lng, Lat)

sens = read.table("Reference/Ref_sens.csv",sep = ";", header=TRUE) %>% tbl_df

##########
##########
# divide lattitude by 0.5 degree
# u = upper
# d = down
t.lat <- data.frame(
  d = seq(42, 49 - gridStep, gridStep),
  u = seq(42 + gridStep, 49, gridStep),
  Row = seq(1, (49-42)/gridStep)
)
# divide longitude by 0.5 degree
# l = left
# r = right
t.lng <- data.frame(
  l = seq(-2, 8 - gridStep, gridStep),
  r = seq(-2 + gridStep, 8  , gridStep),
  Col = seq(1,(8+2)/gridStep)
)
# create grid with Row & Col number
t.lat$t <- 1
t.lng$t <- 1

# Create GridLimit, define the 4 bounds of each rectangle in Grid System
GridLimit <- inner_join(t.lat, t.lng) %>% 
  tbl_df %>%
  mutate(Grid = paste0(Row,"-",Col))
rm(t.lat, t.lng)

##########
### Step 2: find upper, down, left, right limit for each Entr-Sor possible
##########
##########
### 2.1 find relationship: Entr, Sor -> U,D,L,R (four limit for each Entr-Sor) 
##########
temp <- sens %>% select(Entr, Sor) %>% distinct %>% tbl_df
t <- gares %>% transmute(Entr = Cde, Elng = Lng, Elat = Lat)
temp <- left_join(temp, t)
t <- gares %>% transmute(Sor = Cde, Slng = Lng, Slat = Lat)
temp <- left_join(temp, t)
rm(t)

delta.Lat = zoneStep
delta.Lng = zoneStep

t1 <- temp %>% 
  filter(is.na(Elat) & !is.na(Slat)) %>%
  mutate(u=Slat + delta.Lat ,
         d=Slat - delta.Lat,
         r=Slng + delta.Lng, 
         l=Slng - delta.Lng)

t2 <- temp %>% 
  filter(!is.na(Elat) & is.na(Slat)) %>%
  mutate(u=Elat + delta.Lat,
         d=Elat - delta.Lat,
         r=Elng + delta.Lng,
         l=Elng - delta.Lng)

t3 <- temp %>%
  filter(!is.na(Elat) & !is.na(Slat)) %>%
  mutate(
    u = ( Elat + Slat + abs(Elat - Slat) )/2,
    d = ( Elat + Slat - abs(Elat - Slat) )/2,
    r = ( Elng + Slng + abs(Elng - Slng) )/2,
    l = ( Elng + Slng - abs(Elng - Slng) )/2
  )

temp1 <- rbind(t1,t2,t3) %>% rename(U=u, D=d, L=l, R=r)
temp1 <- temp1 %>%
  mutate(t = 1) %>%
  select(- c(Elng,Elat,Slng,Slat))
rm(t1,t2,t3,temp, gares, sens)

##########
### 2.2 create ref table: Entr-Sor to Grid
##########
# Create reference table from OD to Grid
ODtoGrid <- inner_join(GridLimit,temp1, by = "t") %>%
  filter(u > D,
         d < U,
         r > L,
         l < R) %>%
  select(Entr,Sor,Grid)

rm(temp1, delta.Lng, delta.Lat)

##########
### 3 Output result
##########
GridLimit <- GridLimit %>% 
  select(Grid,
         Row,Col,
         u,d,l,r)

write.table(GridLimit, "Reference/Ref_GridLimit.csv",sep=";",row.name=FALSE,quote=FALSE)
write.table(ODtoGrid, "Reference/Ref_ODtoGrid.csv",sep=";",row.name=FALSE,quote=FALSE)

# rm(GridLimit,ODtoGrid)
rm(zoneStep,gridStep)