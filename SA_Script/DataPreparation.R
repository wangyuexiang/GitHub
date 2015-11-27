##########
### load package
##########
library(dplyr)

##########
### input
##########
trx <- input %>%
  mutate(
    ID = as.character(Badge * 100000 + Porteur),
    Date = as.Date(as.character(Date)),
    Sens = ifelse(Entr == 0,
                  ifelse(Voie <=20, 1,2),
                  0))

trx <- trx %>% filter(Date >= start)

##########
### ID segmentation
##########
ID <- trx %>% 
  group_by(ID) %>% 
  summarise(Dmin = min(Date), 
            Dmax = max(Date), 
            Ddiff = Dmax - Dmin + 1, 
            Day = n_distinct(Date), 
            noPsg = n())
### add small =
#   F:  history interesting
#   T:  nOD < 10            
#       Day < 5 
#       Ddiff < 5
ID$Small <- FALSE
ID$Small[ID$noPsg < 10 | ID$Day < 5 | ID$Ddiff < 5] <- TRUE
### add Inactive =
#   F: always active
#   T: no trx after 30 days before end date
ID$Inactive <- FALSE
ID$Inactive[ID$Dmax < end - 30] <- TRUE

### remove small and inactive
ID <- ID %>%
  filter(Small == FALSE, Inactive == FALSE) %>%
  select(ID)

trx <- inner_join(trx, ID)
rm(ID)

##########
### ID.OD segmentation
##########
t <- trx %>%
  group_by(ID, Entr, Sor, Sens) %>% 
  summarise(Dmin = min(Date), 
            Dmax = max(Date), 
            Ddiff = Dmax - Dmin + 1, 
            Day = n_distinct(Date), 
            noPsg = n()
  ) 

### add small =
#   F:  history interesting
#   T:  nOD < 10            
#       Day < 5 
#       Ddiff < 5
t$Small <- FALSE
t$Small[t$noPsg < 5 | t$Day < 5 | t$Ddiff < 5] <- TRUE
### add Inactive =
#   F: always active
#   T: no trx after 30 days before end date
t$Inactive <- FALSE
t$Inactive[t$Dmax < end - 30] <- TRUE

t <- t %>%
  filter(Small == FALSE & Inactive == FALSE) %>%
  select(ID,Entr,Sor,Sens)

trx <- inner_join(trx,t)
rm(t)

##########
### add sens & create OD
##########
sens = read.table("Ref_sens.csv",sep = ";", header=TRUE)
trx <- trx %>% mutate(Voie = ifelse(Entr == 0, Voie, 0))
trx <- trx %>% left_join(sens)
trx <- trx %>% mutate(SensEntr = ifelse(is.na(SensEntr), 0, SensEntr),
                      SensSor = ifelse(is.na(SensSor), 0, SensSor))
rm(sens)
trx <- trx %>% mutate(OD = paste0(Entr,"-",Sor,"-",SensEntr,"-",SensSor))

output <- trx
rm(trx)
