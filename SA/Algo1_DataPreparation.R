####################
# Alog1_DataPreparation
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
#   result of model Zone Window: Reference/filename.Output
####################

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

if(nrow(ID) > 0) {
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
  ID$Inactive[ID$Dmax < day.end - 30] <- TRUE
  
  ### remove small and inactive
  ID <- ID %>%
    filter(Small == FALSE, Inactive == FALSE) %>%
    select(ID)
  
  trx <- inner_join(trx, ID, by = "ID")
  rm(ID)
}

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

if(nrow(t) > 0){
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
  t$Inactive[t$Dmax < day.end - 30] <- TRUE
  
  t <- t %>%
    filter(Small == FALSE & Inactive == FALSE) %>%
    select(ID,Entr,Sor,Sens)
  
  trx <- inner_join(trx,t)
  rm(t)
}
