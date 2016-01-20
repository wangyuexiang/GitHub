##########
##########
# SA_function.R
# define all functions
##########
### List of all functions
# GetLngLat:	get the longitude and lattitude of the transaction
# GetResult:	apply model result to test set
# GetInd:			get indicators for each model
### Old functions - in S_function_old.R

##########
# ??? parameters to be justified or modified
# GetInd:
param.ind3 <- 5 # weight of Ind3 in calculation of Ind
# Model:
param.min.noPsg = 5            # low limit for choosing the result
param.min.nb.for.cluster = 10  # lower than which we won't clustering
param.max.nb.cluster = 3			 # the max number of cluster we'll test when choosing the number of cluster
param.model.2 = .3						 # low limit for choosing the ID-OD for Model.2 Space - Time
#getModelunits : 
param.SDWprnoW <- 0.060

GetNumberDays <- function (period){
  result <- period %>%
    summarise (
      D0 = sum(DOW == 0),
      D1 = sum(DOW == 1),
      D2 = sum(DOW == 2),
      D3 = sum(DOW == 3),
      D4 = sum(DOW == 4),
      D5 = sum(DOW == 5),
      D6 = sum(DOW == 6),
      Weekdays = sum(DOW == 1 | DOW == 2 | DOW == 3 | DOW == 4 | DOW == 5),
      Weekends = sum(DOW == 0 | DOW == 6)
      )
  return (result)
}

GetLngLat <- function(transaction) {
  # Add longitude and lattitude to the transaction
  # Args:
  #	  tansaction:	Entr, Sor, ...
  # Returns:
  #	  tansaction:	Entr, Sor, ..., ELng, ELat, SLng, SLat

  ### !!! to be removed
  #   transaction$Entr <- as.numeric(transaction$Entr)
  #   transaction$Sor <- as.numeric(transaction$Sor)
  cde <- gares %>% transmute(Entr = Cde, ELng = Lng, ELat = Lat)
  transaction <- left_join(transaction, cde, by = "Entr")
  names(cde) <- c("Sor", "SLng", "SLat")
  transaction <- left_join(transaction, cde, by = "Sor")
  return(transaction)	
}

GetResult <- function(test, result) {
  # Identify the passage in the test set that the model could predict
  # Args:
  #	  test: ID Entr Sor Date DOW WOY TimeEntr TimeSor Sens SensEntr SensSor
  #	  result: ID Entr Sor SensEntr SensSor Sens DOW Tmin Tmax noPsg

  # !!! removed when modify data_preparation
  # test$DOW <- as.numeric(test$DOW)
  test$result <- 0
  name <- names(test)
	# temp <- inner_join(test, result, by = c("ID", "Entr", "Sor", "SensEntr", "SensSor","Sens", "DOW"))
	temp <- inner_join(test, result, by = c("ID", "OD", "DOW"))
  temp[temp$TimeSor >= temp$Tmin & temp$TimeSor <= temp$Tmax,]$result <- 1
  return(temp[, name])
}

GetInd <- function(test, result) {
  # Get 3 indicators and combine them to evaluate each model
  # Args:
  #	  test: ID Entr Sor Date DOW WOY TimeEntr TimeSor Sens SensEntr SensSor
  #	  result: result of Model in format: ID, Entr, Sor, SensEntr, SensSor, Sens, (DOW,) Tmin, Tmax
  
  Ind <-  test %>%
    group_by(ID) %>%
    summarise(Tpos = sum(result[result == 1]), Fneg = n() - Tpos, Ind1 = Tpos/(Tpos+Fneg), Ind2 = Fneg/(Tpos+Fneg) )
  # Ind1: Sensitivity, Recall
  # Ind2: Miss rate
  
  result <- inner_join(test.period, result, by="DOW")
  result$Mark <- 0
  
  # keep names
  #	name <- names(result)   #useless ?
  # temp <- inner_join(test, result, by = c("ID", "Entr", "Sor", "SensEntr", "SensSor", "Sens", "DOW"))
  temp <- inner_join(test, result, by = c("ID", "OD", "DOW"))

  temp[temp$TimeSor >= temp$Tmin & temp$TimeSor <= temp$Tmax,]$Mark <- 1
  
  #Ind3 <- result %>% group_by(ID) %>% summarise(nMark = sum(Mark[Mark==1]), t = (n() - nMark), Ind3 = t/ n() )
  Ind3 <- temp %>% group_by(ID) %>% summarise(nMark = sum(Mark[Mark==1]), t = (n() - nMark), Ind3 = t/ n() )
  # Ind3: fake alert
  
  Ind <- inner_join(Ind,Ind3, by = "ID")
  Ind <- Ind[, c("ID", "Ind1", "Ind2", "Ind3")]
  # !!! to be justified or modified
  Ind$Ind <- Ind$Ind1 - Ind$Ind3 / param.ind3
  return(Ind)
}

Sens <- function (Transactions){
  # Decide the direction of each transaction (A789)
  # Args:
  #	  Transactions:	ID, Entr, Sor, Date, DOW, WOY, TimeEntr, TimeSor, Voie, SensEntr, SensSor
  # Returns:
  #	  Transactions:	ID, Entr, Sor, Date, DOW, WOY, TimeEntr, TimeSor, Voie, Sens, SensEntr, SensSor
  Transactions <- Transactions %>% select(ID,Entr,Sor,TimeEntr,TimeSor,DOW,WOY,Date,Voie,SensEntr, SensSor)
  Transactions$Sens <- 0
  for (i in 1:nrow(Transactions)){
    if (Transactions$Entr[i] == 0 & ( Transactions$Sor[i] %in% garesorder$Cde) ){ #Si Entr = 0 et Sor dans A789
      if (Transactions$Sor[i] == 25006001 | Transactions$Sor[i] == 25006014 | 
          Transactions$Sor[i] == 25006019 | Transactions$Sor[i] == 25006026) { # Ceux dont les voies >= 20 sont en sens 1
        #Canet de mereuil, Antibes ouest, Saint Isidore Ech Ouest, La Turibe Ech Est
        if (Transactions$Voie[i] >= 20 ){ Transactions$Sens[i] <- 1 
        } else 
        { Transactions$Sens[i] <- 2 }
      } else 
        if (Transactions$Sor[i] == 25006010 | Transactions$Sor[i] == 25006011 | Transactions$Sor[i] == 25006013 |
            Transactions$Sor[i] == 25006016 | Transactions$Sor[i] == 25006021 | Transactions$Sor[i] == 25006020 | 
            Transactions$Sor[i] == 25006027 | Transactions$Sor[i] == 25006012){ #Ceux dont les voies >= 20 sont en sens 2
          #Fréjus, Les Adrets, Antibes Est, Cagnes Est, Saint Isidore ech Est, Saint Isidore PV, La Turbie PV, Antibes PV
          if (Transactions$Voie[i] >= 20 ){ Transactions$Sens[i] <- 2 
          } else 
          { Transactions$Sens[i] <- 1 }
        } else 
          if (Transactions$Sor[i] == 25006024){ #Sophia
            Transactions$Sens[i] <- 1  #Entrée seule
          } else 
            if (Transactions$Sor[i] == 25006017){ Transactions$Sens[i] <- 1
            } else
              if (Transactions$Sor[i] == 25006015){ Transactions$Sens[i] <- 2
              }
    } else 
      if (  (Transactions$Entr[i] %in% garesorder$Cde) & 
            (Transactions$Sor[i] %in% garesorder$Cde) ) { #Entrée et sortie dans A789
        if (match(Transactions$Entr[i],garesorder$Cde) < match(Transactions$Sor[i],garesorder$Cde)){
          Transactions$Sens[i] <- 1
        } else 
        { Transactions$Sens[i] <- 2
        }
      }
  }
  return( Transactions)
}

SO <- function (Transactions){
  Transactions <- Transactions %>% select(ID,Entr,Sor,TimeEntr,TimeSor,DOW,WOY,Date,Voie,Sens, SensEntr, SensSor)
  Transactions$Date <- as.character(Transactions$Date)
  
  #Create the useful indicators for fusion :
  # sf (bool) if the OD is in SF
  # Time = The time to consider while calculating the difference with the previous OD  : TimeEntr if it is not 0, else TimeSor
  # diff = the time difference with the previous OD
  # indic (bool) : if diff <= 1, and NA are replaced with False
  intermediaire <- Transactions %>%
    group_by(ID,Date,Sens) %>%
    arrange(TimeSor) %>%
    mutate(sf = (TimeEntr != 0), Time = (TimeEntr + TimeSor)-(sf*TimeSor) , diff = Time - lag(TimeSor), indic = (diff <= 1 & !is.na(diff)) )
  
  #Create afusioner : the ODs to be combined.
  # diff <=1 or the next OD's diff <= 1
  afusionner <- intermediaire %>%
    filter (indic | lead(indic,default = FALSE) ) %>%
    SO.aux
  
  #Select all the ODs non concerned by a fusion
  anepasfusionner <- intermediaire %>%
    filter( !(indic | lead(indic,default = FALSE)) )
  
  return( rbind(afusionner,anepasfusionner) %>%
            ungroup() %>%
            select(ID,Entr,Sor,TimeEntr,TimeSor,DOW,WOY,Date,Voie,Sens, SensEntr, SensSor) %>%
						tbl_df()
				) 
}

SO.aux <- function(intermediaire){
  # Aux function for SO
  # Takes as argument the ODs to be fusioned
  # much less test to do than the previous version
  i <- 2
  while (i  <= nrow(intermediaire)){
    if (intermediaire$indic[i]){
      if (intermediaire$sf[i-1]){
        newrow <- data.frame( ID = intermediaire$ID[i], Entr = intermediaire$Entr[i-1], Sor = intermediaire$Sor[i], TimeEntr = intermediaire$TimeEntr[i-1],
                              TimeSor = intermediaire$TimeSor[i], DOW = intermediaire$DOW[i], WOY = intermediaire$WOY[i], Date = intermediaire$Date[i],
                              Voie = -1, Sens = intermediaire$Sens[i], sf = intermediaire$sf[i], Time = intermediaire$Time[i], diff =intermediaire$diff[i],
                              indic = intermediaire$indic[i],
															SensEntr = intermediaire$SensEntr[i-1],
															SensSor = intermediaire$SensSor[i])
      } else {
        newrow <- data.frame( ID = intermediaire$ID[i], Entr = intermediaire$Sor[i-1], Sor = intermediaire$Sor[i], TimeEntr = intermediaire$TimeSor[i-1],
                              TimeSor = intermediaire$TimeSor[i], DOW = intermediaire$DOW[i], WOY = intermediaire$WOY[i], Date = intermediaire$Date[i],
                              Voie = -1, Sens = intermediaire$Sens[i], sf = intermediaire$sf[i], Time = intermediaire$Time[i], diff =intermediaire$diff[i],
                              indic = intermediaire$indic[i],
															SensEntr = intermediaire$SensSor[i-1],
															SensSor = intermediaire$SensSor[i])
      }
      if (i ==2){
        intermediaire <- rbind(
          newrow,
          intermediaire[(i+1):nrow(intermediaire),])
      } else if( i == nrow(intermediaire)){
        intermediaire <- rbind(intermediaire[1:(i-2),],
                               newrow
        )
      }else{
        intermediaire <- rbind(intermediaire[1:(i-2),],
                               newrow,
                               intermediaire[(i+1):nrow(intermediaire),])
        
      }
      intermediaire$Date <- as.character(intermediaire$Date)
      intermediaire$ID <- as.character(intermediaire$ID)
      i <- i-1
    }
    i <- i+1
  }
  return(intermediaire)
}

inverse.after.SO <- function( Transactions ) {
#Inversing Entr & Sor for the SO, if Sens = 2
#TO BE APPLYED AFTER SO
  nottouch <- Transactions %>% filter (Entr != 0 | Sens != 2)
  touch <- Transactions %>% filter (Entr == 0 & Sens == 2)
  if(nrow(touch) > 0){
#     touch $ Entr <- touch $ Sor
#     touch $ Sor <- 0
    touch <- touch %>% mutate(
      Entr = Sor,
      Sor = 0,
      SensEntr = SensSor,
      SensSor = 0
    )
  }
  return( rbind(nottouch,touch))
}

getModel.units <- function ( Transactions ){
  result <- Transactions %>%
    group_by(ID) %>%
    summarise(
      D1 = sum(DOW == 1),
      D2 = sum(DOW == 2),
      D3 = sum(DOW == 3),
      D4 = sum(DOW == 4),
      D5 = sum(DOW == 5),
      D6 = sum(DOW == 6),
      D0 = sum(DOW == 0),
      
      noPsg = n(),
      noWE = D6+D0,
      noW = noPsg - noWE,
   
      SD = sd(c(D1,D2,D3,D4,D5,D6,D0)),
      SDW = sd(c(D1,D2,D3,D4,D5)),
      SDprnoPsg = SD / noPsg,
      SDWprnoW = SDW / noW,
      sumSD = SDWprnoW + SDprnoPsg,
      
      avg = noPsg / 7,
      avgWE = noWE / 2,
      avgW = noW / 5,
      model = chose.model(SDWprnoW)
    ) %>%
    select (ID, model)
  
  return(result)
}

chose.model <-function ( SDWprnoW ) {
  # Input : ID, SDWprnoW
  if( is.na(SDWprnoW)) return(NA)
  
  if ( SDWprnoW > param.SDWprnoW ) model <- 2 
  else  model <- 1

  return(model)
}

Model.for.a.decade <- function ( Transactions, decades, units){
#### Apply the Model of the given decade, and of the model units of each person
  result <- NULL
  withmodel <- inner_join(Transactions, units )
  
  for (k in 0:2){
    applymodel <- withmodel %>%
      filter (model == k )
    if ( nrow(applymodel) >= 1 ){
      applymodel <- Model(transaction = applymodel, model.decades = decades , model.units = k)
      result <- rbind(result,applymodel)
    }
  }
  return ( result )
}

Model <- function(transaction, model.decades, model.units) {
  # Run the model over the transaction set
  # Args:
  #	  transaction:	ID, Entr, Sor, Date, DOW, WOY, TimeEntr, TimeSor, Voie
  #   model: 0,1,2,10,11,12,20,21,22
  # Returns:
  #	  transaction:	ID, Entr, Sor, DOW, Tmin, Tmax, Model
  #   transaction:  ID, OD = paste0(Entr,Sor,Sens), ...
  
  model <- model.decades * 10 + model.units
  
  if (model == 0){
    T.matin <- transaction %>%
      filter(TimeSor < 12 ) %>%
      group_by(ID, OD) %>%
      summarise(noPsg = n(), T = mean(TimeSor), SD = sd(TimeSor), Tmin = T -SD, Tmax = T + SD)
    
    T.aprem <- transaction %>%
      filter(TimeSor >= 12 ) %>%
      group_by(ID, OD) %>%
      summarise(noPsg = n(), T = mean(TimeSor), SD = sd(TimeSor), Tmin = T -SD, Tmax = T + SD)
    
    T <- rbind(T.matin, T.aprem)
    T <- T %>% filter(noPsg > param.min.noPsg)
    
    # add DOW
    T$DOW <- 0
    temp <- T
    for(i in 1:6) {
      # add DOW to the T
      temp$DOW <- i
      T <- rbind(T, temp)
    }
    if(nrow(T)>0)  T$Model <- model
    result <- T
    ##########
    # end of model 00
  } else 
	if (model == 1) {
    temp <- transaction 
    temp$weekday <- 0
    temp[temp$DOW %in% c(1:5), ]$weekday <- 1
    
    T.matin <- temp %>%
      filter(TimeSor < 12 ) %>%
      group_by(ID, OD, weekday) %>%
      summarise(noPsg = n(), T = mean(TimeSor), SD = sd(TimeSor), Tmin = T -SD, Tmax = T + SD)
    
    T.aprem <- temp %>%
      filter(TimeSor >= 12 ) %>%
      group_by(ID, OD, weekday) %>%
      summarise(noPsg = n(), T = mean(TimeSor), SD = sd(TimeSor), Tmin = T -SD, Tmax = T + SD)
    
    T <- rbind(T.matin, T.aprem)
    T <- T %>% filter(noPsg > param.min.noPsg)
    
    # add DOW
    # for weekdays
    result <- T %>% filter(weekday == 1)
    result$DOW <- 1
    temp <- result
    for(i in 2:5) {
      # add DOW to the result
      temp$DOW <- i
      result <- rbind(result, temp)
    }
    
    # for weekends
    T <- T %>% filter(weekday == 0)
    if (nrow(T) > 0){
      T$DOW <- 0
      temp <- T
      temp$DOW <- 6
      T <- rbind(T, temp)
    }
    
    result <- rbind(result, T)
    result$weekday <- NULL
    if(nrow(result)>0)  result$Model <- model
    
    ##########
    # end of model 01
  } else 
	if (model == 2) {
    T.matin <- transaction %>%
      filter(TimeSor < 12 ) %>%
      group_by(ID, OD, DOW) %>%
      summarise(noPsg = n(), T = mean(TimeSor), SD = sd(TimeSor), Tmin = T -SD, Tmax = T + SD)
    
    T.aprem <- transaction %>%
      filter(TimeSor >= 12 ) %>%
      group_by(ID, OD, DOW) %>%
      summarise(noPsg = n(), T = mean(TimeSor), SD = sd(TimeSor), Tmin = T -SD, Tmax = T + SD)
    
    T <- rbind(T.matin, T.aprem)
    T <- T %>% filter(noPsg > param.min.noPsg)
    
    if(nrow(T)>0)  T$Model <- model
    result <- T
    ##########
    # end of model 02
  } else 
	if(model == 10) {
    # result <- data.frame(ID="", Entr=0, Sor=0, SensEntr = 0, SensSor = 0, Sens = 0, SD=0, T=0, Tmin=0, Tmax=0, noPsg=0)
	  result <- data.frame(ID="", OD="", SD=0, T=0, Tmin=0, Tmax=0, noPsg=0)

    result$ID <- as.character(result$ID)
    for (i in 1:nrow(ID.list)) {
      temp <- transaction %>% filter(ID == ID.list$ID[i])
			 max.cluster <- length(unique(temp$TimeSor))
      if(nrow(temp) >= param.min.nb.for.cluster & max.cluster > 1) {
        # if not many passages, we will not cluster
        # decide nb of cluster
        clus<- clusGap(temp %>% select(TimeSor), kmeans, max(min(param.max.nb.cluster, max.cluster),2))
        n.cluster <- with(clus,maxSE(Tab[,"gap"],Tab[,"SE.sim"]))
        
        set.seed(1234)
        temp.kmeans <-   kmeans(temp[, "TimeSor"], centers = n.cluster)
        temp$cluster <- temp.kmeans$cluster
        T <- temp %>%
          group_by(ID, OD, cluster) %>%
          summarise(noPsg = n(), T = mean(TimeSor), SD = sd(TimeSor), Tmin = T -SD, Tmax = T + SD)
        
        # remove line with SD = N.A.
        T <- T %>% filter(noPsg > 1)
        
        T$cluster <- NULL
        result <- rbind(result, T)
      } # end of if
    } # end of loop i
    result <- result[-1,]
    result <- result %>% filter(noPsg > param.min.noPsg)
    
    # add DOW
    result$DOW <- 0
    temp <- result
    for(i in 1:6) {
      # add DOW to the T
      temp$DOW <- i
      result <- rbind(result, temp)
    }
    
    if(nrow(result)>0)  result$Model <- model

    ##########
    # end of model 10
  } else 
	if(model == 11) {
    temp1 <- transaction 
    temp1$weekday <- 0
    temp1[temp1$DOW %in% c(1:5), ]$weekday <- 1
    
    # clustering TimeSor
    # result <- data.frame(ID="", Entr=0, Sor=0, SensEntr = 0, SensSor = 0, Sens = 0, weekday = 0, SD=0, T=0, Tmin=0, Tmax=0, noPsg=0)
	  result <- data.frame(ID="", OD="", weekday = 0, SD=0, T=0, Tmin=0, Tmax=0, noPsg=0)

		result$ID <- as.character(result$ID)
    for (i in 1:nrow(ID.list)) {
      for (j in 0:1) {
        temp <- temp1 %>% filter(ID == ID.list$ID[i] & weekday == j)
			 max.cluster <- length(unique(temp$TimeSor))
      if(nrow(temp) >= param.min.nb.for.cluster & max.cluster > 1) {
          # if not many passages, we will not cluster
          # decide nb of cluster
					clus<- clusGap(temp %>% select(TimeSor), kmeans, max(min(param.max.nb.cluster, max.cluster),2))
          n.cluster <- with(clus,maxSE(Tab[,"gap"],Tab[,"SE.sim"]))
          
          set.seed(1234)
          temp.kmeans <-   kmeans(temp[, "TimeSor"], centers = n.cluster)
          temp$cluster <- temp.kmeans$cluster
          T <- temp %>%
            group_by(ID, OD, weekday, cluster) %>%
            summarise(noPsg = n(), T = mean(TimeSor), SD = sd(TimeSor), Tmin = T -SD, Tmax = T + SD)
          
          # remove line with SD = N.A.
          T <- T %>% filter(noPsg > 1)
          
          T$cluster <- NULL
          result <- rbind(result, T)
        } # end of if
      } # end of loop j
    } # end of loop i
    
    result <- result[-1,]
    result <- result %>% filter(noPsg > param.min.noPsg)
    
    T <- result  
    # add DOW
    # for weekdays
    result <- T %>% filter(weekday == 1)
    result$DOW <- 1
    temp <- result
    for(i in 2:5) {
      # add DOW to the result
      temp$DOW <- i
      result <- rbind(result, temp)
    }
    
    # for weekends
    T <- T %>% filter(weekday == 0)
    if (nrow(T) > 0){
      T$DOW <- 0
      temp <- T
      temp$DOW <- 6
      T <- rbind(T, temp)
    }
    
    result <- rbind(result, T)
    result$weekday <- NULL
    
    if(nrow(result)>0)  result$Model <- model
    
    ##########
    # end of model 11
  } else 
	if(model == 12) {
    # result <- data.frame(ID="", Entr=0, Sor=0, SensEntr = 0, SensSor = 0, Sens = 0, DOW=0, SD=0, T=0, Tmin=0, Tmax=0, noPsg=0)
	  result <- data.frame(ID="", OD="", DOW = 0, SD=0, T=0, Tmin=0, Tmax=0, noPsg=0)

    result$ID <- as.character(result$ID)
    for (i in 1:nrow(ID.list)) {
      for (j in 0:6) {
        temp <- train %>% filter(ID == ID.list$ID[i] & DOW == j)
        max.cluster <- length(unique(temp$TimeSor))
				if(nrow(temp) >= param.min.nb.for.cluster & max.cluster > 1) {
          # if not many passages, we will not cluster
          # decide nb of cluster
					clus<- clusGap(temp %>% select(TimeSor), kmeans, max(min(param.max.nb.cluster, max.cluster),2))
					
          n.cluster <- with(clus,maxSE(Tab[,"gap"],Tab[,"SE.sim"]))
          
          set.seed(1234)
          temp.kmeans <-   kmeans(temp[, "TimeSor"], centers = n.cluster)
          temp$cluster <- temp.kmeans$cluster
          T <- temp %>%
            group_by(ID, OD, DOW, cluster) %>%
            summarise(noPsg = n(), T = mean(TimeSor), SD = sd(TimeSor), Tmin = T -SD, Tmax = T + SD)
          
          # remove line with SD = N.A.
          T <- T %>% filter(noPsg > 1)
          
          T$cluster <- NULL
          result <- rbind(result, T)
        } # end of if
      } # end of loop j
    } # end of loop i
    
    result <- result[-1,]
    result <- result %>% filter(noPsg > param.min.noPsg)
    
    if(nrow(result)>0)  result$Model <- model
    
    ##########
    # end of model 12
  } else 
	{
    ####################
    ### model.2:	OD -> Space -> Time
    ####################
    #COMPTAGE DES TRAJETS PAR OD
    Compteur <- transaction %>% 
      group_by(ID, Entr,Sor) %>%
      summarise( n = n())
    #DECISION AUTOMATIQUE DES TRONCONS FREQUENTS
    Troncon_Selection <- Compteur %>% 
      group_by(ID) %>%
      filter (n > (param.model.2 * max(n)))
    VIP2_espace <- inner_join(transaction, Troncon_Selection, by = c("ID","Entr","Sor"))
    VIP2_espace <- tbl_df(VIP2_espace)
    #Create OD.list for the kmeans
    OD.list <- VIP2_espace %>% group_by( ID,Entr,Sor, Sens) %>% summarise()
    
    if(model == 20) {
      # result <- data.frame(ID="", Entr=0, Sor=0, SensEntr = 0, SensSor = 0, Sens = 0, DOW=0, SD=0, T=0, Tmin=0, Tmax=0, noPsg=0)
			result <- data.frame(ID="", OD="", DOW=0, SD=0, T=0, Tmin=0, Tmax=0, noPsg=0)

      result$ID <- as.character(result$ID)
      for (k in 1:nrow(OD.list)){
        temp <- VIP2_espace %>% filter(ID == OD.list$ID[k] &  Entr == OD.list$Entr[k] & Sor == OD.list$Sor[k] )
        # base to be verified
				max.cluster <- length(unique(temp$TimeSor))
				if(nrow(temp) >= param.min.nb.for.cluster & max.cluster > 1) {				
          # if not many passages, we will not cluster
          # decide nb of cluster
          clus<- clusGap(temp %>% select(TimeSor), kmeans, max(min(param.max.nb.cluster, max.cluster),2))
          n.cluster <- with(clus,maxSE(Tab[,"gap"],Tab[,"SE.sim"]))
          
          set.seed(1234)
          temp.kmeans <-   kmeans(temp[, "TimeSor"], centers = n.cluster)
          temp$cluster <- temp.kmeans$cluster
          T <- temp %>%
            group_by(ID, OD, DOW, cluster) %>%
            summarise(SD = sd(TimeSor), T = mean(TimeSor),Tmin = T -SD, Tmax = T + SD, noPsg = n())
          T <- T %>% filter(noPsg>1)
          T$cluster <- NULL
          result <- rbind(result, T) 
        } # end of if
      } # end of k loop
      result <- result[-1,]
      if(nrow(result)>0)  result$Model <- model
      
      ##########
      # end of model 20
    } else 
		if(model == 21) {
      VIP2_espace$weekday <- 0
      VIP2_espace[VIP2_espace$DOW %in% c(1:5), ]$weekday <- 1
      
      # result <- data.frame(ID="", Entr=0, Sor=0, SensEntr = 0, SensSor = 0, Sens = 0, DOW=0, SD=0, T=0, Tmin=0, Tmax=0, noPsg=0)
			result <- data.frame(ID="", OD="", DOW = 0, SD=0, T=0, Tmin=0, Tmax=0, noPsg=0)

			result$ID <- as.character(result$ID)
      for (k in 1:nrow(OD.list)){
        for (j in 0:1){
          temp <- VIP2_espace %>% filter(ID == OD.list$ID[k] &  Entr == OD.list$Entr[k] & Sor == OD.list$Sor[k] & weekday == j)
					max.cluster <- length(unique(temp$TimeSor))
					if(nrow(temp) >= param.min.nb.for.cluster & max.cluster > 1) {				
						# if not many passages, we will not cluster
						#	decide nb of cluster
						clus<- clusGap(temp %>% select(TimeSor), kmeans, max(min(param.max.nb.cluster, max.cluster),2))
            n.cluster <- with(clus,maxSE(Tab[,"gap"],Tab[,"SE.sim"]))
            
            set.seed(1234)
            temp.kmeans <-   kmeans(temp[, "TimeSor"], centers = n.cluster)
            temp$cluster <- temp.kmeans$cluster
            T <- temp %>%
              group_by(ID, OD, DOW, cluster) %>%
              summarise(SD = sd(TimeSor), T = mean(TimeSor),Tmin = T -SD, Tmax = T + SD, noPsg = n())
            T <- T %>% filter(noPsg>1)
            T$cluster <- NULL
            result <- rbind(result, T) 
          } # end of if
        } # end of j loop
      } # end of k loop
      result <- result[-1,]
      if(nrow(result)>0)  result$Model <- model
      ##########
      # end of model 21
    } else 
		if(model == 22) {
      # result <- data.frame(ID="", Entr=0, Sor=0, SensEntr = 0, SensSor = 0, Sens = 0, DOW=0, SD=0, T=0, Tmin=0, Tmax=0, noPsg=0)
			result <- data.frame(ID="", OD="", DOW = 0, SD=0, T=0, Tmin=0, Tmax=0, noPsg=0)

			result$ID <- as.character(result$ID)
      for (k in 1:nrow(OD.list)){
        for (j in 0:6){
          temp <- VIP2_espace %>% filter(ID == OD.list$ID[k] & DOW == j &  Entr == OD.list$Entr[k] & Sor == OD.list$Sor[k] )
					max.cluster <- length(unique(temp$TimeSor))
					if(nrow(temp) >= param.min.nb.for.cluster & max.cluster > 1) {				
						# if not many passages, we will not cluster
						# decide nb of cluster
						clus<- clusGap(temp %>% select(TimeSor), kmeans, max(min(param.max.nb.cluster, max.cluster),2))
            n.cluster <- with(clus,maxSE(Tab[,"gap"],Tab[,"SE.sim"]))
            
            set.seed(1234)
            temp.kmeans <-   kmeans(temp[, "TimeSor"], centers = n.cluster)
            temp$cluster <- temp.kmeans$cluster
            T <- temp %>%
              group_by(ID, OD, DOW, cluster) %>%
              summarise(SD = sd(TimeSor), T = mean(TimeSor),Tmin = T -SD, Tmax = T + SD, noPsg = n())
            T <- T %>% filter(noPsg>1)
            T$cluster <- NULL
            result <- rbind(result, T) 
          } # end of if
        } # end of j loop 
      } # end of k loop 
      
      result <- result[-1,]

			if(nrow(result) > 0)	result$Model <- model
      ##########
      # end of model 22
    }
  } # end of if
  
	if(nrow(result) > 0){
	result$DOW <- as.integer(result$DOW)
  result <- tbl_df(result)
  result <- result %>% select(ID, OD, DOW, Tmin, Tmax, Model, noPsg)
	}
  return(result )
} # end of fuction Model
