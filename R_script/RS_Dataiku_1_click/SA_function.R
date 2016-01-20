##########
##########
# SA_function.R
# first: 20150723
# 1-click
# define all functions

##########
##########
### List of all functions
# GetLngLat:	get the longitude and lattitude of the transaction
# GetResult:	apply model result to test set
# GetInd:			get indicators for each model
# Fusion_OD_2: 		link Lancon and La Barque
##########
### Old functions
# Docompose: 	transform OD -> Troncon

##########

GetLngLat <- function(transaction) {
	# Add longitude and lattitude to the transaction
	# Args:
	#	  tansaction:	Entr, Sor, ...
	# Returns:
	#	  tansaction:	Entr, Sor, ..., ELng, ELat, SLng, SLat
  transaction$Entr <- as.numeric(transaction$Entr)
  transaction$Sor <- as.numeric(transaction$Sor)
	cde <- gares[, c("Cde", "Lng", "Lat")]
	names(cde) <- c("Entr", "ELng", "ELat")
	transaction <- left_join(transaction, cde, by = "Entr")
	names(cde) <- c("Sor", "SLng", "SLat")
	transaction <- left_join(transaction, cde, by = "Sor")
	return(transaction)	
}

GetResult <- function(test, result) {
	# Identify the passage in the test set that the model could predict
	#
	# Args:
	#	  test: set of passages
	#	  result: result of Model in format: ID Entr Sor (DOW) Tmin Tmax
	test$result <- 0
	for (i in 1:nrow(result)){
	  test[
		test$ID   == result$ID[i] &
	    test$Entr == result$Entr[i]&
	    test$Sor  == result$Sor[i]&
	    test$DOW  == result$DOW[i]&
	    test$TimeSor >= result$Tmin[i]&
	    test$TimeSor <= result$Tmax[i],
	    "result"]<-1
	}
	return(test)
}

GetInd <- function(test, result) {
	# Get 3 indicators and combine them to evaluate each model
	#
	# Args:
	#	  test: set of passages
	#	  result: result of Model in format: ID, Entr, Sor, (DOW,) Tmin, Tmax
	Ind <-  test %>%
			group_by(ID) %>%
			summarise(Tpos = sum(result[result == 1]), Fneg = n() - Tpos, Ind1 = Tpos/(Tpos+Fneg), Ind2 = Fneg/(Tpos+Fneg) )
			# Ind1: Sensitivity, Recall
			# Ind2: Miss rate

	result$DOW <- as.character(result$DOW)
	result <- inner_join(test.period, result, by="DOW")
	result$Mark <- 0

	for(i in 1:nrow(test)){
	  result[
	    result$ID   == test$ID[i] &
	      result$Entr == test$Entr[i] &
	      result$Sor  == test$Sor[i] &
	      result$Date  == test$Date[i] &
	      result$Tmin <= test$TimeSor[i]&
	      result$Tmax >= test$TimeSor[i],
	    "Mark"]<-1
	}
	
	Ind3 <- result %>% group_by(ID) %>% summarise(nMark = sum(Mark[Mark==1]), t = (n() - nMark), Ind3 = t/ n() )
	# Ind3: fake alert
	
	Ind <- inner_join(Ind,Ind3)
	Ind <- Ind[, c("ID", "Ind1", "Ind2", "Ind3")]
	# !!! to be justified or modified
	Ind$Ind <- Ind$Ind1 - Ind$Ind3 / 5
	return(Ind)
}

Sens <- function (Transactions){
  # Decide the direction of each transaction (A789)
  # Args:
  #	  Transactions:	ID, Entr, Sor, Date, DOW, WOY, TimeEntr, TimeSor, Voie
  # Returns:
  #	  Transactions:	ID, Entr, Sor, Date, DOW, WOY, TimeEntr, TimeSor, Voie, Sens
  Transactions <- Transactions[, c("ID","Entr","Sor","TimeEntr","TimeSor","DOW","WOY","Date","Voie")]
  Transactions$Sens <- 0
  for (i in 1:nrow(Transactions)){
    if (Transactions$Entr[i] == 0 & ( Transactions$Sor[i] %in% garesorder$Cde) ){ #Si Entr = 0 et Sor dans A789
      if (Transactions$Sor[i] == 25006001){ #Canet de mereuil
        if (Transactions$Voie[i] >= 20 ){ Transactions$Sens[i] <- 1 
        } else 
        { Transactions$Sens[i] <- 2 }
      } else 
        if (Transactions$Sor[i] == 25006010){ #Fréjus
          if (Transactions$Voie[i] >= 20 ){ Transactions$Sens[i] <- 2 
          } else 
          { Transactions$Sens[i] <- 1 }
        } else 
          if (Transactions$Sor[i] == 25006011){ #Les Adrets
            if (Transactions$Voie[i] >= 20 ){Transactions$Sens[i] <- 2 
            } else 
            { Transactions$Sens[i] <- 1}
          } else 
            if (Transactions$Sor[i] == 25006014){ #Antibes Ouest
              if (Transactions$Voie[i] >= 20 ){
                Transactions$Sens[i] <- 1 
              } else 
              {Transactions$Sens[i] <- 2}
            } else 
              if (Transactions$Sor[i] == 25006012){ #Antibes PV
                if (Transactions$Voie[i] >= 20 ){#PV Sud, donc de Cannes vers Nice
                  Transactions$Sens[i] <- 1 
                } else
                { #PV Nord, de Nice vers Cannes
                  Transactions$Sens[i] <- 2}
              } else
                if (Transactions$Sor[i] == 25006024){ #Sophia
                  Transactions$Sens[i] <- 1  #Entrée seule
                } else 
                  if (Transactions$Sor[i] == 25006013){ #Antibes Est
                    if (Transactions$Voie[i] >= 20 ){Transactions$Sens[i] <- 2
                    } else
                    {Transactions$Sens[i] <- 1}  
                  } else
                    if (Transactions$Sor[i] == 25006017){ Transactions$Sens[i] <- 1
                    } else
                      if (Transactions$Sor[i] == 25006015){ Transactions$Sens[i] <- 2
                      } else 
                        if (Transactions$Sor[i] == 25006016){ #Cagnes Est
                          if (Transactions$Voie[i] >= 20 ){Transactions$Sens[i] <- 2
                          } else
                          { Transactions$Sens[i] <- 1}
                        } else
                          if (Transactions$Sor[i] == 25006021){ #Saint Isidore Ech Est
                            if (Transactions$Voie[i] >= 20 ){Transactions$Sens[i] <- 2
                            } else 
                            {Transactions$Sens[i] <- 1}
                          } else 
                            if (Transactions$Sor[i] == 25006021){ #Saint Isidore Ech Est
                              if (Transactions$Voie[i] >= 20 ){Transactions$Sens[i] <- 2
                              } else
                              { Transactions$Sens[i] <- 1} 
                            } else 
                              if (Transactions$Sor[i] == 25006019){ #Saint Isidore Ech Ouest
                                if (Transactions$Voie[i] >= 20 ){Transactions$Sens[i] <- 1 
                                } else
                                {Transactions$Sens[i] <- 2}
                              } else
                                if (Transactions$Sor[i] == 25006020){ #Saint Isidore PV
                                  if (Transactions$Voie[i] >= 20 ){Transactions$Sens[i] <- 2
                                  } else
                                  {Transactions$Sens[i] <- 1}
                                } else 
                                  if (Transactions$Sor[i] == 25006026){ #La Turbie Ech
                                    if (Transactions$Voie[i] >= 20 ){Transactions$Sens[i] <- 1
                                    } else 
                                    {  Transactions$Sens[i] <- 2} 
                                  } else 
                                    if (Transactions$Sor[i] == 25006027){ #La Turbie PV
                                      if (Transactions$Voie[i] >= 20 ){Transactions$Sens[i] <- 2
                                      } else
                                      { Transactions$Sens[i] <- 1 } 
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
  # Combine different OD(S.O. or those like Lancon-La Barque) to form one single real trajet
  # Args:
  #	  transaction:	ID, Entr, Sor, Date, DOW, WOY, TimeEntr, TimeSor, Voie, Sens
  # Returns:
  #	  tansaction:	ID, Entr, Sor, Date, DOW, WOY, TimeEntr, TimeSor, Voie, Sens
  Transactions <- Transactions[, c("ID","Entr","Sor","TimeEntr","TimeSor","DOW","WOY","Date","Voie", "Sens")]
  Transactions$Date <- as.character(Transactions$Date)
  Transactions$TimeEntr <- as.numeric(Transactions$TimeEntr)
  Transactions$TimeSor <- as.numeric(Transactions$TimeSor)
  #First, group the ODs by ID and Date, then arrange by hour
  Transactions <- Transactions %>%
    group_by(ID,Date) %>%
    arrange(TimeSor)
  i <- 1
  #Look for each row if :
  
  while( i < nrow(Transactions)){
    if (Transactions$Date[i] == Transactions$Date [i+1]){
      if(Transactions$Sens[i] == Transactions$Sens[i+1]){
        if (Transactions$Entr[i] == 0) {
          if ( Transactions$Entr[i+1] == 0){
            
            
            # ligne i en SO et i+1 en SO
            if (Transactions$TimeSor[i+1] < (Transactions$TimeSor[i] + 1.5)){ # ???
              if( i == 1){ #if the first row is concerned, we have to treat the case apart, about the fusion and the index.
                Transactions <- rbind(
                  c(Transactions$ID[i],Transactions$Sor[i],Transactions$Sor[i+1],Transactions$TimeSor[i],Transactions$TimeSor[i+1],Transactions$DOW[i],Transactions$WOY[i],Transactions$Date[i],-1,Transactions$Sens[i]),
                  Transactions[(i+2):nrow(Transactions),])
                Transactions$TimeEntr <- as.numeric(Transactions$TimeEntr)
                Transactions$TimeSor <- as.numeric(Transactions$TimeSor)
              } else 
                if (i == (nrow(Transactions)-1)){ #Same if it is for the last two rows.
                  Transactions <- rbind(Transactions[1:(i-1),],
                                        c(Transactions$ID[i],Transactions$Sor[i],Transactions$Sor[i+1],Transactions$TimeSor[i],Transactions$TimeSor[i+1],Transactions$DOW[i],Transactions$WOY[i],Transactions$Date[i],-1,Transactions$Sens[i])
                  )
                  Transactions$TimeEntr <- as.numeric(Transactions$TimeEntr)
                  Transactions$TimeSor <- as.numeric(Transactions$TimeSor)
                } else
                { #General case.
                  Transactions <- rbind(Transactions[1:(i-1),],
                                        c(Transactions$ID[i],Transactions$Sor[i],Transactions$Sor[i+1],Transactions$TimeSor[i],Transactions$TimeSor[i+1],Transactions$DOW[i],Transactions$WOY[i],Transactions$Date[i],-1,Transactions$Sens[i]),
                                        Transactions[(i+2):nrow(Transactions),])
                  Transactions$TimeEntr <- as.numeric(Transactions$TimeEntr)
                  Transactions$TimeSor <- as.numeric(Transactions$TimeSor)
                }
              i <- i-1
            }
          } else 
          {
            
            
            # ligne i en SO et i+1 en SF
            if (Transactions$TimeEntr[i+1] < (Transactions$TimeSor[i] + 1.5)){ # ???
              if( i == 1){ #if the first row is concerned, we have to treat the case apart, about the fusion and the index.
                Transactions <- rbind(
                  c(Transactions$ID[i],Transactions$Sor[i],Transactions$Sor[i+1],Transactions$TimeSor[i],Transactions$TimeSor[i+1],Transactions$DOW[i],Transactions$WOY[i],Transactions$Date[i],-1,Transactions$Sens[i]),
                  Transactions[(i+2):nrow(Transactions),])
                Transactions$TimeEntr <- as.numeric(Transactions$TimeEntr)
                Transactions$TimeSor <- as.numeric(Transactions$TimeSor)
              } else 
                if (i == (nrow(Transactions)-1)){ #Same if it is for the last two rows.
                  Transactions <- rbind(Transactions[1:(i-1),],
                                        c(Transactions$ID[i],Transactions$Sor[i],Transactions$Sor[i+1],Transactions$TimeSor[i],Transactions$TimeSor[i+1],Transactions$DOW[i],Transactions$WOY[i],Transactions$Date[i],-1,Transactions$Sens[i])
                  )
                  Transactions$TimeEntr <- as.numeric(Transactions$TimeEntr)
                  Transactions$TimeSor <- as.numeric(Transactions$TimeSor)
                } else
                { #General case.
                  Transactions <- rbind(Transactions[1:(i-1),],
                                        c(Transactions$ID[i],Transactions$Sor[i],Transactions$Sor[i+1],Transactions$TimeSor[i],Transactions$TimeSor[i+1],Transactions$DOW[i],Transactions$WOY[i],Transactions$Date[i],-1,Transactions$Sens[i]),
                                        Transactions[(i+2):nrow(Transactions),])
                  Transactions$TimeEntr <- as.numeric(Transactions$TimeEntr)
                  Transactions$TimeSor <- as.numeric(Transactions$TimeSor)
                }
              i <- i-1
            }
          }
        } else 
        { 
          if ( Transactions$Entr[i+1] == 0){
            
            
            # ligne i en SF et i+1 en SO
            if (Transactions$TimeSor[i+1] < (Transactions$TimeSor[i] + 1.5)){ # ???
              if( i == 1){ #if the first row is concerned, we have to treat the case apart, about the fusion and the index.
                Transactions <- rbind(
                  c(Transactions$ID[i],Transactions$Entr[i],Transactions$Sor[i+1],Transactions$TimeEntr[i],Transactions$TimeSor[i+1],Transactions$DOW[i],Transactions$WOY[i],Transactions$Date[i],-1,Transactions$Sens[i]),
                  Transactions[(i+2):nrow(Transactions),])
                Transactions$TimeEntr <- as.numeric(Transactions$TimeEntr)
                Transactions$TimeSor <- as.numeric(Transactions$TimeSor)
              } else 
                if (i == (nrow(Transactions)-1)){ #Same if it is for the last two rows.
                  Transactions <- rbind(Transactions[1:(i-1),],
                                        c(Transactions$ID[i],Transactions$Entr[i],Transactions$Sor[i+1],Transactions$TimeEntr[i],Transactions$TimeSor[i+1],Transactions$DOW[i],Transactions$WOY[i],Transactions$Date[i],-1,Transactions$Sens[i])
                  )
                  Transactions$TimeEntr <- as.numeric(Transactions$TimeEntr)
                  Transactions$TimeSor <- as.numeric(Transactions$TimeSor)
                } else
                { #General case.
                  Transactions <- rbind(Transactions[1:(i-1),],
                                        c(Transactions$ID[i],Transactions$Entr[i],Transactions$Sor[i+1],Transactions$TimeEntr[i],Transactions$TimeSor[i+1],Transactions$DOW[i],Transactions$WOY[i],Transactions$Date[i],-1,Transactions$Sens[i]),
                                        Transactions[(i+2):nrow(Transactions),])
                  Transactions$TimeEntr <- as.numeric(Transactions$TimeEntr)
                  Transactions$TimeSor <- as.numeric(Transactions$TimeSor)
                }
              i <- i-1
            }
          } else 
          {
            
            
            # ligne i en SF et i+1 en SF
            if (Transactions$TimeEntr[i+1] < (Transactions$TimeSor[i] + 1.5)){ # ???
              if( i == 1){ #if the first row is concerned, we have to treat the case apart, about the fusion and the index.
                Transactions <- rbind(
                  c(Transactions$ID[i],Transactions$Entr[i],Transactions$Sor[i+1],Transactions$TimeEntr[i],Transactions$TimeSor[i+1],Transactions$DOW[i],Transactions$WOY[i],Transactions$Date[i],-1,Transactions$Sens[i]),
                  Transactions[(i+2):nrow(Transactions),])
                Transactions$TimeEntr <- as.numeric(Transactions$TimeEntr)
                Transactions$TimeSor <- as.numeric(Transactions$TimeSor)
              } else 
                if (i == (nrow(Transactions)-1)){ #Same if it is for the last two rows.
                  Transactions <- rbind(Transactions[1:(i-1),],
                                        c(Transactions$ID[i],Transactions$Entr[i],Transactions$Sor[i+1],Transactions$TimeEntr[i],Transactions$TimeSor[i+1],Transactions$DOW[i],Transactions$WOY[i],Transactions$Date[i],-1,Transactions$Sens[i])
                  )
                  Transactions$TimeEntr <- as.numeric(Transactions$TimeEntr)
                  Transactions$TimeSor <- as.numeric(Transactions$TimeSor)
                } else
                { #General case.
                  Transactions <- rbind(Transactions[1:(i-1),],
                                        c(Transactions$ID[i],Transactions$Entr[i],Transactions$Sor[i+1],Transactions$TimeEntr[i],Transactions$TimeSor[i+1],Transactions$DOW[i],Transactions$WOY[i],Transactions$Date[i],-1,Transactions$Sens[i]),
                                        Transactions[(i+2):nrow(Transactions),])
                  Transactions$TimeEntr <- as.numeric(Transactions$TimeEntr)
                  Transactions$TimeSor <- as.numeric(Transactions$TimeSor)
                }
              i <- i-1
            }
          }
        }
      }
    }
    
    i <- i +1 
  }
  return (Transactions)
}

Model <- function(transaction, model) {
  # Run the model over the transaction set
  # Args:
  #	  transaction:	ID, Entr, Sor, Date, DOW, WOY, TimeEntr, TimeSor, Voie
  #   model: 0,1,2,10,11,12,20,21,22
  # Returns:
  #	  tansaction:	ID, Entr, Sor, DOW, Tmin, Tmax, Model
	parameter = 5
	min.nb.for.cluster = 10
	max.nb.cluster = 5
	para.model.2 = .3
	# ??? parameters to be justified or modified
	
	if(model == 0){
		T.matin <- transaction %>%
			filter(TimeSor < 12 ) %>%
			group_by(ID, Entr, Sor) %>%
			summarise(n = n(), T = mean(TimeSor), SD = sd(TimeSor), Tmin = T -SD, Tmax = T + SD)

		T.aprem <- transaction %>%
			filter(TimeSor >= 12 ) %>%
			group_by(ID, Entr, Sor) %>%
			summarise(n = n(), T = mean(TimeSor), SD = sd(TimeSor), Tmin = T -SD, Tmax = T + SD)

		T <- rbind(T.matin, T.aprem)
		T <- T %>% filter(n > parameter)

		# add DOW
		T$DOW <- 0
		temp <- T
		for(i in 1:6) {
			# add DOW to the T
			temp$DOW <- i
			T <- rbind(T, temp)
		}
		T$Model <- 00
		result <- T
		##########
		# end of model 00
	} else if (model == 1) {
			temp <- transaction 
			temp$weekday <- 0
			temp[temp$DOW %in% c(1:5), ]$weekday <- 1

			T.matin <- temp %>%
				filter(TimeSor < 12 ) %>%
				group_by(ID, Entr, Sor, weekday) %>%
				summarise(n = n(), T = mean(TimeSor), SD = sd(TimeSor), Tmin = T -SD, Tmax = T + SD)

			T.aprem <- temp %>%
				filter(TimeSor >= 12 ) %>%
				group_by(ID, Entr, Sor, weekday) %>%
				summarise(n = n(), T = mean(TimeSor), SD = sd(TimeSor), Tmin = T -SD, Tmax = T + SD)

			T <- rbind(T.matin, T.aprem)
			T <- T %>% filter(n > parameter)

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
			result$Model <- 01
			##########
			# end of model 01
	} else if (model == 2) {
			T.matin <- transaction %>%
				filter(TimeSor < 12 ) %>%
				group_by(ID, Entr, Sor, DOW) %>%
				summarise(nDOW = n(), T = mean(TimeSor), SD = sd(TimeSor), Tmin = T -SD, Tmax = T + SD)

			T.aprem <- transaction %>%
				filter(TimeSor >= 12 ) %>%
				group_by(ID, Entr, Sor, DOW) %>%
				summarise(nDOW = n(), T = mean(TimeSor), SD = sd(TimeSor), Tmin = T -SD, Tmax = T + SD)

			T <- rbind(T.matin, T.aprem)
			T <- T %>% filter(nDOW > parameter)
			
			T$Model <- 02
			result <- T
			##########
			# end of model 02
	} else if(model == 10) {
			result <- data.frame(ID="", Entr=0, Sor=0, SD=0, T=0, Tmin=0, Tmax=0, n=0)
			for (i in 1:nrow(ID.list)) {
				temp <- transaction %>% filter(ID == ID.list$ID[i])
				if(nrow(temp) >= min.nb.for.cluster) {
					max.cluster <- length(unique(temp$TimeSor))
					# if not many passages, we will not cluster
					# decide nb of cluster
					clus<- clusGap(temp[,"TimeSor"], kmeans, min(max.nb.cluster, max.cluster))
					n.cluster <- with(clus,maxSE(Tab[,"gap"],Tab[,"SE.sim"]))
					
					set.seed(1234)
					temp.kmeans <-   kmeans(temp[, "TimeSor"], centers = n.cluster)
					temp$cluster <- temp.kmeans$cluster
					T <- temp %>%
						group_by(ID, Entr, Sor, cluster) %>%
						summarise(n = n(), T = mean(TimeSor), SD = sd(TimeSor), Tmin = T -SD, Tmax = T + SD)
					
					# remove line with SD = N.A.
					T <- T %>% filter(n > 1)
					
					T$cluster <- NULL
					result <- rbind(result, T)
					} # end of if
			} # end of loop i
			result <- result[-1,]
			result <- result %>% filter(n > parameter)

			# add DOW
			result$DOW <- 0
			temp <- result
			for(i in 1:6) {
				# add DOW to the T
				temp$DOW <- i
				result <- rbind(result, temp)
			}

			result$Model <- 10
			##########
			# end of model 10
	} else if(model == 11) {
			temp1 <- transaction 
			temp1$weekday <- 0
			temp1[temp1$DOW %in% c(1:5), ]$weekday <- 1

			# clustering TimeSor
			result <- data.frame(ID="", Entr=0, Sor=0, weekday = 0, SD=0, T=0, Tmin=0, Tmax=0, n=0)
			for (i in 1:nrow(ID.list)) {
				for (j in 0:1) {
					temp <- temp1 %>% filter(ID == ID.list$ID[i] & weekday == j)
					if(nrow(temp) >= 	min.nb.for.cluster) {
						max.cluster <- length(unique(temp$TimeSor))
						# if not many passages, we will not cluster
						# decide nb of cluster
						clus<- clusGap(temp[,"TimeSor"], kmeans, min(max.nb.cluster, max.cluster))
						n.cluster <- with(clus,maxSE(Tab[,"gap"],Tab[,"SE.sim"]))
					
						set.seed(1234)
						temp.kmeans <-   kmeans(temp[, "TimeSor"], centers = n.cluster)
						temp$cluster <- temp.kmeans$cluster
						T <- temp %>%
							group_by(ID, Entr, Sor, weekday, cluster) %>%
							summarise(n = n(), T = mean(TimeSor), SD = sd(TimeSor), Tmin = T -SD, Tmax = T + SD)
					
						# remove line with SD = N.A.
						T <- T %>% filter(n > 1)
					
						T$cluster <- NULL
						result <- rbind(result, T)
						} # end of if
				} # end of loop j
			} # end of loop i

			result <- result[-1,]
			result <- result %>% filter(n > parameter)

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

			result$Model <- 11

			##########
			# end of model 11
	} else if(model == 12) {
			result <- data.frame(ID="", Entr=0, Sor=0, DOW=0, SD=0, T=0, Tmin=0, Tmax=0, n=0)
			for (i in 1:nrow(ID.list)) {
				for (j in 0:6) {
					temp <- train %>% filter(ID == ID.list$ID[i] & DOW == j)
					if(nrow(temp) >= 	min.nb.for.cluster) {
						max.cluster <- length(unique(temp$TimeSor))
						# if not many passages, we will not cluster
						# decide nb of cluster
						clus<- clusGap(temp[,"TimeSor"], kmeans, min(max.nb.cluster, max.cluster))
						n.cluster <- with(clus,maxSE(Tab[,"gap"],Tab[,"SE.sim"]))
						
						set.seed(1234)
						temp.kmeans <-   kmeans(temp[, "TimeSor"], centers = n.cluster)
						temp$cluster <- temp.kmeans$cluster
						T <- temp %>%
							group_by(ID, Entr, Sor, DOW, cluster) %>%
							summarise(n = n(), T = mean(TimeSor), SD = sd(TimeSor), Tmin = T -SD, Tmax = T + SD)
						
						# remove line with SD = N.A.
						T <- T %>% filter(n > 1)
						
						T$cluster <- NULL
						result <- rbind(result, T)
						} # end of if
					} # end of loop j
			} # end of loop i

			result <- result[-1,]
			result <- result %>% filter(n > parameter)

			result$Model <- 12

			##########
			# end of model 12
	} else {
	  ####################
	  ### model.2:	OD -> Space -> Time
	  ####################
			#######
			#COMPTAGE DES TRAJETS PAR TRONCON (PAR SENS ? PAS ENCORE)
			#######
			Compteur <- train %>% 
				group_by(ID, Entr,Sor) %>%
				summarise( n = n())

			########
			#DECISION AUTOMATIQUE DES TRONCONS FREQUENTS
			########
			# ??? [Pourcentagedumax max ; max ]
			Pourcentagedumax <- para.model.2
			Troncon_Selection <- Compteur %>% 
				group_by(ID) %>%
				filter (n > (Pourcentagedumax * max(n)))

			VIP2_espace <- inner_join(train, Troncon_Selection, by = c("ID","Entr","Sor"))

			########
			# Kmeans 
			########
			VIP2_espace$TimeEntr <- as.numeric(VIP2_espace$TimeEntr)
			VIP2_espace$TimeSor <- as.numeric(VIP2_espace$TimeSor)
			VIP2_espace$Entr <- as.numeric(VIP2_espace$Entr)
			VIP2_espace$Sor <- as.numeric(VIP2_espace$Sor)
			VIP2_espace <- tbl_df(VIP2_espace)

			########
			#Create OD.list for the kmeans

			OD.list <- VIP2_espace %>% group_by( Entr,Sor) %>% summarise()
			
			if(model == 20) {
				result <- data.frame(ID="", Entr=0, Sor=0, DOW=0, SD=0, T=0, Tmin=0, Tmax=0, n=0)
				for (k in 1:nrow(OD.list)){
					for (i in 1:nrow(ID.list)){
						temp <- VIP2_espace %>% filter(ID == ID.list$ID[i] &  Entr == OD.list$Entr[k] & Sor == OD.list$Sor[k] )
						# base to be verified
						if(nrow(temp) >= 	min.nb.for.cluster) {
							# if not many passages, we will not cluster
							max.cluster <- length(unique(temp$TimeSor))
							# decide nb of cluster
							clus<- clusGap(temp[,"TimeSor"], kmeans, min(5, max.cluster))
							n.cluster <- with(clus,maxSE(Tab[,"gap"],Tab[,"SE.sim"]))
							
							set.seed(1234)
							temp.kmeans <-   kmeans(temp[, "TimeSor"], centers = n.cluster)
							temp$cluster <- temp.kmeans$cluster
							T <- temp %>%
								group_by(ID, Entr, Sor, DOW, cluster) %>%
								summarise(SD = sd(TimeSor), T = mean(TimeSor),Tmin = T -SD, Tmax = T + SD, n = n())
							T <- T %>% filter(n>1)
							T$cluster <- NULL
							result <- rbind(result, T) 
						}
					}
				}
				result <- result[-1,]
				result$Model <- 20	

				##########
				# end of model 20
			} else if(model == 21) {
					VIP2_espace$weekday <- 0
					VIP2_espace[VIP2_espace$DOW %in% c(1:5), ]$weekday <- 1

					result <- data.frame(ID="", Entr=0, Sor=0, DOW=0, SD=0, T=0, Tmin=0, Tmax=0, n=0)
					for (k in 1:nrow(OD.list)){
						for (i in 1:nrow(ID.list)){
							for (j in 0:1){
								temp <- VIP2_espace %>% filter(ID == ID.list$ID[i] &  Entr == OD.list$Entr[k] & Sor == OD.list$Sor[k] & weekday == j)
								if(nrow(temp) >= 	min.nb.for.cluster) {
									# if not many passages, we will not cluster
									max.cluster <- length(unique(temp$TimeSor))
									# decide nb of cluster
									clus<- clusGap(temp[,"TimeSor"], kmeans, min(5, max.cluster))
									n.cluster <- with(clus,maxSE(Tab[,"gap"],Tab[,"SE.sim"]))
									
									set.seed(1234)
									temp.kmeans <-   kmeans(temp[, "TimeSor"], centers = n.cluster)
									temp$cluster <- temp.kmeans$cluster
									T <- temp %>%
										group_by(ID, Entr, Sor, DOW, cluster) %>%
										summarise(SD = sd(TimeSor), T = mean(TimeSor),Tmin = T -SD, Tmax = T + SD, n = n())
									T <- T %>% filter(n>1)
									T$cluster <- NULL
									result <- rbind(result, T) 
								}
							}
						}
					}

				result <- result[-1,]
				result$Model <- 21

				##########
				# end of model 21
			} else if(model == 22) {
					result <- data.frame(ID="", Entr=0, Sor=0, DOW=0, SD=0, T=0, Tmin=0, Tmax=0, n=0)
					for (k in 1:nrow(OD.list)){
						for (i in 1:nrow(ID.list)){
							for (j in 0:6){
								temp <- VIP2_espace %>% filter(ID == ID.list$ID[i] & DOW == j &  Entr == OD.list$Entr[k] & Sor == OD.list$Sor[k] )

								if(nrow(temp) >= 	min.nb.for.cluster) {
									# if not many passages, we will not cluster
									
									max.cluster <- length(unique(temp$TimeSor))
									# decide nb of cluster
									clus<- clusGap(temp[,"TimeSor"], kmeans, min(5, max.cluster))
									n.cluster <- with(clus,maxSE(Tab[,"gap"],Tab[,"SE.sim"]))
									
									set.seed(1234)
									temp.kmeans <-   kmeans(temp[, "TimeSor"], centers = n.cluster)
									temp$cluster <- temp.kmeans$cluster
									T <- temp %>%
										group_by(ID, Entr, Sor, DOW, cluster) %>%
										summarise(SD = sd(TimeSor), T = mean(TimeSor),Tmin = T -SD, Tmax = T + SD, n = n())
									T <- T %>% filter(n>1)
									T$cluster <- NULL
									result <- rbind(result, T) 
								}
							}
						}
					}

					result <- result[-1,]
					result$Model <- 22

				##########
				# end of model 22
			}
	} # end of if
	
	result$DOW <- as.integer(result$DOW)
	return(result[, c("ID", "Entr", "Sor", "DOW", "Tmin", "Tmax", "Model")])
} # end of fuction Model

##########
### OLD functions
# Fusion_OD_2 <- function ( Transactions) {
#   # Connect OD Lancon - La Barque
#   #
#   #Args: ID  Entr  Sor  TimeEntr  TimeSor  DOW  WOY  Date  Voie 
#   Transactions$Date <- as.character.Date(Transactions$Date)
#   Transactions$TimeEntr <- as.numeric(Transactions$TimeEntr)
#   Transactions$TimeSor <- as.numeric(Transactions$TimeSor)
#   Transactions$Entr <- as.numeric(Transactions$Entr)
#   Transactions$Sor <- as.numeric(Transactions$Sor)
#   
#   Transactions <- Transactions[, c("ID", "Entr", "Sor", "TimeEntr", "TimeSor", "DOW", "WOY", "Date", "Voie")]
#   
#   i <- 1
#   #On commence par trier par ID et Date Heure chronologique
#   Transactions <- Transactions %>%
#     group_by(ID,Date) %>%
#     arrange(TimeSor)
#   
#   while (i < nrow(Transactions)){
#     if (Transactions$ID[i] == Transactions$ID[i+1]) {
#       if (Transactions$Sor[i] == 25004220 ) { #Sortie = Lancon
#         if((Transactions$Entr[i+1] == 25006002) & #Next Entrée = La Barque
#            (Transactions$TimeSor[i] < Transactions$TimeEntr[i+1]) & #two transaction happened within an hour
#            (Transactions$TimeEntr[i+1] < (Transactions$TimeSor[i] + 1.5))){ 
#           if( i == 1){ #if the first row is concerned, we have to treat the case apart, about the fusion and the index.
#             Transactions <- rbind(c(Transactions$ID[i],Transactions$Entr[i],Transactions$Sor[i+1],
#                                     Transactions$TimeEntr[i],Transactions$TimeSor[i+1],
#                                     Transactions$DOW[i],Transactions$WOY[i],Transactions$Date[i],Transactions$Voie[i]),
#                                   Transactions[3:nrow(Transactions),])
#             Transactions$TimeEntr <- as.numeric(Transactions$TimeEntr)
#             Transactions$TimeSor <- as.numeric(Transactions$TimeSor)
#           }
#           else if (i == (nrow(Transactions)-1)){ #Same if it is for the last two rows.
#             Transactions <- rbind(Transactions[1:(nrow(Transactions)-2),],
#                                   c(Transactions$ID[i],Transactions$Entr[i],Transactions$Sor[i+1],
#                                     Transactions$TimeEntr[i],Transactions$TimeSor[i+1],
#                                     Transactions$DOW[i],Transactions$WOY[i],Transactions$Date[i],Transactions$Voie[i]))
#             Transactions$TimeEntr <- as.numeric(Transactions$TimeEntr)
#             Transactions$TimeSor <- as.numeric(Transactions$TimeSor)
#           }
#           else{ #General case.
#             Transactions <- rbind(Transactions[1:(i-1),],
#                                   c(Transactions$ID[i],Transactions$Entr[i],Transactions$Sor[i+1],
#                                     Transactions$TimeEntr[i],Transactions$TimeSor[i+1],
#                                     Transactions$DOW[i],Transactions$WOY[i],Transactions$Date[i],Transactions$Voie[i]),
#                                   Transactions[(i+2):nrow(Transactions),])
#             Transactions$TimeEntr <- as.numeric(Transactions$TimeEntr)
#             Transactions$TimeSor <- as.numeric(Transactions$TimeSor)
#           } 
#         } # end of Lancon -> La Barque
#       } # end of Sortie = Lancon
#       else if (Transactions$Sor[i] == 25006002){ # Sortie = La Barque
#         if((Transactions$Entr[i+1] == 25004220) & #Next Entrée = Lancon
#            (Transactions$TimeSor[i] < Transactions$TimeEntr[i+1]) & #two transaction happened within an hour
#            (Transactions$TimeEntr[i+1] < (Transactions$TimeSor[i] + 1.5))){
#           if( i == 1){
#             Transactions <- rbind(c(Transactions$ID[i],Transactions$Entr[i],Transactions$Sor[i+1],
#                                     Transactions$TimeEntr[i],Transactions$TimeSor[i+1],
#                                     Transactions$DOW[i],Transactions$WOY[i],Transactions$Date[i],Transactions$Voie[i]),
#                                   Transactions[3:nrow(Transactions),])
#             Transactions$TimeEntr <- as.numeric(Transactions$TimeEntr)
#             Transactions$TimeSor <- as.numeric(Transactions$TimeSor)
#           }
#           else if (i == (nrow(Transactions)-1)){
#             Transactions <- rbind(Transactions[1:(nrow(Transactions)-2),],
#                                   c(Transactions$ID[i],Transactions$Entr[i],Transactions$Sor[i+1],
#                                     Transactions$TimeEntr[i],Transactions$TimeSor[i+1],
#                                     Transactions$DOW[i],Transactions$WOY[i],Transactions$Date[i],Transactions$Voie[i]))
#             Transactions$TimeEntr <- as.numeric(Transactions$TimeEntr)
#             Transactions$TimeSor <- as.numeric(Transactions$TimeSor)
#           }
#           else{
#             Transactions <- rbind(Transactions[1:(i-1),],
#                                   c(Transactions$ID[i],Transactions$Entr[i],Transactions$Sor[i+1],
#                                     Transactions$TimeEntr[i],Transactions$TimeSor[i+1],
#                                     Transactions$DOW[i],Transactions$WOY[i],Transactions$Date[i],Transactions$Voie[i]),
#                                   Transactions[(i+2):nrow(Transactions),])
#             Transactions$TimeEntr <- as.numeric(Transactions$TimeEntr)
#             Transactions$TimeSor <- as.numeric(Transactions$TimeSor)
#           }
#         }# end of La Barque - Lancon
#       }# end of Sortie = La Barque
#     }
#     i <- i + 1 
#   } # end of while
#   return (Transactions)
# }
# 
# BeforeDecompose <- function(Transaction) {
# 	Transaction$KMS <- 0
# 	return(Transaction)
# }
# 
# AfterDecompose <- function(Transaction) {
# 	Transaction <- Transaction[, c("ID", "Entr", "Sor", "Date", "DOW", "WOY", "TimeEntr", "TimeSor")]
# 	Transaction$DOW <- as.numeric(Transaction$DOW)
# 	Transaction$WOY <- as.numeric(Transaction$WOY)
# 	Transaction$TimeEntr <- as.numeric(Transaction$TimeEntr)
# 	Transaction$TimeSor <- as.numeric(Transaction$TimeSor)
# 	Transaction <- tbl_df(Transaction)
# 	return(Transaction)
# }
# 
# Decompose <- function ( Transactions ) {
# 	#DECOMPOSER LES TRAJETS PAR AUTOROUTE UNIQUE
# 	# Args:
# 	#		Transactions: ID, Entr, Sor, Date, DOW, WOY, TimeEntr, TimeSor, Voie
# 	# Returns:
# 	# 	Transactions: ID, ID_Troncon, Autoroute, Entr, Sor, KMS, Date, TimeEntr, TimeSor, DOW, WOY, Sens
#   gares$Autoroute <- as.character(gares$Autoroute)
#   Transactions$Entr <- as.numeric(as.character(Transactions$Entr))
#   Transactions$Sor <- as.numeric(as.character(Transactions$Sor))
#   Autoroute <- vector(mode = "character", length = nrow(Transactions))
#   Transactions_decompose <- cbind(Transactions, Autoroute)
#   Transactions_decompose$Autoroute <- as.character(Transactions_decompose$Autoroute)
#   Transactions_restant <- Transactions
#   Pointeur <- 1
#   Pointeur_restant <- 1
#   for (i in 1 : nrow(Transactions)){
#     if (  ( (Transactions$Entr[i] %in% Troncons_A789[,4]) | (Transactions$Entr[i] %in% Troncons_A789[,6])  ) & 
#           ( (Transactions$Sor[i] %in% Troncons_A789[,4]) | (Transactions$Sor[i] %in% Troncons_A789[,6])  )  ) {  ### if OD in A789
#       if (gares$Autoroute[match(Transactions$Entr[i],gares$Cde)] !=  gares$Autoroute[match(Transactions$Sor[i],gares$Cde)] ) { #E=A7etS=A9 ou l'inverse
#         if (Transactions$Entr[i] != 25004210 & Transactions$Sor[i] != 25004210){  # Entrée et sortie <> Orange Centre
#           if(gares$Autoroute[match(Transactions$Entr[i],gares$Cde)] == "A7"){# E=A7 S=A9
#             newrow1 <- c(Transactions$ID[i],Transactions$Entr[i],25004210,Transactions$TimeEntr[i],Transactions$TimeSor[i],Transactions$DOW[i],Transactions$WOY[i],Transactions$Voie[i],Transactions$Date[i],"A7")
#             newrow2 <- c(Transactions$ID[i],25004210,Transactions$Sor[i],Transactions$TimeEntr[i],Transactions$TimeSor[i],Transactions$DOW[i],Transactions$WOY[i],Transactions$Voie[i],Transactions$Date[i],"A9")
#             Transactions_decompose = rbind(Transactions_decompose[1:Pointeur,],newrow1,newrow2,Transactions_decompose[-(1:Pointeur),])
#             Pointeur <- Pointeur +2
#           }
#           else if(gares$Autoroute[match(Transactions$Entr[i],gares$Cde)] == "A9"){# E=A9 S=A7
#             newrow1 <- c(Transactions$ID[i],Transactions$Entr[i],25004210,Transactions$TimeEntr[i],Transactions$TimeSor[i],Transactions$DOW[i],Transactions$WOY[i],Transactions$Voie[i],Transactions$Date[i],"A9")
#             newrow2 <- c(Transactions$ID[i],25004210,Transactions$Sor[i],Transactions$TimeEntr[i],Transactions$TimeSor[i],Transactions$DOW[i],Transactions$WOY[i],Transactions$Voie[i],Transactions$Date[i],"A7")
#             Transactions_decompose = rbind(Transactions_decompose[1:Pointeur,],newrow1,newrow2,Transactions_decompose[-(1:Pointeur),])
#             Pointeur <- Pointeur +2
#           }
#         }
#         else { # E=A9 S=OC ou l'inverse
#           Transactions_decompose$Autoroute[Pointeur] <- "A9"
#         }
#       }
#       else {
#         Transactions_decompose$Autoroute[Pointeur] <- gares$Autoroute[match(Transactions$Entr[i],gares$Cde)]
#       } ### end of if OD is in A789
#       if (Pointeur_restant == 1){
#         Transactions_restant <- Transactions_restant[2:nrow(Transactions_restant),]
#       }
#       else {
#         Transactions_restant <- rbind(Transactions_restant[(1:(Pointeur_restant-1)),],Transactions_restant[-(1:(Pointeur_restant)),])
#         Pointeur_restant <- Pointeur_restant - 1
#       }
#     }
#     Pointeur <- Pointeur +1
#     Pointeur_restant <- Pointeur_restant +1
#   }
#     
#   Transactions_decompose <- Transactions_decompose[Transactions_decompose$Autoroute > 0,]
#     
#   #DECOMPOSER LES OD PAR TRONCONS :
#   Transactions_par_troncons <- data.frame(ID = "", Entr= 0, Sor=0, Date = 0, TimeEntr = 0, TimeSor =0, DOW=0, WOY=0,Voie = 0)
#   Transactions_par_troncons$ID <- as.character(Transactions_par_troncons$ID)
#   Transactions_decompose$Date <- as.character(Transactions_decompose$Date)
#   Transactions_par_troncons$Date <- as.character(Transactions_par_troncons$Date)
#   
#   for (i in 1:nrow(Transactions_decompose)){
#     if ( Transactions_decompose$Autoroute[i] == "A7"){
#       entree <- match(Transactions_decompose$Entr[i],A7_par_pk$Cde)
#       sortie <- match(Transactions_decompose$Sor[i],A7_par_pk$Cde)
#       if ( entree < sortie ) { # SENS 1
#         for ( j in entree : (sortie-1) ){
#           Transactions_par_troncons <- rbind(Transactions_par_troncons,c(Transactions_decompose$ID[i],A7_par_pk$Cde[j],A7_par_pk$Cde[j+1],Transactions_decompose$Date[i],Transactions_decompose$TimeEntr[i],Transactions_decompose$TimeSor[i],Transactions_decompose$DOW[i],Transactions_decompose$WOY[i],Transactions_decompose$Voie[i]))
#         }
#       }
#       else{ #SENS 2
#         for ( j in entree : (sortie+1) ){
#           Transactions_par_troncons <- rbind(Transactions_par_troncons,c(Transactions_decompose$ID[i],A7_par_pk$Cde[j],A7_par_pk$Cde[j-1],Transactions_decompose$Date[i],Transactions_decompose$TimeEntr[i],Transactions_decompose$TimeSor[i],Transactions_decompose$DOW[i],Transactions_decompose$WOY[i],Transactions_decompose$Voie[i]))
#         }
#       }
#     }
#     else if ( Transactions_decompose$Autoroute[i] == "A8"){
#       entree <- match(Transactions_decompose$Entr[i],A8_par_pk$Cde)
#       sortie <- match(Transactions_decompose$Sor[i],A8_par_pk$Cde)
#       if ( entree < sortie ) { # SENS 1
#         for ( j in entree : (sortie-1) ){
#           Transactions_par_troncons <- rbind(Transactions_par_troncons,c(Transactions_decompose$ID[i],A8_par_pk$Cde[j],A8_par_pk$Cde[j+1],Transactions_decompose$Date[i],Transactions_decompose$TimeEntr[i],Transactions_decompose$TimeSor[i],Transactions_decompose$DOW[i],Transactions_decompose$WOY[i],Transactions_decompose$Voie[i]))
#         }
#       }
#       else{ #SENS 2
#         for ( j in entree : (sortie+1) ){
#           Transactions_par_troncons <- rbind(Transactions_par_troncons,c(Transactions_decompose$ID[i],A8_par_pk$Cde[j],A8_par_pk$Cde[j-1],Transactions_decompose$Date[i],Transactions_decompose$TimeEntr[i],Transactions_decompose$TimeSor[i],Transactions_decompose$DOW[i],Transactions_decompose$WOY[i],Transactions_decompose$Voie[i]))
#         }
#       }
#     }
#     else if ( Transactions_decompose$Autoroute[i] == "A9"){
#       entree <- match(Transactions_decompose$Entr[i],A9_par_pk$Cde)
#       sortie <- match(Transactions_decompose$Sor[i],A9_par_pk$Cde)
#       if ( entree < sortie ) { # SENS 1
#         for ( j in entree : (sortie-1) ){
#           Transactions_par_troncons <- rbind(Transactions_par_troncons,c(Transactions_decompose$ID[i],A9_par_pk$Cde[j],A9_par_pk$Cde[j+1],Transactions_decompose$Date[i],Transactions_decompose$TimeEntr[i],Transactions_decompose$TimeSor[i],Transactions_decompose$DOW[i],Transactions_decompose$WOY[i],Transactions_decompose$Voie[i]))
#         }
#       }
#       else{ #SENS 2
#         for ( j in entree : (sortie+1) ){
#           Transactions_par_troncons <- rbind(Transactions_par_troncons,c(Transactions_decompose$ID[i],A9_par_pk$Cde[j],A9_par_pk$Cde[j-1],Transactions_decompose$Date[i],Transactions_decompose$TimeEntr[i],Transactions_decompose$TimeSor[i],Transactions_decompose$DOW[i],Transactions_decompose$WOY[i],Transactions_decompose$Voie[i]))
#         }
#       }
#     }
#   }
#   Transactions_par_troncons <- Transactions_par_troncons[-1,]
#   
#   
#   #Rajouter demi trajet LANCON LA BARQUE
#   Pointeur <- 1
#   for (i in 1:nrow(Transactions_par_troncons)){
#     if ( Transactions_par_troncons$Sor[Pointeur]==25004220){ # (A7 -> Lancon)
#       newrow1 <- c(Transactions_par_troncons$ID[Pointeur],25004220,25004278,Transactions_par_troncons$Date[Pointeur],Transactions_par_troncons$TimeEntr[Pointeur],Transactions_par_troncons$TimeSor[Pointeur],Transactions_par_troncons$DOW[Pointeur],Transactions_par_troncons$WOY[Pointeur],Transactions_par_troncons$Voie[Pointeur])
#       newrow2 <- c(Transactions_par_troncons$ID[Pointeur],25004278,25004279,Transactions_par_troncons$Date[Pointeur],Transactions_par_troncons$TimeEntr[Pointeur],Transactions_par_troncons$TimeSor[Pointeur],Transactions_par_troncons$DOW[Pointeur],Transactions_par_troncons$WOY[Pointeur],Transactions_par_troncons$Voie[Pointeur])
#       Transactions_par_troncons <- rbind(Transactions_par_troncons[(1:Pointeur),],newrow1,newrow2,Transactions_par_troncons[-(1:Pointeur),])
#       Pointeur <- Pointeur + 2
#     } 
#     if (Transactions_par_troncons$Entr[Pointeur]==25004220){ # (Lancon -> A7)
#       newrow1 <- c(Transactions_par_troncons$ID[Pointeur],25004279,25004278,Transactions_par_troncons$Date[Pointeur],Transactions_par_troncons$TimeEntr[Pointeur],Transactions_par_troncons$TimeSor[Pointeur],Transactions_par_troncons$DOW[Pointeur],Transactions_par_troncons$WOY[Pointeur],Transactions_par_troncons$Voie[Pointeur])
#       newrow2 <- c(Transactions_par_troncons$ID[Pointeur],25004278,25004220,Transactions_par_troncons$Date[Pointeur],Transactions_par_troncons$TimeEntr[Pointeur],Transactions_par_troncons$TimeSor[Pointeur],Transactions_par_troncons$DOW[Pointeur],Transactions_par_troncons$WOY[Pointeur],Transactions_par_troncons$Voie[Pointeur])
#       Transactions_par_troncons <- rbind(Transactions_par_troncons[(1:Pointeur),],newrow1,newrow2,Transactions_par_troncons[-(1:Pointeur),])
#       Pointeur <- Pointeur + 2
#     }
#     if (Transactions_par_troncons$Sor[Pointeur] == 25006002){ # (La Barque -> A8)
#       newrow1 <- c(Transactions_par_troncons$ID[Pointeur],25004279,25006001,Transactions_par_troncons$Date[Pointeur],Transactions_par_troncons$TimeEntr[Pointeur],Transactions_par_troncons$TimeSor[Pointeur],Transactions_par_troncons$DOW[Pointeur],Transactions_par_troncons$WOY[Pointeur],Transactions_par_troncons$Voie[Pointeur])
#       newrow2 <- c(Transactions_par_troncons$ID[Pointeur],25006001,25006080,Transactions_par_troncons$Date[Pointeur],Transactions_par_troncons$TimeEntr[Pointeur],Transactions_par_troncons$TimeSor[Pointeur],Transactions_par_troncons$DOW[Pointeur],Transactions_par_troncons$WOY[Pointeur],Transactions_par_troncons$Voie[Pointeur])
#       newrow3 <- c(Transactions_par_troncons$ID[Pointeur],25006080,25006002,Transactions_par_troncons$Date[Pointeur],Transactions_par_troncons$TimeEntr[Pointeur],Transactions_par_troncons$TimeSor[Pointeur],Transactions_par_troncons$DOW[Pointeur],Transactions_par_troncons$WOY[Pointeur],Transactions_par_troncons$Voie[Pointeur])
#       Transactions_par_troncons <- rbind(Transactions_par_troncons[(1:Pointeur),],newrow1,newrow2,newrow3,Transactions_par_troncons[-(1:Pointeur),])
#       Pointeur <- Pointeur + 3
#     }
#     if (Transactions_par_troncons$Entr[Pointeur] == 25006002){ # (A8 -> La Barque)
#       newrow1 <- c(Transactions_par_troncons$ID[Pointeur],25006002,25006080,Transactions_par_troncons$Date[Pointeur],Transactions_par_troncons$TimeEntr[Pointeur],Transactions_par_troncons$TimeSor[Pointeur],Transactions_par_troncons$DOW[Pointeur],Transactions_par_troncons$WOY[Pointeur],Transactions_par_troncons$Voie[Pointeur])
#       newrow2 <- c(Transactions_par_troncons$ID[Pointeur],25006080,25006001,Transactions_par_troncons$Date[Pointeur],Transactions_par_troncons$TimeEntr[Pointeur],Transactions_par_troncons$TimeSor[Pointeur],Transactions_par_troncons$DOW[Pointeur],Transactions_par_troncons$WOY[Pointeur],Transactions_par_troncons$Voie[Pointeur])
#       newrow3 <- c(Transactions_par_troncons$ID[Pointeur],25006001,25004279,Transactions_par_troncons$Date[Pointeur],Transactions_par_troncons$TimeEntr[Pointeur],Transactions_par_troncons$TimeSor[Pointeur],Transactions_par_troncons$DOW[Pointeur],Transactions_par_troncons$WOY[Pointeur],Transactions_par_troncons$Voie[Pointeur])
#       Transactions_par_troncons <- rbind(Transactions_par_troncons[(1:Pointeur),],newrow1,newrow2,newrow3,Transactions_par_troncons[-(1:Pointeur),])
#       Pointeur <- Pointeur + 3
#     }
#     Pointeur <- Pointeur + 1
#   }
#   
#   return ( list(result = Transactions_par_troncons,rest = Transactions_restant) )
# }
