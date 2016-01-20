##########
##########
# 20150723
##########
##########
### List of all functions
# GetLngLat:	get the longitude and lattitude of the transaction
# GetResult:	apply model result to test set
# GetInd:		get indicators for each model
# Docompose: 	transform OD -> Troncon

##########

GetLngLat <- function(transaction) {
	# Add longitude and lattitude to the transaction
	# Args:
	#	transaction:	Entr, Sor, ...
	# Returns:
	#	transaction:	Entr, Sor, ..., ELng, ELat, SLng, SLat
  transaction$Entr <- as.numeric(transaction$Entr)
  transaction$Sor <- as.numeric(transaction$Sor)
	cde <- gares[, c("Cde", "Lng", "Lat")]
	names(cde) <- c("Entr", "ELng", "ELat")
	transaction <- left_join(transaction, cde, by = "Entr")
	names(cde) <- c("Sor", "SLng", "SLat")
	transaction <- left_join(transaction, cde, by = "Sor")
	return(transaction)	
}


GetResult <- function(test, result,
					  DOW = TRUE ) {
	# Identify the passage in the test set that the model could predict
	#
	# Args:
	#	test: set of passages
	#	result: result of Model in format: ID Entr Sor (DOW) Tmin Tmax
	#	DOW: if the model considers DOW
	test$result <- 0
	if (DOW == TRUE) {
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
		# DOW == TRUE 		
	} else {
		for (i in 1:nrow(result)){
		  test[
			test$ID   == result$ID[i] &
		    test$Entr == result$Entr[i]&
		    test$Sor  == result$Sor[i]&
		    test$TimeSor >= result$Tmin[i]&
		    test$TimeSor <= result$Tmax[i],
		    "result"]<-1
		}
		# DOW == FALSE
	}	
	return(test)
}


GetInd <- function(test, result,
				   DOW = TRUE) {
	# Get 3 indicators and combine them to evaluate each model
	#
	# Args:
	#	test: set of passages
	#	result: result of Model in format: ID, Entr, Sor, (DOW,) Tmin, Tmax
	#	DOW: if the model considers DOW
	Ind <-  test %>%
			group_by(ID) %>%
			summarise(Tpos = sum(result[result == 1]), Fneg = n() - Tpos, Ind1 = Tpos/(Tpos+Fneg), Ind2 = Fneg/(Tpos+Fneg) )
			# Ind1: Sensitivity, Recall
			# Ind2: Miss rate
			
	result <- inner_join(test.period, result, by="DOW")
	result$Mark <- 0

	# result$DOW <- as.character(result$DOW)
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
	Ind$Ind <- Ind$Ind1 - Ind$Ind2 - Ind$Ind3 / 5
	return(Ind)
}



BeforeDecompose <- function(Transaction) {
	Transaction$KMS <- 0
	return(Transaction)
}

AfterDecompose <- function(Transaction) {
	Transaction <- Transaction[, c("ID", "Entr", "Sor", "Date", "DOW", "WOY", "TimeEntr", "TimeSor")]
	Transaction$DOW <- as.numeric(Transaction$DOW)
	Transaction$WOY <- as.numeric(Transaction$WOY)
	Transaction$TimeEntr <- as.numeric(Transaction$TimeEntr)
	Transaction$TimeSor <- as.numeric(Transaction$TimeSor)
	Transaction <- tbl_df(Transaction)
	return(Transaction)
}

# Args:
#		Transactions: ID, Entr, Sor, Date, DOW, WOY, TimeEntr, TimeSor, KMS
# Returns:
# 	  ID, ID_Troncon, Autoroute, Entr, Sor, KMS, Date, TimeEntr, TimeSor, DOW, WOY, Sens
Decompose <- function ( Transactions ) {
	#DECOMPOSER LES TRAJETS PAR AUTOROUTE UNIQUE
	#Et traiter le système ouvert
	gares$Autoroute <- as.character(gares$Autoroute)
	Transactions$Entr <- as.numeric(as.character(Transactions$Entr))
	Transactions$Sor <- as.numeric(as.character(Transactions$Sor))
	Autoroute <- vector(mode = "character", length = nrow(Transactions))
	Transactions_decompose <- cbind(Transactions, Autoroute)
	Transactions_decompose$Autoroute <- as.character(Transactions_decompose$Autoroute)
	Transactions_restant <- Transactions
	Pointeur <- 1
	Pointeur_restant <- 1
	Gare_inconnue <- 0 #Gares du système ouvert que le code ne connait pas
	for (i in 1 : nrow(Transactions)){
	  if (  ( (Transactions$Entr[i] %in% Troncons_A789[,4]) | (Transactions$Entr[i] %in% Troncons_A789[,6])  ) & 
	        ( (Transactions$Sor[i] %in% Troncons_A789[,4]) | (Transactions$Sor[i] %in% Troncons_A789[,6])  )  ) {  # SI TRAJET DANS LES TROIS AUTOROUTES
	    if (gares$Autoroute[match(Transactions$Entr[i],gares$Cde)] !=  gares$Autoroute[match(Transactions$Sor[i],gares$Cde)] ) { #E=A7etS=A9 ou l'inverse
	      if (Transactions$Entr[i] != 25004210 & Transactions$Sor[i] != 25004210){  # Entrée et sortie <> Orange Centre
	        if(gares$Autoroute[match(Transactions$Entr[i],gares$Cde)] == "A7"){# E=A7 S=A9
	          newrow1 <- c(Transactions[i,1],Transactions$Entr[i],25004210,Transactions$KMS[i],Transactions$Year[i],Transactions$Month[i],Transactions$Day[i],Transactions$Hour[i],Transactions$Minute[i],Transactions$TimeEntr[i],Transactions$TimeSor[i],Transactions$DOW[i],Transactions$WOY[i],Transactions$Voie[i],"A7")
	          newrow2 <- c(Transactions[i,1],25004210,Transactions$Sor[i],Transactions$KMS[i],Transactions$Year[i],Transactions$Month[i],Transactions$Day[i],Transactions$Hour[i],Transactions$Minute[i],Transactions$TimeEntr[i],Transactions$TimeSor[i],Transactions$DOW[i],Transactions$WOY[i],Transactions$Voie[i],"A9")
	          Transactions_decompose = rbind(Transactions_decompose[1:Pointeur,],newrow1,newrow2,Transactions_decompose[-(1:Pointeur),])
	          Pointeur <- Pointeur +2
	        }
	        else if(gares$Autoroute[match(Transactions$Entr[i],gares$Cde)] == "A9"){# E=A9 S=A7
	          newrow1 <- c(Transactions[i,1],Transactions$Entr[i],25004210,Transactions$KMS[i],Transactions$Year[i],Transactions$Month[i],Transactions$Day[i],Transactions$Hour[i],Transactions$Minute[i],Transactions$TimeEntr[i],Transactions$TimeSor[i],Transactions$DOW[i],Transactions$WOY[i],Transactions$Voie[i],"A9")
	          newrow2 <- c(Transactions[i,1],25004210,Transactions$Sor[i],Transactions$KMS[i],Transactions$Year[i],Transactions$Month[i],Transactions$Day[i],Transactions$Hour[i],Transactions$Minute[i],Transactions$TimeEntr[i],Transactions$TimeSor[i],Transactions$DOW[i],Transactions$WOY[i],Transactions$Voie[i],"A7")
	          Transactions_decompose = rbind(Transactions_decompose[1:Pointeur,],newrow1,newrow2,Transactions_decompose[-(1:Pointeur),])
	          Pointeur <- Pointeur +2
	        }
	      }
	      else { # E=A9 S=OC ou l'inverse
	        Transactions_decompose$Autoroute[Pointeur] <- "A9"
	      }
	    }
	    else {
	      Transactions_decompose$Autoroute[Pointeur] <- gares$Autoroute[match(Transactions$Entr[i],gares$Cde)]
	    }
	    if (Pointeur_restant == 1){
	      Transactions_restant <- Transactions_restant[2:nrow(Transactions_restant),]
	    }
	    else {
	      Transactions_restant <- rbind(Transactions_restant[(1:(Pointeur_restant-1)),],Transactions_restant[-(1:(Pointeur_restant)),])
	      Pointeur_restant <- Pointeur_restant - 1
	    }
	  }
	  Pointeur <- Pointeur +1
	  Pointeur_restant <- Pointeur_restant +1
	}


	Transactions_decompose <- Transactions_decompose[Transactions_decompose$Autoroute > 0,]






	#DECOMPOSER LES OD PAR TRONCONS :
	Transactions_par_troncons <- data.frame(ID = "", ID_Troncon= 0, Autoroute= "", Entr= 0, Sor=0, KMS=0, Date = 0, TimeEntr = 0, TimeSor =0, DOW=0, WOY=0, Sens = 0)
	Transactions_par_troncons$Autoroute <- as.character(Transactions_par_troncons$Autoroute)
	Transactions_par_troncons$ID <- as.character(Transactions_par_troncons$ID)
	Transactions_decompose$Date <- as.character(Transactions_decompose$Date)
	Transactions_par_troncons$Date <- as.character(Transactions_par_troncons$Date)

	for (i in 1:nrow(Transactions_decompose)){
	  if ( Transactions_decompose$Autoroute[i] == "A7"){
	    entree <- match(Transactions_decompose$Entr[i],A7_par_pk$Cde)
	    sortie <- match(Transactions_decompose$Sor[i],A7_par_pk$Cde)
	    if ( entree < sortie ) { # SENS 1
	      for ( j in entree : (sortie-1) ){
	        Transactions_par_troncons <- rbind(Transactions_par_troncons,c(Transactions_decompose$ID[i],Troncons_A7$ID_Troncon[j],"A7",A7_par_pk$Cde[j],A7_par_pk$Cde[j+1],Transactions_decompose$KMS[i],Transactions_decompose$Date[i],Transactions_decompose$TimeEntr[i],Transactions_decompose$TimeSor[i],Transactions_decompose$DOW[i],Transactions_decompose$WOY[i],1))
	      }
	    }
	    else{ #SENS 2
	      for ( j in entree : (sortie+1) ){
	        Transactions_par_troncons <- rbind(Transactions_par_troncons,c(Transactions_decompose$ID[i],Troncons_A7$ID_Troncon[j-1],"A7",A7_par_pk$Cde[j],A7_par_pk$Cde[j-1],Transactions_decompose$KMS[i],Transactions_decompose$Date[i],Transactions_decompose$TimeEntr[i],Transactions_decompose$TimeSor[i],Transactions_decompose$DOW[i],Transactions_decompose$WOY[i],2))
	      }
	    }
	  }
	  else if ( Transactions_decompose$Autoroute[i] == "A8"){
	    entree <- match(Transactions_decompose$Entr[i],A8_par_pk$Cde)
	    sortie <- match(Transactions_decompose$Sor[i],A8_par_pk$Cde)
	    if ( entree < sortie ) { # SENS 1
	      for ( j in entree : (sortie-1) ){
	        Transactions_par_troncons <- rbind(Transactions_par_troncons,c(Transactions_decompose$ID[i],Troncons_A8$ID_Troncon[j],"A8",A8_par_pk$Cde[j],A8_par_pk$Cde[j+1],Transactions_decompose$KMS[i],Transactions_decompose$Date[i],Transactions_decompose$TimeEntr[i],Transactions_decompose$TimeSor[i],Transactions_decompose$DOW[i],Transactions_decompose$WOY[i],1))
	      }
	    }
	    else{ #SENS 2
	      for ( j in entree : (sortie+1) ){
	        Transactions_par_troncons <- rbind(Transactions_par_troncons,c(Transactions_decompose$ID[i],Troncons_A8$ID_Troncon[j-1],"A8",A8_par_pk$Cde[j],A8_par_pk$Cde[j-1],Transactions_decompose$KMS[i],Transactions_decompose$Date[i],Transactions_decompose$TimeEntr[i],Transactions_decompose$TimeSor[i],Transactions_decompose$DOW[i],Transactions_decompose$WOY[i],2))
	      }
	    }
	  }
	  else if ( Transactions_decompose$Autoroute[i] == "A9"){
	    entree <- match(Transactions_decompose$Entr[i],A9_par_pk$Cde)
	    sortie <- match(Transactions_decompose$Sor[i],A9_par_pk$Cde)
	    if ( entree < sortie ) { # SENS 1
	      for ( j in entree : (sortie-1) ){
	        Transactions_par_troncons <- rbind(Transactions_par_troncons,c(Transactions_decompose$ID[i],Troncons_A9$ID_Troncon[j],"A9",A9_par_pk$Cde[j],A9_par_pk$Cde[j+1],Transactions_decompose$KMS[i],Transactions_decompose$Date[i],Transactions_decompose$TimeEntr[i],Transactions_decompose$TimeSor[i],Transactions_decompose$DOW[i],Transactions_decompose$WOY[i],1))
	      }
	    }
	    else{ #SENS 2
	      for ( j in entree : (sortie+1) ){
	        Transactions_par_troncons <- rbind(Transactions_par_troncons,c(Transactions_decompose$ID[i],Troncons_A9$ID_Troncon[j-1],"A9",A9_par_pk$Cde[j],A9_par_pk$Cde[j-1],Transactions_decompose$KMS[i],Transactions_decompose$Date[i],Transactions_decompose$TimeEntr[i],Transactions_decompose$TimeSor[i],Transactions_decompose$DOW[i],Transactions_decompose$WOY[i],2))
	      }
	    }
	  }
	}
	Transactions_par_troncons <- Transactions_par_troncons[-1,]


	#Rajouter demi trajet LANCON LA BARQUE
	Pointeur <- 1
	for (i in 1:nrow(Transactions_par_troncons)){
	  if (Transactions_par_troncons$ID_Troncon[Pointeur] == 20 & Transactions_par_troncons$Sens[Pointeur] == 1){ # (A7 -> Lancon)
	    newrow1 <- c(Transactions_par_troncons$ID[Pointeur],21,"A7",25004220,25004278,Transactions_par_troncons$KMS[Pointeur],Transactions_par_troncons$Date[Pointeur],Transactions_par_troncons$TimeEntr[Pointeur],Transactions_par_troncons$TimeSor[Pointeur],Transactions_par_troncons$DOW[Pointeur],Transactions_par_troncons$WOY[Pointeur],1)
	    newrow2 <- c(Transactions_par_troncons$ID[Pointeur],22,"A7",25004278,25004279,Transactions_par_troncons$KMS[Pointeur],Transactions_par_troncons$Date[Pointeur],Transactions_par_troncons$TimeEntr[Pointeur],Transactions_par_troncons$TimeSor[Pointeur],Transactions_par_troncons$DOW[Pointeur],Transactions_par_troncons$WOY[Pointeur],1)
	    Transactions_par_troncons <- rbind(Transactions_par_troncons[(1:Pointeur),],newrow1,newrow2,Transactions_par_troncons[-(1:Pointeur),])
	    Pointeur <- Pointeur + 2
	  } 
	  if (Transactions_par_troncons$ID_Troncon[Pointeur] == 20 & Transactions_par_troncons$Sens[Pointeur] == 2){ # (Lancon -> A7)
	    newrow1 <- c(Transactions_par_troncons$ID[Pointeur],21,"A7",25004220,25004278,Transactions_par_troncons$KMS[Pointeur],Transactions_par_troncons$Date[Pointeur],Transactions_par_troncons$TimeEntr[Pointeur],Transactions_par_troncons$TimeSor[Pointeur],Transactions_par_troncons$DOW[Pointeur],Transactions_par_troncons$WOY[Pointeur],2)
	    newrow2 <- c(Transactions_par_troncons$ID[Pointeur],22,"A7",25004278,25004279,Transactions_par_troncons$KMS[Pointeur],Transactions_par_troncons$Date[Pointeur],Transactions_par_troncons$TimeEntr[Pointeur],Transactions_par_troncons$TimeSor[Pointeur],Transactions_par_troncons$DOW[Pointeur],Transactions_par_troncons$WOY[Pointeur],2)
	    Transactions_par_troncons <- rbind(Transactions_par_troncons[(1:Pointeur),],newrow1,newrow2,Transactions_par_troncons[-(1:Pointeur),])
	    Pointeur <- Pointeur + 2
	  }
	  if (Transactions_par_troncons$ID_Troncon[Pointeur] == 26 & Transactions_par_troncons$Sens[Pointeur] == 1){ # (La Barque -> A8)
	    newrow1 <- c(Transactions_par_troncons$ID[Pointeur],23,"A8",25004279,25006001,Transactions_par_troncons$KMS[Pointeur],Transactions_par_troncons$Date[Pointeur],Transactions_par_troncons$TimeEntr[Pointeur],Transactions_par_troncons$TimeSor[Pointeur],Transactions_par_troncons$DOW[Pointeur],Transactions_par_troncons$WOY[Pointeur],1)
	    newrow2 <- c(Transactions_par_troncons$ID[Pointeur],24,"A8",25006001,25006080,Transactions_par_troncons$KMS[Pointeur],Transactions_par_troncons$Date[Pointeur],Transactions_par_troncons$TimeEntr[Pointeur],Transactions_par_troncons$TimeSor[Pointeur],Transactions_par_troncons$DOW[Pointeur],Transactions_par_troncons$WOY[Pointeur],1)
	    newrow3 <- c(Transactions_par_troncons$ID[Pointeur],25,"A8",25006080,25006002,Transactions_par_troncons$KMS[Pointeur],Transactions_par_troncons$Date[Pointeur],Transactions_par_troncons$TimeEntr[Pointeur],Transactions_par_troncons$TimeSor[Pointeur],Transactions_par_troncons$DOW[Pointeur],Transactions_par_troncons$WOY[Pointeur],1)
	    Transactions_par_troncons <- rbind(Transactions_par_troncons[(1:Pointeur),],newrow1,newrow2,newrow3,Transactions_par_troncons[-(1:Pointeur),])
	    Pointeur <- Pointeur + 3
	  }
	  if (Transactions_par_troncons$ID_Troncon[Pointeur] == 26 & Transactions_par_troncons$Sens[Pointeur] == 2){ # (A8 -> La Barque)
	    newrow1 <- c(Transactions_par_troncons$ID[Pointeur],23,"A8",25004279,25006001,Transactions_par_troncons$KMS[Pointeur],Transactions_par_troncons$Date[Pointeur],Transactions_par_troncons$TimeEntr[Pointeur],Transactions_par_troncons$TimeSor[Pointeur],Transactions_par_troncons$DOW[Pointeur],Transactions_par_troncons$WOY[Pointeur],2)
	    newrow2 <- c(Transactions_par_troncons$ID[Pointeur],24,"A8",25006001,25006080,Transactions_par_troncons$KMS[Pointeur],Transactions_par_troncons$Date[Pointeur],Transactions_par_troncons$TimeEntr[Pointeur],Transactions_par_troncons$TimeSor[Pointeur],Transactions_par_troncons$DOW[Pointeur],Transactions_par_troncons$WOY[Pointeur],2)
	    newrow3 <- c(Transactions_par_troncons$ID[Pointeur],25,"A8",25006080,25006002,Transactions_par_troncons$KMS[Pointeur],Transactions_par_troncons$Date[Pointeur],Transactions_par_troncons$TimeEntr[Pointeur],Transactions_par_troncons$TimeSor[Pointeur],Transactions_par_troncons$DOW[Pointeur],Transactions_par_troncons$WOY[Pointeur],2)
	    Transactions_par_troncons <- rbind(Transactions_par_troncons[(1:Pointeur),],newrow1,newrow2,newrow3,Transactions_par_troncons[-(1:Pointeur),])
	    Pointeur <- Pointeur + 3
	  }
	  Pointeur <- Pointeur + 1
	}

	return ( Transactions_par_troncons)
	}
