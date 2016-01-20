##########
##########
# 20150723
# OD -> Troncon
##########
##########
# load("Troncons_A789.RData")
# load("A7_par_pk.RData")
# load("A8_par_pk.RData")
# load("A9_par_pk.RData")
# load("Troncons_A7.RData")
# load("Troncons_A8.RData")
# load("Troncons_A9.RData")
# gares <- read.table("garesLatLng.csv", header = T, sep = ",")

decompose <- function(transaction){
  #DECOMPOSER LES TRAJETS PAR AUTOROUTE UNIQUE
  #Et traiter le système ouvert
  gares$Autoroute <- as.character(gares$Autoroute)
  transaction$Entr <- as.numeric(as.character(transaction$Entr))
  transaction$Sor <- as.numeric(as.character(transaction$Sor))
  Autoroute <- vector(mode = "character", length = nrow(transaction))
  transaction_decompose <- cbind(transaction, Autoroute)
  transaction_decompose$Autoroute <- as.character(transaction_decompose$Autoroute)
  transaction_restant <- transaction
  Pointeur <- 1
  Pointeur_restant <- 1
  Gare_inconnue <- 0 #Gares du système ouvert que le code ne connait pas
  for (i in 1 : nrow(transaction)){
    if (  ( (transaction$Entr[i] %in% Troncons_A789[,4]) | (transaction$Entr[i] %in% Troncons_A789[,6])  ) & 
          ( (transaction$Sor[i] %in% Troncons_A789[,4]) | (transaction$Sor[i] %in% Troncons_A789[,6])  )  ) {  # SI TRAJET DANS LES TROIS AUTOROUTES
      if (gares$Autoroute[match(transaction$Entr[i],gares$Cde)] !=  gares$Autoroute[match(transaction$Sor[i],gares$Cde)] ) { #E=A7etS=A9 ou l'inverse
        if (transaction$Entr[i] != 25004210 & transaction$Sor[i] != 25004210){  # Entrée et sortie <> Orange Centre
          if(gares$Autoroute[match(transaction$Entr[i],gares$Cde)] == "A7"){# E=A7 S=A9
            newrow1 <- c(transaction[i,1],transaction$Entr[i],25004210,transaction$Date[i],transaction$DOW[i],transaction$WOY[i],transaction$TimeEntr[i],transaction$TimeSor[i],"A7")
            newrow2 <- c(transaction[i,1],25004210,transaction$Sor[i], transaction$Date[i],transaction$DOW[i],transaction$WOY[i],transaction$TimeEntr[i],transaction$TimeSor[i],"A9")
            transaction_decompose = rbind(transaction_decompose[1:Pointeur,],newrow1,newrow2,transaction_decompose[-(1:Pointeur),])
            Pointeur <- Pointeur +2
          }
          else if(gares$Autoroute[match(transaction$Entr[i],gares$Cde)] == "A9"){# E=A9 S=A7
            newrow1 <- c(transaction[i,1],transaction$Entr[i],25004210,transaction$Date[i],transaction$DOW[i],transaction$WOY[i],transaction$TimeEntr[i],transaction$TimeSor[i],"A9")
            newrow2 <- c(transaction[i,1],25004210,transaction$Sor[i], transaction$Date[i],transaction$DOW[i],transaction$WOY[i],transaction$TimeEntr[i],transaction$TimeSor[i],"A7")
            transaction_decompose = rbind(transaction_decompose[1:Pointeur,],newrow1,newrow2,transaction_decompose[-(1:Pointeur),])
            Pointeur <- Pointeur +2
          }
        }
        else { # E=A9 S=OC ou l'inverse
          transaction_decompose$Autoroute[Pointeur] <- "A9"
        }
      }
      else {
        transaction_decompose$Autoroute[Pointeur] <- gares$Autoroute[match(transaction$Entr[i],gares$Cde)]
      }
      if (Pointeur_restant == 1){
        transaction_restant <- transaction_restant[-1:1,]
      }
      else {
        transaction_restant <- rbind(transaction_restant[(1:(Pointeur_restant-1)),],transaction_restant[-(1:(Pointeur_restant)),])
        Pointeur_restant <- Pointeur_restant - 1
      }
    }
    
    ## deactivate: no information about Voie
    else if (transaction$Entr[i] < 0 & ( (transaction$Sor[i] %in% Troncons_A789[,4])|(transaction$Sor[i] %in% Troncons_A789[,6]) ) ){ #Si Entr = 0 et Sor dans A789
      if (transaction$Sor[i] == 25006001){ #Canet de mereuil
        if (transaction$Voie[i] >= 20 ){#Sortie
          transaction_decompose[Pointeur,] <- c(transaction$ID[i],25004278 ,25006001,transaction$Date[i],transaction$DOW[i],transaction$WOY[i],transaction$TimeEntr[i],transaction$TimeSor[i],"A8")
        } #On crée un trajet Coudoux -> Canet de méreuil
        else { 
          transaction_decompose[Pointeur,] <- c(transaction$ID[i], 25006001 ,25004278,transaction$Date[i],transaction$DOW[i],transaction$WOY[i],transaction$TimeEntr[i],transaction$TimeSor[i],"A8")
        }  #Trajet Canet -> Coudoux
      }
      else if (transaction$Sor[i] == 25006010){ #Fréjus
        if (transaction$Voie[i] >= 20 ){#Sortie
          transaction_decompose[Pointeur,] <- c(transaction$ID[i],25006022 ,25006010,transaction$Date[i],transaction$DOW[i],transaction$WOY[i],transaction$TimeEntr[i],transaction$TimeSor[i],"A8")
        } #On crée un trajet Antibes PV Nord-> Fréjus
        else { 
          transaction_decompose[Pointeur,] <- c(transaction$ID[i], 25006010 ,25006012,transaction$Date[i],transaction$DOW[i],transaction$WOY[i],transaction$TimeEntr[i],transaction$TimeSor[i],"A8")
        }  #Trajet Fréjus -> Antibes PV 
      }
      else if (transaction$Sor[i] == 25006011){ #Les Adrets
        if (transaction$Voie[i] >= 20 ){#Sortie
          transaction_decompose[Pointeur,] <- c(transaction$ID[i],25006022 ,25006011,transaction$Date[i],transaction$DOW[i],transaction$WOY[i],transaction$TimeEntr[i],transaction$TimeSor[i],"A8")
        } #On crée un trajet Antibes PV Nord-> Les Adrets
        else { 
          transaction_decompose[Pointeur,] <- c(transaction$ID[i], 25006011 ,25006012,transaction$Date[i],transaction$DOW[i],transaction$WOY[i],transaction$TimeEntr[i],transaction$TimeSor[i],"A8")
        }  #Trajet Les Adrets -> Antibes PV 
      }
      else if (transaction$Sor[i] == 25006014){ #Antibes Ouest
        if (transaction$Voie[i] >= 20 ){#Sortie
          transaction_decompose[Pointeur,] <- c(transaction$ID[i],25006009 ,25006014,transaction$Date[i],transaction$DOW[i],transaction$WOY[i],transaction$TimeEntr[i],transaction$TimeSor[i],"A8")
        } #On crée un trajet Capitou -> Antibes Ouest
        else { 
          transaction_decompose[Pointeur,] <- c(transaction$ID[i], 25006014 ,25006009,transaction$Date[i],transaction$DOW[i],transaction$WOY[i],transaction$TimeEntr[i],transaction$TimeSor[i],"A8")
        }  #Trajet Antibes Ouest -> Capitou 
      }
      else if (transaction$Sor[i] == 25006012){ #Antibes PV
        if (transaction$Voie[i] >= 20 ){#PV Sud, donc de Cannes vers Nice
          transaction_decompose[Pointeur,] <- c(transaction$ID[i],25006009 ,25006015,transaction$Date[i],transaction$DOW[i],transaction$WOY[i],transaction$TimeEntr[i],transaction$TimeSor[i],"A8")
        } #On crée un trajet Capitou -> Cagnes Ouest Nord
        else { #PV Nord, de Nice vers Cannes
          transaction_decompose[Pointeur,] <- c(transaction$ID[i], 25006015 ,25006009,transaction$Date[i],transaction$DOW[i],transaction$WOY[i],transaction$TimeEntr[i],transaction$TimeSor[i],"A8")
        }  #Trajet Cagnes Ouest Nord -> Capitou 
      }
      else if (transaction$Sor[i] == 25006024){ #Sophia
        transaction_decompose[Pointeur,] <- c(transaction$ID[i],25006024 ,25006020,transaction$Date[i],transaction$DOW[i],transaction$WOY[i],transaction$TimeEntr[i],transaction$TimeSor[i],"A8")
        #Entrée seule, don trajet Sophia -> Saint Isidore
      }
      else if (transaction$Sor[i] == 25006013){ #Antibes Est
        if (transaction$Voie[i] >= 20 ){#Sortie
          transaction_decompose[Pointeur,] <- c(transaction$ID[i],25006020 ,25006013,transaction$Date[i],transaction$DOW[i],transaction$WOY[i],transaction$TimeEntr[i],transaction$TimeSor[i],"A8")
        } #On crée un trajet Saint Isidore -> Antibes Est
        else { 
          transaction_decompose[Pointeur,] <- c(transaction$ID[i], 25006013 ,25006020,transaction$Date[i],transaction$DOW[i],transaction$WOY[i],transaction$TimeEntr[i],transaction$TimeSor[i],"A8")
        }  #Trajet Antibes Est -> Saint Isidore
      }
      else if (transaction$Sor[i] == 25006017){ #Cagnes Ouest Sud
        transaction_decompose[Pointeur,] <- c(transaction$ID[i],25006017 ,25006020,transaction$Date[i],transaction$DOW[i],transaction$WOY[i],transaction$TimeEntr[i],transaction$TimeSor[i],"A8")
        #On crée un trajet  Cagnes Ouest Sud -> Saint Isidore
      }
      else if (transaction$Sor[i] == 25006015){ #Cagnes Ouest Nord
        transaction_decompose[Pointeur,] <- c(transaction$ID[i],25006020 ,25006015,transaction$Date[i],transaction$DOW[i],transaction$WOY[i],transaction$TimeEntr[i],transaction$TimeSor[i],"A8")
        #On crée un trajet  Saint Isidore -> Cagnes Ouest Nord
      }
      else if (transaction$Sor[i] == 25006016){ #Cagnes Est
        if (transaction$Voie[i] >= 20 ){#Sortie
          transaction_decompose[Pointeur,] <- c(transaction$ID[i],25006020 ,25006016,transaction$Date[i],transaction$DOW[i],transaction$WOY[i],transaction$TimeEntr[i],transaction$TimeSor[i],"A8")
        } #On crée un trajet Saint Isidore -> Cagnes Est
        else { 
          transaction_decompose[Pointeur,] <- c(transaction$ID[i], 25006016 ,25006020,transaction$Date[i],transaction$DOW[i],transaction$WOY[i],transaction$TimeEntr[i],transaction$TimeSor[i],"A8")
        }  #TrajetCagnes Est -> Saint Isidore
      }
      else if (transaction$Sor[i] == 25006021){ #Saint Isidore Ech Est
        if (transaction$Voie[i] >= 20 ){#Sortie
          transaction_decompose[Pointeur,] <- c(transaction$ID[i],25006027 ,25006021,transaction$Date[i],transaction$DOW[i],transaction$WOY[i],transaction$TimeEntr[i],transaction$TimeSor[i],"A8")
        } #On crée un trajet La Turbie -> Saint Isidore Ech Est
        else { 
          transaction_decompose[Pointeur,] <- c(transaction$ID[i], 25006021 ,25006027,transaction$Date[i],transaction$DOW[i],transaction$WOY[i],transaction$TimeEntr[i],transaction$TimeSor[i],"A8")
        }  #Trajet Saint Isidore Ech Est -> La Turbie
      }
      else if (transaction$Sor[i] == 25006021){ #Saint Isidore Ech Est
        if (transaction$Voie[i] >= 20 ){#Sortie
          transaction_decompose[Pointeur,] <- c(transaction$ID[i],25006027 ,25006021,transaction$Date[i],transaction$DOW[i],transaction$WOY[i],transaction$TimeEntr[i],transaction$TimeSor[i],"A8")
        } #On crée un trajet La Turbie -> Saint Isidore Ech Est
        else { 
          transaction_decompose[Pointeur,] <- c(transaction$ID[i], 25006021 ,25006027,transaction$Date[i],transaction$DOW[i],transaction$WOY[i],transaction$TimeEntr[i],transaction$TimeSor[i],"A8")
        }  #Trajet Saint Isidore Ech Est -> La Turbie
      }
      else if (transaction$Sor[i] == 25006019){ #Saint Isidore Ech Ouest
        if (transaction$Voie[i] >= 20 ){#Sortie
          transaction_decompose[Pointeur,] <- c(transaction$ID[i],25006012 ,25006019,transaction$Date[i],transaction$DOW[i],transaction$WOY[i],transaction$TimeEntr[i],transaction$TimeSor[i],"A8")
        } #On crée un trajet Antibes PV -> Saint Isidore Ech Ouest
        else { 
          transaction_decompose[Pointeur,] <- c(transaction$ID[i], 25006019 ,25006012,transaction$Date[i],transaction$DOW[i],transaction$WOY[i],transaction$TimeEntr[i],transaction$TimeSor[i],"A8")
        }  #Trajet Saint Isidore Ech Ouest -> Antibes PV
      }
      else if (transaction$Sor[i] == 25006020){ #Saint Isidore PV
        if (transaction$Voie[i] >= 20 ){#Nord, donc de l'Italie vers Cannes
          transaction_decompose[Pointeur,] <- c(transaction$ID[i],25006026 ,25006020,transaction$Date[i],transaction$DOW[i],transaction$WOY[i],transaction$TimeEntr[i],transaction$TimeSor[i],"A8")
        } #On crée un trajet La Turbie Ech -> Cagnes Ouest Nord
        else { #Sud, de Cannes vers l'Italie
          transaction_decompose[Pointeur,] <- c(transaction$ID[i], 25006020 ,25006026,transaction$Date[i],transaction$DOW[i],transaction$WOY[i],transaction$TimeEntr[i],transaction$TimeSor[i],"A8")
        }  #Trajet Cagnes Ouest Nord -> La Turbie Ech
      }
      else if (transaction$Sor[i] == 25006026){ #La Turbie Ech
        if (transaction$Voie[i] >= 20 ){#Sortie
          transaction_decompose[Pointeur,] <- c(transaction$ID[i],25006020 ,25006026,transaction$Date[i],transaction$DOW[i],transaction$WOY[i],transaction$TimeEntr[i],transaction$TimeSor[i],"A8")
        } #On crée un trajet Saint isidore PV -> La Turbie Ech
        else { 
          transaction_decompose[Pointeur,] <- c(transaction$ID[i], 25006026 ,25006020,transaction$Date[i],transaction$DOW[i],transaction$WOY[i],transaction$TimeEntr[i],transaction$TimeSor[i],"A8")
        }  #Trajet La Turbie Ech -> Saint isidore PV
      }
      else if (transaction$Sor[i] == 25006027){ #La Turbie PV
        if (transaction$Voie[i] >= 20 ){# Nord, donc de l'Italie vers Cannes
          transaction_decompose[Pointeur,] <- c(transaction$ID[i],25006027 ,25006026,transaction$Date[i],transaction$DOW[i],transaction$WOY[i],transaction$TimeEntr[i],transaction$TimeSor[i],"A8")
        } #On crée un trajet La Turbie PV -> La Turbie Ech
        else { #Sud, de Cannes vers l'Italie
          transaction_decompose[Pointeur,] <- c(transaction$ID[i], 25006026 ,25006027,transaction$Date[i],transaction$DOW[i],transaction$WOY[i],transaction$TimeEntr[i],transaction$TimeSor[i],"A8")
        }  #Trajet La Turbie Ech -> La Turbie PV
      }
      else { Gare_inconnue <- Gare_inconnue +1
      }
    }
    Pointeur <- Pointeur +1
    Pointeur_restant <- Pointeur_restant +1
  }
  
  transaction_decompose <- transaction_decompose[transaction_decompose$Autoroute > 0,]
  
  
  
  
  
  #DECOMPOSER LES OD PAR TRONCONS :
  transaction_par_troncons <- data.frame(ID = "", ID_Troncon= 0, Autoroute= "", Entr= 0, Sor=0,Date = 0, DOW=0, WOY=0, TimeEntr = 0, TimeSor =0, Sens = 0)
  transaction_par_troncons$Autoroute <- as.character(transaction_par_troncons$Autoroute)
  #transaction_decompose$Year <- as.character(transaction_decompose$Year)
  transaction_par_troncons$ID <- as.character(transaction_par_troncons$ID)
  
  for (i in 1:nrow(transaction_decompose)){
    if ( transaction_decompose$Autoroute[i] == "A7"){
      entree <- match(transaction_decompose$Entr[i],A7_par_pk$Cde)
      sortie <- match(transaction_decompose$Sor[i],A7_par_pk$Cde)
      if ( entree < sortie ) { # SENS 1
        for ( j in entree : (sortie-1) ){
          transaction_par_troncons <- rbind(transaction_par_troncons,c(transaction_decompose$ID[i],Troncons_A7$ID_Troncon[j],"A7",A7_par_pk$Cde[j],A7_par_pk$Cde[j+1],transaction_decompose$Date[i],transaction_decompose$DOW[i],transaction_decompose$WOY[i],transaction_decompose$TimeEntr[i],transaction_decompose$TimeSor[i],1))
        }
      }
      else{ #SENS 2
        for ( j in entree : (sortie+1) ){
          transaction_par_troncons <- rbind(transaction_par_troncons,c(transaction_decompose$ID[i],Troncons_A7$ID_Troncon[j-1],"A7",A7_par_pk$Cde[j],A7_par_pk$Cde[j-1],transaction_decompose$Date[i],transaction_decompose$DOW[i],transaction_decompose$WOY[i],transaction_decompose$TimeEntr[i],transaction_decompose$TimeSor[i],2))
        }
      }
    }
    else if ( transaction_decompose$Autoroute[i] == "A8"){
      entree <- match(transaction_decompose$Entr[i],A8_par_pk$Cde)
      sortie <- match(transaction_decompose$Sor[i],A8_par_pk$Cde)
      if ( entree < sortie ) { # SENS 1
        for ( j in entree : (sortie-1) ){
          transaction_par_troncons <- rbind(transaction_par_troncons,c(transaction_decompose$ID[i],Troncons_A8$ID_Troncon[j],"A8",A8_par_pk$Cde[j],A8_par_pk$Cde[j+1],transaction_decompose$Date[i],transaction_decompose$DOW[i],transaction_decompose$WOY[i],transaction_decompose$TimeEntr[i],transaction_decompose$TimeSor[i],1))
        }
      }
      else{ #SENS 2
        for ( j in entree : (sortie+1) ){
          transaction_par_troncons <- rbind(transaction_par_troncons,c(transaction_decompose$ID[i],Troncons_A8$ID_Troncon[j-1],"A8",A8_par_pk$Cde[j],A8_par_pk$Cde[j-1],transaction_decompose$Date[i],transaction_decompose$DOW[i],transaction_decompose$WOY[i],transaction_decompose$TimeEntr[i],transaction_decompose$TimeSor[i],2))
        }
      }
    }
    else if ( transaction_decompose$Autoroute[i] == "A9"){
      entree <- match(transaction_decompose$Entr[i],A9_par_pk$Cde)
      sortie <- match(transaction_decompose$Sor[i],A9_par_pk$Cde)
      if ( entree < sortie ) { # SENS 1
        for ( j in entree : (sortie-1) ){
          transaction_par_troncons <- rbind(transaction_par_troncons,c(transaction_decompose$ID[i],Troncons_A9$ID_Troncon[j],"A9",A9_par_pk$Cde[j],A9_par_pk$Cde[j+1],transaction_decompose$Date[i],transaction_decompose$DOW[i],transaction_decompose$WOY[i],transaction_decompose$TimeEntr[i],transaction_decompose$TimeSor[i],1))
        }
      }
      else{ #SENS 2
        for ( j in entree : (sortie+1) ){
          transaction_par_troncons <- rbind(transaction_par_troncons,c(transaction_decompose$ID[i],Troncons_A9$ID_Troncon[j-1],"A9",A9_par_pk$Cde[j],A9_par_pk$Cde[j-1],transaction_decompose$Date[i],transaction_decompose$DOW[i],transaction_decompose$WOY[i],transaction_decompose$TimeEntr[i],transaction_decompose$TimeSor[i],2))
        }
      }
    }
  }
  transaction_par_troncons <- transaction_par_troncons[-1,]
  transaction_par_troncons$Date <- as.Date(as.numeric(transaction_par_troncons$Date), origin = as.Date("1970-1-1"))
  transaction_par_troncons$TimeEntr <- as.numeric(transaction_par_troncons$TimeEntr)
  transaction_par_troncons$TimeSor <- as.numeric(transaction_par_troncons$TimeSor)
  
  return(transaction_par_troncons)
}