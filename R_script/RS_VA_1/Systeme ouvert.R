#Découypage des systèmes ouverts : Entrée 0 remplacée par celle du troncon précédent

#DECOMPOSER LES TRAJETS PAR AUTOROUTE UNIQUE
#Et traiter le système ouvert

temp$Autoroute <- as.character(temp$Autoroute)
Escotis3$Entr <- as.numeric(as.character(Escotis3$Entr))
Escotis3$Sor <- as.numeric(as.character(Escotis3$Sor))
Autoroute <- vector(mode = "character", length = nrow(Escotis3))
Escotis3_decompose <- cbind(Escotis3, Autoroute)
Escotis3_decompose$Autoroute <- as.character(Escotis3_decompose$Autoroute)
Escotis3_restant <- Escotis3
Pointeur <- 1
Pointeur_restant <- 1
Gare_inconnue <- 0 #Gares du système ouvert que le code ne connait pas
for (i in 1 : nrow(Escotis3)){
  if (  ( (Escotis3$Entr[i] %in% Troncons_A789[,4]) | (Escotis3$Entr[i] %in% Troncons_A789[,6])  ) & 
        ( (Escotis3$Sor[i] %in% Troncons_A789[,4]) | (Escotis3$Sor[i] %in% Troncons_A789[,6])  )  ) {  # SI TRAJET DANS LES TROIS AUTOROUTES
    if (temp$Autoroute[match(Escotis3$Entr[i],temp$Cde)] !=  temp$Autoroute[match(Escotis3$Sor[i],temp$Cde)] ) { #E=A7etS=A9 ou l'inverse
      if (Escotis3$Entr[i] != 25004210 & Escotis3$Sor[i] != 25004210){  # Entrée et sortie <> Orange Centre
        if(temp$Autoroute[match(Escotis3$Entr[i],temp$Cde)] == "A7"){# E=A7 S=A9
          newrow1 <- c(Escotis3[i,1],Escotis3$Entr[i],25004210,Escotis3$KMS[i],Escotis3$Year[i],Escotis3$Month[i],Escotis3$Day[i],Escotis3$Hour[i],Escotis3$Minute[i],Escotis3$TimeEntr[i],Escotis3$TimeSor[i],Escotis3$DOW[i],Escotis3$WOY[i],Escotis3$Voie[i],"A7")
          newrow2 <- c(Escotis3[i,1],25004210,Escotis3$Sor[i],Escotis3$KMS[i],Escotis3$Year[i],Escotis3$Month[i],Escotis3$Day[i],Escotis3$Hour[i],Escotis3$Minute[i],Escotis3$TimeEntr[i],Escotis3$TimeSor[i],Escotis3$DOW[i],Escotis3$WOY[i],Escotis3$Voie[i],"A9")
          Escotis3_decompose = rbind(Escotis3_decompose[1:Pointeur,],newrow1,newrow2,Escotis3_decompose[-(1:Pointeur),])
          Pointeur <- Pointeur +2
        }
        else if(temp$Autoroute[match(Escotis3$Entr[i],temp$Cde)] == "A9"){# E=A9 S=A7
          newrow1 <- c(Escotis3[i,1],Escotis3$Entr[i],25004210,Escotis3$KMS[i],Escotis3$Year[i],Escotis3$Month[i],Escotis3$Day[i],Escotis3$Hour[i],Escotis3$Minute[i],Escotis3$TimeEntr[i],Escotis3$TimeSor[i],Escotis3$DOW[i],Escotis3$WOY[i],Escotis3$Voie[i],"A9")
          newrow2 <- c(Escotis3[i,1],25004210,Escotis3$Sor[i],Escotis3$KMS[i],Escotis3$Year[i],Escotis3$Month[i],Escotis3$Day[i],Escotis3$Hour[i],Escotis3$Minute[i],Escotis3$TimeEntr[i],Escotis3$TimeSor[i],Escotis3$DOW[i],Escotis3$WOY[i],Escotis3$Voie[i],"A7")
          Escotis3_decompose = rbind(Escotis3_decompose[1:Pointeur,],newrow1,newrow2,Escotis3_decompose[-(1:Pointeur),])
          Pointeur <- Pointeur +2
        }
      }
      else { # E=A9 S=OC ou l'inverse
        Escotis3_decompose$Autoroute[Pointeur] <- "A9"
      }
    }
    else {
      Escotis3_decompose$Autoroute[Pointeur] <- temp$Autoroute[match(Escotis3$Entr[i],temp$Cde)]
    }
    if (Pointeur_restant == 1){
      Escotis3_restant <- Escotis3_restant[-1:1,]
    }
    else {
      Escotis3_restant <- rbind(Escotis3_restant[(1:(Pointeur_restant-1)),],Escotis3_restant[-(1:(Pointeur_restant)),])
      Pointeur_restant <- Pointeur_restant - 1
    }
  }
  else if (Escotis3$Entr[i] == 0 & ( (Escotis3$Sor[i] %in% Troncons_A789[,4])|(Escotis3$Sor[i] %in% Troncons_A789[,6]) ) ){ #Si Entr = 0 et Sor dans A789
    if (Escotis3$Sor[i] == 25006001){ #Canet de mereuil
      if (Escotis3$Voie[i] >= 20 ){#Sortie
        Escotis3_decompose[Pointeur,] <- c(Escotis3$ID[i],25004278 ,25006001,Escotis3$KMS[i],Escotis3$Year[i],Escotis3$Month[i],Escotis3$Day[i],Escotis3$Hour[i],Escotis3$Minute[i],Escotis3$TimeEntr[i],Escotis3$TimeSor[i],Escotis3$DOW[i],Escotis3$WOY[i],Escotis3$Voie[i],"A8")
            } #On crée un trajet Coudoux -> Canet de méreuil
      else { 
      Escotis3_decompose[Pointeur,] <- c(Escotis3$ID[i], 25006001 ,25004278,Escotis3$KMS[i],Escotis3$Year[i],Escotis3$Month[i],Escotis3$Day[i],Escotis3$Hour[i],Escotis3$Minute[i],Escotis3$TimeEntr[i],Escotis3$TimeSor[i],Escotis3$DOW[i],Escotis3$WOY[i],Escotis3$Voie[i],"A8")
      }  #Trajet Canet -> Coudoux
    }
    else if (Escotis3$Sor[i] == 25006010){ #Fréjus
      if (Escotis3$Voie[i] >= 20 ){#Sortie
        Escotis3_decompose[Pointeur,] <- c(Escotis3$ID[i],25006022 ,25006010,Escotis3$KMS[i],Escotis3$Year[i],Escotis3$Month[i],Escotis3$Day[i],Escotis3$Hour[i],Escotis3$Minute[i],Escotis3$TimeEntr[i],Escotis3$TimeSor[i],Escotis3$DOW[i],Escotis3$WOY[i],Escotis3$Voie[i],"A8")
      } #On crée un trajet Antibes PV Nord-> Fréjus
      else { 
        Escotis3_decompose[Pointeur,] <- c(Escotis3$ID[i], 25006010 ,25006012,Escotis3$KMS[i],Escotis3$Year[i],Escotis3$Month[i],Escotis3$Day[i],Escotis3$Hour[i],Escotis3$Minute[i],Escotis3$TimeEntr[i],Escotis3$TimeSor[i],Escotis3$DOW[i],Escotis3$WOY[i],Escotis3$Voie[i],"A8")
      }  #Trajet Fréjus -> Antibes PV 
    }
    else if (Escotis3$Sor[i] == 25006011){ #Les Adrets
      if (Escotis3$Voie[i] >= 20 ){#Sortie
        Escotis3_decompose[Pointeur,] <- c(Escotis3$ID[i],25006022 ,25006011,Escotis3$KMS[i],Escotis3$Year[i],Escotis3$Month[i],Escotis3$Day[i],Escotis3$Hour[i],Escotis3$Minute[i],Escotis3$TimeEntr[i],Escotis3$TimeSor[i],Escotis3$DOW[i],Escotis3$WOY[i],Escotis3$Voie[i],"A8")
      } #On crée un trajet Antibes PV Nord-> Les Adrets
      else { 
        Escotis3_decompose[Pointeur,] <- c(Escotis3$ID[i], 25006011 ,25006012,Escotis3$KMS[i],Escotis3$Year[i],Escotis3$Month[i],Escotis3$Day[i],Escotis3$Hour[i],Escotis3$Minute[i],Escotis3$TimeEntr[i],Escotis3$TimeSor[i],Escotis3$DOW[i],Escotis3$WOY[i],Escotis3$Voie[i],"A8")
      }  #Trajet Les Adrets -> Antibes PV 
    }
    else if (Escotis3$Sor[i] == 25006014){ #Antibes Ouest
      if (Escotis3$Voie[i] >= 20 ){#Sortie
        Escotis3_decompose[Pointeur,] <- c(Escotis3$ID[i],25006009 ,25006014,Escotis3$KMS[i],Escotis3$Year[i],Escotis3$Month[i],Escotis3$Day[i],Escotis3$Hour[i],Escotis3$Minute[i],Escotis3$TimeEntr[i],Escotis3$TimeSor[i],Escotis3$DOW[i],Escotis3$WOY[i],Escotis3$Voie[i],"A8")
      } #On crée un trajet Capitou -> Antibes Ouest
      else { 
        Escotis3_decompose[Pointeur,] <- c(Escotis3$ID[i], 25006014 ,25006009,Escotis3$KMS[i],Escotis3$Year[i],Escotis3$Month[i],Escotis3$Day[i],Escotis3$Hour[i],Escotis3$Minute[i],Escotis3$TimeEntr[i],Escotis3$TimeSor[i],Escotis3$DOW[i],Escotis3$WOY[i],Escotis3$Voie[i],"A8")
      }  #Trajet Antibes Ouest -> Capitou 
    }
    else if (Escotis3$Sor[i] == 25006012){ #Antibes PV
      if (Escotis3$Voie[i] >= 20 ){#PV Sud, donc de Cannes vers Nice
        Escotis3_decompose[Pointeur,] <- c(Escotis3$ID[i],25006009 ,25006015,Escotis3$KMS[i],Escotis3$Year[i],Escotis3$Month[i],Escotis3$Day[i],Escotis3$Hour[i],Escotis3$Minute[i],Escotis3$TimeEntr[i],Escotis3$TimeSor[i],Escotis3$DOW[i],Escotis3$WOY[i],Escotis3$Voie[i],"A8")
      } #On crée un trajet Capitou -> Cagnes Ouest Nord
      else { #PV Nord, de Nice vers Cannes
        Escotis3_decompose[Pointeur,] <- c(Escotis3$ID[i], 25006015 ,25006009,Escotis3$KMS[i],Escotis3$Year[i],Escotis3$Month[i],Escotis3$Day[i],Escotis3$Hour[i],Escotis3$Minute[i],Escotis3$TimeEntr[i],Escotis3$TimeSor[i],Escotis3$DOW[i],Escotis3$WOY[i],Escotis3$Voie[i],"A8")
      }  #Trajet Cagnes Ouest Nord -> Capitou 
    }
    else if (Escotis3$Sor[i] == 25006024){ #Sophia
        Escotis3_decompose[Pointeur,] <- c(Escotis3$ID[i],25006024 ,25006020,Escotis3$KMS[i],Escotis3$Year[i],Escotis3$Month[i],Escotis3$Day[i],Escotis3$Hour[i],Escotis3$Minute[i],Escotis3$TimeEntr[i],Escotis3$TimeSor[i],Escotis3$DOW[i],Escotis3$WOY[i],Escotis3$Voie[i],"A8")
      #Entrée seule, don trajet Sophia -> Saint Isidore
    }
    else if (Escotis3$Sor[i] == 25006013){ #Antibes Est
      if (Escotis3$Voie[i] >= 20 ){#Sortie
        Escotis3_decompose[Pointeur,] <- c(Escotis3$ID[i],25006020 ,25006013,Escotis3$KMS[i],Escotis3$Year[i],Escotis3$Month[i],Escotis3$Day[i],Escotis3$Hour[i],Escotis3$Minute[i],Escotis3$TimeEntr[i],Escotis3$TimeSor[i],Escotis3$DOW[i],Escotis3$WOY[i],Escotis3$Voie[i],"A8")
      } #On crée un trajet Saint Isidore -> Antibes Est
      else { 
        Escotis3_decompose[Pointeur,] <- c(Escotis3$ID[i], 25006013 ,25006020,Escotis3$KMS[i],Escotis3$Year[i],Escotis3$Month[i],Escotis3$Day[i],Escotis3$Hour[i],Escotis3$Minute[i],Escotis3$TimeEntr[i],Escotis3$TimeSor[i],Escotis3$DOW[i],Escotis3$WOY[i],Escotis3$Voie[i],"A8")
      }  #Trajet Antibes Est -> Saint Isidore
    }
    else if (Escotis3$Sor[i] == 25006017){ #Cagnes Ouest Sud
        Escotis3_decompose[Pointeur,] <- c(Escotis3$ID[i],25006017 ,25006020,Escotis3$KMS[i],Escotis3$Year[i],Escotis3$Month[i],Escotis3$Day[i],Escotis3$Hour[i],Escotis3$Minute[i],Escotis3$TimeEntr[i],Escotis3$TimeSor[i],Escotis3$DOW[i],Escotis3$WOY[i],Escotis3$Voie[i],"A8")
       #On crée un trajet  Cagnes Ouest Sud -> Saint Isidore
    }
    else if (Escotis3$Sor[i] == 25006015){ #Cagnes Ouest Nord
      Escotis3_decompose[Pointeur,] <- c(Escotis3$ID[i],25006020 ,25006015,Escotis3$KMS[i],Escotis3$Year[i],Escotis3$Month[i],Escotis3$Day[i],Escotis3$Hour[i],Escotis3$Minute[i],Escotis3$TimeEntr[i],Escotis3$TimeSor[i],Escotis3$DOW[i],Escotis3$WOY[i],Escotis3$Voie[i],"A8")
      #On crée un trajet  Saint Isidore -> Cagnes Ouest Nord
    }
    else if (Escotis3$Sor[i] == 25006016){ #Cagnes Est
      if (Escotis3$Voie[i] >= 20 ){#Sortie
        Escotis3_decompose[Pointeur,] <- c(Escotis3$ID[i],25006020 ,25006016,Escotis3$KMS[i],Escotis3$Year[i],Escotis3$Month[i],Escotis3$Day[i],Escotis3$Hour[i],Escotis3$Minute[i],Escotis3$TimeEntr[i],Escotis3$TimeSor[i],Escotis3$DOW[i],Escotis3$WOY[i],Escotis3$Voie[i],"A8")
      } #On crée un trajet Saint Isidore -> Cagnes Est
      else { 
        Escotis3_decompose[Pointeur,] <- c(Escotis3$ID[i], 25006016 ,25006020,Escotis3$KMS[i],Escotis3$Year[i],Escotis3$Month[i],Escotis3$Day[i],Escotis3$Hour[i],Escotis3$Minute[i],Escotis3$TimeEntr[i],Escotis3$TimeSor[i],Escotis3$DOW[i],Escotis3$WOY[i],Escotis3$Voie[i],"A8")
      }  #TrajetCagnes Est -> Saint Isidore
    }
    else if (Escotis3$Sor[i] == 25006021){ #Saint Isidore Ech Est
      if (Escotis3$Voie[i] >= 20 ){#Sortie
        Escotis3_decompose[Pointeur,] <- c(Escotis3$ID[i],25006027 ,25006021,Escotis3$KMS[i],Escotis3$Year[i],Escotis3$Month[i],Escotis3$Day[i],Escotis3$Hour[i],Escotis3$Minute[i],Escotis3$TimeEntr[i],Escotis3$TimeSor[i],Escotis3$DOW[i],Escotis3$WOY[i],Escotis3$Voie[i],"A8")
      } #On crée un trajet La Turbie -> Saint Isidore Ech Est
      else { 
        Escotis3_decompose[Pointeur,] <- c(Escotis3$ID[i], 25006021 ,25006027,Escotis3$KMS[i],Escotis3$Year[i],Escotis3$Month[i],Escotis3$Day[i],Escotis3$Hour[i],Escotis3$Minute[i],Escotis3$TimeEntr[i],Escotis3$TimeSor[i],Escotis3$DOW[i],Escotis3$WOY[i],Escotis3$Voie[i],"A8")
      }  #Trajet Saint Isidore Ech Est -> La Turbie
    }
    else if (Escotis3$Sor[i] == 25006021){ #Saint Isidore Ech Est
      if (Escotis3$Voie[i] >= 20 ){#Sortie
        Escotis3_decompose[Pointeur,] <- c(Escotis3$ID[i],25006027 ,25006021,Escotis3$KMS[i],Escotis3$Year[i],Escotis3$Month[i],Escotis3$Day[i],Escotis3$Hour[i],Escotis3$Minute[i],Escotis3$TimeEntr[i],Escotis3$TimeSor[i],Escotis3$DOW[i],Escotis3$WOY[i],Escotis3$Voie[i],"A8")
      } #On crée un trajet La Turbie -> Saint Isidore Ech Est
      else { 
        Escotis3_decompose[Pointeur,] <- c(Escotis3$ID[i], 25006021 ,25006027,Escotis3$KMS[i],Escotis3$Year[i],Escotis3$Month[i],Escotis3$Day[i],Escotis3$Hour[i],Escotis3$Minute[i],Escotis3$TimeEntr[i],Escotis3$TimeSor[i],Escotis3$DOW[i],Escotis3$WOY[i],Escotis3$Voie[i],"A8")
      }  #Trajet Saint Isidore Ech Est -> La Turbie
    }
    else if (Escotis3$Sor[i] == 25006019){ #Saint Isidore Ech Ouest
      if (Escotis3$Voie[i] >= 20 ){#Sortie
        Escotis3_decompose[Pointeur,] <- c(Escotis3$ID[i],25006012 ,25006019,Escotis3$KMS[i],Escotis3$Year[i],Escotis3$Month[i],Escotis3$Day[i],Escotis3$Hour[i],Escotis3$Minute[i],Escotis3$TimeEntr[i],Escotis3$TimeSor[i],Escotis3$DOW[i],Escotis3$WOY[i],Escotis3$Voie[i],"A8")
      } #On crée un trajet Antibes PV -> Saint Isidore Ech Ouest
      else { 
        Escotis3_decompose[Pointeur,] <- c(Escotis3$ID[i], 25006019 ,25006012,Escotis3$KMS[i],Escotis3$Year[i],Escotis3$Month[i],Escotis3$Day[i],Escotis3$Hour[i],Escotis3$Minute[i],Escotis3$TimeEntr[i],Escotis3$TimeSor[i],Escotis3$DOW[i],Escotis3$WOY[i],Escotis3$Voie[i],"A8")
      }  #Trajet Saint Isidore Ech Ouest -> Antibes PV
    }
    else if (Escotis3$Sor[i] == 25006020){ #Saint Isidore PV
      if (Escotis3$Voie[i] >= 20 ){#Nord, donc de l'Italie vers Cannes
        Escotis3_decompose[Pointeur,] <- c(Escotis3$ID[i],25006026 ,25006020,Escotis3$KMS[i],Escotis3$Year[i],Escotis3$Month[i],Escotis3$Day[i],Escotis3$Hour[i],Escotis3$Minute[i],Escotis3$TimeEntr[i],Escotis3$TimeSor[i],Escotis3$DOW[i],Escotis3$WOY[i],Escotis3$Voie[i],"A8")
      } #On crée un trajet La Turbie Ech -> Cagnes Ouest Nord
      else { #Sud, de Cannes vers l'Italie
        Escotis3_decompose[Pointeur,] <- c(Escotis3$ID[i], 25006020 ,25006026,Escotis3$KMS[i],Escotis3$Year[i],Escotis3$Month[i],Escotis3$Day[i],Escotis3$Hour[i],Escotis3$Minute[i],Escotis3$TimeEntr[i],Escotis3$TimeSor[i],Escotis3$DOW[i],Escotis3$WOY[i],Escotis3$Voie[i],"A8")
      }  #Trajet Cagnes Ouest Nord -> La Turbie Ech
    }
    else if (Escotis3$Sor[i] == 25006026){ #La Turbie Ech
      if (Escotis3$Voie[i] >= 20 ){#Sortie
        Escotis3_decompose[Pointeur,] <- c(Escotis3$ID[i],25006020 ,25006026,Escotis3$KMS[i],Escotis3$Year[i],Escotis3$Month[i],Escotis3$Day[i],Escotis3$Hour[i],Escotis3$Minute[i],Escotis3$TimeEntr[i],Escotis3$TimeSor[i],Escotis3$DOW[i],Escotis3$WOY[i],Escotis3$Voie[i],"A8")
      } #On crée un trajet Saint isidore PV -> La Turbie Ech
      else { 
        Escotis3_decompose[Pointeur,] <- c(Escotis3$ID[i], 25006026 ,25006020,Escotis3$KMS[i],Escotis3$Year[i],Escotis3$Month[i],Escotis3$Day[i],Escotis3$Hour[i],Escotis3$Minute[i],Escotis3$TimeEntr[i],Escotis3$TimeSor[i],Escotis3$DOW[i],Escotis3$WOY[i],Escotis3$Voie[i],"A8")
      }  #Trajet La Turbie Ech -> Saint isidore PV
    }
    else if (Escotis3$Sor[i] == 25006027){ #La Turbie PV
      if (Escotis3$Voie[i] >= 20 ){# Nord, donc de l'Italie vers Cannes
        Escotis3_decompose[Pointeur,] <- c(Escotis3$ID[i],25006027 ,25006026,Escotis3$KMS[i],Escotis3$Year[i],Escotis3$Month[i],Escotis3$Day[i],Escotis3$Hour[i],Escotis3$Minute[i],Escotis3$TimeEntr[i],Escotis3$TimeSor[i],Escotis3$DOW[i],Escotis3$WOY[i],Escotis3$Voie[i],"A8")
      } #On crée un trajet La Turbie PV -> La Turbie Ech
      else { #Sud, de Cannes vers l'Italie
        Escotis3_decompose[Pointeur,] <- c(Escotis3$ID[i], 25006026 ,25006027,Escotis3$KMS[i],Escotis3$Year[i],Escotis3$Month[i],Escotis3$Day[i],Escotis3$Hour[i],Escotis3$Minute[i],Escotis3$TimeEntr[i],Escotis3$TimeSor[i],Escotis3$DOW[i],Escotis3$WOY[i],Escotis3$Voie[i],"A8")
      }  #Trajet La Turbie Ech -> La Turbie PV
    }
    else { Gare_inconnue <- Gare_inconnue +1
      }
  }
  Pointeur <- Pointeur +1
  Pointeur_restant <- Pointeur_restant +1
}


Escotis3_decompose <- Escotis3_decompose[Escotis3_decompose$Autoroute > 0,]






#DECOMPOSER LES OD PAR TRONCONS :
Escotis3_par_troncons <- data.frame(ID = "", ID_Troncon= 0, Autoroute= "", Entr= 0, Sor=0, KMS=0, Year=0, Month=0, Day=0, Hour=0, Minute=0, TimeEntr = 0, TimeSor =0, DOW=0, WOY=0, Sens = 0)
Escotis3_par_troncons$Autoroute <- as.character(Escotis3_par_troncons$Autoroute)
Escotis3_decompose$Year <- as.character(Escotis3_decompose$Year)
Escotis3_par_troncons$ID <- as.character(Escotis3_par_troncons$ID)

for (i in 1:nrow(Escotis3_decompose)){
  if ( Escotis3_decompose$Autoroute[i] == "A7"){
    entree <- match(Escotis3_decompose$Entr[i],A7_par_pk$Cde)
    sortie <- match(Escotis3_decompose$Sor[i],A7_par_pk$Cde)
    if ( entree < sortie ) { # SENS 1
      for ( j in entree : (sortie-1) ){
        Escotis3_par_troncons <- rbind(Escotis3_par_troncons,c(Escotis3_decompose$ID[i],Troncons_A7$ID_Troncon[j],"A7",A7_par_pk$Cde[j],A7_par_pk$Cde[j+1],Escotis3_decompose$KMS[i],Escotis3_decompose$Year[i],Escotis3_decompose$Month[i],Escotis3_decompose$Day[i],Escotis3_decompose$Hour[i],Escotis3_decompose$Minute[i],Escotis3_decompose$TimeEntr[i],Escotis3_decompose$TimeSor[i],Escotis3_decompose$DOW[i],Escotis3_decompose$WOY[i],1))
      }
    }
    else{ #SENS 2
      for ( j in entree : (sortie+1) ){
        Escotis3_par_troncons <- rbind(Escotis3_par_troncons,c(Escotis3_decompose$ID[i],Troncons_A7$ID_Troncon[j-1],"A7",A7_par_pk$Cde[j],A7_par_pk$Cde[j-1],Escotis3_decompose$KMS[i],Escotis3_decompose$Year[i],Escotis3_decompose$Month[i],Escotis3_decompose$Day[i],Escotis3_decompose$Hour[i],Escotis3_decompose$Minute[i],Escotis3_decompose$TimeEntr[i],Escotis3_decompose$TimeSor[i],Escotis3_decompose$DOW[i],Escotis3_decompose$WOY[i],2))
      }
    }
  }
  else if ( Escotis3_decompose$Autoroute[i] == "A8"){
    entree <- match(Escotis3_decompose$Entr[i],A8_par_pk$Cde)
    sortie <- match(Escotis3_decompose$Sor[i],A8_par_pk$Cde)
    if ( entree < sortie ) { # SENS 1
      for ( j in entree : (sortie-1) ){
        Escotis3_par_troncons <- rbind(Escotis3_par_troncons,c(Escotis3_decompose$ID[i],Troncons_A8$ID_Troncon[j],"A8",A8_par_pk$Cde[j],A8_par_pk$Cde[j+1],Escotis3_decompose$KMS[i],Escotis3_decompose$Year[i],Escotis3_decompose$Month[i],Escotis3_decompose$Day[i],Escotis3_decompose$Hour[i],Escotis3_decompose$Minute[i],Escotis3_decompose$TimeEntr[i],Escotis3_decompose$TimeSor[i],Escotis3_decompose$DOW[i],Escotis3_decompose$WOY[i],1))
      }
    }
    else{ #SENS 2
      for ( j in entree : (sortie+1) ){
        Escotis3_par_troncons <- rbind(Escotis3_par_troncons,c(Escotis3_decompose$ID[i],Troncons_A8$ID_Troncon[j-1],"A8",A8_par_pk$Cde[j],A8_par_pk$Cde[j-1],Escotis3_decompose$KMS[i],Escotis3_decompose$Year[i],Escotis3_decompose$Month[i],Escotis3_decompose$Day[i],Escotis3_decompose$Hour[i],Escotis3_decompose$Minute[i],Escotis3_decompose$TimeEntr[i],Escotis3_decompose$TimeSor[i],Escotis3_decompose$DOW[i],Escotis3_decompose$WOY[i],2))
      }
    }
  }
  else if ( Escotis3_decompose$Autoroute[i] == "A9"){
    entree <- match(Escotis3_decompose$Entr[i],A9_par_pk$Cde)
    sortie <- match(Escotis3_decompose$Sor[i],A9_par_pk$Cde)
    if ( entree < sortie ) { # SENS 1
      for ( j in entree : (sortie-1) ){
        Escotis3_par_troncons <- rbind(Escotis3_par_troncons,c(Escotis3_decompose$ID[i],Troncons_A9$ID_Troncon[j],"A9",A9_par_pk$Cde[j],A9_par_pk$Cde[j+1],Escotis3_decompose$KMS[i],Escotis3_decompose$Year[i],Escotis3_decompose$Month[i],Escotis3_decompose$Day[i],Escotis3_decompose$Hour[i],Escotis3_decompose$Minute[i],Escotis3_decompose$TimeEntr[i],Escotis3_decompose$TimeSor[i],Escotis3_decompose$DOW[i],Escotis3_decompose$WOY[i],1))
      }
    }
    else{ #SENS 2
      for ( j in entree : (sortie+1) ){
        Escotis3_par_troncons <- rbind(Escotis3_par_troncons,c(Escotis3_decompose$ID[i],Troncons_A9$ID_Troncon[j-1],"A9",A9_par_pk$Cde[j],A9_par_pk$Cde[j-1],Escotis3_decompose$KMS[i],Escotis3_decompose$Year[i],Escotis3_decompose$Month[i],Escotis3_decompose$Day[i],Escotis3_decompose$Hour[i],Escotis3_decompose$Minute[i],Escotis3_decompose$TimeEntr[i],Escotis3_decompose$TimeSor[i],Escotis3_decompose$DOW[i],Escotis3_decompose$WOY[i],2))
      }
    }
  }
}
Escotis3_par_troncons <- Escotis3_par_troncons[-1,]


#Rajouter demi trajet LANCON LA BARQUE
Pointeur <- 1
for (i in 1:nrow(Escotis3_par_troncons)){
  if (Escotis3_par_troncons$ID_Troncon[Pointeur] == 20 & Escotis3_par_troncons$Sens[Pointeur] == 1){ # (A7 -> Lancon)
    newrow1 <- c(Escotis3_par_troncons$ID[Pointeur],21,"A7",25004220,25004278,Escotis3_par_troncons$KMS[Pointeur],Escotis3_par_troncons$Year[Pointeur],Escotis3_par_troncons$Month[Pointeur],Escotis3_par_troncons$Day[Pointeur],Escotis3_par_troncons$Hour[Pointeur],Escotis3_par_troncons$Minute[Pointeur],Escotis3_par_troncons$TimeEntr[Pointeur],Escotis3_par_troncons$TimeSor[Pointeur],Escotis3_par_troncons$DOW[Pointeur],Escotis3_par_troncons$WOY[Pointeur],1)
    newrow2 <- c(Escotis3_par_troncons$ID[Pointeur],22,"A7",25004278,25004279,Escotis3_par_troncons$KMS[Pointeur],Escotis3_par_troncons$Year[Pointeur],Escotis3_par_troncons$Month[Pointeur],Escotis3_par_troncons$Day[Pointeur],Escotis3_par_troncons$Hour[Pointeur],Escotis3_par_troncons$Minute[Pointeur],Escotis3_par_troncons$TimeEntr[Pointeur],Escotis3_par_troncons$TimeSor[Pointeur],Escotis3_par_troncons$DOW[Pointeur],Escotis3_par_troncons$WOY[Pointeur],1)
    Escotis3_par_troncons <- rbind(Escotis3_par_troncons[(1:Pointeur),],newrow1,newrow2,Escotis3_par_troncons[-(1:Pointeur),])
    Pointeur <- Pointeur + 2
  } 
  if (Escotis3_par_troncons$ID_Troncon[Pointeur] == 20 & Escotis3_par_troncons$Sens[Pointeur] == 2){ # (Lancon -> A7)
    newrow1 <- c(Escotis3_par_troncons$ID[Pointeur],21,"A7",25004220,25004278,Escotis3_par_troncons$KMS[Pointeur],Escotis3_par_troncons$Year[Pointeur],Escotis3_par_troncons$Month[Pointeur],Escotis3_par_troncons$Day[Pointeur],Escotis3_par_troncons$Hour[Pointeur],Escotis3_par_troncons$Minute[Pointeur],Escotis3_par_troncons$TimeEntr[Pointeur],Escotis3_par_troncons$TimeSor[Pointeur],Escotis3_par_troncons$DOW[Pointeur],Escotis3_par_troncons$WOY[Pointeur],2)
    newrow2 <- c(Escotis3_par_troncons$ID[Pointeur],22,"A7",25004278,25004279,Escotis3_par_troncons$KMS[Pointeur],Escotis3_par_troncons$Year[Pointeur],Escotis3_par_troncons$Month[Pointeur],Escotis3_par_troncons$Day[Pointeur],Escotis3_par_troncons$Hour[Pointeur],Escotis3_par_troncons$Minute[Pointeur],Escotis3_par_troncons$TimeEntr[Pointeur],Escotis3_par_troncons$TimeSor[Pointeur],Escotis3_par_troncons$DOW[Pointeur],Escotis3_par_troncons$WOY[Pointeur],2)
    Escotis3_par_troncons <- rbind(Escotis3_par_troncons[(1:Pointeur),],newrow1,newrow2,Escotis3_par_troncons[-(1:Pointeur),])
    Pointeur <- Pointeur + 2
  }
  if (Escotis3_par_troncons$ID_Troncon[Pointeur] == 26 & Escotis3_par_troncons$Sens[Pointeur] == 1){ # (La Barque -> A8)
    newrow1 <- c(Escotis3_par_troncons$ID[Pointeur],23,"A8",25004279,25006001,Escotis3_par_troncons$KMS[Pointeur],Escotis3_par_troncons$Year[Pointeur],Escotis3_par_troncons$Month[Pointeur],Escotis3_par_troncons$Day[Pointeur],Escotis3_par_troncons$Hour[Pointeur],Escotis3_par_troncons$Minute[Pointeur],Escotis3_par_troncons$TimeEntr[Pointeur],Escotis3_par_troncons$TimeSor[Pointeur],Escotis3_par_troncons$DOW[Pointeur],Escotis3_par_troncons$WOY[Pointeur],1)
    newrow2 <- c(Escotis3_par_troncons$ID[Pointeur],24,"A8",25006001,25006080,Escotis3_par_troncons$KMS[Pointeur],Escotis3_par_troncons$Year[Pointeur],Escotis3_par_troncons$Month[Pointeur],Escotis3_par_troncons$Day[Pointeur],Escotis3_par_troncons$Hour[Pointeur],Escotis3_par_troncons$Minute[Pointeur],Escotis3_par_troncons$TimeEntr[Pointeur],Escotis3_par_troncons$TimeSor[Pointeur],Escotis3_par_troncons$DOW[Pointeur],Escotis3_par_troncons$WOY[Pointeur],1)
    newrow3 <- c(Escotis3_par_troncons$ID[Pointeur],25,"A8",25006080,25006002,Escotis3_par_troncons$KMS[Pointeur],Escotis3_par_troncons$Year[Pointeur],Escotis3_par_troncons$Month[Pointeur],Escotis3_par_troncons$Day[Pointeur],Escotis3_par_troncons$Hour[Pointeur],Escotis3_par_troncons$Minute[Pointeur],Escotis3_par_troncons$TimeEntr[Pointeur],Escotis3_par_troncons$TimeSor[Pointeur],Escotis3_par_troncons$DOW[Pointeur],Escotis3_par_troncons$WOY[Pointeur],1)
    Escotis3_par_troncons <- rbind(Escotis3_par_troncons[(1:Pointeur),],newrow1,newrow2,newrow3,Escotis3_par_troncons[-(1:Pointeur),])
    Pointeur <- Pointeur + 3
  }
  if (Escotis3_par_troncons$ID_Troncon[Pointeur] == 26 & Escotis3_par_troncons$Sens[Pointeur] == 2){ # (A8 -> La Barque)
    newrow1 <- c(Escotis3_par_troncons$ID[Pointeur],23,"A8",25004279,25006001,Escotis3_par_troncons$KMS[Pointeur],Escotis3_par_troncons$Year[Pointeur],Escotis3_par_troncons$Month[Pointeur],Escotis3_par_troncons$Day[Pointeur],Escotis3_par_troncons$Hour[Pointeur],Escotis3_par_troncons$Minute[Pointeur],Escotis3_par_troncons$TimeEntr[Pointeur],Escotis3_par_troncons$TimeSor[Pointeur],Escotis3_par_troncons$DOW[Pointeur],Escotis3_par_troncons$WOY[Pointeur],2)
    newrow2 <- c(Escotis3_par_troncons$ID[Pointeur],24,"A8",25006001,25006080,Escotis3_par_troncons$KMS[Pointeur],Escotis3_par_troncons$Year[Pointeur],Escotis3_par_troncons$Month[Pointeur],Escotis3_par_troncons$Day[Pointeur],Escotis3_par_troncons$Hour[Pointeur],Escotis3_par_troncons$Minute[Pointeur],Escotis3_par_troncons$TimeEntr[Pointeur],Escotis3_par_troncons$TimeSor[Pointeur],Escotis3_par_troncons$DOW[Pointeur],Escotis3_par_troncons$WOY[Pointeur],2)
    newrow3 <- c(Escotis3_par_troncons$ID[Pointeur],25,"A8",25006080,25006002,Escotis3_par_troncons$KMS[Pointeur],Escotis3_par_troncons$Year[Pointeur],Escotis3_par_troncons$Month[Pointeur],Escotis3_par_troncons$Day[Pointeur],Escotis3_par_troncons$Hour[Pointeur],Escotis3_par_troncons$Minute[Pointeur],Escotis3_par_troncons$TimeEntr[Pointeur],Escotis3_par_troncons$TimeSor[Pointeur],Escotis3_par_troncons$DOW[Pointeur],Escotis3_par_troncons$WOY[Pointeur],2)
    Escotis3_par_troncons <- rbind(Escotis3_par_troncons[(1:Pointeur),],newrow1,newrow2,newrow3,Escotis3_par_troncons[-(1:Pointeur),])
    Pointeur <- Pointeur + 3
  }
  Pointeur <- Pointeur + 1
}

#COMPTAGE DES TRAJETS PAR TRONCON PAR SENS PAR DOW
CompteurNP <- vector(mode= "integer" , length = 75)
CompteurPC <- vector(mode= "integer" , length = 75)
CompteurFF <- vector(mode= "integer" , length = 75)
CompteurCC <- vector(mode= "integer" , length = 75)
CompteurJA <- vector(mode= "integer" , length = 75)
CompteurMJ <- vector(mode= "integer" , length = 75)
CompteurLF <- vector(mode= "integer" , length = 75)
CompteurMR <- vector(mode= "integer" , length = 75)
CompteurVM <- vector(mode= "integer" , length = 75)
CompteurMC <- vector(mode= "integer" , length = 75)
CompteurJP <- vector(mode= "integer" , length = 75)
CompteurFC <- vector(mode= "integer" , length = 75)

for ( j in 1 : 75 ){
  CompteurNP[j] <- sum(Escotis3_par_troncons$ID == "NP" & Escotis3_par_troncons$ID_Troncon == j)
  CompteurPC[j] <- sum(Escotis3_par_troncons$ID == "PC" & Escotis3_par_troncons$ID_Troncon == j)
  CompteurFF[j] <- sum(Escotis3_par_troncons$ID == "FF" & Escotis3_par_troncons$ID_Troncon == j)
  CompteurCC[j] <- sum(Escotis3_par_troncons$ID == "CC" & Escotis3_par_troncons$ID_Troncon == j)
  CompteurJA[j] <- sum(Escotis3_par_troncons$ID == "JA" & Escotis3_par_troncons$ID_Troncon == j)
  CompteurMJ[j] <- sum(Escotis3_par_troncons$ID == "MJ" & Escotis3_par_troncons$ID_Troncon == j)
  CompteurLF[j] <- sum(Escotis3_par_troncons$ID == "LF" & Escotis3_par_troncons$ID_Troncon == j)
  CompteurMR[j] <- sum(Escotis3_par_troncons$ID == "MR" & Escotis3_par_troncons$ID_Troncon == j)
  CompteurVM[j] <- sum(Escotis3_par_troncons$ID == "VM" & Escotis3_par_troncons$ID_Troncon == j)
  CompteurMC[j] <- sum(Escotis3_par_troncons$ID == "MC" & Escotis3_par_troncons$ID_Troncon == j)
  CompteurJP[j] <- sum(Escotis3_par_troncons$ID == "JP" & Escotis3_par_troncons$ID_Troncon == j)
  CompteurFC[j] <- sum(Escotis3_par_troncons$ID == "FC" & Escotis3_par_troncons$ID_Troncon == j)
  
}

ggplot(ComptageFFNP) + geom_point(aes(Début_Lng,Début_Lat)) +
  geom_segment(aes(x = Début_Lng, xend =Fin_Lng, y = Début_Lat, yend = Fin_Lat, size = CompteurCC)) +
  scale_size(range = c(0, 10))

#DECISION AUTOMATIQUE DES TRONCONS FREQUENTS

#FF et NP
# [70% max ; max ]
max_des_troncons_FF <- max(CompteurFF)
CritereFF <- max_des_troncons_FF * 7 / 10
FF_troncons_selection <- NULL
max_des_troncons_NP <- max(CompteurNP)
CritereNP <- max_des_troncons_NP * 7 / 10
NP_troncons_selection <- NULL
for (i in 1 : 75){
  if (CompteurFF[i] >= CritereFF){
    FF_troncons_selection <- c(FF_troncons_selection,i)
  }
  if (CompteurNP[i] >= CritereNP){
    NP_troncons_selection <- c(NP_troncons_selection,i)
  }
}
