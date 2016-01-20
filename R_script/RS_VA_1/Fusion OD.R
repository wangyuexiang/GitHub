#Objectif : fusionner les OD du type de Claire :
#Tout OD sortant à Lancon et suivi dans l'heure et 30min suivante d'une entrée à La Barque -> Fusion en une seule OD
#Avec les bons temps et infos complémentaires. (Temps d'entrée de l'OD XX -> Lancon, temps de sortie du trajet la Barque -> XXX)
#Idem dans l'autre sens bien sûr.


fusion_OD <- function ( Transactions) {
  Transactions$Date <- as.character.Date(Transactions$Date)
  Transactions$TimeEntr <- as.numeric(Transactions$TimeEntr)
  Transactions$TimeSor <- as.numeric(Transactions$TimeSor)
  
  i <- 1
  #On commence par trier par ID et Date Heure chronologique
  Transactions <- Transactions %>%
    group_by(ID,Date) %>%
    arrange(TimeEntr)
  
  while (i < nrow(Transactions)){
    if (Transactions$Sor[i] == 25004220 ) { #Sortie = Lancon
      if((Transactions$Entr[i+1] == 25006002) & (Transactions$TimeSor[i] < Transactions$TimeEntr[i+1]) & (Transactions$TimeEntr[i+1] < (Transactions$TimeSor[i] + 1.5))){
        if( i == 1){ #if the first row is concerned, we have to treat the case apart, about the fusion and the index.
          Transactions <- rbind(c(Transactions$ID[i],Transactions$Entr[i],Transactions$Sor[i+1],0,0,0,0,0,0,Transactions$TimeEntr[i],Transactions$TimeSor[i+1],Transactions$DOW[i],Transactions$WOY[i],Transactions$Date[i]),
                                Transactions[3:nrow(Transactions),])
          Transactions$TimeEntr <- as.numeric(Transactions$TimeEntr)
          Transactions$TimeSor <- as.numeric(Transactions$TimeSor)
        }
        else if (i == (nrow(Transactions)-1)){ #Same if it is for the last two rows.
          Transactions <- rbind(Transactions[1:(nrow(Transactions)-2),],
                                c(Transactions$ID[i],Transactions$Entr[i],Transactions$Sor[i+1],0,0,0,0,0,0,Transactions$TimeEntr[i],Transactions$TimeSor[i+1],Transactions$DOW[i],Transactions$WOY[i],Transactions$Date[i]))
          Transactions$TimeEntr <- as.numeric(Transactions$TimeEntr)
          Transactions$TimeSor <- as.numeric(Transactions$TimeSor)
        }
        else{ #General case.
          Transactions <- rbind(Transactions[1:(i-1),],
                                c(Transactions$ID[i],Transactions$Entr[i],Transactions$Sor[i+1],0,0,0,0,0,0,Transactions$TimeEntr[i],Transactions$TimeSor[i+1],Transactions$DOW[i],Transactions$WOY[i],Transactions$Date[i]),
                                Transactions[(i+2):nrow(Transactions),])
          Transactions$TimeEntr <- as.numeric(Transactions$TimeEntr)
          Transactions$TimeSor <- as.numeric(Transactions$TimeSor)
        }
      }
    }
    else if (Transactions$Sor[i] == 25006002){ # Sortie = La Barque
      if( i == 1){
        Transactions <- rbind(c(Transactions$ID[i],Transactions$Entr[i],Transactions$Sor[i+1],0,0,0,0,0,0,Transactions$TimeEntr[i],Transactions$TimeSor[i+1],Transactions$DOW[i],Transactions$WOY[i],Transactions$Date[i]),
                              Transactions[3:nrow(Transactions),])
        Transactions$TimeEntr <- as.numeric(Transactions$TimeEntr)
        Transactions$TimeSor <- as.numeric(Transactions$TimeSor)
      }
      else if (i == (nrow(Transactions)-1)){
        Transactions <- rbind(Transactions[1:(nrow(Transactions)-2),],
                              c(Transactions$ID[i],Transactions$Entr[i],Transactions$Sor[i+1],0,0,0,0,0,0,Transactions$TimeEntr[i],Transactions$TimeSor[i+1],Transactions$DOW[i],Transactions$WOY[i],Transactions$Date[i]))
        Transactions$TimeEntr <- as.numeric(Transactions$TimeEntr)
        Transactions$TimeSor <- as.numeric(Transactions$TimeSor)
      }
      else{
        Transactions <- rbind(Transactions[1:(i-1),],
                              c(Transactions$ID[i],Transactions$Entr[i],Transactions$Sor[i+1],0,0,0,0,0,0,Transactions$TimeEntr[i],Transactions$TimeSor[i+1],Transactions$DOW[i],Transactions$WOY[i],Transactions$Date[i]),
                              Transactions[(i+2):nrow(Transactions),])
        Transactions$TimeEntr <- as.numeric(Transactions$TimeEntr)
        Transactions$TimeSor <- as.numeric(Transactions$TimeSor)
      }
    }
    i <- i + 1 
  }
  return (Transactions)
}