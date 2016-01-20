#Transformer les DF
NP2 <- cbind(NP$ID,NP[1:2],NP$Year,NP$Month,NP$Day,NP$Hour,NP$Minute,NP$Time,NP$Date,NP$DOW,NP$WOY)
x <- vector(mode = "integer", length = nrow(NP2))
NP2 <- cbind(NP2[,1],x,NP2[,2:3],x,x,NP2[,4:12])
colnames(NP2)<- colnames(ID_1_5)

FF2 <- cbind(FF$ID,FF[1:2],FF$Year,FF$Month,FF$Day,FF$Hour,FF$Minute,FF$Time,FF$Date,FF$DOW,FF$WOY)
x <- vector(mode = "integer", length = nrow(FF2))
FF2 <- cbind(FF2[,1],x,FF2[,2:3],x,x,FF2[,4:12])
colnames(FF2)<- colnames(ID_1_5)

#FF
#DECOMPOSER LES TRAJETS PAR AUTOROUTE UNIQUE
temp$Autoroute <- as.character(temp$Autoroute)
FF2$Entr <- as.numeric(as.character(FF2$Entr))
FF2$Sor <- as.numeric(as.character(FF2$Sor))
FF2_decompose <- FF2
Autoroute <- vector(mode = "character", length = nrow(FF2))
FF2_decompose <- cbind(FF2_decompose, Autoroute)
FF2_decompose$Autoroute <- as.character(FF2_decompose$Autoroute)
FF2_restant <- FF2
Pointeur <- 1
Pointeur_restant <- 1
for (i in 1 : nrow(FF2)){
  if (  ( (FF2$Entr[i] %in% Troncons_A789[,4]) | (FF2$Entr[i] %in% Troncons_A789[,6])  ) & 
        ( (FF2$Sor[i] %in% Troncons_A789[,4]) | (FF2$Sor[i] %in% Troncons_A789[,6])  )  ) {  # SI TRAJET DANS LES TROIS AUTOROUTES
    if (temp$Autoroute[match(FF2$Entr[i],temp$Cde)] !=  temp$Autoroute[match(FF2$Sor[i],temp$Cde)] ) { #E=A7etS=A9 ou l'inverse
      if (FF2$Entr[i] != 25004210 & FF2$Sor[i] != 25004210){  # Entrée et sortie <> OC
        if(temp$Autoroute[match(FF2$Entr[i],temp$Cde)] == "A7"){# E=A7 S=A9
          newrow1 <- c(FF2[i,1],FF2$Label[i],FF2$Entr[i],25004210,0,FF2$KMS[i],FF2$Year[i],FF2$Month[i],FF2$Day[i],FF2$Hour[i],FF2$Minute[i],FF2$Time[i],FF2$Date[i],FF2$DOW[i],FF2$WOY[i],"A7")
          newrow2 <- c(FF2[i,1],FF2$Label[i],25004210,FF2$Sor[i],0,FF2$KMS[i],FF2$Year[i],FF2$Month[i],FF2$Day[i],FF2$Hour[i],FF2$Minute[i],FF2$Time[i],FF2$Date[i],FF2$DOW[i],FF2$WOY[i],"A9")
          FF2_decompose = rbind(FF2_decompose[1:Pointeur,],newrow1,newrow2,FF2_decompose[-(1:Pointeur),])
          Pointeur <- Pointeur +2
        }
        else if(temp$Autoroute[match(FF2$Entr[i],temp$Cde)] == "A9"){# E=A9 S=A7
          newrow1 <- c(FF2[i,1],FF2$Label[i],FF2$Entr[i],25004210,0,FF2$KMS[i],FF2$Year[i],FF2$Month[i],FF2$Day[i],FF2$Hour[i],FF2$Minute[i],FF2$Time[i],FF2$Date[i],FF2$DOW[i],FF2$WOY[i],"A9")
          newrow2 <- c(FF2[i,1],FF2$Label[i],25004210,FF2$Sor[i],0,FF2$KMS[i],FF2$Year[i],FF2$Month[i],FF2$Day[i],FF2$Hour[i],FF2$Minute[i],FF2$Time[i],FF2$Date[i],FF2$DOW[i],FF2$WOY[i],"A7")
          FF2_decompose = rbind(FF2_decompose[1:Pointeur,],newrow1,newrow2,FF2_decompose[-(1:Pointeur),])
          Pointeur <- Pointeur +2
        }
      }
      else { # E=A9 S=OC ou l'inverse
        FF2_decompose$Autoroute[Pointeur] <- "A9"
      }
    }
    else {
      FF2_decompose$Autoroute[Pointeur] <- temp$Autoroute[match(FF2$Entr[i],temp$Cde)]
    }
    if (Pointeur_restant == 1){
      FF2_restant <- FF2_restant[-1:1,]
    }
    else {
      FF2_restant <- rbind(FF2_restant[(1:(Pointeur_restant-1)),],FF2_restant[-(1:(Pointeur_restant)),])
      Pointeur_restant <- Pointeur_restant - 1
    }
  }
  Pointeur <- Pointeur +1
  Pointeur_restant <- Pointeur_restant +1
}


FF2_decompose <- FF2_decompose[FF2_decompose$Autoroute > 0,]

#DECOMPOSER LES OD PAR TRONCONS :
FF2_par_troncons <- data.frame( ID_Troncon= 0, Autoroute= "", Entr= 0, Sor=0, KMS=0, Year=0, Month=0, Day=0, Hour=0, Minute=0, Time = 0, DOW=0, WOY=0, Sens = 0)
FF2_par_troncons$Autoroute <- as.character(FF2_par_troncons$Autoroute)
FF2_decompose$Year <- as.character(FF2_decompose$Year)
for (i in 1:nrow(FF2_decompose)){
  if ( FF2_decompose$Autoroute[i] == "A7"){
    entree <- match(FF2_decompose$Entr[i],A7_par_pk$Cde)
    sortie <- match(FF2_decompose$Sor[i],A7_par_pk$Cde)
    if ( entree < sortie ) { # SENS 1
      for ( j in entree : (sortie-1) ){
        FF2_par_troncons <- rbind(FF2_par_troncons,c(Troncons_A7$ID_Troncon[j],"A7",A7_par_pk$Cde[j],A7_par_pk$Cde[j+1],FF2_decompose$KMS[i],FF2_decompose$Year[i],FF2_decompose$Month[i],FF2_decompose$Day[i],FF2_decompose$Hour[i],FF2_decompose$Minute[i],FF2_decompose$Time[i],FF2_decompose$DOW[i],FF2_decompose$WOY[i],1))
      }
    }
    else{ #SENS 2
      for ( j in entree : (sortie+1) ){
        FF2_par_troncons <- rbind(FF2_par_troncons,c(Troncons_A7$ID_Troncon[j-1],"A7",A7_par_pk$Cde[j],A7_par_pk$Cde[j-1],FF2_decompose$KMS[i],FF2_decompose$Year[i],FF2_decompose$Month[i],FF2_decompose$Day[i],FF2_decompose$Hour[i],FF2_decompose$Minute[i],FF2_decompose$Time[i],FF2_decompose$DOW[i],FF2_decompose$WOY[i],2))
      }
    }
  }
  else if ( FF2_decompose$Autoroute[i] == "A8"){
    entree <- match(FF2_decompose$Entr[i],A8_par_pk$Cde)
    sortie <- match(FF2_decompose$Sor[i],A8_par_pk$Cde)
    if ( entree < sortie ) { # SENS 1
      for ( j in entree : (sortie-1) ){
        FF2_par_troncons <- rbind(FF2_par_troncons,c(Troncons_A8$ID_Troncon[j],"A8",A8_par_pk$Cde[j],A8_par_pk$Cde[j+1],FF2_decompose$KMS[i],FF2_decompose$Year[i],FF2_decompose$Month[i],FF2_decompose$Day[i],FF2_decompose$Hour[i],FF2_decompose$Minute[i],FF2_decompose$Time[i],FF2_decompose$DOW[i],FF2_decompose$WOY[i],1))
      }
    }
    else{ #SENS 2
      for ( j in entree : (sortie+1) ){
        FF2_par_troncons <- rbind(FF2_par_troncons,c(Troncons_A8$ID_Troncon[j-1],"A8",A8_par_pk$Cde[j],A8_par_pk$Cde[j-1],FF2_decompose$KMS[i],FF2_decompose$Year[i],FF2_decompose$Month[i],FF2_decompose$Day[i],FF2_decompose$Hour[i],FF2_decompose$Minute[i],FF2_decompose$Time[i],FF2_decompose$DOW[i],FF2_decompose$WOY[i],2))
      }
    }
  }
  else if ( FF2_decompose$Autoroute[i] == "A9"){
    entree <- match(FF2_decompose$Entr[i],A9_par_pk$Cde)
    sortie <- match(FF2_decompose$Sor[i],A9_par_pk$Cde)
    if ( entree < sortie ) { # SENS 1
      for ( j in entree : (sortie-1) ){
        FF2_par_troncons <- rbind(FF2_par_troncons,c(Troncons_A9$ID_Troncon[j],"A9",A9_par_pk$Cde[j],A9_par_pk$Cde[j+1],FF2_decompose$KMS[i],FF2_decompose$Year[i],FF2_decompose$Month[i],FF2_decompose$Day[i],FF2_decompose$Hour[i],FF2_decompose$Minute[i],FF2_decompose$Time[i],FF2_decompose$DOW[i],FF2_decompose$WOY[i],1))
      }
    }
    else{ #SENS 2
      for ( j in entree : (sortie+1) ){
        FF2_par_troncons <- rbind(FF2_par_troncons,c(Troncons_A9$ID_Troncon[j-1],"A9",A9_par_pk$Cde[j],A9_par_pk$Cde[j-1],FF2_decompose$KMS[i],FF2_decompose$Year[i],FF2_decompose$Month[i],FF2_decompose$Day[i],FF2_decompose$Hour[i],FF2_decompose$Minute[i],FF2_decompose$Time[i],FF2_decompose$DOW[i],FF2_decompose$WOY[i],2))
      }
    }
  }
}
FF2_par_troncons <- FF2_par_troncons[-1,]


#Rajouter demi trajet LANCON LA BARQUE
Pointeur <- 1
for (i in 1:nrow(FF2_par_troncons)){
  if (FF2_par_troncons$ID_Troncon[Pointeur] == 20 & FF2_par_troncons$Sens[Pointeur] == 1){ # (A7 -> Lancon)
    newrow1 <- c(21,"A7",25004220,25004278,FF2_par_troncons$KMS[Pointeur],FF2_par_troncons$Year[Pointeur],FF2_par_troncons$Month[Pointeur],FF2_par_troncons$Day[Pointeur],FF2_par_troncons$Hour[Pointeur],FF2_par_troncons$Minute[Pointeur],FF2_par_troncons$Time[Pointeur],FF2_par_troncons$DOW[Pointeur],FF2_par_troncons$WOY[Pointeur],1)
    newrow2 <- c(22,"A7",25004278,25004279,FF2_par_troncons$KMS[Pointeur],FF2_par_troncons$Year[Pointeur],FF2_par_troncons$Month[Pointeur],FF2_par_troncons$Day[Pointeur],FF2_par_troncons$Hour[Pointeur],FF2_par_troncons$Minute[Pointeur],FF2_par_troncons$Time[Pointeur],FF2_par_troncons$DOW[Pointeur],FF2_par_troncons$WOY[Pointeur],1)
    FF2_par_troncons <- rbind(FF2_par_troncons[(1:Pointeur),],newrow1,newrow2,FF2_par_troncons[-(1:Pointeur),])
    Pointeur <- Pointeur + 2
  } 
  if (FF2_par_troncons$ID_Troncon[Pointeur] == 20 & FF2_par_troncons$Sens[Pointeur] == 2){ # (Lancon -> A7)
    newrow1 <- c(21,"A7",25004220,25004278,FF2_par_troncons$KMS[Pointeur],FF2_par_troncons$Year[Pointeur],FF2_par_troncons$Month[Pointeur],FF2_par_troncons$Day[Pointeur],FF2_par_troncons$Hour[Pointeur],FF2_par_troncons$Minute[Pointeur],FF2_par_troncons$Time[Pointeur],FF2_par_troncons$DOW[Pointeur],FF2_par_troncons$WOY[Pointeur],2)
    newrow2 <- c(22,"A7",25004278,25004279,FF2_par_troncons$KMS[Pointeur],FF2_par_troncons$Year[Pointeur],FF2_par_troncons$Month[Pointeur],FF2_par_troncons$Day[Pointeur],FF2_par_troncons$Hour[Pointeur],FF2_par_troncons$Minute[Pointeur],FF2_par_troncons$Time[Pointeur],FF2_par_troncons$DOW[Pointeur],FF2_par_troncons$WOY[Pointeur],2)
    FF2_par_troncons <- rbind(FF2_par_troncons[(1:Pointeur),],newrow1,newrow2,FF2_par_troncons[-(1:Pointeur),])
    Pointeur <- Pointeur + 2
  }
  if (FF2_par_troncons$ID_Troncon[Pointeur] == 26 & FF2_par_troncons$Sens[Pointeur] == 1){ # (La Barque -> A8)
    newrow1 <- c(23,"A8",25004279,25006001,FF2_par_troncons$KMS[Pointeur],FF2_par_troncons$Year[Pointeur],FF2_par_troncons$Month[Pointeur],FF2_par_troncons$Day[Pointeur],FF2_par_troncons$Hour[Pointeur],FF2_par_troncons$Minute[Pointeur],FF2_par_troncons$Time[Pointeur],FF2_par_troncons$DOW[Pointeur],FF2_par_troncons$WOY[Pointeur],1)
    newrow2 <- c(24,"A8",25006001,25006080,FF2_par_troncons$KMS[Pointeur],FF2_par_troncons$Year[Pointeur],FF2_par_troncons$Month[Pointeur],FF2_par_troncons$Day[Pointeur],FF2_par_troncons$Hour[Pointeur],FF2_par_troncons$Minute[Pointeur],FF2_par_troncons$Time[Pointeur],FF2_par_troncons$DOW[Pointeur],FF2_par_troncons$WOY[Pointeur],1)
    newrow3 <- c(25,"A8",25006080,25006002,FF2_par_troncons$KMS[Pointeur],FF2_par_troncons$Year[Pointeur],FF2_par_troncons$Month[Pointeur],FF2_par_troncons$Day[Pointeur],FF2_par_troncons$Hour[Pointeur],FF2_par_troncons$Minute[Pointeur],FF2_par_troncons$Time[Pointeur],FF2_par_troncons$DOW[Pointeur],FF2_par_troncons$WOY[Pointeur],1)
    FF2_par_troncons <- rbind(FF2_par_troncons[(1:Pointeur),],newrow1,newrow2,newrow3,FF2_par_troncons[-(1:Pointeur),])
    Pointeur <- Pointeur + 3
  }
  if (FF2_par_troncons$ID_Troncon[Pointeur] == 26 & FF2_par_troncons$Sens[Pointeur] == 2){ # (A8 -> La Barque)
    newrow1 <- c(23,"A8",25004279,25006001,FF2_par_troncons$KMS[Pointeur],FF2_par_troncons$Year[Pointeur],FF2_par_troncons$Month[Pointeur],FF2_par_troncons$Day[Pointeur],FF2_par_troncons$Hour[Pointeur],FF2_par_troncons$Minute[Pointeur],FF2_par_troncons$Time[Pointeur],FF2_par_troncons$DOW[Pointeur],FF2_par_troncons$WOY[Pointeur],2)
    newrow2 <- c(24,"A8",25006001,25006080,FF2_par_troncons$KMS[Pointeur],FF2_par_troncons$Year[Pointeur],FF2_par_troncons$Month[Pointeur],FF2_par_troncons$Day[Pointeur],FF2_par_troncons$Hour[Pointeur],FF2_par_troncons$Minute[Pointeur],FF2_par_troncons$Time[Pointeur],FF2_par_troncons$DOW[Pointeur],FF2_par_troncons$WOY[Pointeur],2)
    newrow3 <- c(25,"A8",25006080,25006002,FF2_par_troncons$KMS[Pointeur],FF2_par_troncons$Year[Pointeur],FF2_par_troncons$Month[Pointeur],FF2_par_troncons$Day[Pointeur],FF2_par_troncons$Hour[Pointeur],FF2_par_troncons$Minute[Pointeur],FF2_par_troncons$Time[Pointeur],FF2_par_troncons$DOW[Pointeur],FF2_par_troncons$WOY[Pointeur],2)
    FF2_par_troncons <- rbind(FF2_par_troncons[(1:Pointeur),],newrow1,newrow2,newrow3,FF2_par_troncons[-(1:Pointeur),])
    Pointeur <- Pointeur + 3
  }
  Pointeur <- Pointeur + 1
}
#NP
#DECOMPOSER LES TRAJETS PAR AUTOROUTE UNIQUE
temp$Autoroute <- as.character(temp$Autoroute)
NP2$Entr <- as.numeric(as.character(NP2$Entr))
NP2$Sor <- as.numeric(as.character(NP2$Sor))
NP2_decompose <- NP2
Autoroute <- vector(mode = "character", length = nrow(NP2))
NP2_decompose <- cbind(NP2_decompose, Autoroute)
NP2_decompose$Autoroute <- as.character(NP2_decompose$Autoroute)
NP2_restant <- NP2
Pointeur <- 1
Pointeur_restant <- 1
for (i in 1 : nrow(NP2)){
  if (  ( (NP2$Entr[i] %in% Troncons_A789[,4]) | (NP2$Entr[i] %in% Troncons_A789[,6])  ) & 
        ( (NP2$Sor[i] %in% Troncons_A789[,4]) | (NP2$Sor[i] %in% Troncons_A789[,6])  )  ) {  # SI TRAJET DANS LES TROIS AUTOROUTES
    if (temp$Autoroute[match(NP2$Entr[i],temp$Cde)] !=  temp$Autoroute[match(NP2$Sor[i],temp$Cde)] ) { #E=A7etS=A9 ou l'inverse
      if (NP2$Entr[i] != 25004210 & NP2$Sor[i] != 25004210){  # Entrée et sortie <> OC
        if(temp$Autoroute[match(NP2$Entr[i],temp$Cde)] == "A7"){# E=A7 S=A9
          newrow1 <- c(NP2[i,1],NP2$Label[i],NP2$Entr[i],25004210,0,NP2$KMS[i],NP2$Year[i],NP2$Month[i],NP2$Day[i],NP2$Hour[i],NP2$Minute[i],NP2$Time[i],NP2$Date[i],NP2$DOW[i],NP2$WOY[i],"A7")
          newrow2 <- c(NP2[i,1],NP2$Label[i],25004210,NP2$Sor[i],0,NP2$KMS[i],NP2$Year[i],NP2$Month[i],NP2$Day[i],NP2$Hour[i],NP2$Minute[i],NP2$Time[i],NP2$Date[i],NP2$DOW[i],NP2$WOY[i],"A9")
          NP2_decompose = rbind(NP2_decompose[1:Pointeur,],newrow1,newrow2,NP2_decompose[-(1:Pointeur),])
          Pointeur <- Pointeur +2
        }
        else if(temp$Autoroute[match(NP2$Entr[i],temp$Cde)] == "A9"){# E=A9 S=A7
          newrow1 <- c(NP2[i,1],NP2$Label[i],NP2$Entr[i],25004210,0,NP2$KMS[i],NP2$Year[i],NP2$Month[i],NP2$Day[i],NP2$Hour[i],NP2$Minute[i],NP2$Time[i],NP2$Date[i],NP2$DOW[i],NP2$WOY[i],"A9")
          newrow2 <- c(NP2[i,1],NP2$Label[i],25004210,NP2$Sor[i],0,NP2$KMS[i],NP2$Year[i],NP2$Month[i],NP2$Day[i],NP2$Hour[i],NP2$Minute[i],NP2$Time[i],NP2$Date[i],NP2$DOW[i],NP2$WOY[i],"A7")
          NP2_decompose = rbind(NP2_decompose[1:Pointeur,],newrow1,newrow2,NP2_decompose[-(1:Pointeur),])
          Pointeur <- Pointeur +2
        }
      }
      else { # E=A9 S=OC ou l'inverse
        NP2_decompose$Autoroute[Pointeur] <- "A9"
      }
    }
    else {
      NP2_decompose$Autoroute[Pointeur] <- temp$Autoroute[match(NP2$Entr[i],temp$Cde)]
    }
    if (Pointeur_restant == 1){
      NP2_restant <- NP2_restant[-1:1,]
    }
    else {
      NP2_restant <- rbind(NP2_restant[(1:(Pointeur_restant-1)),],NP2_restant[-(1:(Pointeur_restant)),])
      Pointeur_restant <- Pointeur_restant - 1
    }
  }
  Pointeur <- Pointeur +1
  Pointeur_restant <- Pointeur_restant + 1
}


NP2_decompose <- NP2_decompose[NP2_decompose$Autoroute > 0,]

#DECOMPOSER LES OD PAR TRONCONS :
NP2_par_troncons <- data.frame( ID_Troncon= 0, Autoroute= "", Entr= 0, Sor=0, KMS=0, Year=0, Month=0, Day=0, Hour=0, Minute=0, Time = 0, DOW=0, WOY=0, Sens = 0)
NP2_par_troncons$Autoroute <- as.character(NP2_par_troncons$Autoroute)
NP2_decompose$Year <- as.character(NP2_decompose$Year)
for (i in 1:nrow(NP2_decompose)){
  if ( NP2_decompose$Autoroute[i] == "A7"){
    entree <- match(NP2_decompose$Entr[i],A7_par_pk$Cde)
    sortie <- match(NP2_decompose$Sor[i],A7_par_pk$Cde)
    if ( entree < sortie ) { # SENS 1
      for ( j in entree : (sortie-1) ){
        NP2_par_troncons <- rbind(NP2_par_troncons,c(Troncons_A7$ID_Troncon[j],"A7",A7_par_pk$Cde[j],A7_par_pk$Cde[j+1],NP2_decompose$KMS[i],NP2_decompose$Year[i],NP2_decompose$Month[i],NP2_decompose$Day[i],NP2_decompose$Hour[i],NP2_decompose$Minute[i],NP2_decompose$Time[i],NP2_decompose$DOW[i],NP2_decompose$WOY[i],1))
      }
    }
    else{ #SENS 2
      for ( j in entree : (sortie+1) ){
        NP2_par_troncons <- rbind(NP2_par_troncons,c(Troncons_A7$ID_Troncon[j-1],"A7",A7_par_pk$Cde[j],A7_par_pk$Cde[j-1],NP2_decompose$KMS[i],NP2_decompose$Year[i],NP2_decompose$Month[i],NP2_decompose$Day[i],NP2_decompose$Hour[i],NP2_decompose$Minute[i],NP2_decompose$Time[i],NP2_decompose$DOW[i],NP2_decompose$WOY[i],2))
      }
    }
  }
  else if ( NP2_decompose$Autoroute[i] == "A8"){
    entree <- match(NP2_decompose$Entr[i],A8_par_pk$Cde)
    sortie <- match(NP2_decompose$Sor[i],A8_par_pk$Cde)
    if ( entree < sortie ) { # SENS 1
      for ( j in entree : (sortie-1) ){
        NP2_par_troncons <- rbind(NP2_par_troncons,c(Troncons_A8$ID_Troncon[j],"A8",A8_par_pk$Cde[j],A8_par_pk$Cde[j+1],NP2_decompose$KMS[i],NP2_decompose$Year[i],NP2_decompose$Month[i],NP2_decompose$Day[i],NP2_decompose$Hour[i],NP2_decompose$Minute[i],NP2_decompose$Time[i],NP2_decompose$DOW[i],NP2_decompose$WOY[i],1))
      }
    }
    else{ #SENS 2
      for ( j in entree : (sortie+1) ){
        NP2_par_troncons <- rbind(NP2_par_troncons,c(Troncons_A8$ID_Troncon[j-1],"A8",A8_par_pk$Cde[j],A8_par_pk$Cde[j-1],NP2_decompose$KMS[i],NP2_decompose$Year[i],NP2_decompose$Month[i],NP2_decompose$Day[i],NP2_decompose$Hour[i],NP2_decompose$Minute[i],NP2_decompose$Time[i],NP2_decompose$DOW[i],NP2_decompose$WOY[i],2))
      }
    }
  }
  else if ( NP2_decompose$Autoroute[i] == "A9"){
    entree <- match(NP2_decompose$Entr[i],A9_par_pk$Cde)
    sortie <- match(NP2_decompose$Sor[i],A9_par_pk$Cde)
    if ( entree < sortie ) { # SENS 1
      for ( j in entree : (sortie-1) ){
        NP2_par_troncons <- rbind(NP2_par_troncons,c(Troncons_A9$ID_Troncon[j],"A9",A9_par_pk$Cde[j],A9_par_pk$Cde[j+1],NP2_decompose$KMS[i],NP2_decompose$Year[i],NP2_decompose$Month[i],NP2_decompose$Day[i],NP2_decompose$Hour[i],NP2_decompose$Minute[i],NP2_decompose$Time[i],NP2_decompose$DOW[i],NP2_decompose$WOY[i],1))
      }
    }
    else{ #SENS 2
      for ( j in entree : (sortie+1) ){
        NP2_par_troncons <- rbind(NP2_par_troncons,c(Troncons_A9$ID_Troncon[j-1],"A9",A9_par_pk$Cde[j],A9_par_pk$Cde[j-1],NP2_decompose$KMS[i],NP2_decompose$Year[i],NP2_decompose$Month[i],NP2_decompose$Day[i],NP2_decompose$Hour[i],NP2_decompose$Minute[i],NP2_decompose$Time[i],NP2_decompose$DOW[i],NP2_decompose$WOY[i],2))
      }
    }
  }
}
NP2_par_troncons <- NP2_par_troncons[-1,]



#Rajouter demi trajet LANCON LA BARQUE
Pointeur <- 1
for (i in 1:nrow(NP2_par_troncons)){
  if (NP2_par_troncons$ID_Troncon[Pointeur] == 20 & NP2_par_troncons$Sens[Pointeur] == 1){ # (A7 -> Lancon)
    newrow1 <- c(21,"A7",25004220,25004278,NP2_par_troncons$KMS[Pointeur],NP2_par_troncons$Year[Pointeur],NP2_par_troncons$Month[Pointeur],NP2_par_troncons$Day[Pointeur],NP2_par_troncons$Hour[Pointeur],NP2_par_troncons$Minute[Pointeur],NP2_par_troncons$Time[Pointeur],NP2_par_troncons$DOW[Pointeur],NP2_par_troncons$WOY[Pointeur],1)
    newrow2 <- c(22,"A7",25004278,25004279,NP2_par_troncons$KMS[Pointeur],NP2_par_troncons$Year[Pointeur],NP2_par_troncons$Month[Pointeur],NP2_par_troncons$Day[Pointeur],NP2_par_troncons$Hour[Pointeur],NP2_par_troncons$Minute[Pointeur],NP2_par_troncons$Time[Pointeur],NP2_par_troncons$DOW[Pointeur],NP2_par_troncons$WOY[Pointeur],1)
    NP2_par_troncons <- rbind(NP2_par_troncons[(1:Pointeur),],newrow1,newrow2,NP2_par_troncons[-(1:Pointeur),])
    Pointeur <- Pointeur + 2
  } 
  if (NP2_par_troncons$ID_Troncon[Pointeur] == 20 & NP2_par_troncons$Sens[Pointeur] == 2){ # (Lancon -> A7)
    newrow1 <- c(21,"A7",25004220,25004278,NP2_par_troncons$KMS[Pointeur],NP2_par_troncons$Year[Pointeur],NP2_par_troncons$Month[Pointeur],NP2_par_troncons$Day[Pointeur],NP2_par_troncons$Hour[Pointeur],NP2_par_troncons$Minute[Pointeur],NP2_par_troncons$Time[Pointeur],NP2_par_troncons$DOW[Pointeur],NP2_par_troncons$WOY[Pointeur],2)
    newrow2 <- c(22,"A7",25004278,25004279,NP2_par_troncons$KMS[Pointeur],NP2_par_troncons$Year[Pointeur],NP2_par_troncons$Month[Pointeur],NP2_par_troncons$Day[Pointeur],NP2_par_troncons$Hour[Pointeur],NP2_par_troncons$Minute[Pointeur],NP2_par_troncons$Time[Pointeur],NP2_par_troncons$DOW[Pointeur],NP2_par_troncons$WOY[Pointeur],2)
    NP2_par_troncons <- rbind(NP2_par_troncons[(1:Pointeur),],newrow1,newrow2,NP2_par_troncons[-(1:Pointeur),])
    Pointeur <- Pointeur + 2
  }
  if (NP2_par_troncons$ID_Troncon[Pointeur] == 26 & NP2_par_troncons$Sens[Pointeur] == 1){ # (La Barque -> A8)
    newrow1 <- c(23,"A8",25004279,25006001,NP2_par_troncons$KMS[Pointeur],NP2_par_troncons$Year[Pointeur],NP2_par_troncons$Month[Pointeur],NP2_par_troncons$Day[Pointeur],NP2_par_troncons$Hour[Pointeur],NP2_par_troncons$Minute[Pointeur],NP2_par_troncons$Time[Pointeur],NP2_par_troncons$DOW[Pointeur],NP2_par_troncons$WOY[Pointeur],1)
    newrow2 <- c(24,"A8",25006001,25006080,NP2_par_troncons$KMS[Pointeur],NP2_par_troncons$Year[Pointeur],NP2_par_troncons$Month[Pointeur],NP2_par_troncons$Day[Pointeur],NP2_par_troncons$Hour[Pointeur],NP2_par_troncons$Minute[Pointeur],NP2_par_troncons$Time[Pointeur],NP2_par_troncons$DOW[Pointeur],NP2_par_troncons$WOY[Pointeur],1)
    newrow3 <- c(25,"A8",25006080,25006002,NP2_par_troncons$KMS[Pointeur],NP2_par_troncons$Year[Pointeur],NP2_par_troncons$Month[Pointeur],NP2_par_troncons$Day[Pointeur],NP2_par_troncons$Hour[Pointeur],NP2_par_troncons$Minute[Pointeur],NP2_par_troncons$Time[Pointeur],NP2_par_troncons$DOW[Pointeur],NP2_par_troncons$WOY[Pointeur],1)
    NP2_par_troncons <- rbind(NP2_par_troncons[(1:Pointeur),],newrow1,newrow2,newrow3,NP2_par_troncons[-(1:Pointeur),])
    Pointeur <- Pointeur + 3
  }
  if (NP2_par_troncons$ID_Troncon[Pointeur] == 26 & NP2_par_troncons$Sens[Pointeur] == 2){ # (A8 -> La Barque)
    newrow1 <- c(23,"A8",25004279,25006001,NP2_par_troncons$KMS[Pointeur],NP2_par_troncons$Year[Pointeur],NP2_par_troncons$Month[Pointeur],NP2_par_troncons$Day[Pointeur],NP2_par_troncons$Hour[Pointeur],NP2_par_troncons$Minute[Pointeur],NP2_par_troncons$Time[Pointeur],NP2_par_troncons$DOW[Pointeur],NP2_par_troncons$WOY[Pointeur],2)
    newrow2 <- c(24,"A8",25006001,25006080,NP2_par_troncons$KMS[Pointeur],NP2_par_troncons$Year[Pointeur],NP2_par_troncons$Month[Pointeur],NP2_par_troncons$Day[Pointeur],NP2_par_troncons$Hour[Pointeur],NP2_par_troncons$Minute[Pointeur],NP2_par_troncons$Time[Pointeur],NP2_par_troncons$DOW[Pointeur],NP2_par_troncons$WOY[Pointeur],2)
    newrow3 <- c(25,"A8",25006080,25006002,NP2_par_troncons$KMS[Pointeur],NP2_par_troncons$Year[Pointeur],NP2_par_troncons$Month[Pointeur],NP2_par_troncons$Day[Pointeur],NP2_par_troncons$Hour[Pointeur],NP2_par_troncons$Minute[Pointeur],NP2_par_troncons$Time[Pointeur],NP2_par_troncons$DOW[Pointeur],NP2_par_troncons$WOY[Pointeur],2)
    NP2_par_troncons <- rbind(NP2_par_troncons[(1:Pointeur),],newrow1,newrow2,newrow3,NP2_par_troncons[-(1:Pointeur),])
    Pointeur <- Pointeur + 3
  }
  Pointeur <- Pointeur + 1
}

#COMPTAGE DES TRAJETS PAR TRONCON PAR SENS PAR DOW

CompteurFF <- vector(mode= "integer" , length = 75)
CompteurNP <- vector(mode= "integer" , length = 75)
CompteurFF_sens1 <- vector(mode= "integer" , length = 75)
CompteurFF_sens2 <- vector(mode= "integer" , length = 75)
CompteurNP_sens1 <- vector(mode= "integer" , length = 75)
CompteurNP_sens2 <- vector(mode= "integer" , length = 75)
CompteurFF_sens1_lundi <- vector(mode= "integer" , length = 75)
CompteurFF_sens2_lundi <- vector(mode= "integer" , length = 75)
CompteurFF_sens1_mardi <- vector(mode= "integer" , length = 75)
CompteurFF_sens2_mardi <- vector(mode= "integer" , length = 75)
CompteurFF_sens1_mercredi <- vector(mode= "integer" , length = 75)
CompteurFF_sens2_mercredi <- vector(mode= "integer" , length = 75)
CompteurFF_sens1_jeudi <- vector(mode= "integer" , length = 75)
CompteurFF_sens2_jeudi <- vector(mode= "integer" , length = 75)
CompteurFF_sens1_vendredi <- vector(mode= "integer" , length = 75)
CompteurFF_sens2_vendredi <- vector(mode= "integer" , length = 75)
CompteurFF_sens1_samedi <- vector(mode= "integer" , length = 75)
CompteurFF_sens2_samedi <- vector(mode= "integer" , length = 75)
CompteurFF_sens1_dimanche <- vector(mode= "integer" , length = 75)
CompteurFF_sens2_dimanche <- vector(mode= "integer" , length = 75)
CompteurNP_sens1_lundi <- vector(mode= "integer" , length = 75)
CompteurNP_sens2_lundi <- vector(mode= "integer" , length = 75)
CompteurNP_sens1_mardi <- vector(mode= "integer" , length = 75)
CompteurNP_sens2_mardi <- vector(mode= "integer" , length = 75)
CompteurNP_sens1_mercredi <- vector(mode= "integer" , length = 75)
CompteurNP_sens2_mercredi <- vector(mode= "integer" , length = 75)
CompteurNP_sens1_jeudi <- vector(mode= "integer" , length = 75)
CompteurNP_sens2_jeudi <- vector(mode= "integer" , length = 75)
CompteurNP_sens1_vendredi <- vector(mode= "integer" , length = 75)
CompteurNP_sens2_vendredi <- vector(mode= "integer" , length = 75)
CompteurNP_sens1_samedi <- vector(mode= "integer" , length = 75)
CompteurNP_sens2_samedi <- vector(mode= "integer" , length = 75)
CompteurNP_sens1_dimanche <- vector(mode= "integer" , length = 75)
CompteurNP_sens2_dimanche <- vector(mode= "integer" , length = 75)
for ( j in 1 : 75 ){
  CompteurFF_sens1_lundi[j] <- sum( FF2_par_troncons$DOW == 1 & FF2_par_troncons$ID_Troncon == j & FF2_par_troncons$Sens == 1 )
  CompteurFF_sens2_lundi[j] <- sum( FF2_par_troncons$DOW == 1 & FF2_par_troncons$ID_Troncon == j & FF2_par_troncons$Sens == 2 )
  CompteurFF_sens1_mardi[j] <- sum( FF2_par_troncons$DOW == 2 & FF2_par_troncons$ID_Troncon == j & FF2_par_troncons$Sens == 1 )
  CompteurFF_sens2_mardi[j] <- sum( FF2_par_troncons$DOW == 2 & FF2_par_troncons$ID_Troncon == j & FF2_par_troncons$Sens == 2 )
  CompteurFF_sens1_mercredi[j] <- sum( FF2_par_troncons$DOW == 3 & FF2_par_troncons$ID_Troncon == j & FF2_par_troncons$Sens == 1 )
  CompteurFF_sens2_mercredi[j] <- sum( FF2_par_troncons$DOW == 3 & FF2_par_troncons$ID_Troncon == j & FF2_par_troncons$Sens == 2 )
  CompteurFF_sens1_jeudi[j] <- sum( FF2_par_troncons$DOW == 4 & FF2_par_troncons$ID_Troncon == j & FF2_par_troncons$Sens == 1 )
  CompteurFF_sens2_jeudi[j] <- sum( FF2_par_troncons$DOW == 4 & FF2_par_troncons$ID_Troncon == j & FF2_par_troncons$Sens == 2 )
  CompteurFF_sens1_vendredi[j] <- sum( FF2_par_troncons$DOW == 5 & FF2_par_troncons$ID_Troncon == j & FF2_par_troncons$Sens == 1 )
  CompteurFF_sens2_vendredi[j] <- sum( FF2_par_troncons$DOW == 5 & FF2_par_troncons$ID_Troncon == j & FF2_par_troncons$Sens == 2 )
  CompteurFF_sens1_samedi[j] <- sum( FF2_par_troncons$DOW == 6 & FF2_par_troncons$ID_Troncon == j & FF2_par_troncons$Sens == 1 )
  CompteurFF_sens2_samedi[j] <- sum( FF2_par_troncons$DOW == 6 & FF2_par_troncons$ID_Troncon == j & FF2_par_troncons$Sens == 2 )
  CompteurFF_sens1_dimanche[j] <- sum( FF2_par_troncons$DOW == 7 & FF2_par_troncons$ID_Troncon == j & FF2_par_troncons$Sens == 1 )
  CompteurFF_sens2_dimanche[j] <- sum( FF2_par_troncons$DOW == 7 & FF2_par_troncons$ID_Troncon == j & FF2_par_troncons$Sens == 2 )
  CompteurNP[j] <- sum( NP2_par_troncons$ID_Troncon == j )
  CompteurFF[j] <- sum( FF2_par_troncons$ID_Troncon == j )
  CompteurNP_sens1[j] <- sum( NP2_par_troncons$ID_Troncon == j & NP2_par_troncons == 1)
  CompteurNP_sens2[j] <- sum( NP2_par_troncons$ID_Troncon == j & NP2_par_troncons == 2)
  CompteurFF_sens1[j] <- sum( FF2_par_troncons$ID_Troncon == j & FF2_par_troncons == 1)
  CompteurFF_sens2[j] <- sum( FF2_par_troncons$ID_Troncon == j & FF2_par_troncons == 2)
}

ComptageFFNP <- cbind(Troncons_A789,CompteurFF,CompteurFF)

ggplot(ComptageFFNP) + geom_point(aes(Début_Lng,Début_Lat)) +
  geom_segment(aes(x = Début_Lng, xend =Fin_Lng, y = Début_Lat, yend = Fin_Lat, size = CompteurNP)) +
  scale_size(range = c(0, 10))



#DECISION AUTOMATIQUE DES TRONCONS FREQUENTS

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

# Dérivée
max_des_troncons_FF <- max(CompteurFF)
CritereFF <- max_des_troncons_FF * 7 / 10
FF_troncons_selection <- NULL
max_des_troncons_NP <- max(CompteurNP)
CritereNP <- max_des_troncons_NP * 7 / 10
NP_troncons_selection <- NULL
vecteur_derive_NP <- NULL
vecteur_derive_FF <- NULL
if ( CompteurNP[1] > CritereNP){
  vecteur_derive_NP <- 1
}
for (i in 2 : 75){
  if( abs(CompteurNP[i-1]-CompteurNP[i]) > CritereNP){
    vecteur_derive_NP <- c(vecteur_derive_NP,i)
  }
}
if ( CompteurNP[75] > CritereNP){
  vecteur_derive_NP <- c(vecteur_derive_NP,75)
}
if ( CompteurFF[1] > CritereFF){
  vecteur_derive_FF <- 1
}
for (i in 2 : 75){
  if( abs(CompteurFF[i-1]-CompteurFF[i]) > CritereFF){
    vecteur_derive_FF <- c(vecteur_derive_FF,i)
  }
}
if ( CompteurFF[75] > CritereFF){
  vecteur_derive_FF <- c(vecteur_derive_FF,75)
}

#Troncons restants
CompteurFF_restant <- vector(mode = "integer", length = 75)
CompteurNP_restant <- vector(mode = "integer", length = 75)
for (i in 1:75){
  if ( (i %in% FF_troncons_selection) == FALSE){
    CompteurFF_restant[i] <- CompteurFF[i]
  }
  if ( (i %in% NP_troncons_selection) == FALSE){
    CompteurNP_restant[i] <- CompteurNP[i]
  }
}
#Deuxième fois
max_des_troncons_FF <- max(CompteurFF_restant)
CritereFF <- max_des_troncons_FF * 7 / 10
FF_troncons_selection <- NULL
max_des_troncons_NP <- max(CompteurNP_restant)
CritereNP <- max_des_troncons_NP * 7 / 10
NP_troncons_selection <- NULL
for (i in 1 : 75){
  if (CompteurFF_restant[i] >= CritereFF){
    FF_troncons_selection <- c(FF_troncons_selection,i)
  }
  if (CompteurNP_restant[i] >= CritereNP){
    NP_troncons_selection <- c(NP_troncons_selection,i)
  }
}
