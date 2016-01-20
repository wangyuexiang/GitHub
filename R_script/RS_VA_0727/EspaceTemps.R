# OD -> Espace -> Temps 
VIP2 <- VIP2[VIP2$ID %in% c("CC","FF", "NP", "PC") & VIP2$Date > as.Date("2014-12-31") & VIP2$Date < as.Date("2015-5-29"), ]
x <- vector(mode = "integer", length = nrow(VIP2))


VIP2 <- cbind(VIP2[,1:3],x,x,x,x,x,x,VIP2$TimeEntr,VIP2$TimeSor,VIP2$DOW,VIP2$WOY,VIP2$Date)
colnames(VIP2)<- c(colnames(MODELE),"Date")

VIP2_pour_modele <- VIP2[VIP2$Date < as.Date("2015-5-1"),]
VIP2_pour_test <- VIP2[VIP2$Date >= as.Date("2015-5-1"),]



#DECOMPOSER LES TRAJETS PAR AUTOROUTE UNIQUE
#Et traiter le système ouvert

temp$Autoroute <- as.character(temp$Autoroute)
VIP2_pour_modele$Entr <- as.numeric(as.character(VIP2_pour_modele$Entr))
VIP2_pour_modele$Sor <- as.numeric(as.character(VIP2_pour_modele$Sor))
Autoroute <- vector(mode = "character", length = nrow(VIP2_pour_modele))
VIP2_pour_modele_decompose <- cbind(VIP2_pour_modele, Autoroute)
VIP2_pour_modele_decompose$Autoroute <- as.character(VIP2_pour_modele_decompose$Autoroute)
VIP2_pour_modele_restant <- VIP2_pour_modele
Pointeur <- 1
Pointeur_restant <- 1
Gare_inconnue <- 0 #Gares du système ouvert que le code ne connait pas
for (i in 1 : nrow(VIP2_pour_modele)){
  if (  ( (VIP2_pour_modele$Entr[i] %in% Troncons_A789[,4]) | (VIP2_pour_modele$Entr[i] %in% Troncons_A789[,6])  ) & 
        ( (VIP2_pour_modele$Sor[i] %in% Troncons_A789[,4]) | (VIP2_pour_modele$Sor[i] %in% Troncons_A789[,6])  )  ) {  # SI TRAJET DANS LES TROIS AUTOROUTES
    if (temp$Autoroute[match(VIP2_pour_modele$Entr[i],temp$Cde)] !=  temp$Autoroute[match(VIP2_pour_modele$Sor[i],temp$Cde)] ) { #E=A7etS=A9 ou l'inverse
      if (VIP2_pour_modele$Entr[i] != 25004210 & VIP2_pour_modele$Sor[i] != 25004210){  # Entrée et sortie <> Orange Centre
        if(temp$Autoroute[match(VIP2_pour_modele$Entr[i],temp$Cde)] == "A7"){# E=A7 S=A9
          newrow1 <- c(VIP2_pour_modele[i,1],VIP2_pour_modele$Entr[i],25004210,VIP2_pour_modele$KMS[i],VIP2_pour_modele$Year[i],VIP2_pour_modele$Month[i],VIP2_pour_modele$Day[i],VIP2_pour_modele$Hour[i],VIP2_pour_modele$Minute[i],VIP2_pour_modele$TimeEntr[i],VIP2_pour_modele$TimeSor[i],VIP2_pour_modele$DOW[i],VIP2_pour_modele$WOY[i],VIP2_pour_modele$Voie[i],"A7")
          newrow2 <- c(VIP2_pour_modele[i,1],25004210,VIP2_pour_modele$Sor[i],VIP2_pour_modele$KMS[i],VIP2_pour_modele$Year[i],VIP2_pour_modele$Month[i],VIP2_pour_modele$Day[i],VIP2_pour_modele$Hour[i],VIP2_pour_modele$Minute[i],VIP2_pour_modele$TimeEntr[i],VIP2_pour_modele$TimeSor[i],VIP2_pour_modele$DOW[i],VIP2_pour_modele$WOY[i],VIP2_pour_modele$Voie[i],"A9")
          VIP2_pour_modele_decompose = rbind(VIP2_pour_modele_decompose[1:Pointeur,],newrow1,newrow2,VIP2_pour_modele_decompose[-(1:Pointeur),])
          Pointeur <- Pointeur +2
        }
        else if(temp$Autoroute[match(VIP2_pour_modele$Entr[i],temp$Cde)] == "A9"){# E=A9 S=A7
          newrow1 <- c(VIP2_pour_modele[i,1],VIP2_pour_modele$Entr[i],25004210,VIP2_pour_modele$KMS[i],VIP2_pour_modele$Year[i],VIP2_pour_modele$Month[i],VIP2_pour_modele$Day[i],VIP2_pour_modele$Hour[i],VIP2_pour_modele$Minute[i],VIP2_pour_modele$TimeEntr[i],VIP2_pour_modele$TimeSor[i],VIP2_pour_modele$DOW[i],VIP2_pour_modele$WOY[i],VIP2_pour_modele$Voie[i],"A9")
          newrow2 <- c(VIP2_pour_modele[i,1],25004210,VIP2_pour_modele$Sor[i],VIP2_pour_modele$KMS[i],VIP2_pour_modele$Year[i],VIP2_pour_modele$Month[i],VIP2_pour_modele$Day[i],VIP2_pour_modele$Hour[i],VIP2_pour_modele$Minute[i],VIP2_pour_modele$TimeEntr[i],VIP2_pour_modele$TimeSor[i],VIP2_pour_modele$DOW[i],VIP2_pour_modele$WOY[i],VIP2_pour_modele$Voie[i],"A7")
          VIP2_pour_modele_decompose = rbind(VIP2_pour_modele_decompose[1:Pointeur,],newrow1,newrow2,VIP2_pour_modele_decompose[-(1:Pointeur),])
          Pointeur <- Pointeur +2
        }
      }
      else { # E=A9 S=OC ou l'inverse
        VIP2_pour_modele_decompose$Autoroute[Pointeur] <- "A9"
      }
    }
    else {
      VIP2_pour_modele_decompose$Autoroute[Pointeur] <- temp$Autoroute[match(VIP2_pour_modele$Entr[i],temp$Cde)]
    }
    if (Pointeur_restant == 1){
      VIP2_pour_modele_restant <- VIP2_pour_modele_restant[2:nrow(VIP2_pour_modele_restant),]
    }
    else {
      VIP2_pour_modele_restant <- rbind(VIP2_pour_modele_restant[(1:(Pointeur_restant-1)),],VIP2_pour_modele_restant[-(1:(Pointeur_restant)),])
      Pointeur_restant <- Pointeur_restant - 1
    }
  }
  Pointeur <- Pointeur +1
  Pointeur_restant <- Pointeur_restant +1
}


VIP2_pour_modele_decompose <- VIP2_pour_modele_decompose[VIP2_pour_modele_decompose$Autoroute > 0,]






#DECOMPOSER LES OD PAR TRONCONS :
VIP2_pour_modele_par_troncons <- data.frame(ID = "", ID_Troncon= 0, Autoroute= "", Entr= 0, Sor=0, KMS=0, Date = 0, TimeEntr = 0, TimeSor =0, DOW=0, WOY=0, Sens = 0)
VIP2_pour_modele_par_troncons$Autoroute <- as.character(VIP2_pour_modele_par_troncons$Autoroute)
VIP2_pour_modele_par_troncons$ID <- as.character(VIP2_pour_modele_par_troncons$ID)
VIP2_pour_modele_decompose$Date <- as.character(VIP2_pour_modele_decompose$Date)
VIP2_pour_modele_par_troncons$Date <- as.character(VIP2_pour_modele_par_troncons$Date)

for (i in 1:nrow(VIP2_pour_modele_decompose)){
  if ( VIP2_pour_modele_decompose$Autoroute[i] == "A7"){
    entree <- match(VIP2_pour_modele_decompose$Entr[i],A7_par_pk$Cde)
    sortie <- match(VIP2_pour_modele_decompose$Sor[i],A7_par_pk$Cde)
    if ( entree < sortie ) { # SENS 1
      for ( j in entree : (sortie-1) ){
        VIP2_pour_modele_par_troncons <- rbind(VIP2_pour_modele_par_troncons,c(VIP2_pour_modele_decompose$ID[i],Troncons_A7$ID_Troncon[j],"A7",A7_par_pk$Cde[j],A7_par_pk$Cde[j+1],VIP2_pour_modele_decompose$KMS[i],VIP2_pour_modele_decompose$Date[i],VIP2_pour_modele_decompose$TimeEntr[i],VIP2_pour_modele_decompose$TimeSor[i],VIP2_pour_modele_decompose$DOW[i],VIP2_pour_modele_decompose$WOY[i],1))
      }
    }
    else{ #SENS 2
      for ( j in entree : (sortie+1) ){
        VIP2_pour_modele_par_troncons <- rbind(VIP2_pour_modele_par_troncons,c(VIP2_pour_modele_decompose$ID[i],Troncons_A7$ID_Troncon[j-1],"A7",A7_par_pk$Cde[j],A7_par_pk$Cde[j-1],VIP2_pour_modele_decompose$KMS[i],VIP2_pour_modele_decompose$Date[i],VIP2_pour_modele_decompose$TimeEntr[i],VIP2_pour_modele_decompose$TimeSor[i],VIP2_pour_modele_decompose$DOW[i],VIP2_pour_modele_decompose$WOY[i],2))
      }
    }
  }
  else if ( VIP2_pour_modele_decompose$Autoroute[i] == "A8"){
    entree <- match(VIP2_pour_modele_decompose$Entr[i],A8_par_pk$Cde)
    sortie <- match(VIP2_pour_modele_decompose$Sor[i],A8_par_pk$Cde)
    if ( entree < sortie ) { # SENS 1
      for ( j in entree : (sortie-1) ){
        VIP2_pour_modele_par_troncons <- rbind(VIP2_pour_modele_par_troncons,c(VIP2_pour_modele_decompose$ID[i],Troncons_A8$ID_Troncon[j],"A8",A8_par_pk$Cde[j],A8_par_pk$Cde[j+1],VIP2_pour_modele_decompose$KMS[i],VIP2_pour_modele_decompose$Date[i],VIP2_pour_modele_decompose$TimeEntr[i],VIP2_pour_modele_decompose$TimeSor[i],VIP2_pour_modele_decompose$DOW[i],VIP2_pour_modele_decompose$WOY[i],1))
      }
    }
    else{ #SENS 2
      for ( j in entree : (sortie+1) ){
        VIP2_pour_modele_par_troncons <- rbind(VIP2_pour_modele_par_troncons,c(VIP2_pour_modele_decompose$ID[i],Troncons_A8$ID_Troncon[j-1],"A8",A8_par_pk$Cde[j],A8_par_pk$Cde[j-1],VIP2_pour_modele_decompose$KMS[i],VIP2_pour_modele_decompose$Date[i],VIP2_pour_modele_decompose$TimeEntr[i],VIP2_pour_modele_decompose$TimeSor[i],VIP2_pour_modele_decompose$DOW[i],VIP2_pour_modele_decompose$WOY[i],2))
      }
    }
  }
  else if ( VIP2_pour_modele_decompose$Autoroute[i] == "A9"){
    entree <- match(VIP2_pour_modele_decompose$Entr[i],A9_par_pk$Cde)
    sortie <- match(VIP2_pour_modele_decompose$Sor[i],A9_par_pk$Cde)
    if ( entree < sortie ) { # SENS 1
      for ( j in entree : (sortie-1) ){
        VIP2_pour_modele_par_troncons <- rbind(VIP2_pour_modele_par_troncons,c(VIP2_pour_modele_decompose$ID[i],Troncons_A9$ID_Troncon[j],"A9",A9_par_pk$Cde[j],A9_par_pk$Cde[j+1],VIP2_pour_modele_decompose$KMS[i],VIP2_pour_modele_decompose$Date[i],VIP2_pour_modele_decompose$TimeEntr[i],VIP2_pour_modele_decompose$TimeSor[i],VIP2_pour_modele_decompose$DOW[i],VIP2_pour_modele_decompose$WOY[i],1))
      }
    }
    else{ #SENS 2
      for ( j in entree : (sortie+1) ){
        VIP2_pour_modele_par_troncons <- rbind(VIP2_pour_modele_par_troncons,c(VIP2_pour_modele_decompose$ID[i],Troncons_A9$ID_Troncon[j-1],"A9",A9_par_pk$Cde[j],A9_par_pk$Cde[j-1],VIP2_pour_modele_decompose$KMS[i],VIP2_pour_modele_decompose$Date[i],VIP2_pour_modele_decompose$TimeEntr[i],VIP2_pour_modele_decompose$TimeSor[i],VIP2_pour_modele_decompose$DOW[i],VIP2_pour_modele_decompose$WOY[i],2))
      }
    }
  }
}
VIP2_pour_modele_par_troncons <- VIP2_pour_modele_par_troncons[-1,]


#Rajouter demi trajet LANCON LA BARQUE
Pointeur <- 1
for (i in 1:nrow(VIP2_pour_modele_par_troncons)){
  if (VIP2_pour_modele_par_troncons$ID_Troncon[Pointeur] == 20 & VIP2_pour_modele_par_troncons$Sens[Pointeur] == 1){ # (A7 -> Lancon)
    newrow1 <- c(VIP2_pour_modele_par_troncons$ID[Pointeur],21,"A7",25004220,25004278,VIP2_pour_modele_par_troncons$KMS[Pointeur],VIP2_pour_modele_par_troncons$Date[Pointeur],VIP2_pour_modele_par_troncons$TimeEntr[Pointeur],VIP2_pour_modele_par_troncons$TimeSor[Pointeur],VIP2_pour_modele_par_troncons$DOW[Pointeur],VIP2_pour_modele_par_troncons$WOY[Pointeur],1)
    newrow2 <- c(VIP2_pour_modele_par_troncons$ID[Pointeur],22,"A7",25004278,25004279,VIP2_pour_modele_par_troncons$KMS[Pointeur],VIP2_pour_modele_par_troncons$Date[Pointeur],VIP2_pour_modele_par_troncons$TimeEntr[Pointeur],VIP2_pour_modele_par_troncons$TimeSor[Pointeur],VIP2_pour_modele_par_troncons$DOW[Pointeur],VIP2_pour_modele_par_troncons$WOY[Pointeur],1)
    VIP2_pour_modele_par_troncons <- rbind(VIP2_pour_modele_par_troncons[(1:Pointeur),],newrow1,newrow2,VIP2_pour_modele_par_troncons[-(1:Pointeur),])
    Pointeur <- Pointeur + 2
  } 
  if (VIP2_pour_modele_par_troncons$ID_Troncon[Pointeur] == 20 & VIP2_pour_modele_par_troncons$Sens[Pointeur] == 2){ # (Lancon -> A7)
    newrow1 <- c(VIP2_pour_modele_par_troncons$ID[Pointeur],21,"A7",25004220,25004278,VIP2_pour_modele_par_troncons$KMS[Pointeur],VIP2_pour_modele_par_troncons$Date[Pointeur],VIP2_pour_modele_par_troncons$TimeEntr[Pointeur],VIP2_pour_modele_par_troncons$TimeSor[Pointeur],VIP2_pour_modele_par_troncons$DOW[Pointeur],VIP2_pour_modele_par_troncons$WOY[Pointeur],2)
    newrow2 <- c(VIP2_pour_modele_par_troncons$ID[Pointeur],22,"A7",25004278,25004279,VIP2_pour_modele_par_troncons$KMS[Pointeur],VIP2_pour_modele_par_troncons$Date[Pointeur],VIP2_pour_modele_par_troncons$TimeEntr[Pointeur],VIP2_pour_modele_par_troncons$TimeSor[Pointeur],VIP2_pour_modele_par_troncons$DOW[Pointeur],VIP2_pour_modele_par_troncons$WOY[Pointeur],2)
    VIP2_pour_modele_par_troncons <- rbind(VIP2_pour_modele_par_troncons[(1:Pointeur),],newrow1,newrow2,VIP2_pour_modele_par_troncons[-(1:Pointeur),])
    Pointeur <- Pointeur + 2
  }
  if (VIP2_pour_modele_par_troncons$ID_Troncon[Pointeur] == 26 & VIP2_pour_modele_par_troncons$Sens[Pointeur] == 1){ # (La Barque -> A8)
    newrow1 <- c(VIP2_pour_modele_par_troncons$ID[Pointeur],23,"A8",25004279,25006001,VIP2_pour_modele_par_troncons$KMS[Pointeur],VIP2_pour_modele_par_troncons$Date[Pointeur],VIP2_pour_modele_par_troncons$TimeEntr[Pointeur],VIP2_pour_modele_par_troncons$TimeSor[Pointeur],VIP2_pour_modele_par_troncons$DOW[Pointeur],VIP2_pour_modele_par_troncons$WOY[Pointeur],1)
    newrow2 <- c(VIP2_pour_modele_par_troncons$ID[Pointeur],24,"A8",25006001,25006080,VIP2_pour_modele_par_troncons$KMS[Pointeur],VIP2_pour_modele_par_troncons$Date[Pointeur],VIP2_pour_modele_par_troncons$TimeEntr[Pointeur],VIP2_pour_modele_par_troncons$TimeSor[Pointeur],VIP2_pour_modele_par_troncons$DOW[Pointeur],VIP2_pour_modele_par_troncons$WOY[Pointeur],1)
    newrow3 <- c(VIP2_pour_modele_par_troncons$ID[Pointeur],25,"A8",25006080,25006002,VIP2_pour_modele_par_troncons$KMS[Pointeur],VIP2_pour_modele_par_troncons$Date[Pointeur],VIP2_pour_modele_par_troncons$TimeEntr[Pointeur],VIP2_pour_modele_par_troncons$TimeSor[Pointeur],VIP2_pour_modele_par_troncons$DOW[Pointeur],VIP2_pour_modele_par_troncons$WOY[Pointeur],1)
    VIP2_pour_modele_par_troncons <- rbind(VIP2_pour_modele_par_troncons[(1:Pointeur),],newrow1,newrow2,newrow3,VIP2_pour_modele_par_troncons[-(1:Pointeur),])
    Pointeur <- Pointeur + 3
  }
  if (VIP2_pour_modele_par_troncons$ID_Troncon[Pointeur] == 26 & VIP2_pour_modele_par_troncons$Sens[Pointeur] == 2){ # (A8 -> La Barque)
    newrow1 <- c(VIP2_pour_modele_par_troncons$ID[Pointeur],23,"A8",25004279,25006001,VIP2_pour_modele_par_troncons$KMS[Pointeur],VIP2_pour_modele_par_troncons$Date[Pointeur],VIP2_pour_modele_par_troncons$TimeEntr[Pointeur],VIP2_pour_modele_par_troncons$TimeSor[Pointeur],VIP2_pour_modele_par_troncons$DOW[Pointeur],VIP2_pour_modele_par_troncons$WOY[Pointeur],2)
    newrow2 <- c(VIP2_pour_modele_par_troncons$ID[Pointeur],24,"A8",25006001,25006080,VIP2_pour_modele_par_troncons$KMS[Pointeur],VIP2_pour_modele_par_troncons$Date[Pointeur],VIP2_pour_modele_par_troncons$TimeEntr[Pointeur],VIP2_pour_modele_par_troncons$TimeSor[Pointeur],VIP2_pour_modele_par_troncons$DOW[Pointeur],VIP2_pour_modele_par_troncons$WOY[Pointeur],2)
    newrow3 <- c(VIP2_pour_modele_par_troncons$ID[Pointeur],25,"A8",25006080,25006002,VIP2_pour_modele_par_troncons$KMS[Pointeur],VIP2_pour_modele_par_troncons$Date[Pointeur],VIP2_pour_modele_par_troncons$TimeEntr[Pointeur],VIP2_pour_modele_par_troncons$TimeSor[Pointeur],VIP2_pour_modele_par_troncons$DOW[Pointeur],VIP2_pour_modele_par_troncons$WOY[Pointeur],2)
    VIP2_pour_modele_par_troncons <- rbind(VIP2_pour_modele_par_troncons[(1:Pointeur),],newrow1,newrow2,newrow3,VIP2_pour_modele_par_troncons[-(1:Pointeur),])
    Pointeur <- Pointeur + 3
  }
  Pointeur <- Pointeur + 1
}

#COMPTAGE DES TRAJETS PAR TRONCON PAR SENS PAR DOW
CompteurNP <- vector(mode= "integer" , length = 75)
CompteurPC <- vector(mode= "integer" , length = 75)
CompteurFF <- vector(mode= "integer" , length = 75)
CompteurCC <- vector(mode= "integer" , length = 75)

for ( j in 1 : 75 ){
  CompteurNP[j] <- sum(VIP2_pour_modele_par_troncons$ID == "NP" & VIP2_pour_modele_par_troncons$ID_Troncon == j)
  CompteurPC[j] <- sum(VIP2_pour_modele_par_troncons$ID == "PC" & VIP2_pour_modele_par_troncons$ID_Troncon == j)
  CompteurFF[j] <- sum(VIP2_pour_modele_par_troncons$ID == "FF" & VIP2_pour_modele_par_troncons$ID_Troncon == j)
  CompteurCC[j] <- sum(VIP2_pour_modele_par_troncons$ID == "CC" & VIP2_pour_modele_par_troncons$ID_Troncon == j)
}

ggplot(ComptageFFNP) + geom_point(aes(Début_Lng,Début_Lat)) +
  geom_segment(aes(x = Début_Lng, xend =Fin_Lng, y = Début_Lat, yend = Fin_Lat, size = CompteurCC)) +
  scale_size(range = c(0, 10))

#DECISION AUTOMATIQUE DES TRONCONS FREQUENTS


# [Pourcentagedumax max ; max ]
Pourcentagedumax <- 0.56

max_des_troncons_FF <- max(CompteurFF)
CritereFF <- max_des_troncons_FF * Pourcentagedumax 
FF_troncons_selection <- NULL
max_des_troncons_NP <- max(CompteurNP)
CritereNP <- max_des_troncons_NP * Pourcentagedumax 
NP_troncons_selection <- NULL
max_des_troncons_CC <- max(CompteurCC)
CritereCC <- max_des_troncons_CC * Pourcentagedumax 
CC_troncons_selection <- NULL
max_des_troncons_PC <- max(CompteurPC)
CriterePC <- max_des_troncons_PC * Pourcentagedumax 
PC_troncons_selection <- NULL
for (i in 1 : 75){
  if (CompteurFF[i] >= CritereFF){
    FF_troncons_selection <- c(FF_troncons_selection,i)
  }
  if (CompteurNP[i] >= CritereNP){
    NP_troncons_selection <- c(NP_troncons_selection,i)
  }
  if (CompteurCC[i] >= CritereCC){
    CC_troncons_selection <- c(CC_troncons_selection,i)
  }
  if (CompteurPC[i] >= CriterePC){
    PC_troncons_selection <- c(PC_troncons_selection,i)
  }
}


VIP2_espace <- VIP2_pour_modele_par_troncons[(VIP2_pour_modele_par_troncons$ID == "NP" & VIP2_pour_modele_par_troncons$ID_Troncon %in% NP_troncons_selection)|(VIP2_pour_modele_par_troncons$ID == "FF" & VIP2_pour_modele_par_troncons$ID_Troncon %in% FF_troncons_selection)|(VIP2_pour_modele_par_troncons$ID == "CC" & VIP2_pour_modele_par_troncons$ID_Troncon %in% CC_troncons_selection)|(VIP2_pour_modele_par_troncons$ID == "PC" & VIP2_pour_modele_par_troncons$ID_Troncon %in% PC_troncons_selection),]





# Kmeans 
temp2 <- VIP2_espace
Nombre_de_clusters_NP <- vector( mode = "integer" , length = 75)
Nombre_de_clusters_FF <- vector( mode = "integer" , length = 75)
Nombre_de_clusters_CC <- vector( mode = "integer" , length = 75)
Nombre_de_clusters_PC <- vector( mode = "integer" , length = 75)
NP.kmeans <- NULL
FF.kmeans <- NULL
CC.kmeans <- NULL
PC.kmeans <- NULL
set.seed(1234)
for (j in 1:75){ #Clustering pour chaque troncon de chaque personne
#  for (k in 0:6){
  if (j %in% NP_troncons_selection){
   # if (length(temp2[temp2$ID == "NP" & temp2$ID_Troncon == j & temp2$DOW == k, "TimeEntr"]) > 5)
    Nombre_de_clusters_NP[j] <- 2 #Pour le moment choix abritraire du nombre de cluster
    NP.kmeans<- c(NP.kmeans,list(kmeans(temp2[temp2$ID == "NP" & temp2$ID_Troncon == j, "TimeEntr"], centers = Nombre_de_clusters_NP[j])))
    #lapply(t.kmeans, function(z) z$withinss)
    #within.ss <- sapply(t.kmeans, function(z) sum(z$withinss))
  }
  if (j %in% FF_troncons_selection){
    Nombre_de_clusters_FF[j] <- 2 #Pour le moment choix abritraire du nombre de cluster
    FF.kmeans<- c(FF.kmeans,list(kmeans(temp2[temp2$ID == "FF" & temp2$ID_Troncon == j, "TimeEntr"], centers = Nombre_de_clusters_FF[j])))
    #lapply(t.kmeans, function(z) z$withinss)
    #within.ss <- sapply(t.kmeans, function(z) sum(z$withinss))
  }
  if (j %in% CC_troncons_selection){
    Nombre_de_clusters_CC[j] <- 3 #Pour le moment choix abritraire du nombre de cluster
    CC.kmeans<- c(CC.kmeans,list(kmeans(temp2[temp2$ID == "CC" & temp2$ID_Troncon == j, "TimeEntr"], centers = Nombre_de_clusters_CC[j])))
    #lapply(t.kmeans, function(z) z$withinss)
    #within.ss <- sapply(t.kmeans, function(z) sum(z$withinss))
  }
  if (j %in% PC_troncons_selection){
    Nombre_de_clusters_PC[j] <- 2 #Pour le moment choix abritraire du nombre de cluster
    PC.kmeans<- c(PC.kmeans,list(kmeans(temp2[temp2$ID == "PC" & temp2$ID_Troncon == j, "TimeEntr"], centers = Nombre_de_clusters_PC[j])))
    #lapply(t.kmeans, function(z) z$withinss)
    #within.ss <- sapply(t.kmeans, function(z) sum(z$withinss))
 # }
  }
}

#Maintenant on a toutes les infos, création de la dataframe finale XX_espace_temps
NP_espace_temps <- data.frame(ID_Troncon = 0, Autoroute = "", Time = 0, Standard_Deviation = 0, Tmin = 0, Tmax = 0)
NP_espace_temps$Autoroute <- as.character(NP_espace_temps$Autoroute)
FF_espace_temps <- data.frame(ID_Troncon = 0, Autoroute = "", Time = 0, Standard_Deviation = 0, Tmin = 0, Tmax = 0)
FF_espace_temps$Autoroute <- as.character(FF_espace_temps$Autoroute)
CC_espace_temps <- data.frame(ID_Troncon = 0, Autoroute = "", Time = 0, Standard_Deviation = 0, Tmin = 0, Tmax = 0)
CC_espace_temps$Autoroute <- as.character(CC_espace_temps$Autoroute)
PC_espace_temps <- data.frame(ID_Troncon = 0, Autoroute = "", Time = 0, Standard_Deviation = 0, Tmin = 0, Tmax = 0)
PC_espace_temps$Autoroute <- as.character(PC_espace_temps$Autoroute)
Troncons_A789$Autoroute <- as.character(Troncons_A789$Autoroute)
PointeurNP <- 1 #Va parcourir la liste NP.kmeans, chaque élément étant le kmeans d'un troncon
PointeurFF <- 1
PointeurCC <- 1
PointeurPC <- 1
for (i in 1:75){ # Pour les  75 troncons
  if (i %in% NP_troncons_selection){ # Si c'est un des troncons en question
    for (j in 1:Nombre_de_clusters_NP[i]){ #Pour chaque cluster du troncon en question
      sd <- (NP.kmeans[[PointeurNP]]$withinss[j]) / sum(NP.kmeans[[1]]$cluster == j)
      NP_espace_temps <- rbind(NP_espace_temps,c(i,Troncons_A789$Autoroute[match(i,Troncons_A789$ID_Troncon)],NP.kmeans[[PointeurNP]]$centers[j,1],sd,NP.kmeans[[PointeurNP]]$centers[j,1]-sd,NP.kmeans[[PointeurNP]]$centers[j,1]+sd))
    }
    PointeurNP <- PointeurNP +1
  }
  if (i %in% FF_troncons_selection){ # Si c'est un des troncons en question
    for (j in 1:Nombre_de_clusters_FF[i]){ #Pour chaque cluster du troncon en question
      sd <- (FF.kmeans[[PointeurFF]]$withinss[j]) / sum(FF.kmeans[[1]]$cluster == j)
      FF_espace_temps <- rbind(FF_espace_temps,c(i,Troncons_A789$Autoroute[match(i,Troncons_A789$ID_Troncon)],FF.kmeans[[PointeurFF]]$centers[j,1],sd,FF.kmeans[[PointeurFF]]$centers[j,1]-sd,FF.kmeans[[PointeurFF]]$centers[j,1]+sd))
    }
    PointeurFF <- PointeurFF +1
  }
  if (i %in% CC_troncons_selection){ # Si c'est un des troncons en question
    for (j in 1:Nombre_de_clusters_CC[i]){ #Pour chaque cluster du troncon en question
      sd <- (CC.kmeans[[PointeurCC]]$withinss[j]) / sum(CC.kmeans[[1]]$cluster == j)
      CC_espace_temps <- rbind(CC_espace_temps,c(i,Troncons_A789$Autoroute[match(i,Troncons_A789$ID_Troncon)],CC.kmeans[[PointeurCC]]$centers[j,1],sd,CC.kmeans[[PointeurCC]]$centers[j,1]-sd,CC.kmeans[[PointeurCC]]$centers[j,1]+sd))
    }
    PointeurCC <- PointeurCC +1
  }
  if (i %in% PC_troncons_selection){ # Si c'est un des troncons en question
    for (j in 1:Nombre_de_clusters_PC[i]){ #Pour chaque cluster du troncon en question
      sd <- (PC.kmeans[[PointeurPC]]$withinss[j]) / sum(PC.kmeans[[1]]$cluster == j)
      PC_espace_temps <- rbind(PC_espace_temps,c(i,Troncons_A789$Autoroute[match(i,Troncons_A789$ID_Troncon)],PC.kmeans[[PointeurPC]]$centers[j,1],sd,PC.kmeans[[PointeurPC]]$centers[j,1]-sd,PC.kmeans[[PointeurPC]]$centers[j,1]+sd))
    }
    PointeurPC <- PointeurPC +1
  }
}
NP_espace_temps <- NP_espace_temps[-1,]
FF_espace_temps <- FF_espace_temps[-1,]
CC_espace_temps <- CC_espace_temps[-1,]
PC_espace_temps <- PC_espace_temps[-1,]
NP_espace_temps$Time <- as.numeric(NP_espace_temps$Time)
NP_espace_temps$Standard_Deviation <- as.numeric(NP_espace_temps$Standard_Deviation)
FF_espace_temps$Time <- as.numeric(FF_espace_temps$Time)
FF_espace_temps$Standard_Deviation <- as.numeric(FF_espace_temps$Standard_Deviation)
CC_espace_temps$Time <- as.numeric(CC_espace_temps$Time)
CC_espace_temps$Standard_Deviation <- as.numeric(CC_espace_temps$Standard_Deviation)
PC_espace_temps$Time <- as.numeric(PC_espace_temps$Time)
PC_espace_temps$Standard_Deviation <- as.numeric(PC_espace_temps$Standard_Deviation)
#Evaluer le modele

#Décomposer les données de test par troncons
temp$Autoroute <- as.character(temp$Autoroute)
VIP2_pour_test$Entr <- as.numeric(as.character(VIP2_pour_test$Entr))
VIP2_pour_test$Sor <- as.numeric(as.character(VIP2_pour_test$Sor))
Autoroute <- vector(mode = "character", length = nrow(VIP2_pour_test))
VIP2_pour_test_decompose <- cbind(VIP2_pour_test, Autoroute)
VIP2_pour_test_decompose$Autoroute <- as.character(VIP2_pour_test_decompose$Autoroute)
VIP2_pour_test_restant <- VIP2_pour_test
Pointeur <- 1
Pointeur_restant <- 1
Gare_inconnue <- 0 #Gares du système ouvert que le code ne connait pas
for (i in 1 : nrow(VIP2_pour_test)){
  if (  ( (VIP2_pour_test$Entr[i] %in% Troncons_A789[,4]) | (VIP2_pour_test$Entr[i] %in% Troncons_A789[,6])  ) & 
        ( (VIP2_pour_test$Sor[i] %in% Troncons_A789[,4]) | (VIP2_pour_test$Sor[i] %in% Troncons_A789[,6])  )  ) {  # SI TRAJET DANS LES TROIS AUTOROUTES
    if (temp$Autoroute[match(VIP2_pour_test$Entr[i],temp$Cde)] !=  temp$Autoroute[match(VIP2_pour_test$Sor[i],temp$Cde)] ) { #E=A7etS=A9 ou l'inverse
      if (VIP2_pour_test$Entr[i] != 25004210 & VIP2_pour_test$Sor[i] != 25004210){  # Entrée et sortie <> Orange Centre
        if(temp$Autoroute[match(VIP2_pour_test$Entr[i],temp$Cde)] == "A7"){# E=A7 S=A9
          newrow1 <- c(VIP2_pour_test[i,1],VIP2_pour_test$Entr[i],25004210,VIP2_pour_test$KMS[i],VIP2_pour_test$Year[i],VIP2_pour_test$Month[i],VIP2_pour_test$Day[i],VIP2_pour_test$Hour[i],VIP2_pour_test$Minute[i],VIP2_pour_test$TimeEntr[i],VIP2_pour_test$TimeSor[i],VIP2_pour_test$DOW[i],VIP2_pour_test$WOY[i],VIP2_pour_test$Voie[i],"A7")
          newrow2 <- c(VIP2_pour_test[i,1],25004210,VIP2_pour_test$Sor[i],VIP2_pour_test$KMS[i],VIP2_pour_test$Year[i],VIP2_pour_test$Month[i],VIP2_pour_test$Day[i],VIP2_pour_test$Hour[i],VIP2_pour_test$Minute[i],VIP2_pour_test$TimeEntr[i],VIP2_pour_test$TimeSor[i],VIP2_pour_test$DOW[i],VIP2_pour_test$WOY[i],VIP2_pour_test$Voie[i],"A9")
          VIP2_pour_test_decompose = rbind(VIP2_pour_test_decompose[1:Pointeur,],newrow1,newrow2,VIP2_pour_test_decompose[-(1:Pointeur),])
          Pointeur <- Pointeur +2
        }
        else if(temp$Autoroute[match(VIP2_pour_test$Entr[i],temp$Cde)] == "A9"){# E=A9 S=A7
          newrow1 <- c(VIP2_pour_test[i,1],VIP2_pour_test$Entr[i],25004210,VIP2_pour_test$KMS[i],VIP2_pour_test$Year[i],VIP2_pour_test$Month[i],VIP2_pour_test$Day[i],VIP2_pour_test$Hour[i],VIP2_pour_test$Minute[i],VIP2_pour_test$TimeEntr[i],VIP2_pour_test$TimeSor[i],VIP2_pour_test$DOW[i],VIP2_pour_test$WOY[i],VIP2_pour_test$Voie[i],"A9")
          newrow2 <- c(VIP2_pour_test[i,1],25004210,VIP2_pour_test$Sor[i],VIP2_pour_test$KMS[i],VIP2_pour_test$Year[i],VIP2_pour_test$Month[i],VIP2_pour_test$Day[i],VIP2_pour_test$Hour[i],VIP2_pour_test$Minute[i],VIP2_pour_test$TimeEntr[i],VIP2_pour_test$TimeSor[i],VIP2_pour_test$DOW[i],VIP2_pour_test$WOY[i],VIP2_pour_test$Voie[i],"A7")
          VIP2_pour_test_decompose = rbind(VIP2_pour_test_decompose[1:Pointeur,],newrow1,newrow2,VIP2_pour_test_decompose[-(1:Pointeur),])
          Pointeur <- Pointeur +2
        }
      }
      else { # E=A9 S=OC ou l'inverse
        VIP2_pour_test_decompose$Autoroute[Pointeur] <- "A9"
      }
    }
    else {
      VIP2_pour_test_decompose$Autoroute[Pointeur] <- temp$Autoroute[match(VIP2_pour_test$Entr[i],temp$Cde)]
    }
    if (Pointeur_restant == 1){
      VIP2_pour_test_restant <- VIP2_pour_test_restant[2:nrow(VIP2_pour_test_restant),]
    }
    else {
      VIP2_pour_test_restant <- rbind(VIP2_pour_test_restant[(1:(Pointeur_restant-1)),],VIP2_pour_test_restant[-(1:(Pointeur_restant)),])
      Pointeur_restant <- Pointeur_restant - 1
    }
  }
  Pointeur <- Pointeur +1
  Pointeur_restant <- Pointeur_restant +1
}


VIP2_pour_test_decompose <- VIP2_pour_test_decompose[VIP2_pour_test_decompose$Autoroute > 0,]






#DECOMPOSER LES OD PAR TRONCONS :
VIP2_pour_test_par_troncons <- data.frame(ID = "", ID_Troncon= 0, Autoroute= "", Entr= 0, Sor=0, KMS=0, Date = 0, TimeEntr = 0, TimeSor =0, DOW=0, WOY=0, Sens = 0)
VIP2_pour_test_par_troncons$Autoroute <- as.character(VIP2_pour_test_par_troncons$Autoroute)
VIP2_pour_test_par_troncons$ID <- as.character(VIP2_pour_test_par_troncons$ID)
VIP2_pour_test_decompose$Date <- as.character(VIP2_pour_test_decompose$Date)
VIP2_pour_test_par_troncons$Date <- as.character(VIP2_pour_test_par_troncons$Date)

for (i in 1:nrow(VIP2_pour_test_decompose)){
  if ( VIP2_pour_test_decompose$Autoroute[i] == "A7"){
    entree <- match(VIP2_pour_test_decompose$Entr[i],A7_par_pk$Cde)
    sortie <- match(VIP2_pour_test_decompose$Sor[i],A7_par_pk$Cde)
    if ( entree < sortie ) { # SENS 1
      for ( j in entree : (sortie-1) ){
        VIP2_pour_test_par_troncons <- rbind(VIP2_pour_test_par_troncons,c(VIP2_pour_test_decompose$ID[i],Troncons_A7$ID_Troncon[j],"A7",A7_par_pk$Cde[j],A7_par_pk$Cde[j+1],VIP2_pour_test_decompose$KMS[i],VIP2_pour_test_decompose$Date[i],VIP2_pour_test_decompose$TimeEntr[i],VIP2_pour_test_decompose$TimeSor[i],VIP2_pour_test_decompose$DOW[i],VIP2_pour_test_decompose$WOY[i],1))
      }
    }
    else{ #SENS 2
      for ( j in entree : (sortie+1) ){
        VIP2_pour_test_par_troncons <- rbind(VIP2_pour_test_par_troncons,c(VIP2_pour_test_decompose$ID[i],Troncons_A7$ID_Troncon[j-1],"A7",A7_par_pk$Cde[j],A7_par_pk$Cde[j-1],VIP2_pour_test_decompose$KMS[i],VIP2_pour_test_decompose$Date[i],VIP2_pour_test_decompose$TimeEntr[i],VIP2_pour_test_decompose$TimeSor[i],VIP2_pour_test_decompose$DOW[i],VIP2_pour_test_decompose$WOY[i],2))
      }
    }
  }
  else if ( VIP2_pour_test_decompose$Autoroute[i] == "A8"){
    entree <- match(VIP2_pour_test_decompose$Entr[i],A8_par_pk$Cde)
    sortie <- match(VIP2_pour_test_decompose$Sor[i],A8_par_pk$Cde)
    if ( entree < sortie ) { # SENS 1
      for ( j in entree : (sortie-1) ){
        VIP2_pour_test_par_troncons <- rbind(VIP2_pour_test_par_troncons,c(VIP2_pour_test_decompose$ID[i],Troncons_A8$ID_Troncon[j],"A8",A8_par_pk$Cde[j],A8_par_pk$Cde[j+1],VIP2_pour_test_decompose$KMS[i],VIP2_pour_test_decompose$Date[i],VIP2_pour_test_decompose$TimeEntr[i],VIP2_pour_test_decompose$TimeSor[i],VIP2_pour_test_decompose$DOW[i],VIP2_pour_test_decompose$WOY[i],1))
      }
    }
    else{ #SENS 2
      for ( j in entree : (sortie+1) ){
        VIP2_pour_test_par_troncons <- rbind(VIP2_pour_test_par_troncons,c(VIP2_pour_test_decompose$ID[i],Troncons_A8$ID_Troncon[j-1],"A8",A8_par_pk$Cde[j],A8_par_pk$Cde[j-1],VIP2_pour_test_decompose$KMS[i],VIP2_pour_test_decompose$Date[i],VIP2_pour_test_decompose$TimeEntr[i],VIP2_pour_test_decompose$TimeSor[i],VIP2_pour_test_decompose$DOW[i],VIP2_pour_test_decompose$WOY[i],2))
      }
    }
  }
  else if ( VIP2_pour_test_decompose$Autoroute[i] == "A9"){
    entree <- match(VIP2_pour_test_decompose$Entr[i],A9_par_pk$Cde)
    sortie <- match(VIP2_pour_test_decompose$Sor[i],A9_par_pk$Cde)
    if ( entree < sortie ) { # SENS 1
      for ( j in entree : (sortie-1) ){
        VIP2_pour_test_par_troncons <- rbind(VIP2_pour_test_par_troncons,c(VIP2_pour_test_decompose$ID[i],Troncons_A9$ID_Troncon[j],"A9",A9_par_pk$Cde[j],A9_par_pk$Cde[j+1],VIP2_pour_test_decompose$KMS[i],VIP2_pour_test_decompose$Date[i],VIP2_pour_test_decompose$TimeEntr[i],VIP2_pour_test_decompose$TimeSor[i],VIP2_pour_test_decompose$DOW[i],VIP2_pour_test_decompose$WOY[i],1))
      }
    }
    else{ #SENS 2
      for ( j in entree : (sortie+1) ){
        VIP2_pour_test_par_troncons <- rbind(VIP2_pour_test_par_troncons,c(VIP2_pour_test_decompose$ID[i],Troncons_A9$ID_Troncon[j-1],"A9",A9_par_pk$Cde[j],A9_par_pk$Cde[j-1],VIP2_pour_test_decompose$KMS[i],VIP2_pour_test_decompose$Date[i],VIP2_pour_test_decompose$TimeEntr[i],VIP2_pour_test_decompose$TimeSor[i],VIP2_pour_test_decompose$DOW[i],VIP2_pour_test_decompose$WOY[i],2))
      }
    }
  }
}
VIP2_pour_test_par_troncons <- VIP2_pour_test_par_troncons[-1,]


#Rajouter demi trajet LANCON LA BARQUE
Pointeur <- 1
for (i in 1:nrow(VIP2_pour_test_par_troncons)){
  if (VIP2_pour_test_par_troncons$ID_Troncon[Pointeur] == 20 & VIP2_pour_test_par_troncons$Sens[Pointeur] == 1){ # (A7 -> Lancon)
    newrow1 <- c(VIP2_pour_test_par_troncons$ID[Pointeur],21,"A7",25004220,25004278,VIP2_pour_test_par_troncons$KMS[Pointeur],VIP2_pour_test_par_troncons$Date[Pointeur],VIP2_pour_test_par_troncons$TimeEntr[Pointeur],VIP2_pour_test_par_troncons$TimeSor[Pointeur],VIP2_pour_test_par_troncons$DOW[Pointeur],VIP2_pour_test_par_troncons$WOY[Pointeur],1)
    newrow2 <- c(VIP2_pour_test_par_troncons$ID[Pointeur],22,"A7",25004278,25004279,VIP2_pour_test_par_troncons$KMS[Pointeur],VIP2_pour_test_par_troncons$Date[Pointeur],VIP2_pour_test_par_troncons$TimeEntr[Pointeur],VIP2_pour_test_par_troncons$TimeSor[Pointeur],VIP2_pour_test_par_troncons$DOW[Pointeur],VIP2_pour_test_par_troncons$WOY[Pointeur],1)
    VIP2_pour_test_par_troncons <- rbind(VIP2_pour_test_par_troncons[(1:Pointeur),],newrow1,newrow2,VIP2_pour_test_par_troncons[-(1:Pointeur),])
    Pointeur <- Pointeur + 2
  } 
  if (VIP2_pour_test_par_troncons$ID_Troncon[Pointeur] == 20 & VIP2_pour_test_par_troncons$Sens[Pointeur] == 2){ # (Lancon -> A7)
    newrow1 <- c(VIP2_pour_test_par_troncons$ID[Pointeur],21,"A7",25004220,25004278,VIP2_pour_test_par_troncons$KMS[Pointeur],VIP2_pour_test_par_troncons$Date[Pointeur],VIP2_pour_test_par_troncons$TimeEntr[Pointeur],VIP2_pour_test_par_troncons$TimeSor[Pointeur],VIP2_pour_test_par_troncons$DOW[Pointeur],VIP2_pour_test_par_troncons$WOY[Pointeur],2)
    newrow2 <- c(VIP2_pour_test_par_troncons$ID[Pointeur],22,"A7",25004278,25004279,VIP2_pour_test_par_troncons$KMS[Pointeur],VIP2_pour_test_par_troncons$Date[Pointeur],VIP2_pour_test_par_troncons$TimeEntr[Pointeur],VIP2_pour_test_par_troncons$TimeSor[Pointeur],VIP2_pour_test_par_troncons$DOW[Pointeur],VIP2_pour_test_par_troncons$WOY[Pointeur],2)
    VIP2_pour_test_par_troncons <- rbind(VIP2_pour_test_par_troncons[(1:Pointeur),],newrow1,newrow2,VIP2_pour_test_par_troncons[-(1:Pointeur),])
    Pointeur <- Pointeur + 2
  }
  if (VIP2_pour_test_par_troncons$ID_Troncon[Pointeur] == 26 & VIP2_pour_test_par_troncons$Sens[Pointeur] == 1){ # (La Barque -> A8)
    newrow1 <- c(VIP2_pour_test_par_troncons$ID[Pointeur],23,"A8",25004279,25006001,VIP2_pour_test_par_troncons$KMS[Pointeur],VIP2_pour_test_par_troncons$Date[Pointeur],VIP2_pour_test_par_troncons$TimeEntr[Pointeur],VIP2_pour_test_par_troncons$TimeSor[Pointeur],VIP2_pour_test_par_troncons$DOW[Pointeur],VIP2_pour_test_par_troncons$WOY[Pointeur],1)
    newrow2 <- c(VIP2_pour_test_par_troncons$ID[Pointeur],24,"A8",25006001,25006080,VIP2_pour_test_par_troncons$KMS[Pointeur],VIP2_pour_test_par_troncons$Date[Pointeur],VIP2_pour_test_par_troncons$TimeEntr[Pointeur],VIP2_pour_test_par_troncons$TimeSor[Pointeur],VIP2_pour_test_par_troncons$DOW[Pointeur],VIP2_pour_test_par_troncons$WOY[Pointeur],1)
    newrow3 <- c(VIP2_pour_test_par_troncons$ID[Pointeur],25,"A8",25006080,25006002,VIP2_pour_test_par_troncons$KMS[Pointeur],VIP2_pour_test_par_troncons$Date[Pointeur],VIP2_pour_test_par_troncons$TimeEntr[Pointeur],VIP2_pour_test_par_troncons$TimeSor[Pointeur],VIP2_pour_test_par_troncons$DOW[Pointeur],VIP2_pour_test_par_troncons$WOY[Pointeur],1)
    VIP2_pour_test_par_troncons <- rbind(VIP2_pour_test_par_troncons[(1:Pointeur),],newrow1,newrow2,newrow3,VIP2_pour_test_par_troncons[-(1:Pointeur),])
    Pointeur <- Pointeur + 3
  }
  if (VIP2_pour_test_par_troncons$ID_Troncon[Pointeur] == 26 & VIP2_pour_test_par_troncons$Sens[Pointeur] == 2){ # (A8 -> La Barque)
    newrow1 <- c(VIP2_pour_test_par_troncons$ID[Pointeur],23,"A8",25004279,25006001,VIP2_pour_test_par_troncons$KMS[Pointeur],VIP2_pour_test_par_troncons$Date[Pointeur],VIP2_pour_test_par_troncons$TimeEntr[Pointeur],VIP2_pour_test_par_troncons$TimeSor[Pointeur],VIP2_pour_test_par_troncons$DOW[Pointeur],VIP2_pour_test_par_troncons$WOY[Pointeur],2)
    newrow2 <- c(VIP2_pour_test_par_troncons$ID[Pointeur],24,"A8",25006001,25006080,VIP2_pour_test_par_troncons$KMS[Pointeur],VIP2_pour_test_par_troncons$Date[Pointeur],VIP2_pour_test_par_troncons$TimeEntr[Pointeur],VIP2_pour_test_par_troncons$TimeSor[Pointeur],VIP2_pour_test_par_troncons$DOW[Pointeur],VIP2_pour_test_par_troncons$WOY[Pointeur],2)
    newrow3 <- c(VIP2_pour_test_par_troncons$ID[Pointeur],25,"A8",25006080,25006002,VIP2_pour_test_par_troncons$KMS[Pointeur],VIP2_pour_test_par_troncons$Date[Pointeur],VIP2_pour_test_par_troncons$TimeEntr[Pointeur],VIP2_pour_test_par_troncons$TimeSor[Pointeur],VIP2_pour_test_par_troncons$DOW[Pointeur],VIP2_pour_test_par_troncons$WOY[Pointeur],2)
    VIP2_pour_test_par_troncons <- rbind(VIP2_pour_test_par_troncons[(1:Pointeur),],newrow1,newrow2,newrow3,VIP2_pour_test_par_troncons[-(1:Pointeur),])
    Pointeur <- Pointeur + 3
  }
  Pointeur <- Pointeur + 1
}


#Calculer les indicateurs
IND <- data.frame (ID = c("NP","FF","CC","PC"),Tpos=c(0,0,0,0), Fneg = c(0,0,0,0), Ind1 = c(0,0,0,0),Ind2 = c(0,0,0,0),Ind3 = c(0,0,0,0))
for (i in 1:nrow(VIP2_pour_test_par_troncons)){
  if ( VIP2_pour_test_par_troncons$ID[i] == "NP" & (VIP2_pour_test_par_troncons$ID_Troncon[i] %in% NP_espace_temps$ID_Troncon)){ 
   index <- match(VIP2_pour_test_par_troncons$ID_Troncon[i],NP_espace_temps$ID_Troncon)
     if (((VIP2_pour_test_par_troncons$TimeEntr[i] > NP_espace_temps$Tmin[index]) & (VIP2_pour_test_par_troncons$TimeEntr[i] < NP_espace_temps$Tmax[index])) |
         ((VIP2_pour_test_par_troncons$TimeEntr[i] > NP_espace_temps$Tmin[index+1]) & (VIP2_pour_test_par_troncons$TimeEntr[i] < NP_espace_temps$Tmax[index+1])) ) {
      IND$Tpos[1] <- IND$Tpos[1] +1
    }
    else {
      IND$Fneg[1] <- IND$Fneg[1] +1
    }
  }
  if ( VIP2_pour_test_par_troncons$ID[i] == "FF" & (VIP2_pour_test_par_troncons$ID_Troncon[i] %in% FF_espace_temps$ID_Troncon)){ 
    index <- match(VIP2_pour_test_par_troncons$ID_Troncon[i],FF_espace_temps$ID_Troncon)
    if (((VIP2_pour_test_par_troncons$TimeEntr[i] > FF_espace_temps$Tmin[index]) & (VIP2_pour_test_par_troncons$TimeEntr[i] < FF_espace_temps$Tmax[index] )) |
        ((VIP2_pour_test_par_troncons$TimeEntr[i] > FF_espace_temps$Tmin[index+1]) & (VIP2_pour_test_par_troncons$TimeEntr[i] < FF_espace_temps$Tmax[index+1])) ){
      IND$Tpos[2] <- IND$Tpos[2] +1
    }
    else {
      IND$Fneg[2] <- IND$Fneg[2] +1
    }
  }
  if ( VIP2_pour_test_par_troncons$ID[i] == "CC" & (VIP2_pour_test_par_troncons$ID_Troncon[i] %in% CC_espace_temps$ID_Troncon)){ 
    index <- match(VIP2_pour_test_par_troncons$ID_Troncon[i],CC_espace_temps$ID_Troncon)
    if (((VIP2_pour_test_par_troncons$TimeEntr[i] > CC_espace_temps$Tmin[index]) & (VIP2_pour_test_par_troncons$TimeEntr[i] < CC_espace_temps$Tmax[index])) |
        ((VIP2_pour_test_par_troncons$TimeEntr[i] > CC_espace_temps$Tmin[index+1]) & (VIP2_pour_test_par_troncons$TimeEntr[i] < CC_espace_temps$Tmax[index+1])) |
        ((VIP2_pour_test_par_troncons$TimeEntr[i] > CC_espace_temps$Tmin[index+2]) & (VIP2_pour_test_par_troncons$TimeEntr[i] < CC_espace_temps$Tmax[index+2]))){
      IND$Tpos[3] <- IND$Tpos[3] +1
    }
    else {
      IND$Fneg[3] <- IND$Fneg[3] +1
    }
  }
  if ( VIP2_pour_test_par_troncons$ID[i] == "PC" & (VIP2_pour_test_par_troncons$ID_Troncon[i] %in% PC_espace_temps$ID_Troncon)){ 
    index <- match(VIP2_pour_test_par_troncons$ID_Troncon[i],PC_espace_temps$ID_Troncon)
    if (((VIP2_pour_test_par_troncons$TimeEntr[i] > PC_espace_temps$Time[index] - PC_espace_temps$Standard_Deviation[index]) & (VIP2_pour_test_par_troncons$TimeEntr[i] < PC_espace_temps$Time[index] + PC_espace_temps$Standard_Deviation[index])) |
        ((VIP2_pour_test_par_troncons$TimeEntr[i] > PC_espace_temps$Time[index+1] - PC_espace_temps$Standard_Deviation[index+1]) & (VIP2_pour_test_par_troncons$TimeEntr[i] < PC_espace_temps$Time[index+1] + PC_espace_temps$Standard_Deviation[index+1])) ){
      IND$Tpos[4] <- IND$Tpos[4] +1
    }
    else {
      IND$Fneg[4] <- IND$Fneg[4] +1
    }
  }
}

for (i in 1:4){
IND$Ind1[i] <-  IND$Tpos[i] / (IND$Tpos[i] + IND$Fneg[i]) # de correction :  
IND$Ind2[i] <-  IND$Fneg[i] / (IND$Tpos[i] + IND$Fneg[i]) # de raté :
#Ind3_NP <-  Fpos_NP / (Tpos_NP + Fpos_NP)  # de fausse alerte : 
}

