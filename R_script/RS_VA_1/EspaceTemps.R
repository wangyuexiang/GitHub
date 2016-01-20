# call packages
library(dplyr)
library(ggplot2)
library(gridExtra)
library(knitr)

# OD -> Espace -> Temps 
#######
# Créer VIP2
####### 
VIP2 <- VIP2[VIP2$ID %in% c("CC","FF", "NP", "PC") & VIP2$Date > as.Date("2014-12-31") & VIP2$Date < as.Date("2015-5-29"), ]
x <- vector(mode = "integer", length = nrow(VIP2))


VIP2 <- cbind(VIP2[,1:3],x,x,x,x,x,x,VIP2$TimeEntr,VIP2$TimeSor,VIP2$DOW,VIP2$WOY,VIP2$Date)
colnames(VIP2)<- c(colnames(MODELE),"Date")

VIP2_pour_modele <- VIP2[VIP2$Date < as.Date("2015-5-1"),]
VIP2_pour_test <- VIP2[VIP2$Date >= as.Date("2015-5-1"),]

VIP2_decompose <- decompose(VIP2_pour_modele)

# get the ID list
ID.list <- as.data.frame(VIP2_decompose %>% group_by(ID) %>% summarise())

#######
#COMPTAGE DES TRAJETS PAR TRONCON (PAR SENS ? PAS ENCORE)
#######

Compteur <- VIP2_decompose %>% 
  group_by(ID, ID_Troncon) %>%
  summarise( n = n())


########
#DECISION AUTOMATIQUE DES TRONCONS FREQUENTS
########


# [Pourcentagedumax max ; max ]
Pourcentagedumax <- 0.56
  Troncon_Selection <- Compteur %>% 
    group_by(ID) %>%
    filter (n > (Pourcentagedumax * max(n)))

VIP2_espace <- inner_join(VIP2_decompose, Troncon_Selection, by = c("ID","ID_Troncon"))


########
# Kmeans 
########

VIP2_espace$TimeSor <- as.numeric(VIP2_espace$TimeSor)
VIP2_espace_temps <- data.frame(ID="", Entr=0, Sor=0, DOW=0, SD=0, T=0, Tmin=0, Tmax=0, n=0)
for (k in 1:75){
  for (i in 1:nrow(ID.list)){
    #Mettre 3 cluster pour CC et 2 pour les autres
    if (ID.list[i,] == "CC" ){ 
      n.cluster <- 3
    }
    else {
      n.cluster <- 2
    }
    
    for (j in 0:6){
      temp <- VIP2_espace %>% filter(ID == ID.list$ID[i] & DOW == j &  ID_Troncon == k )
      # base to be verified
      if(nrow(temp) >= 5) {
        # if not many passages, we will not cluster
        set.seed(1234)
        temp.kmeans <-   kmeans(temp[, "TimeSor"], centers = n.cluster)
        temp$cluster <- temp.kmeans$cluster
        T <- temp %>%
          group_by(ID, Entr, Sor, DOW, cluster) %>%
          summarise(SD = sd(TimeSor), T = mean(TimeSor),Tmin = T -SD, Tmax = T + SD, n = n())
        T <- T %>% filter(n>1)
        T$cluster <- NULL
        VIP2_espace_temps <- rbind(VIP2_espace_temps, T) 
      }
    }
  }
}

VIP2_espace_temps <- VIP2_espace_temps[-1,]


result.model2<- VIP2_espace_temps[,-c(5:6,9)]
result.model2$Model <- 22

#######
#Evaluer le modele
#######
#######
#Décomposer par troncons les données de test
#######
VIP2_pour_test_par_troncons <- decompose(VIP2_pour_test)
#######
#Calculer les indicateurs









#Appliquer la méthode de Yuexiang




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

