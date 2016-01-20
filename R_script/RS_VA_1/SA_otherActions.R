# load("Troncons_A789.RData")
# load("A7_par_pk.RData")
# load("A8_par_pk.RData")
# load("A9_par_pk.RData")
# load("Troncons_A7.RData")
# load("Troncons_A8.RData")
# load("Troncons_A9.RData")
# gares <- read.table("garesLatLng.csv", header = T, sep = ",")
# load("MODELE.RData")

# remove unuseful data set after Rmd
rm(centers1, centers2, cl1, cl2, gg1, gg2, within.ss, t.kmeans)
rm(T.matin, T.aprem)
rm(i,j,clus)
rm(T, temp)
##########
### CC

train_decompose.LngLat <- GetLngLat(train_decompose)

ggplot(train_decompose.LngLat) + 
  geom_point(aes(ELng, ELat, col = "Entr")) +
  geom_point(aes(SLng, SLat, col = "Sor")) +
  geom_segment(aes(x = ELng, xend = SLng, y = ELat, yend = SLat)) + 
  facet_wrap(~ID) + ggtitle("Result: Geo-representation")
	
ggplot(train_decompose.LngLat) + 
  geom_point(aes(ELng, ELat, col = "Entr")) +
  geom_point(aes(SLng, SLat, col = "Sor")) +
  geom_segment(aes(x = ELng, xend = SLng, y = ELat, yend = SLat)) 


##########
# viz Ind

ggplot(Ind) + 
	geom_point(aes(Model, Ind1, col = as.factor(ID))) + 
	geom_point(aes(Model, Ind2, col = as.factor(ID))) + 
	geom_point(aes(Model, Ind3, col = as.factor(ID))) 

ggplot(Ind) + 
  geom_point(aes(Model, Ind1, col = "% de trajets réels prédits")) + 
  # geom_point(aes(Model, Ind2, col = "Ind2")) + 
  geom_point(aes(Model, Ind3, col = "% de fausse alerts")) +
  facet_wrap(~ID) +
  labs(y = " Indicator") +
  theme(legend.title = element_blank())



##########
# viz: test result
ggplot(test_decompose) + geom_point(aes(Date, TimeSor, col = as.factor(result))) + facet_wrap(~ID) + ggtitle("Test Result")

##########
# viz: trian result - prepare
temp <- train_decompose
temp$result <- 0
for (i in 1:nrow(result)){
  temp[
    temp$ID   == result$ID[i] &
      temp$Sor  == result$Sor[i]&
      temp$DOW  == result$DOW[i]&
      temp$TimeSor >= result$Tmin[i]&
      temp$TimeSor <= result$Tmax[i],
    "result"]<-1
}
##########
# viz: train result
ggplot(temp) + geom_point(aes(Date, TimeSor, col = as.factor(result))) + facet_wrap(~ID) + ggtitle("Train Result")
ggplot(VIP2) + geom_point(aes(Date, TimeSor)) + facet_wrap(~ID) + ggtitle("VIP2")


##########
# viz: result
ggplot(result) + 
  geom_point(aes(DOW, Tmin, col = "Tmin")) +
  geom_point(aes(DOW, Tmax, col = "Tmax")) +
  geom_segment(aes(x=DOW, xend=DOW, y=Tmin, yend=Tmax)) +
  facet_grid(ID~Model) + ggtitle("Result: Time Interval by DOW")


##########
# get the number of cluster
temp <- trx
ggplot(temp) + geom_point(aes(Date, TimeSor, shape = as.factor(DOW))) + facet_wrap(~ID)

clus<- clusGap(temp[,"TimeSor"], kmeans, 10)
clus
with(clus,maxSE(Tab[,"gap"],Tab[,"SE.sim"]))
plot(clus)



  



##########
##########
# OLD
##########
##########
  
##########
# model02: Benchmark - consider DOW
##########
matin <- train %>%
  select(ID, Entr, Sor, Date, DOW, TimeSor) %>%
  filter(TimeSor < 12 ) %>%
  group_by(ID, Entr, Sor, DOW) %>%
  summarise(nDOW = n())

aprem <- train %>%
  select(ID, Entr, Sor, Date, DOW, TimeSor) %>%
  filter(TimeSor >= 12 ) %>%
  group_by(ID, Entr, Sor, DOW) %>%
  summarise(nDOW = n())

T.matin <- train %>%
  select(ID, Entr, Sor, Date, DOW, TimeSor) %>%
  filter(TimeSor < 12 ) %>%
  group_by(ID, Entr, Sor, DOW) %>%
  summarise(SD = sd(TimeSor), T = mean(TimeSor),Tmin = T -SD, Tmax = T + SD)

T.aprem <- train %>%
  select(ID, Entr, Sor, Date, DOW, TimeSor) %>%
  filter(TimeSor >= 12 ) %>%
  group_by(ID, Entr, Sor, DOW) %>%
  summarise(SD = sd(TimeSor), T = mean(TimeSor),Tmin = T -SD, Tmax = T + SD)

T.matin <- inner_join(T.matin, matin)
T.aprem <- inner_join(T.aprem, aprem)
T <- rbind(T.matin, T.aprem)

##########
##########
# get model.02 result
test_decompose$result <- 0
for (i in 1:nrow(result)){
  test_decompose[
    test_decompose$ID   == result$ID[i] &
    test_decompose$Entr == result$Entr[i]&
    test_decompose$Sor  == result$Sor[i]&
    test_decompose$DOW  == result$DOW[i]&
    test_decompose$TimeSor >= result$Tmin[i]&
    test_decompose$TimeSor <= result$Tmax[i],
    "result"]<-1
}

  
##########
# get Ind1, Ind2
test.result.model0 <- test0 %>%
  group_by(ID) %>%
  summarise(Tpos = sum(result[result == 1]), Fneg = n() - Tpos, Ind1 = Tpos/(Tpos+Fneg), Ind2 = Fneg/(Tpos+Fneg) )
	
# ##########
# # get Ind3
#
# test.period <- data.frame(Date = seq(as.Date("2015-5-1"), as.Date("2015-5-28"), "day"))
# test.period$DOW <- as.POSIXlt(test.period$Date)$wday
#
# temp <- result.model1
# temp$DOW <- as.numeric(temp$DOW)
# temp <- inner_join(test.period, temp, by="DOW")
# temp$Mark <- 0
#
# # temp$DOW <- as.character(temp$DOW)
# for(i in 1:nrow(test_decompose)){
#   temp[
#     temp$ID   == test_decompose$ID[i] &
#     temp$Entr == test_decompose$Entr[i] &
#     temp$Sor  == test_decompose$Sor[i] &
#     temp$Date  == test_decompose$Date[i] &
#     temp$Tmin <= test_decompose$TimeSor[i]&
#     temp$Tmax >= test_decompose$TimeSor[i],
#     "Mark"]<-1
# }
# count(temp,Mark)
# m1<-temp
#
# for(i in 1:nrow(temp)){
#   for(j in 1:nrow(test_decompose)){
#     if(temp$ID[i]   == test_decompose$ID[j] &
#        temp$Entr[i] == test_decompose$Entr[j] &
#        temp$Sor[i]  == test_decompose$Sor[j] &
#        temp$Date[i] == test_decompose$Date[j] &
#        temp$Tmax[i] >= test_decompose$TimeSor[j] &
#        temp$Tmin[i] <= test_decompose$TimeSor[j]) {
#       temp$Mark[i] <- temp$Mark[i] + 1
#       break # end for loop
#     } # next trx in real
#   } # next trx predicted
# }
# m2<-temp
#
# temp.ind <- temp %>%
#   filter(Mark == 0)
# group_by(ID) %>%
#   summarize(Fpos = n())

# get Ind3
temp <- result.model0
temp$DOW <- as.numeric(temp$DOW)
temp <- inner_join(test.period, temp, by="DOW")
temp$Mark <- 0

# temp$DOW <- as.character(temp$DOW)
for(i in 1:nrow(test0)){
  temp[
    temp$ID   == test0$ID[i] &
      temp$Entr == test0$Entr[i] &
      temp$Sor  == test0$Sor[i] &
      temp$Date  == test0$Date[i] &
      temp$Tmin <= test0$TimeSor[i]&
      temp$Tmax >= test0$TimeSor[i],
    "Mark"]<-1
}

Ind3 <- temp %>% group_by(ID) %>% summarise(nMark = sum(Mark[Mark==1]), t = (n() - nMark), Ind3 = t/ n() )

# get the final indicator
test.result.model0$Ind <- test.result.model0$Ind1 - test.result.model0$Ind2 / 15
test.result.model0 <- test.result.model0[, c(1, ncol(test.result.model0))]
test.result.model0$Model <- 02	
   

##########
##########
# model.12: Time-Space - consider DOW
# get indicators
test_result <- test_decompose %>%
  group_by(ID) %>%
  summarise(Tpos = sum(result[result == 1]), Fneg = n() - Tpos, Ind1 = Tpos/(Tpos+Fneg), Ind2 = Fneg/(Tpos+Fneg) )

# get the final indicator
test_result$Ind <- test_result$Ind1 - test_result$Ind2 / 15
test.result.model1 <- test_result[, c(1, ncol(test_result))]
test.result.model1$Model <- 1

##########
##########
# final step
Ind <- rbind(test.result.model0, test.result.model1)




##########
##########

Decompose <- function(transaction){
	# break OD to troncon (A7, A8, A9)
	# treat the Systeme Ouvert
	#
	# Args: 
	#	transaction:	ID, Entr, Sor, ...
	#
	# Returns:
	#	OD in A7,A8,A9: OD --> troncons
	#	OD other: does not change
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
          ( (transaction$Sor[i] %in% Troncons_A789[,4]) | (transaction$Sor[i] %in% Troncons_A789[,6])  )  ) { 
      # SI TRAJET DANS LES TROIS AUTOROUTES
      if (gares$Autoroute[match(transaction$Entr[i],gares$Cde)] !=  gares$Autoroute[match(transaction$Sor[i],gares$Cde)] ) { 
        #E=A7etS=A9 ou l'inverse
        if (transaction$Entr[i] != 25004210 & transaction$Sor[i] != 25004210){  
          # Entrée et sortie <> Orange Centre
          if(gares$Autoroute[match(transaction$Entr[i],gares$Cde)] == "A7"){
            # E=A7 S=A9
            newrow1 <- c(transaction[i,1],transaction$Entr[i],25004210,transaction[i, c(4:ncol(transaction))],"A7")
            newrow2 <- c(transaction[i,1],25004210,transaction$Sor[i], transaction[i, c(4:ncol(transaction))],"A9")
            transaction_decompose = rbind(transaction_decompose[1:Pointeur,],newrow1,newrow2,transaction_decompose[-(1:Pointeur),])
            Pointeur <- Pointeur +2
          }
          else if(gares$Autoroute[match(transaction$Entr[i],gares$Cde)] == "A9"){
            # E=A9 S=A7
            newrow1 <- c(transaction[i,1],transaction$Entr[i],25004210,transaction[i, c(4:ncol(transaction))],"A9")
            newrow2 <- c(transaction[i,1],25004210,transaction$Sor[i], transaction[i, c(4:ncol(transaction))],"A7")
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
        transaction_restant <- transaction_restant[2:nrow(transaction_restant),]
      }
      else {
        transaction_restant <- rbind(transaction_restant[(1:(Pointeur_restant-1)),],transaction_restant[-(1:(Pointeur_restant)),])
        Pointeur_restant <- Pointeur_restant - 1
      }
    }
    
    ## ??? deactivate: no information about Voie
    else if (transaction$Entr[i] < 0 & ( (transaction$Sor[i] %in% Troncons_A789[,4])|(transaction$Sor[i] %in% Troncons_A789[,6]) ) ){ 
      #Si Entr = 0 et Sor dans A789
      if (transaction$Sor[i] == 25006001){ #Canet de mereuil
        if (transaction$Voie[i] >= 20 ){#Sortie
          transaction_decompose[Pointeur,] <- c(transaction$ID[i],25004278 ,25006001,transaction[i, c(4:ncol(transaction))],"A8")
        } #On crée un trajet Coudoux -> Canet de méreuil
        else { 
          transaction_decompose[Pointeur,] <- c(transaction$ID[i], 25006001 ,25004278,transaction[i, c(4:ncol(transaction))],"A8")
        }  #Trajet Canet -> Coudoux
      }
      else if (transaction$Sor[i] == 25006010){ #Fréjus
        if (transaction$Voie[i] >= 20 ){#Sortie
          transaction_decompose[Pointeur,] <- c(transaction$ID[i],25006022 ,25006010,transaction[i, c(4:ncol(transaction))],"A8")
        } #On crée un trajet Antibes PV Nord-> Fréjus
        else { 
          transaction_decompose[Pointeur,] <- c(transaction$ID[i], 25006010 ,25006012,transaction[i, c(4:ncol(transaction))],"A8")
        }  #Trajet Fréjus -> Antibes PV 
      }
      else if (transaction$Sor[i] == 25006011){ #Les Adrets
        if (transaction$Voie[i] >= 20 ){#Sortie
          transaction_decompose[Pointeur,] <- c(transaction$ID[i],25006022 ,25006011,transaction[i, c(4:ncol(transaction))],"A8")
        } #On crée un trajet Antibes PV Nord-> Les Adrets
        else { 
          transaction_decompose[Pointeur,] <- c(transaction$ID[i], 25006011 ,25006012,transaction[i, c(4:ncol(transaction))],"A8")
        }  #Trajet Les Adrets -> Antibes PV 
      }
      else if (transaction$Sor[i] == 25006014){ #Antibes Ouest
        if (transaction$Voie[i] >= 20 ){#Sortie
          transaction_decompose[Pointeur,] <- c(transaction$ID[i],25006009 ,25006014,transaction[i, c(4:ncol(transaction))],"A8")
        } #On crée un trajet Capitou -> Antibes Ouest
        else { 
          transaction_decompose[Pointeur,] <- c(transaction$ID[i], 25006014 ,25006009,transaction[i, c(4:ncol(transaction))],"A8")
        }  #Trajet Antibes Ouest -> Capitou 
      }
      else if (transaction$Sor[i] == 25006012){ #Antibes PV
        if (transaction$Voie[i] >= 20 ){#PV Sud, donc de Cannes vers Nice
          transaction_decompose[Pointeur,] <- c(transaction$ID[i],25006009 ,25006015,transaction[i, c(4:ncol(transaction))],"A8")
        } #On crée un trajet Capitou -> Cagnes Ouest Nord
        else { #PV Nord, de Nice vers Cannes
          transaction_decompose[Pointeur,] <- c(transaction$ID[i], 25006015 ,25006009,transaction[i, c(4:ncol(transaction))],"A8")
        }  #Trajet Cagnes Ouest Nord -> Capitou 
      }
      else if (transaction$Sor[i] == 25006024){ #Sophia
        transaction_decompose[Pointeur,] <- c(transaction$ID[i],25006024 ,25006020,transaction[i, c(4:ncol(transaction))],"A8")
        #Entrée seule, don trajet Sophia -> Saint Isidore
      }
      else if (transaction$Sor[i] == 25006013){ #Antibes Est
        if (transaction$Voie[i] >= 20 ){#Sortie
          transaction_decompose[Pointeur,] <- c(transaction$ID[i],25006020 ,25006013,transaction[i, c(4:ncol(transaction))],"A8")
        } #On crée un trajet Saint Isidore -> Antibes Est
        else { 
          transaction_decompose[Pointeur,] <- c(transaction$ID[i], 25006013 ,25006020,transaction[i, c(4:ncol(transaction))],"A8")
        }  #Trajet Antibes Est -> Saint Isidore
      }
      else if (transaction$Sor[i] == 25006017){ #Cagnes Ouest Sud
        transaction_decompose[Pointeur,] <- c(transaction$ID[i],25006017 ,25006020,transaction[i, c(4:ncol(transaction))],"A8")
        #On crée un trajet  Cagnes Ouest Sud -> Saint Isidore
      }
      else if (transaction$Sor[i] == 25006015){ #Cagnes Ouest Nord
        transaction_decompose[Pointeur,] <- c(transaction$ID[i],25006020 ,25006015,transaction[i, c(4:ncol(transaction))],"A8")
        #On crée un trajet  Saint Isidore -> Cagnes Ouest Nord
      }
      else if (transaction$Sor[i] == 25006016){ #Cagnes Est
        if (transaction$Voie[i] >= 20 ){#Sortie
          transaction_decompose[Pointeur,] <- c(transaction$ID[i],25006020 ,25006016,transaction[i, c(4:ncol(transaction))],"A8")
        } #On crée un trajet Saint Isidore -> Cagnes Est
        else { 
          transaction_decompose[Pointeur,] <- c(transaction$ID[i], 25006016 ,25006020,transaction[i, c(4:ncol(transaction))],"A8")
        }  #TrajetCagnes Est -> Saint Isidore
      }
      else if (transaction$Sor[i] == 25006021){ #Saint Isidore Ech Est
        if (transaction$Voie[i] >= 20 ){#Sortie
          transaction_decompose[Pointeur,] <- c(transaction$ID[i],25006027 ,25006021,transaction[i, c(4:ncol(transaction))],"A8")
        } #On crée un trajet La Turbie -> Saint Isidore Ech Est
        else { 
          transaction_decompose[Pointeur,] <- c(transaction$ID[i], 25006021 ,25006027,transaction[i, c(4:ncol(transaction))],"A8")
        }  #Trajet Saint Isidore Ech Est -> La Turbie
      }
      else if (transaction$Sor[i] == 25006021){ #Saint Isidore Ech Est
        if (transaction$Voie[i] >= 20 ){#Sortie
          transaction_decompose[Pointeur,] <- c(transaction$ID[i],25006027 ,25006021,transaction[i, c(4:ncol(transaction))],"A8")
        } #On crée un trajet La Turbie -> Saint Isidore Ech Est
        else { 
          transaction_decompose[Pointeur,] <- c(transaction$ID[i], 25006021 ,25006027,transaction[i, c(4:ncol(transaction))],"A8")
        }  #Trajet Saint Isidore Ech Est -> La Turbie
      }
      else if (transaction$Sor[i] == 25006019){ #Saint Isidore Ech Ouest
        if (transaction$Voie[i] >= 20 ){#Sortie
          transaction_decompose[Pointeur,] <- c(transaction$ID[i],25006012 ,25006019,transaction[i, c(4:ncol(transaction))],"A8")
        } #On crée un trajet Antibes PV -> Saint Isidore Ech Ouest
        else { 
          transaction_decompose[Pointeur,] <- c(transaction$ID[i], 25006019 ,25006012,transaction[i, c(4:ncol(transaction))],"A8")
        }  #Trajet Saint Isidore Ech Ouest -> Antibes PV
      }
      else if (transaction$Sor[i] == 25006020){ #Saint Isidore PV
        if (transaction$Voie[i] >= 20 ){#Nord, donc de l'Italie vers Cannes
          transaction_decompose[Pointeur,] <- c(transaction$ID[i],25006026 ,25006020,transaction[i, c(4:ncol(transaction))],"A8")
        } #On crée un trajet La Turbie Ech -> Cagnes Ouest Nord
        else { #Sud, de Cannes vers l'Italie
          transaction_decompose[Pointeur,] <- c(transaction$ID[i], 25006020 ,25006026,transaction[i, c(4:ncol(transaction))],"A8")
        }  #Trajet Cagnes Ouest Nord -> La Turbie Ech
      }
      else if (transaction$Sor[i] == 25006026){ #La Turbie Ech
        if (transaction$Voie[i] >= 20 ){#Sortie
          transaction_decompose[Pointeur,] <- c(transaction$ID[i],25006020 ,25006026,transaction[i, c(4:ncol(transaction))],"A8")
        } #On crée un trajet Saint isidore PV -> La Turbie Ech
        else { 
          transaction_decompose[Pointeur,] <- c(transaction$ID[i], 25006026 ,25006020,transaction[i, c(4:ncol(transaction))],"A8")
        }  #Trajet La Turbie Ech -> Saint isidore PV
      }
      else if (transaction$Sor[i] == 25006027){ #La Turbie PV
        if (transaction$Voie[i] >= 20 ){# Nord, donc de l'Italie vers Cannes
          transaction_decompose[Pointeur,] <- c(transaction$ID[i],25006027 ,25006026,transaction[i, c(4:ncol(transaction))],"A8")
        } #On crée un trajet La Turbie PV -> La Turbie Ech
        else { #Sud, de Cannes vers l'Italie
          transaction_decompose[Pointeur,] <- c(transaction$ID[i], 25006026 ,25006027,transaction[i, c(4:ncol(transaction))],"A8")
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
  # ??? transaction_decompose$Year <- as.character(transaction_decompose$Year)
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
          transaction_par_troncons <- rbind(transaction_par_troncons,
                                            c(transaction_decompose$ID[i],Troncons_A8$ID_Troncon[j],"A8",
                                              A8_par_pk$Cde[j],A8_par_pk$Cde[j+1],transaction_decompose$Date[i],transaction_decompose$DOW[i],transaction_decompose$WOY[i],transaction_decompose$TimeEntr[i],transaction_decompose$TimeSor[i],1))
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
	

### test
	#Rajouter demi trajet LANCON LA BARQUE
	Pointeur <- 1
	for (i in 1:nrow(transaction_par_troncons)){
	  if (!is.na(transaction_par_troncons$Sor[Pointeur]) &
		transaction_par_troncons$Sor[Pointeur] == 25004220 ){ # (A7 -> Lancon)
	    newrow1 <- c(transaction_par_troncons$ID[Pointeur],25004220,25004278,transaction_decompose$Date[Pointeur],transaction_decompose$DOW[Pointeur],transaction_decompose$WOY[Pointeur],transaction_decompose$TimeEntr[Pointeur],transaction_decompose$TimeSor[Pointeur])
	    newrow2 <- c(transaction_par_troncons$ID[Pointeur],25004278,25004279,transaction_decompose$Date[Pointeur],transaction_decompose$DOW[Pointeur],transaction_decompose$WOY[Pointeur],transaction_decompose$TimeEntr[Pointeur],transaction_decompose$TimeSor[Pointeur])
	    transaction_par_troncons <- rbind(transaction_par_troncons[(1:Pointeur),],newrow1,newrow2,transaction_par_troncons[-(1:Pointeur),])
	    Pointeur <- Pointeur + 2
	  } 
	  if (!is.na(transaction_par_troncons$Entr[Pointeur]) &
		transaction_par_troncons$Entr[Pointeur] == 25004220){ # (Lancon -> A7)
	    newrow1 <- c(transaction_par_troncons$ID[Pointeur],25004220,25004278,transaction_decompose$Date[Pointeur],transaction_decompose$DOW[Pointeur],transaction_decompose$WOY[Pointeur],transaction_decompose$TimeEntr[Pointeur],transaction_decompose$TimeSor[Pointeur])
	    newrow2 <- c(transaction_par_troncons$ID[Pointeur],25004278,25004279,transaction_decompose$Date[Pointeur],transaction_decompose$DOW[Pointeur],transaction_decompose$WOY[Pointeur],transaction_decompose$TimeEntr[Pointeur],transaction_decompose$TimeSor[Pointeur])
	    transaction_par_troncons <- rbind(transaction_par_troncons[(1:Pointeur),],newrow1,newrow2,transaction_par_troncons[-(1:Pointeur),])
	    Pointeur <- Pointeur + 2
	  }
	  if (!is.na(transaction_par_troncons$Entr[Pointeur]) &
		transaction_par_troncons$Entr[Pointeur] == 25006002){ # (La Barque -> A8)
	    newrow1 <- c(transaction_par_troncons$ID[Pointeur],25004279,25006001,transaction_decompose$Date[Pointeur],transaction_decompose$DOW[Pointeur],transaction_decompose$WOY[Pointeur],transaction_decompose$TimeEntr[Pointeur],transaction_decompose$TimeSor[Pointeur])
	    newrow2 <- c(transaction_par_troncons$ID[Pointeur],25006001,25006080,transaction_decompose$Date[Pointeur],transaction_decompose$DOW[Pointeur],transaction_decompose$WOY[Pointeur],transaction_decompose$TimeEntr[Pointeur],transaction_decompose$TimeSor[Pointeur])
	    newrow3 <- c(transaction_par_troncons$ID[Pointeur],25006080,25006002,transaction_decompose$Date[Pointeur],transaction_decompose$DOW[Pointeur],transaction_decompose$WOY[Pointeur],transaction_decompose$TimeEntr[Pointeur],transaction_decompose$TimeSor[Pointeur])
	    transaction_par_troncons <- rbind(transaction_par_troncons[(1:Pointeur),],newrow1,newrow2,newrow3,transaction_par_troncons[-(1:Pointeur),])
	    Pointeur <- Pointeur + 3
	  }
	  if (!is.na(transaction_par_troncons$Sor[Pointeur]) &
		transaction_par_troncons$Sor[Pointeur] == 25006002){ # (A8 -> La Barque)
	    newrow1 <- c(transaction_par_troncons$ID[Pointeur],25004279,25006001,transaction_decompose$Date[Pointeur],transaction_decompose$DOW[Pointeur],transaction_decompose$WOY[Pointeur],transaction_decompose$TimeEntr[Pointeur],transaction_decompose$TimeSor[Pointeur])
	    newrow2 <- c(transaction_par_troncons$ID[Pointeur],25006001,25006080,transaction_decompose$Date[Pointeur],transaction_decompose$DOW[Pointeur],transaction_decompose$WOY[Pointeur],transaction_decompose$TimeEntr[Pointeur],transaction_decompose$TimeSor[Pointeur])
	    newrow3 <- c(transaction_par_troncons$ID[Pointeur],25006080,25006002,transaction_decompose$Date[Pointeur],transaction_decompose$DOW[Pointeur],transaction_decompose$WOY[Pointeur],transaction_decompose$TimeEntr[Pointeur],transaction_decompose$TimeSor[Pointeur])
	    transaction_par_troncons <- rbind(transaction_par_troncons[(1:Pointeur),],newrow1,newrow2,newrow3,transaction_par_troncons[-(1:Pointeur),])
	    Pointeur <- Pointeur + 3
	  }
	  Pointeur <- Pointeur + 1
	}
	
	
	
	
	
	
	
	
	
  transaction_par_troncons$Date <- as.Date(as.numeric(transaction_par_troncons$Date), origin = as.Date("1970-1-1"))
  transaction_par_troncons$TimeEntr <- as.numeric(transaction_par_troncons$TimeEntr)
  transaction_par_troncons$TimeSor <- as.numeric(transaction_par_troncons$TimeSor)
  transaction_par_troncons <- transaction_par_troncons[, c(1,4:(ncol(transaction_par_troncons)-1))]
  
  trx <- rbind(transaction_par_troncons, transaction_restant)
  trx$DOW <- as.numeric(trx$DOW)
  trx$WOY <- as.numeric(trx$WOY)
  trx <- tbl_df(trx)
  return(trx)
}
