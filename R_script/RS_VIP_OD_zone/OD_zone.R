#CREER LES TRONCONS
temp<- read.table("garesLatLng.csv", sep= ";", head = T)


plot(temp$Lng,temp$Lat)
ggplot(temp)+geom_point(aes(Lng,Lat))

A7 <- temp[temp$Autoroute=="A7",]
A8 <- temp[temp$Autoroute=="A8",]
A9 <- temp[temp$Autoroute=="A9",]

A7_8_9 <- rbind(A7,A8,A9)
ggplot(A7_8_9)+geom_point(aes(Lng,Lat))

A7_par_pk <- A7[order(A7[,6]),]
A8_par_pk <- A8[order(A8[,6]),]
A9_par_pk <- A9[order(A9[,6]),]

ID7 <- c(1:20)
test7 <- A7_par_pk
DébutCde7 <- test7$Cde[-21]
DébutLib7 <- test7$Lib[-21]
FinCde7 <- test7$Cde[-1]
FinLib7 <- test7$Lib[-1]
Longueur7 <- test7$PK[-1]-test7$PK[-21]
Troncons_A7 <- data.frame( ID_Troncon = ID7, Autoroute = c("A7"), Début = DébutLib7, Début_Code = DébutCde7, Fin = FinLib7, Fin_Code = FinCde7, Longueur = Longueur7 )


ID8 <- c(22:48)
test8 <- A8_par_pk
DébutCde8 <- test8$Cde[-28]
DébutLib8 <- test8$Lib[-28]
FinCde8 <- test8$Cde[-1]
FinLib8 <- test8$Lib[-1]
Longueur8 <- test8$PK[-1]-test8$PK[-28]
Troncons_A8 <- data.frame( ID_Troncon = ID8, Autoroute = c("A8"), Début = DébutLib8, Début_Code = DébutCde8, Fin = FinLib8, Fin_Code = FinCde8, Longueur = Longueur8 )

ID9 <- c(50:75)
test9 <- A9_par_pk
DébutCde9 <- test9$Cde[-27]
DébutLib9 <- test9$Lib[-27]
FinCde9 <- test9$Cde[-1]
FinLib9 <- test9$Lib[-1]
Longueur9 <- test9$PK[-1]-test9$PK[-27]
Troncons_A9 <- data.frame( ID_Troncon = ID9, Autoroute = c("A9"), Début = DébutLib9, Début_Code = DébutCde9, Fin = FinLib9, Fin_Code = FinCde9, Longueur = Longueur9 )


Nouvel78 <- c(ID = 21 , Autoroute = "A7/A8", Début = "LANCON", Début_Code = 25004220, Fin = "COUDOUX", Fin_Code = 25004278, Longueur = 7)
Troncons_A78 <- rbind(Troncons_A7,Nouvel78,Troncons_A8)
Nouvel79  <- c(ID = 49, Autoroute = "A9", Début = "ORANGE CENTRE", Début_Code = 25004210, Fin = "ROQUEMAURE", Fin_Code = 25004221, Longueur = 12)
Troncons_A789 <- rbind(Troncons_A78,Nouvel79,Troncons_A9)


Troncons_A789_inverse <- Troncons_A789[c(nrow(Troncons_A789):1),c(1,2,5,6,3,4,7)]
colnames(Troncons_A789_inverse) <- c("ID_Troncon","Autoroute","Début","Début_Code","Fin","Fin_Code","Longueur")
# Dans Troncons_a789 : A7 Vienne -> Coudoux ; A8  Coudoux -> La Turbie ; A9 Orange Centre -> Espagne
# Dans Troncons_a789_inverse : A7 Coudoux -> Vienne ; A8  La Turbie -> Coudoux ; A9 Espagne -> Orange Centre

#AJOUT DES LAT LONG:
Début_Lat <- vector(mode="integer",length = nrow(Troncons_A789))
Début_Lng <- vector(mode="integer",length = nrow(Troncons_A789))
Fin_Lat <- vector(mode="integer",length = nrow(Troncons_A789))
Fin_Lng <- vector(mode="integer",length = nrow(Troncons_A789))
for (i in 1 : nrow(Troncons_A789)){
  Début_Lat[i] <- temp$Lat[match(Troncons_A789[i,4],temp$Cde,nomatch=0)[match(Troncons_A789[i,4],temp$Cde,nomatch=0)>0]]
  Début_Lng[i] <- temp$Lng[match(Troncons_A789[i,4],temp$Cde,nomatch=0)[match(Troncons_A789[i,4],temp$Cde,nomatch=0)>0]]
  Fin_Lat[i] <- temp$Lat[match(Troncons_A789[i,6],temp$Cde,nomatch=0)[match(Troncons_A789[i,6],temp$Cde,nomatch=0)>0]]
  Fin_Lng[i] <- temp$Lng[match(Troncons_A789[i,6],temp$Cde,nomatch=0)[match(Troncons_A789[i,6],temp$Cde,nomatch=0)>0]]
}
Troncons_A789 <- cbind(Troncons_A789,Début_Lat,Début_Lng,Fin_Lat,Fin_Lng)


#COMPTER LES TRONCONS POUR UN CLIENT
t <-read.table("ID_OD_2014.csv",sep=",",head=T)

Client <- t[t$Label == 4888,]
Compteur <- vector(mode="integer",length = nrow(Troncons_A789))
Compteur_inverse <- vector(mode="integer",length = nrow(Troncons_A789))
Compteur_total <- vector(mode="integer",length = nrow(Troncons_A789))

for ( i in 1:nrow(Client)) { 
  
  if ((Client[i,2] %in% Troncons_A789[,4]  &  Client[i,3] %in% Troncons_A789[,6] )) {
    a <- match(Client[i,2],Troncons_A789[,4],nomatch=0)[match(Client[i,2],Troncons_A789[,4],nomatch=0)>0]
    b <- match(Client[i,3],Troncons_A789[,6],nomatch=0)[match(Client[i,3],Troncons_A789[,6],nomatch=0)>0]
    
    if ((Client[i,2] == 25004210) & temp$Autoroute[match(Client[i,3],temp$Cde,nomatch=0)[match(Client[i,3],temp$Cde,nomatch=0)>0]] == "A7"){ #Entrée = OC, sortie A7
      if (b>13){
        Compteur[14:b] <- Compteur[14:b]+1
      }
      else {
        Compteur[(b+1):13] <- Compteur[(b+1):13]+1
      }
    }
    else if ((Client[i,2] == 25004210) & temp$Autoroute[match(Client[i,3],temp$Cde,nomatch=0)[match(Client[i,3],temp$Cde,nomatch=0)>0]] == "A9"){#Entrée = OC, sortie A9
      Compteur[49:b] <- Compteur[49:b]+1
    }
    else if ((Client[i,3] == 25004210) & temp$Autoroute[match(Client[i,2],temp$Cde,nomatch=0)[match(Client[i,2],temp$Cde,nomatch=0)>0]] == "A7"){#Sortie = OC, Entrée A7
      if (a<13){
        Compteur[a:13] <- Compteur[a:13]+1
      }
      else {
        Compteur[14:(a-1)] <- Compteur[14:(a-1)] +1
      }
    }
    else if ((Client[i,3] == 25004210) & temp$Autoroute[match(Client[i,2],temp$Cde,nomatch=0)[match(Client[i,2],temp$Cde,nomatch=0)>0]] == "A9"){#Sortie = OC, Entrée A9
      Compteur[49:(a-1)] <- Compteur[49:(a-1)]+1
    }
    else if ( (temp$Autoroute[match(Client[i,2],temp$Cde,nomatch=0)[match(Client[i,2],temp$Cde,nomatch=0)>0]] == "A7" ) & ( temp$Autoroute[match(Client[i,3],temp$Cde,nomatch=0)[match(Client[i,3],temp$Cde,nomatch=0)>0]] == "A9" ) ){#Entrée A7 Sortie A9
      Compteur[a:13] <- Compteur[a:13]+1
      Compteur[49:b] <- Compteur[49:b]+1
    }
    else if ( (temp$Autoroute[match(Client[i,2],temp$Cde,nomatch=0)[match(Client[i,2],temp$Cde,nomatch=0)>0]] == "A9" ) & ( temp$Autoroute[match(Client[i,3],temp$Cde,nomatch=0)[match(Client[i,3],temp$Cde,nomatch=0)>0]] == "A7" ) ){#Entrée A9 Sortie A7
      Compteur[49:(a-1)] <- Compteur[49:(a-1)]+1
      Compteur[(b+1):13] <- Compteur[(b+1):13]+1
    }
    else {
      if (a<=b) {
        Compteur[a:b] <- Compteur[a:b] +1
      }
      else {
        Compteur[(b+1):(a-1)] <- Compteur[(b+1):(a-1)]+1
      }
    }
  }
  
  
  else if ( (Client[i,2] %in% Troncons_A789_inverse[,4]  & Client[i,3] %in% Troncons_A789_inverse[,6]) ) { #SORTIE = VIENNE ou ENTREE = ESPAGNE ou ENTREE = LA TURBIE PV
    c <- match(Client[i,2],Troncons_A789_inverse[,4],nomatch=0)[match(Client[i,2],Troncons_A789_inverse[,4],nomatch=0)>0]
    d <- match(Client[i,3],Troncons_A789_inverse[,6],nomatch=0)[match(Client[i,3],Troncons_A789_inverse[,6],nomatch=0)>0]
    if ((Client[i,2] == 25004210) & Client[i,3] == 25004201){#Entrée = OC, sortie = Vienne
      Compteur_inverse[63:75] <- Compteur_inverse[63:75]+1
    }
    else if ((Client[i,2] == 25004391) & Client[i,3] == 25004210){#Entrée = Espagne, sortie = OC
      Compteur_inverse[1:27] <- Compteur_inverse[1:27]+1
    }
    else if ( (temp$Autoroute[match(Client[i,2],temp$Cde,nomatch=0)[match(Client[i,2],temp$Cde,nomatch=0)>0]] == "A9" ) & ( temp$Autoroute[match(Client[i,3],temp$Cde,nomatch=0)[match(Client[i,3],temp$Cde,nomatch=0)>0]] == "A7" ) ){#Entrée A9 Sortie A7
      Compteur_inverse[c:27] <- Compteur_inverse[c:27] +1
      Compteur_inverse[63:d] <- Compteur_inverse[63:d] +1
    }
    else {
      if (c<=d) {
        Compteur_inverse[c:d] <- Compteur_inverse[c:d] +1
      }
      else {
        Compteur_inverse[(d+1):(c-1)] <- Compteur_inverse[(d+1):(c-1)]+1
      }
    }
  }
}


Compteur_total = Compteur + Compteur_inverse[length(Compteur_inverse):1]

Clientresult <- cbind(Troncons_A789 , Compteur_total)
plot(Compteur_total)

ggplot(Clientresult) + geom_point(aes(Début_Lng,Début_Lat)) +
  geom_segment(aes(x = Début_Lng, xend =Fin_Lng, y = Début_Lat, yend = Fin_Lat, size = Compteur_total)) +
  scale_size(range = c(0, 10))



#DECOMPOSER LES OD PAR TRONCONS :
Client$Sor <- as.numeric(as.character(Client$Sor))
Client$Entr <- as.numeric(as.character(Client$Entr))
Client_travail <- Client
ID_troncon <- vector(mode="integer",length = nrow(Client))
Pointeur_travail <- 1
for (i in 1:nrow(Client)){
  if ((Client[i,2] %in% Troncons_A789[,4]  &  Client[i,3] %in% Troncons_A789[,6] )) {
    a <-  match(Client[i,2],Troncons_A789[,4],nomatch=0)[match(Client[i,2],Troncons_A789[,4],nomatch=0)>0]
    b <-  match(Client[i,3],Troncons_A789[,6],nomatch=0)[match(Client[i,3],Troncons_A789[,6],nomatch=0)>0]
    
    if ((Client[i,2] == 25004210) & temp$Autoroute[match(Client[i,3],temp$Cde,nomatch=0)[match(Client[i,3],temp$Cde,nomatch=0)>0]] == "A7"){ #Entrée = OC, sortie A7
      if (b>13){
        for (j in 14:b ){
          newrow <- c(Client[1,1],Troncons_A789[j,4],Troncons_A789[j,6],0,0,0,Client[i,7],Client[i,8],Client[i,9],Client[i,10],0,Client[i,12],Client[i,13],0,0,0,Client[1,17])
          Client_travail = rbind(Client_travail[1:Pointeur_travail,],newrow,Client_travail[-(1:Pointeur_travail),])
          ID_troncon <- append(ID_troncon,Troncons_A789[j,1],Pointeur_travail)
          Pointeur_travail <- Pointeur_travail +1
        }
      }
      else {
        for (j in (b+1):13 ){
          newrow <- c(Client[1,1],Troncons_A789[j,4],Troncons_A789[j,6],0,0,0,Client[i,7],Client[i,8],Client[i,9],Client[i,10],0,Client[i,12],Client[i,13],0,0,0,Client[1,17])
          Client_travail = rbind(Client_travail[1:Pointeur_travail,],newrow,Client_travail[-(1:Pointeur_travail),])
          ID_troncon <- append(ID_troncon,Troncons_A789[j,1],Pointeur_travail)
          Pointeur_travail <- Pointeur_travail +1
        }
      }
    }
    else if ((Client[i,2] == 25004210) & temp$Autoroute[match(Client[i,3],temp$Cde,nomatch=0)[match(Client[i,3],temp$Cde,nomatch=0)>0]] == "A9"){#Entrée = OC, sortie A9
      for (j in 49:b ){
        newrow <- c(Client[1,1],Troncons_A789[j,4],Troncons_A789[j,6],0,0,0,Client[i,7],Client[i,8],Client[i,9],Client[i,10],0,Client[i,12],Client[i,13],0,0,0,Client[1,17])
        Client_travail = rbind(Client_travail[1:Pointeur_travail,],newrow,Client_travail[-(1:Pointeur_travail),])
        ID_troncon <- append(ID_troncon,Troncons_A789[j,1],Pointeur_travail)
        Pointeur_travail <- Pointeur_travail +1
      }
    }
    else if ((Client[i,3] == 25004210) & temp$Autoroute[match(Client[i,2],temp$Cde,nomatch=0)[match(Client[i,2],temp$Cde,nomatch=0)>0]] == "A7"){#Sortie = OC, Entrée A7
      if (a<13){
        for (j in a:13 ){
          newrow <- c(Client[1,1],Troncons_A789[j,4],Troncons_A789[j,6],0,0,0,Client[i,7],Client[i,8],Client[i,9],Client[i,10],0,Client[i,12],Client[i,13],0,0,0,Client[1,17])
          Client_travail = rbind(Client_travail[1:Pointeur_travail,],newrow,Client_travail[-(1:Pointeur_travail),])
          ID_troncon <- append(ID_troncon,Troncons_A789[j,1],Pointeur_travail)
          Pointeur_travail <- Pointeur_travail +1
        }
      }
      else {
        for (j in 14:(a-1) ){
          newrow <- c(Client[1,1],Troncons_A789[j,4],Troncons_A789[j,6],0,0,0,Client[i,7],Client[i,8],Client[i,9],Client[i,10],0,Client[i,12],Client[i,13],0,0,0,Client[1,17])
          Client_travail = rbind(Client_travail[1:Pointeur_travail,],newrow,Client_travail[-(1:Pointeur_travail),])
          ID_troncon <- append(ID_troncon,Troncons_A789[j,1],Pointeur_travail)
          Pointeur_travail <- Pointeur_travail +1
        }
      }
    }
    else if ((Client[i,3] == 25004210) & temp$Autoroute[match(Client[i,2],temp$Cde,nomatch=0)[match(Client[i,2],temp$Cde,nomatch=0)>0]] == "A9"){#Sortie = OC, Entrée A9
      for (j in 49:(a-1) ){
        newrow <- c(Client[1,1],Troncons_A789[j,4],Troncons_A789[j,6],0,0,0,Client[i,7],Client[i,8],Client[i,9],Client[i,10],0,Client[i,12],Client[i,13],0,0,0,Client[1,17])
        Client_travail = rbind(Client_travail[1:Pointeur_travail,],newrow,Client_travail[-(1:Pointeur_travail),])
        ID_troncon <- append(ID_troncon,Troncons_A789[j,1],Pointeur_travail)
        Pointeur_travail <- Pointeur_travail +1
      }
    }
    else if ( (temp$Autoroute[match(Client[i,2],temp$Cde,nomatch=0)[match(Client[i,2],temp$Cde,nomatch=0)>0]] == "A7" ) & ( temp$Autoroute[match(Client[i,3],temp$Cde,nomatch=0)[match(Client[i,3],temp$Cde,nomatch=0)>0]] == "A9" ) ){#Entrée A7 Sortie A9
      for (j in a:13 ){
        newrow <- c(Client[1,1],Troncons_A789[j,4],Troncons_A789[j,6],0,0,0,Client[i,7],Client[i,8],Client[i,9],Client[i,10],0,Client[i,12],Client[i,13],0,0,0,Client[1,17])
        Client_travail = rbind(Client_travail[1:Pointeur_travail,],newrow,Client_travail[-(1:Pointeur_travail),])
        ID_troncon <- append(ID_troncon,Troncons_A789[j,1],Pointeur_travail)
        Pointeur_travail <- Pointeur_travail +1
      }
      for (j in 49:b ){
        newrow <- c(Client[1,1],Troncons_A789[j,4],Troncons_A789[j,6],0,0,0,Client[i,7],Client[i,8],Client[i,9],Client[i,10],0,Client[i,12],Client[i,13],0,0,0,Client[1,17])
        Client_travail = rbind(Client_travail[1:Pointeur_travail,],newrow,Client_travail[-(1:Pointeur_travail),])
        ID_troncon <- append(ID_troncon,Troncons_A789[j,1],Pointeur_travail)
        Pointeur_travail <- Pointeur_travail +1
      }
    }
    else if ( (temp$Autoroute[match(Client[i,2],temp$Cde,nomatch=0)[match(Client[i,2],temp$Cde,nomatch=0)>0]] == "A9" ) & ( temp$Autoroute[match(Client[i,3],temp$Cde,nomatch=0)[match(Client[i,3],temp$Cde,nomatch=0)>0]] == "A7" ) ){#Entrée A9 Sortie A7
      for (j in 49:(a-1) ){
        newrow <- c(Client[1,1],Troncons_A789[j,4],Troncons_A789[j,6],0,0,0,Client[i,7],Client[i,8],Client[i,9],Client[i,10],0,Client[i,12],Client[i,13],0,0,0,Client[1,17])
        Client_travail = rbind(Client_travail[1:Pointeur_travail,],newrow,Client_travail[-(1:Pointeur_travail),])
        ID_troncon <- append(ID_troncon,Troncons_A789[j,1],Pointeur_travail)
        Pointeur_travail <- Pointeur_travail +1
      }
      for (j in (b+1):13 ){
        newrow <- c(Client[1,1],Troncons_A789[j,4],Troncons_A789[j,6],0,0,0,Client[i,7],Client[i,8],Client[i,9],Client[i,10],0,Client[i,12],Client[i,13],0,0,0,Client[1,17])
        Client_travail = rbind(Client_travail[1:Pointeur_travail,],newrow,Client_travail[-(1:Pointeur_travail),])
        ID_troncon <- append(ID_troncon,Troncons_A789[j,1],Pointeur_travail)
        Pointeur_travail <- Pointeur_travail +1
      }
    }
    else if (a<=b){
      for (j in a:b ){
        newrow <- c(Client[1,1],Troncons_A789[j,4],Troncons_A789[j,6],0,0,0,Client[i,7],Client[i,8],Client[i,9],Client[i,10],0,Client[i,12],Client[i,13],0,0,0,Client[1,17])
        Client_travail = rbind(Client_travail[1:Pointeur_travail,],newrow,Client_travail[-(1:Pointeur_travail),])
        ID_troncon <- append(ID_troncon,Troncons_A789[j,1],Pointeur_travail)
        Pointeur_travail <- Pointeur_travail +1
      }
    }
    else {
      for (j in (b+1):(a-1) ){
        newrow <- c(Client[1,1],Troncons_A789[j,4],Troncons_A789[j,6],0,0,0,Client[i,7],Client[i,8],Client[i,9],Client[i,10],0,Client[i,12],Client[i,13],0,0,0,Client[1,17])
        Client_travail = rbind(Client_travail[1:Pointeur_travail,],newrow,Client_travail[-(1:Pointeur_travail),])
        ID_troncon <- append(ID_troncon,Troncons_A789[j,1],Pointeur_travail)
        Pointeur_travail <- Pointeur_travail +1
      }
    }
  }
  
  else if ( (Client[i,2] %in% Troncons_A789_inverse[,4]  & Client[i,3] %in% Troncons_A789_inverse[,6]) ) { #SORTIE = VIENNE ou ENTREE = ESPAGNE ou ENTREE = LA TURBIE PV
    c <- match(Client[i,2],Troncons_A789_inverse[,4],nomatch=0)[match(Client[i,2],Troncons_A789_inverse[,4],nomatch=0)>0]
    d <- match(Client[i,3],Troncons_A789_inverse[,6],nomatch=0)[match(Client[i,3],Troncons_A789_inverse[,6],nomatch=0)>0]
    c <- 75 - c + 1
    d <- 75 - d + 1
    if ((Client[i,2] == 25004210) & Client[i,3] == 25004201){#Entrée = OC, sortie = Vienne
      for (j in 1:13 ){
        newrow <- c(Client[1,1],Troncons_A789[j,4],Troncons_A789[j,6],0,0,0,Client[i,7],Client[i,8],Client[i,9],Client[i,10],0,Client[i,12],Client[i,13],0,0,0,Client[1,17])
        Client_travail = rbind(Client_travail[1:Pointeur_travail,],newrow,Client_travail[-(1:Pointeur_travail),])
        ID_troncon <- append(ID_troncon,Troncons_A789[j,1],Pointeur_travail)
        Pointeur_travail <- Pointeur_travail +1
      }
    }
    else if ((Client[i,2] == 25004391) & Client[i,3] == 25004210){#Entrée = Espagne, sortie = OC
      for (j in 49:75 ){
        newrow <- c(Client[1,1],Troncons_A789[j,4],Troncons_A789[j,6],0,0,0,Client[i,7],Client[i,8],Client[i,9],Client[i,10],0,Client[i,12],Client[i,13],0,0,0,Client[1,17])
        Client_travail = rbind(Client_travail[1:Pointeur_travail,],newrow,Client_travail[-(1:Pointeur_travail),])
        ID_troncon <- append(ID_troncon,Troncons_A789[j,1],Pointeur_travail)
        Pointeur_travail <- Pointeur_travail +1
      }
    }
    else if ( (temp$Autoroute[match(Client[i,2],temp$Cde,nomatch=0)[match(Client[i,2],temp$Cde,nomatch=0)>0]] == "A9" ) & ( temp$Autoroute[match(Client[i,3],temp$Cde,nomatch=0)[match(Client[i,3],temp$Cde,nomatch=0)>0]] == "A7" ) ){#Entrée A9 Sortie A7
      for (j in 49:c ){
        newrow <- c(Client[1,1],Troncons_A789[j,4],Troncons_A789[j,6],0,0,0,Client[i,7],Client[i,8],Client[i,9],Client[i,10],0,Client[i,12],Client[i,13],0,0,0,Client[1,17])
        Client_travail = rbind(Client_travail[1:Pointeur_travail,],newrow,Client_travail[-(1:Pointeur_travail),])
        ID_troncon <- append(ID_troncon,Troncons_A789[j,1],Pointeur_travail)
        Pointeur_travail <- Pointeur_travail +1
      }
      for (j in d:13 ){
        newrow <- c(Client[1,1],Troncons_A789[j,4],Troncons_A789[j,6],0,0,0,Client[i,7],Client[i,8],Client[i,9],Client[i,10],0,Client[i,12],Client[i,13],0,0,0,Client[1,17])
        Client_travail = rbind(Client_travail[1:Pointeur_travail,],newrow,Client_travail[-(1:Pointeur_travail),])
        ID_troncon <- append(ID_troncon,Troncons_A789[j,1],Pointeur_travail)
        Pointeur_travail <- Pointeur_travail +1
      }
    }
    else {
      if (c<=d) {
        for (j in c:d) {
          newrow <- c(Client[1,1],Troncons_A789[j,4],Troncons_A789[j,6],0,0,0,Client[i,7],Client[i,8],Client[i,9],Client[i,10],0,Client[i,12],Client[i,13],0,0,0,Client[1,17])
          Client_travail = rbind(Client_travail[1:Pointeur_travail,],newrow,Client_travail[-(1:Pointeur_travail),])
          ID_troncon <- append(ID_troncon,Troncons_A789[j,1],Pointeur_travail)
          Pointeur_travail <- Pointeur_travail +1
        }
      }
      else {
        for (j in (d+1):(c-1) ){
          newrow <- c(Client[1,1],Troncons_A789[j,4],Troncons_A789[j,6],0,0,0,Client[i,7],Client[i,8],Client[i,9],Client[i,10],0,Client[i,12],Client[i,13],0,0,0,Client[1,17])
          Client_travail = rbind(Client_travail[1:Pointeur_travail,],newrow,Client_travail[-(1:Pointeur_travail),])
          ID_troncon <- append(ID_troncon,Troncons_A789[j,1],Pointeur_travail)
          Pointeur_travail <- Pointeur_travail +1
        }
      }
    }
  }
  Pointeur_travail <- Pointeur_travail +1
}
Client_travail <- cbind(Client_travail,ID_troncon)


#TEST SI LES DEUX DONNENET LE MEME RESULTAT