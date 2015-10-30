##########
##########
# SA_function_old.R
# first: 20150824
# codes for old functions


##########
##########
###  Old data_preparation

##########
##########
### EXPLANATION
# !!! action to be added
# ??? action with doubt

##########
##########
### Data Source
# ASF
# Escota
# BO
### Remarkable points
# csv
#   sep = "," | ";"
#   S.O.
#     Entr
#     TimeEntr
#     voieSortie

##########
##########
### Escota
### from csv to data.frame to tbl
input <- read.table("BDD_ESCOTA.csv", sep = ",", header = TRUE)
input <- tbl_df(input)

names(input) <- c("pays", "ste", "ID", "badge", "sEntr", "cEntr", "voieEntr", "DateEntr", "hEntr", "sSor", "cSor", "Voie", "hSor", "DateSor")

input <- input %>%
  mutate(
    Entr = 25000000 + sEntr * 1000 + cEntr,
    Sor = 25000000 + sSor * 1000 + cSor,
    Y = substr(DateSor, 1, 4), M = substr(DateSor, 5, 6), D = substr(DateSor, 7, 8),
    Date = as.Date(paste0(Y, "-", M, "-", D)),
    DOW = as.POSIXlt(Date)$wday,
    WOY = as.numeric(format(Date+3, "%U")),
    HH = hEntr %/% 10000, MM = ((hEntr %% 10000) %/% 100),
    TimeEntr = HH + MM / 60,
    HH = hSor %/% 10000, MM = ((hSor %% 10000) %/% 100),
    TimeSor = HH + MM / 60
  ) %>%
  select(ID, cEntr, Entr, Sor, Voie, Date, DOW, WOY, TimeEntr, TimeSor) 

input[input$cEntr == 0, ]$Entr <- 0 
# Final
input <- input %>% select(ID, Entr, Sor, Voie, Date, DOW, WOY, TimeEntr, TimeSor)


# get ID.ref

# ID.ref.old <-ID.ref
# names(ID.ref)[c(1,3,6)] <- c("Nom", "NOM", "ID")
# ID.ref <- ID.ref %>% select(Nom, ID)


# join Input & ID.ref
input.escota <-  inner_join(input, ID.ref)

##########
##########
### BO
# only in ASF
# S.O. always with a virtual Entr but no TimeEntr
input <- read.table("BDD_BO.csv", sep = ";", header = TRUE)
input <- tbl_df(input)

names(input) <- c("ID", "cEntr","cSor", "nEntr","nSor","DEntr", "DSor")

input <- input %>% 
  mutate(
    ID = substr(ID, 6, 12), # get the number of employee
    Entr = 25004000 + cEntr,
    Sor = 25004000 + cSor,
    Y = substr(DSor, 1, 4), M = substr(DSor, 5, 6),Day = substr(DSor, 7, 8),
    Date = as.Date(paste0(Y, "-", M, "-", Day)),
    DOW = as.POSIXlt(Date)$wday,
    WOY = as.numeric(format(Date+3, "%U")),
    HH = as.numeric(substr(DEntr, 9, 10)), MM = as.numeric(substr(DEntr, 11, 12)),
    TimeEntr = HH + MM / 60,
    HH = as.numeric(substr(DSor, 9, 10)), MM = as.numeric(substr(DSor, 11, 12)),
    TimeSor = HH + MM / 60
  ) %>%
  select(ID, cEntr, Entr, Sor, Date, DOW, WOY, TimeEntr, TimeSor)

# ??? S.O. TimeEntr = 0
input[is.na(input$TimeEntr), ]$TimeEntr <- 0
# !!! add voieSor, Nom
input$Voie <- 0
input$Nom <- "PM"

input.BO <- input %>% select(ID, Entr, Sor, Voie, Date, DOW, WOY, TimeEntr, TimeSor, Nom)

##########
##########
### PM
PM.escota <- input.escota %>% filter(Nom == "PM")
PM.ASF <- input.BO

PM.escota$Societe <- "ESCOTA"
PM.ASF$Societe <- "ASF"

PM <- rbind(PM.escota, PM.ASF)

PM.all <- PM

#Treating Yvan's data and Lisa's, Delphine's
yvan <- read.table( file = "BDD.Yvan.csv",sep = ";", head = T)
lisa <- read.table( file = "BDD.Lisa.csv",sep = ";", head = T)
delphine <- read.table( file = "BDD.Delphine.csv",sep = ";", head = T)
# temp <- tbl_df(rbind (yvan, lisa, delphine ))


###########
### add new users
temp2 <- read.table( file = "BDD.Eric.csv",sep = ";", head = T)

ID.ref <- read.table("ID.ref.csv", sep = ";", header = TRUE)
names(ID.ref) <- c("Nom", "Prenom", "NOM", "Ste", "N.badge", "ID", "EVA")
ID.ref <- ID.ref %>% slice(2:nrow(ID.ref))
ID.ref <- ID.ref %>% arrange(ID)

temp3 <- ID.ref %>% select(Nom, ID)

temp <- tbl_df(rbind(temp1, temp2 ))

names(temp) <- c("ID","Badge","SSor","GareSor","Voie","DateSor","HeureSor","SEntr","GareEntr","DateEntr","HeureEntr","VoieEntr")

temp <- read.table( file = "BDD.Laurent.csv",sep = ";", head = T) %>% tbl_df
names(temp) <- c("SSor","GareSor","Voie","DateSor","HeureSor","ID","Badge","SEntr","GareEntr","DateEntr","HeureEntr","VoieEntr")


temp <- temp %>%
  mutate(
    ID = paste0(ID,Badge),
    Entr = 25000000 + SEntr * 1000 + GareEntr,
    Sor = 25000000 + SSor * 1000 + GareSor,
    Y = substr(DateSor, 1, 4), M = substr(DateSor, 5, 6), D = substr(DateSor, 7, 8),
    Date = as.Date(paste0(Y, "-", M, "-", D)),
    DOW = as.POSIXlt(Date)$wday,
    WOY = as.numeric(format(Date+3, "%U")),
    HH = HeureEntr %/% 10000, MM = ((HeureEntr %% 10000) %/% 100),
    TimeEntr = HH + MM / 60,
    HH = HeureSor %/% 10000, MM = ((HeureSor %% 10000) %/% 100),
    TimeSor = HH + MM / 60
  ) %>%
  select(ID, GareEntr, Entr, Sor, Voie, Date, DOW, WOY, TimeEntr, TimeSor) 


temp[temp$GareEntr == 0, ]$Entr <- 0 
temp[temp$Entr != 0, ]$Voie <- 0 
# Final
temp <- temp %>% select(ID, Entr, Sor, Voie, Date, DOW, WOY, TimeEntr, TimeSor)

#Add Nom 
temp <- inner_join(temp,temp3)

# input.escota.v20150924 <- input.escota # 30125
input.escota <- rbind(input.escota, temp)

##########
##########
### all
input <- rbind(input.escota, input.BO)
input$ID <- input$Nom
input$Nom <- NULL
input$ID <- as.character(input$ID)

##########
##########
### remove unnecessary data.frame
# rm(delphine, lisa, yvan, yvanlisedelph)
# rm(temp, temp.L, temp1, temp2,temp1.L)
# rm(A7_par_pk, A8_par_pk, A9_par_pk, Troncons_A7, Troncons_A8, Troncons_A9)
# 
# rm(ind.model.00,ind.model.01,ind.model.02,ind.model.10,ind.model.11,ind.model.12,ind.model.20,ind.model.21,ind.model.22)
# rm(result.model.00,result.model.01,result.model.02,result.model.10,result.model.11,result.model.12,result.model.20,result.model.21,result.model.22)
# rm(test.model.00,test.model.01,test.model.02,test.model.10,test.model.11,test.model.12,test.model.20,test.model.21,test.model.22)
# rm(train.model.00,train.model.01,train.model.02,train.model.10,train.model.11,train.model.12,train.model.20,train.model.21,train.model.22)
# 
# rm(AfterDecompose, BeforeDecompose, Decompose, Fusion_OD_2)
# rm(t1,t2)
# rm(start.time,end.time,end.time.model.0,end.time.preparation, time.taken, time.taken.model.0, time.taken.preparation)
# rm(i,j,k, max.cluster, n.cluster,clus, order, temp.kmeans)
# 
# rm(VIP2_pour_modele, VIP2_pour_test, VIP2_pour_test_par_troncons)
# rm(VIP2_decompose,VIP2_espace, VIP2_espace_temps)


##########
##########
load("Sens_ref.RData")
load("JF.RData")

##########
##########
# prepare

# input from ASF
VIP3 <- VIP2
VIP3$Entr <- as.numeric(VIP3$Entr)
VIP3$Sor <- as.numeric(VIP3$Sor)
trx <- tbl_df(VIP3)
# remove Lng & Lat
trx <- trx[, -c(9:13)]
trx <- trx %>% filter(DOW < 7)

trx$Voie <- 0
# rm transaction in ESCOTA
trx <- trx %>% filter(Sor < 25005000)
trx[trx$ID=="PC", ]$ID <- "PCO"

# combine with transaction in ESCOTA
trx.ready <- rbind(trx, input)

# Ind.6months <- Ind.final
# result.6months <- temp
# result.6months %>% count(ID)
# 
# t1 <- result.6months %>% count(ID) %>% mutate(N1 =n, L1 = "6mois") %>% select(-n)
# t2 <- temp %>% count(ID) %>% mutate(N2 =n, L2 = "4mois") %>% select(-n)
# full_join(t1,t2) %>% print(n=30)

##########
# BDD.ESCOTA.v20150930
##########
t <- read.table("BDD.ESCOTA.v20150930.csv", sep = ";", header = TRUE) %>% tbl_df

names(t) <- c("pays", "ste", "ID", "porteur", "Badge","Nom",
              "sSor", "cSor", "Voie", "DateSor", "hSor",
              "sEntr", "cEntr", "DateEntr","hEntr",  "voieEntr")

t1 <- t %>%  
  mutate(
    ste = pays * 100 + ste,
    client = ID * 1e5 + porteur,
    Entr = 25000000 + sEntr * 1000 + cEntr,
    Sor = 25000000 + sSor * 1000 + cSor,
    Y = substr(DateSor, 1, 4), M = substr(DateSor, 5, 6), D = substr(DateSor, 7, 8),
    Date = as.Date(paste0(Y, "-", M, "-", D)),
    DOW = as.POSIXlt(Date)$wday,
    WOY = as.numeric(format(Date+3, "%U")),
    HH = hEntr %/% 10000, MM = ((hEntr %% 10000) %/% 100),
    TimeEntr = HH + MM / 60,
    HH = hSor %/% 10000, MM = ((hSor %% 10000) %/% 100),
    TimeSor = HH + MM / 60
  ) %>%
  select(Nom, ste, client, cEntr, Entr, Sor, Voie, Date, DOW, WOY, TimeEntr, TimeSor) 

t1[t1$cEntr == 0, ]$Entr <- 0 
t1[t1$cEntr != 0, ]$Voie <- 0 
t1 <- t1 %>% mutate(Nom = as.character(Nom))

count(t1, Nom, ste, client) %>% print(n = 21)

t1[t1$client == 819831400001,]$Nom <- "Philippe Bertreau"
t1[t1$client == 819831400004,]$Nom <- "Philippe Bertreau"
t1[t1$client == 901383300004,]$Nom <- "Nicolas Schwab"
t1[t1$client == 902781100004,]$Nom <- "Michel Domilici"

# Final
t1 <- t1 %>% select(-cEntr)

write.table(t1, file="BDDv0930.ESCOTA.csv", sep = ";", row.names = F, quote = F)
write.table(count(t1,Nom, ste, client ), file="BDDv0930.ref.ESCOTA.csv", sep = ";", row.names = F, quote = F)

B1 <- t1
ref.B1 <- count(t1,Nom, ste, client )
rm(t,t1)

ref.B1 <- ref.B1 %>% ungroup() %>%  transmute(ID = client, Ste = ste, Nom)
##########
# B0
##########
t <- ID.ref %>% 
  mutate( Nom = paste0(Prenom, " ",NOM)) %>%
  select(ID, Ste, N.badge, EVA, Nom)

t <- t %>% slice(1:24)

t <- t %>% transmute(ID = N.badge, Ste, EVA, Nom)
ref.B0 <- t %>% tbl_df

ref.B1$EVA <- NA
ref <- rbind(ref.B0,ref.B1)

B1 <- B1 %>% rename(ID = client, Ste = ste) 
B1 <- B1 %>% select(-c(Nom,Ste))

t <- trx.ready
t1 <- ID.ref %>% transmute(ID = Nom, EVA)
t2 <- left_join(t,t1)

t3 <- ref.B0 %>% select(ID,EVA)
t4 <- left_join(t2 %>% select(-ID), t3)

B0 <- t4 %>% select(-EVA)
rm(t,t1,t2,t3,t4)
BDD <- rbind(B1,B0)
BDD <- BDD %>% mutate(ID = as.character(ID))
ref <- ref %>% mutate(ID = as.character(ID))
st

##########
# BDD v20151001
##########

BDD <- rbind(BDD, B2)

t2 <- t %>% count(ID,Ste)
t2$Nom <- c("Philippe Bertreau", "Philippe Bori", "Nicolas Schwab", "Michel Domilici")
ref.B2 <- t2 %>%
  mutate(EVA = NA) %>%
  select(- n)
ref.B2 <- ref.B2[2,]

ref.B1 <- ref.B1 %>% filter(ID != 982890310001)

ref.B0[ref.B0$ID == 9828252,]$ID <- 982825210003
ref.B0[ref.B0$ID == 9827320,]$ID <- 982732000001
ref.B0[ref.B0$ID == 9801533,]$ID <- 980153310002

t[t$ID ==9828252, ]$ID <- 982825210003
t[t$ID ==9827320, ]$ID <- 982732000001
t[t$ID ==9801533, ]$ID <- 980153310002

ref <- rbind(ref.B0,ref.B1,ref.B2) %>%
  mutate(ID = as.character(ID))


t1 <- inner_join(t, ref)
BDD <- t1
write.table(BDD, file="BDD.v20151001.csv", sep = ";", row.names = F, quote = F)

temp1 <- BDD %>% 
  group_by(ID) %>%
  summarise(
    Dmin = min(Date), 
    Dmax = max(Date), 
    Ddiff = Dmax - Dmin + 1, 
    Day = n_distinct(Date),
    noPsg = n()
  )

temp2 <- left_join(ref, temp1)
write.table(temp2, file="Ref.ID.v20151001.csv", sep = ";", row.names = F, quote = F)


write.table(BDD, file="BDD.old.v20151001.csv", sep = ";", row.names = F, quote = F)

BDD.ready <- BDD %>% select(-c(Ste,EVA,Nom))





##########
##########
### List of Old functions
# Fusion_OD_2: 		link Lancon and La Barque
# BeforeDecompose: 	prepare data.frame before passing to Decompose
# AfterDecompose: 	prepare data.frame after Decompose
# Docompose: 	transform OD -> Troncon
# getNbClusterMax
# getNbClusterMax.aux

Fusion_OD_2 <- function ( Transactions) {
  # Connect OD Lancon - La Barque
  #
  #Args: ID  Entr  Sor  TimeEntr  TimeSor  DOW  WOY  Date  Voie 
  Transactions$Date <- as.character.Date(Transactions$Date)
  Transactions$TimeEntr <- as.numeric(Transactions$TimeEntr)
  Transactions$TimeSor <- as.numeric(Transactions$TimeSor)
  Transactions$Entr <- as.numeric(Transactions$Entr)
  Transactions$Sor <- as.numeric(Transactions$Sor)
  
  Transactions <- Transactions[, c("ID", "Entr", "Sor", "TimeEntr", "TimeSor", "DOW", "WOY", "Date", "Voie")]
  
  i <- 1
  #On commence par trier par ID et Date Heure chronologique
  Transactions <- Transactions %>%
    group_by(ID,Date) %>%
    arrange(TimeSor)
  
  while (i < nrow(Transactions)){
    if (Transactions$ID[i] == Transactions$ID[i+1]) {
      if (Transactions$Sor[i] == 25004220 ) { #Sortie = Lancon
        if((Transactions$Entr[i+1] == 25006002) & #Next Entrée = La Barque
           (Transactions$TimeSor[i] < Transactions$TimeEntr[i+1]) & #two transaction happened within an hour
           (Transactions$TimeEntr[i+1] < (Transactions$TimeSor[i] + 1.5))){ 
          if( i == 1){ #if the first row is concerned, we have to treat the case apart, about the fusion and the index.
            Transactions <- rbind(c(Transactions$ID[i],Transactions$Entr[i],Transactions$Sor[i+1],
                                    Transactions$TimeEntr[i],Transactions$TimeSor[i+1],
                                    Transactions$DOW[i],Transactions$WOY[i],Transactions$Date[i],Transactions$Voie[i]),
                                  Transactions[3:nrow(Transactions),])
            Transactions$TimeEntr <- as.numeric(Transactions$TimeEntr)
            Transactions$TimeSor <- as.numeric(Transactions$TimeSor)
          }
          else if (i == (nrow(Transactions)-1)){ #Same if it is for the last two rows.
            Transactions <- rbind(Transactions[1:(nrow(Transactions)-2),],
                                  c(Transactions$ID[i],Transactions$Entr[i],Transactions$Sor[i+1],
                                    Transactions$TimeEntr[i],Transactions$TimeSor[i+1],
                                    Transactions$DOW[i],Transactions$WOY[i],Transactions$Date[i],Transactions$Voie[i]))
            Transactions$TimeEntr <- as.numeric(Transactions$TimeEntr)
            Transactions$TimeSor <- as.numeric(Transactions$TimeSor)
          }
          else{ #General case.
            Transactions <- rbind(Transactions[1:(i-1),],
                                  c(Transactions$ID[i],Transactions$Entr[i],Transactions$Sor[i+1],
                                    Transactions$TimeEntr[i],Transactions$TimeSor[i+1],
                                    Transactions$DOW[i],Transactions$WOY[i],Transactions$Date[i],Transactions$Voie[i]),
                                  Transactions[(i+2):nrow(Transactions),])
            Transactions$TimeEntr <- as.numeric(Transactions$TimeEntr)
            Transactions$TimeSor <- as.numeric(Transactions$TimeSor)
          } 
        } # end of Lancon -> La Barque
      } # end of Sortie = Lancon
      else if (Transactions$Sor[i] == 25006002){ # Sortie = La Barque
        if((Transactions$Entr[i+1] == 25004220) & #Next Entrée = Lancon
           (Transactions$TimeSor[i] < Transactions$TimeEntr[i+1]) & #two transaction happened within an hour
           (Transactions$TimeEntr[i+1] < (Transactions$TimeSor[i] + 1.5))){
          if( i == 1){
            Transactions <- rbind(c(Transactions$ID[i],Transactions$Entr[i],Transactions$Sor[i+1],
                                    Transactions$TimeEntr[i],Transactions$TimeSor[i+1],
                                    Transactions$DOW[i],Transactions$WOY[i],Transactions$Date[i],Transactions$Voie[i]),
                                  Transactions[3:nrow(Transactions),])
            Transactions$TimeEntr <- as.numeric(Transactions$TimeEntr)
            Transactions$TimeSor <- as.numeric(Transactions$TimeSor)
          }
          else if (i == (nrow(Transactions)-1)){
            Transactions <- rbind(Transactions[1:(nrow(Transactions)-2),],
                                  c(Transactions$ID[i],Transactions$Entr[i],Transactions$Sor[i+1],
                                    Transactions$TimeEntr[i],Transactions$TimeSor[i+1],
                                    Transactions$DOW[i],Transactions$WOY[i],Transactions$Date[i],Transactions$Voie[i]))
            Transactions$TimeEntr <- as.numeric(Transactions$TimeEntr)
            Transactions$TimeSor <- as.numeric(Transactions$TimeSor)
          }
          else{
            Transactions <- rbind(Transactions[1:(i-1),],
                                  c(Transactions$ID[i],Transactions$Entr[i],Transactions$Sor[i+1],
                                    Transactions$TimeEntr[i],Transactions$TimeSor[i+1],
                                    Transactions$DOW[i],Transactions$WOY[i],Transactions$Date[i],Transactions$Voie[i]),
                                  Transactions[(i+2):nrow(Transactions),])
            Transactions$TimeEntr <- as.numeric(Transactions$TimeEntr)
            Transactions$TimeSor <- as.numeric(Transactions$TimeSor)
          }
        }# end of La Barque - Lancon
      }# end of Sortie = La Barque
    }
    i <- i + 1 
  } # end of while
  return (Transactions)
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

Decompose <- function ( Transactions ) {
	#DECOMPOSER LES TRAJETS PAR AUTOROUTE UNIQUE
	# Args:
	#		Transactions: ID, Entr, Sor, Date, DOW, WOY, TimeEntr, TimeSor, Voie
	# Returns:
	# 	Transactions: ID, ID_Troncon, Autoroute, Entr, Sor, KMS, Date, TimeEntr, TimeSor, DOW, WOY, Sens
  gares$Autoroute <- as.character(gares$Autoroute)
  Transactions$Entr <- as.numeric(as.character(Transactions$Entr))
  Transactions$Sor <- as.numeric(as.character(Transactions$Sor))
  Autoroute <- vector(mode = "character", length = nrow(Transactions))
  Transactions_decompose <- cbind(Transactions, Autoroute)
  Transactions_decompose$Autoroute <- as.character(Transactions_decompose$Autoroute)
  Transactions_restant <- Transactions
  Pointeur <- 1
  Pointeur_restant <- 1
  for (i in 1 : nrow(Transactions)){
    if (  ( (Transactions$Entr[i] %in% Troncons_A789[,4]) | (Transactions$Entr[i] %in% Troncons_A789[,6])  ) & 
          ( (Transactions$Sor[i] %in% Troncons_A789[,4]) | (Transactions$Sor[i] %in% Troncons_A789[,6])  )  ) {  ### if OD in A789
      if (gares$Autoroute[match(Transactions$Entr[i],gares$Cde)] !=  gares$Autoroute[match(Transactions$Sor[i],gares$Cde)] ) { #E=A7etS=A9 ou l'inverse
        if (Transactions$Entr[i] != 25004210 & Transactions$Sor[i] != 25004210){  # Entrée et sortie <> Orange Centre
          if(gares$Autoroute[match(Transactions$Entr[i],gares$Cde)] == "A7"){# E=A7 S=A9
            newrow1 <- c(Transactions$ID[i],Transactions$Entr[i],25004210,Transactions$TimeEntr[i],Transactions$TimeSor[i],Transactions$DOW[i],Transactions$WOY[i],Transactions$Voie[i],Transactions$Date[i],"A7")
            newrow2 <- c(Transactions$ID[i],25004210,Transactions$Sor[i],Transactions$TimeEntr[i],Transactions$TimeSor[i],Transactions$DOW[i],Transactions$WOY[i],Transactions$Voie[i],Transactions$Date[i],"A9")
            Transactions_decompose = rbind(Transactions_decompose[1:Pointeur,],newrow1,newrow2,Transactions_decompose[-(1:Pointeur),])
            Pointeur <- Pointeur +2
          }
          else if(gares$Autoroute[match(Transactions$Entr[i],gares$Cde)] == "A9"){# E=A9 S=A7
            newrow1 <- c(Transactions$ID[i],Transactions$Entr[i],25004210,Transactions$TimeEntr[i],Transactions$TimeSor[i],Transactions$DOW[i],Transactions$WOY[i],Transactions$Voie[i],Transactions$Date[i],"A9")
            newrow2 <- c(Transactions$ID[i],25004210,Transactions$Sor[i],Transactions$TimeEntr[i],Transactions$TimeSor[i],Transactions$DOW[i],Transactions$WOY[i],Transactions$Voie[i],Transactions$Date[i],"A7")
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
      } ### end of if OD is in A789
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
  Transactions_par_troncons <- data.frame(ID = "", Entr= 0, Sor=0, Date = 0, TimeEntr = 0, TimeSor =0, DOW=0, WOY=0,Voie = 0)
  Transactions_par_troncons$ID <- as.character(Transactions_par_troncons$ID)
  Transactions_decompose$Date <- as.character(Transactions_decompose$Date)
  Transactions_par_troncons$Date <- as.character(Transactions_par_troncons$Date)
  
  for (i in 1:nrow(Transactions_decompose)){
    if ( Transactions_decompose$Autoroute[i] == "A7"){
      entree <- match(Transactions_decompose$Entr[i],A7_par_pk$Cde)
      sortie <- match(Transactions_decompose$Sor[i],A7_par_pk$Cde)
      if ( entree < sortie ) { # SENS 1
        for ( j in entree : (sortie-1) ){
          Transactions_par_troncons <- rbind(Transactions_par_troncons,c(Transactions_decompose$ID[i],A7_par_pk$Cde[j],A7_par_pk$Cde[j+1],Transactions_decompose$Date[i],Transactions_decompose$TimeEntr[i],Transactions_decompose$TimeSor[i],Transactions_decompose$DOW[i],Transactions_decompose$WOY[i],Transactions_decompose$Voie[i]))
        }
      }
      else{ #SENS 2
        for ( j in entree : (sortie+1) ){
          Transactions_par_troncons <- rbind(Transactions_par_troncons,c(Transactions_decompose$ID[i],A7_par_pk$Cde[j],A7_par_pk$Cde[j-1],Transactions_decompose$Date[i],Transactions_decompose$TimeEntr[i],Transactions_decompose$TimeSor[i],Transactions_decompose$DOW[i],Transactions_decompose$WOY[i],Transactions_decompose$Voie[i]))
        }
      }
    }
    else if ( Transactions_decompose$Autoroute[i] == "A8"){
      entree <- match(Transactions_decompose$Entr[i],A8_par_pk$Cde)
      sortie <- match(Transactions_decompose$Sor[i],A8_par_pk$Cde)
      if ( entree < sortie ) { # SENS 1
        for ( j in entree : (sortie-1) ){
          Transactions_par_troncons <- rbind(Transactions_par_troncons,c(Transactions_decompose$ID[i],A8_par_pk$Cde[j],A8_par_pk$Cde[j+1],Transactions_decompose$Date[i],Transactions_decompose$TimeEntr[i],Transactions_decompose$TimeSor[i],Transactions_decompose$DOW[i],Transactions_decompose$WOY[i],Transactions_decompose$Voie[i]))
        }
      }
      else{ #SENS 2
        for ( j in entree : (sortie+1) ){
          Transactions_par_troncons <- rbind(Transactions_par_troncons,c(Transactions_decompose$ID[i],A8_par_pk$Cde[j],A8_par_pk$Cde[j-1],Transactions_decompose$Date[i],Transactions_decompose$TimeEntr[i],Transactions_decompose$TimeSor[i],Transactions_decompose$DOW[i],Transactions_decompose$WOY[i],Transactions_decompose$Voie[i]))
        }
      }
    }
    else if ( Transactions_decompose$Autoroute[i] == "A9"){
      entree <- match(Transactions_decompose$Entr[i],A9_par_pk$Cde)
      sortie <- match(Transactions_decompose$Sor[i],A9_par_pk$Cde)
      if ( entree < sortie ) { # SENS 1
        for ( j in entree : (sortie-1) ){
          Transactions_par_troncons <- rbind(Transactions_par_troncons,c(Transactions_decompose$ID[i],A9_par_pk$Cde[j],A9_par_pk$Cde[j+1],Transactions_decompose$Date[i],Transactions_decompose$TimeEntr[i],Transactions_decompose$TimeSor[i],Transactions_decompose$DOW[i],Transactions_decompose$WOY[i],Transactions_decompose$Voie[i]))
        }
      }
      else{ #SENS 2
        for ( j in entree : (sortie+1) ){
          Transactions_par_troncons <- rbind(Transactions_par_troncons,c(Transactions_decompose$ID[i],A9_par_pk$Cde[j],A9_par_pk$Cde[j-1],Transactions_decompose$Date[i],Transactions_decompose$TimeEntr[i],Transactions_decompose$TimeSor[i],Transactions_decompose$DOW[i],Transactions_decompose$WOY[i],Transactions_decompose$Voie[i]))
        }
      }
    }
  }
  Transactions_par_troncons <- Transactions_par_troncons[-1,]
  
  
  #Rajouter demi trajet LANCON LA BARQUE
  Pointeur <- 1
  for (i in 1:nrow(Transactions_par_troncons)){
    if ( Transactions_par_troncons$Sor[Pointeur]==25004220){ # (A7 -> Lancon)
      newrow1 <- c(Transactions_par_troncons$ID[Pointeur],25004220,25004278,Transactions_par_troncons$Date[Pointeur],Transactions_par_troncons$TimeEntr[Pointeur],Transactions_par_troncons$TimeSor[Pointeur],Transactions_par_troncons$DOW[Pointeur],Transactions_par_troncons$WOY[Pointeur],Transactions_par_troncons$Voie[Pointeur])
      newrow2 <- c(Transactions_par_troncons$ID[Pointeur],25004278,25004279,Transactions_par_troncons$Date[Pointeur],Transactions_par_troncons$TimeEntr[Pointeur],Transactions_par_troncons$TimeSor[Pointeur],Transactions_par_troncons$DOW[Pointeur],Transactions_par_troncons$WOY[Pointeur],Transactions_par_troncons$Voie[Pointeur])
      Transactions_par_troncons <- rbind(Transactions_par_troncons[(1:Pointeur),],newrow1,newrow2,Transactions_par_troncons[-(1:Pointeur),])
      Pointeur <- Pointeur + 2
    } 
    if (Transactions_par_troncons$Entr[Pointeur]==25004220){ # (Lancon -> A7)
      newrow1 <- c(Transactions_par_troncons$ID[Pointeur],25004279,25004278,Transactions_par_troncons$Date[Pointeur],Transactions_par_troncons$TimeEntr[Pointeur],Transactions_par_troncons$TimeSor[Pointeur],Transactions_par_troncons$DOW[Pointeur],Transactions_par_troncons$WOY[Pointeur],Transactions_par_troncons$Voie[Pointeur])
      newrow2 <- c(Transactions_par_troncons$ID[Pointeur],25004278,25004220,Transactions_par_troncons$Date[Pointeur],Transactions_par_troncons$TimeEntr[Pointeur],Transactions_par_troncons$TimeSor[Pointeur],Transactions_par_troncons$DOW[Pointeur],Transactions_par_troncons$WOY[Pointeur],Transactions_par_troncons$Voie[Pointeur])
      Transactions_par_troncons <- rbind(Transactions_par_troncons[(1:Pointeur),],newrow1,newrow2,Transactions_par_troncons[-(1:Pointeur),])
      Pointeur <- Pointeur + 2
    }
    if (Transactions_par_troncons$Sor[Pointeur] == 25006002){ # (La Barque -> A8)
      newrow1 <- c(Transactions_par_troncons$ID[Pointeur],25004279,25006001,Transactions_par_troncons$Date[Pointeur],Transactions_par_troncons$TimeEntr[Pointeur],Transactions_par_troncons$TimeSor[Pointeur],Transactions_par_troncons$DOW[Pointeur],Transactions_par_troncons$WOY[Pointeur],Transactions_par_troncons$Voie[Pointeur])
      newrow2 <- c(Transactions_par_troncons$ID[Pointeur],25006001,25006080,Transactions_par_troncons$Date[Pointeur],Transactions_par_troncons$TimeEntr[Pointeur],Transactions_par_troncons$TimeSor[Pointeur],Transactions_par_troncons$DOW[Pointeur],Transactions_par_troncons$WOY[Pointeur],Transactions_par_troncons$Voie[Pointeur])
      newrow3 <- c(Transactions_par_troncons$ID[Pointeur],25006080,25006002,Transactions_par_troncons$Date[Pointeur],Transactions_par_troncons$TimeEntr[Pointeur],Transactions_par_troncons$TimeSor[Pointeur],Transactions_par_troncons$DOW[Pointeur],Transactions_par_troncons$WOY[Pointeur],Transactions_par_troncons$Voie[Pointeur])
      Transactions_par_troncons <- rbind(Transactions_par_troncons[(1:Pointeur),],newrow1,newrow2,newrow3,Transactions_par_troncons[-(1:Pointeur),])
      Pointeur <- Pointeur + 3
    }
    if (Transactions_par_troncons$Entr[Pointeur] == 25006002){ # (A8 -> La Barque)
      newrow1 <- c(Transactions_par_troncons$ID[Pointeur],25006002,25006080,Transactions_par_troncons$Date[Pointeur],Transactions_par_troncons$TimeEntr[Pointeur],Transactions_par_troncons$TimeSor[Pointeur],Transactions_par_troncons$DOW[Pointeur],Transactions_par_troncons$WOY[Pointeur],Transactions_par_troncons$Voie[Pointeur])
      newrow2 <- c(Transactions_par_troncons$ID[Pointeur],25006080,25006001,Transactions_par_troncons$Date[Pointeur],Transactions_par_troncons$TimeEntr[Pointeur],Transactions_par_troncons$TimeSor[Pointeur],Transactions_par_troncons$DOW[Pointeur],Transactions_par_troncons$WOY[Pointeur],Transactions_par_troncons$Voie[Pointeur])
      newrow3 <- c(Transactions_par_troncons$ID[Pointeur],25006001,25004279,Transactions_par_troncons$Date[Pointeur],Transactions_par_troncons$TimeEntr[Pointeur],Transactions_par_troncons$TimeSor[Pointeur],Transactions_par_troncons$DOW[Pointeur],Transactions_par_troncons$WOY[Pointeur],Transactions_par_troncons$Voie[Pointeur])
      Transactions_par_troncons <- rbind(Transactions_par_troncons[(1:Pointeur),],newrow1,newrow2,newrow3,Transactions_par_troncons[-(1:Pointeur),])
      Pointeur <- Pointeur + 3
    }
    Pointeur <- Pointeur + 1
  }
  
  return ( list(result = Transactions_par_troncons,rest = Transactions_restant) )
}

getNbClusterMax <- function (Transactions){
# GetNbClusterMax :
# Find the number of maximas of the density (TimeSor over 24h) -> nb max of clusters for this person
# Input : Trx of classical structure : ID & TimeSor required
# OUtput : a DataFrame of two columns : ID (unique) and the number max of cluster associated
  result <- Transactions %>% 
    group_by ( ID ) %>%
    summarise ( nbclus = getNbClusterMax.aux(TimeSor))
  return(result)
}

getNbClusterMax.aux <- function (Timevector){
  dens <- density(Timevector, n = 64, from = 0 , to = 24)
  y <- dens$y
  xz <- as.zoo(y)
  res <- rollapply(xz, 3, function(y) which.max(y)==2)
  nbmax <- sum( c(F,res,F) & ( y >= 0.01)) #???
  return(nbmax)
} 

##########
### Train Model
### Explanation of Models
# return:
#	result.model.##:  	ID Entr Sor DOW Tmin Tmax Model
#	test.model.##:			ID Entr SOr Date DOW WOY TimeEntr TimeSor result
#	ind.model.##:				ID Ind1 Ind2 Ind3 Ind Model
###
# regardless of DOW: make difference between weekdays and weekends
# consider DOW: treat each day of week separately 
###
# Model 0: Benchmark
#		Model 00: regardless of DOW		done
#		Model 01: weekday & weekend		done
#		Model 02: consider DOW				done
# Model 1: Time - Space
# 	Model 10: regardless of DOW		done
# 	Model 11: weekday & weekend   done
# 	Model 12: consider DOW				done
# Model 2: Space - Time
# 	Model 20: regardless of DOW		done
# 	Model 21: weekday & weekend   done
# 	Model 22: consider DOW				done
###
# ### model.00: Benchmark - regardless of DOW
# result.model.00 <- Model(train, 0)
# ### model.01: Benchmark - weekdays & weekends
# result.model.01 <- Model(train, 1)
# ### model.02: Benchmark - consider DOW
# result.model.02 <- Model(train, 2)
# 
# test.model.00 <- GetResult(test, result.model.00)
# test.model.00$Model <- 00
# ind.model.00 <- GetInd(test.model.00, result.model.00)
# ind.model.00$Model <- 00
# ### evalutaion model.01
# test.model.01 <- GetResult(test, result.model.01)
# test.model.01$Model <- 01
# ind.model.01 <- GetInd(test.model.01, result.model.01)
# ind.model.01$Model <- 01
# ### evalutaion model.02
# test.model.02 <- GetResult(test, result.model.02)
# test.model.02$Model <- 02
# ind.model.02 <- GetInd(test.model.02, result.model.02)
# ind.model.02$Model <- 02
# 
# Ind <- rbind(ind.model.00, ind.model.01, ind.model.02, 
#              ind.model.10, ind.model.11, ind.model.12, 
#              ind.model.20, ind.model.21, ind.model.22)
# 
# ggplot(Ind) + 
#   geom_point(aes(Model, Ind1, col = "% de trajets réels prédits")) + 
#   # geom_point(aes(Model, Ind2, col = "Ind2")) + 
#   geom_point(aes(Model, Ind3, col = "% de fausse alerts")) +
#   facet_wrap(~ID) +
#   labs(y = " Indicator") +
#   theme(legend.title = element_blank())
# 
# #temp <- Ind
# cbind(Ind, temp) %>% filter(Model < 3)