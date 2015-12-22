library(dplyr)
library(rmarkdown)
library(ggplot2)
library(gridExtra)

##########
### JF
##########
JF <- read.table("JF_2014-2015-2016.csv", header = TRUE, sep = ",") %>% tbl_df()
JF <- JF %>% filter(Year > 2014) 
ggplot(JF) + geom_bar(aes(DOW, fill = as.factor(JF)), binwidth=1) + xlim(c(0,7))
ggplot(JF) + geom_bar(aes(WOY, fill = as.factor(JF)), binwidth=1) + xlim(c(0,53))
ggplot(JF) + geom_tile(aes(Year*100+WOY, DOW, fill = as.factor(JF)))

JF <- JF %>% select(Date,DOW, JF)
JF$Date <- as.Date(as.character(JF$Date))
save(JF, file="JF.RData")

##########
### Old_viz_gares: update with Ref_gares.csv
##########
# g1 <- read.table("Gares_1.csv", header = T, sep = ";", dec = ",") %>% tbl_df()
# names(g1) <- c("Ste", "Lib", "Autoroute", "Cde", "Sens", "PK", "Lat", "Lng")
# 
# g2 <- read.table("Gares_2.csv", header = T, sep = ";") %>% tbl_df()
# names(g2) <- c("Cde", "Lib")
# g2 <- g2 %>% mutate(Ste = substr(Cde, 4,5))
# 
# g3 <- read.table("Gares_Cofiroute.csv", header = T, sep = ";") %>% tbl_df()
# names(g3) <- c("Ste", "Gare", "Autoroute", "Lib_complete", "PK", "INDBAR", "DATMES", "Cde", "Lib", "Lib_commercial" )
# 
# rm(g1, g2, g3)

##########
### Old_LngLat: update with Ref_gares.csv
##########
gares.LngLat <- read.table("garesLngLat.csv", header = T, sep = ",") %>% tbl_df()
names(gares.LngLat)[1] <- "Ste"

GetLngLat <- function(transaction) {
  # Add longitude and lattitude to the transaction
  # Args:
  #	  tansaction:	Entr, Sor, ...
  # Returns:
  #	  tansaction:	Entr, Sor, ..., ELng, ELat, SLng, SLat
  
  ### !!! to be removed
  #   transaction$Entr <- as.numeric(transaction$Entr)
  #   transaction$Sor <- as.numeric(transaction$Sor)
  cde <- gares.LngLat %>% transmute(Entr = Cde, ELng = Lng, ELat = Lat)
  transaction <- left_join(transaction, cde, by = "Entr")
  names(cde) <- c("Sor", "SLng", "SLat")
  transaction <- left_join(transaction, cde, by = "Sor")
  return(transaction)	
}

##########
### OLD_ESCOTA - Ref sur les voies: replace with new ESCOTA table
##########
ESCOTA.ref <- read.table("Gares_ESCOTA_ref.csv", header = T, sep = ";") %>% tbl_df()
names(ESCOTA.ref)[2] <- "ES"

ESCOTA.ref <- ESCOTA.ref %>% 
  filter(ES %in% c("E", "S")) %>%
  mutate(voie = as.numeric(as.character(voie))) %>%
  filter(!is.na(voie))

ESCOTA.temp <- ESCOTA.ref %>% distinct(gare, ES) %>% transmute(gare, ES, Cde = 25006000 + gare)

##########
### OLD_ESCOTA - Ref sur Tarif: replace with new ESCOTA table
##########
ESCOTA.SF <-  read.table("SF_2014.csv", header = T,  sep = ";", dec = ".") %>% tbl_df()
tSF.E <- ESCOTA.SF %>% distinct(Entr) %>% transmute(gare = Entr, Lib = Entree, ES = "E" , Cde = 25006000 + Entr) 
tSF.S <- ESCOTA.SF %>% distinct(Sor) %>% transmute(gare = Sor, Lib = Sortie, ES = "S" , Cde = 25006000 + Sor) 
tSF <- rbind(tSF.E, tSF.S)
# ggplot(tSF) + geom_bar(aes(gare, fill = as.factor(ES)), binwidth = 1)
tSF$Sys <- "F"

ESCOTA.SO <-  read.table("SO_2014.csv", header = T,  sep = ";", dec = ".") %>% tbl_df()
tSO <- ESCOTA.SO %>% transmute(gare = Cde, Lib, Cde = 25006000 + gare)
tSO$ES <- "O"
tSO$Sys <-"O"

tTarif <- rbind(tSF, tSO)
temp <- tTarif %>% distinct(gare) %>% select(gare, Lib, Cde, Sys)
ESCOTA.temp <- left_join(ESCOTA.temp, temp) # get Sys

# ggplot(t4) + geom_tile(aes(gare,ES, fill = ES)) + facet_wrap(~Sys, ncol = 1)

ESCOTA.temp$es <- 1
ESCOTA.temp$es[ESCOTA.temp$ES == "S"] <- 10
ESCOTA.temp <- ESCOTA.temp %>% group_by(Cde, gare, Lib, Sys) %>% summarise(es = sum(es))

ESCOTA.temp$ES <- "ES"
ESCOTA.temp$ES[ESCOTA.temp$es == 1] <- "E"
ESCOTA.temp$ES[ESCOTA.temp$es == 10] <- "S"

rm(tSF, tSF.E, tSF.S, tSO, tTarif)

temp <- gares.LngLat %>% filter(Ste == 6) %>% select(Autoroute, Cde, PK, Lng, Lat) 
ESCOTA.temp <- left_join(ESCOTA.temp, temp)

ESCOTA.temp <- ESCOTA.temp %>% ungroup() %>% select(-es)

##########
### gares.ESCOTA
##########
ESCOTA <- ESCOTA.temp
rm(ESCOTA.temp)

ggplot(ESCOTA,aes(Lng, Lat)) + geom_point(aes(col = as.factor(Sys)), size = 3, alpha = .5) +
  geom_text(aes(label = gare), hjust=1, vjust=0)

##########
### get Sens
##########
tSF <- ESCOTA.SF %>% transmute(Entr = 25006000 + Entr,
                         Sor = 25006000 + Sor,
                         Entree, Sortie)  
tSF <- GetLngLat(tSF)

ggplot(tSF) + 
  geom_segment(aes(x=ELng, xend = SLng, y = ELat, yend = SLat), alpha = .1) +
  geom_point(data = gares.ESCOTA, aes(Lng, Lat, col = as.factor(Autoroute)), size = 3, alpha = .5)

t <- tSF %>% select(Entr, Sor)
t1 <- gares.ESCOTA %>% transmute(Entr = Cde, AE = Autoroute, PKE = PK)
t <- left_join(t, t1)
t1 <- gares.ESCOTA %>% transmute(Sor = Cde, AS = Autoroute, PKS = PK)
t <- left_join(t, t1)

temp <- count(t, AE, AS)

t$SensEntr <- 1
t$SensSor <- 1

# case 1: AE == AS
t1 <- t %>% 
  filter(AE == AS) %>%
  mutate(SensEntr = ifelse(PKE < PKS, 1, 2),
         SensSor = SensEntr)

# case 2: A52 - A57 
t2 <- t %>%
  filter( (AE == "A52" & AS == "A57") |
          (AE == "A57" & AS == "A52") 
         ) %>%
  mutate(SensEntr = ifelse(AE == "A52", 2, 1), 
         SensSor = SensEntr)

# temp <- tTarif %>% distinct(gare)
# names(temp)[2] <- "Lib_Tarif"
# t1 <- gares.LngLat %>% filter(Societe == 6) %>% transmute(Cde, Lib)
# temp1 <- full_join(t1, temp)
# result: LngLat has 3 more gares 
# 22 ANTIBES PV NORD 
# 23 ANTIBES EST SORTIE
# 80 LA BARQUE ENTREES

# case 3 : A52 - A520
t3 <- t %>%
  filter(AE %in% c("A520") |
         AS %in% c("A520")) %>%
  mutate(SensEntr = 2, 
         SensSor = 1)

temp.sens <- rbind(t1,t2,t3)

# case 4: A52 -> A8
t1 <- t %>% 
  filter(AE =="A52" & AS == "A8") %>%
  mutate(SensEntr = 2,
         SensSor = ifelse(PKS < 29, 2, 1))

# case 5: A8 -> A52
t2 <- t %>% 
  filter(AE =="A8" & AS == "A52") %>%
  mutate(SensSor = 1,
         SensEntr = ifelse(PKE < 29, 1, 2))

# case 6: A57 -> A8
t3 <- t %>% 
  filter(AE =="A57" & AS == "A8") %>%
  mutate(SensEntr = 1,
         SensSor = ifelse(PKS < 98, 2, 1))

# case 5: A8 -> A57
t4 <- t %>% 
  filter(AE =="A8" & AS == "A57") %>%
  mutate(SensSor = 2,
         SensEntr = ifelse(PKE < 98, 1, 2))

##########
### SF
##########
temp.sens <- rbind(temp.sens,t1,t2,t3,t4)
ESCOTA.sens.SF <- temp.sens
rm(t, tSF, t1,t2,t3,t4,temp.sens, temp)

##########
### SO
##########
tSO <- ESCOTA.SO %>% transmute(gare = Cde, Lib) 
tSO <- left_join(tSO, ESCOTA %>% select(Cde, gare, Autoroute))

t <- ESCOTA.ref %>% 
  select(gare, ES, voie)

tSO <- left_join(tSO, t)


# case 1:
# Canet de mereuil, Antibes ouest,  Saint Isidore Ech Ouest, La Turibe Ech Est
# Ceux dont les voies >= 20 sont en sens 1
t1 <- tSO %>%
  filter(gare %in% c(1,14,19,26)) %>%
  mutate(Sens = ifelse(voie >=20, 1, 2))

# case 2:
# Fréjus, Les Adrets, Antibes PV, Antibes Est, Cagnes Est, Saint Isidore ech Est, Saint Isidore PV, La Turbie PV
# Ceux dont les voies >= 20 sont en sens 2
t2 <- tSO %>%
  filter(gare %in% c(10,11,12,13,16,20,21,27)) %>%
  mutate(Sens = ifelse(voie >=20, 2, 1))

# case 3:
# 17,24: Sens = 1
# 15: Sens = 2
t3 <- tSO%>%
  filter(gare %in% c(15,17,24)) %>%
  mutate(Sens = ifelse(gare == 15, 2, 1))

# case4:
# not in A8
t4 <- tSO%>%
  filter(Autoroute != "A8") %>%
  mutate(Sens = 0)

ESCOTA.sens.SO <- rbind(t1,t2,t3,t4)
rm(t,t1,t2,t3,t4, tSO)

##########
### OLD_ESCOTA.sens: replace with new ESCOTA table
##########
t1 <- ESCOTA.sens.SF %>%
  select(Entr, Sor, SensEntr, SensSor) %>%
  mutate(Voie = 0)

t2 <- ESCOTA.sens.SO %>% 
  transmute (Entr = 0, Sor = Cde, Voie = voie, SensEntr = 0, SensSor = Sens)

ESCOTA.sens <- rbind(t1,t2)
rm(t1,t2, ESCOTA.sens.SF, ESCOTA.sens.SO)
         
##########
### ASF.sens
##########
temp <- read.table("export_trjtsns_asf.csv", sep = ";", header = TRUE) %>% tbl_df()
names(temp) <- c("E1","E2","E3","EA","SensEntr",
                 "S1","S2","S3","SA","SensSor")
temp <- temp %>% 
  transmute(Entr = E1 * 100000 + E2 * 1000 + as.numeric(as.character(E3)), 
            SensEntr, SteEntr = E2,
            Sor = S1 * 100000 + S2 * 1000 + as.numeric(as.character(S3)), 
            SensSor, SteSor = S2) %>%
  filter(!is.na(Entr) & !is.na(Sor))

temp %>% group_by(SteEntr) %>% summarise(n = n_distinct(Entr))
temp %>% group_by(SteSor) %>% summarise(n = n_distinct(Sor))

ASF.sens <- temp
rm(temp)

ASF.sens <- ASF.sens %>%
  transmute(Entr, Sor, SensEntr, SensSor, Voie = 0)
rm(temp)

##########
### sens
##########
sens <- rbind(ASF.sens, ESCOTA.sens)
save(sens, file="Sens_ref.RData")
write.table(sens,"Ref_sens.csv",sep=";",row.name=FALSE,quote=FALSE)
write.table(JF,"Ref_JF.csv",sep=";",row.name=FALSE,quote=FALSE)
rm(ASF.sens, ESCOTA.sens)
rm(t,t3,t5,temp)

##########
### OLD_Prepare for PlugIt
##########
### SF
t1 <- sens %>% filter(Entr != 0) %>% select(Entr, Sor)
t <- gares.LngLat %>% transmute(Entr = Cde, Elng = Lng, Elat = Lat)
t1 <- inner_join(t1,t)
t <- gares.LngLat %>% transmute(Sor = Cde, Slng = Lng, Slat = Lat)
t1 <- inner_join(t1,t)
trajetSF <- t1 %>% mutate(OD = paste0(Entr,"-",Sor))
write.table(trajetSF,"trajetSF.csv",sep=";",row.name=FALSE,quote=FALSE)

###SO
t <- gares.LngLat %>% transmute(Sor = Cde, Autoroute, PK)
t1 <- sens %>% filter(Entr == 0) %>% transmute(Entr, Sor, Sens = SensSor) %>% distinct
trajetSO <- inner_join(t,t1) %>% mutate(OD = paste0(Entr,"-",Sor,"-",Sens))
write.table(trajetSO,"trajetSO.csv",sep=";",row.name=FALSE,quote=FALSE)
rm(t,t1)


##########
### gares update Ref_gares.csv
##########
t0 <- read.table("Ref_gares_v0.csv", sep = ";", header = TRUE) %>% tbl_df
t1 <- read.table("Ref_gares_v1.csv", sep = ";", header = TRUE,quote = "") %>% tbl_df %>%
  mutate(Lat = as.numeric(Lat),
         Lng = as.numeric(Lng)
         )

ggplot() +
  geom_point(data = t0, aes(Lng, Lat, col = "New", size = 2, alpha = .5)) +
  geom_point(data = t1, aes(Lng, Lat, col = "Old", alpha = .5)) 