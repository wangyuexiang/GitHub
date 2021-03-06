##########
### Sens
##########
library(dplyr)
library(ggplot2)

gares <- read.table("Ref_gares.csv", header = T, sep = ";", quote = "") %>% tbl_df 

##########
### prepare tSF, tSO
##########
# SO
tSO <- read.table("Rsens_SO.csv", header = T, sep = ";", quote = "", strip.white=TRUE) %>% tbl_df 
tSO <- tSO %>%
  mutate(Cde = 25006000+Gare) %>%
  left_join(gares %>% select(Cde, Autoroute)) %>%
  mutate(Autoroute = as.character(Autoroute), 
         DirE = tolower(as.character(DirE)),
         DirS = tolower(as.character(DirS)))

write.table(tSO, "Rsens_SO_after.csv",sep=";",row.name=FALSE,quote=FALSE)

# SF
tSF <- read.table("Rsens_SF.csv", header = T, sep = ";", quote = "", strip.white=TRUE) %>% tbl_df  
t <- tSF %>%
  mutate(Entr = 25006000+ entr,
         Sor = 25006000 + sor)
t1 <- gares %>% select(Cde, Autoroute) %>% rename(Entr = Cde, AutoE = Autoroute)
t <- left_join(t,t1)
t1 <- gares %>% select(Cde, Autoroute) %>% rename(Sor = Cde, AutoS = Autoroute)
t <- left_join(t,t1)

tSF <- t %>%
  mutate(AutoE = as.character(AutoE), 
         AutoS = as.character(AutoS),
         From = tolower(as.character(From)),
         To = tolower(as.character(To)))
write.table(tSF, "Rsens_SF_after.csv",sep=";",row.name=FALSE,quote=FALSE)

tSF <- read.table("Rsens_SF_after_v1.csv", header = T, sep = ";", quote = "", strip.white=TRUE) %>% 
  tbl_df %>%
  mutate(AutoE = as.character(AutoE), 
         AutoS = as.character(AutoS),
         From = tolower(as.character(From)),
         To = tolower(as.character(To)))
##########
### Voie
##########
# get Cde, Voie, Sens
tVoie <- read.table("Rsens_Voie.csv", header = T, sep = ";", quote = "", strip.white=TRUE) %>%
  tbl_df %>%
  mutate(Cde = 25006000+Gare)
# verify voie-sens
count(tVoie, Voie < 20, Sens)
# which shows that:
#   voie < 20: sens == 1 - Entrée
#   voie >= 20: sens == 2 - Sortie

##########
### Direction
##########
tDirection <- read.table("Rsens_Direction.csv", header = T, sep = ";", quote = "", strip.white=TRUE) %>% 
  tbl_df %>%
  mutate(Autoroute = as.character(Autoroute),
            From = tolower(as.character(From)),
            To = tolower(as.character(To)))

##########
### get Sens.SF
##########
t <- tSF
t1 <- tDirection %>% transmute(AutoE = Autoroute, From, SensEntr = Sens)
t2 <- left_join(t,t1)
t1 <- tDirection %>% transmute(AutoS = Autoroute, To, SensSor = Sens)
t2 <- left_join(t2,t1)
# Sens.SF <- t2 %>% select(Entr,Sor,SensEntr,SensSor)
Sens.SF <- t2 %>% select(Entr, AutoE, LibE, SensEntr,
                         Sor,  AutoS, LibS, SensSor)

##########
### get Sens.SO
##########
t <- tSO %>% select(Cde, Autoroute, DirE,DirS) %>% inner_join(tVoie)
t1 <- t %>% filter(Sens == 1) %>% select(Cde, Lib, Voie,Autoroute, To = DirE)
t2 <- tDirection %>% select(Autoroute , To, Sens)
Sens.SO <- left_join(t1,t2)

t1 <- t %>% filter(Sens == 2) %>% select(Cde, Lib, Voie,Autoroute, To = DirS)
t3 <- left_join(t1,t2)
Sens.SO <- rbind(Sens.SO, t3)
rm(t,t1,t2,t3)

# display problems with SF
Sens.SF %>% filter(is.na(SensEntr) | is.na(SensSor)) %>% count(AutoE, AutoS, LibE, LibS, SensEntr, SensSor) 
Sens.SF %>% filter(is.na(SensEntr) | is.na(SensSor)) %>% count(AutoE, AutoS, SensEntr, SensSor)
# display problems with SO
Sens.SO %>% count(Cde, Lib, Autoroute, To, Sens) %>% filter(is.na(Sens))

write.table(Sens.SF, "Reference_ESCOTA_SF.csv",sep=";",row.name=FALSE,quote=FALSE)
write.table(Sens.SO, "Reference_ESCOTA_SO.csv",sep=";",row.name=FALSE,quote=FALSE)

t1 <- Sens.SF %>% select(Entr, Sor, SensEntr, SensSor) %>% mutate(Voie = 0)
t2 <- Sens.SO %>% transmute(Entr = 0, Sor = Cde, Voie, SensEntr = 0, SensSor = Sens)
Sens.Escota <- rbind(t1,t2)
write.table(Sens.Escota, "Reference_ESCOTA.csv",sep=";",row.name=FALSE,quote=FALSE)

##########
### compare with sens given by Nicolas
##########
t <- Sens.Escota
t1 <- t %>% filter(Entr == 0) %>% transmute(Entr, Sor, Voie, M1 = "new")

t2 <- sens %>% filter(Sor > 25006000 & Sor < 25007000) %>% filter(Entr == 0) %>% transmute(Entr, Sor, Voie, M2 = "old")

t3 <- left_join(t1,t2)
t3 %>% count(M1,M2)
t3 %>% filter(is.na(M2))

# result
# 25006049 is SO
# miss SO voie

##########
### regenerate sens
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

ASF.sens <- temp %>%
  transmute(Entr, Sor, SensEntr, SensSor, Voie = 0)
rm(temp)

sens <- rbind(ASF.sens, Sens.Escota)
write.table(sens, "Ref_sens.csv",sep=";",row.name=FALSE,quote=FALSE)
