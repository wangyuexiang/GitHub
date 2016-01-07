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
tSO <- read.table("Rsens_SO.csv", header = T, sep = ";", quote = "") %>% tbl_df 
tSO <- tSO %>%
  mutate(Cde = 25006000+Gare) %>%
  left_join(gares %>% select(Cde, Autoroute))
write.table(tSO, "Rsens_SO_after.csv",sep=";",row.name=FALSE,quote=FALSE)

# SF
tSF <- read.table("Rsens_SF.csv", header = T, sep = ";", quote = "") %>% tbl_df  
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

##########
### Voie
##########
# get Cde, Voie, Sens
tVoie <- read.table("Rsens_Voie.csv", header = T, sep = ";", quote = "") %>%
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
tDirection <- read.table("Rsens_Direction.csv", header = T, sep = ";", quote = "") %>% 
  tbl_df %>%
  mutate(AutoE = as.character(Autoroute),
            From = tolower(as.character(From)),
            To = tolower(as.character(To)))

##########
### get Sens.SF
##########
t <- tSF
t1 <- tDirection %>% transmute(AutoE, From, SensEntr = Sens)
t2 <- left_join(t,t1)
t1 <- tDirection %>% transmute(AutoE, From, SensSor = Sens)
t2 <- left_join(t2,t1)
Sens.SF <- t2

##########
### get Sens.SO
##########