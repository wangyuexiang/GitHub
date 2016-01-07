##########
### Sens
##########
gares <- read.table("Ref_gares.csv", header = T, sep = ";", quote = "") %>% tbl_df 

# SO
tSO <- read.table("Rsens_SO.csv", header = T, sep = ";", quote = "") %>% tbl_df 
t <- tSO %>%
  mutate(Cde = 25006000+gare) %>%
  left_join(gares %>% select(Cde, Autoroute))
write.table(t, "Rsens_SO_after.csv",sep=";",row.name=FALSE,quote=FALSE)

# SF
tSF <- read.table("Rsens_SF.csv", header = T, sep = ";", quote = "") %>% tbl_df 
t <- tSF %>%
  mutate(Entr = 25006000+ entr,
         Sor = 25006000 + sor)
t1 <- gares %>% select(Cde, Autoroute) %>% rename(Entr = Cde, AutoE = Autoroute)
t <- left_join(t,t1)
t1 <- gares %>% select(Cde, Autoroute) %>% rename(Sor = Cde, AutoS = Autoroute)
t <- left_join(t,t1)

t1 <-  read.table(text = as.character(t$direction), sep="-")$V1 %>% as.character
t$DirE <- t1
t1 <-  read.table(text = as.character(t$direction), sep="-")$V2 %>% as.character
t$DirS <- t1

write.table(t, "Rsens_SO_after.csv",sep=";",row.name=FALSE,quote=FALSE)
