# load("trx.2014.RData")
# trx <- trx.2014
# rm(trx.2014)

# load("Troncons_A789.RData")
# load("A7_par_pk.RData")
# load("A8_par_pk.RData")
# load("A9_par_pk.RData")
# load("Troncons_A7.RData")
# load("Troncons_A8.RData")
# load("Troncons_A9.RData")
# gares <- read.table("garesLatLng.csv", header = TRUE, sep = ",")
# load("MODELE.RData")
# garesorder <- rbind(A7_par_pk[,c("Lib","Cde","PK","Lat","Lng")],A8_par_pk[-1,c("Lib","Cde","PK","Lat","Lng")],A9_par_pk[,c("Lib","Cde","PK","Lat","Lng")])

trx <- trx.2014

Label.list <- trx %>% group_by(ID, Label) %>% summarise( n = n()) %>% ungroup() %>% arrange(desc(n))
  
trx <- trx %>%
  mutate(
    Entr = as.numeric(as.character(Entr)),
    Sor = as.numeric(as.character(Sor)),
    ID = as.character(ID),
    Label = as.character(Label)
  )

trx.ref <- trx %>% group_by(ID, Label) %>% summarise( n = n()) %>% ungroup() %>% arrange(desc(n))
trx$ID <- trx$Label
trx$Label <- NULL
transaction <- trx