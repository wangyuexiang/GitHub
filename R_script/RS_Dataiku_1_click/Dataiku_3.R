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
# gares <- read.table("garesLatLng.csv", header = T, sep = ",")
# load("MODELE.RData")
# garesorder <- rbind(A7_par_pk[,c("Lib","Cde","PK","Lat","Lng")],A8_par_pk[-1,c("Lib","Cde","PK","Lat","Lng")],A9_par_pk[,c("Lib","Cde","PK","Lat","Lng")])


temp <- trx %>% group_by(Label) %>% 
  summarise(Dmin = min(Date), 
            Dmax = max(Date), 
            Ddiff = Dmax - Dmin + 1, 
            Day = n_distinct(Date), 
            Wmin = min(WOY),
            Wmax = max(WOY),
            Wdiff = Wmax - Wmin + 1,
            Week = n_distinct(WOY),
            nPsg = n(),
            Dper = Day/Ddiff,
            Wper = Week/Wdiff)


