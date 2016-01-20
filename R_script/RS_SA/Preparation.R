load("VIP.RData")
load("JF.RData")
load("Sens_ref.RData")

gares <- read.table("garesLatLng.csv", header = T, sep = ",")

trx.ready <- trx