library(dplyr)
library(ggplot2)

t <- read.table("gares_geolocalisees.csv", sep = ";", header = TRUE) %>% tbl_df %>%
names(t) <- c("Nom", "Type", "Region", "Lat", "Lng")

gare.ferro <- t
ggplot(gare.ferro %>% filter(Lat > 0)) + geom_point(aes(Lng,Lat, col = as.factor(Region)))

t <- read.table("shell_points_france.csv", sep = ";", header = TRUE, fill = TRUE, quote = "") %>% tbl_df
names(t) <- c("Enseigne", "Nom", "PDV", "Adresse", "Postal", 
              "Ville", "Departement", "Region", "Pays", 
              "Lng", "Lat", 
              "GPL", "AdBluePompe", "PL", "CarteSingle")

gare.essence <- t

ggplot(gare.essence) + geom_point(aes(Lng,Lat, col = Region))
ggplot(gare.essence) + geom_point(aes(Lng,Lat, col = Region)) + facet_wrap(~Enseigne)
