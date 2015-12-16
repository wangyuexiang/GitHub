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


##########
### geom_tile: GridLimit
##########
ggplot(GridLimit) + geom_tile(aes(l,d,fill = d))
t <- GridLimit %>% mutate(Test = (Col == 20))
ggplot(t) + geom_tile(aes(l,d,fill = Test))
ggplot(t) + geom_tile(aes(l,u,fill = Test))
ggplot(t) + geom_raster(aes(l + .25,u - .25,fill = Test))

ggplot(t %>% filter(Col == 1)) + geom_tile(aes(l,u,fill = Zone))

ggplot(t) + 
  geom_tile(aes(l,d, fill = "ld", alpha = .5)) +
  geom_tile(aes(l,u, fill = "lu", alpha = .5))

