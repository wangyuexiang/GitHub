### Visulisation of result
library(dplyr)
library(ggplot2)

##########
### Get Ref
##########
GridLimit <- read.table("Ref_GridLimit.csv", header = T, sep = ";") %>% tbl_df 
gares <- read.table("Ref_gares.csv", header = T, sep = ",") %>% tbl_df 
 
##########
### Zone
##########
# Connect AcitveZone with GridLimit
t <- trxZoneActive %>% inner_join(GridLimit)
# Get the first 9 users
k <- (t %>% select(ID) %>% distinct %>% slice(1:16))$ID
# Display
ggplot(t %>% filter(ID %in% k)) + 
  geom_tile(aes(l,d, alpha = Per)) + 
  xlim(c(-2,8)) + ylim(c(42,49)) + 
  facet_wrap(~ID) +
  geom_point(data= gares, aes(Lng, Lat, col = as.factor(Societe)))

##########
### Window
##########
t1 <- trxZoneActiveH %>% group_by(ID,DOW,H) %>% summarise(freq = n())
t2 <- trxZoneActiveH %>% group_by(ID,DOW,H_2) %>% summarise(freq = n())
ggplot(t1 %>% filter(ID %in% k)) + geom_tile(aes(DOW,H, fill = freq)) + facet_wrap(~ID)
ggplot(t2 %>% filter(ID %in% k)) + geom_tile(aes(DOW,H_2, fill = freq)) + facet_wrap(~ID)

rm(t,t1,t2,k,gares)
