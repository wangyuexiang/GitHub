####################
# Algo_VizVIP
####################
# Visualise the result for VIPs
#
# Input: 
#   Input:
#     result.TS
#     result.ZW
#     trxZoneActive
#   Reference:
#     Ref_VIPn
#     Ref_gares
#     Ref_GridLimit
#     Ref_ODtoGrid
# Output:
#   ggplot viz
####################

# import library
library(ggplot2)

# import reference
GridLimit <- read.table("Reference/Ref_GridLimit.csv", header = T, sep = ";") %>% tbl_df
gares <- read.table("Reference/Ref_gares.csv", sep = ";", header = TRUE, quote = "") %>% tbl_df
ref <- read.table("Reference/Ref_VIPn.csv", header = T, sep = ";") %>%
  tbl_df %>% mutate(ID = as.character(ID))

# TODO: remove irrelavant ID == 980022310004
##########
### viz Result.TS
##########
t <- left_join(result.TS, ref)

t1 <- result.TS %>% transmute(OD =as.character(OD))
t2 <- read.table(text = t1$OD,sep="-") %>% tbl_df %>%
  transmute(Entr = V1, Sor = V2)
t1 <- cbind(result.TS,t2) %>% tbl_df

t <- left_join(t1, ref)

t1 <- gares %>% transmute(Entr = Cde, Elng = Lng, Elat = Lat)
t2 <- left_join(t,t1)
t1 <- gares %>% transmute(Sor = Cde, Slng = Lng, Slat = Lat)
t2 <- left_join(t2,t1)

viz.TS <- t2 %>% mutate(
  Elng = ifelse(Entr == 0, Slng, Elng),
  Elat = ifelse(Entr == 0, Slat, Elat)
  )

rm(t,t1,t2)

# viz
ggplot(viz.TS) +
  geom_segment(aes(x=Elng,xend=Slng,
                   y=Elat,yend=Slat, alpha = noPsg )) +
  facet_wrap(~Nom)

##########
### viz Result.ZW
##########
# adjust step
t <- GridLimit %>% slice(1)
gridStep <- t$u - t$d

# Zone
t <- left_join(trxZoneActive, ref)
viz.ZW <- t %>% inner_join(GridLimit)

# Display
ggplot(viz.ZW) + 
  geom_tile(aes(l + gridStep/2,d + gridStep/2, alpha = Per)) +
  facet_wrap(~Nom)

# Window
t <- left_join(trxZoneActiveH, ref)
t1 <- t %>% group_by(Nom,DOW,H) %>% summarise(freq = n())

ggplot(t1) + geom_tile(aes(DOW,H, fill = freq)) + facet_wrap(~Nom)

##########
### viz both
##########
# for all VIP 
ggplot() +
  geom_segment(data = viz.TS, 
               aes(x=Elng,xend=Slng,
                   y=Elat,yend=Slat, alpha = noPsg )) +
  geom_tile(data = viz.ZW, 
            aes(l + gridStep/2,d + gridStep/2, alpha = Per)) +
  facet_wrap(~Nom)

# add gares
ggplot() +
  geom_tile(data = viz.ZW, 
            aes(l + gridStep/2, d + gridStep/2, alpha = Per)) +  
  geom_point(data= gares %>% filter(Societe != 5), 
             aes(Lng, Lat, col = as.factor(Societe)),
             alpha = .5) +
  geom_segment(data = viz.TS, 
               aes(x=Elng,xend=Slng,
                   y=Elat,yend=Slat),
               colour = "red",
               size = 2
  ) +
  geom_point(data = viz.TS %>% filter(Entr == 0), 
             aes(Slng,Slat),
             colour = "red",
             size = 4
  ) +
  facet_wrap(~Nom)  +
  xlim(1,8) + ylim(42,45)


# individual
k = 50
# for every one
for(k in 1:nrow(ref)){
  t <- ggplot() +
    geom_tile(data = viz.ZW %>% filter(N == k), 
              aes(r,u, alpha = Per)
    ) +  
    geom_point(data= gares %>% filter(Societe != 5), 
               aes(Lng, Lat, 
                   col = as.factor(Societe))) +
    geom_segment(data = viz.TS %>% filter(N == k), 
                 aes(x=Elng,xend=Slng,
                     y=Elat,yend=Slat
                 ),
                 colour = "red",
                 size = 2
    ) +
    geom_point(data = viz.TS %>% filter(Entr == 0, N == k), 
               aes(Slng,
                   Slat
               ),
               colour = "red",
               size = 4
    ) +
    facet_wrap(~Nom) 
  
  ggsave(filename = paste(ref$Nom[ref$N == k], '.jpg', sep=""),
         plot = t)
}



t <- ggplot() +
  geom_tile(data = viz.ZW %>% filter(N == k), 
            aes(r,u, alpha = Per)
  ) 
t1 <- t+  geom_point(data= gares %>% filter(Societe != 5), 
             aes(Lng, Lat, 
                 col = as.factor(Societe))) +
  geom_segment(data = viz.TS %>% filter(N == k), 
               aes(x=Elng,xend=Slng,
                   y=Elat,yend=Slat
               ),
               colour = "red",
               size = 2
  ) +
  geom_point(data = viz.TS %>% filter(Entr == 0, N == k), 
             aes(Slng,
                 Slat
             ),
             colour = "red",
             size = 4
  ) +
  facet_wrap(~Nom) 


