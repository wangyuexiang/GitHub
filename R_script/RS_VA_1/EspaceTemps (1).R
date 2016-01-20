# call packages
library(dplyr)
library(cluster)
library(ggplot2)
library(gridExtra)
library(knitr)

# OD -> Espace -> Temps 
#######
# Créer VIP2
####### 
VIP2 <- VIP2[VIP2$ID %in% c("CC","FF", "NP", "PC") & VIP2$Date > as.Date("2014-12-31") & VIP2$Date < as.Date("2015-5-29"), ]
x <- vector(mode = "integer", length = nrow(VIP2))


VIP2 <- cbind(VIP2[,1:3],x,x,x,x,x,x,VIP2$TimeEntr,VIP2$TimeSor,VIP2$DOW,VIP2$WOY,VIP2$Date)
colnames(VIP2)<- c(colnames(MODELE),"Date")


######
# predefined parameters
train.start <- as.Date("2015-1-1")
test.start <- as.Date("2015-5-1")
test.end <- as.Date("2015-5-28")
test.period <- data.frame(Date = seq(test.start, test.end, "day"))
test.period$DOW <- as.POSIXlt(test.period$Date)$wday


VIP2_pour_modele <- VIP2[VIP2$Date < as.Date("2015-5-1"),]
VIP2_pour_test <- VIP2[VIP2$Date >= as.Date("2015-5-1"),]

VIP2_decompose <- decompose(VIP2_pour_modele)
VIP2_pour_test_par_troncons <- decompose(VIP2_pour_test)

# get the ID list
ID.list <- as.data.frame(VIP2_decompose %>% group_by(ID) %>% summarise())

#######
#COMPTAGE DES TRAJETS PAR TRONCON (PAR SENS ? PAS ENCORE)
#######

Compteur <- VIP2_decompose %>% 
  group_by(ID, ID_Troncon) %>%
  summarise( n = n())


########
#DECISION AUTOMATIQUE DES TRONCONS FREQUENTS
########


# [Pourcentagedumax max ; max ]
Pourcentagedumax <- 0.56
Troncon_Selection <- Compteur %>% 
  group_by(ID) %>%
  filter (n > (Pourcentagedumax * max(n)))

VIP2_espace <- inner_join(VIP2_decompose, Troncon_Selection, by = c("ID","ID_Troncon"))


########
# Kmeans 
########

VIP2_espace$TimeSor <- as.numeric(VIP2_espace$TimeSor)
VIP2_espace <- tbl_df(VIP2_espace)


########
###  Model 22 : Space - Time & by DOW
VIP2_espace_temps <- data.frame(ID="", Entr=0, Sor=0, DOW=0, SD=0, T=0, Tmin=0, Tmax=0, n=0)
for (k in 1:75){
  for (i in 1:nrow(ID.list)){
    for (j in 0:6){
      temp <- VIP2_espace %>% filter(ID == ID.list$ID[i] & DOW == j &  ID_Troncon == k )
      # base to be verified
      if(nrow(temp) >= 10) {
        # if not many passages, we will not cluster
        
        max.cluster <- length(unique(temp$TimeSor))
        # decide nb of cluster
        clus<- clusGap(temp[,"TimeSor"], kmeans, min(5, max.cluster))
        n.cluster <- with(clus,maxSE(Tab[,"gap"],Tab[,"SE.sim"]))
        
        set.seed(1234)
        temp.kmeans <-   kmeans(temp[, "TimeSor"], centers = n.cluster)
        temp$cluster <- temp.kmeans$cluster
        T <- temp %>%
          group_by(ID, Entr, Sor, DOW, cluster) %>%
          summarise(SD = sd(TimeSor), T = mean(TimeSor),Tmin = T -SD, Tmax = T + SD, n = n())
        T <- T %>% filter(n>1)
        T$cluster <- NULL
        VIP2_espace_temps <- rbind(VIP2_espace_temps, T) 
      }
    }
  }
}

VIP2_espace_temps <- VIP2_espace_temps[-1,]


result.model.22<- VIP2_espace_temps[,-c(5:6,9)]
result.model.22$Model <- 22
result.model.22$DOW <- as.integer(result.model.22$DOW)
result.model.22$ID <- as.character(result.model.22$ID)

#######
#Evaluer le modele
#######

#######

##########
### evalutaion model.22
test.model.22 <- GetResult(VIP2_pour_test_par_troncons, result.model.22)
ind.model.22 <- GetInd(test.model.22, result.model.22)
ind.model.22$Model <- 22



########
###  Model 20 : Space - Time
########
result.model.20 <- data.frame(ID="", Entr=0, Sor=0, DOW=0, SD=0, T=0, Tmin=0, Tmax=0, n=0)
for (k in 1:75){
  for (i in 1:nrow(ID.list)){
    temp <- VIP2_espace %>% filter(ID == ID.list$ID[i] &  ID_Troncon == k )
    # base to be verified
    if(nrow(temp) >= 10) {
      # if not many passages, we will not cluster
      max.cluster <- length(unique(temp$TimeSor))
      # decide nb of cluster
      clus<- clusGap(temp[,"TimeSor"], kmeans, min(5, max.cluster))
      n.cluster <- with(clus,maxSE(Tab[,"gap"],Tab[,"SE.sim"]))
      
      set.seed(1234)
      temp.kmeans <-   kmeans(temp[, "TimeSor"], centers = n.cluster)
      temp$cluster <- temp.kmeans$cluster
      T <- temp %>%
        group_by(ID, Entr, Sor, DOW, cluster) %>%
        summarise(SD = sd(TimeSor), T = mean(TimeSor),Tmin = T -SD, Tmax = T + SD, n = n())
      T <- T %>% filter(n>1)
      T$cluster <- NULL
      result.model.20 <- rbind(result.model.20, T) 
    }
  }
}

result.model.20 <- result.model.20[-1,]


result.model.20<- result.model.20[,-c(5:6,9)]
result.model.20$Model <- 20
result.model.20$DOW <- as.integer(result.model.20$DOW)
result.model.20$ID <- as.character(result.model.20$ID)

#######
#Evaluer le modele
#######

#######

##########
### evalutaion model.20
test.model.20 <- GetResult(VIP2_pour_test_par_troncons, result.model.20)
ind.model.20 <- GetInd(test.model.20, result.model.20)
ind.model.20$Model <- 20



########
### Model 21 : Space - Time & Weekdays vs weekends
VIP2_espace$weekday <- 0
VIP2_espace[VIP2_espace$DOW %in% c(1:5), ]$weekday <- 1

result.model.21 <- data.frame(ID="", Entr=0, Sor=0, DOW=0, SD=0, T=0, Tmin=0, Tmax=0, n=0)
for (k in 1:75){
  for (i in 1:nrow(ID.list)){
    for (j in 0:1){
      temp <- VIP2_espace %>% filter(ID == ID.list$ID[i] &  ID_Troncon == k & weekday == j)
      # base to be verified
      if(nrow(temp) >= 10) {
        # if not many passages, we will not cluster
        max.cluster <- length(unique(temp$TimeSor))
        # decide nb of cluster
        clus<- clusGap(temp[,"TimeSor"], kmeans, min(5, max.cluster))
        n.cluster <- with(clus,maxSE(Tab[,"gap"],Tab[,"SE.sim"]))
        
        set.seed(1234)
        temp.kmeans <-   kmeans(temp[, "TimeSor"], centers = n.cluster)
        temp$cluster <- temp.kmeans$cluster
        T <- temp %>%
          group_by(ID, Entr, Sor, DOW, cluster) %>%
          summarise(SD = sd(TimeSor), T = mean(TimeSor),Tmin = T -SD, Tmax = T + SD, n = n())
        T <- T %>% filter(n>1)
        T$cluster <- NULL
        result.model.21 <- rbind(result.model.21, T) 
      }
    }
  }
}

result.model.21 <- result.model.21[-1,]


result.model.21<- result.model.21[,-c(5:6,9)]
result.model.21$Model <- 21
result.model.21$DOW <- as.integer(result.model.21$DOW)
result.model.21$ID <- as.character(result.model.21$ID)

#######
#Evaluer le modele
#######

#######

##########
### evalutaion model.21
test.model.21 <- GetResult(VIP2_pour_test_par_troncons, result.model.21)
ind.model.21 <- GetInd(test.model.21, result.model.21)
ind.model.21$Model <- 21