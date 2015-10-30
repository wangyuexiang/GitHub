##########
##########
# SA_otherActions.R
# first: 20150723
# other actions

##########
##########
### Major steps
# Result Visualisation
# Optimization of Models
#   PM
#   CC
# Test
#   First data view
# Reserve code
#   Initialisation

t <- temp1 %>% filter(ord <3)
t <- t %>% filter(DOW == 2)


t1 <- t %>%
  group_by(Ste, EVA, Nom, N, ID, Entr, Sor) %>%
  summarise(Tmoy = mean(TimeSor),
            SD = sd(TimeSor),
            Tmin = Tmoy - SD,
            Tmax = Tmoy + SD,
            noPsg = n())

t2 <- sens %>% select(-Voie)
t1 <- inner_join(t1, t2)

t3 <- t1 %>% 
  ungroup() %>%
  mutate(OD = paste0(Entr,"-",Sor,"-",SensEntr,"-",SensSor,"-",0)) %>%
  select(Ste, EVA, Nom, N, ID, OD, Tmin, Tmax, noPsg)

t3$DOW <- 2


##########
##########
### 20151022
### Phillipe Bori:     44 - tous les deux semaines mardi
### Phillipe Bertreau: 42
### Patricia  45
### Denis     46
### Stephane  47: trop peu de trajets, à demander 
### James     48
### Julien    49
temp <- BDD.temp %>% filter(N == 44)
temp.OD <- temp %>%
  group_by(Entr, Sor) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  mutate(ord = row_number())

title <- temp$Nom[1]

t <- temp.OD %>% select(-n)
temp1 <- inner_join(temp,t)

# ggplot(temp1) + geom_point(aes(Date, TimeSor))
g1 <- ggplot(temp1) + geom_point(aes(Date, TimeSor, col = as.factor(ord))) + theme(legend.position = "none")
g2 <- ggplot(temp1 %>% filter(ord < 3)) + geom_point(aes(Date, TimeSor, col = as.factor(ord)))

temp2 <- temp1 %>% filter(ord <3)
g3 <- ggplot(temp2) + geom_bar(aes(DOW, fill = as.factor(ord)), binwidth = 1, position = "dodge") + theme(legend.position = "none")
g4 <- ggplot(temp2) + geom_bar(aes(TimeSor, fill = as.factor(ord)), binwidth = 1, position = "dodge") + theme(legend.position = "none")

g5 <- ggplot(temp2) + geom_tile(aes(WOY,DOW, fill = as.factor(ord))) + facet_wrap(~ord, ncol = 1) + theme(legend.position = "none")
grid.arrange(g1,g2,g3,g4, g5, ncol = 2, main = title) 
temp.OD

##########





##########
##########
### Model: verstion before OD identifier

Model <- function(transaction, model.decades, model.units) {
  # Run the model over the transaction set
  # Args:
  #	  transaction:	ID, Entr, Sor, Date, DOW, WOY, TimeEntr, TimeSor, Voie
  #   model: 0,1,2,10,11,12,20,21,22
  # Returns:
  #	  transaction:	ID, Entr, Sor, DOW, Tmin, Tmax, Model
  #   transaction:  ID, OD = paste0(Entr,Sor,Sens), ...
  
  model <- model.decades * 10 + model.units
  
  if (model == 0){
    T.matin <- transaction %>%
      filter(TimeSor < 12 ) %>%
      group_by(ID, Entr, Sor, SensEntr, SensSor, Sens) %>%
      summarise(noPsg = n(), T = mean(TimeSor), SD = sd(TimeSor), Tmin = T -SD, Tmax = T + SD)
    
    T.aprem <- transaction %>%
      filter(TimeSor >= 12 ) %>%
      group_by(ID, Entr, Sor, SensEntr, SensSor, Sens) %>%
      summarise(noPsg = n(), T = mean(TimeSor), SD = sd(TimeSor), Tmin = T -SD, Tmax = T + SD)
    
    T <- rbind(T.matin, T.aprem)
    T <- T %>% filter(noPsg > param.min.noPsg)
    
    # add DOW
    T$DOW <- 0
    temp <- T
    for(i in 1:6) {
      # add DOW to the T
      temp$DOW <- i
      T <- rbind(T, temp)
    }
    T$Model <- model
    result <- T
    ##########
    # end of model 00
  } else 
    if (model == 1) {
      temp <- transaction 
      temp$weekday <- 0
      temp[temp$DOW %in% c(1:5), ]$weekday <- 1
      
      T.matin <- temp %>%
        filter(TimeSor < 12 ) %>%
        group_by(ID, Entr, Sor, SensEntr, SensSor, Sens, weekday) %>%
        summarise(noPsg = n(), T = mean(TimeSor), SD = sd(TimeSor), Tmin = T -SD, Tmax = T + SD)
      
      T.aprem <- temp %>%
        filter(TimeSor >= 12 ) %>%
        group_by(ID, Entr, Sor, SensEntr, SensSor, Sens, weekday) %>%
        summarise(noPsg = n(), T = mean(TimeSor), SD = sd(TimeSor), Tmin = T -SD, Tmax = T + SD)
      
      T <- rbind(T.matin, T.aprem)
      T <- T %>% filter(noPsg > param.min.noPsg)
      
      # add DOW
      # for weekdays
      result <- T %>% filter(weekday == 1)
      result$DOW <- 1
      temp <- result
      for(i in 2:5) {
        # add DOW to the result
        temp$DOW <- i
        result <- rbind(result, temp)
      }
      
      # for weekends
      T <- T %>% filter(weekday == 0)
      if (nrow(T) > 0){
        T$DOW <- 0
        temp <- T
        temp$DOW <- 6
        T <- rbind(T, temp)
      }
      
      result <- rbind(result, T)
      result$weekday <- NULL
      result$Model <- model
      ##########
      # end of model 01
    } else 
      if (model == 2) {
        T.matin <- transaction %>%
          filter(TimeSor < 12 ) %>%
          group_by(ID, Entr, Sor, SensEntr, SensSor, Sens, DOW) %>%
          summarise(noPsg = n(), T = mean(TimeSor), SD = sd(TimeSor), Tmin = T -SD, Tmax = T + SD)
        
        T.aprem <- transaction %>%
          filter(TimeSor >= 12 ) %>%
          group_by(ID, Entr, Sor, SensEntr, SensSor, Sens, DOW) %>%
          summarise(noPsg = n(), T = mean(TimeSor), SD = sd(TimeSor), Tmin = T -SD, Tmax = T + SD)
        
        T <- rbind(T.matin, T.aprem)
        T <- T %>% filter(noPsg > param.min.noPsg)
        
        T$Model <- model
        result <- T
        ##########
        # end of model 02
      } else 
        if(model == 10) {
          result <- data.frame(ID="", Entr=0, Sor=0, SensEntr = 0, SensSor = 0, Sens = 0, SD=0, T=0, Tmin=0, Tmax=0, noPsg=0)
          result$ID <- as.character(result$ID)
          for (i in 1:nrow(ID.list)) {
            temp <- transaction %>% filter(ID == ID.list$ID[i])
            max.cluster <- length(unique(temp$TimeSor))
            if(nrow(temp) >= param.min.nb.for.cluster & max.cluster > 1) {
              # if not many passages, we will not cluster
              # decide nb of cluster
              clus<- clusGap(temp %>% select(TimeSor), kmeans, max(min(param.max.nb.cluster, max.cluster),2))
              n.cluster <- with(clus,maxSE(Tab[,"gap"],Tab[,"SE.sim"]))
              
              set.seed(1234)
              temp.kmeans <-   kmeans(temp[, "TimeSor"], centers = n.cluster)
              temp$cluster <- temp.kmeans$cluster
              T <- temp %>%
                group_by(ID, Entr, Sor, SensEntr, SensSor, Sens, cluster) %>%
                summarise(noPsg = n(), T = mean(TimeSor), SD = sd(TimeSor), Tmin = T -SD, Tmax = T + SD)
              
              # remove line with SD = N.A.
              T <- T %>% filter(noPsg > 1)
              
              T$cluster <- NULL
              result <- rbind(result, T)
            } # end of if
          } # end of loop i
          result <- result[-1,]
          result <- result %>% filter(noPsg > param.min.noPsg)
          
          # add DOW
          result$DOW <- 0
          temp <- result
          for(i in 1:6) {
            # add DOW to the T
            temp$DOW <- i
            result <- rbind(result, temp)
          }
          
          result$Model <- model
          ##########
          # end of model 10
        } else 
          if(model == 11) {
            temp1 <- transaction 
            temp1$weekday <- 0
            temp1[temp1$DOW %in% c(1:5), ]$weekday <- 1
            
            # clustering TimeSor
            result <- data.frame(ID="", Entr=0, Sor=0, SensEntr = 0, SensSor = 0, Sens = 0, weekday = 0, SD=0, T=0, Tmin=0, Tmax=0, noPsg=0)
            result$ID <- as.character(result$ID)
            for (i in 1:nrow(ID.list)) {
              for (j in 0:1) {
                temp <- temp1 %>% filter(ID == ID.list$ID[i] & weekday == j)
                max.cluster <- length(unique(temp$TimeSor))
                if(nrow(temp) >= param.min.nb.for.cluster & max.cluster > 1) {
                  # if not many passages, we will not cluster
                  # decide nb of cluster
                  clus<- clusGap(temp %>% select(TimeSor), kmeans, max(min(param.max.nb.cluster, max.cluster),2))
                  n.cluster <- with(clus,maxSE(Tab[,"gap"],Tab[,"SE.sim"]))
                  
                  set.seed(1234)
                  temp.kmeans <-   kmeans(temp[, "TimeSor"], centers = n.cluster)
                  temp$cluster <- temp.kmeans$cluster
                  T <- temp %>%
                    group_by(ID, Entr, Sor, SensEntr, SensSor, Sens, weekday, cluster) %>%
                    summarise(noPsg = n(), T = mean(TimeSor), SD = sd(TimeSor), Tmin = T -SD, Tmax = T + SD)
                  
                  # remove line with SD = N.A.
                  T <- T %>% filter(noPsg > 1)
                  
                  T$cluster <- NULL
                  result <- rbind(result, T)
                } # end of if
              } # end of loop j
            } # end of loop i
            
            result <- result[-1,]
            result <- result %>% filter(noPsg > param.min.noPsg)
            
            T <- result  
            # add DOW
            # for weekdays
            result <- T %>% filter(weekday == 1)
            result$DOW <- 1
            temp <- result
            for(i in 2:5) {
              # add DOW to the result
              temp$DOW <- i
              result <- rbind(result, temp)
            }
            
            # for weekends
            T <- T %>% filter(weekday == 0)
            if (nrow(T) > 0){
              T$DOW <- 0
              temp <- T
              temp$DOW <- 6
              T <- rbind(T, temp)
            }
            
            result <- rbind(result, T)
            result$weekday <- NULL
            
            result$Model <- model
            
            ##########
            # end of model 11
          } else 
            if(model == 12) {
              result <- data.frame(ID="", Entr=0, Sor=0, SensEntr = 0, SensSor = 0, Sens = 0, DOW=0, SD=0, T=0, Tmin=0, Tmax=0, noPsg=0)
              result$ID <- as.character(result$ID)
              for (i in 1:nrow(ID.list)) {
                for (j in 0:6) {
                  temp <- train %>% filter(ID == ID.list$ID[i] & DOW == j)
                  max.cluster <- length(unique(temp$TimeSor))
                  if(nrow(temp) >= param.min.nb.for.cluster & max.cluster > 1) {
                    # if not many passages, we will not cluster
                    # decide nb of cluster
                    clus<- clusGap(temp %>% select(TimeSor), kmeans, max(min(param.max.nb.cluster, max.cluster),2))
                    n.cluster <- with(clus,maxSE(Tab[,"gap"],Tab[,"SE.sim"]))
                    
                    set.seed(1234)
                    temp.kmeans <-   kmeans(temp[, "TimeSor"], centers = n.cluster)
                    temp$cluster <- temp.kmeans$cluster
                    T <- temp %>%
                      group_by(ID, Entr, Sor, SensEntr, SensSor, Sens, DOW, cluster) %>%
                      summarise(noPsg = n(), T = mean(TimeSor), SD = sd(TimeSor), Tmin = T -SD, Tmax = T + SD)
                    
                    # remove line with SD = N.A.
                    T <- T %>% filter(noPsg > 1)
                    
                    T$cluster <- NULL
                    result <- rbind(result, T)
                  } # end of if
                } # end of loop j
              } # end of loop i
              
              result <- result[-1,]
              result <- result %>% filter(noPsg > param.min.noPsg)
              
              result$Model <- model
              
              ##########
              # end of model 12
            } else 
            {
              ####################
              ### model.2:	OD -> Space -> Time
              ####################
              #COMPTAGE DES TRAJETS PAR OD
              Compteur <- transaction %>% 
                group_by(ID, Entr,Sor) %>%
                summarise( n = n())
              #DECISION AUTOMATIQUE DES TRONCONS FREQUENTS
              Troncon_Selection <- Compteur %>% 
                group_by(ID) %>%
                filter (n > (param.model.2 * max(n)))
              VIP2_espace <- inner_join(transaction, Troncon_Selection, by = c("ID","Entr","Sor"))
              VIP2_espace <- tbl_df(VIP2_espace)
              #Create OD.list for the kmeans
              OD.list <- VIP2_espace %>% group_by( ID,Entr,Sor, Sens) %>% summarise()
              
              if(model == 20) {
                result <- data.frame(ID="", Entr=0, Sor=0, SensEntr = 0, SensSor = 0, Sens = 0, DOW=0, SD=0, T=0, Tmin=0, Tmax=0, noPsg=0)
                result$ID <- as.character(result$ID)
                for (k in 1:nrow(OD.list)){
                  temp <- VIP2_espace %>% filter(ID == OD.list$ID[k] &  Entr == OD.list$Entr[k] & Sor == OD.list$Sor[k] )
                  # base to be verified
                  max.cluster <- length(unique(temp$TimeSor))
                  if(nrow(temp) >= param.min.nb.for.cluster & max.cluster > 1) {				
                    # if not many passages, we will not cluster
                    # decide nb of cluster
                    clus<- clusGap(temp %>% select(TimeSor), kmeans, max(min(param.max.nb.cluster, max.cluster),2))
                    n.cluster <- with(clus,maxSE(Tab[,"gap"],Tab[,"SE.sim"]))
                    
                    set.seed(1234)
                    temp.kmeans <-   kmeans(temp[, "TimeSor"], centers = n.cluster)
                    temp$cluster <- temp.kmeans$cluster
                    T <- temp %>%
                      group_by(ID, Entr, Sor, SensEntr, SensSor, Sens, DOW, cluster) %>%
                      summarise(SD = sd(TimeSor), T = mean(TimeSor),Tmin = T -SD, Tmax = T + SD, noPsg = n())
                    T <- T %>% filter(noPsg>1)
                    T$cluster <- NULL
                    result <- rbind(result, T) 
                  } # end of if
                } # end of k loop
                result <- result[-1,]
                result$Model <- model	
                
                ##########
                # end of model 20
              } else 
                if(model == 21) {
                  VIP2_espace$weekday <- 0
                  VIP2_espace[VIP2_espace$DOW %in% c(1:5), ]$weekday <- 1
                  
                  result <- data.frame(ID="", Entr=0, Sor=0, SensEntr = 0, SensSor = 0, Sens = 0, DOW=0, SD=0, T=0, Tmin=0, Tmax=0, noPsg=0)
                  result$ID <- as.character(result$ID)
                  for (k in 1:nrow(OD.list)){
                    for (j in 0:1){
                      temp <- VIP2_espace %>% filter(ID == OD.list$ID[k] &  Entr == OD.list$Entr[k] & Sor == OD.list$Sor[k] & weekday == j)
                      max.cluster <- length(unique(temp$TimeSor))
                      if(nrow(temp) >= param.min.nb.for.cluster & max.cluster > 1) {				
                        # if not many passages, we will not cluster
                        #	decide nb of cluster
                        clus<- clusGap(temp %>% select(TimeSor), kmeans, max(min(param.max.nb.cluster, max.cluster),2))
                        n.cluster <- with(clus,maxSE(Tab[,"gap"],Tab[,"SE.sim"]))
                        
                        set.seed(1234)
                        temp.kmeans <-   kmeans(temp[, "TimeSor"], centers = n.cluster)
                        temp$cluster <- temp.kmeans$cluster
                        T <- temp %>%
                          group_by(ID, Entr, Sor, SensEntr, SensSor, Sens, DOW, cluster) %>%
                          summarise(SD = sd(TimeSor), T = mean(TimeSor),Tmin = T -SD, Tmax = T + SD, noPsg = n())
                        T <- T %>% filter(noPsg>1)
                        T$cluster <- NULL
                        result <- rbind(result, T) 
                      } # end of if
                    } # end of j loop
                  } # end of k loop
                  result <- result[-1,]
                  result$Model <- model
                  ##########
                  # end of model 21
                } else 
                  if(model == 22) {
                    result <- data.frame(ID="", Entr=0, Sor=0, SensEntr = 0, SensSor = 0, Sens = 0, DOW=0, SD=0, T=0, Tmin=0, Tmax=0, noPsg=0)
                    result$ID <- as.character(result$ID)
                    for (k in 1:nrow(OD.list)){
                      for (j in 0:6){
                        temp <- VIP2_espace %>% filter(ID == OD.list$ID[k] & DOW == j &  Entr == OD.list$Entr[k] & Sor == OD.list$Sor[k] )
                        max.cluster <- length(unique(temp$TimeSor))
                        if(nrow(temp) >= param.min.nb.for.cluster & max.cluster > 1) {				
                          # if not many passages, we will not cluster
                          # decide nb of cluster
                          clus<- clusGap(temp %>% select(TimeSor), kmeans, max(min(param.max.nb.cluster, max.cluster),2))
                          n.cluster <- with(clus,maxSE(Tab[,"gap"],Tab[,"SE.sim"]))
                          
                          set.seed(1234)
                          temp.kmeans <-   kmeans(temp[, "TimeSor"], centers = n.cluster)
                          temp$cluster <- temp.kmeans$cluster
                          T <- temp %>%
                            group_by(ID, Entr, Sor, SensEntr, SensSor, Sens, DOW, cluster) %>%
                            summarise(SD = sd(TimeSor), T = mean(TimeSor),Tmin = T -SD, Tmax = T + SD, noPsg = n())
                          T <- T %>% filter(noPsg>1)
                          T$cluster <- NULL
                          result <- rbind(result, T) 
                        } # end of if
                      } # end of j loop 
                    } # end of k loop 
                    
                    result <- result[-1,]
                    result$Model <- model
                    ##########
                    # end of model 22
                  }
            } # end of if
  
  result$DOW <- as.integer(result$DOW)
  result <- tbl_df(result)
  return(result %>% select(ID, Entr, Sor, SensEntr, SensSor, Sens, DOW, Tmin, Tmax, Model, noPsg) )
} # end of fuction Model


##########
##########
### SA_end2end : Time
# 20150930 06:56
# Time difference of 14.65324 secs
# Time difference of 0.05180001 secs
# Time difference of 50.31127 secs
# 20150916 22:11
# Time difference of 19.27926 secs
# Time difference of 0.066401 secs
# Time difference of 49.31837 secs
# 20150814 ???
# 1.161716 mins    (nbmaxclusters = 5 ; 7.879 sec) (NbMaxCluster 8s)
# 2.967297 secs    (nbmaxclusters = 5 ; 0.246 sec) (NbMaxCluster 0.3s)
# 2.779545 mins    (nbmaxclusters = 5 ; 45.65 sec) (NbMaxCluster 35s)

##########
##########
### Sens O/D
gares <- gares %>% tbl_df()

temp <- read.table("export_trjtsns_asf.csv", sep = ";", header = T) %>% tbl_df()
names(temp) <- c("E1","E2","E3","EA","SensEntr",
                 "S1","S2","S3","SA","SensSor")
gares.sens <- temp %>% 
  transmute(Entr = E1 * 100000 + E2 * 1000 + as.numeric(as.character(E3)), 
            SensEntr,
            Sor = S1 * 100000 + S2 * 1000 + as.numeric(as.character(S3)), 
            SensSor) %>%
  filter(!is.na(Entr) & !is.na(Sor))


##########
##########
###   Result Visualisation
##########
# viz Ind
ggplot(Ind) + 
  geom_point(aes(Model, Ind1, col = "% de trajets réels prédits")) + 
  # geom_point(aes(Model, Ind2, col = "Ind2")) + 
  geom_point(aes(Model, Ind3, col = "% de fausse alerts")) +
  facet_wrap(~ID) +
  labs(y = " Indicator") +
  theme(legend.title = element_blank())

##########
# viz: result
ggplot(result.final) + 
  geom_point(aes(DOW, Tmin, col = "Tmin")) +
  geom_point(aes(DOW, Tmax, col = "Tmax")) +
  geom_segment(aes(x=DOW, xend=DOW, y=Tmin, yend=Tmax)) +
  facet_wrap(~ID) + ggtitle("Result: Time Interval by DOW")

##########
# viz: test result
ggplot(test.final) + geom_point(aes(Date, TimeSor, col = as.factor(result))) + facet_wrap(~ID) + ggtitle("Test Result")
plyr::count(test.final, c("ID","result"))
test.final %>% group_by(ID) %>% summarise(noPsg = n(), noTP = sum(result), Per_TP = noTP/noPsg)

##########
# viz: train result
ggplot(train.final) + geom_point(aes(Date, TimeSor, col = as.factor(result))) + facet_wrap(~ID) + ggtitle("Train Result")

# result in text
print(result.final %>% filter(ID == "CC") %>% group_by(ID,Entr,Sor) %>% summarise(), n = 35)
print(result.final %>% filter(ID == "PM") %>% group_by(ID,Entr,Sor) %>% summarise(), n = 35)
print(result.final %>% filter(ID == "NP") %>% group_by(ID,Entr,Sor) %>% summarise(), n = 35)
print(result.final %>% filter(ID == "PC") %>% group_by(ID,Entr,Sor) %>% summarise(), n = 35)
print(result.final %>% filter(ID == "FF") %>% group_by(ID,Entr,Sor) %>% summarise(), n = 35)

# result in map
result.LngLat <- GetLngLat(result.final)
ggplot(result.LngLat) + 
  geom_point(aes(ELng, ELat, col = "Entr")) +
  geom_point(aes(SLng, SLat, col = "Sor")) +
  geom_segment(aes(x = ELng, xend = SLng, y = ELat, yend = SLat)) +
  facet_wrap(~ID)

ggplot(result.LngLat %>% filter(ID == "PM")) + 
  geom_point(aes(ELng, ELat, col = "Entr")) +
  geom_point(aes(SLng, SLat, col = "Sor")) +
  geom_segment(aes(x = ELng, xend = SLng, y = ELat, yend = SLat)) +
  facet_wrap(~DOW)


##########
##########
###   Result Visualisation
##########

trx %>% group_by(ID) %>% summarise(Dmin = min(Date), Dmax = max(Date), Ddiff = Dmax - Dmin, 
                                   Day = n_distinct(Date), active = Day / Ddiff,
                                   n = n(), nPerDay = n / Day)

ggplot(trx) + geom_bar(aes(DOW, y = (..count..)/sum(..count..)), binwidth = 1) + facet_wrap(~ID)
ggplot(trx) + geom_density(aes(DOW)) + facet_wrap(~ID)
ggplot(trx) + geom_density(aes(TimeSor)) + facet_wrap(~ID)

temp <- trx
temp[temp$DOW == 0,]$DOW <- 7
temp$WE <- 0
temp[temp$DOW >5,]$WE <- 1

ggplot(trx) + geom_tile(aes(WOY, ID))
ggplot(temp) + geom_tile(aes(Date, ID, fill = as.factor(WE)))

temp %>% group_by(ID) %>% summarise(n = n(), 
                                    D1 = sum(DOW[DOW==1]),
                                    D2 = sum(DOW[DOW==2])/2,
                                    D3 = sum(DOW[DOW==3])/3,
                                    D4 = sum(DOW[DOW==4])/4,
                                    D5 = sum(DOW[DOW==5])/5,
                                    D6 = sum(DOW[DOW==6])/6,
                                    D7 = sum(DOW[DOW==7])/7,
                                    sum = D1+D2+D3+D4+D5+D6+D7
                                    )

PM <- trx %>% filter(ID == "PM", Date < as.Date("2015-6-1"))
PM %>% group_by(Entr, Sor) %>% summarise(n = n()) %>% ungroup() %>% arrange(desc(n))

PM.temp <- train %>% filter(ID == "PM") 
PM.temp %>% group_by(Entr, Sor) %>% summarise(n = n()) %>% ungroup() %>% arrange(desc(n))


##########
##########
### Test
##########
### Fist data view All

##### 
# 20150811
# SO
temp <- Sens(trx)
temp1 <- SO(temp)

temp.L <- GetLngLat(temp)
temp1.L <- GetLngLat(temp1)

ggplot(temp.L) + 
  geom_point(aes(ELng, ELat, col = "Entr")) +
  geom_point(aes(SLng, SLat, col = "Sor")) +
  geom_segment(aes(x = ELng, xend = SLng, y = ELat, yend = SLat))+
  facet_wrap(~ID)
ggplot(temp1.L) + 
  geom_point(aes(ELng, ELat, col = "Entr")) +
  geom_point(aes(SLng, SLat, col = "Sor")) +
  geom_segment(aes(x = ELng, xend = SLng, y = ELat, yend = SLat))+
  facet_wrap(~ID)

rm(temp, temp1, temp.L, temp1.L)
#####


temp <- trx %>% group_by(ID) %>% summarise(Dmin = min(Date), Dmax = max(Date), noPsg = n())
ggplot(temp) + geom_segment(aes(x=Dmin, xend=Dmax, y=as.factor(ID), yend=as.factor(ID), size = noPsg))

ggplot(trx) + 
  geom_point(aes(Date,TimeEntr, shape="Entr")) +
  geom_point(aes(Date,TimeSor, shape="Sor")) +
  geom_segment(aes(x=Date, xend=Date, y=TimeEntr, yend=TimeSor)) +
  facet_wrap(~ID)

trx.LngLat <- GetLngLat(trx)
ggplot(trx.LngLat) + 
  geom_point(aes(ELng, ELat, col = "Entr")) +
  geom_point(aes(SLng, SLat, col = "Sor")) +
  geom_segment(aes(x = ELng, xend = SLng, y = ELat, yend = SLat))+
  facet_wrap(~ID)


##########
### FF
temp <- result.final %>% filter(ID == "PM")
plyr::count(temp, c("Entr", "Sor"))
plyr::count(temp, c("Entr", "Sor", "Tmin"))

##########
### CC
train_decompose.LngLat <- GetLngLat(train_decompose)

ggplot(train_decompose.LngLat) + 
  geom_point(aes(ELng, ELat, col = "Entr")) +
  geom_point(aes(SLng, SLat, col = "Sor")) +
  geom_segment(aes(x = ELng, xend = SLng, y = ELat, yend = SLat)) + 
  facet_wrap(~ID) + ggtitle("Result: Geo-representation")
	
ggplot(train_decompose.LngLat) + 
  geom_point(aes(ELng, ELat, col = "Entr")) +
  geom_point(aes(SLng, SLat, col = "Sor")) +
  geom_segment(aes(x = ELng, xend = SLng, y = ELat, yend = SLat)) 

##########
### PM
##########
##########
### PM

PM <- PM.all %>% filter(Date >= as.Date("2015/1/1"))
PM <- Sens(PM)

PM.ref <- PM %>% group_by(Entr, Sor, Sens) %>%
  summarise(noPsg = n()) %>%
  ungroup() %>%
  arrange(desc(noPsg)) %>%
  mutate(ord = row_number())

# temp <- gares %>% tbl_df() %>% transmute(Sor = Cde, Lib.Sor) 
# PM.ref <- left_join(PM.ref, temp)

PM <- PM %>% inner_join(PM.ref)
PM$Ord <- NA
PM$Ord[PM$ord <= 6] <- paste0(PM$Entr, "-", PM$Sor, ":", PM$Sens)[PM$ord <= 6]

temp <- PM %>% filter(ord %in% 3:4)

ggplot(temp) + geom_bar(aes(DOW),binwidth = 1) + facet_wrap(~Ord, ncol = 1)
ggplot(temp) + geom_bar(aes(WOY),binwidth = 1) + facet_wrap(~Ord, ncol = 1)

ggplot(temp) + 
  geom_tile(aes(WOY, DOW, fill = as.factor(Ord), alpha = .5)) + 
  theme(legend.position = "bottom")

###
temp <- PM %>% 
  arrange(Date, TimeSor) %>% 
  mutate(r = row_number()) %>% 
  select(Entr, Sor, TimeSor, DOW, WOY, Date, Voie, Sens)
# select(Entr, Sor, TimeSor, DOW, WOY, Date, Voie, Sens, Lib, r, Ord)

temp0 <- temp %>% filter( Sor == 25006012 )
ggplot(temp0) + geom_point(aes(Date, TimeSor, col = as.factor(Sens))) + ylim(0,25)

temp1 <- temp %>% 
  filter( (Sens == 1 & Sor == 25006012 & lead(Sens) == 2 & lead(Sor) == 25006012) |
            (Sens == 2 & Sor == 25006012 & lag(Sens) == 1 & lag(Sor) == 25006012)  )

ggplot(temp1) + geom_point(aes(Date, TimeSor, col = as.factor(Sens))) + ylim(0,25)

temp2 <- temp1 %>% filter(
  (Sens == 1 & DOW == (lead(DOW)-1) ) |
    (Sens == 2 & DOW == (lag(DOW)+1) ) 
)
ggplot(temp2) + geom_point(aes(Date, TimeSor, col = as.factor(Sens))) + ylim(0,25)

temp2 <- temp2 %>% mutate(r = floor(row_number()/2 + .5))
temp3 <- temp2
names(temp3) <- c("Entr", "Sor", "TS", "DO", "W", "D", "V","Sens","r")
temp3$Sens <- 3 - temp3$Sens

temp4 <- temp2 %>% inner_join(temp3) %>%
  filter(Date < D)

ggplot(temp4) +
  geom_segment(aes(x=Date, xend = D, y =TimeSor, yend = TS)) +
  geom_point(aes(Date,TimeSor, col = "aller")) +
  geom_point(aes(D,TS, col = "retour"))  + ylim(0,25)


ggplot(temp) + 
  geom_point(aes(Date, TimeSor, col = as.factor(Ord))) + 
  ggtitle("All") + theme(legend.position = "bottom")

ggplot(temp) + geom_bar(aes(WOY), binwidth = 1)
#












##########
### create period
period <- data.frame(Date = seq(train.start, test.end, "day")) %>% tbl_df()
period$DOW <- as.POSIXlt(period$Date)$wday
period$WOY = as.numeric(format(period$Date+3, "%U"))

ggplot(temp) + 
  geom_tile(aes(Date, DOW, fill = as.factor(Ord), alpha = .5)) + 
  theme(legend.position = "bottom") +
  geom_point(data= period , aes(Date, DOW, size = WOY))



#



ggplot(temp) + 
  geom_point(aes(Date, TimeSor, col = as.factor(Ord), size = noPsg)) + 
  geom_segment(aes(WOY, DOW, fill = as.factor(Ord), alpha = .5)) + 
  theme(legend.position = "bottom")


##########
### PM.after
PM.after <- SO(PM) %>% tbl_df()
PM.after.ref <- PM.after %>% group_by(Entr, Sor, Sens) %>%
  summarise(noPsg = n()) %>%
  ungroup() %>%
  arrange(desc(noPsg)) %>%
  mutate(ord = row_number())

##########
### ord: trx -> transaction
ggplot(trx) + geom_point(aes(Date, TimeSor)) + facet_wrap(~ID)

transaction <- Sens(trx) %>% tbl_df()
trx.ref <- transaction %>% group_by(ID, Entr, Sor, Sens) %>%
  summarise(noPsg = n()) %>%
  ungroup() %>%
  group_by(ID) %>%
  arrange(desc(noPsg)) %>%
  mutate(ord = row_number())
transaction <- transaction %>% inner_join(trx.ref)
##########
### viz ord: transaction
ggplot(transaction) + geom_point(aes(Date, TimeSor)) + facet_wrap(~ID) + ggtitle("VIPs-all")
ggplot(transaction) + geom_point(aes(Date, TimeSor, size = noPsg)) + facet_wrap(~ID) + ggtitle("VIPs-all")

ggplot(transaction %>% filter(ord < 6 | noPsg > 50)) + 
  geom_point(aes(Date, TimeSor, size = noPsg)) + 
  facet_wrap(~ID) + ggtitle("VIPs- ord <6 | noPsg > 50")

ggplot(transaction %>% filter(ord < 6 | noPsg > 50)) + 
  geom_point(aes(Date, TimeSor, size = noPsg, col = as.factor(ord))) + 
  facet_wrap(~ID) + ggtitle("VIPs- ord <6 | noPsg > 50, color")

##########
### ord: train + test -> trx.after
trx.after <- rbind(train, test) %>% tbl_df()
trx.after.ref <- trx.after %>% group_by(ID, Entr, Sor, Sens) %>%
  summarise(noPsg = n()) %>%
  ungroup() %>%
  group_by(ID) %>%
  arrange(desc(noPsg)) %>%
  mutate(ord = row_number())
trx.after <- trx.after %>% inner_join(trx.after.ref)
trx.after$Date <- as.Date(trx.after$Date)
##########
### viz ord: trx.after
ggplot(trx.after %>% filter(ord < 6 | noPsg > 50)) + 
  geom_point(aes(Date, TimeSor, size = noPsg, col = as.factor(ord))) + 
  facet_wrap(~ID) + ggtitle("VIPs.after- ord <6 | noPsg > 50, color") +
  scale_size(range = c(0,5))



	
##########
##########
### Reserve
##########
##########
###   Initialisation
# load("Troncons_A789.RData")
# load("A7_par_pk.RData")
# load("A8_par_pk.RData")
# load("A9_par_pk.RData")
# load("Troncons_A7.RData")
# load("Troncons_A8.RData")
# load("Troncons_A9.RData")
# gares <- read.table("garesLatLng.csv", header = T, sep = ",")
# load("MODELE.RData")
garesorder <- rbind(A7_par_pk[,c("Lib","Cde","PK","Lat","Lng")],A8_par_pk[-1,c("Lib","Cde","PK","Lat","Lng")],A9_par_pk[,c("Lib","Cde","PK","Lat","Lng")])

VIP2$Entr <- as.numeric(VIP2$Entr)
VIP2$Sor <- as.numeric(VIP2$Sor)

# remove unuseful data set after Rmd
rm(centers1, centers2, cl1, cl2, gg1, gg2, within.ss, t.kmeans)
rm(T.matin, T.aprem)
rm(i,j,clus)
rm(T, temp)

##########
##########
### data preparation
### ASF
input <- read.table("BDD_ASF_1.csv", sep = ";", header = TRUE)
input <- tbl_df(input)

names(input) <- c("ID", 
                  "pEntr", "sEntr", "cEntr", "nEntr",
                  "pSor", "sSor", "cSor", "nSor",
                  "D1", "D")

input <- input %>% 
  mutate(
    Entr = pEntr * 100000 + sEntr * 1000 + cEntr,
    Sor = pSor * 100000 + sSor * 1000 + cSor,
    Y = substr(D, 1, 4), M = substr(D, 5, 6),Day = substr(D, 7, 8),
    Date = as.Date(paste0(Y, "-", M, "-", Day)),
    DOW = as.POSIXlt(Date)$wday,
    WOY = as.numeric(format(Date+3, "%U")),
    HH = as.numeric(substr(D, 9, 10)), MM = as.numeric(substr(D, 11, 12)),
    TimeSor = HH + MM / 60
  ) %>%
  select(ID, Entr, Sor, Date, DOW, WOY, TimeSor)

temp <- read.table("BDD_ASF_0.csv", sep = ";", header = TRUE)
names(temp) <- c("ID", 
                 "pEntr", "sEntr", "cEntr", "nEntr",
                 "pSor", "sSor", "cSor", "nSor",
                 "D1", "D")
temp <- temp %>% 
  mutate(
    Entr = pEntr * 100000 + sEntr * 1000 + cEntr,
    Sor = pSor * 100000 + sSor * 1000 + cSor,
    Y = substr(D, 1, 4), M = substr(D, 5, 6),Day = substr(D, 7, 8),
    Date = as.Date(paste0(Y, "-", M, "-", Day)),
    DOW = as.POSIXlt(Date)$wday,
    WOY = as.numeric(format(Date+3, "%U")),
    HH = as.numeric(substr(D, 9, 10)), MM = as.numeric(substr(D, 11, 12)),
    TimeSor = HH + MM / 60
  ) %>%
  select(ID, Entr, Sor, Date, DOW, WOY, TimeSor)

input.ASF <- rbind(input, temp)

##########
# model02: Benchmark - consider DOW
##########
matin <- train %>%
  select(ID, Entr, Sor, Date, DOW, TimeSor) %>%
  filter(TimeSor < 12 ) %>%
  group_by(ID, Entr, Sor, DOW) %>%
  summarise(nDOW = n())

aprem <- train %>%
  select(ID, Entr, Sor, Date, DOW, TimeSor) %>%
  filter(TimeSor >= 12 ) %>%
  group_by(ID, Entr, Sor, DOW) %>%
  summarise(nDOW = n())

T.matin <- train %>%
  select(ID, Entr, Sor, Date, DOW, TimeSor) %>%
  filter(TimeSor < 12 ) %>%
  group_by(ID, Entr, Sor, DOW) %>%
  summarise(SD = sd(TimeSor), T = mean(TimeSor),Tmin = T -SD, Tmax = T + SD)

T.aprem <- train %>%
  select(ID, Entr, Sor, Date, DOW, TimeSor) %>%
  filter(TimeSor >= 12 ) %>%
  group_by(ID, Entr, Sor, DOW) %>%
  summarise(SD = sd(TimeSor), T = mean(TimeSor),Tmin = T -SD, Tmax = T + SD)

T.matin <- inner_join(T.matin, matin)
T.aprem <- inner_join(T.aprem, aprem)
T <- rbind(T.matin, T.aprem)

##########
##########
# get model.02 result
test_decompose$result <- 0
for (i in 1:nrow(result)){
  test_decompose[
    test_decompose$ID   == result$ID[i] &
    test_decompose$Entr == result$Entr[i]&
    test_decompose$Sor  == result$Sor[i]&
    test_decompose$DOW  == result$DOW[i]&
    test_decompose$TimeSor >= result$Tmin[i]&
    test_decompose$TimeSor <= result$Tmax[i],
    "result"]<-1
}
##########
##########

Decompose <- function(transaction){
	# break OD to troncon (A7, A8, A9)
	# treat the Systeme Ouvert
	#
	# Args: 
	#	transaction:	ID, Entr, Sor, ...
	#
	# Returns:
	#	OD in A7,A8,A9: OD --> troncons
	#	OD other: does not change
  gares$Autoroute <- as.character(gares$Autoroute)
  transaction$Entr <- as.numeric(as.character(transaction$Entr))
  transaction$Sor <- as.numeric(as.character(transaction$Sor))
  Autoroute <- vector(mode = "character", length = nrow(transaction))
  transaction_decompose <- cbind(transaction, Autoroute)
  transaction_decompose$Autoroute <- as.character(transaction_decompose$Autoroute)
  transaction_restant <- transaction
  Pointeur <- 1
  Pointeur_restant <- 1
  Gare_inconnue <- 0 #Gares du système ouvert que le code ne connait pas
  for (i in 1 : nrow(transaction)){
    if (  ( (transaction$Entr[i] %in% Troncons_A789[,4]) | (transaction$Entr[i] %in% Troncons_A789[,6])  ) & 
          ( (transaction$Sor[i] %in% Troncons_A789[,4]) | (transaction$Sor[i] %in% Troncons_A789[,6])  )  ) { 
      # SI TRAJET DANS LES TROIS AUTOROUTES
      if (gares$Autoroute[match(transaction$Entr[i],gares$Cde)] !=  gares$Autoroute[match(transaction$Sor[i],gares$Cde)] ) { 
        #E=A7etS=A9 ou l'inverse
        if (transaction$Entr[i] != 25004210 & transaction$Sor[i] != 25004210){  
          # Entrée et sortie <> Orange Centre
          if(gares$Autoroute[match(transaction$Entr[i],gares$Cde)] == "A7"){
            # E=A7 S=A9
            newrow1 <- c(transaction[i,1],transaction$Entr[i],25004210,transaction[i, c(4:ncol(transaction))],"A7")
            newrow2 <- c(transaction[i,1],25004210,transaction$Sor[i], transaction[i, c(4:ncol(transaction))],"A9")
            transaction_decompose = rbind(transaction_decompose[1:Pointeur,],newrow1,newrow2,transaction_decompose[-(1:Pointeur),])
            Pointeur <- Pointeur +2
          }
          else if(gares$Autoroute[match(transaction$Entr[i],gares$Cde)] == "A9"){
            # E=A9 S=A7
            newrow1 <- c(transaction[i,1],transaction$Entr[i],25004210,transaction[i, c(4:ncol(transaction))],"A9")
            newrow2 <- c(transaction[i,1],25004210,transaction$Sor[i], transaction[i, c(4:ncol(transaction))],"A7")
            transaction_decompose = rbind(transaction_decompose[1:Pointeur,],newrow1,newrow2,transaction_decompose[-(1:Pointeur),])
            Pointeur <- Pointeur +2
          }
        }
        else { # E=A9 S=OC ou l'inverse
          transaction_decompose$Autoroute[Pointeur] <- "A9"
        }
      }
      else {
        transaction_decompose$Autoroute[Pointeur] <- gares$Autoroute[match(transaction$Entr[i],gares$Cde)]
      }
      if (Pointeur_restant == 1){
        transaction_restant <- transaction_restant[2:nrow(transaction_restant),]
      }
      else {
        transaction_restant <- rbind(transaction_restant[(1:(Pointeur_restant-1)),],transaction_restant[-(1:(Pointeur_restant)),])
        Pointeur_restant <- Pointeur_restant - 1
      }
    }
    
    ## ??? deactivate: no information about Voie
    else if (transaction$Entr[i] < 0 & ( (transaction$Sor[i] %in% Troncons_A789[,4])|(transaction$Sor[i] %in% Troncons_A789[,6]) ) ){ 
      #Si Entr = 0 et Sor dans A789
      if (transaction$Sor[i] == 25006001){ #Canet de mereuil
        if (transaction$Voie[i] >= 20 ){#Sortie
          transaction_decompose[Pointeur,] <- c(transaction$ID[i],25004278 ,25006001,transaction[i, c(4:ncol(transaction))],"A8")
        } #On crée un trajet Coudoux -> Canet de méreuil
        else { 
          transaction_decompose[Pointeur,] <- c(transaction$ID[i], 25006001 ,25004278,transaction[i, c(4:ncol(transaction))],"A8")
        }  #Trajet Canet -> Coudoux
      }
      else if (transaction$Sor[i] == 25006010){ #Fréjus
        if (transaction$Voie[i] >= 20 ){#Sortie
          transaction_decompose[Pointeur,] <- c(transaction$ID[i],25006022 ,25006010,transaction[i, c(4:ncol(transaction))],"A8")
        } #On crée un trajet Antibes PV Nord-> Fréjus
        else { 
          transaction_decompose[Pointeur,] <- c(transaction$ID[i], 25006010 ,25006012,transaction[i, c(4:ncol(transaction))],"A8")
        }  #Trajet Fréjus -> Antibes PV 
      }
      else if (transaction$Sor[i] == 25006011){ #Les Adrets
        if (transaction$Voie[i] >= 20 ){#Sortie
          transaction_decompose[Pointeur,] <- c(transaction$ID[i],25006022 ,25006011,transaction[i, c(4:ncol(transaction))],"A8")
        } #On crée un trajet Antibes PV Nord-> Les Adrets
        else { 
          transaction_decompose[Pointeur,] <- c(transaction$ID[i], 25006011 ,25006012,transaction[i, c(4:ncol(transaction))],"A8")
        }  #Trajet Les Adrets -> Antibes PV 
      }
      else if (transaction$Sor[i] == 25006014){ #Antibes Ouest
        if (transaction$Voie[i] >= 20 ){#Sortie
          transaction_decompose[Pointeur,] <- c(transaction$ID[i],25006009 ,25006014,transaction[i, c(4:ncol(transaction))],"A8")
        } #On crée un trajet Capitou -> Antibes Ouest
        else { 
          transaction_decompose[Pointeur,] <- c(transaction$ID[i], 25006014 ,25006009,transaction[i, c(4:ncol(transaction))],"A8")
        }  #Trajet Antibes Ouest -> Capitou 
      }
      else if (transaction$Sor[i] == 25006012){ #Antibes PV
        if (transaction$Voie[i] >= 20 ){#PV Sud, donc de Cannes vers Nice
          transaction_decompose[Pointeur,] <- c(transaction$ID[i],25006009 ,25006015,transaction[i, c(4:ncol(transaction))],"A8")
        } #On crée un trajet Capitou -> Cagnes Ouest Nord
        else { #PV Nord, de Nice vers Cannes
          transaction_decompose[Pointeur,] <- c(transaction$ID[i], 25006015 ,25006009,transaction[i, c(4:ncol(transaction))],"A8")
        }  #Trajet Cagnes Ouest Nord -> Capitou 
      }
      else if (transaction$Sor[i] == 25006024){ #Sophia
        transaction_decompose[Pointeur,] <- c(transaction$ID[i],25006024 ,25006020,transaction[i, c(4:ncol(transaction))],"A8")
        #Entrée seule, don trajet Sophia -> Saint Isidore
      }
      else if (transaction$Sor[i] == 25006013){ #Antibes Est
        if (transaction$Voie[i] >= 20 ){#Sortie
          transaction_decompose[Pointeur,] <- c(transaction$ID[i],25006020 ,25006013,transaction[i, c(4:ncol(transaction))],"A8")
        } #On crée un trajet Saint Isidore -> Antibes Est
        else { 
          transaction_decompose[Pointeur,] <- c(transaction$ID[i], 25006013 ,25006020,transaction[i, c(4:ncol(transaction))],"A8")
        }  #Trajet Antibes Est -> Saint Isidore
      }
      else if (transaction$Sor[i] == 25006017){ #Cagnes Ouest Sud
        transaction_decompose[Pointeur,] <- c(transaction$ID[i],25006017 ,25006020,transaction[i, c(4:ncol(transaction))],"A8")
        #On crée un trajet  Cagnes Ouest Sud -> Saint Isidore
      }
      else if (transaction$Sor[i] == 25006015){ #Cagnes Ouest Nord
        transaction_decompose[Pointeur,] <- c(transaction$ID[i],25006020 ,25006015,transaction[i, c(4:ncol(transaction))],"A8")
        #On crée un trajet  Saint Isidore -> Cagnes Ouest Nord
      }
      else if (transaction$Sor[i] == 25006016){ #Cagnes Est
        if (transaction$Voie[i] >= 20 ){#Sortie
          transaction_decompose[Pointeur,] <- c(transaction$ID[i],25006020 ,25006016,transaction[i, c(4:ncol(transaction))],"A8")
        } #On crée un trajet Saint Isidore -> Cagnes Est
        else { 
          transaction_decompose[Pointeur,] <- c(transaction$ID[i], 25006016 ,25006020,transaction[i, c(4:ncol(transaction))],"A8")
        }  #TrajetCagnes Est -> Saint Isidore
      }
      else if (transaction$Sor[i] == 25006021){ #Saint Isidore Ech Est
        if (transaction$Voie[i] >= 20 ){#Sortie
          transaction_decompose[Pointeur,] <- c(transaction$ID[i],25006027 ,25006021,transaction[i, c(4:ncol(transaction))],"A8")
        } #On crée un trajet La Turbie -> Saint Isidore Ech Est
        else { 
          transaction_decompose[Pointeur,] <- c(transaction$ID[i], 25006021 ,25006027,transaction[i, c(4:ncol(transaction))],"A8")
        }  #Trajet Saint Isidore Ech Est -> La Turbie
      }
      else if (transaction$Sor[i] == 25006021){ #Saint Isidore Ech Est
        if (transaction$Voie[i] >= 20 ){#Sortie
          transaction_decompose[Pointeur,] <- c(transaction$ID[i],25006027 ,25006021,transaction[i, c(4:ncol(transaction))],"A8")
        } #On crée un trajet La Turbie -> Saint Isidore Ech Est
        else { 
          transaction_decompose[Pointeur,] <- c(transaction$ID[i], 25006021 ,25006027,transaction[i, c(4:ncol(transaction))],"A8")
        }  #Trajet Saint Isidore Ech Est -> La Turbie
      }
      else if (transaction$Sor[i] == 25006019){ #Saint Isidore Ech Ouest
        if (transaction$Voie[i] >= 20 ){#Sortie
          transaction_decompose[Pointeur,] <- c(transaction$ID[i],25006012 ,25006019,transaction[i, c(4:ncol(transaction))],"A8")
        } #On crée un trajet Antibes PV -> Saint Isidore Ech Ouest
        else { 
          transaction_decompose[Pointeur,] <- c(transaction$ID[i], 25006019 ,25006012,transaction[i, c(4:ncol(transaction))],"A8")
        }  #Trajet Saint Isidore Ech Ouest -> Antibes PV
      }
      else if (transaction$Sor[i] == 25006020){ #Saint Isidore PV
        if (transaction$Voie[i] >= 20 ){#Nord, donc de l'Italie vers Cannes
          transaction_decompose[Pointeur,] <- c(transaction$ID[i],25006026 ,25006020,transaction[i, c(4:ncol(transaction))],"A8")
        } #On crée un trajet La Turbie Ech -> Cagnes Ouest Nord
        else { #Sud, de Cannes vers l'Italie
          transaction_decompose[Pointeur,] <- c(transaction$ID[i], 25006020 ,25006026,transaction[i, c(4:ncol(transaction))],"A8")
        }  #Trajet Cagnes Ouest Nord -> La Turbie Ech
      }
      else if (transaction$Sor[i] == 25006026){ #La Turbie Ech
        if (transaction$Voie[i] >= 20 ){#Sortie
          transaction_decompose[Pointeur,] <- c(transaction$ID[i],25006020 ,25006026,transaction[i, c(4:ncol(transaction))],"A8")
        } #On crée un trajet Saint isidore PV -> La Turbie Ech
        else { 
          transaction_decompose[Pointeur,] <- c(transaction$ID[i], 25006026 ,25006020,transaction[i, c(4:ncol(transaction))],"A8")
        }  #Trajet La Turbie Ech -> Saint isidore PV
      }
      else if (transaction$Sor[i] == 25006027){ #La Turbie PV
        if (transaction$Voie[i] >= 20 ){# Nord, donc de l'Italie vers Cannes
          transaction_decompose[Pointeur,] <- c(transaction$ID[i],25006027 ,25006026,transaction[i, c(4:ncol(transaction))],"A8")
        } #On crée un trajet La Turbie PV -> La Turbie Ech
        else { #Sud, de Cannes vers l'Italie
          transaction_decompose[Pointeur,] <- c(transaction$ID[i], 25006026 ,25006027,transaction[i, c(4:ncol(transaction))],"A8")
        }  #Trajet La Turbie Ech -> La Turbie PV
      }
      else { Gare_inconnue <- Gare_inconnue +1
      }
    }
    Pointeur <- Pointeur +1
    Pointeur_restant <- Pointeur_restant +1
  }
  
  transaction_decompose <- transaction_decompose[transaction_decompose$Autoroute > 0,]
  
  
  #DECOMPOSER LES OD PAR TRONCONS :
  transaction_par_troncons <- data.frame(ID = "", ID_Troncon= 0, Autoroute= "", Entr= 0, Sor=0,Date = 0, DOW=0, WOY=0, TimeEntr = 0, TimeSor =0, Sens = 0)
  transaction_par_troncons$Autoroute <- as.character(transaction_par_troncons$Autoroute)
  # ??? transaction_decompose$Year <- as.character(transaction_decompose$Year)
  transaction_par_troncons$ID <- as.character(transaction_par_troncons$ID)
  
  for (i in 1:nrow(transaction_decompose)){
    if ( transaction_decompose$Autoroute[i] == "A7"){
      entree <- match(transaction_decompose$Entr[i],A7_par_pk$Cde)
      sortie <- match(transaction_decompose$Sor[i],A7_par_pk$Cde)
      if ( entree < sortie ) { # SENS 1
        for ( j in entree : (sortie-1) ){
          transaction_par_troncons <- rbind(transaction_par_troncons,c(transaction_decompose$ID[i],Troncons_A7$ID_Troncon[j],"A7",A7_par_pk$Cde[j],A7_par_pk$Cde[j+1],transaction_decompose$Date[i],transaction_decompose$DOW[i],transaction_decompose$WOY[i],transaction_decompose$TimeEntr[i],transaction_decompose$TimeSor[i],1))
        }
      }
      else{ #SENS 2
        for ( j in entree : (sortie+1) ){
          transaction_par_troncons <- rbind(transaction_par_troncons,c(transaction_decompose$ID[i],Troncons_A7$ID_Troncon[j-1],"A7",A7_par_pk$Cde[j],A7_par_pk$Cde[j-1],transaction_decompose$Date[i],transaction_decompose$DOW[i],transaction_decompose$WOY[i],transaction_decompose$TimeEntr[i],transaction_decompose$TimeSor[i],2))
        }
      }
    }
    else if ( transaction_decompose$Autoroute[i] == "A8"){
      entree <- match(transaction_decompose$Entr[i],A8_par_pk$Cde)
      sortie <- match(transaction_decompose$Sor[i],A8_par_pk$Cde)
      if ( entree < sortie ) { # SENS 1
        for ( j in entree : (sortie-1) ){
          transaction_par_troncons <- rbind(transaction_par_troncons,
                                            c(transaction_decompose$ID[i],Troncons_A8$ID_Troncon[j],"A8",
                                              A8_par_pk$Cde[j],A8_par_pk$Cde[j+1],transaction_decompose$Date[i],transaction_decompose$DOW[i],transaction_decompose$WOY[i],transaction_decompose$TimeEntr[i],transaction_decompose$TimeSor[i],1))
        }
      }
      else{ #SENS 2
        for ( j in entree : (sortie+1) ){
          transaction_par_troncons <- rbind(transaction_par_troncons,c(transaction_decompose$ID[i],Troncons_A8$ID_Troncon[j-1],"A8",A8_par_pk$Cde[j],A8_par_pk$Cde[j-1],transaction_decompose$Date[i],transaction_decompose$DOW[i],transaction_decompose$WOY[i],transaction_decompose$TimeEntr[i],transaction_decompose$TimeSor[i],2))
        }
      }
    }
    else if ( transaction_decompose$Autoroute[i] == "A9"){
      entree <- match(transaction_decompose$Entr[i],A9_par_pk$Cde)
      sortie <- match(transaction_decompose$Sor[i],A9_par_pk$Cde)
      if ( entree < sortie ) { # SENS 1
        for ( j in entree : (sortie-1) ){
          transaction_par_troncons <- rbind(transaction_par_troncons,c(transaction_decompose$ID[i],Troncons_A9$ID_Troncon[j],"A9",A9_par_pk$Cde[j],A9_par_pk$Cde[j+1],transaction_decompose$Date[i],transaction_decompose$DOW[i],transaction_decompose$WOY[i],transaction_decompose$TimeEntr[i],transaction_decompose$TimeSor[i],1))
        }
      }
      else{ #SENS 2
        for ( j in entree : (sortie+1) ){
          transaction_par_troncons <- rbind(transaction_par_troncons,c(transaction_decompose$ID[i],Troncons_A9$ID_Troncon[j-1],"A9",A9_par_pk$Cde[j],A9_par_pk$Cde[j-1],transaction_decompose$Date[i],transaction_decompose$DOW[i],transaction_decompose$WOY[i],transaction_decompose$TimeEntr[i],transaction_decompose$TimeSor[i],2))
        }
      }
    }
  }
  transaction_par_troncons <- transaction_par_troncons[-1,]
	

### test
	#Rajouter demi trajet LANCON LA BARQUE
	Pointeur <- 1
	for (i in 1:nrow(transaction_par_troncons)){
	  if (!is.na(transaction_par_troncons$Sor[Pointeur]) &
		transaction_par_troncons$Sor[Pointeur] == 25004220 ){ # (A7 -> Lancon)
	    newrow1 <- c(transaction_par_troncons$ID[Pointeur],25004220,25004278,transaction_decompose$Date[Pointeur],transaction_decompose$DOW[Pointeur],transaction_decompose$WOY[Pointeur],transaction_decompose$TimeEntr[Pointeur],transaction_decompose$TimeSor[Pointeur])
	    newrow2 <- c(transaction_par_troncons$ID[Pointeur],25004278,25004279,transaction_decompose$Date[Pointeur],transaction_decompose$DOW[Pointeur],transaction_decompose$WOY[Pointeur],transaction_decompose$TimeEntr[Pointeur],transaction_decompose$TimeSor[Pointeur])
	    transaction_par_troncons <- rbind(transaction_par_troncons[(1:Pointeur),],newrow1,newrow2,transaction_par_troncons[-(1:Pointeur),])
	    Pointeur <- Pointeur + 2
	  } 
	  if (!is.na(transaction_par_troncons$Entr[Pointeur]) &
		transaction_par_troncons$Entr[Pointeur] == 25004220){ # (Lancon -> A7)
	    newrow1 <- c(transaction_par_troncons$ID[Pointeur],25004220,25004278,transaction_decompose$Date[Pointeur],transaction_decompose$DOW[Pointeur],transaction_decompose$WOY[Pointeur],transaction_decompose$TimeEntr[Pointeur],transaction_decompose$TimeSor[Pointeur])
	    newrow2 <- c(transaction_par_troncons$ID[Pointeur],25004278,25004279,transaction_decompose$Date[Pointeur],transaction_decompose$DOW[Pointeur],transaction_decompose$WOY[Pointeur],transaction_decompose$TimeEntr[Pointeur],transaction_decompose$TimeSor[Pointeur])
	    transaction_par_troncons <- rbind(transaction_par_troncons[(1:Pointeur),],newrow1,newrow2,transaction_par_troncons[-(1:Pointeur),])
	    Pointeur <- Pointeur + 2
	  }
	  if (!is.na(transaction_par_troncons$Entr[Pointeur]) &
		transaction_par_troncons$Entr[Pointeur] == 25006002){ # (La Barque -> A8)
	    newrow1 <- c(transaction_par_troncons$ID[Pointeur],25004279,25006001,transaction_decompose$Date[Pointeur],transaction_decompose$DOW[Pointeur],transaction_decompose$WOY[Pointeur],transaction_decompose$TimeEntr[Pointeur],transaction_decompose$TimeSor[Pointeur])
	    newrow2 <- c(transaction_par_troncons$ID[Pointeur],25006001,25006080,transaction_decompose$Date[Pointeur],transaction_decompose$DOW[Pointeur],transaction_decompose$WOY[Pointeur],transaction_decompose$TimeEntr[Pointeur],transaction_decompose$TimeSor[Pointeur])
	    newrow3 <- c(transaction_par_troncons$ID[Pointeur],25006080,25006002,transaction_decompose$Date[Pointeur],transaction_decompose$DOW[Pointeur],transaction_decompose$WOY[Pointeur],transaction_decompose$TimeEntr[Pointeur],transaction_decompose$TimeSor[Pointeur])
	    transaction_par_troncons <- rbind(transaction_par_troncons[(1:Pointeur),],newrow1,newrow2,newrow3,transaction_par_troncons[-(1:Pointeur),])
	    Pointeur <- Pointeur + 3
	  }
	  if (!is.na(transaction_par_troncons$Sor[Pointeur]) &
		transaction_par_troncons$Sor[Pointeur] == 25006002){ # (A8 -> La Barque)
	    newrow1 <- c(transaction_par_troncons$ID[Pointeur],25004279,25006001,transaction_decompose$Date[Pointeur],transaction_decompose$DOW[Pointeur],transaction_decompose$WOY[Pointeur],transaction_decompose$TimeEntr[Pointeur],transaction_decompose$TimeSor[Pointeur])
	    newrow2 <- c(transaction_par_troncons$ID[Pointeur],25006001,25006080,transaction_decompose$Date[Pointeur],transaction_decompose$DOW[Pointeur],transaction_decompose$WOY[Pointeur],transaction_decompose$TimeEntr[Pointeur],transaction_decompose$TimeSor[Pointeur])
	    newrow3 <- c(transaction_par_troncons$ID[Pointeur],25006080,25006002,transaction_decompose$Date[Pointeur],transaction_decompose$DOW[Pointeur],transaction_decompose$WOY[Pointeur],transaction_decompose$TimeEntr[Pointeur],transaction_decompose$TimeSor[Pointeur])
	    transaction_par_troncons <- rbind(transaction_par_troncons[(1:Pointeur),],newrow1,newrow2,newrow3,transaction_par_troncons[-(1:Pointeur),])
	    Pointeur <- Pointeur + 3
	  }
	  Pointeur <- Pointeur + 1
	}
	

	
  transaction_par_troncons$Date <- as.Date(as.numeric(transaction_par_troncons$Date), origin = as.Date("1970-1-1"))
  transaction_par_troncons$TimeEntr <- as.numeric(transaction_par_troncons$TimeEntr)
  transaction_par_troncons$TimeSor <- as.numeric(transaction_par_troncons$TimeSor)
  transaction_par_troncons <- transaction_par_troncons[, c(1,4:(ncol(transaction_par_troncons)-1))]
  
  trx <- rbind(transaction_par_troncons, transaction_restant)
  trx$DOW <- as.numeric(trx$DOW)
  trx$WOY <- as.numeric(trx$WOY)
  trx <- tbl_df(trx)
  return(trx)
}
