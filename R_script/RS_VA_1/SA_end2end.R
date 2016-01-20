##########
##########
# first: 20150723
# last modified: 20150730
# click button

##########
##########
### EXPLANATION
# !!! action to be added
# ??? action with doubt

##########
##########
### call packages
library(dplyr)
library(cluster)
library(ggplot2)
library(gridExtra)
library(knitr)

##########
##########
### data preparation
# load data
# !!! to be replaced by: csv -> data.frame  
 load("VIP2.RData")

# get history for: CC,FF,NP,PC
# from 2015-1-1 to 2015-5-28

# Input (Pre-model):
#	trx:		ID Entr Sor Date DOW WOY TimeEntr TimeSor result
# 		train
#		test
#	ID.list:	ID

### from csv to data.frame to tbl
input <- read.table("Tis_historique.csv", sep = ",", header = TRUE)
input <- tbl_df(input)

names(input) <- c("pays", "ste", "ID", "badge", 
                  "sEntr", "cEntr", "voieEntr", "DateEntr", "hEntr", 
                  "sSor", "cSor", "voieSor", "hSor", "DateSor")

# badge
input$Societe <- input$pays * 100 + input$ste

input$pays <- NULL
input$ste <- NULL

# Cde for Entr & Sor
input$Entr <- 25000000 + input$sEntr * 1000 + input$cEntr
input$Sor <- 25000000 + input$sSor * 1000 + input$cSor
input[input$cEntr == 0, ]$Entr <- 0 

input$sEntr <- NULL
input$cEntr <- NULL
input$sSor <- NULL
input$cSor <- NULL

# DateSor
input$Y <- substr(input$DateSor, 1, 4)
input$M <- substr(input$DateSor, 5, 6)
input$D <- substr(input$DateSor, 7, 8)
input$Date <- as.Date(paste0(input$Y, "-", input$M, "-", input$D))

input$Y <- NULL
input$M <- NULL
input$D <- NULL

# HeureSor
input$H <- substr(input$hSor, 1, 2)
input$M <- substr(input$hSor, 3, 4)
input$TimeSor <- as.numeric(input$H) + as.numeric(input$M) / 60 

input$H <- substr(input$hEntr, 1, 2)
input$M <- substr(input$hEntr, 3, 4)
input$TimeEntr <- as.numeric(input$H) + as.numeric(input$M) / 60 

input[is.na(input$TimeEntr), ]$TimeEntr <- input[is.na(input$TimeEntr), ]$TimeSor - .5 

input$H <- NULL
input$M <- NULL

# Final
Input <- input %>% select(Societe, ID, Entr, Sor, voieSor, Date, TimeEntr, TimeSor)
Input$DOW <- as.POSIXlt(Input$Date)$wday 

# get ID.ref
ID.ref <- read.table("ID.ref.csv", sep = ",", header = TRUE)
names(ID.ref)[c(1,3,6)] <- c("Nom", "NOM", "ID")
ID.ref <- ID.ref %>% select(Nom, ID)

# join Input & ID.ref
Input <-  inner_join(Input, ID.ref)
# print(count(Input, ID) %>% full_join(ID.ref), n = 22)

# predefined parameters
train.start <- as.Date("2015-1-1")
test.start <- as.Date("2015-5-1")
test.end <- as.Date("2015-5-28")
test.period <- data.frame(Date = seq(test.start, test.end, "day"))
test.period$DOW <- as.POSIXlt(test.period$Date)$wday

trx <- tbl_df(VIP2 %>% 
# !!! remove this phrase to have the result of all VIP in VIP2
  filter(ID %in% c("CC", "FF", "NP", "PC")) %>%
  filter(Date >= train.start & Date < test.end))
# remove Lng & Lat
trx <- trx[, -c(9:13)]
trx <- trx %>% filter(DOW < 7)

# construct train & test set
train <- trx %>% filter(Date < test.start)
test <- trx %>% filter(Date >= test.start)

# get the ID list
ID.list <- trx %>% group_by(ID) %>% summarise()

##########
##########
### Train Model
### Explanation of Models
# return:
#	result.model.##:  	ID Entr Sor DOW Tmin Tmax Model
#	test.model.##:			ID Entr SOr Date DOW WOY TimeEntr TimeSor result
#	ind.model.##:				ID Ind1 Ind2 Ind3 Ind Model
##########
# regardless of DOW: make difference between weekdays and weekends
# consider DOW: treat each day of week separately 
##########
# Model 0: Benchmark
#		Model 00: regardless of DOW		done
#		Model 01: weekday & weekend		done
#		Model 02: consider DOW				done
# Model 1: Time - Space
# 	Model 10: regardless of DOW		done
# 	Model 11: weekday & weekend   done
# 	Model 12: consider DOW				done
# Model 2: Space - Time
# 	Model 20: regardless of DOW		done
# 	Model 21: weekday & weekend   done
# 	Model 22: consider DOW				done
##########
##########

####################
####################
####################
### model.0:	Benchmark
####################
####################
### model.00: Benchmark - regardless of DOW
##########
T.matin <- train %>%
  filter(TimeSor < 12 ) %>%
  group_by(ID, Entr, Sor) %>%
  summarise(n = n(), T = mean(TimeSor), SD = sd(TimeSor), Tmin = T -SD, Tmax = T + SD)

T.aprem <- train %>%
  filter(TimeSor >= 12 ) %>%
  group_by(ID, Entr, Sor) %>%
  summarise(n = n(), T = mean(TimeSor), SD = sd(TimeSor), Tmin = T -SD, Tmax = T + SD)

T <- rbind(T.matin, T.aprem)
# ??? limit = 5 to be justified or modified
T <- T %>% filter(n > 5)

# add DOW
T$DOW <- 0
temp <- T
for(i in 1:6) {
	# add DOW to the T
	temp$DOW <- i
	T <- rbind(T, temp)
}

result.model.00 <- T[, c("ID", "Entr", "Sor", "DOW", "Tmin", "Tmax")]
result.model.00$Model <- 00
##########
### evalutaion model.00
test.model.00 <- GetResult(test, result.model.00)
ind.model.00 <- GetInd(test.model.00, result.model.00)
ind.model.00$Model <- 00


####################
####################
### model.01: Benchmark - weekdays & weekends
##########
temp <- train 
temp$weekday <- 0
temp[temp$DOW %in% c(1:5), ]$weekday <- 1

T.matin <- temp %>%
  filter(TimeSor < 12 ) %>%
  group_by(ID, Entr, Sor, weekday) %>%
  summarise(n = n(), T = mean(TimeSor), SD = sd(TimeSor), Tmin = T -SD, Tmax = T + SD)

T.aprem <- temp %>%
  filter(TimeSor >= 12 ) %>%
  group_by(ID, Entr, Sor, weekday) %>%
  summarise(n = n(), T = mean(TimeSor), SD = sd(TimeSor), Tmin = T -SD, Tmax = T + SD)

T <- rbind(T.matin, T.aprem)
# ??? limit = 5 to be justified or modified
T <- T %>% filter(n > 5)

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

result.model.01 <- result[, c("ID", "Entr", "Sor", "DOW", "Tmin", "Tmax")]
result.model.01$Model <- 01
##########
### evalutaion model.01
test.model.01 <- GetResult(test, result.model.01)
ind.model.01 <- GetInd(test.model.01, result.model.01)
ind.model.01$Model <- 01

####################
####################
### model.02: Benchmark - consider DOW
##########
T.matin <- train %>%
  filter(TimeSor < 12 ) %>%
  group_by(ID, Entr, Sor, DOW) %>%
  summarise(nDOW = n(), T = mean(TimeSor), SD = sd(TimeSor), Tmin = T -SD, Tmax = T + SD)

T.aprem <- train %>%
  filter(TimeSor >= 12 ) %>%
  group_by(ID, Entr, Sor, DOW) %>%
  summarise(nDOW = n(), T = mean(TimeSor), SD = sd(TimeSor), Tmin = T -SD, Tmax = T + SD)

T <- rbind(T.matin, T.aprem)
# ??? limit = 5 to be justified or modified
T <- T %>% filter(nDOW > 5)

result.model.02 <- T[, c("ID", "Entr", "Sor", "DOW", "Tmin", "Tmax")]
result.model.02$Model <- 02
##########
### evalutaion model.02
test.model.02 <- GetResult(test, result.model.02)
ind.model.02 <- GetInd(test.model.02, result.model.02)
ind.model.02$Model <- 02


####################
####################
####################
### model.1:	Time-Space
# call the function to get the troncons
train_decompose <- AfterDecompose(Decompose(BeforeDecompose(train)))  
test_decompose <- AfterDecompose(Decompose(BeforeDecompose((test))))

####################
####################
### model.10: Time-Space - regardless of DOW
# clustering TimeSor
result <- data.frame(ID="", Entr=0, Sor=0, SD=0, T=0, Tmin=0, Tmax=0, n=0)
for (i in 1:nrow(ID.list)) {
  temp <- train_decompose %>% filter(ID == ID.list$ID[i])
  # ??? limit = 10 to be justified or modified
  if(nrow(temp) >= 10) {
		max.cluster <- length(unique(temp$TimeSor))
    # if not many passages, we will not cluster
    # decide nb of cluster
    clus<- clusGap(temp[,"TimeSor"], kmeans, min(9, max.cluster))
    n.cluster <- with(clus,maxSE(Tab[,"gap"],Tab[,"SE.sim"]))
    
    set.seed(1234)
    temp.kmeans <-   kmeans(temp[, "TimeSor"], centers = n.cluster)
    temp$cluster <- temp.kmeans$cluster
    T <- temp %>%
      group_by(ID, Entr, Sor, cluster) %>%
      summarise(n = n(), T = mean(TimeSor), SD = sd(TimeSor), Tmin = T -SD, Tmax = T + SD)
		
		# remove line with SD = N.A.
    T <- T %>% filter(n > 1)
    
    T$cluster <- NULL
    result <- rbind(result, T)
    } # end of if
} # end of loop i

result <- result[-1,]
# ??? parameter: 5
result <- result %>% filter(n > 5)

# add DOW
result$DOW <- 0
temp <- result
for(i in 1:6) {
	# add DOW to the T
	temp$DOW <- i
	result <- rbind(result, temp)
}

result.model.10 <- result[, c("ID", "Entr", "Sor", "DOW", "Tmin", "Tmax")]
result.model.10$DOW <- as.numeric(result.model.10$DOW)
result.model.10$Model <- 10
##########
### evalutaion model.10
test.model.10 <- GetResult(test_decompose, result.model.10)
ind.model.10 <- GetInd(test.model.10, result.model.10)
ind.model.10$Model <- 10



####################
####################
### model1.11: Time-Space - weekdays & weekends
temp1 <- train_decompose 
temp1$weekday <- 0
temp1[temp1$DOW %in% c(1:5), ]$weekday <- 1

# clustering TimeSor
result <- data.frame(ID="", Entr=0, Sor=0, weekday = 0, SD=0, T=0, Tmin=0, Tmax=0, n=0)
for (i in 1:nrow(ID.list)) {
	for (j in 0:1) {
	  temp <- temp1 %>% filter(ID == ID.list$ID[i] & weekday == j)
	  # ??? limit = 10 to be justified or modified
	  if(nrow(temp) >= 10) {
			max.cluster <- length(unique(temp$TimeSor))
	    # if not many passages, we will not cluster
	    # decide nb of cluster
	    clus<- clusGap(temp[,"TimeSor"], kmeans, min(9, max.cluster))
	    n.cluster <- with(clus,maxSE(Tab[,"gap"],Tab[,"SE.sim"]))
    
	    set.seed(1234)
	    temp.kmeans <-   kmeans(temp[, "TimeSor"], centers = n.cluster)
	    temp$cluster <- temp.kmeans$cluster
	    T <- temp %>%
	      group_by(ID, Entr, Sor, weekday, cluster) %>%
	      summarise(n = n(), T = mean(TimeSor), SD = sd(TimeSor), Tmin = T -SD, Tmax = T + SD)
		
			# remove line with SD = N.A.
	    T <- T %>% filter(n > 1)
    
	    T$cluster <- NULL
	    result <- rbind(result, T)
	    } # end of if
	} # end of loop j
} # end of loop i

result <- result[-1,]
# ??? parameter: 5
result <- result %>% filter(n > 5)

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

result.model.11 <- result[, c("ID", "Entr", "Sor", "DOW", "Tmin", "Tmax")]
result.model.11$DOW <- as.numeric(result.model.11$DOW)
result.model.11$Model <- 11
##########
### evalutaion model.11
test.model.11 <- GetResult(test_decompose, result.model.11)
ind.model.11 <- GetInd(test.model.11, result.model.11)
ind.model.11$Model <- 11


####################
####################
### model.12: Time-Space - consider DOW
# clustering TimeSor
result <- data.frame(ID="", Entr=0, Sor=0, DOW=0, SD=0, T=0, Tmin=0, Tmax=0, n=0)
for (i in 1:nrow(ID.list)) {
  for (j in 0:6) {
    temp <- train_decompose %>% filter(ID == ID.list$ID[i] & DOW == j)
    # ??? limit = 10 to be justified or modified
    if(nrow(temp) >= 10) {
			max.cluster <- length(unique(temp$TimeSor))
      # if not many passages, we will not cluster
      # decide nb of cluster
      clus<- clusGap(temp[,"TimeSor"], kmeans, min(9, max.cluster))
      n.cluster <- with(clus,maxSE(Tab[,"gap"],Tab[,"SE.sim"]))
      
      set.seed(1234)
      temp.kmeans <-   kmeans(temp[, "TimeSor"], centers = n.cluster)
      temp$cluster <- temp.kmeans$cluster
      T <- temp %>%
        group_by(ID, Entr, Sor, DOW, cluster) %>%
        summarise(n = n(), T = mean(TimeSor), SD = sd(TimeSor), Tmin = T -SD, Tmax = T + SD)
			
			# remove line with SD = N.A.
      T <- T %>% filter(n > 1)
      
      T$cluster <- NULL
      result <- rbind(result, T)
      } # end of if
    } # end of loop j
} # end of loop i

result <- result[-1,]
# ??? parameter: 5
result <- result %>% filter(n > 5)
result.model.12 <- result[, c("ID", "Entr", "Sor", "DOW", "Tmin", "Tmax")]
result.model.12$DOW <- as.numeric(result.model.12$DOW)
result.model.12$Model <- 12
##########
### evalutaion model.12
test.model.12 <- GetResult(test_decompose, result.model.12)
ind.model.12 <- GetInd(test.model.12, result.model.12)
ind.model.12$Model <- 12




####################
####################
####################
### model.2:	OD -> Space -> Time
#######
# Create VIP2
####### 
VIP2 <- VIP2[VIP2$ID %in% c("CC","FF", "NP", "PC") & VIP2$Date > as.Date("2014-12-31") & VIP2$Date < as.Date("2015-5-29"), ]
x <- vector(mode = "integer", length = nrow(VIP2))

VIP2 <- cbind(VIP2[,1:3],x,x,x,x,x,x,VIP2$TimeEntr,VIP2$TimeSor,VIP2$DOW,VIP2$WOY,VIP2$Date)
colnames(VIP2)<- c(colnames(MODELE),"Date")

######
# predefined parameters
# train.start <- as.Date("2015-1-1")
# test.start <- as.Date("2015-5-1")
# test.end <- as.Date("2015-5-28")
# test.period <- data.frame(Date = seq(test.start, test.end, "day"))
# test.period$DOW <- as.POSIXlt(test.period$Date)$wday

VIP2_pour_modele <- VIP2[VIP2$Date < as.Date("2015-5-1"),]
VIP2_pour_test <- VIP2[VIP2$Date >= as.Date("2015-5-1"),]

VIP2_decompose <- Decompose(VIP2_pour_modele)
VIP2_pour_test_par_troncons <- Decompose(VIP2_pour_test)

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
# ??? [Pourcentagedumax max ; max ]
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


####################
####################
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

##########
### evalutaion model.20
test.model.20 <- GetResult(VIP2_pour_test_par_troncons, result.model.20)
ind.model.20 <- GetInd(test.model.20, result.model.20)
ind.model.20$Model <- 20


####################
####################
### Model 21 : Space - Time & Weekdays vs weekends
########
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

##########
### evalutaion model.21
test.model.21 <- GetResult(VIP2_pour_test_par_troncons, result.model.21)
ind.model.21 <- GetInd(test.model.21, result.model.21)
ind.model.21$Model <- 21


####################
####################
###  Model 22 : Space - Time & by DOW
########
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

##########
### evalutaion model.22
test.model.22 <- GetResult(VIP2_pour_test_par_troncons, result.model.22)
ind.model.22 <- GetInd(test.model.22, result.model.22)
ind.model.22$Model <- 22









##########
##########
# compare model results
##########
Ind <- rbind(ind.model.00, ind.model.01, ind.model.02, 
						 ind.model.10, ind.model.11, ind.model.12, 
						 ind.model.20, ind.model.21, ind.model.22)
Ind.result <- Ind %>% group_by(ID) %>% summarise(Model = sum(Model[Ind == max(Ind)]))

result <- rbind(result.model.00, result.model.01, result.model.02, 
								result.model.10, result.model.11, result.model.12,
								result.model.20, result.model.21, result.model.22)

result.final <- inner_join(result, Ind.result)
result.LngLat <- GetLngLat(result)

# output: csv file
result.final$Model <- NULL
write.table(result.final, file="result.csv", sep = ";", row.names = F, quote = F)


