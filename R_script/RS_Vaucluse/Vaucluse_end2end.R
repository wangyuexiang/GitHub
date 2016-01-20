library(dplyr)
library(cluster)
library(ggplot2)
library(zoo)
#####
# get the ID list
ID.list <- train %>% group_by(ID) %>% summarise( n = n()) %>% ungroup() %>% arrange(desc(n))

# !!! get running time
t0 <- Sys.time()
########
#Find the number max of cluster by the density of TimeSor over 24h
# For each person
models.units <- getModel.units( train )

###?
# train <- inverse.after.SO(train)
# test <- inverse.after.SO(test)

# !!! get running time
t2 <- Sys.time()

#Model 0 (decades) : BENCHMARK
result.model.decade.0 <- Model.for.a.decade(train,0,models.units)

# !!! get running time
t3 <- Sys.time()

#Model 1 (decades)	Time-Space
result.model.decade.1 <- Model.for.a.decade(train,1,models.units)

# !!! get running time
t4 <- Sys.time()

#Model 2 (decades) OD -> Space -> Time
result.model.decade.2 <- Model.for.a.decade(train,2,models.units)

# !!! get running time
t5 <- Sys.time()

##########
# evaluation
##########
####################
####################
### evalutaion model.00
test.model.0 <- GetResult(test, result.model.decade.0)
test.model.0$ModelDecade <- 0
ind.model.0 <- GetInd(test.model.0, result.model.decade.0)
ind.model.0$ModelDecade <- 0
### evalutaion model.01
test.model.1 <- GetResult(test, result.model.decade.1)
test.model.1$ModelDecade <- 1
ind.model.1 <- GetInd(test.model.1, result.model.decade.1)
ind.model.1$ModelDecade <- 1
### evalutaion model.02
test.model.2 <- GetResult(test, result.model.decade.2)
test.model.2$ModelDecade <- 2
ind.model.2 <- GetInd(test.model.2, result.model.decade.2)
ind.model.2$ModelDecade <- 2

# !!! get running time
t6 <- Sys.time()

##########
# compare model results
##########
Ind <- rbind(ind.model.0, ind.model.1, ind.model.2)
Ind <- inner_join ( Ind, models.units)

Ind.final <- Ind %>% 
  group_by(ID) %>% 
  summarise( Model = ModelDecade[Ind == max(Ind)][1]*10 + model[Ind == max(Ind)][1])

result <- rbind(result.model.decade.0, result.model.decade.1, result.model.decade.2)

result.final <- inner_join(result, Ind.final)

# !!! get running time
t7 <- Sys.time()