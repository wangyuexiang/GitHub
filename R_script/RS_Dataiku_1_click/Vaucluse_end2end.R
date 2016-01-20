library(dplyr)
library(cluster)
library(ggplot2)

# predefined parameters
train.start <- as.Date("2014-1-1")
test.start <- as.Date("2014-6-1")
test.end <- as.Date("2014-6-30")
test.period <- data.frame(Date = seq(test.start, test.end, "day"))
test.period$DOW <- as.POSIXlt(test.period$Date)$wday
test.period$DOW <- as.character(test.period$DOW)

# !!! get running time
t0 <- Sys.time()

trx <- trx %>% filter(Date >= train.start & Date < test.end)
# construct train & test set
train <- trx %>% filter(Date < test.start)
test <- trx %>% filter(Date >= test.start)

# combine OD to create trajet
# train <- Sens(train)
# test <- Sens(test)
# train <- SO(train) %>% ungroup()
# test <- SO(test) %>% ungroup()

# get the ID list
ID.list <- trx %>% group_by(ID, Label) %>% summarise( n = n()) %>% ungroup() %>% arrange(desc(n))



### temp
train0 <- train
list <- ID.list %>% slice(1:5)
train <- train0 %>% inner_join(list)

# !!! get running time
t1.preparatin.end <- Sys.time()

####################
### model.0:	Benchmark
####################
### model.00: Benchmark - regardless of DOW
result.model.00 <- Model(train, 0)
### model.01: Benchmark - weekdays & weekends
result.model.01 <- Model(train, 1)
### model.02: Benchmark - consider DOW
result.model.02 <- Model(train, 2)

# !!! get running time
t2.model0.end <- Sys.time()

####################
### model.1:	Time-Space
####################
### model.10: Time-Space - regardless of DOW
result.model.10 <- Model(train, 10)
### model1.11: Time-Space - weekdays & weekends
result.model.11 <- Model(train, 11)
### model.12: Time-Space - consider DOW
result.model.12 <- Model(train, 12)

# !!! get running time
t3.model1.end <- Sys.time()


##########
# evaluation
##########
####################
### model.0:	Benchmark
####################
### evalutaion model.00
test.model.00 <- GetResult(test, result.model.00)
test.model.00$Model <- 00
ind.model.00 <- GetInd(test.model.00, result.model.00)
ind.model.00$Model <- 00
### evalutaion model.01
test.model.01 <- GetResult(test, result.model.01)
test.model.01$Model <- 01
ind.model.01 <- GetInd(test.model.01, result.model.01)
ind.model.01$Model <- 01
### evalutaion model.02
test.model.02 <- GetResult(test, result.model.02)
test.model.02$Model <- 02
ind.model.02 <- GetInd(test.model.02, result.model.02)
ind.model.02$Model <- 02

# !!! get running time
t4.model0.evaluation.end <- Sys.time

####################
### model.1:	Time-Space
####################
### evalutaion model.10
test.model.10 <- GetResult(test, result.model.10)
test.model.10$Model <- 10
ind.model.10 <- GetInd(test.model.10, result.model.10)
ind.model.10$Model <- 10
### evalutaion model.11
test.model.11 <- GetResult(test, result.model.11)
test.model.11$Model <- 11
ind.model.11 <- GetInd(test.model.11, result.model.11)
ind.model.11$Model <- 11
### evalutaion model.12
test.model.12 <- GetResult(test, result.model.12)
test.model.12$Model <- 12
ind.model.12 <- GetInd(test.model.12, result.model.12)
ind.model.12$Model <- 12

# !!! get running time
t5.model1.evaluation.end <- Sys.time()




##########
# compare model results
##########
Ind <- rbind(ind.model.00, ind.model.01, ind.model.02, 
             ind.model.10, ind.model.11, ind.model.12, 
             ind.model.20, ind.model.21, ind.model.22)

Ind.final <- Ind %>% group_by(ID) %>% summarise(Model = Model[Ind == max(Ind)][1])

result <- rbind(result.model.00, result.model.01, result.model.02, 
                result.model.10, result.model.11, result.model.12,
                result.model.20, result.model.21, result.model.22)

result.final <- inner_join(result, Ind.result)