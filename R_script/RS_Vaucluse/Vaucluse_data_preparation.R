# get the first 500 users
# temp <- trx.ref %>% slice(4951:5000)
# temp$ID <- temp$Label
# temp$Label <- NULL
# temp$n <- NULL
# trx <- inner_join(temp, transaction)

# predefined parameters
train.start <- as.Date("2014-1-1")
test.start <- as.Date("2014-6-1")
test.end <- as.Date("2014-6-30")
test.period <- data.frame(Date = seq(test.start, test.end, "day"))
test.period$DOW <- as.POSIXlt(test.period$Date)$wday
test.period$DOW <- as.integer(test.period$DOW)

transaction[is.na(transaction$TimeEntr),]$TimeEntr<-0
transaction <- transaction %>% filter(Date >= train.start & Date < test.end)

trx <- transaction %>% filter(as.numeric(ID) >= 3501 & as.numeric(ID) <= 5000 )
# # !!! get running time
# t1 <- Sys.time()
# trx <- Sens(trx)
# t2 <- Sys.time()
# trx <- SO(trx)
# t3 <- Sys.time()
# 
# trx.SO <- rbind(trx.SO,trx)


# construct train & test set
train <- trx %>% filter(Date < test.start)
test <- trx %>% filter(Date >= test.start)

# combine OD to create trajet
# train <- Sens(train)
# test <- Sens(test)
# train <- SO(train) %>% ungroup()
# test <- SO(test) %>% ungroup()

# get the ID list
ID.list <- trx %>% group_by(ID) %>% summarise( n = n()) %>% ungroup() %>% arrange(desc(n))

