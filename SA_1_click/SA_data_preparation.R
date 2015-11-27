##########
##########
# SA_data_preparation.R
# first: 20150806
# from csv to data.frame/tbl

# from 20151001 - 45 VIP
#   history:    BDD.v20151001.csv
#   reference:  Ref.ID.v20151001.csv

BDD <- read.table("BDD.v20151001.csv", sep = ";", header = TRUE) %>% tbl_df
BDD <- BDD %>% 
  mutate(ID = as.character(ID),
         Date = as.Date(as.character(Date)),
         Nom = as.character(Nom)
         )

ref <- read.table("Ref.ID.v20151001.csv", sep = ";", header = TRUE) %>% tbl_df
ref <- ref %>% mutate(ID = as.character(ID))
ref <- ref %>% mutate(N = row_number())
ref <- ref %>% filter(N !=43)
ref <- ref %>% mutate(N = row_number())

temp <- ref %>% select(ID, N)
BDD <- inner_join(BDD, temp)

BDD <- BDD %>% distinct



result <- read.table("result.v20151001.csv", sep = ";", header = TRUE) %>% tbl_df
result <- result %>% mutate(ID = as.character(ID), OD = as.character(OD))

result <- inner_join(result, temp)
result <- result %>% distinct


##########
# BDD.BO
##########
t <- read.table("BDD.BO.v20151022.csv", sep = ";", header = TRUE) %>% tbl_df
t <- read.table("BDD.fabrice_v20151119.csv", sep = ";", header = TRUE) %>% tbl_df

names(t) <- c("cEntr","cSor", "na","badge","DEntr", "DSor", "na2","Ste", "ID" )

t1 <- t %>% 
  mutate(
    ID,
    Entr = 25004000 + cEntr,
    Sor = 25004000 + cSor,
    Y = substr(DSor, 7, 10), M = substr(DSor, 4, 5),Day = substr(DSor, 1, 2),
    Date = as.Date(paste0(Y, "-", M, "-", Day)),
    DOW = as.POSIXlt(Date)$wday,
    WOY = as.numeric(format(Date+3, "%U")),
    HH = as.numeric(substr(DEntr, 12, 13)), MM = as.numeric(substr(DEntr, 15, 16)),
    TimeEntr = HH + MM / 60,
    HH = as.numeric(substr(DSor, 12, 13)), MM = as.numeric(substr(DSor, 15, 16)),
    TimeSor = HH + MM / 60
  ) %>%
  select(ID, Entr, Sor, Date, DOW, WOY, TimeEntr, TimeSor)

t1$Voie <- 0

t1 <- t1 %>%
  mutate(ID = as.character(903965600001),
         Ste = 25004,
         EVA = 376235,
         Nom = "Fabrice Frajut",
         N = 4
  )

t2 <- BDD %>% filter(N == 4)
t3 <- rbind(t1,t2) %>% distinct
BDD <- rbind(BDD, t1) %>% distinct

##########
# BDD.ESCOTA
##########
t <- read.table("BDD.ESCOTA.v20151029.csv", sep = ";", header = TRUE) %>% tbl_df
t <- read.table("BDD_FF_v20151120.csv", sep = ";", header = TRUE) %>% tbl_df
t <- read.table("BDD_PB_v20151120.csv", sep = ";", header = TRUE) %>% tbl_df
t <- read.table("BDD.ESCOTA.v20151029.csv", sep = ";", header = TRUE) %>% tbl_df

names(t) <- c("sSor", "cSor", "Voie", "DateSor", "hSor",
              "ID", "porteur",
              "sEntr", "cEntr", "DateEntr","hEntr",  "voieEntr")

t2 <- t %>%  
  mutate(
    ID = ID * 1e5 + porteur,
    Entr = 25000000 + sEntr * 1000 + cEntr,
    Sor = 25000000 + sSor * 1000 + cSor,
    Y = substr(DateSor, 1, 4), M = substr(DateSor, 5, 6), D = substr(DateSor, 7, 8),
    Date = as.Date(paste0(Y, "-", M, "-", D)),
    DOW = as.POSIXlt(Date)$wday,
    WOY = as.numeric(format(Date+3, "%U")),
    HH = hEntr %/% 10000, MM = ((hEntr %% 10000) %/% 100),
    TimeEntr = HH + MM / 60,
    HH = hSor %/% 10000, MM = ((hSor %% 10000) %/% 100),
    TimeSor = HH + MM / 60
  ) %>%
  select(ID, cEntr, Entr, Sor, Voie, Date, DOW, WOY, TimeEntr, TimeSor) 

t2[t2$cEntr == 0, ]$Entr <- 0 
t2[t2$cEntr != 0, ]$Voie <- 0 
t2$cEntr <- NULL

t2 <- t2 %>% mutate(ID = as.character(ID))
temp <- t2

### 20151123 integrate the latest history of FF & PB
temp_PB <- inner_join(t2,ref)
temp_FF <- inner_join(t2,ref)
BDD <- rbind(BDD, temp_PB,temp_FF)
write.table(BDD, file="BDD.v20151123.csv", sep = ";", row.names = F, quote = F)
###

t2 <- t1 %>% mutate(Nom = as.character(Nom))

############
t <- rbind(t1,t2)
temp <- count(t2,ID)
temp$Nom <- "Magali Mannarini"
temp$Ste <- 25006
temp$EVA <- 2424049
temp$N <- 50
temp$n <- NULL
ref <- rbind(ref,temp)


temp <- temp[-c(1,2),]
temp$Nom <-c("James",
             "Julien-sous prefet",
             "Stephane LOQUET",
             "Patricia LANTERI",
             "Denis LAURENT")
temp$Ste <- c(25006, 25004, 25004, 25006, 25006)

temp <- temp[c(4,5,3,1,2),]
temp$EVA <- NA
temp$n <- NULL

ref.temp <- rbind(ref %>% select(ID,Ste,EVA,Nom), temp)
ref.temp <- ref.temp %>% mutate(N = row_number())

t <- t %>% mutate(ID = as.character(ID))
t0 <- inner_join(t,ref.temp)

BDD.temp <- rbind(BDD,t0)

##########
### change EVA number
ref$EVA[ref$N == 23] <- 2629771 
ref$EVA[ref$N == 19] <- 2424144
ref$EVA[ref$N == 38] <- 598771

result$EVA[result$N == 23] <- 2629771 
result$EVA[result$N == 19] <- 2424144
result$EVA[result$N == 38] <- 598771

write.table(ref, file="Ref.ID.v20151102.csv", sep = ";", row.names = F, quote = F)
write.table(ref.temp, file="Ref.ID.v20151029.csv", sep = ";", row.names = F, quote = F)
write.table(BDD.temp, file="BDD.old.v20151022.csv", sep = ";", row.names = F, quote = F)
write.table(BDD, file="BDD.v20151119.csv", sep = ";", row.names = F, quote = F)

t1 <- left_join(result.final, ref)
t2 <- rbind(result,t1)

r <- read.table(file="Result.v20151026.csv", sep = ";", header = TRUE) %>% tbl_df

rbind(r,t1)
result <- rbind(r,t1)

write.table(result, file="Result.v20151102.csv", sep = ";", row.names = F, quote = F)

t1 <- left_join(result.final, ref)
t2 <- rbind(result,t1)

r <- read.table(file="Result.v20151026.csv", sep = ";", header = TRUE) %>% tbl_df

rbind(r,t1)
result <- rbind(r,t1)

write.table(result, file="Result.v20151029.csv", sep = ";", row.names = F, quote = F)

##########
### For SA_Script
##########
t <- BDD %>%
  mutate(Badge = ID) %>%
  select(EVA, Badge, Entr, Sor, Date, DOW, WOY, TimeEntr, TimeSor, Voie, Ste)
write.table(t, "VIP.csv",sep=";",row.name=FALSE,quote=FALSE)
