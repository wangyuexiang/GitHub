library(dplyr)

filename.Input <- "VIP.csv"
day.start <- as.Date("2015-5-1")
day.end <- as.Date("2015-8-1")

input <- read.table(paste0("Input/",filename.Input), header = T, sep = ";") %>% tbl_df 

trx <- input %>%
  mutate(
    ID = as.character(Badge * 100000 + Porteur),
    Date = as.Date(as.character(Date)),
    Sens = ifelse(Entr == 0,
                  ifelse(Voie <=20, 1,2),
                  0)) %>%
  filter(Date >= day.start & Date <= day.end)

temp <- trx %>% select(Badge, Porteur, ID) %>% distinct
temp <- left_join(ref,temp)
write.table(temp,file = "Reference/Ref_VIPn.csv",sep=";",row.name=FALSE,quote=FALSE)

######
ref <- read.table("Reference/Ref_VIPn.csv", header = T, sep = ";") %>%
  tbl_df %>% mutate(ID = as.character(ID))

t <- left_join(result.TS,ref) %>% distinct
t1 <- left_join(result.ZW,ref) 

t2 <- count(t,Nom) %>% rename(nTS = n)
t3 <- count(t1,Nom) %>% rename(nZW = n)
t4 <- full_join(t2,t3)

##########
### combine Algo 1 & 2
##########
t <- trx

### find trx in result.TS
t1 <- result.TS %>% transmute(OD =as.character(OD))
t2 <- read.table(text = t1$OD,sep="-") %>% tbl_df %>%
  transmute(Entr = V1, Sor = V2)
t1 <- cbind(result.TS,t2) %>% tbl_df

t3 <- inner_join(t,t1)
t4 <- t3 %>% filter(TimeSor <= Tmax & TimeSor >= Tmin)
t5 <- t4 %>% select(Ste: Sens) %>% distinct

t5$TS <- TRUE

trx.Algo2 <- left_join(t,t5) %>% filter(is.na(TS)) %>% select(-TS)
rm(t1,t2,t3,t4,t5)
