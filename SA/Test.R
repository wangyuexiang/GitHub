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
