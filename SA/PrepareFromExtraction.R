### prepare input files from extractions from BO & ESCOTA

### get ref
ref <- read.table("Reference/Ref_VIPn.csv", header = T, sep = ";") %>%
  tbl_df %>% mutate(ID = as.character(ID))

##########
### escota files
##########
t <- read.table("ExtractionDB/VIP_Escota_v20151209.csv", sep = ";", header = TRUE) %>% tbl_df

names(t) <- c("sSor", "cSor", "Voie", "DateSor", "hSor",
              "Badge", "Porteur",
              "sEntr", "cEntr", "DateEntr","hEntr",  "voieEntr")

t2 <- t %>%  
  mutate(
    ID = Badge * 1e5 + Porteur,
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
  select(ID,Badge,Porteur, cEntr, Entr, Sor, Voie, Date, DOW, WOY, TimeEntr, TimeSor) 

t2[t2$cEntr == 0, ]$Entr <- 0 
t2[t2$cEntr != 0, ]$Voie <- 0 
t2$cEntr <- NULL

t2 <- t2 %>% mutate(ID = as.character(ID))

t <- full_join(t2,ref)

# t[is.na(t$Nom),]%>% select(- (Entr:TimeSor)) %>% distinct
# t[is.na(t$Badge),]%>% select(- (Entr:TimeSor)) %>% distinct

t.ESCOTA <- t %>% 
  filter(ID != 819831400001 & ID != 815815900011) %>%
  select(-c(ID, Nom,N))

##########
### BO files
##########
t <- read.table("ExtractionDB/VIP_BO_v20151210.csv", sep = ";", header = TRUE) %>% tbl_df

names(t) <- c("cEntr","cSor", "na","ID","DEntr", "DSor", "na2","E","S","Ste", "Badge", "Porteur" )

t1 <- t %>% 
  mutate(
    ID = as.character(Badge * 1e5 + Porteur),
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
  select(ID, Ste, Badge, Porteur, Entr, Sor, Date, DOW, WOY, TimeEntr, TimeSor)

t1$Voie <- 0

t2 <- left_join(t1, ref)

t.BO <- t2 %>% select(-c(ID, Nom,N)) %>% distinct

##########
### Output
##########
t <- rbind(t.BO,t.ESCOTA)

time <- Sys.time() %>% format(format = "%Y%m%d")
write.table(t, paste0("Input/VIP_v",time,".csv"),sep=";",row.name=FALSE,quote=FALSE)
rm(t.ESCOTA,t.BO,t,t1,t2)
