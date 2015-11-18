##########
### 4250 abonnes app VA
##########
library(dplyr)

##########
### BDD Ref
##########
sens = read.table("Ref_sens.csv",sep = ";", header=TRUE)
JF = read.table("Ref_JF.csv",sep = ";", header=TRUE)

##########
### start/end date
##########
train.start <- as.Date("2015-5-1")
test.start <- as.Date("2015-8-1")
test.end <- as.Date("2015-8-31")

##########
### input -> transaction -> transaction1
##########
transaction <- read.table("BDD.csv", header = T, sep = ";")
transaction <- tbl_df(transaction)

names(transaction) <- c("EVA","Badge",
                         "cde_soc_entr","cde_entr", "heure_entr",
                         "cde_soc_sor","cde_sor", "heure_sor",
                         "Voie")

str(transaction)

# some gares are with cde_soc_entr as character, just removed
temp <- transaction %>%
  mutate(
    cde_soc_entr = as.numeric(as.character(cde_soc_entr)),
    cde_soc_sor = as.numeric(as.character(cde_soc_sor)),
    heure_entr = as.character(heure_entr),
    heure_sor = as.character(heure_sor)) %>%
  filter(!is.na(cde_soc_entr) & !is.na(cde_soc_sor))

# remove cde_soc_sor == A1 - A7, Z2

temp <- temp %>%
  mutate(
    Entr = 25000000 + cde_soc_entr * 1000 + cde_entr,
    Sor = 25000000 + cde_soc_sor * 1000 + cde_sor ,
    Y = substr(heure_sor, 7, 10), M = substr(heure_sor, 4, 5), D = substr(heure_sor, 1, 2),
    Date = as.Date(paste0(Y, "-", M, "-", D)),
    DOW = as.POSIXlt(Date)$wday,
    WOY = as.numeric(format(Date+3, "%U")),
    HH = as.numeric(substr(heure_entr, 12, 13)), MM = as.numeric(substr(heure_entr, 15, 16)) ,
    TimeEntr = HH + MM / 60,
    HH = as.numeric(substr(heure_sor, 12, 13)), MM = as.numeric(substr(heure_sor, 15, 16)) ,
    TimeSor = HH + MM / 60
  ) %>%
  select(EVA,Badge, cde_entr, Entr, Sor, Voie, Date, DOW, WOY, TimeEntr, TimeSor) 

temp$Entr[temp$cde_entr==0] <- 0
temp$TimeEntr[temp$cde_entr==0] <- 0

transaction1 <- temp %>% select(EVA, Badge, Entr, Sor, Date, DOW, WOY, TimeEntr, TimeSor, Voie)
rm(temp)

transaction1 <- transaction1 %>% filter(Date >= as.Date("2015-1-1"))
transaction1 <- transaction1 %>% mutate(Sens = ifelse(Entr == 0,
                                                      ifelse(Voie <=20, 1,2),
                                                      0))

##########
### ID
##########
ID <- transaction1 %>% 
  group_by(Badge) %>% 
  summarise(Dmin = min(Date), 
            Dmax = max(Date), 
            Ddiff = Dmax - Dmin + 1, 
            Day = n_distinct(Date), 
            noPsg = n()) %>%
  arrange(desc(noPsg)) %>%
  mutate(ID = as.character(row_number())) 

temp <- ID %>% select(ID, Badge)
transaction1 <- inner_join(temp,transaction1)
rm(temp)

##########
### transaction 2 <- period
##########
transaction2 <- transaction1 %>% filter(Date >= train.start)

# period <- data.frame(Date = seq(train.start, test.end, "day"))
t1 <- data.frame(Date = seq(train.start, test.end, "day")) %>% transmute(Dmin = Date, t=1)
t2 <- t1 %>% rename(Dmax = Dmin)
t <- inner_join(t1,t2) %>% filter(Dmin < Dmax) %>% select(-t) %>% tbl_df

t1 <- data.frame(D0 = 0,
                 D1 = 0,
                 D2 = 0,
                 D3 = 0,
                 D4 = 0,
                 D5 = 0,
                 D6 = 0,
                 Day = 0
)
for(i in 1:nrow(t)){
  temp <- GetDays(t$Dmin[i],t$Dmax[i])
  t1 <- rbind(t1,temp)
}

t1 <- t1[-1,]
period <- cbind(t, t1 ) %>% tbl_df
rm(t,t1,t2)

##########
### ID2 <- period
##########
ID2 <- transaction2 %>% 
  group_by(ID) %>% 
  summarise(Dmin = min(Date), 
            Dmax = max(Date), 
            Ddiff = Dmax - Dmin + 1, 
            Day = n_distinct(Date), 
            noPsg = n()) %>%
  arrange(desc(noPsg))

ID2 <- left_join(ID2,period %>% select(-Day))

##########
### ID2 - segmentation
##########
### add small =
#   F:  history interesting
#   T:  nOD < 10            
#       Day < 5 
#       Ddiff < 5
ID2$Small <- FALSE
ID2$Small[ID2$noPsg < 10 | ID2$Day < 5 | ID2$Ddiff < 5] <- TRUE
### add Inactive =
#   F: always active
#   T: no trx after 2015/8/1
ID2$Inactive <- FALSE
ID2$Inactive[ID2$Dmax < as.Date("2015/8/1")] <- TRUE

##########
### ID3 -> transaction3 -> ID.OD3
##########
ID3 <- ID2 %>%
  filter(Small == FALSE, Inactive == FALSE) %>%
  select(-c(Small, Inactive))

t <- ID3 %>% select(ID)
transaction3 <- inner_join(transaction2, t)

ID.OD3 <- transaction3 %>%
  group_by(ID, Entr, Sor, Sens) %>% 
  summarise(Dmin = min(Date), 
            Dmax = max(Date), 
            Ddiff = Dmax - Dmin + 1, 
            Day = n_distinct(Date), 
            noPsg = n(),
            
            R0 = sum(DOW == 0),
            R1 = sum(DOW == 1 & JF == 0),
            R2 = sum(DOW == 2 & JF == 0),
            R3 = sum(DOW == 3 & JF == 0),
            R4 = sum(DOW == 4 & JF == 0),
            R5 = sum(DOW == 5 & JF == 0),
            R6 = sum(DOW == 6)
  ) 

ID.OD3 <- ID.OD3 %>%
  ungroup() %>%
  group_by(ID) %>%
  arrange(desc(noPsg)) %>%
  mutate(ord = row_number())

t <- ID3 %>% transmute(ID, Total = noPsg)
ID.OD3 <- inner_join(ID.OD3,t)

ID.OD3 <- ID.OD3 %>% 
  mutate(Per = noPsg / Total,
         noPsgbyDay = noPsg / Day)

t <- transaction3 %>%
  group_by(ID) %>%
  summarise(Wmin = min(WOY),
            Wmax = max(WOY),
            Wdiff = Wmax - Wmin + 1, 
            Week = n_distinct(WOY)
            )

t <- t %>% mutate(ActiveW = Week/ Wdiff)

##########
### ID.OD3 -> 
##########
t1 <- ID.OD3 %>%
  summarise(
    Total = max(Total),
    noOD = max(ord),
    maxNoPsg = max(noPsg),
    maxNoPsgbyDay = ceiling(max(noPsgbyDay)),
    maxPer = max(Per),
    Psg100 = sum(noPsg >= 100),
    Psg50 = sum(noPsg >= 50 & noPsg < 100),
    Psg10 = sum(noPsg >= 10 & noPsg < 50),
    Psg5 = sum(noPsg >= 5 & noPsg < 10),
    Psg2 = sum(noPsg >= 2 & noPsg < 5),
    Psg1 = sum(noPsg == 1)
  )

count(t1,maxNoPsgbyDay) # frequently used trajet
count(t1, Psg1 == noOD) # only small trajet
count(t1, sum(Psg1,Psg2,Psg5) == noOD)

ggplot(ID.OD3) + geom_bar(aes(as.numeric(ID), fill = noPsg), binwidth = 1)


ID.OD3 <- ID.OD3 %>% mutate(Label = ifelse(noPsg>=100, 
                                           100,
                                           ifelse(noPsg >= 50,
                                                  50,
                                                  ifelse(noPsg >= 10,
                                                         10,
                                                         ifelse(noPsg >= 5,
                                                                5,
                                                                ifelse(noPsg >=2,
                                                                       2,
                                                                       1))))))

ggplot(ID.OD3) + 
  geom_bar(aes(as.numeric(ID), fill = as.factor(Label)), binwidth = 1) +
  coord_flip()


ID.Entr3

##########
### Get transaction4 
##########
t <- ID.OD3 
### t
### add small =
#   F:  history interesting
#   T:  nOD < 10            
#       Day < 5 
#       Ddiff < 5
t$Small <- FALSE
t$Small[t$noPsg < 5 | t$Day < 5 | t$Ddiff < 5] <- TRUE
### add Inactive =
#   F: always active
#   T: no trx after 2015/8/1
t$Inactive <- FALSE
t$Inactive[t$Dmax < as.Date("2015/8/1")] <- TRUE

ID.OD4 <- t %>%
  filter(Small == FALSE & Inactive == FALSE) %>%
  select(ID,Entr,Sor,Sens)

transaction4 <- inner_join(transaction3,ID.OD4)

##########
### extraction - save intermediate results
##########
write.table(transaction2,"transaction2.csv",sep=";",row.name=FALSE,quote=FALSE)
write.table(ID,"ID.csv",sep=";",row.name=FALSE,quote=FALSE)
write.table(ID2,"ID2.csv",sep=";",row.name=FALSE,quote=FALSE)
write.table(ID.OD4,"ID.OD4.csv",sep=";",row.name=FALSE,quote=FALSE)
write.table(result.final,"result.final.csv",sep=";",row.name=FALSE,quote=FALSE)

t <- transaction2
t1 <- inner_join(t,t.segment)
t1 <- t1 %>% filter(Seg == "5_high_potential")
t1$Seg <- NULL
write.table(t1,"Hihg_potential.csv",sep=";",row.name=FALSE,quote=FALSE)

##########
### construct Grid
##########
### construct t.grid system
### !!! parameter step = .5
step <- .5
t.lat <- data.frame(
  d = seq(42, 49 - step, step),
  u = seq(42 + step, 49, step)
) %>% 
  tbl_df %>%
  mutate(Row = row_number())

t.lng <- data.frame(
  l = seq(-2, 8 - step, step),
  r = seq(-2 + step, 8, step)
) %>% 
  tbl_df %>%
  mutate(Col = row_number())

t.lat$t <- 1
t.lng$t <- 1
t.grid <- inner_join(t.lat, t.lng) %>% tbl_df %>% select(Row, Col, u, d, l, r)
rm(t.lat,t.lng)

### get all the trajectory possible
t <- sens %>% select(Entr, Sor) %>% distinct %>% tbl_df
t1 <- gares %>% transmute(Entr = Cde, Elng = Lng, Elat = Lat)
t <- left_join(temp, t1)
t1 <- gares %>% transmute(Sor = Cde, Slng = Lng, Slat = Lat)
t <- left_join(temp, t1)

### !!! parameter delta.Lat = delta.Lng = .1
delta.Lat = .1
delta.Lng = .1

### Convert (Entr,Sor) to rectangle(U,D,L,R)
t1 <- t %>% 
  filter(is.na(Elat) & !is.na(Slat)) %>%
  mutate(u=Slat + delta.Lat ,
         d=Slat - delta.Lat,
         r=Slng + delta.Lng, 
         l=Slng - delta.Lng)

t2 <- t %>% 
  filter(!is.na(Elat) & is.na(Slat)) %>%
  mutate(u=Elat + delta.Lat,
         d=Elat - delta.Lat,
         r=Elng + delta.Lng,
         l=Elng - delta.Lng)

t3 <- t %>%
  filter(!is.na(Elat) & !is.na(Slat)) %>%
  mutate(
    u = ( Elat + Slat + abs(Elat - Slat) )/2,
    d = ( Elat + Slat - abs(Elat - Slat) )/2,
    r = ( Elng + Slng + abs(Elng - Slng) )/2,
    l = ( Elng + Slng - abs(Elng - Slng) )/2
  )

temp <- rbind(t1,t2,t3) %>% rename(U=u, D=d, L=l, R=r)
temp <- temp %>%
  mutate(t = 1) %>%
  select(- c(Elng,Elat,Slng,Slat))

### Link Rectangle to small rectangle in t.grid
Grid <- data.frame(Entr = 0, Sor = 0, Row = 0, Col = 0) %>% tbl_df

for(i in 1:nrow(temp1)){
  t1 <- temp %>% slice(i)
  t2 <- inner_join(t.grid, t1, by = "t") %>%
    filter(u > D,
           d < U,
           r > L,
           l < R) %>%
    select(Entr,Sor,Row,Col)
  
  Grid <- rbind(Grid,t2)   
}

Grid <- Grid %>% tbl_df %>% slice(-1)
grid.display <- t.grid %>% mutate(OD = paste0(Row,"-",Col))

##########
### viz Grid
##########
ggplot(grid.display) +
  geom_tile(aes(l,d, fill = l, col = d, alpha = .1)) +
  geom_point(data = gares, aes(Lng, Lat))
  geom_point(data = gares, aes(Lng, Lat, col = as.factor(Societe)))


##########
### Segmentation
##########
result.TS <- read.table(file = "result.final.TimeSpace.v20151029.csv", header = TRUE, sep = ";") %>% tbl_df %>% mutate(ID = as.character(ID))
result.Geo <- read.table(file = "result.treated.Geographic.v20151105.csv", header = TRUE, sep = ";") %>% tbl_df %>% mutate(ID = as.character(ID))

t <- GetCharacters(transaction1)

### by TimeSpace Model
t1 <- result.TS %>% select(ID) %>% distinct(ID)
t1$ResultTS <- TRUE
t <- left_join(t,t1)
t$ResultTS[is.na(t$ResultTS)] <- FALSE

### by Geographic Model
t1 <- result.Geo %>% select(ID) %>% distinct(ID)
t1$ResultGeo <- TRUE
t <- left_join(t,t1)
t$ResultGeo[is.na(t$ResultGeo)] <- FALSE

# get Recent
t$Recent <- FALSE
t$Recent[t$Dmin > as.Date("2015-8-15")] <- TRUE

# get GoodTrx
t1 <- ID.OD4 %>% select(ID) %>% distinct(ID)
t1$GoodTrx <- TRUE
t <- left_join(t,t1)
t$GoodTrx[is.na(t$GoodTrx)] <- FALSE

# get Small
t$Small <- FALSE
t$Small[t$Day < 5 | t$noPsg < 20 | t$Ddiff < 5] <- TRUE

# get Inactive
t$Inactive <- FALSE
t$Inactive[t$Dmax < as.Date("2015-8-1")] <- TRUE

Ref <- t 
count(Ref,ResultTS, ResultGeo, Recent, GoodTrx, Small, Inactive)

# construct Seg
Ref$Seg <- "Improbable"
Ref$Seg[Ref$GoodTrx] <- "Probable"

Ref$Seg[Ref$Small == TRUE] <- "Small"

Ref$Seg[Ref$Recent == TRUE] <- "Recent"
Ref$Seg[Ref$Inactive == TRUE] <- "Inactive"

t.segment <- Ref %>% select(ID, ResultTS, ResultGeo, Seg)
