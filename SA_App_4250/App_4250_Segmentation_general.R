library(dplyr)
library(knitr)
library(ggplot2)
library(gridExtra)

transaction2 <- read.table("transaction2.csv", sep= ";", header = TRUE)
ID <- read.table("ID.csv", sep= ";", header = TRUE)
ID2 <- read.table("ID2.csv", sep= ";", header = TRUE)
ID.OD4 <- read.table("ID.OD4.csv", sep= ";", header = TRUE)
result.final <- read.table("result.final.csv", sep= ";", header = TRUE)


rm(temp.E,temp.S)
rm(t,t1,t2,t3,t4,t5)
rm(t.First,t.Last,t.FirstLast)
rm(t.OD)
rm(t.Chain, t.Chain.summary, t.E, t.FL, t.new,t.G,t.S,t.noFL,t.noDP,t.Tag)

###########
###########
### 20151020
# temp.Gare
# temp.noGare

temp <- transaction2

temp.ID <- temp %>%
  group_by(ID) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

temp.E <- temp %>%
  group_by(ID, Entr) %>% 
  summarise(noE = n()) %>%
  filter(Entr != 0)

temp.E <- temp.E %>%
  ungroup() %>%
  group_by(ID) %>%
  arrange(desc(noE)) %>%
  mutate(ord = row_number())

temp.S <- transaction2 %>%
  group_by(ID, Sor) %>% 
  summarise(noS = n()
  ) 

temp.S <- temp.S %>%
  ungroup() %>%
  group_by(ID) %>%
  arrange(desc(noS)) %>%
  mutate(ord = row_number())

names(temp.E) <- c("ID","Gare","noE","ordE")
temp.E <- inner_join(temp.E, temp.ID)
temp.E <- temp.E %>% mutate(perE = noE/n)

names(temp.S) <- c("ID","Gare","noS","ordS")
temp.S <- inner_join(temp.S, temp.ID)
temp.S <- temp.S %>% mutate(perS = noS/n)

temp.Gare <- full_join(temp.E,temp.S)

# analyse no of Entr
temp.noGE <- temp.E %>%
  group_by(ID) %>%
  arrange(ordE) %>%
  summarise(noGE = n(),
            maxPerE = max(perE),
            secPerE = nth(perE, 2),
            thrPerE = nth(perE, 3),
            fouPerE = nth(perE, 4)
  )

temp.noGS <- temp.S %>%
  group_by(ID) %>%
  arrange(ordS) %>%
  summarise(noGS = n(),
            maxPerS = max(perS),
            secPerS = nth(perS, 2),
            thrPerS = nth(perS, 3),
            fouPerS = nth(perS, 4)
  )

temp.noGare <- full_join(temp.noGE,temp.noGS)

ggplot(temp.noGare) + 
  geom_point(aes(maxPerS,secPerS, col = "Sor", alpha = .2)) +
  geom_point(aes(maxPerE,secPerE, col = "Entr", alpha = .2)) +
  #   geom_segment(aes(x= maxPerE, y = secPerE,
  #                    xend=maxPerS, yend = secPerS)) +
  scale_y_continuous(limits = c(0,1))

# ggplot(temp.noGare) + geom_point(aes(maxPerS,secPerS, size = noGS))

# ggplot(temp.noGare) + 
#   geom_segment(aes(x= maxPerE, y = secPerE + maxPerE,
#                    xend=maxPerS, yend = secPerS + maxPerS, alpha = .1)) +
#   geom_point(aes(maxPerS, secPerS + maxPerS, col = "Sor", alpha = .2 )) +
#   geom_point(aes(maxPerE, secPerE + maxPerE, col = "Entr", alpha = .2 )) 

ggplot(temp.noGare) + geom_point(aes(maxPerS, secPerS + maxPerS)) + ggtitle("1st & 2nd most frequented Transactions")
ggplot(temp.noGare) + geom_point(aes(maxPerS, secPerS + maxPerS, size = noGS)) + ggtitle("1st & 2nd most frequented Transactions with noPassage")

temp.result <- count(result.final, ID) %>%
  transmute(ID, result = (n>=1))
t <- full_join(temp.result,temp.noGare)
t$result <- !is.na(t$result)

temp.noGare <- inner_join(t.segment, t)
ggplot(temp.noGare) + 
  geom_point(aes(maxPerS, secPerS + maxPerS, col = as.factor(result), alpha = .2)) +
  facet_wrap(~Seg)
  ggtitle("1st & 2nd most frequented Transactions with Result")

# ggplot(temp.Gare) + geom_point(aes(ordE,ordS))
# ggplot(temp.Gare) + geom_point(aes(noE,noS))

t1 <- temp.noGare %>% filter(maxPerS == secPerS) %>%
  select(ID, maxPerS)
# t1.1 <- left_join(t1 %>% rename(perS = maxPerS), temp.Gare)

### useful graph

ggplot(temp.noGare) + geom_point(aes(maxPerS,maxPerS + secPerS))

ggplot(temp.noGare) + 
  geom_density(aes(maxPerE, col = "1")) +
  geom_density(aes(maxPerE + secPerE, col = "1+2")) +
  geom_density(aes(maxPerE + secPerE + thrPerE, col = "1+2+3")) +
  geom_density(aes(maxPerE + secPerE + thrPerE + fouPerE, col = "1+2+3+4")) +
  xlab("Per") +
  ggtitle("Distribution of most frequent Entr")

ggplot(temp.noGare) + 
  geom_density(aes(maxPerS, col = "1")) +
  geom_density(aes(maxPerS + secPerS, col = "1+2")) +
  geom_density(aes(maxPerS + secPerS + thrPerS, col = "1+2+3")) +
  geom_density(aes(maxPerS + secPerS + thrPerS + fouPerS, col = "1+2+3+4")) +
  xlab("Per") +
  ggtitle("Distribution of most frequent Sor")


###########
### 20151029
# Herfindahl index
t <- temp.E
t <- t %>% mutate(s = perE ^ 2)
t <- t %>%
  group_by(ID) %>%
  summarise(HHI_E = sum(s))

t1 <- temp.S
t1 <- t1 %>% mutate(s = perS ^ 2)
t1 <- t1 %>%
  group_by(ID) %>%
  summarise(HHI_S = sum(s))

Ref <-left_join(Ref,t)
Ref <-left_join(Ref,t1)

ggplot(Ref) + geom_bar(aes(HHI_E),binwidth = .1) + facet_wrap(~Seg)
ggplot(Ref) + geom_bar(aes(HHI_S),binwidth = .1) + facet_wrap(~Seg)

###########
### 20151028
# Segmentation

t <- ID
t <- t %>%
  arrange(Dmin, Dmax) %>%
  mutate(ord = row_number())

# get Small/Inactive

t1 <- ID2 %>% select(ID, Small,Inactive)
t <- left_join(t, t1)

ggplot(t) + geom_segment(aes(x=Dmin, xend = Dmax, y = ord, yend = ord, col = as.factor(Inactive))) + facet_wrap(~Small)

# get Result
t1 <- result.final %>% select(ID) %>% distinct(ID)
t1$Result <- TRUE
t <- left_join(t,t1)
t$Result[is.na(t$Result)] <- FALSE

# get Recent
t$Recent <- FALSE
t$Recent[t$Dmin > as.Date("2015-8-15")] <- TRUE

# get GoodTrx
t1 <- ID.OD4 %>% select(ID) %>% distinct(ID)
t1$GoodTrx <- TRUE
t <- left_join(t,t1)
t$GoodTrx[is.na(t$GoodTrx)] <- FALSE

Ref <- t
count(Ref,Result, Recent, GoodTrx, Small, Inactive)

# construct Seg
Ref$Seg <- "4_no_important_trajet"

Ref$Seg[Ref$Result == TRUE] <- "6_with_result"
Ref$Seg[is.na(Ref$Small)] <- "0_out_range"

Ref$Seg[Ref$Result == FALSE &
        Ref$GoodTrx] <- "5_high_potential"

Ref$Seg[Ref$Small == TRUE] <- "2_Small_but_not_Recent"
Ref$Seg[Ref$Small == TRUE&
        Ref$Recent == TRUE] <- "3_new"

Ref$Seg[Ref$Inactive == TRUE] <- "1_inactive"

t.segment <- Ref %>% select(ID, Seg)

# display segmentation
ggplot(Ref) + geom_bar(aes(ord, fill = as.factor(Seg)), binwidth = 200)
ggplot(Ref) + geom_bar(aes(noPsg, fill = as.factor(Seg)), binwidth = 100)

ggplot(Ref %>% filter(noPsg < 100)) + geom_bar(aes(noPsg, fill = as.factor(Seg)), binwidth = 20)

# ggplot(Ref) + geom_bar(aes(noPsg), binwidth = 100) + facet_wrap(~Seg)
# ggplot(Ref  %>% filter(noPsg < 100)) + geom_bar(aes(noPsg), binwidth = 20) + facet_wrap(~Seg)

### link Ref temp.noGare
t <- left_join(Ref, temp.noGare)

g2 <- ggplot(temp.noGare) +
  geom_density(aes(maxPerS, col = "1")) +
  geom_density(aes(maxPerS + secPerS, col = "1+2")) +
  geom_density(aes(maxPerS + secPerS + thrPerS, col = "1+2+3")) +
  geom_density(aes(maxPerS + secPerS + thrPerS + fouPerS, col = "1+2+3+4")) +
  xlab("Per") +
  facet_wrap(~Seg, ncol = 1)

g1 <- ggplot(temp.noGare) +
  geom_density(aes(maxPerE, col = "1")) +
  geom_density(aes(maxPerE + secPerE, col = "1+2")) +
  geom_density(aes(maxPerE + secPerE + thrPerE, col = "1+2+3")) +
  geom_density(aes(maxPerE + secPerE + thrPerE + fouPerE, col = "1+2+3+4")) +
  xlab("Per")+
  facet_wrap(~Seg, ncol = 1)

grid.arrange(g1,g2, ncol = 2)

t %>% filter(Result == FALSE & maxPerS > .5)

###########
###########
### 20151027
# geographic model

### reference: gares
gares <- read.table("garesLngLat.csv", sep =",", header = TRUE) %>% tbl_df
ggplot(gares) + geom_point(aes(Lng,Lat, col = as.factor(Societe)))

### find people who have more than 4 transactions daily
temp <- transaction2

temp.ID <- temp %>%
  group_by(ID) %>%
  summarise(n = n(),
            Dmin = min(Date),
            Dmax = max(Date),
            Ddiff = Dmax - Dmin + 1,
            Day = n_distinct(Date),
            noPperD = n / Day) %>%
  arrange(desc(n))

### find people who have more than 4 transactions daily
t <- transaction2 %>% filter(ID == 1)

t1 <- gares %>% transmute(Entr = Cde, Elng= Lng, Elat = Lat)
t2 <- left_join(t, t1)
t1 <- gares %>% transmute(Sor = Cde, Slng= Lng, Slat = Lat)
t <- left_join(t2, t1)

ggplot(t) + 
  geom_point(aes(Elng, Elat, col ="Entr")) +
  geom_point(aes(Slng, Slat, col ="Sor")) +
  geom_segment(aes(x=Elng,xend=Slng,y=Elat,yend=Slat)) +
  geom_point(data= gares, aes(Lng,Lat, col = as.factor(Societe), alpha = .2))

t.OD <- t %>%
  group_by(Entr, Sor) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  mutate(ord = row_number())

t1 <- t.OD %>% select(-n)
t <- inner_join(t,t1)

ggplot(t) + geom_point(aes(Date, TimeSor, col = as.factor(ord))) + theme(legend.position = "none")
ggplot(t %>% filter(ord < 8)) + geom_point(aes(Date, TimeSor, col = as.factor(ord)))
ggplot(t %>% filter(ord < 9)) + geom_point(aes(Date, TimeSor, col = as.factor(ord))) + facet_grid(Entr~Sor)

t3 <- t %>% filter(ord <3)
ggplot(t3) + geom_bar(aes(DOW, fill = as.factor(ord)), binwidth = 1, position = "dodge") + theme(legend.position = "none")
ggplot(t3) + geom_bar(aes(TimeSor, fill = as.factor(ord)), binwidth = 1, position = "dodge") + theme(legend.position = "none")

ggplot(t3) + geom_tile(aes(WOY,DOW, fill = as.factor(ord))) + facet_wrap(~ord, ncol = 1) + theme(legend.position = "none")

### gares
t.G <- temp.Gare %>% filter(ID == 1)
t1 <- gares %>% transmute(Gare = Cde, Lng, Lat)
t.G <- left_join(t.G, t1)

ggplot(t.G) + 
  geom_point(aes(Lng,Lat, size = perE)) +
  geom_point(data= gares, aes(Lng,Lat, col = as.factor(Societe), alpha = .2))

### chain
t.Chain <- t %>%
  group_by(Date, DOW) %>%
  arrange(TimeSor) %>%
  mutate(Dord = row_number())

t.Chain.summary <- t.Chain %>%
  summarise(last = n())

ggplot(t.Chain.summary) + geom_point(aes(Date, last)) + geom_path(aes(Date, last)) + ggtitle("ID = 1")
ggplot(t.Chain.summary) + geom_point(aes(Date, last, col = as.factor(DOW ))) + geom_path(aes(Date, last)) + facet_wrap(~DOW)+ ggtitle("ID = 1")

###########
###########
### 20151028
# First Entr
# Last Sor
t <- transaction2

t0 <- t %>%
  group_by(ID, Date) %>%
  summarise(noByDay = n()) 

ggplot(t0) + geom_bar(aes(noByDay), binwidth = 1)
ggplot(t0) + geom_bar(aes(x=noByDay, y=(..count..)/sum(..count..)), binwidth = 1)

t.DailyPassage <- count(t0,noByDay) %>%
  tbl_df %>%
  mutate(per = n / nrow(t0))
# 31% : 1 trx during the day
# 38% : 2 trx during the 
t.DailyPassage <- t0 %>% mutate(no = noByDay)
t.DailyPassage$no[t.DailyPassage$noByDay > 5] <- "DailyPassage > 5"

ggplot(t.DailyPassage) + 
  geom_bar(aes(x = noByDay,y= (..count..)/sum(..count..),
                                      fill=as.factor(no)),binwidth = 32) +
  ylab("Percentage") + ggtitle("Distribution of number of passages by Day")

t.DailyPassage <- inner_join(t.DailyPassage, t.segment)

ggplot(t.DailyPassage) + 
  geom_bar(aes(x = noByDay,y= (..count..)/sum(..count..),
               fill=as.factor(no)),binwidth = 1) +
  ylab("Percentage") + ggtitle("Distribution of number of passages by Day") +
  facet_wrap(~Seg)


ggplot(t.DailyPassage) + 
  geom_bar(aes(x = noByDay,y= (..count..)/sum(..count..),
                                      fill=as.factor(no)),binwidth = 32) +
  facet_wrap(~Seg)

t.noDP <- t.DailyPassage %>%
  group_by(ID, no, Seg) %>%
  summarise(noDay = n() )

temp <- t.noDP %>% arrange(desc(noDay)) %>% slice(1:1)

ggplot(temp) + geom_bar(aes(no, fill = as.factor(Seg)))
ggplot(temp) + geom_bar(aes(Seg, fill = as.factor(no)))

t1 <- t %>%
  group_by(ID, Date) %>%
  arrange(TimeSor) %>%
  mutate(ord = row_number())

t2 <- inner_join(t0,t1)
# it's possible that a person has only 1 trx one day
t2 <- t2 %>% mutate(Tag = ifelse(noByDay == 1,
                                 "FL",
                                 ifelse(ord == 1,
                                        "First",
                                        ifelse(ord == noByDay,
                                               "Last",
                                               "irrelevant")
                                        )
                                 )
                    )

t.Tag <- t2

count(t.Tag,Tag)
#          Tag     n
# 1         FL 17678
# 2      First 39048
# 3       Last 39048
# 4 irrelevant 41613

count(Ref, (HHI_E > .25| HHI_S > .25), Seg)

# # first & last
# t.FL <- t3 %>%
#   group_by(ID, Tag, Gare) %>%
#   summarise(n = n())
# 
# t.FL <- t.FL %>%
#   group_by(ID, Tag) %>%
#   arrange(desc(n)) %>%
#   mutate(ord = row_number())

# # get per
# t <- Ref %>% select(ID, Day, Seg)
# 
# t.FL <- inner_join(t.FL, t)
# t.FL <- t.FL %>% mutate(per = n/Day)
# 
# t.noFL <- t.FL %>%
#   group_by(ID, Tag, Seg) %>%
#   arrange(n) %>%
#   summarise(noFL = n(),
#             maxPer = max(per),
#             secPer = nth(per, 2),
#             thrPer = nth(per, 3),
#             fouPer = nth(per, 4))
# 
# ggplot(t.noFL) +  geom_density(aes(maxPer, col = "1")) + facet_wrap(~Tag)
#          
# ggplot(t.noFL) +
#   geom_density(aes(maxPer, col = "1")) +
#   geom_density(aes(maxPer + secPer, col = "1+2")) +
#   geom_density(aes(maxPer + secPer + thrPer, col = "1+2+3")) +
#   geom_density(aes(maxPer + secPer + thrPer + fouPer, col = "1+2+3+4")) +
#   xlab("Per")+
#   facet_grid(Seg~Tag)              
#             
# t4 <- t3 %>%
#   group_by(ID,Tag) %>%
#   summarise(newDay = n_distinct(Date))
# 
# t5 <- left_join(t, t4)

t.FL <- t.Tag %>% filter(Tag == "FL")

t.First <- t.Tag %>% 
  filter(Tag == "FL" | Tag == "First") %>%
  mutate(Tag = "First",
         Gare = ifelse(Entr == 0,
                       Sor,
                       Entr)
  )

t.Last <- t.Tag %>% 
  filter(Tag == "FL" | Tag == "Last") %>%
  mutate(Tag = "Last",
         Gare = Sor
  )

t.FirstLast <- rbind(t.First, t.Last) %>%
  select(ID, Date, DOW, WOY, Sens, JF, ord, Tag, Gare)

t <- t.FirstLast %>% group_by(ID, Tag) %>% summarise(total = n())
t1 <- t.FirstLast %>% 
  group_by(ID, Tag, Gare) %>%
  summarise(n = n()) %>% inner_join(t)

t1 <- t1 %>% mutate(per = n/total,
                  s = per * per)

t1 <- t1 %>% inner_join(t.segment)

t2 <- t1 %>%
  group_by(ID, Tag) %>%
  summarise(HHI = sum(s))

t2 <- t2 %>% inner_join(t.segment)
ggplot(t2) + geom_density(aes(HHI, col = Tag))
ggplot(t2) + geom_density(aes(HHI, col = Tag)) + facet_wrap(~Seg)

##########
### 20151030
### Day Model
t <- transaction2 %>% group_by(ID) %>% summarise(Day = n_distinct(Date))
t1 <- t.DailyPassage %>% group_by(ID) %>% summarise(DayDP = n_distinct(Date))
t2 <- inner_join(t,t1)

t2 <- inner_join(t,t.noDP)
t2 <- t2 %>% 
  mutate(per = noDay / Day,
         s = per^2)
t3 <- t2 %>% group_by(ID) %>% summarise(HHI = sum(s))
ggplot(t3) + geom_density(aes(HHI))
t4 <- left_join(t3, t.segment)
ggplot(t4) + geom_density(aes(HHI)) + facet_wrap(~Seg)

##########
### 20151030
### HP - high_potential
# HP <- t2 %>% filter(Seg == "5_high_potential")
# 
# ID.HP <- Ref %>% filter(Seg == "5_high_potential") 
# 
# ggplot(ID.HP) + geom_point(aes(Ddiff, Day))
# ggplot(ID.HP) + geom_point(aes(Dmin, Dmax))
# 
# ggplot(ID.HP) + geom_point(aes(Day, noPsg))t
# 
# t <- count(HP, ID) %>% rename(no = n)
# t <- count(HP, ID) %>% filter(n == 2)


t <- transaction2 %>% 
  group_by(ID) %>% 
  summarise(ActiveDay = n_distinct(Date)) %>%
  inner_join(t.segment)

### OD
t1 <- transaction2 %>%
  group_by(ID, Entr, Sor) %>%
  summarise(noPsg = n(), Day = n_distinct(Date) ) %>%
  inner_join(t)
### find OD that one does more than 50% of his active day
t2 <- t1 %>% filter(Day * 2 > ActiveDay )
t3 <- count(t2, ID) %>% rename(noOD = n)
### add noOD
temp.ID <- left_join(t,t3)

### Entr
t1 <- transaction2 %>%
  group_by(ID, Entr) %>%
  summarise(noPsg = n(), Day = n_distinct(Date) ) %>%
  inner_join(t)
t2 <- t1 %>% filter(Day * 2 > ActiveDay )
t3 <- count(t2, ID) %>% rename(noEntr = n)
### add noEntr
temp.ID <- temp.ID %>% left_join(t3)

### Sor
t1 <- transaction2 %>%
  group_by(ID, Sor) %>%
  summarise(noPsg = n(), Day = n_distinct(Date) ) %>%
  inner_join(t)
t2 <- t1 %>% filter(Day * 2 > ActiveDay )
t3 <- count(t2, ID) %>% rename(noSor = n)
### add noSor
temp.ID <- temp.ID %>% left_join(t3)

### First & Last
t1 <- t.FirstLast %>%
  group_by(ID, Tag, Gare) %>%
  summarise(noPsg = n(), Day = n_distinct(Date) ) %>%
  inner_join(t)
t2 <- t1 %>% filter(Day * 2 > ActiveDay )
t3 <- count(t2, ID, Tag) %>% rename(no = n)

t4 <- t3 %>% filter(Tag == "First") %>% rename(noFirst = no) %>% select(ID, noFirst)
t5 <- t3 %>% filter(Tag == "Last") %>% rename(noLast = no) %>% select(ID, noLast)
### add noFirst & noLast
temp.ID <- temp.ID %>% left_join(t4) %>% left_join(t5)

t <- temp.ID %>% filter(Seg == "5_high_potential")


t <- temp.ID %>% filter(Seg != "6_with_result")
ggplot(t) + geom_bar(aes(ActiveDay),binwidth = 10)

t1 <- temp.ID %>% filter(! (is.na(noOD) &
                      is.na(noEntr) &
                      is.na(noSor) &
                      is.na(noFirst) &
                      is.na(noLast)
                      )
                   )
count(t1, Seg)

ggplot(t1) + geom_density(aes(ActiveDay))
ggplot(t1) + geom_density(aes(ActiveDay)) + facet_wrap(~Seg)
ggplot(t1 %>% filter(Seg != "6_with_result")) + geom_density(aes(ActiveDay)) + facet_wrap(~Seg)


ggplot(t1) + 
  geom_density(aes(noOD, col = "OD")) +
  geom_density(aes(noEntr, col = "Entr")) +
  geom_density(aes(noSor, col = "Sor")) +
  geom_density(aes(noFirst, col = "First")) +
  geom_density(aes(noLast, col = "Last")) +
  xlim(c(0,12))

ggplot(t1) + 
  geom_density(aes(noOD, col = "OD")) +
  geom_density(aes(noEntr, col = "Entr")) +
  geom_density(aes(noSor, col = "Sor")) +
  geom_density(aes(noFirst, col = "First")) +
  geom_density(aes(noLast, col = "Last")) +
  xlim(c(0,12)) +
  facet_wrap(~Seg)

t2 <- t1 %>% 
  group_by(Seg) %>%
  arrange(desc(ActiveDay, noOD))

t3 <- t1 %>% filter(noOD > 4 |
                    noEntr >4 |
                    noSor > 4 |
                    noFirst > 4 |
                    noLast > 4)


temp <- count(t1, Seg) %>% rename(nPotential = n)

temp <- t1 %>% transmute(ID, Potential = TRUE)
temp.ID <- left_join(temp.ID, temp)
temp.ID$Potential[is.na(temp.ID$Potential)] <- FALSE
temp.ID <- temp.ID %>% mutate(Potential = as.factor(Potential))

ggplot(temp.ID) + geom_bar(aes(Seg, fill = Potential))

count(temp.ID, Seg, Potential)

### noFirst == noLast
t <- temp.ID %>% filter(noFirst == noLast) %>% select(ID)

t1 <- t.FirstLast %>% 
  group_by(ID,Tag, Gare) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  slice(1)

t2 <- t1 %>% filter(Tag == "First") %>% ungroup %>% rename(First = Gare, noFirst = n) %>% select(ID, First, noFirst)
t3 <- t1 %>% filter(Tag == "Last") %>% ungroup %>% rename(Last = Gare, noLast = n) %>% select(ID, Last, noLast)
t4 <- inner_join(t2,t3)

t5 <- inner_join(t,t4)


## testing for GetCharacters and GetNoEntr

t <- transaction2
t1 <- GetCharacters(t)

t <- t %>% mutate(Voie = ifelse(Entr == 0, Voie, 0))
t2 <- t %>% left_join(sens)
t2 <- t2 %>% mutate(SensEntr = ifelse(is.na(SensEntr), 0, SensEntr),
                      SensSor = ifelse(is.na(SensSor), 0, SensSor))

t.E <- GetNoEntr(t2)
t.S <- GetNoSor(t2)

t3 <- TagFirstLast(t2)
t4 <- GetNoFirstLast(t3)


temp <- GetTimeFirstLast(t3)

temp1 <- temp %>% filter(SD < 1.5 & noPsg > 5)
temp2 <- count(temp1, ID)
temp3 <- inner_join(temp2,t.segment)

ggplot(temp) + geom_bar(aes(SD),binwidth = 1) + facet_grid(DOW~Tag)
ggplot(temp) + geom_bar(aes(Tmoy),binwidth = 1) + facet_grid(DOW~Tag)

ggplot(temp) + geom_density(aes(Tmoy, col = as.factor(Tag) ),binwidth = 1) + facet_wrap(~DOW)


t <- temp.ID %>% filter( !(is.na(noEntr) & is.na(noSor)))

t <- temp.ID %>% arrange(as.numeric(ID)) %>% filter(Seg == "4_no_important_trajet" & Potential == TRUE)
Dashboard(transaction2 %>% filter(ID == 980))


### get ID with a First, find the distance
t <- temp.ID %>% filter( !is.na(noFirst) )
t <- t %>% select(ID,Seg)
t1 <- transaction2 %>% inner_join(t)

t1 <- t1 %>% 
  mutate(Voie = ifelse(Entr == 0, Voie, 0)) %>% 
  left_join(sens) %>% 
  mutate(SensEntr = ifelse(is.na(SensEntr), 0, SensEntr),
                    SensSor = ifelse(is.na(SensSor), 0, SensSor))

t2 <- TagFirstLast(t1)
t3 <- GetNoFirstLast(t2, .4) %>% filter(Tag == "First")

### distance
t <- transaction2
# t1 <- GetCharacters(t)

t <- t %>% 
  mutate(Voie = ifelse(Entr == 0, Voie, 0)) %>%
  left_join(sens) %>%
  mutate(SensEntr = ifelse(is.na(SensEntr), 0, SensEntr),
         SensSor = ifelse(is.na(SensSor), 0, SensSor))

t <- TagFirstLast(t)

t1 <- gares %>% transmute(Entr = Cde, Elng = Lng, Elat = Lat)
t <- left_join(t, t1)
t1 <- gares %>% transmute(Sor = Cde, Slng = Lng, Slat = Lat)
t <- left_join(t, t1)

# ### 20151103
# t1 <- t %>% filter(Tag == 'First' & !is.na(Slng))
# t2 <- t1 %>% filter(is.na(Elng)) %>% ungroup %>% transmute(ID, Date, Lng = Slng, Lat = Slat)
# t3 <- t1 %>% filter(!is.na(Elng)) %>% ungroup %>% transmute(ID, Date, Lng = Elng, Lat = Elat)
# 
# t2 <- rbind(t2,t3)
# t3 <- left_join(t, t2) %>% select(-c(Badge, EVA, Voie, Sens, SensEntr, SensSor))
# 
# t4 <- t3 %>% 
#   mutate(
#     E = sqrt((Elng - Lng)^2 + (Elat - Lat)^2),
#     S = sqrt((Slng - Lng)^2 + (Slat - Lat)^2),
#     dist = (E + S + abs(E - S))/2
#     )
# 
# t5 <- t4 %>%
#   group_by(ID,Date) %>%
#   filter(!is.na(dist)) %>%
#   summarise(dist = max(dist))
# 
# t6 <- t5 %>% 
#   group_by(ID) %>%
#   summarise(
#     dMax = max(dist),
#     dMin = min(dist),
#     dMoy = mean(dist),
#     dSD = sd(dist)
#     )
# 
# t6 <- t6 %>% inner_join(t.segment)

# u: up
# d: down
# l: left
# r: right


t <- transaction2
t1 <- gares %>% transmute(Entr = Cde, Elng = Lng, Elat = Lat)
t <- left_join(t, t1)
t1 <- gares %>% transmute(Sor = Cde, Slng = Lng, Slat = Lat)
t <- left_join(t, t1)

delta.Lat = .1
delta.Lng = .1

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

tRect <- rbind(t1,t2,t3)
temp <- tRect %>% filter(ID == 1)
temp
ggplot(temp) +geom_rect(aes(xmin=l, xmax=r, ymin=d, ymax=u, alpha = .1))
ggplot(temp) +geom_rect(aes(xmin=l, xmax=r, ymin=d, ymax=u, alpha = .1)) + facet_wrap(~DOW)

ggplot(temp) +
  geom_rect(aes(xmin=l, xmax=r, ymin=d, ymax=u, alpha = .1)) +
  theme(
    scale_y_continuous( breaks = seq(40, 50, 1))
    # scale_y_continuous(minor_breaks = seq(42 , 49, 0.5), breaks = seq(42, 49, 1))
    )

t4 <- t3 %>%
  group_by(ID, u,d,r,l) %>%
  summarise(n = n()) %>%
  ungroup %>%
  group_by(ID) %>%
  arrange(desc(n))

temp <- t4 %>% filter(ID == 1)
temp
ggplot(t4) + geom_tile(aes(x=l, xend=r, y=d, yend=u, alpha = n))
ggplot(temp) + geom_segment(aes(x=l, xend=r, y=d, yend=u))
ggplot(temp) + geom_rect(aes(xmin=l, xmax=r, ymin=d, ymax=u, alpha =n))

ggplot(t3 %>% filter(ID == 1)) + geom_rect(aes(xmin=l, xmax=r, ymin=d, ymax=u, alpha =.1)) + facet_wrap(~DOW)

temp1 <- t4 %>% filter(as.numeric(ID) < 20)
ggplot(temp1) + geom_rect(aes(xmin=l, xmax=r, ymin=d, ymax=u, alpha =n)) + facet_wrap(~ID)


t5 <- count(t4, ID) %>% left_join(t.segment)

### add grid
t.lat <- data.frame(
  d = seq(42  , 48.9,.5),
  u = seq(42.1, 49  ,.5),
  Row = seq(1, 14)
)

t.lng <- data.frame(
  l = seq(-2  , 7.9, .5),
  r = seq(-1.9, 8  , .5),
  Col = seq(1,20)
)

t.lat$temp <- 1
t.lng$temp <- 1
t.grid <- inner_join(t.lat, t.lng) %>% tbl_df
t.grid$temp <- NULL

temp <- sens %>% select(Entr, Sor) %>% distinct %>% tbl_df
t1 <- gares %>% transmute(Entr = Cde, Elng = Lng, Elat = Lat)
temp <- left_join(temp, t1)
t1 <- gares %>% transmute(Sor = Cde, Slng = Lng, Slat = Lat)
temp <- left_join(temp, t1)


t1 <- temp %>% 
  filter(is.na(Elat) & !is.na(Slat)) %>%
  mutate(u=Slat + delta.Lat ,
       d=Slat - delta.Lat,
       r=Slng + delta.Lng, 
       l=Slng - delta.Lng)

t2 <- temp %>% 
  filter(!is.na(Elat) & is.na(Slat)) %>%
  mutate(u=Elat + delta.Lat,
       d=Elat - delta.Lat,
       r=Elng + delta.Lng,
       l=Elng - delta.Lng)

t3 <- temp %>%
  filter(!is.na(Elat) & !is.na(Slat)) %>%
  mutate(
  u = ( Elat + Slat + abs(Elat - Slat) )/2,
  d = ( Elat + Slat - abs(Elat - Slat) )/2,
  r = ( Elng + Slng + abs(Elng - Slng) )/2,
  l = ( Elng + Slng - abs(Elng - Slng) )/2
  )

temp1 <- rbind(t1,t2,t3) %>% rename(U=u, D=d, L=l, R=r)
temp1 <- temp1 %>%
  mutate(t = 1) %>%
  select(- c(Elng,Elat,Slng,Slat))

t.grid$t <- 1

t.start <- Sys.time()
Grid <- data.frame(Entr = 0, Sor = 0, Row = 0, Col = 0)

for(i in 1:nrow(temp1)){
 t1 <- temp1 %>% slice(i)
 t2 <- inner_join(t.grid, t1) %>%
   filter(u > D,
          d < U,
          r > L,
          l < R) %>%
   select(Entr,Sor,Row,Col)
 
 Grid <- rbind(Grid,t2)   
}

t.end <- Sys.time()

Grid <- Grid %>% tbl_df %>% slice(-1)

t1 <- transaction2 %>% filter(ID == 1)
t2 <- inner_join(t1,Grid)
ggplot(t2) + geom_tile(aes(Col, Row),alpha = 0.1)

t2 <- inner_join(transaction2,Grid)
ggplot(t2 %>% filter(as.numeric(ID) < 10)) + geom_tile(aes(Col, Row),alpha = 0.1) + facet_wrap(~ID)


t3 <- t2 %>%
  filter(TimeSor < 12 ) %>%
  group_by(ID, Row, Col) %>%
  summarise(noPsg = n(), T = mean(TimeSor), SD = sd(TimeSor), Tmin = T -SD, Tmax = T + SD)


t3 <- t2 %>%
  filter(TimeSor < 12 ) %>%
  group_by(ID, Row, Col) %>%
  summarise(noPsg = n(), T = mean(TimeSor), SD = sd(TimeSor), Tmin = T -SD, Tmax = T + SD)

t4 <- t2 %>%
  filter(TimeSor < 12 ) %>%
  group_by(ID, Row, Col) %>%
  summarise(noPsg = n(), T = mean(TimeSor), SD = sd(TimeSor), Tmin = T -SD, Tmax = T + SD)





