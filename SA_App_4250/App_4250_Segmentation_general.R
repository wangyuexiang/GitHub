library(ggplot2)
library(gridExtra)

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
Ref$Seg <- 4

Ref$Seg[Ref$Result == TRUE] <- "6_with_result"
Ref$Seg[is.na(Ref$Small)] <- "0_out_range"

Ref$Seg[Ref$Result == FALSE &
        Ref$GoodTrx] <- "5_high_potential"

Ref$Seg[Ref$Small == TRUE] <- 2
Ref$Seg[Ref$Small == TRUE&
        Ref$Recent == TRUE] <- "3_new"

Ref$Seg[Ref$Inactive == TRUE] <- "1_inactive"

# display segmentation
ggplot(Ref) + geom_bar(aes(ord, fill = as.factor(Seg)), binwidth = 200)
ggplot(Ref) + geom_bar(aes(noPsg, fill = as.factor(Seg)), binwidth = 100)

ggplot(Ref %>% filter(noPsg < 100)) + geom_bar(aes(noPsg, fill = as.factor(Seg)), binwidth = 20)

ggplot(Ref) + geom_bar(aes(noPsg), binwidth = 100) + facet_wrap(~Seg)
ggplot(Ref  %>% filter(noPsg < 100)) + geom_bar(aes(noPsg), binwidth = 20) + facet_wrap(~Seg)

t <- Ref %>%  filter(Seg == "5_high_potential")
t <- t %>% arrange(desc(noPsg))


### link Ref temp.noGare

t <- left_join(Ref, temp.noGare)

g2 <- ggplot(t) +
  geom_density(aes(maxPerS, col = "1")) +
  geom_density(aes(maxPerS + secPerS, col = "1+2")) +
  geom_density(aes(maxPerS + secPerS + thrPerS, col = "1+2+3")) +
  geom_density(aes(maxPerS + secPerS + thrPerS + fouPerS, col = "1+2+3+4")) +
  xlab("Per") +
  facet_wrap(~Seg, ncol = 1)


g1 <- ggplot(t) +
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
### 20151028
# First Entr
# Last Sor

t <- transaction2

t0 <- t %>%
  group_by(ID, Date) %>%
  summarise(noByDay = n()) 

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

t2 <- t1 %>% filter(Tag == "First" | Tag == "Last")

t3 <- t2 %>%
  mutate(Gare = ifelse(Tag == "Last",
                       Sor,
                       ifelse(Entr == 0,
                              Sor,
                              Entr))) %>%
  select(ID, Date, DOW, Tag, Gare)

  
# first & last
t.FL <- t3 %>%
  group_by(ID, Tag, Gare) %>%
  summarise(n = n())

t.FL <- t.FL %>%
  group_by(ID, Tag) %>%
  arrange(desc(n)) %>%
  mutate(ord = row_number())

# get per
t <- Ref %>% select(ID, Day, Seg)

t.FL <- inner_join(t.FL, t)
t.FL <- t.FL %>% mutate(per = n/Day)

t.noFL <- t.FL %>%
  group_by(ID, Tag, Seg) %>%
  arrange(n) %>%
  summarise(noFL = n(),
            maxPer = max(per),
            secPer = nth(per, 2),
            thrPer = nth(per, 3),
            fouPer = nth(per, 4))

ggplot(t.noFL) +  geom_density(aes(maxPer, col = "1")) + facet_wrap(~Tag)
         
ggplot(t.noFL) +
  geom_density(aes(maxPer, col = "1")) +
  geom_density(aes(maxPer + secPer, col = "1+2")) +
  geom_density(aes(maxPer + secPer + thrPer, col = "1+2+3")) +
  geom_density(aes(maxPer + secPer + thrPer + fouPer, col = "1+2+3+4")) +
  xlab("Per")+
  facet_grid(Seg~Tag)              
            
t4 <- t3 %>%
  group_by(ID,Tag) %>%
  summarise(newDay = n_distinct(Date))

t5 <- left_join(t, t4)

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

ggplot(t.Chain.summary) + geom_point(aes(Date, last)) + geom_path(aes(Date, last))
ggplot(t.Chain.summary) + geom_point(aes(Date, last, col = as.factor(DOW ))) + geom_path(aes(Date, last)) + facet_wrap(~DOW)


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

ggplot(temp.noGare) + geom_point(aes(maxPerS,secPerS, size = noGS))

ggplot(temp.noGare) + 
  geom_segment(aes(x= maxPerE, y = secPerE + maxPerE,
                   xend=maxPerS, yend = secPerS + maxPerS, alpha = .1)) +
  geom_point(aes(maxPerS, secPerS + maxPerS, col = "Sor", alpha = .2 )) +
  geom_point(aes(maxPerE, secPerE + maxPerE, col = "Entr", alpha = .2 )) 



ggplot(temp.noGare) + geom_point(aes(maxPerS, secPerS + maxPerS))
ggplot(temp.noGare) + geom_point(aes(maxPerS, secPerS + maxPerS, size = noGS))

ggplot(temp.noGare) + geom_bar(aes( maxPerS))
ggplot(temp.noGare) + geom_bar(aes(secPerS + maxPerS))


temp.result <- count(result.final, ID) %>%
  transmute(ID, result = (n>=1))

t <- full_join(temp.result, temp.noGare)
t$result <- !is.na(t$result)
ggplot(t) + geom_point(aes(maxPerS, secPerS + maxPerS, col = as.factor(result), alpha = .2))


ggplot(temp.Gare) + geom_point(aes(ordE,ordS))
ggplot(temp.Gare) + geom_point(aes(noE,noS))




t1 <- temp.noGare %>% filter(maxPerS == secPerS) %>%
  select(ID, maxPerS)
t1.1 <- left_join(t1 %>% rename(perS = maxPerS), temp.Gare)


### useful graph

ggplot(temp.noGare) + geom_point(aes(maxPerS,maxPerS + secPerS))

ggplot(temp.noGare) + 
  geom_density(aes(maxPerE, col = "1")) +
  geom_density(aes(maxPerE + secPerE, col = "1+2")) +
  geom_density(aes(maxPerE + secPerE + thrPerE, col = "1+2+3")) +
  geom_density(aes(maxPerE + secPerE + thrPerE + fouPerE, col = "1+2+3+4")) +
  xlab("Per")

ggplot(temp.noGare) + 
  geom_density(aes(maxPerS, col = "1")) +
  geom_density(aes(maxPerS + secPerS, col = "1+2")) +
  geom_density(aes(maxPerS + secPerS + thrPerS, col = "1+2+3")) +
  geom_density(aes(maxPerS + secPerS + thrPerS + fouPerS, col = "1+2+3+4")) +
  xlab("Per")
