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



t <- Ref %>%
  filter(Inactive == FALSE,
         Small == FALSE,
         GoodTrx == FALSE)

t <- t %>% arrange(desc(noPsg))

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

