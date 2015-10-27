library(ggplot2)
library(gridExtra)

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

ggplot(t.Chain.summary) + 
  geom_point(aes(Date, last, col = as.factor(DOW))) + 
  geom_point(data = t.Chain.summary %>% filter(DOW == 6 | DOW == 0), aes(Date, last, size = 2)) +
  geom_path(aes(Date, last)) 


ggplot(t) + geom_tile(aes(WOY,DOW))


k <- 4^5

getDrink <- function(k){
  Item <- data.frame(Drink = k, Head = k, Bottle = k, Round = 1)
  while(Item$Drink[nrow(Item)] > 0){
    n <- nrow(Item)
    
    last <- Item[n, ]
    new <- last %>% mutate(Drink = Head %/% 4 + Bottle %/% 2,
                           Head = Drink + Head %% 4,
                           Bottle = Drink + Bottle %% 2,
                           Round = n + 1
    )
    Item <- rbind(Item, new)
  }
  return sum(Item$Drink)
}

for( i = 1:10){
  
}