##########
##########
# Zone_TimeWindow
##########

##########
### hour heatmap
##########
trx.time <- transaction2 %>%
  mutate(H = round(TimeSor, digits = 0),
         H_2 = H - H %% 2
  ) %>%
  inner_join(JF)

t <- trx.time %>% filter(as.numeric(ID) < 9)
t1 <- t %>% group_by(ID,DOW,H) %>% summarise(freq = n())
t2 <- t %>% group_by(ID,DOW,H_2) %>% summarise(freq = n())
ggplot(t1) + geom_tile(aes(DOW,H, fill = freq)) + facet_wrap(~ID)
ggplot(t2) + geom_tile(aes(DOW,H_2, fill = freq)) + facet_wrap(~ID)
ggplot(t2 %>% filter(freq > 15)) + geom_tile(aes(DOW,H_2, fill = freq))


t3 <-  t %>% group_by(ID,DOW,H) %>% summarise(freq = n())
ggplot(t) + geom_tile(aes(Date,H)) + facet_wrap(~ID)

##########
### get important TimeWindow
##########
t1 <- trx.time %>%
  group_by(ID, DOW) %>%
  summarise(ActiveDay = n_distinct(Date))

t2 <- trx.time %>%
  group_by(ID, DOW, H) %>%
  summarise(Day = n_distinct(Date))

t2 <- trx.time %>%
  group_by(ID, DOW, H_2) %>%
  summarise(Day = n_distinct(Date))

t.TimeWindow <- inner_join(t1,t2) %>% 
  mutate(Per = Day / ActiveDay,
         Per_floor = round(Per, 1))

t <- t.TimeWindow %>% filter(as.numeric(ID) < 9)
ggplot(t) + geom_tile(aes(DOW,H, fill = as.factor(Per_floor))) + facet_wrap(~ID)
ggplot(t %>% filter(Per_floor > .4)) + geom_tile(aes(DOW,H, fill = as.factor(Per_floor)))+ facet_wrap(~ID)


ggplot(t) + geom_tile(aes(DOW,H_2, fill = as.factor(Per_floor))) + facet_wrap(~ID)
ggplot(t %>% filter(Per_floor > .4)) + geom_tile(aes(DOW,H_2, fill = as.factor(Per_floor)))+ facet_wrap(~ID)

ggplot(t %>% filter(ActiveDay > 4)) + geom_tile(aes(DOW,H_2, fill = as.factor(Per_floor)))+ facet_wrap(~ID)
ggplot(t %>% filter(ActiveDay > 4 & Per_floor > .4)) + geom_tile(aes(DOW,H_2, fill = as.factor(Per_floor)))+ facet_wrap(~ID)

##########
### Connect Grid
##########
t <- t.Active %>% group_by(ID)

# Get 4 points
#   NW  NE
#   SW  SE
t1 <- t %>% arrange(Row, Col)             %>% select(Row, Col) %>% slice(1) %>% rename(R_NW = Row, C_NW = Col)
t2 <- t %>% arrange(Row, desc(Col))       %>% select(Row, Col) %>% slice(1) %>% rename(R_NE = Row, C_NE = Col)
t3 <- t %>% arrange(desc(Row), Col)       %>% select(Row, Col) %>% slice(1) %>% rename(R_SW = Row, C_SW = Col)
t4 <- t %>% arrange(desc(Row), desc(Col)) %>% select(Row, Col) %>% slice(1) %>% rename(R_SE = Row, C_SE = Col)
  
temp <- inner_join(t1, t2) %>% inner_join(t3) %>% inner_join(t4)

# Compare to get One_Zone
#   C_NW & C_SW
#   C_NE & C_SE
temp <- temp %>% mutate(Left = (C_NW == C_SW),
                        Right = (C_NE == C_SE),
                        One_Zone = Left && Right)

# Viz
t1 <- temp %>% filter(Left != Right) %>% select(ID) %>% ungroup %>% slice(c(1:9))
t1 <- temp %>% filter(Left == Right, Left == TRUE) %>% select(ID) %>% ungroup %>% slice(c(1:9))
k <- t1$ID
ggplot(t.Active %>% filter(ID %in% k)) + 
  geom_tile(aes(l,d, alpha = Per)) + 
  # xlim(c(-2,8)) + ylim(c(42,49)) + 
  facet_wrap(~ID) +
  geom_point(data= gares, aes(Lng, Lat, col = as.factor(Societe)))

# Integrate result to ID.segment
t2 <- t.Active %>% count(ID, ActiveDay) %>% right_join(t.segment)
ID.segment <- temp %>%
  select(ID, One_Zone) %>%
  right_join(t2)

##########
### Get Hourheatmap for One_Zone
##########
# Get ID with only One_Zone
t <- ID.segment %>% filter(One_Zone == TRUE) %>% select(ID)
# Get grid for all ID with One_Zone
# t1 <- inner_join(t,t.Active) %>% select(ID,OD,Per) %>% mutate(PerR = round(Per,1))
t1 <- inner_join(t,t.Active) %>% select(ID,OD) %>% ungroup %>% distinct
# Get all trx for ID with One_Zone
t2 <- inner_join(t1,trx)

### from Row_Col to Entr_Sor
temp1 <- trx %>% select(-c(Row:OD)) %>% ungroup %>% distinct
temp2 <- t2 %>% select(-c(OD,Row,Col)) %>% ungroup %>% distinct

# Know how many Psg of these ID are in the One_Zone
temp <- GetCharacters(temp1)
t4 <- count(temp2, ID) %>% rename(nTrx_in_Zone = n)
temp <- left_join(temp, t4) %>%
  mutate(Per_in_Zone = nTrx_in_Zone / noPsg)

ggplot(temp) + geom_density(aes(Per_in_Zone))

### display
temp.time <- temp2 %>%
  mutate(H = round(TimeSor, digits = 0),
         H_2 = H - H %% 2
  ) 

t5 <-  temp.time %>% count(ID) %>% filter( n > 80) %>%
  select(ID) %>%
  left_join(ID.segment) %>%
  filter(ResultTS == FALSE & ResultGeo == TRUE) %>%
  slice(1:9)

t <- temp.time %>% inner_join(t5)
t1 <- t %>% group_by(ID,DOW,H) %>% summarise(freq = n())
t2 <- t %>% group_by(ID,DOW,H_2) %>% summarise(freq = n())
ggplot(t1) + geom_tile(aes(DOW,H, fill = freq)) + facet_wrap(~ID)
ggplot(t2) + geom_tile(aes(DOW,H_2, fill = freq)) + facet_wrap(~ID)


t3 <- t.Active %>% inner_join(t5)
ggplot(t3) + geom_tile(aes(l,d, alpha = Per)) + xlim(c(-2,8)) + ylim(c(42,49)) + facet_wrap(~ID) +
  geom_point(data= gares, aes(Lng, Lat, col = as.factor(Societe)))

ggplot(t3) + geom_tile(aes(l,d, alpha = Per)) + xlim(c(-2,8)) + ylim(c(42,44)) + facet_wrap(~ID) +
  geom_point(data= gares, aes(Lng, Lat, col = as.factor(Societe)))

t4 <- ID.segment %>% inner_join(t5)









ggplot(t2 %>% filter(freq > 15)) + geom_tile(aes(DOW,H_2, fill = freq))
t3 <-  t %>% group_by(ID,DOW,H) %>% summarise(freq = n())
ggplot(t) + geom_tile(aes(Date,H)) + facet_wrap(~ID)
