
T.matin <- transaction %>%
  filter(TimeSor < 12 ) %>%
  group_by(ID, Entr, Sor) %>%
  summarise(noPsg = n(), T = mean(TimeSor), SD = sd(TimeSor), Tmin = T -SD, Tmax = T + SD)

T.aprem <- transaction %>%
  filter(TimeSor < 12 ) %>%
  group_by(ID, Entr, Sor) %>%
  summarise(noPsg = n(), T = mean(TimeSor), SD = sd(TimeSor), Tmin = T -SD, Tmax = T + SD)

T <- rbind(T.matin, T.aprem)
T <- T %>% filter(noPsg > param.min.noPsg & SD < 1) # param.min.noPsg = 5
temp <- count(T, ID)
ggplot(temp) + geom_bar(aes(n), binwidth = 1)


temp <- transaction %>% group_by(ID) %>% 
  summarise(Dmin = min(Date), 
            Dmax = max(Date), 
            Ddiff = Dmax - Dmin + 1, 
            Day = n_distinct(Date), 
            Wmin = min(WOY),
            Wmax = max(WOY),
            Wdiff = Wmax - Wmin + 1,
            Week = n_distinct(WOY),
            nPsg = n(),
            Dper = Day/Ddiff,
            Wper = Week/Wdiff)

temp1 <- transaction %>% group_by(ID) %>% 
  summarise(Dmin = min(Date), 
            Dmax = max(Date), 
            Ddiff = Dmax - Dmin + 1, 
            Day = n_distinct(Date), 
            nOD = n())

temp2 <- trx.SO %>% group_by(ID) %>% 
  summarise(nTrajet = n())

##########
ID.temp <- inner_join(temp2, temp1)
##########
ggplot(ID.temp) + geom_point(aes(Dmin,Dmax))

ggplot(ID.temp) +
  geom_point(aes(ID, nOD, col = "OD")) +
  geom_point(aes(ID, nTrajet, col = "Trajet")) +
  geom_segment(aes(x=ID, xend=ID, y=nOD, yend = nTrajet))
# weild in the end

ggplot(ID.temp) + geom_bar(aes(Ddiff))
ggplot(ID.temp) + geom_bar(aes(Day))
#####

t0 <- Sys.time()
ID.OD <- trx.SO %>% group_by(ID, Entr, Sor, Sens, Date) %>%
  summarise(noPsg = n()) %>%
  ungroup() %>%
  group_by(ID, Entr, Sor) %>%
  summarise(max = max(noPsg), min = min(noPsg), mean = mean(noPsg), SD = sd(noPsg), n = sum(noPsg))
t1 <- Sys.time()

ID.OD <- ID.OD %>% ungroup() %>%
  group_by(ID) %>%
  arrange(desc(n))

ggplot(ID.OD) + geom_bar(aes(max) )

temp <- trx.SO %>% group_by(ID,Entr,Sor, Sens) %>%
  summarise(Day = n_distinct(Date))

ID.OD <- inner_join(ID.OD, temp)

ggplot(ID.OD) + geom_bar(aes(Day / n) )
temp <- ID.temp %>% select(ID, nTrajet, Day)
names(temp)[3] <- "DayTotal"
ID.OD <- inner_join(ID.OD, temp)
ggplot(ID.OD) + geom_bar(aes(Day / DayTotal) )


temp <- ID.OD %>% filter(Day/DayTotal > .8)
temp1 <- count(temp,ID)
names(temp1)[2] <- "noFreq"
temp2 <- inner_join(temp, temp1)
###

period <- data.frame(Date = seq(train.start, test.end, "day"))
period$DOW <- as.POSIXlt(period$Date)$wday
period$DOW <- as.integer(period$DOW)













##########
##########
###   Result Visualisation
##########
# viz Ind
ggplot(Ind) + 
  geom_point(aes(Model, Ind1, col = "% de trajets réels prédits")) + 
  # geom_point(aes(Model, Ind2, col = "Ind2")) + 
  geom_point(aes(Model, Ind3, col = "% de fausse alerts")) +
  facet_wrap(~ID) +
  labs(y = " Indicator") +
  theme(legend.title = element_blank())

##########
# viz: result
ggplot(result.final) + 
  geom_point(aes(DOW, Tmin, col = "Tmin")) +
  geom_point(aes(DOW, Tmax, col = "Tmax")) +
  geom_segment(aes(x=DOW, xend=DOW, y=Tmin, yend=Tmax)) +
  facet_wrap(~ID) + ggtitle("Result: Time Interval by DOW")

##########
# viz: test result
ggplot(test.final) + geom_point(aes(Date, TimeSor, col = as.factor(result))) + facet_wrap(~ID) + ggtitle("Test Result")
plyr::count(test.final, c("ID","result"))
test.final %>% group_by(ID) %>% summarise(noPsg = n(), noTP = sum(result), Per_TP = noTP/noPsg)