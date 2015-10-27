library(ggplot2)
library(gridExtra)

###########
### 20151023

temp.result <- result.final
temp.result$HP <- NA # heure pointe
temp.result$HP[temp.result$Tmin >= 6 & temp.result$Tmax <= 10] <- "10-Matin"
temp.result$HP[temp.result$Tmin >= 10 & temp.result$Tmax <= 14] <- "14-Midi"
temp.result$HP[temp.result$Tmin >= 14 & temp.result$Tmax <= 16] <- "16-Aprem"
temp.result$HP[temp.result$Tmin >= 16 & temp.result$Tmax <= 20] <- "20-Soir"
temp.result$HP[temp.result$Tmin >= 20] <- "24-Nuit"

temp.result <- temp.result %>% mutate(TDiff = Tmax - Tmin)
temp.result$Diff <- NA # Tmax - Tmin
temp.result$Diff[temp.result$TDiff <= 1] <- "1 SuperRegular"
temp.result$Diff[temp.result$TDiff > 1 & temp.result$TDiff <= 2] <- "2 Regular"
temp.result$Diff[temp.result$TDiff > 2 & temp.result$TDiff <= 4] <- "4 So-So"
temp.result$Diff[temp.result$TDiff > 4] <- "6 Nul"


temp.result$WE[temp.result$WE == TRUE] <- "Weekend"
temp.result$WE[temp.result$WE == FALSE] <- "Weedday"




ggplot(temp.result) + geom_point(aes(Tmin,Tmax, col = as.factor(Diff)))+
  geom_abline(slope = 1, intercept = 2) +
  geom_abline(slope = 1, intercept = 4) +
  geom_abline(slope = 1, intercept = 6) 

g1 <- ggplot(temp.result) + 
  geom_density(aes(Tmax, col = "Tmax")) +
  geom_density(aes(Tmin, col = "Tmin")) + facet_wrap(~WE)

g2 <- ggplot(temp.result) + geom_point(aes(Tmin,Tmax, col = as.factor(HP)))+  geom_abline(slope = 1, intercept = 2, size = 1.5)
grid.arrange(g1,g2)


g4 <- ggplot(temp.result) + geom_point(aes(Tmin,Tmax - Tmin, col = as.factor(HP))) + ylab("Time Difference")
g3 <- ggplot(temp.result) + geom_point(aes(Tmin,Tmax, col = as.factor(HP)))+
  geom_abline(slope = 1, intercept = 2) +
  geom_abline(slope = 1, intercept = 4) +
  geom_abline(slope = 1, intercept = 6) +
  facet_wrap(~WE)
grid.arrange(g1, g3, main = "Distribution of Tmin & Tmax")
# peak at HP matin & soir
# weird peak around afternoon

ggplot(temp.result) + 
  geom_density(aes(Tmax, col = "Tmax")) +
  geom_density(aes(Tmin, col = "Tmin")) + facet_wrap(~WE)


ggplot(temp.result) + geom_density(aes(Tmax - Tmin)) + xlab("Time Difference")
ggplot(temp.result) + geom_bar(aes(Tmax - Tmin), binwidth = 2) + xlab("Time Difference")

temp.result$DOW[temp.result$DOW==0] <- 7
temp.result <- temp.result %>% mutate(WE = (DOW>5))


# during the week: centered in the HP matin & soir |,trajet more frequent 
# during the weekend, more disperse
g6 <- ggplot(temp.result) + geom_point(aes(Tmin,Tmax, col = as.factor(WE), alpha = .2)) + facet_wrap(~WE)
g7 <- ggplot(temp.result) + geom_point(aes(Tmin,Tmax, col = as.factor(WE), size = noPsg, alpha = .2)) + facet_wrap(~WE)
grid.arrange(g6, g7, main = "weekday vs. weekend")
###
ggplot(temp.result) + geom_point(aes(Tmin,Tmax, col = as.factor(Diff))) + facet_grid(Diff~WE)

g8 <- ggplot(temp.result) + geom_point(aes(Tmin,Tmax, col = as.factor(Diff), alpha = .2)) + facet_wrap(~WE) + ggtitle("Régularité des trajets")

grid.arrange(g8, g7, main = "weekday vs. weekend")

ggplot(temp.result) + geom_point(aes(Tmin,Tmax, col = as.factor(Diff)))
count(temp.result, Diff, HP)



###########
### 20151022
temp.result <- count(result.final,ID, OD)
temp.result <- temp.result %>% rename(WeekPattern = n)

t <- count(result.final,ID,OD, Tmax <12)
t <- t %>% rename(WeekPattern = n)
t1 <- t %>% group_by(ID,OD) %>% summarise(AMPM = n())

# find taxi driver

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
            thrPerE = nth(perE, 3)
            )

temp.noGS <- temp.S %>%
  group_by(ID) %>%
  arrange(ordS) %>%
  summarise(noGS = n(),
            maxPerS = max(perS),
            secPerS = nth(perS, 2),
            thrPerS = nth(perS, 3)
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
  geom_density(aes(maxPerS, col = "1")) +
  geom_density(aes(maxPerS + secPerS, col = "1+2")) +
  geom_density(aes(maxPerS + secPerS + thrPerS, col = "1+2+3")) +
  xlab("Per")
