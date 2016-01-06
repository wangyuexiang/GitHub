library(ggplot2)

t <- result.ZW

ggplot(t) + geom_bar(aes(H),binwidth = 1)
ggplot(t) + geom_bar(aes(DOW),binwidth = 1)

# add WeekDay
t$WE <- "WeekDay"
t[t$DOW == 0 | t$DOW == 6,]$WE <- "WeekEnd"
ggplot(t) + geom_bar(aes(H, fill = WE),binwidth = 1)
ggplot(t) + geom_bar(aes(H),binwidth = 1) + facet_wrap(~WE)

# add KeyTime
t$KeyTime <- "Night"
t[t$H >= 6 & t$H <= 22,]$KeyTime <- "Day"
ggplot(t) + geom_bar(aes(H,fill = KeyTime),binwidth = 1) + facet_wrap(~WE)

# First Result
t1 <- t %>%
  group_by(ID,WE,KeyTime) %>%
  summarise(NoPsg = n())
ggplot(t1) + geom_bar(aes(NoPsg,fill = KeyTime),binwidth = 1) + facet_wrap(~WE)

ggplot(t1) + 
  geom_bar(aes(NoPsg),binwidth = 1) + 
  facet_grid(KeyTime~WE) +
  geom_vline(xintercept = 5) +
  theme(panel.grid.minor.y = element_blank())

# Combined Result
t2 <- t %>% 
  group_by(ID) %>%
  summarise(
    WeekDay = sum(WE == "WeekDay" & KeyTime == "Day"),
    WeekNight = sum(WE == "WeekDay" & KeyTime == "Night"),
    WEDay = sum(WE == "WeekEnd" & KeyTime == "Day"),
    WENight = sum(WE == "WeekEnd" & KeyTime == "Night")
    )

ggplot(t2) + 
  geom_density(aes(WeekDay, col = "WeekDay")) +
  geom_density(aes(WeekNight, col = "WeekNight")) +
  geom_density(aes(WEDay, col = "WEDay")) +
  geom_density(aes(WENight, col = "WENight")) 

ggplot(t2) + 
  geom_bar(aes(WeekDay, fill = "WeekDay"), binwidth = 1) +
  geom_bar(aes(WeekNight, fill = "WeekNight"), binwidth = 1) +
  geom_bar(aes(WEDay, fill = "WEDay"), binwidth = 1) +
  geom_bar(aes(WENight, fill = "WENight"), binwidth = 1) 

ggplot(t2) + 
  geom_point(aes(WeekDay, WeekNight, col = "Week")) +
  geom_point(aes(WEDay, WENight, col = "WE"))
