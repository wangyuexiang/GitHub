library(ggplot2)

t <- result.ZW


ggplot(t) + geom_bar(aes(H),binwidth = 1)
ggplot(t) + geom_bar(aes(DOW),binwidth = 1)

t$WE <- "WeekDay"
t[t$DOW == 0 | t$DOW == 6,]$WE <- "WeekEnd"
ggplot(t) + geom_bar(aes(H, fill = WE),binwidth = 1)
ggplot(t) + geom_bar(aes(H),binwidth = 1) + facet_wrap(~WE)

t$KeyTime <- "Night"
t[t$H >= 6 & t$H <= 22,]$KeyTime <- "Day"
ggplot(t) + geom_bar(aes(H,fill = KeyTime),binwidth = 1) + facet_wrap(~WE)

t1 <- t %>%
  group_by(ID,WE,KeyTime) %>%
  summarise(NoPsg = n())
ggplot(t1) + geom_bar(aes(NoPsg,fill = KeyTime),binwidth = 1) + facet_wrap(~WE)

ggplot(t1) + 
  geom_bar(aes(NoPsg),binwidth = 1) + 
  facet_grid(KeyTime~WE) +
  geom_vline(xintercept = 5) +
  theme(panel.grid.minor.y = element_blank())
