##########
##########
# Zone_TimeWindow
##########

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
