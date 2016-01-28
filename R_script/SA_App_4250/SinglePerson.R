### Analyse 1 person

# get all transaction of the person

t <- Ref %>%  filter(Seg == "5_high_potential")
t <- t %>% arrange(desc(noPsg)) %>% select(ID, noPsg)


# 4: 79,142,181,201
# 5: 33,43,61,73,89
for(k in c(33,43,61,73,89))
{  
  temp <- transaction2 %>% filter(ID == k)
  
  # order his OD
  temp.OD <- temp %>%
    group_by(Entr, Sor) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    arrange(desc(n)) %>%
    mutate(ord = row_number())
  
  # add ord
  t1 <- temp.OD %>% select(-n)
  temp1 <- inner_join(temp,t1)
  
  # ggplot(temp1) + geom_point(aes(Date, TimeSor))
  g1 <- ggplot(temp1) + geom_point(aes(Date, TimeSor, col = as.factor(ord))) + theme(legend.position = "none")
  g2 <- ggplot(temp1 %>% filter(ord < 3)) + geom_point(aes(Date, TimeSor, col = as.factor(ord)))
  
  temp2 <- temp1 %>% filter(ord <3)
  g3 <- ggplot(temp2) + geom_bar(aes(DOW, fill = as.factor(ord)), binwidth = 1, position = "dodge") + theme(legend.position = "none")
  g4 <- ggplot(temp2) + geom_bar(aes(TimeSor, fill = as.factor(ord)), binwidth = 1, position = "dodge") + theme(legend.position = "none")
  
  g5 <- ggplot(temp2) + geom_tile(aes(WOY,DOW, fill = as.factor(ord))) + facet_wrap(~ord, ncol = 1) + theme(legend.position = "none")
  # grid.arrange(g1,g2,g3,g4, g5, ncol = 2, main = paste0("ID = ", k)) 
  
  # Notice that A4: width=11.69, height=8.27
  png(file = paste("ID_",k, '.png', sep=""), width = 1000, height = 1000)
  print(grid.arrange(g1,g2,g3,g4, g5, ncol = 2, main = paste0("ID = ", k) )) 
  dev.off()
  
  temp.OD
}