GetDays <- function (Dmin, Dmax){
  t <- JF %>% filter(Date >= Dmin & Date <= Dmax)
  result <- t %>%
    summarise (
      D0 = sum(DOW == 0),
      D1 = sum(DOW == 1 & JF == 0),
      D2 = sum(DOW == 2 & JF == 0),
      D3 = sum(DOW == 3 & JF == 0),
      D4 = sum(DOW == 4 & JF == 0),
      D5 = sum(DOW == 5 & JF == 0),
      D6 = sum(DOW == 6),
      Day = n()
    )
  return (result)
}



GetNumberDays <- function (period){
  result <- period %>%
    summarise (
      D0 = sum(DOW == 0),
      D1 = sum(DOW == 1 & JF == 0),
      D2 = sum(DOW == 2 & JF == 0),
      D3 = sum(DOW == 3 & JF == 0),
      D4 = sum(DOW == 4 & JF == 0),
      D5 = sum(DOW == 5 & JF == 0),
      D6 = sum(DOW == 6),
      Day = n(),
      Weekends = D0 +D6,
      Weekdays = Day -WE
    )
  return (result)
}

##########
### DOW
##########
train.start <- as.Date("2015-6-1")
test.start <- as.Date("2015-8-1")
test.end <- as.Date("2015-8-31")

train.period <- JF %>% filter(Date >= train.start & Date < test.start)
test.period <- JF %>% filter(Date >= test.start & Date <= test.end)



GetNumberDays(train.period)

t <- transaction1 %>%
  group_by(ID) %>%
  summarise(
    D1 = sum(DOW == 1),
    D2 = sum(DOW == 2),
    D3 = sum(DOW == 3),
    D4 = sum(DOW == 4),
    D5 = sum(DOW == 5),
    D6 = sum(DOW == 6),
    D0 = sum(DOW == 0),
    
    noPsg = n(),
    noWE = D6+D0,
    noW = noPsg - noWE,
    
    SD = sd(c(D1,D2,D3,D4,D5,D6,D0)),
    SDW = sd(c(D1,D2,D3,D4,D5)),
    SDprnoPsg = SD / noPsg,
    SDWprnoW = SDW / noW,
    sumSD = SDWprnoW + SDprnoPsg,
    
    avg = noPsg / 7,
    avgWE = noWE / 2,
    avgW = noW / 5
  )


##########
### ID.segment: add ord by Dmin & Dmax
##########
ID.segment <- ID %>%
  select(ID,Dmin,Dmax,Ddiff,Day,nOD) %>%
  arrange(Dmin, Dmax) %>% mutate(ord = row_number())
ID.segment <- ID.segment %>% arrange(desc(nOD))
##########
### add segmentation : Small, Inactive
##########
### add small =
#   F:  history interesting
#   T:  nOD < 10            
#       Day < 5 
#       Ddiff < 5
ID.segment$Small <- FALSE
ID.segment$Small[ID.segment$nOD < 10 | ID.segment$Day < 5 | ID.segment$Ddiff < 5] <- TRUE
### add Inactive =
#   F: always active
#   T: no trx after 2015/8/1
ID.segment$Inactive <- FALSE
ID.segment$Inactive[ID.segment$Dmax < as.Date("2015/8/1")] <- TRUE
##########
### add New
##########
#   0:  Dmin < 2015/8/1
#   1:  Dmin >= 2015/8/1            
ID.segment$New <- FALSE
ID.segment$New[ID.segment$Dmin >= as.Date("2015/8/1")] <- TRUE
##########
### add noT, Potential
##########
### add noT
T.matin <- transaction1 %>%
  filter(TimeSor < 12 ) %>%
  group_by(ID, Entr, Sor, Sens) %>%
  summarise(noPsg = n(), T = mean(TimeSor), SD = sd(TimeSor), Tmin = T -SD, Tmax = T + SD)

T.aprem <- transaction1 %>%
  filter(TimeSor >= 12 ) %>%
  group_by(ID, Entr, Sor, Sens) %>%
  summarise(noPsg = n(), T = mean(TimeSor), SD = sd(TimeSor), Tmin = T -SD, Tmax = T + SD)

T <- rbind(T.matin, T.aprem)

temp <- T %>% filter(noPsg > 10 & SD < 1) # param.min.noPsg = 5
temp1 <- count(temp, ID)
names(temp1)[2] <- "noT"
ID.segment <- left_join(ID.segment, temp1)

rm(T,T.matin,T.aprem,temp,temp1)
### add Potential
# T: high potential
# F: low potential
ID.segment$Potential <- TRUE
ID.segment$Potential[is.na(ID.segment$noT)] <- FALSE

##########
### add Label
##########
ID.segment$Label <- "Low Potential"
ID.segment$Label[ID.segment$Small == TRUE | ID.segment$Inactive == TRUE] <- "Small or Inactive"
ID.segment$Label[ID.segment$Potential == TRUE] <- "High Potential"
ID.segment$Label[ID.segment$New == TRUE] <- "New"

ggplot(ID.segment) + geom_bar(aes(nOD, fill = as.factor(Label)),binwidth = 50)
ggplot(ID.segment) + geom_segment(aes(x=Dmin, xend=Dmax, y=ord, yend=ord, col = as.factor(Label)))
