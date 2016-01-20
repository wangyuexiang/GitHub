### 
# summary
# 366 n_client with 376 n_badge
# 10 n_client are linked to 2 badge
#
# trx.app: after preparation
#   app.trx: remove trx before 2015
#     trx: between 2015/1/1 and 2015/6/30
#       train: to 2015/5/31
#       test: after 2015/6/1
#     app.ID: Dmin, Dmax, Ddiff, Day, nOD

##########
### old
# temp <- app.ID %>% select(n_badge, Label)
# app.trx <- app.trx %>% inner_join(temp)
# app.ID$avgDay <- app.ID$nOD/app.ID$Day
# ggplot(app.ID) + geom_bar(aes(avgDay), binwidth = 1)
# ggplot(app.ID) + geom_point(aes(Day, avgDay))

##########
### temp - count DOW
# temp <- app.trx %>% group_by(Label) %>%
#   summarise(
#             D1 = sum(DOW == 1),
#             D2 = sum(DOW == 2),
#             D3 = sum(DOW == 3),
#             D4 = sum(DOW == 4),
#             D5 = sum(DOW == 5),
#             D6 = sum(DOW == 6),
#             D0 = sum(DOW == 0),
#             
#             noPsg = n(),
#             SD = sd(c(D1,D2,D3,D4,D5,D6,D0)),
#             
#             noWE = D6+D0,
#             noW = noPsg - noWE,
#             
#             avg = noPsg / 7,
#             avgWE = noWE / 2,
#             avgW = noW / 5
#             )
# 
# temp$over <- temp$avgW/temp$avgWE
# ggplot(temp) + geom_bar(aes(over), binwidth = 1)
# ggplot(temp) + geom_point(aes(avgW, avgWE))
# ggplot(temp) + geom_bar(aes(avgW/avgWE, binwidth = 1))
# ggplot(temp) + geom_bar(aes(avg, binwidth = 1))

# app.ID as starting point, not to be changed
app.segment <- app.ID
app.segment$ID <- app.segment$Label
app.segment <- app.segment %>% select(ID, Dmin, Dmax, Ddiff, Day, nOD)

##########
# add Model result to app.ID
##########
temp <- Ind.final
app.segment <- left_join(app.segment, temp)

##########
### add Result
##########
#   T: Model exists
#   F: NA
app.segment$Result <- TRUE
app.segment$Result[is.na(app.segment$Model)] <- FALSE

##########
### add segmentation : Small, Inactive
##########
### add small =
#   F:  history interesting
#   T:  nOD < 10            
#       Day < 5 
#       Ddiff < 5
app.segment$Small <- FALSE
app.segment$Small[app.segment$nOD < 10 | app.segment$Day < 5 | app.segment$Ddiff < 5] <- TRUE
### add Inactive =
#   0: always active
#   1: no trx after 2015/7/1
app.segment$Inactive <- FALSE
app.segment$Inactive[app.segment$Dmax < as.Date("2015/7/1")] <- TRUE

##########
### add ord by Dmin & Dmax
##########
app.segment <- app.segment %>% arrange(Dmin, Dmax) %>% mutate(ord = row_number())
app.segment <- app.segment %>% arrange(desc(nOD))

##########
### add Units
##########
temp <- models.units
names(temp)[2] <- "Units"
app.segment <- left_join(app.segment, temp)

##########
### add noT, Potential
##########
### add noT
T.matin <- app.trx %>%
  filter(TimeSor < 12 ) %>%
  group_by(ID, Entr, Sor, Sens) %>%
  summarise(noPsg = n(), T = mean(TimeSor), SD = sd(TimeSor), Tmin = T -SD, Tmax = T + SD)

T.aprem <- app.trx %>%
  filter(TimeSor >= 12 ) %>%
  group_by(ID, Entr, Sor, Sens) %>%
  summarise(noPsg = n(), T = mean(TimeSor), SD = sd(TimeSor), Tmin = T -SD, Tmax = T + SD)

T <- rbind(T.matin, T.aprem)

temp <- T %>% filter(noPsg > 10 & SD < 1) # param.min.noPsg = 5
temp1 <- count(temp, ID)
names(temp1)[2] <- "noT"
app.segment <- left_join(app.segment, temp1)

### add Potential
# T: high potential
# F: low potential
app.segment$Potential <- TRUE
app.segment$Potential[is.na(app.segment$noT)] <- FALSE

##########
### add noG, Good
##########
### add noG
temp <- count(result.final.treated,ID)
names(temp)[2] <- "noG"
app.segment <- left_join(app.segment, temp)
### add Good
#   T: Good
#   F: Bad - noG = NA
app.segment$Good <- TRUE
app.segment$Good[is.na(app.segment$noG)] <- FALSE

##########
### add New
##########
#   F:  Dmin < 2015/7/1
#   T:  Dmin >= 2015/7/1            
app.segment$New <- FALSE
app.segment$New[app.segment$Dmin >= as.Date("2015/7/1")] <- TRUE


app.segment$Label <- "Low Potential"
app.segment$Label[app.segment$Small == TRUE | app.segment$Inactive == TRUE] <- "Small or Inactive"
app.segment$Label[app.segment$Potential == TRUE] <- "High Potential"
app.segment$Label[app.segment$New == TRUE] <- "New"
app.segment$Label[app.segment$Result == TRUE] <- "With Result"


#






##########
app.ID$all <- app.ID$result + app.ID$small + app.ID$active + app.ID$T

app.ID$ind <- "Small or Inactive"
app.ID$ind[app.ID$all < 1] <- "w/ result"
app.ID$ind[app.ID$all == 1.5] <- "low potential"
app.ID$ind[app.ID$all == 1] <- "high potential"

app.ID$Result <- "no result"
app.ID$Result[app.ID$all < 1] <- "w/ result"

##########
### vis segmentation
# ggplot(app.ID) + geom_segment(aes(x=Dmin, xend=Dmax, y=ord, yend=ord, col = as.factor(all == 1.5)))
# ggplot(app.ID) + geom_bar(aes(nOD, fill = as.factor(all == 1.5)), binwidth = 50)
# 
# ggplot(app.ID) + geom_segment(aes(x=Dmin, xend=Dmax, y=ord, yend=ord, col = as.factor(all == 1)))
# ggplot(app.ID %>% filter(all < 1)) + geom_segment(aes(x=Dmin, xend=Dmax, y=ord, yend=ord, col = as.factor(all)))

##########
### create app.segment
##########
app.segment <- count(app.ID,all)
app.segment$Per <- app.ID$n/376
app.segment$Type <- c("W/ result,      ,         ", 
                      "No result,      ,         ", 
                      "No result, Small,         ", 
                      "W/ result,      , Inactive", 
                      "No result,      , Inactive",
                      "No result, Small, Inactive")

##########
### analyse 1 ID
##########
app.ID %>% filter(all == 1 & nOD > 150 & nOD <= 200)
app.ID %>% filter(is.na(Model) & !is.na(noT))

n = 124
temp <- app.trx %>% filter(Label == n)
ggplot(temp) + geom_point(aes(Date, TimeSor))

##########
### show the 5 OD most used
##########
temp1 <- count(temp, Entr, Sor) %>% ungroup() %>% arrange(desc(n)) %>% slice(1:5)
temp1$L <- 1
for(i in 2:nrow(temp1)) temp1$L[i] <- i
temp <- left_join(temp, temp1)
ggplot(temp) + geom_point(aes(Date, TimeSor, col = as.factor(L)))

##########
### analyse the result.final of the 9 models
##########
temp$ID <- temp$Label
temp1 <- result.final %>% filter(ID == n)
temp1 %>% group_by(ID,Entr,Sor,Tmin,Tmax,noPsg) %>% summarise() %>% arrange(desc(noPsg))
temp2 <- GetResult(temp,temp1)
ggplot(temp2) + geom_point(aes(Date, TimeSor, col = as.factor(result)))


##########
### viz
# ggplot(app.ID) + geom_point(aes(Dmin, Dmax,  size = nOD))
# ggplot(app.ID) + geom_tile(aes(Dmin, Dmax))
# 
# ggplot(app.ID) + geom_bar(aes(nOD), binwidth = 20)
# ggplot(app.ID) + geom_bar(aes(Day), binwidth = 5)
# 
# ggplot(app.ID) + geom_point(aes(Day, nOD))
# 
# ggplot(app.ID) +
#   geom_point(aes(Day, nOD, col = "nOD", alpha = .3)) + 
#   geom_point(aes(Day, nTrajet, col = "nTrajet", alpha = .3)) +
#   geom_point(data = app.ID, aes(Day, nOD, col = "nOD.app", alpha = .3))


nrow(result.final)
temp <- count(result.final, ID)

temp1 <- result.final %>% filter( Tmax - Tmin < 5)
temp2 <- count(temp1, ID)
names(temp2)[2] <- "nReal"
temp <- left_join(temp, temp2)

ggplot(temp) +
  geom_point(aes(ID, n , col="n")) +
  geom_point(aes(ID, nReal , col="nReal")) +
  geom_point(data = temp %>% filter(is.na(nReal)),aes(ID, n , col="n", size = 2) )+
  geom_segment(aes(x=ID, xend=ID, y= n, yend = nReal, arrow = 1))


##########
##########
### app.ID.OD
trx.after <- Sens(trx)

trx.after.ref <- trx.after %>% group_by(ID, Entr, Sor, Sens) %>%
  summarise(noPsg = n()) %>%
  ungroup() %>%
  group_by(ID) %>%
  arrange(desc(noPsg)) %>%
  mutate(ord = row_number())

trx.after <- trx.after %>% inner_join(trx.after.ref)

trx.after.ID <- trx.after %>% group_by(ID) %>%
  summarise(
    Dmin = min(Date),
    Dmax = max(Date),
    nOD = n())

temp <- trx.after.ref %>%
  summarise(no200 = sum(noPsg >= 200),
            no100 = sum(noPsg >= 100 & noPsg < 200),
            no50 = sum(noPsg >= 50 & noPsg < 100),
            no10 = sum(noPsg >= 10 & noPsg < 50),
            no3 = sum(noPsg >= 3 & noPsg < 10),
            no2 = sum(noPsg == 2),
            no1 = sum(noPsg == 1)
            )

trx.after.ID <- inner_join(trx.after.ID, temp)

temp <- app.ID %>% select(Label, Model, Ddiff, Day, Units, noT)
temp <- temp %>% mutate(ID = Label)
trx.after.ID <- inner_join(trx.after.ID, temp)

trx.after.ID <- trx.after.ID %>% 
  select(ID, Dmin, Dmax, Ddiff, Day, 
         Units, Model, noT,
         nOD, no200, no100, no50, no10, no3, no2, no1)

write.table(trx.after.ID %>% arrange(as.numeric(ID)), file = "app.ID.csv", sep=";",row.name=FALSE,quote=FALSE)

n <- 20
temp <- trx.after %>% filter(ID %in% n:(n+15))

ggplot(temp) + geom_point(aes(Date, TimeSor)) + facet_wrap(~ID) + ggtitle(paste0(n,"-",n+15))
ggplot(temp) + geom_point(aes(Date, TimeSor, size = noPsg)) + facet_wrap(~ID) + ggtitle(paste0(n,"-",n+15))
ggplot(temp%>% filter(ord < 6 | noPsg > 50)) + 
  geom_point(aes(Date, TimeSor, size = noPsg)) + facet_wrap(~ID) + ggtitle(paste0(n,"-",n+15))
ggplot(temp%>% filter(ord < 6 | noPsg > 50)) + 
  geom_point(aes(Date, TimeSor, size = noPsg, col = as.factor(ord))) +  facet_wrap(~ID) + ggtitle(paste0(n,"-",n+15))


library(knitr)
library(gridExtra)




