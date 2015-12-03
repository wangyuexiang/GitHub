### TLPC
### 20151127 10:13

library(dplyr)
library(ggplot2)
library(gridExtra)

##########
# rm(test.model.0,test.model.1,test.model.2,
#    result.model.decade.0,result.model.decade.1,result.model.decade.2,
#    ind.model.0,ind.model.1,ind.model.2)
# rm(input.ASF,input.BO,input.escota,input.escota.v20150924,
#    ref.B0,ref.B1,ref.temp)
# rm(JF,gares,sens,MODELE)
# rm(BDD.temp,
#    Ind.6months, result.6months,
#    result.final, result,result1,
#    OD.list,ID.list)
# rm(r,temp,temp1,temp2,test,train,test.final)
# rm(Ind,Ind.final,input,models.units,temp.gares,temp.OD,test.result)
# rm(period,train.period,test.period)
# rm(param.days)
# rm(train.final,train.result,transaction,trx)
# rm(trx.after,trx.after.ref,trx.ready,trx.ref, VIP2, VIP3)
# rm(end.time,end.time.model.0,end.time.preparation)
# rm(g1,g2,g3,g4,g5)
# rm(param.ind3,param.max.nb.cluster,param.min.nb.for.cluster,param.min.noPsg)
# rm(param.min.uniqueTimeSor,param.model.2,param.percentage,param.SDWprnoW,param.var,param.varweekdays)
# rm(i,start.time,test.end,test.start,time.taken,time.taken.model.0,time.taken.preparation,title,train.start)
# rm(chose.model,GetInd,GetLngLat,getModel.units,getNbClusterMax,getNbClusterMax.aux,GetNumberDays,GetResult,inverse.after.SO)
# rm(Model,Model.for.a.decade,Sens,SO,SO.aux)
##########

gares <- read.table("Ref_gares.csv",sep=",",header=TRUE) %>% tbl_df

### input
TLPC <- read.table("BDD_TLPC.csv", sep = ";", header = TRUE) %>% tbl_df %>% distinct

# might be many duplicate
TLPC %>% arrange(CLIPER, DPASS, NGAPE) %>% count(CLIPER, DPASS,NGAPE,HPASS) %>% filter(n>2)

t <- TLPC
names(t) <- c("Ste", "Badge", "Porteur",
              "Sor",  "Voie", 
              "DateSor", "hSor",
              "Entr", "ind",
              "DateEntr","hEntr")

t <- t %>%  
  mutate(
    ID = Badge * 1e5 + Porteur,
    Y = substr(DateSor, 1, 4), M = substr(DateSor, 5, 6), D = substr(DateSor, 7, 8),
    DateSor = as.Date(paste0(Y, "-", M, "-", D)),
    Y = substr(DateEntr, 1, 4), M = substr(DateEntr, 5, 6), D = substr(DateEntr, 7, 8),
    DateEntr = as.Date(paste0(Y, "-", M, "-", D)),
    
    DOW = as.POSIXlt(DateSor)$wday,
    WOY = as.numeric(format(DateSor+3, "%U")),
    
    HH = hEntr %/% 10000, MM = ((hEntr %% 10000) %/% 100),
    TimeEntr = HH + MM / 60,
    HH = hSor %/% 10000, MM = ((hSor %% 10000) %/% 100),
    TimeSor = HH + MM / 60
  ) %>%
  select(Ste, ID, Badge, Porteur, Entr, Sor, Voie, DateEntr, DateSor, DOW, WOY, TimeEntr, TimeSor, ind) 

# cas1: ind == 999
# cas2: Entr == Sor
# cas3: DateSor > DateEntr & TimeSor > TimeSor
t <- t %>%
  mutate(C1 = ind == 999,
         C2 = Entr == Sor,
         C3 = !is.na(DateEntr) & ( ((DateSor == DateEntr + 1) & TimeSor > TimeEntr) |
                                    DateSor > DateEntr + 1),
         TLPC = C1 | C2 | C3
         ) 

t1 <- t %>% group_by(ID) %>% summarise(nC1 = sum(C1), nC2 = sum(C2))


# ind == 999
# 9 = 2, 36, 38
# 36 = 37

t1 <- t %>% rename(EVA = ID, Date = DateSor) %>%
  select(-c(DateEntr, ind))

# extract the prepared data
# use Algo1_TimeSpace to get the regular trx
# input the result in result
write.table(t1, "TLPC.csv",sep=";",row.name=FALSE,quote=FALSE)
result <- read.table("result.TLPC.v20151203.csv", sep = ";", header = TRUE) %>% tbl_df

t2 <- result %>% transmute(OD =as.character(OD))
t2 <- read.table(text = t2$OD,sep="-") %>% tbl_df %>%
  transmute(Entr = V1, Sor = V2)
result <- cbind(result,t2) %>% tbl_df

# get all the trx with Entr not known
t.TLPC <- t %>% filter(C1 == TRUE | C2 == TRUE) 
t.TLPC %>% count(Entr, Sor)

# compare the result and the anormal trx
t1 <- t.TLPC %>% transmute(ID, Entr, Sor, DateSor, DOW, TimeSor)
t2 <- result %>% transmute(ID, RegularEntr= Entr, Sor, DOW, Tmin, Tmax, noPsg)

t3 <-t1 %>% left_join(t2, by = c("ID","Sor", "DOW"))
t3 <- t3 %>% mutate(TLPC = (TimeSor <= Tmax) &
                           (TimeSor >= Tmin) )
                    
ACorriger <- t3 %>% filter(TLPC == TRUE) %>% select(ID:RegularEntr)
write.table(ACorriger, "ACorriger.csv",sep=";",row.name=FALSE,quote=FALSE)

t3 %>% count(Tmin <= TimeSor, Tmax >= TimeSor)


# result shows that many ID,Sor,Date has multiple sor
t4 <- t3 %>% count(ID, Default, Sor, DateSor, TimeSor) %>% filter(n>1)
t5 <- t3 %>% inner_join(t4)
