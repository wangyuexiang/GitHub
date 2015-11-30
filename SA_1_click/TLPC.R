### TLPC
### 20151127 10:13

library(dplyr)
library(ggplot2)
library(gridExtra)


### input
TLPC <- read.table("BDD_TLPC.csv", sep = ";", header = TRUE) %>% tbl_df

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
