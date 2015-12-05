library(dplyr)

filename.Input <- "VIP.csv"
day.start <- as.Date("2015-5-1")
day.end <- as.Date("2015-8-1")

input <- read.table(paste0("Input/",filename.Input), header = T, sep = ";") %>% tbl_df 
ref <- read.table("Reference/Ref_VIP.csv", header = T, sep = ";") %>% tbl_df 


trx <- input %>%
  mutate(
    ID = as.character(Badge * 100000 + Porteur),
    Date = as.Date(as.character(Date)),
    Sens = ifelse(Entr == 0,
                  ifelse(Voie <=20, 1,2),
                  0)) %>%
  filter(Date >= day.start & Date <= day.end)
