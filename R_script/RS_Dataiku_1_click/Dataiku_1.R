library(dplyr)

trx <- read.csv("Trajets_84.csv", head = T, sep =";")
trx <- tbl_df(trx)

names(trx) <- c("ID", "Entr", "EHoro", "Sor", "SHoro", "Prix", "KMS", "Cl")
trx$Cl <- NULL
trx$Prix <- NULL

trx <- trx %>% filter(KMS > 0)
trx %>% filter(Sor == 25006001)

trx$Year <- substr(trx$SHoro, 1, 4)

ID.2014 <- trx %>%
  filter(Year == 2014) %>%
  group_by(ID) %>%
  summarise(no = n())

ID.5000 <- ID.2014 %>% 
  arrange(desc(no)) %>%
  slice(1:5000)

ID.5000$Label <- seq(1:5000)
trx.5000 <- ID.5000 %>% inner_join(trx, by = "ID")
