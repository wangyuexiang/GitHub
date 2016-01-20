###
# 500 abonnes app VA

trx.app <- read.table("App_VA_500.csv", header = T, sep = ";")
trx.app <- tbl_df(trx.app)

trx.app$heure_sor <- as.character(trx.app$heure_sor)
trx.app$cde_soc_entr <- as.numeric(as.character(trx.app$cde_soc_entr))
trx.app$cde_soc_sor <- as.numeric(as.character(trx.app$cde_soc_sor))

trx.app <- trx.app %>% filter( !is.na(cde_soc_sor)) # remove cde_soc_sor == A2, A6, A7, Z2

trx.app <- trx.app %>%
  mutate(
    Entr = 25000000 + cde_soc_entr * 1000 + cde_entr,
    Sor = 25000000 + cde_soc_sor * 1000 + cde_sor ,
    Y = substr(heure_sor, 7, 10), M = substr(heure_sor, 4, 5), D = substr(heure_sor, 1, 2),
    Date = as.Date(paste0(Y, "-", M, "-", D)),
    DOW = as.POSIXlt(Date)$wday,
    WOY = as.numeric(format(Date+3, "%U")),
    HH = as.numeric(substr(heure_sor, 12, 13)), MM = as.numeric(substr(heure_sor, 15, 16)) ,
    TimeSor = HH + MM / 60
  ) %>%
  select(n_client,n_badge, cde_entr, Entr, Sor, Voie, Date, DOW, WOY, TimeSor) 

trx.app$Entr[trx.app$cde_entr==0] <- 0
trx.app$TimeEntr <- 0

trx.app <- trx.app %>% select(n_client, n_badge, Entr, Sor, Date, DOW, WOY, TimeEntr, TimeSor, Voie)


app.trx <- trx.app
app.trx <- app.trx %>% filter(Date >= as.Date("2015/1/1"))

app.ID <- app.trx %>% group_by(n_badge) %>% 
  summarise(Dmin = min(Date), 
            Dmax = max(Date), 
            Ddiff = Dmax - Dmin + 1, 
            Day = n_distinct(Date), 
            nOD = n())

app.ID <- app.ID %>% 
  arrange(desc(nOD)) %>%
  mutate(Label = as.character(row_number()))

temp <- app.ID %>% select(n_badge,Label)
app.trx <- inner_join(app.trx,temp)

app.trx <- app.trx %>%
  mutate ( ID = Label) %>%
  select ( ID, Entr, Sor, Date, DOW, WOY, TimeEntr, TimeSor, Voie)

# save(trx.app, file="trx.app.RData")
# write.table(trx.app, file="trx.app.csv", sep=";",row.name=FALSE,quote=FALSE)

##########
##########
### Sens O/D
gares <- gares %>% tbl_df()

temp <- read.table("export_trjtsns_asf.csv", sep = ";", header = TRUE) %>% tbl_df()
names(temp) <- c("E1","E2","E3","EA","SensEntr",
                 "S1","S2","S3","SA","SensSor")
gares.sens <- temp %>% 
  transmute(Entr = E1 * 100000 + E2 * 1000 + as.numeric(as.character(E3)), 
            SensEntr,
            Sor = S1 * 100000 + S2 * 1000 + as.numeric(as.character(S3)), 
            SensSor) %>%
  filter(!is.na(Entr) & !is.na(Sor))




