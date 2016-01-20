
# DAP.ref <- read.table("DAP_ref.csv", sep = ";")
# names(DAP.ref) <- c("ID", "Nom")
# DAP.ref$Label <- c("JA", "MJ", "MR", "VM", "CC")
# 
# DAP <- read.table("DAP.csv", head = T, sep = ";")
# names(DAP) <- c("ID", "Date", "Time", "Sor", "Voie", "Entr")

##########
# 20150709
##########
ASF.ref <- read.table("ASF_ref.txt", sep =",", head = T)
names(ASF.ref) <- c("NOM", "ID")
ASF.ref$Nom <- c("CC", "PC", "NP", "JA", "FC", "MC", "MJ", "LF", "FF", "VM", "JP", "MR")


ASF2 <- read.table("Vague1_ASF.csv", sep =";", head = TRUE)
temp <- read.table("Vague1_COFIROUTE.csv", sep =";", head = TRUE)
ASF2 <- rbind(ASF2, temp)
temp <- read.table("Vague1_ESCOTA.csv", sep =";", head = TRUE)
ASF2 <- rbind(ASF2, temp)
temp <- read.table("Vague2_ASF.csv", sep =";", head = TRUE)
ASF2 <- rbind(ASF2, temp)
temp <- read.table("Vague2_ESCOTA.csv", sep =";", head = TRUE)
ASF2 <- rbind(ASF2, temp)
names(ASF2) <- c("ID","E1","E2","E3","E4","S1","S2","S3","S4","SH1","SH2")

temp <- ASF
ASF <- ASF2
ASF2 <- temp


names(ASF) <- c("ID","E1","E2","E3","E4","S1","S2","S3","S4","SH1","SH2", "EH1", "EH2")
ASF$E3 <- as.character(ASF$E3)
ASF$S3 <- as.character(ASF$S3)
ASF[as.numeric(ASF$E3) < 10, ]$E3 <- paste0("0",ASF[as.numeric(ASF$E3) < 10, ]$E3)
ASF[as.numeric(ASF$S3) < 10, ]$S3 <- paste0("0",ASF[as.numeric(ASF$S3) < 10, ]$S3)
ASF[as.numeric(ASF$E3) < 100, ]$E3 <- paste0("0",ASF[as.numeric(ASF$E3) < 100, ]$E3)
ASF[as.numeric(ASF$S3) < 100, ]$S3 <- paste0("0",ASF[as.numeric(ASF$S3) < 100, ]$S3)
ASF$Entr <- paste0(ASF$E1, 0, ASF$E2, ASF$E3)
ASF$Sor <- paste0(ASF$S1, 0, ASF$S2, ASF$S3)
ASF$E1 <- NULL
ASF$E2 <- NULL
ASF$E3 <- NULL
ASF$S1 <- NULL
ASF$S2 <- NULL
ASF$S3 <- NULL
ASF$SH1 <- NULL
ASF$EH1 <- NULL
temp <- ASF.ref[, 2:3]
ASF <- merge(ASF, temp, by = "ID")
ASF$ID <- NULL
names(ASF)[1:4] <- c("E","S","SH","EH")

ASF$H <- as.character(ASF$H)

ASF$Y <- substr(ASF$SH,1,4)
ASF$M <- substr(ASF$SH,5,6)
ASF$D <- substr(ASF$SH,7,8)
ASF$Hour <- substr(ASF$SH,9,10)
ASF$Min <- substr(ASF$SH,11,12)
ASF$Sec <- substr(ASF$SH,13,14)

ASF$Date <- as.Date(paste0(ASF$Y, "-", ASF$M, "-", ASF$D))
ASF$DOW <- as.POSIXlt(ASF$Date)$wday
ASF$WOY <- as.numeric(format(ASF$Date+3, "%U"))
ASF$STime <- as.numeric(ASF$Hour) + as.numeric(ASF$Min)/60

ASF$Hour <- substr(ASF$EH,9,10)
ASF$Min <- substr(ASF$EH,11,12)
ASF$Sec <- substr(ASF$EH,13,14)
ASF$ETime <- as.numeric(ASF$Hour) + as.numeric(ASF$Min)/60

ASF$Y <- NULL
ASF$M <- NULL
ASF$D <- NULL
ASF$Hour <- NULL
ASF$Min <- NULL
ASF$Sec <- NULL




ggplot(ASF) + geom_bar(aes(Date), binwidth = 1) +facet_wrap(~Nom)
ggplot(ASF) + geom_bar(aes(DOW), binwidth = 1) +facet_wrap(~Nom)
ggplot(ASF) + geom_point(aes(Date, Time)) +facet_wrap(~Nom)

CC <- ASF[ASF$Nom == "CC", ]
t <- count(CC, c("Entr","Sor"))
t <- t[order(t$freq, decreasing = T), ]


ASF <- tbl_df(ASF)
duree <- ASF %>% group_by(Nom) %>%
  summarize(MinD = min(Date), MaxD = max(Date), diff = MaxD -MinD)

ggplot(duree) +  geom_segment(aes(x=MinD, xend=MaxD, y=Nom, yend=Nom, col=Nom), size = 3)

Escotis2$Date <- as.Date(paste0(Escotis2$Year, "-", Escotis2$Month, "-", Escotis2$Day))
Escotis2 <- tbl_df(Escotis2)
dureeE <- Escotis2 %>% group_by(ID) %>% summarize(MinD = min(Date), MaxD = max(Date), diff = MaxD -MinD)
ggplot(dureeE) +  geom_segment(aes(x=MinD, xend=MaxD, y=ID, yend=ID, col=ID), size = 3)

ggplot(Escotis2) + geom_point(aes(Date, TimeEntr)) + facet_wrap(~ID)
ggplot(Escotis2) + geom_point(aes(Date, Sor)) + facet_wrap(~ID)


Escotis2[Escotis2$Entr == 0, ]$TimeEntr <- Escotis2[Escotis2$Entr == 0, ]$TimeSor - .5
Escotis2[Escotis2$TimeEntr == 0, ]$TimeEntr <- Escotis2[Escotis2$TimeEntr == 0, ]$TimeSor - .5


ggplot(Escotis2) + 
  geom_point(aes(Date, TimeEntr, col ='Entr', shape ='Entr')) + 
  geom_point(aes(Date, TimeSor, col ='Sor', shape ='Sor')) + 
  geom_segment(aes(x = Date, xend = Date, y = TimeEntr, yend = TimeSor, alpha = .1)) +
  facet_wrap(~ID)


Esc <- Escotis2
names(geo)<-c("Sor","Slng","Slat")
Esc<-merge(x=Esc,y=geo,by="Sor",all.x=TRUE)
names(geo)<-c("Entr","Elng","Elat")
Esc<-merge(x=Esc,y=geo,by="Entr",all.x=TRUE)

ggplot(Esc) + 
  geom_point(aes(Date, Slng, col = "Slng", shape = "Slng", alpha = .3)) + 
  geom_point(aes(Date, Elng, col = "Elng", shape = "Elng", alpha = .3)) + 
  geom_segment(aes(x=Date, xend=Date, y=Elng, yend=Slng)) + 
  facet_wrap(~ID)

ggplot(Esc) + 
  geom_point(aes(Date, Slat, col = "Slat", shape = "Slat", alpha = .3)) + 
  geom_point(aes(Date, Elat, col = "Elat", shape = "Elat", alpha = .3)) + 
  geom_segment(aes(x=Date, xend=Date, y=Elat, yend=Slat)) + 
  facet_wrap(~ID)

ggplot(Esc) + 
  geom_point(aes(TimeSor, Slat, col = "Slat", shape = "Slat", alpha = .3)) + 
  geom_point(aes(TimeEntr, Elat, col = "Elat", shape = "Elat", alpha = .3)) + 
  geom_segment(aes(x=TimeEntr, xend=TimeSor, y=Elat, yend=Slat)) + 
  facet_wrap(~ID)


Esc.hot <- Esc %>% 
  group_by(ID, Entr, Sor) %>%
  dplyr::summarise( count = n())

Esc.hot %>%
  group_by(ID) %>%
  arrange(desc(count)) %>%
  slice(1:3)

ggplot(Esc) + 
  geom_point(aes(Slng, Slat, col = "Slat", shape = "Slat", alpha = .3)) + 
  geom_point(aes(Elng, Elat, col = "Elat", shape = "Elat", alpha = .3)) + 
  geom_segment(aes(x=Elng, xend=Slng, y=Elat, yend=Slat)) + 
  facet_wrap(~ID)