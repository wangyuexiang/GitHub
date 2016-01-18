library(ggplot2)

##########
### Step 1: find limit  
##########
t <- result.ZW

ggplot(t) + geom_bar(aes(H),binwidth = 1)
ggplot(t) + geom_bar(aes(DOW),binwidth = 1)

# add WeekDay
t$WE <- "WeekDay"
t[t$DOW == 0 | t$DOW == 6,]$WE <- "WeekEnd"
ggplot(t) + geom_bar(aes(H, fill = WE),binwidth = 1)
ggplot(t) + geom_bar(aes(H),binwidth = 1) + facet_wrap(~WE)

# add KeyTime
t$KeyTime <- "Night"
t[t$H >= 6 & t$H <= 22,]$KeyTime <- "Day"
ggplot(t) + geom_bar(aes(H,fill = KeyTime),binwidth = 1) + facet_wrap(~WE)

# First Result
t1 <- t %>%
  group_by(ID,WE,KeyTime) %>%
  summarise(NoPsg = n())
ggplot(t1) + geom_bar(aes(NoPsg,fill = KeyTime),binwidth = 1) + facet_wrap(~WE)

ggplot(t1) + 
  geom_bar(aes(NoPsg),binwidth = 1) + 
  facet_grid(KeyTime~WE) +
  geom_vline(xintercept = 5) +
  theme(panel.grid.minor.y = element_blank())

# Combined Result
t2 <- t %>% 
  group_by(ID) %>%
  summarise(
    WeekDay = sum(WE == "WeekDay" & KeyTime == "Day"),
    WeekNight = sum(WE == "WeekDay" & KeyTime == "Night"),
    WEDay = sum(WE == "WeekEnd" & KeyTime == "Day"),
    WENight = sum(WE == "WeekEnd" & KeyTime == "Night")
    )

ggplot(t2) + 
  geom_density(aes(WeekDay, col = "WeekDay")) +
  geom_density(aes(WeekNight, col = "WeekNight")) +
  geom_density(aes(WEDay, col = "WEDay")) +
  geom_density(aes(WENight, col = "WENight")) 

ggplot(t2) + 
  geom_bar(aes(WeekDay, fill = "WeekDay"), binwidth = 1) +
  geom_bar(aes(WeekNight, fill = "WeekNight"), binwidth = 1) +
  geom_bar(aes(WEDay, fill = "WEDay"), binwidth = 1) +
  geom_bar(aes(WENight, fill = "WENight"), binwidth = 1) 

ggplot(t2) + 
  geom_point(aes(WeekDay, WeekNight, col = "Week")) +
  geom_point(aes(WEDay, WENight, col = "WE"))

##########
### Step 2: get sample for connecting zone  
##########
# get sample
t0 <- trxZoneActive %>% group_by(ID) %>% summarise( noZone = n()) %>% arrange(desc(noZone)) %>% ungroup %>% slice(17:32)
t <- trxZoneActive %>% inner_join(t0 %>% select(ID)) %>% rename(Grid = Zone)

# viz
GridLimit <- read.table("Reference/Ref_GridLimit.csv", header = T, sep = ";") %>% tbl_df
gares <- read.table("Reference/Ref_gares.csv", header = T, sep = ";") %>% tbl_df 
temp <- GridLimit %>% slice(1)
gridStep <- temp$u - temp$d

# Connect AcitveZone with GridLimit
t1 <- t %>% inner_join(GridLimit)

# Display
ggplot(t1) + 
  geom_tile(aes(l + gridStep/2,d + gridStep/2, alpha = Per)) + 
  xlim(c(-2,8)) + ylim(c(42,49)) + 
  facet_wrap(~ID) +
  geom_point(data= gares, aes(Lng, Lat, col = as.factor(Societe)))


transaction <- t2
##########
### Step 3: connect zone  
##########
ZoneLabel <- function(transaction) {
  # For each ID, connect the Grid to make Zone
  # Args:
  #	  tansaction:	Entr, Sor, Row, Col, ...
  # Returns:
  #	  tansaction:	Entr, Sor, Row, Col, ..., Zone

	if(nrow(transaction) == 0) {
	# if no transaction, return same data with column Zone
		print("No frequent Grid!")
		return(result)
	}
  
  result <- transaction %>% mutate(Zone = 0) %>% slice(0)
  
ID.list <- transaction %>% ungroup %>% select(ID) %>% distinct

numRow <- max(GridLimit$Row)
numCol <- max(GridLimit$Col)

# get 1 personl
for(l in ID.list$ID){
	tempOnePerson <- transaction %>% filter(ID == l)
	m <- matrix(0, nrow = numRow, ncol = numCol)
	
	# prepare the matrix
for(i in 1:nrow(tempOnePerson)) m[tempOnePerson$Row[i],tempOnePerson$Col[i]] <- 1

# # test 1
# m <- matrix(0, nrow = 3, ncol = 6)
# m[c(2,4,5,6,8,9,10,11,14,16,17)] <- 1
# # test 2
# m <- matrix(0, nrow = 5, ncol = 6)
# m[c(2,6,7,12,15,18:20,23,28)] <- 1
# m
# numRow = nrow(m)
# numCol = ncol(m)

# find all Runs
NumberOfRuns <- 0;
rowRun <- c()
stRun <- c()
enRun <- c()

for(i in 1:numRow){
  rowData <- m[i,]
  if(rowData[1] == 1){
    NumberOfRuns <- NumberOfRuns + 1
    stRun <- c(stRun, 1)
    rowRun <- c(rowRun, i)
  }
  j = 1
  while(j < numCol){
    if(rowData[j] == 0 & rowData[j + 1] == 1){
      NumberOfRuns <- NumberOfRuns + 1
      stRun <- c(stRun, j + 1)
      rowRun <- c(rowRun, i)
    } else if(rowData[j] == 1 & rowData[j+1] == 0){
      enRun <- c(enRun, j)
    }
    j = j + 1
  }
  if(rowData[numCol]) enRun <- c(enRun, numCol)
}

# find all pairs
idxLabel = 1
curRowIdx = 1
firstRunOnCur = 1
firstRunOnPre = 1
lastRunOnPre = 0
equivalence <- list()
runLabels <- seq(length = NumberOfRuns, 0,0)
for(i in 1:NumberOfRuns){
  if(rowRun[i] != curRowIdx){
    curRowIdx = rowRun[i]
    firstRunOnPre = firstRunOnCur
    lastRunOnPre = i - 1
    firstRunOnCur = i
  }
  
  j = firstRunOnPre
  while(j <= lastRunOnPre){
    if((stRun[i] <= enRun[j]) &&
       (enRun[i] >= stRun[j]) && 
       rowRun[i] == (rowRun[j] + 1)
       ){
      if(runLabels[i] == 0) runLabels[i] = runLabels[j]
      else if(runLabels[i] != runLabels[j])
        equivalence[[length(equivalence) + 1]] <-c(runLabels[i],runLabels[j])
    }
    j = j + 1
  }
  if(runLabels[i] == 0){
    runLabels[i] = idxLabel
    idxLabel <- idxLabel + 1
  }
}

# get equivalent sets
maxLabel = max(runLabels)
if(maxLabel == 1){
  result <- rbind(result, tempOnePerson %>% mutate(Zone = 1))
} else{
  eqTab = matrix(nrow = maxLabel,ncol = maxLabel, FALSE)
  i = length(equivalence)
  j = 1
  while(j <= i){
    eqTab[equivalence[[j]][1],equivalence[[j]][2]] = TRUE
    eqTab[equivalence[[j]][2],equivalence[[j]][1]] = TRUE
    j = j + 1
  }
  labelFlag = seq(length = maxLabel,0,0)
  equaList <- list()
  tempList <- c()
  
  for(i in 1:maxLabel){
    if(labelFlag[i]) next
    labelFlag[i] = length(equaList) + 1
    tempList <- c(tempList,i)
    j = 1
    while(j < length(tempList) + 1){
      k = 1
      while(k <= length(eqTab[tempList[j], ])){
        if(eqTab[tempList[j],k] & !labelFlag[k]){
          tempList <- c(tempList, k)
          labelFlag[k] = length(equaList) + 1
        }      
        k = k + 1
      }
      j = j + 1
      equaList[[length(equaList) + 1]] <- tempList
      tempList <- c()
    }
  }
  
  zoneLabel <- data.frame(Row = 0, Col = 0, Label = 0)
  for(i in 1:length(rowRun)){
    temp1 <- data.frame(Row = rowRun[i], Col = seq(stRun[i],enRun[i],1), Label = runLabels[i])
    zoneLabel <- rbind(zoneLabel,temp1)
  }
  rm(temp1)
  zoneLabel <- zoneLabel %>% slice(-1)
  
  temp <- data.frame(Label = 0, Zone = 0)
  for(i in 1:length(equaList)){
    temp1 <- data.frame(Label = equaList[[i]], Zone = i)
    temp <- rbind(temp,temp1)
  }
  rm(temp1)
  temp <- temp %>% slice(-1)
  
  zoneLabel <- left_join(zoneLabel, temp) %>% select(-Label)
  
  tempOnePerson <- left_join(tempOnePerson,zoneLabel)
  result <- rbind(result, tempOnePerson)
  
  }
}
  return(result)	
}

t2 <- t1 %>% select(ID:Col)
t3 <- ZoneLabel(t2)

t4 <- t3 %>% left_join(GridLimit)
ggplot(t4) + 
  geom_tile(aes(l + gridStep/2,d + gridStep/2, fill = as.factor(Zone))) + 
  xlim(c(-2,8)) + ylim(c(42,49)) + 
  facet_wrap(~ID) +
  geom_point(data= gares, aes(Lng, Lat, col = as.factor(Societe)))
