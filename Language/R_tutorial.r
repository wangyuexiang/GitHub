source('~/SVN/1-click_Dev/SA_end2end.R', echo=FALSE)
source('~/SVN/1-click_Dev/SA_function.R', encoding = 'UTF-8', echo=FALSE)

stringr::str_split_fixed(result$OD,"-", 5) %>% as.data.frame() %>% tbl_df

ExportPlot <- function(gplot, filename, width=2, height=1.5) {
    # Export plot in PDF and EPS.
    # Notice that A4: width=11.69, height=8.27
    ggsave(paste(filename, '.pdf', sep=""), gplot, width = width, height = height)
    postscript(file = paste(filename, '.eps', sep=""), width = width, height = height)
    print(gplot)
    dev.off()
    png(file = paste(filename, '_.png', sep=""), width = width * 100, height = height * 100)
    print(gplot)
    dev.off()
}

BO - 20151022
25004901446700003;25004815815900011

BO - 20151001
	25004903361400005;25004901084200002;25004819831400001;25004819831400006;25004902781100004;25004901383300004;25004901383300002

BO - première vague
	25004903597500002;25004903357700003;25004903965600001;25004902781100004;25006982804710001;25006982894800001;25006982895200001;25006980530210003
	
	
### get function run time
start.time <- Sys.time()
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#############
#############
# useful functions
# http://www.analyticsvidhya.com/blog/2015/04/comprehensive-guide-data-exploration-r/
library(reshape)
melt()
#
hist()
histinfo

#############
#############
# dplyr
#############
#############

#############
http://cran.r-project.org/web/packages/dplyr/vignettes/introduction.html
#############
select(flights, year, month, day)
select(fligths, year:day))
select(flights, - (year:day))

rename(fligths, tail_num = tailnum)

distinct(select(flights, tailnum))

mutate(fliths,
  gain = arr_delay - dep_delay,
  gain_per_hour = gain / (air_time / 60)
)

transform()
transmute()

summarise(flights,
  delay = mean(dep_delay, na.rm = TRUE))

##########
# Grouped operatoins
#   mutation(), filter() + rank(), min(x) == x  # vignette("window-functions")
#   slice() extracts rows within each group
destinations <- group_by(flights, dest)
summarise(destinations,
  planes = n_distinct(tailnum),
  flights = n()
)
  
daily <- group_by(flights, year:day)
  per_day 	<- summarise(daily, fligths = n())
  per_month <- summarise(per_day, flights = sum(flights))
  per_year 	<- summarise(per_month, flights = sum(flights))

##########
# Chaining
filter(
  summarise(
    select(
	  group_by(flights, year:day),
	  arr_delay, dep_delay
    ),
    arr = mean(arr_delay, na.rm = TRUE),
    dep = mean(dep_delay, na.rm = TRUE)
  ),
  arr > 30 | dep > 30
)  
  
# x %>% f(y)  -->  f(x, y)  
fligths %>%
  group_by(year, month, day) %>%
  select(arr_delay, dep_delay) %>%
  summarise(
    arr = mean(arr_delay, na.rm = TRUE)
    dep = mean(dep_delay, na.rm = TRUE)
  )  %>%
  filter(arr > 30 | dep > 30})
  
  
#############
# http://rpubs.com/justmarkham/dplyr-tutorial
#############
data.frame(head(flights))
# filter
# select
# arrange
# mutate
# summarise
# group_by

#############
## loading
library(dplyr)

flights <- tbl_df(hflights)
# first 10 
flights
print(flights, n=20)

#############
## filter
flights[flights$Month==1 & flights$DayofMonth==1, ]
filter(flights, Month==1, DayofMonth==1)
filter(flights, UniqueCarrier %in% c("AA", "UA"))
#############
## select
flights[, c("DepTime", "ArrTime", "FlightNum")]
select(flights, DepTime, ArrTime, FlightNum)
select(flights, Year:DayofMonth, contains("Taxi"), contains("Delay"))
#############
## chaining pipelining
filter(select(flights, UniqueCarrier, DepDelay), DepDelay > 60)
# chaining method
flights %>%
    select(UniqueCarrier, DepDelay) %>%
    filter(DepDelay > 60)
#############
## arrange
# dplyr approach
flights %>%
    select(UniqueCarrier, DepDelay) %>%
    arrange(DepDelay)
# use `desc` for descending
flights %>%
    select(UniqueCarrier, DepDelay) %>%
    arrange(desc(DepDelay))

	
save.image()
save(, file="")
pie(table(trx$Year))	
	
aggregate(d[, 3:4], list(d$Name), mean)

#############
# Azure
#############
d1 <- maml.mapInputPort(1) # class: data.frame
maml.mapOutputPort("d1");

#############
# Campus Data
#############
#Matrix
 matrix(,byrow=T,nrow=)
 colSums()
 rowSums()
 rownames()
 colnames()
 
 #Factors
 factor(,order=T,levels=)
 levels()
  summary
 
 # data.frame
 head()
 str()
 
 # list
 
Vectors (one dimensional array): can hold numeric, character or logical values. The elements in one vector all have the same datatype.
Matrices (two dimensional array): can hold numeric, character or logical values. The elements in one matrix all have the same datatype.
Data frames (two-dimensional objects): can hold numeric, character or logical values. Within a column all elements have the same data type, but different columns can be of different data type.


Big Data Challenge
	Move
	Merge
	Manage
	Murge


############
############
# Your train and test set are still loaded in
str(train)
str(test)

# Build the decision tree
my_tree_two <- rpart(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,train, method="class")

# Visualize the decision tree using plot() and text()
plot(my_tree_two)
text(my_tree_two)

# Load in the packages to create a fancified version of your tree
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# Time to plot your fancified tree
fancyRpartPlot(my_tree_two)
############
############
# Your train and test set are still loaded in
str(train)
str(test)

# Make your prediction using the test set
my_prediction <- predict(object=my_tree_two,test,type='class')

# Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)

# Check that your data frame has 418 entries
nrow(my_solution)==418

# Write your solution to a csv file with the name my_solution.csv
write.csv(my_solution,file='my_solution.csv' , row.names=FALSE)

############
############
# Your train and test set are still loaded in
str(train)
str(test)

# Create a new decision tree my_tree_three
my_tree_three <- rpart(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,train, method="class",control=rpart.control(cp=0,minsplit=50))

  
# Visualize your new decision tree
fancyRpartPlot(my_tree_three)

#############
# kaggle
#############
#############
# Decision Tree
my_tree_two <- rpart(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,train, method="class")
my_tree_three <- rpart(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,train, method="class",control=rpart.control(cp=0,minsplit=50))
# Visualize the decision tree using plot() and text()
plot(my_tree_two)
text(my_tree_two)
# Load in the packages to create a fancified version of your tree
library(rattle)
library(rpart.plot)
library(RColorBrewer)
# Time to plot your fancified tree
fancyRpartPlot(my_tree_two)
#############
# RF
library(randomForest)
my_forest <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title,
                           data=train, importance=TRUE,ntree=1000)
my_prediction <- predict(my_forest, test)
varImpPlot(my_forest)

#############
# markdown
#############
`r  `

```{r include=FALSE,
	cache=FALSE,
	echo=FALSE,
	message=FALSE,
	warning=FALSE,
	fig.width=10,
	fig.height=10}
# ```
#############
#############
plot.new()
par(mfrow=c(2,2))
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE), 
  	widths=c(3,1), heights=c(1,2))

hist(,breaks)
matplot()
# set up the plot 
plot(xrange, yrange, type="n", xlab="Age (days)",
  	ylab="Circumference (mm)" ) 
colors <- rainbow(ntrees) 
for (i in 1:ntrees) { 
  tree <- subset(Orange, Tree==i) 
  lines(tree$age, tree$circumference, type="b", lwd=1.5,
    lty=linetype[i], col=colors[i], pch=plotchar[i]) 
} 
#############
#############
# Say columns 3, 6-15 and 37 of you dataframe need to be converted to numeric one could:
dat[, c(3,6:15,37)] <- sapply(dat[, c(3,6:15,37)], as.numeric)
###############################
# Basic Functions

getwd()
list.files()
list.files(getwd())
rm(list=ls())

summary()

table() 
count() #plyr
subset()

na.omit()

heatmap()
head(,n=)
typeof()
class()
##########
##########
paste(...,collapse=",")
paste0()
##########
##########
ceiling()
floor()

unique() #data.frame
t3<-unique(ID_OD[,c(3:7,10)])
t3<-t3[order(t3$Sor),] # sort
substr() # get part of a string
sapply()
t$H<-NULL # remove column in data.frame

order(,decreasing=TRUE)
[http]
	proxy = http://proxy.asf.fr:8080

##########
##########
#Date
ISOdate(year, month, day)
as.Date()
weekdays()

Class "POSIXct" represents the (signed) number of seconds since the beginning of 1970 (in the UTC time zone) as a numeric vector. 
Class "POSIXlt" is a named list of vectors representing

Date<-seq(as.Date("2011-1-1"),as.Date("2015-4-30"),"day")
D365<-data.frame(Date)
D365$Year<-as.numeric(format(D365$Date, "%Y"))
D365$Month<-as.numeric(format(D365$Date, "%m"))
D365$Day<-as.numeric(format(D365$Date, "%d"))

D365$dow<-as.POSIXlt(D365$Date)$wday
#D365$dow[D365$dow==0]<-7
D365$woy<-as.numeric( format(D365$Date+3, "%U")

for (i in 2011:2015){
  assign(paste0("Y",i),subset(D365,D365$Year==i))
}
##########
##########


###############################
# example 
all<-read.csv("TransactionsEchantillon_20150324.csv",sep=",",header=TRUE)
all[,1]<-sapply(all[,1],as.character)

t_all<-table(all$NUM_MOY_PAIM)
table(all$NUM_MOY_PAIM,all$COD_STE_ENTR)
table(all$NUM_MOY_PAIM,all$COD_STE_SOR)

freq_all<-count(all,c("NUM_MOY_PAIM","COD_GARE_ENTR","COD_GARE_SOR"))
temp<-subset(freq_all,freq_all$freq>50)

write.table(trajets,"VIP_histo_20150510.csv",sep=",",row.name=FALSE,quote=FALSE)
###############################
# factor --> numeric
temp1$Month<-as.numeric(levels(temp1$Month))[temp1$Month]
temp1$Day<-as.numeric(levels(temp1$Day))[temp1$Day]

temp2$Month<-as.numeric(levels(temp2$Month))[temp2$Month]
temp2$Day<-as.numeric(levels(temp2$Day))[temp2$Day]
###############################
# for loop and if 
# Classify months into seasons
summer<-c(1,2,12)
fall<-c(3,4,5)
winter<-c(6,7,8)
spring<-c(9,10,11)

# Create a new column with seasons
df$season<-NA
for(i in 1:nrow(df)){
  if(df$month[i]%in%summer){df$season[i]<-"1-summer"} else
    if(df$month[i]%in%fall){df$season[i]<-"2-fall"} else
      if(df$month[i]%in%winter){df$season[i]<-"3-winter"} else
        if(df$month[i]%in%spring){df$season[i]<-"spring"} 
	}	

for (i in 1: nrow(temp_In_365)){
  for (j in 1 : nrow(temp1)){
    if(temp_In_365$Year[i] ==temp1$Year[j] && temp_In_365$Month[i]==temp1$Month[j] && temp_In_365$Day[i]  ==temp1$Day[j]) {
      temp_In_365$OD1[i]<-1
      break
    }
  }
  for (k in 1:nrow(temp2)){
    if (temp_In_365$Year[i]==temp2$Year[k] && temp_In_365$Month[i]==temp2$Month[k] && temp_In_365$Day[i]==temp2$Day[k]){
      temp_In_365$OD2[i]<-1
      break
    }
  }
}

###############################
knit2html("VIP_overall.Rmd")
knit2html("VIP_NP.Rmd")
knit2html("VIP_FF.Rmd")
knit2html("VIP.Rmd")

knit2html("UserProfile_ID1.Rmd")

knit2html("Vaucluse_Overall.Rmd")
knit2html("Vaucluse_Test.Rmd")
knit2html("Vaucluse_Test_1.Rmd")
knit2html("Vaucluse_Segmentation.Rmd")
knit2html("Vaucluse_User_Profile.Rmd")
###############################
# Ways to add a column
data$size      <- c("small", "large", "medium")
data[["size"]] <- c("small", "large", "medium")
data[,"size"]  <- c("small", "large", "medium")
data$size      <- 0   # Use the same value (0) for all rows

# Ways to remove the column
data$size      <- NULL
data[["size"]] <- NULL
data[,"size"]  <- NULL
data[[3]]      <- NULL
data[,3]       <- NULL
data           <- subset(data, select=-size)

###############################
ggplot(,aes(,fill=as.factor(),color))

geom_bar(binwidth)
geom_density
geom_point
geom_tile

ggplot(temp)+geom_bar(aes(x=noPsg, y=(..count..)/sum(..count..)),binwidth=1)+xlim(0,20)

coord_flip()
guides(fill=FALSE)

library(gridExtra)
grid.arrange(g1,g2, ncol = 2, main=textGrob("Figure 2.1 Aller-Retour a l'aeroport",gp=gpar(fontsize=20,font=1)))

theme(axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.position="none",
      panel.background=element_blank(),
      panel.border=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      plot.background=element_blank())
###############################
# reshape
library(reshape2)
t1<-dcast(temp1, Entr~Sor, value.var="KMS")

###############################
# PMML
###############################
# Load the required R libraries
library(pmml);
library(XML);
 
# Read in audit data and split into a training file and a testing file
auditDF <- read.csv("http://rattle.togaware.com/audit.csv")
auditDF <- na.omit(auditDF)              # remove NAs to make things easy
 
target <- auditDF$TARGET_Adjusted       # Get number of observations
N <- length(target); M <- N - 500  
i.train <- sample(N,M)                  # Get a random sample for training
audit.train <- auditDF[i.train,]
audit.test  <- auditDF[-i.train,]
 
# Build a logistic regression model
glm.model <- glm(audit.train$TARGET_Adjusted ~ .,data=audit.train,family="binomial")
 
# Describe the model in PMML and save it in an AML file
glm.pmml <- pmml(glm.model,name="glm model",data=trainDF)
xmlFile <- file.path(getwd(),"audit-glm.xml")
saveXML(glm.pmml,xmlFile)
