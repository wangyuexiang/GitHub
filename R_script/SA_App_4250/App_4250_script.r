### load package
library(dplyr)
library(cluster)
# library(zoo)

### get arguments
args <- commandArgs(trailingOnly = TRUE)
# Command Line example
# cd C:\Users\wangy2\Documents\GitHub\SA_App_4250
# rscript App_4250_script.r Input_v20151126.csv Output.csv 2015-8-31 2015-8-1 2015-5-1

InputFileName <- toString(args[1])
OutputFileName <- toString(args[2])

test.end <- as.Date(args[3])
test.start <- as.Date(args[4])
train.start <- as.Date(args[5])

# test.end <- as.Date("2015-8-31")
# test.start <- as.Date("2015-8-1")
# train.start <- as.Date("2015-5-1")

### load function
source('SA_function.R', encoding = 'UTF-8')

### input 
transaction <- read.table(InputFileName, header = T, sep = ";") %>% tbl_df
t <- transaction %>%
  mutate(
    ID = as.character(Badge),
    Date = as.Date(as.character(Date)),
    Sens = ifelse(Entr == 0,
                  ifelse(Voie <=20, 1,2),
                  0))


### process
t1 <- t %>% filter(Date >= train.start & Date <= test.end)

### output
write.table(t1, OutputFileName,sep=";",row.name=FALSE,quote=FALSE)

print("test end")