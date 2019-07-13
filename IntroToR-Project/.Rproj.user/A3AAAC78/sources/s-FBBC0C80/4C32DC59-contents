#
# Intro to R
# by: Jamey Johnston
# @STATCowboy
# http://STATCowboy.com
#
# March 27, 2018
#

# Let's Start Fresh (remove all objects in R Session)
rm (list=ls())

# Turn off warnings
options(warn=-1)

#
# Basics
#
m <- 3 * 5
m

#
# Vector Examples
#

# c (combine) function to create a Vector
v <- c(2, 3, 1.5, 3.1, 49)
v

# seq function generates numeric sequences
s <- seq(from = 0, to = 100, by = .1)
s

# rep function replicates values
r <- rep(c(1,4), times = 4)
r

# : creates a number sequence incremented by 1 or -1
colon <- 1:10
print(colon)

# length of vector
length(colon)

# sort vectors
s <- c(2,3,4,1,10,8,9,7,6,0,5)
s
s2 <- sort(s)
s2


#
# Matrices
#

m1 <- matrix(1, nr = 4, nc = 4)
m2 <- matrix(2, nr = 4, nc = 4)
m1
m2

rbind(m1, m2)
cbind(m1, m2)

#Product of two matrices is `%*%'. 
rbind(m1, m2) %*% cbind(m1, m2)
cbind(m1, m2) %*% rbind(m1, m2)


#
# Data Frames
#

df <- read.table(header = TRUE, stringsAsFactors=FALSE, text = "
id name sal
1 Jamey 100
2 Melanie 500
3 Kevin 150
4 Ken 150
5 Regan 100
6 Trey 100
7 Robyn 100
8 Stef 100
9 Sherri 400
10 Jimmy 300
11 Jamey 120
")
class(df)
View(df)


# Column and Row Names
colnames(df)
rownames(df)

# Number of Rows in DF
nrow(df)
ncol(df)




# Reference Column in DF by name
df['name']

# e.g. Change salary to sal * 1.05 (5% raise) use $ reference
df$sal <- df$sal*1.05
View(df)

# Accessing by Row or Column number [row,column]
df[1,]  # row
df[1,3] # cell
df[,2]  # column


#
# List
#

li <- list(m1, m2, colon, df)
print(li)

names(li) <- c("m1","m2","colon","df")

li[[2]]
li[[4]][1,]


# LM Example
View(anscombe)
# Build Linear Model
lmout <- lm(y3 ~ x3, data.frame(anscombe))
summary(lmout)
anova(lmout)
plot(lmout)

typeof(lmout)

# Press Return in Console 4Xs

# Output of lm is a class "lm"
class(lmout)

#
# Missing Values (NA)
#

x <- c(1.2,2.3,3.4,NA)
print(x)

# Returns integer location of values (not the values)
n <- which (is.na(x))
v <- which (!is.na(x))
print(n)
print(v)

# y will be set to the values not = NA
y <- x[!is.na(x)]
print(y)

# Set missing values to mean of x (Imputation)
x[which (is.na(x))] <- mean(x[which(!is.na(x))])
x



#
# Packages
#

# List Packages already installed
library()

# Install package(s)
# Uncomment the line below to actually run and install the packages if needed!!!!

# install.packages("dplyr", "ggplot2")

# GGPlot2 Graph Example
# Load ggplot2 Package
library(ggplot2)
qplot(gear, mpg, data=mtcars, geom=c("boxplot", "jitter"), 
    fill=gear, main="Mileage by Gear Number",
                xlab="", ylab="Miles per Gallon") 

#
# Comparison Operations
#

1:10 == 2
1:10 != 2
1:10 > 2
1:10 >= 2
1:10 < 2
1:10 <= 2

x <- 2
x > 1
print(x)


#
# Logical Operations
#

x <- 1:4
x

(x > 2) | (x <= 3)

(x > 2) & (x <= 3)

xor((x > 2), (x < 4))

0:5 %in% x


#
# Control Flows
#

# IF Statements
x <- 4
if (x > 3) print("true") else print("false")
ifelse ((x > 3), print("true"), print("false"))

if(x > 3){
  print("true")
} else if(x < 3){
  print("false")
} else {
  print("Don't Know!")
}

# Nested Loops

# FOR Loops
# Simple Loop outputting Vector
for(i in 1:10) 
  print(1:i)

# Loop through data frame and print row at a time
for (i in 1:nrow(df))
  print(df[i,])

# WHILE Loops
i <- 1
while (i <= 10) 
{
  print(i)
  i <- i + 1
}


#
# Functions
#

# Create Function to calculate Bonus
bonus <- function(sal)
{
  bon <- sal * 1.05
  return(bon)
}

# View Data BEFORE Bonus
View(df)

# Create new DataFrame to compare old vs new Sals after Bonus Calc
dfBon <- df

# Run Function to Calculate Bonus
dfBon$sal <- bonus(dfBon$sal)

# View Data BEFORE Bonus
View(dfBon)


#
# Try/Catch
#
# http://adv-r.had.co.nz/Exceptions-Debugging.html
options(show.error.messages = TRUE)
results <- try("a" + "b")
print(results)

options(show.error.messages = FALSE)
results <- try("a" + "b")
print(results)


#
# DyGraphs
#

library(dygraphs)
library(fma)
library(forecast)
dygraph(pigs)
dygraph(pigs, main = "Monthly Pigs Slaughtered - Victoria") %>%  # PIPE
  dyRangeSelector(dateWindow = c("1980-01-01", "1996-01-01"))


#
# Write Sample Data to CSV File
#
View(mtcars)
write.csv(mtcars, file="mtcars.csv", append=FALSE, sep=",", col.names = TRUE)


#
# More Fun with List
# 
# These examples below all do the same thing calculate the final grade for
# for each student using different techniques in R
#
classGrades <- read.table(header = TRUE, stringsAsFactors = FALSE ,text = "
name Test Score
Jamey MidTerm 40
Jamey Project 50
Jamey Final 30
Melanie MidTerm 100
Melanie Project 100
Melanie Final 100
Kevin MidTerm 95
Kevin Project 98
Kevin Final 92
Ken MidTerm 80
Ken Project 90
Ken Final 70
")

mean(classGrades$Score)


library(dplyr)
library(tidyr)

# 
# Option 1
# Pivot via Spread and apply
# 
classGrades.FinalGrade <- spread(classGrades, Test, Score)
classGrades.FinalGrade
classGrades.FinalGrade$FinalGrade <- apply(classGrades.FinalGrade[,2:4],1,mean)
classGrades.FinalGrade

classGrades.FinalGrade <- NULL

#
# For Loop Option 2a
# Really bad code (on purpose for demonstration!!!!)
#
classGrades
classGrades <- classGrades[order(classGrades$name),]
classGrades

totalScores <- classGrades$Score[1]
cntScores <- 1
classGrades.FinalGrade <- data.frame(name=character(), 
                                     Test=character(), 
                                     Score=numeric(), stringsAsFactors = FALSE)

# Start with Row 2 (set Row 1 above)
for (i in 2:nrow(classGrades))
{
  prevName <- as.character(classGrades$name[i-1])
  if (classGrades$name[i] == prevName)
  {
    totalScores <- totalScores+classGrades$Score[i]
    cntScores <- cntScores+1
  }
  else
  {
    classGrades.FinalGrade[nrow(classGrades.FinalGrade)+1,] <- 
      c(as.character(prevName), "FinalGrade", as.numeric(totalScores/cntScores))
    totalScores <- classGrades$Score[i]
    cntScores <- 1
  }
  if (i == nrow(classGrades)) classGrades.FinalGrade[nrow(classGrades.FinalGrade)+1,] <- 
      c(as.character(prevName), "FinalGrade", as.numeric(totalScores/cntScores))
}

classGrades
classGrades.FinalGrade

classGrades.FinalGrade <- rbind(classGrades, classGrades.FinalGrade)
classGrades.FinalGrade

classGrades.FinalGrade <- NULL


#
# For Loop Option 2b
# Good For Loop code but better methods (above and below)
# as For Loops are slow!
#

names <- unique(classGrades$name)

classGrades.FinalGrade <- data.frame(name=character(), 
                                     Test=character(), 
                                     Score=numeric(), stringsAsFactors = FALSE)

for (i in 1:length(names))
{
  grades <- classGrades[classGrades$name == names[i],]
  classGrades.FinalGrade[nrow(classGrades.FinalGrade)+1,] <- 
       c(as.character(names[i]), "FinalGrade", 
         as.numeric(base::mean(grades$Score)))
}

classGrades
classGrades.FinalGrade

classGrades.FinalGrade <- rbind(classGrades, classGrades.FinalGrade)
classGrades.FinalGrade

classGrades.FinalGrade <- NULL

# 
# Option 3
# Split by name into a list of data frames and use lapply (list apply)
#
classGrades.list <- split(classGrades, classGrades$name)
classGrades.list

class(classGrades.list)
class(classGrades.list[["Jamey"]])

# classGrades.FinalGrade <- sapply(classGrades.list, function(x) {mean(x$Score)})
classGrades.FinalGrade <- 
  as.data.frame(do.call("rbind", 
                        lapply(classGrades.list, 
                               function(x) {mean(x$Score)})))
classGrades.FinalGrade <- 
  cbind(names(classGrades.list), classGrades.FinalGrade)
rownames(classGrades.FinalGrade) <- NULL
names(classGrades.FinalGrade) <- c("name", "FinalGrade")
classGrades.FinalGrade

classGrades.FinalGrade <- 
  left_join(classGrades, classGrades.FinalGrade, by="name")
classGrades.FinalGrade

classGrades.FinalGrade <- NULL

#
# Option 4 (add rows)
#
classGrades.FinalGrade <- 
  as.data.frame(do.call("rbind", 
                        lapply(classGrades.list, function(x) {mean(x$Score)})))
classGrades.FinalGrade <- 
  cbind(names(classGrades.list), "FinalGrade", classGrades.FinalGrade)
rownames(classGrades.FinalGrade) <- NULL
names(classGrades.FinalGrade) <- c("name", "Test", "Score")
classGrades.FinalGrade

classGrades.FinalGrade <- rbind(classGrades, classGrades.FinalGrade)
classGrades.FinalGrade

classGrades.FinalGrade <- 
  classGrades.FinalGrade[order(classGrades.FinalGrade$name),]
classGrades.FinalGrade

# Alternative (dplyr)
classGrades.FinalGrade <- arrange(classGrades.FinalGrade, name)
classGrades.FinalGrade

classGrades.FinalGrade <- NULL

#
# Option 5 (sqldf)
#
library(sqldf)
sel <- "select name, 'FinalGrade' as Test, avg(Score) as Score
        from classGrades
        group by name"
classGrades.FinalGrade <- sqldf(sel)
classGrades.FinalGrade

classGrades.FinalGrade <- rbind(classGrades, classGrades.FinalGrade)
classGrades.FinalGrade

classGrades.FinalGrade <- 
  classGrades.FinalGrade[order(classGrades.FinalGrade$name),]
classGrades.FinalGrade


# Install R Package in SQL Server 2016+
#
#
lib.SQL <- "C:\\Program Files\\Microsoft SQL Server\\MSSQL14.MSSQLSERVER\\R_SERVICES\\library"
install.packages("e1071", lib = lib.SQL)  



# 
# RODBC
# Connect to DB via ODBC (SQL Server example)
#
library(RODBC)

dbhandle <- odbcDriverConnect('driver={SQL Server};server=.;database=msdb;trusted_connection=true')
res <- sqlQuery(dbhandle, 'select * from information_schema.tables')
class(res)
View(res)



# Remote Connect to R Services in SQL Server and compute in SQL Server Context
#
# https://docs.microsoft.com/en-us/sql/advanced-analytics/tutorials/deepdive-define-and-use-compute-contexts
#
sqlServerConnString <- "Driver=SQL Server;Server=.;Database=IntroToR;trusted_connection=true";

sqlWait <- TRUE
sqlConsoleOutput <- TRUE
sqlShareDir <- paste0("c:\\users\\", Sys.getenv("USERNAME"), "\\Documents")

sqlCompute <- RxInSqlServer(connectionString = sqlServerConnString,shareDir = sqlShareDir,wait = sqlWait,consoleOutput = sqlConsoleOutput);
rxSetComputeContext(sqlCompute);
sqlServerSepal <- RxSqlServerData(sqlQuery = "select * from [dbo].[iris_data]", connectionString = sqlServerConnString, colInfo = list(Sepal.Length = list(type = "float64")));
sqlServerSepal

# Execute in the 
rxHistogram(~Sepal.Length,data=sqlServerSepal,histType="Percent");

form <- Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width
irisLinMod <- rxLinMod(form, data = sqlServerSepal)
irisLinMod
summary(irisLinMod)


# Bring data from SQL Server into DataFrame
df <- rxImport(inData=sqlServerSepal)
View(df)

histogram(df$Sepal.Length)



#
# Generate Sample Data for Logit Test

# The logistic function is
# f(x) = exp(x) / (exp(x) + 1) = 1 / (1 + exp(-x))
invlogit = function(x) { 1/(1+exp(-x)) }

n <- 100000
x1 <- rnorm(n)
x2 <- rbinom(n, 1, .5)
x3 <- rbinom(n, 1, .5)
b0 <- 1; b1 <- 1.5; b2<- 2; b3 <- 2.5;
y <- rbinom(n, 1, invlogit(b0 + b1*x1 + b2*x2 + b3*x3))

logdata <- as.data.frame(cbind(y,x1,x2,x3))

mylogit <- glm(y ~ x1 + x2 + x3, data=logdata, family="binomial") 

summary(mylogit)
confint(mylogit, level=.95)
plot(mylogit)

