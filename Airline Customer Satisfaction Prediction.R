# Data Cleaning

install.packages('lubridate')
library('lubridate')
Data_Super_Clean <- function()
{
  csv_file <- "Satisfaction Survey.csv"
  csv_data <- read.csv(csv_file)
  csv_data <- csv_data[, -(38898:38900)]
  
  Remove_Departure_NA <- function(tempdf1)
  {
    # Blue : Departure Delay in minutes 
    tempdf_na_removal <- NULL
    tempdf_rownames <- NULL
    Mean_var <- NULL
    tempdf_na_removal <-  subset(tempdf1, (is.na(Departure.Delay.in.Minutes) != "TRUE") & (tempdf1$Airline.Status == 'Blue'))
    tempdf_rownames   <-  rownames(subset(tempdf1, (is.na(Departure.Delay.in.Minutes) == "TRUE") & (tempdf1$Airline.Status == 'Blue')))
    Mean_var  <- round(mean(tempdf_na_removal$Departure.Delay.in.Minutes))
    tempdf1[tempdf_rownames,"Departure.Delay.in.Minutes"] <- Mean_var
    
    # Gold : Departure Delay in minutes 
    tempdf_na_removal <- NULL
    tempdf_rownames <- NULL
    Mean_var <- NULL
    tempdf_na_removal <-  subset(tempdf1, (is.na(Departure.Delay.in.Minutes) != "TRUE") & (tempdf1$Airline.Status == 'Gold'))
    tempdf_rownames   <-  rownames(subset(tempdf1, (is.na(Departure.Delay.in.Minutes) == "TRUE") & (tempdf1$Airline.Status == 'Gold')))
    Mean_var  <- round(mean(tempdf_na_removal$Departure.Delay.in.Minutes))
    tempdf1[tempdf_rownames,"Departure.Delay.in.Minutes"] <- Mean_var
    
    # Silver : Departure Delay in minutes 
    tempdf_na_removal <- NULL
    tempdf_rownames <- NULL
    Mean_var <- NULL
    tempdf_na_removal <-  subset(tempdf1, (is.na(Departure.Delay.in.Minutes) != "TRUE") & (tempdf1$Airline.Status == 'Silver'))
    tempdf_rownames   <-  rownames(subset(tempdf1, (is.na(Departure.Delay.in.Minutes) == "TRUE") & (tempdf1$Airline.Status == 'Silver')))
    Mean_var  <- round(mean(tempdf_na_removal$Departure.Delay.in.Minutes))
    tempdf1[tempdf_rownames,"Departure.Delay.in.Minutes"] <- Mean_var
    
    #Platinum : Departure Delay in minutes 
    tempdf_na_removal <- NULL
    tempdf_rownames <- NULL
    Mean_var <- NULL
    tempdf_na_removal <-  subset(tempdf1, (is.na(Departure.Delay.in.Minutes) != "TRUE") & (tempdf1$Airline.Status == 'Platinum'))
    tempdf_rownames   <-  rownames(subset(tempdf1, (is.na(Departure.Delay.in.Minutes) == "TRUE") & (tempdf1$Airline.Status == 'Platinum')))
    Mean_var  <- round(mean(tempdf_na_removal$Departure.Delay.in.Minutes))
    tempdf1[tempdf_rownames,"Departure.Delay.in.Minutes"] <- Mean_var
    return(tempdf1)
  }
  
  Remove_Flight_Time_NA <- function(tempdf1)
  {
    # Blue : Flight Time in minutes 
    tempdf_na_removal <- NULL
    tempdf_rownames <- NULL
    Mean_var <- NULL
    tempdf_na_removal <-  subset(tempdf1, (is.na(Flight.time.in.minutes) != "TRUE") & (tempdf1$Airline.Status == 'Blue'))
    tempdf_rownames   <-  rownames(subset(tempdf1, (is.na(Flight.time.in.minutes) == "TRUE") & (tempdf1$Airline.Status == 'Blue')))
    Mean_var  <- round(mean(tempdf_na_removal$Flight.time.in.minutes))
    tempdf1[tempdf_rownames,"Flight.time.in.minutes"] <- Mean_var
    
    # Gold : Flight Time in minutes 
    tempdf_na_removal <- NULL
    tempdf_rownames <- NULL
    Mean_var <- NULL
    tempdf_na_removal <-  subset(tempdf1, (is.na(Flight.time.in.minutes) != "TRUE") & (tempdf1$Airline.Status == 'Gold'))
    tempdf_rownames   <-  rownames(subset(tempdf1, (is.na(Flight.time.in.minutes) == "TRUE") & (tempdf1$Airline.Status == 'Gold')))
    Mean_var  <- round(mean(tempdf_na_removal$Flight.time.in.minutes))
    tempdf1[tempdf_rownames,"Flight.time.in.minutes"] <- Mean_var
    
    # Silver : Flight Time in minutes 
    tempdf_na_removal <- NULL
    tempdf_rownames <- NULL
    Mean_var <- NULL
    tempdf_na_removal <-  subset(tempdf1, (is.na(Flight.time.in.minutes) != "TRUE") & (tempdf1$Airline.Status == 'Silver'))
    tempdf_rownames   <-  rownames(subset(tempdf1, (is.na(Flight.time.in.minutes) == "TRUE") & (tempdf1$Airline.Status == 'Silver')))
    Mean_var  <- round(mean(tempdf_na_removal$Flight.time.in.minutes))
    tempdf1[tempdf_rownames,"Flight.time.in.minutes"] <- Mean_var
    
    #Platinum : Flight Time in minutes 
    tempdf_na_removal <- NULL
    tempdf_rownames <- NULL
    Mean_var <- NULL
    tempdf_na_removal <-  subset(tempdf1, (is.na(Flight.time.in.minutes) != "TRUE") & (tempdf1$Airline.Status == 'Platinum'))
    tempdf_rownames   <-  rownames(subset(tempdf1, (is.na(Flight.time.in.minutes) == "TRUE") & (tempdf1$Airline.Status == 'Platinum')))
    Mean_var  <- round(mean(tempdf_na_removal$Flight.time.in.minutes))
    tempdf1[tempdf_rownames,"Flight.time.in.minutes"] <- Mean_var
    return(tempdf1)
  }
  
  Remove_Arrival_Delay_NA <- function(tempdf1)
  {
    # Blue : Arrival Delay in minutes
    tempdf_na_removal <- NULL
    tempdf_rownames <- NULL
    Mean_var <- NULL
    tempdf_na_removal <-  subset(tempdf1, (is.na(Arrival.Delay.in.Minutes) != "TRUE") & (tempdf1$Airline.Status == 'Blue') & (Arrival.Delay.greater.5.Mins == 'no'))
    tempdf_rownames   <-  rownames(subset(tempdf1, (is.na(Arrival.Delay.in.Minutes) == "TRUE") & (tempdf1$Airline.Status == 'Blue') & (Arrival.Delay.greater.5.Mins == 'no')))
    Mean_var  <- round(mean(tempdf_na_removal$Arrival.Delay.in.Minutes))
    tempdf1[tempdf_rownames,"Arrival.Delay.in.Minutes"] <- Mean_var
    
    # Gold : Arrival Delay in minutes
    tempdf_na_removal <- NULL
    tempdf_rownames <- NULL
    Mean_var <- NULL
    tempdf_na_removal <-  subset(tempdf1, (is.na(Arrival.Delay.in.Minutes) != "TRUE") & (tempdf1$Airline.Status == 'Gold') & (Arrival.Delay.greater.5.Mins == 'no'))
    tempdf_rownames   <-  rownames(subset(tempdf1, (is.na(Arrival.Delay.in.Minutes) == "TRUE") & (tempdf1$Airline.Status == 'Gold') & (Arrival.Delay.greater.5.Mins == 'no')))
    Mean_var  <- round(mean(tempdf_na_removal$Arrival.Delay.in.Minutes))
    tempdf1[tempdf_rownames,"Arrival.Delay.in.Minutes"] <- Mean_var
    
    # Silver : Arrival Delay in minutes
    tempdf_na_removal <- NULL
    tempdf_rownames <- NULL
    Mean_var <- NULL
    tempdf_na_removal <-  subset(tempdf1, (is.na(Arrival.Delay.in.Minutes) != "TRUE") & (tempdf1$Airline.Status == 'Silver') & (Arrival.Delay.greater.5.Mins == 'no'))
    tempdf_rownames   <-  rownames(subset(tempdf1, (is.na(Arrival.Delay.in.Minutes) == "TRUE") & (tempdf1$Airline.Status == 'Silver') & (Arrival.Delay.greater.5.Mins == 'no')))
    Mean_var  <- round(mean(tempdf_na_removal$Arrival.Delay.in.Minutes))
    tempdf1[tempdf_rownames,"Arrival.Delay.in.Minutes"] <- Mean_var
    
    #Platinum : Arrival Delay in minutes 
    tempdf_na_removal <- NULL
    tempdf_rownames <- NULL
    Mean_var <- NULL
    tempdf_na_removal <-  subset(tempdf1, (is.na(Arrival.Delay.in.Minutes) != "TRUE") & (tempdf1$Airline.Status == 'Platinum') & (Arrival.Delay.greater.5.Mins == 'no'))
    tempdf_rownames   <-  rownames(subset(tempdf1, (is.na(Arrival.Delay.in.Minutes) == "TRUE") & (tempdf1$Airline.Status == 'Platinum') & (Arrival.Delay.greater.5.Mins == 'no')))
    Mean_var  <- round(mean(tempdf_na_removal$Arrival.Delay.in.Minutes))
    tempdf1[tempdf_rownames,"Arrival.Delay.in.Minutes"] <- Mean_var
    return(tempdf1)
  }
  
  csv_data <- Remove_Departure_NA(csv_data)
  csv_data <- Remove_Flight_Time_NA(csv_data)
  csv_data <- Remove_Arrival_Delay_NA(csv_data)
  csv_data$Flight.date <- mdy(as.character(csv_data$Flight.date))
  
  csv_data$Satisfaction <- as.numeric(paste(csv_data$Satisfaction))
  csv_data$Age <- as.numeric(paste(csv_data$Age))
  csv_data$Price.Sensitivity <- as.numeric(paste(csv_data$Price.Sensitivity))
  csv_data$Year.of.First.Flight <- as.numeric(paste(csv_data$Year.of.First.Flight))
  csv_data$No.of.Flights.p.a. <- as.numeric(paste(csv_data$No.of.Flights.p.a.))
  csv_data$X..of.Flight.with.other.Airlines <- as.numeric(paste(csv_data$X..of.Flight.with.other.Airlines))
  csv_data$No..of.other.Loyalty.Cards <- as.numeric(paste(csv_data$No..of.other.Loyalty.Cards))
  csv_data$Shopping.Amount.at.Airport <- as.numeric(paste(csv_data$Shopping.Amount.at.Airport))
  csv_data$Eating.and.Drinking.at.Airport <- as.numeric(paste(csv_data$Eating.and.Drinking.at.Airport))
  csv_data$Day.of.Month <- as.numeric(paste(csv_data$Day.of.Month))
  csv_data$Scheduled.Departure.Hour <- as.numeric(paste(csv_data$Scheduled.Departure.Hour))
  csv_data$Departure.Delay.in.Minutes <- as.numeric(paste(csv_data$Departure.Delay.in.Minutes))
  csv_data$Arrival.Delay.in.Minutes <- as.numeric(paste(csv_data$Arrival.Delay.in.Minutes))
  csv_data$Flight.time.in.minutes <- as.numeric(paste(csv_data$Flight.time.in.minutes))
  csv_data$Flight.Distance <- as.numeric(paste(csv_data$Flight.Distance))
  
  return(csv_data)
}

Airline_Survey <- Data_Super_Clean()
Airline_Survey <- as.data.frame(Airline_Survey, stringsAsFactors = FALSE)
class(Airline_Survey)
View(Airline_Survey)

# Deleting irrelevant columns based on output of linear modelling
Airline_Survey <- Airline_Survey[, -c(6,7,8,10,16:21,26,27)]
Airline_Survey <- Airline_Survey[, -c(8,10,11)]

# data discretization for association mining
binFactor <- function(vec){
  vBuckets <- replicate(length(vec), "Average")
  vBuckets[vec <= 2] <- "Low"
  vBuckets[vec >= 4] <- "High"
  return(vBuckets)
}

  
AirlineSurvey_bin <- Airline_Survey

AirlineSurvey_bin$Satisfaction <- binFactor(Airline_Survey$Satisfaction)
AirlineSurvey_bin$Price.Sensitivity <- binFactor(Airline_Survey$Price.Sensitivity)


binFactorQuant <- function(vec){
  q <- quantile(vec, c(0.33, 0.66))
  vBuckets <- replicate(length(vec), "Average")
  vBuckets[vec <= q[1]] <- "Low"
  vBuckets[vec > q[2]] <- "High"
  return(vBuckets)
}
AirlineSurvey_bin$Shopping.Amount.at.Airport <- binFactorQuant(Airline_Survey$Shopping.Amount.at.Airport)
AirlineSurvey_bin$Departure.Delay.in.Minutes <- binFactorQuant(AirlineSurvey_bin$Departure.Delay.in.Minutes)
AirlineSurvey_bin$Arrival.Delay.in.Minutes <- binFactorQuant(AirlineSurvey_bin$Arrival.Delay.in.Minutes)

binAge <- function(vec){
  vBuckets <- replicate(length(vec), "Adult")
  vBuckets[vec <34] <- "Young"
  vBuckets[vec < 20] <- "Teen"
  vBuckets[vec >60] <- "Senior"
  return(vBuckets)
}
AirlineSurvey_bin$Age <- binAge(Airline_Survey$Age)

View(AirlineSurvey_bin)

binDay <- function(vec){
  vBuckets <- replicate(length(vec), "Morning")
  vBuckets[vec < 3] <- "Night"
  vBuckets[vec > 11] <- "Noon"
  vBuckets[vec > 16] <- "Evening"
  vBuckets[vec > 20] <- "Night"
  return(vBuckets)
}

AirlineSurvey_bin$Scheduled.Departure.Hour <- binDay(Airline_Survey$Scheduled.Departure.Hour)

AirlineSurvey <- AirlineSurvey_bin

AirlineSurvey$Satisfaction <- as.factor(AirlineSurvey$Satisfaction)
AirlineSurvey$Airline.Status <- as.factor(AirlineSurvey$Airline.Status)
AirlineSurvey$Age <- as.factor(AirlineSurvey$Age)
AirlineSurvey$Gender <- as.factor(AirlineSurvey$Gender)
AirlineSurvey$Price.Sensitivity <- as.factor(AirlineSurvey$Price.Sensitivity)
AirlineSurvey$Type.of.Travel <- as.factor(AirlineSurvey$Type.of.Travel)
AirlineSurvey$Shopping.Amount.at.Airport <- as.factor(AirlineSurvey$Shopping.Amount.at.Airport)
AirlineSurvey$Class <- as.factor(AirlineSurvey$Class)
AirlineSurvey$Scheduled.Departure.Hour <- as.factor(AirlineSurvey$Scheduled.Departure.Hour)
AirlineSurvey$Departure.Delay.in.Minutes <- as.factor(AirlineSurvey$Departure.Delay.in.Minutes)
AirlineSurvey$Arrival.Delay.in.Minutes <- as.factor(AirlineSurvey$Arrival.Delay.in.Minutes)
AirlineSurvey$Flight.cancelled <- as.factor(AirlineSurvey$Flight.cancelled)
AirlineSurvey$Arrival.Delay.greater.5.Mins <- as.factor(AirlineSurvey$Arrival.Delay.greater.5.Mins)

install.packages("arules")
library("arules")

install.packages("arulesViz")
library("arulesViz")


AirlineSurveyX <- as(AirlineSurvey,"transactions")
# Determining the class of he sparse matrix
class(AirlineSurveyX)

# Exploring the contents of the hotelSurveyX
inspect(AirlineSurveyX)

itemFrequency(AirlineSurveyX, type = "relative")

itemFrequencyPlot(AirlineSurveyX, support = 0.1)
View(AirlineSurveyX)
ruleset <- apriori(AirlineSurveyX, parameter = list(support = 0.2, confidence = 0.5), appearance = list(rhs='Satisfaction=High'))

# Exploring the ruleset
inspect(ruleset)
plot(ruleset)
# Providing two rules to the hotel owner in terms of what helps drive high overall customer satisfaction
goodrules <- ruleset[quality(ruleset)$lift>1.5]
inspect(goodrules)
plot(goodrules, method = "paracoord", control = list(reorder = TRUE))


rulesetLow <- apriori(AirlineSurveyX, parameter = list(support = 0.1,
                                                    confidence = 0.4),
                   appearance = list(rhs='Satisfaction=Low'))
# Exploring the ruleset
inspect(rulesetLow)
plot(rulesetLow)

# Providing two rules to the hotel owner in terms of what helps drive high overall customer satisfaction

goodrulesLow <- rulesetLow[quality(rulesetLow)$lift>2.5]
inspect(goodrulesLow)
plot(rulesetLow, method = "paracoord", control = list(reorder = TRUE))




# Decision TRee
#Finding out the cut point at two-third of the dataset
cutPoint2_3 <- floor(2 * dim(AirlineSurvey_bin)[1]/3)
cutPoint2_3
# Storing two-third of the datatset as training dataset
randIndex <- sample(1:dim(AirlineSurvey_bin)[1])
trainData <- AirlineSurvey_bin[randIndex[1:cutPoint2_3],]
# Storing the remaining one-third as testing dataset
testData <- AirlineSurvey_bin[randIndex[(cutPoint2_3+1):dim(AirlineSurvey_bin)[1]],]
dim(trainData)
dim(testData)


binHighLow <- function(vec){
  vBuckets <- replicate(length(vec), "High")
  vBuckets[vec <= 2.5] <- "Low"
  return(vBuckets)
}
  
# binning only satisfaction for decision tree  
AirlineSurvey_bin <- Airline_Survey
AirlineSurvey_bin$Satisfaction <- binFactor(Airline_Survey$Satisfaction)

prop.table(table(AirlineSurvey_bin$Satisfaction))

install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
fit <- rpart(Satisfaction ~., data = trainData, method = 'class', 
             control = rpart.control(cp = -1, maxdepth = 3))
text(fit, cex =1, use.n = TRUE, fancy = FALSE, all = TRUE)
rpart.plot(fit, extra = 106)


# SVM
#Svmstarts here
SatisfiedCustdetermine <- function(Satisfaction){
  colattr <- replicate(length(Satisfaction),"Satisfied")
  colattr[Satisfaction<3] <- "Not Satisfied"
  return(colattr)
}

Airline_Survey$CustSat <-SatisfiedCustdetermine(Airline_Survey$Satisfaction)
View(Airline_Survey)
install.packages("kernlab")
library(kernlab)

#Finding out the cut point at two-third of the dataset
cutPoint2_3 <- floor(2 * dim(Airline_Survey)[1]/3)
cutPoint2_3
# Storing two-third of the datatset as training dataset
randIndex <- sample(1:dim(Airline_Survey)[1])
trainData <- Airline_Survey[randIndex[1:cutPoint2_3],]
# Storing the remaining one-third as testing dataset
testData <- Airline_Survey[randIndex[(cutPoint2_3+1):dim(Airline_Survey)[1]],]
dim(trainData)
dim(testData)
View(trainData)

trainSample <- sample(trainData, size = 100, replace = TRUE)
testSample <- sample(testData ,size =30 , replace = TRUE)
svmOutput <- ksvm(trainSample$CustSat ~ Airline.Status + Age + Gender + Type.of.Travel,data=trainSample, 
                  kernel = "rbfdot",kpar="automatic",C=5,cross=3, prob.model=TRUE)
svmOutput
svmPred <- predict(svmOutput,testData, type = "votes")

str(svmPred)

# Linear modelling
model1<-  lm(formula = Satisfaction ~ Airline_Survey$Airline.Status + Airline_Survey$Age + Airline_Survey$Gender + Airline_Survey$Price.Sensitivity +Airline_Survey$Type.of.Travel ,data=Airline_Survey)
summary(model1)

model2 <- lm(formula = Satisfaction ~ Airline_Survey$Gender + Airline_Survey$Shopping.Amount.at.Airport + Airline_Survey$Type.of.Travel,data=Airline_Survey)
summary(model2)

model3 <- lm(formula = Satisfaction ~  Airline_Survey$ Scheduled.Departure.Hour + Airline_Survey$Departure.Delay.in.Minutes ,data=Airline_Survey)
summary(model3)

model4 <- lm(formula = Satisfaction ~ Airline_Survey$Airline.Status + Airline_Survey$Price.Sensitivity, data = Airline_Survey)
summary(model4)

model5 <- lm(formula = Satisfaction ~  Airline_Survey$Airline.Status + Airline_Survey$Type.of.Travel,data = Airline_Survey)
summary(model5)

model6 <- lm(formula = Satisfaction ~ Airline_Survey$Gender,data = Airline_Survey)
summary(model6)
plot(Airline_Survey$Gender, Airline_Survey$Satisfaction)
abline(model6)

model7 <- lm(formula = Satisfaction ~ Airline_Survey$Class + Airline_Survey$Type.of.Travel,data = Airline_Survey)
summary(model7)

model8 <- lm(formula = Satisfaction ~ Airline_Survey$Flight.cancelled + Airline_Survey$Class,data = Airline_Survey)
summary(model8)

model9 <- lm(formula = Satisfaction ~ Airline_Survey$Airline.Status + Airline_Survey$Type.of.Travel,data = Airline_Survey)
summary(model9)

model10 <- lm(formula = Satisfaction ~ Airline_Survey$Class,data = Airline_Survey)
summary(model10)

# Corelation Matrix
install.packages('corrplot')
library('corrplot')


colnames(x1)
x1<- Airline_Survey
colnames(x1)
x1<- x1[,-2] # Airline Status
colnames(x1)
x1<- x1[,-3] # Gender
colnames(x1)
x1<- x1[,-7] # Type of Travel
colnames(x1)
x1<- x1[,-25] # Arrival Delay less than 5 minutes
colnames(x1)
x1<- x1[,-22] # Flight Cancelled
colnames(x1)
x1<- x1[-12:-18] # Flight Date, Airline Code, Airline Name, Origin City, Origin State, Destination City, Destination State
colnames(x1)
x1<- x1[,-10] # Class

# [1] "Satisfaction"                     
# [2] "Age"                             
# [3] "Price.Sensitivity"                
# [4] "Year.of.First.Flight"            
# [5] "No.of.Flights.p.a."               
# [6] "X..of.Flight.with.other.Airlines"
# [7] "No..of.other.Loyalty.Cards"       
# [8] "Shopping.Amount.at.Airport"      
# [9] "Eating.and.Drinking.at.Airport"   
# [10]"Day.of.Month"                    
# [11]"Scheduled.Departure.Hour"         
# [12]"Departure.Delay.in.Minutes"      
# [13] "Arrival.Delay.in.Minutes"         
# [14]"Flight.time.in.minutes"          
# [15] "Flight.Distance"   
colnames(x1) <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
M <- cor(x1)
corrplot(M, method = 'pie', type = 'upper')

## PIE CHART OF AIRLINE STATUS DISTRIBUTION

prop.table(table(Airline_Survey$Airline.Status))

slices <- prop.table(table(Airline_Survey$Airline.Status))*100

lbls <- levels(Airline_Survey$Airline.Status)

pie3D(slices,labels=lbls, main="Airline Package Distribution")

## Pie Chart of Class Distribution

slices <- prop.table(table(Airline_Survey$Class))*100

lbls <- levels(Airline_Survey$Class)

pie(slices,labels=lbls, main="Pie Chart showing the Class distribution")



Airline_Survey$Satisfaction <- binFactor(Airline_Survey$Satisfaction)


# Plotting all attributes with respect to Satisfaction:
  
# Satisfaction vs Age # As the Age increases after 40 Satisfaction decreases

barplot(table(Airline_Survey$Satisfaction, Airline_Survey$Age), legend.text = T, col = c('red', 'pink','white'), main = "Satisfaction vs Age", xlab = 'Age', ylab = 'Satisfaction')

barplot(table(Airline_Survey$Satisfaction, Airline_Survey$Age), legend.text = T,beside = T, col = c('red', 'pink','white'), main = "Satisfaction vs Age", xlab = 'Age', ylab = 'Satisfaction')


# Satisfation vs Airline Status # Silver has the second largest Share with the maximum

barplot(table(Airline_Survey$Satisfaction, Airline_Survey$Airline.Status), legend.text = T, col = c('red', 'pink','white'), main = "Satisfaction vs Airline Status", xlab = 'Airline Status', ylab = 'Satisfaction')

barplot(table(Airline_Survey$Satisfaction, Airline_Survey$Airline.Status), legend.text = T, beside = T, col = c('red', 'pink','white'), main = "Satisfaction vs Airline Status", xlab = 'Airline Status', ylab = 'Satisfaction')

# Satisfaction vs Gender

barplot(table(Airline_Survey$Satisfaction, Airline_Survey$Gender), legend.text = T, col = c('pink', 'white','red'), beside = T, main = "Satisfaction vs Gender", xlab = 'Gender', ylab = 'Satisfaction')

barplot(table(Airline_Survey$Satisfaction, Airline_Survey$Gender), legend.text = T, col = c('pink', 'white','red'), main = "Satisfaction vs Gender", xlab = 'Gender', ylab = 'Satisfaction')

#For Gender = Male & Female (Satisfaction vs Age)

t1 <- subset(Airline_Survey, Gender == 'Male')
barplot(table(t1$Satisfaction, t1$Age), legend.text = T, col = c('pink', 'white','red'),  main = "Satisfaction vs Age for Men", xlab = 'Age', ylab = 'Satisfaction')

t1 <- subset(Airline_Survey, Gender == 'Female')
barplot(table(t1$Satisfaction, t1$Age), legend.text = T, col = c('pink', 'white','red'),  main = "Satisfaction vs Age for Female", xlab = 'Age', ylab = 'Satisfaction')

# For Gender = Male & Female (Satisfaction vs Airline Status)

t1 <- subset(Airline_Survey, Gender == 'Female')
barplot(table(t1$Satisfaction, t1$Airline.Status), beside = T, legend.text = T, col = c('pink', 'white','red'),  main = "Satisfaction vs Airline Status for Female", xlab = 'Airline Status', ylab = 'Satisfaction')

# Satisfaction vs Price Sensitivity

barplot(table(Airline_Survey$Satisfaction, Airline_Survey$Price.Sensitivity), legend.text = T, col = c('pink', 'white','red'), beside = T, main = "Satisfaction vs Price Sensitivity", xlab = 'Price Sensitivity', ylab = 'Satisfaction')

t1 <- subset(Airline_Survey, Gender == 'Female')
barplot(table(t1$Satisfaction, t1$Price.Sensitivity), beside = T, legend.text = T, col = c('pink', 'white','red'),  main = "Satisfaction vs Price Sensitivity for Female", xlab = 'Price Sensitivity', ylab = 'Satisfaction')

t1 <- subset(Airline_Survey, Gender == 'Male')
barplot(table(t1$Satisfaction, t1$Price.Sensitivity), beside = T, legend.text = T, col = c('pink', 'white','red'),  main = "Satisfaction vs Price Sensitivity for Male", xlab = 'Price Sensitivity', ylab = 'Satisfaction')


plot(Airline_Survey$Gender, Airline_Survey$Price.Sensitivity)
