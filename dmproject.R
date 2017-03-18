install.packages("caret",dependencies = TRUE)
install.packages("plyr")
install.packages("dplyr")
installed.packages("C50")
installed.packages("kernlab")
install.packages("doParallel")
library(doParallel)
library(plyr)
library(caret)
library(dplyr)
library(C50)
library(kernlab)
install.packages("ROSE")
library(ROSE)
library(party)
install.packages("rattle")
library(rattle)
library(partykit)
library(ggplot2)
install.packages("ggthemes")
library(ggthemes)
registerDoParallel(4)
CreditcardData <- read.csv("C:/Users/inama/Desktop/Better Half of my PC/Subjects this quarter/DATA MINING/Project/UCI_Credit_Card.csv")

#View Data
View(CreditcardData)
str(CreditcardData)
class(CreditcardData)
table(CreditcardData$default.payment.next.month)

# excluding attribute "Timestamp" from training dataset
CreditcardData<- CreditcardData[,-1]
head(CreditcardData)
View(CreditcardData)

# discretising the age into young, middle aged and old
CreditcardData$AGE <- cut(CreditcardData$AGE,c(0,30,50,100),labels=c("young","middle","old"))
table(CreditcardData.rose$age)

#defining nominal variables
CreditcardData[, 1:24] <-lapply(CreditcardData[,1:24], as.numeric)
nomvars <- c(2:5,24)
CreditcardData[,nomvars] <-lapply(CreditcardData[,nomvars], as.factor)
str(CreditcardData)

#USING ROSE FUNCTION TO SAMPLE THE DATA AND BALANCING THE DATA.
CreditcardData.rose <- ROSE(default.payment.next.month ~ ., data = CreditcardData, seed = 1)$data
table(CreditcardData.rose$default.payment.next.month)
str(CreditcardData.rose)

# Is there missing data?
sum(is.na(CreditcardData.rose))

# Remove missing data rows
CleanDataset <- na.omit(CreditcardData.rose)
CleanDataset <- droplevels(CleanDataset)
summary(CleanDataset$default.payment.next.month)

# Lets check correlations , see - nomvars to exclude nominal variables
corrMatrix <- cor(CleanDataset[,-nomvars], use = "pairwise.complete.obs")

# Are there NA's ?
table(corrMatrix)

# Column indexes to remove
rmcols <- findCorrelation(corrMatrix, cutoff = 0.7, verbose = TRUE)

# Column names to remove
colnames(corrMatrix[,rmcols])

#setting the seed
set.seed(4)

# Lets do stratified sampling. Select rows to based on Class variable as strata
TrainingDataIndex <- createDataPartition(CleanDataset$default.payment.next.month, p=0.75
                                         , list = FALSE)
TrainingDataIndex

# Create Training Data as subset of Bank dataset with row index numbers as identified above and all columns
TrainData <- CleanDataset[TrainingDataIndex,]
head(TrainData)

# See percentages across classes
prop.table(table(TrainData$default.payment.next.month))

# number of rows in training data
nrow(TrainData)

# Everything else not in training is test data. Note the - (minus)sign
testData <- CleanDataset[-TrainingDataIndex,]
head(testData)
# See percentages across classes
prop.table(table(testData$default.payment.next.month))

# We will use 10 fold cross validation to train and evaluate model
TrainingParameters <- trainControl(method = "cv", number = 10)

#Decision Classification Model
# Train a model with above parameters. We will use C5.0 algorithm
DecTreeModel <- train(default.payment.next.month ~ ., data = TrainData, 
                      method = "C5.0",
                      trControl= TrainingParameters,
                      na.action = na.omit)

# check tree
DecTreeModel

# Plot performance
plot.train(DecTreeModel)
ggplot(DecTreeModel)

#PREDICTIONS
DTPredictions <-predict(DecTreeModel, testData, na.action = na.pass)
#see pridictionjs
DTPredictions

#Confusion Matrix
cmDT <-confusionMatrix(DTPredictions, testData$default.payment.next.month , positive = "yes") 
cmDT$overall
cmDT$byClass
#Heatmap for correlation matrix


#################################################################

ggplot(CreditcardData, aes(x = EDUCATION, fill = default.payment.next.month)) +
  geom_bar() +
  labs(x = 'Education') +
  theme_pander()
# RELATION BTWN DEFAULT AND SEX

ggplot(CreditcardData, aes(x = SEX, fill = default.payment.next.month)) +
  geom_bar() +
  labs(x = 'Sex') +
  theme_pander()

install.packages("corrplot")
library(corrplot)
corrplot(corrMatrix, type = "full", order = "original", 
         tl.col = "black", tl.srt = 45)

ggplot(CreditcardData, aes(x = AGE, fill = default.payment.next.month)) +
  geom_bar() +
  labs(x = 'Age') +
  theme_pander()
ggplot(CreditcardData, aes(x = LIMIT_BAL, fill = default.payment.next.month)) +
  geom_bar() +
  labs(x = 'LIMIT_BAL') +
  theme_pander()



