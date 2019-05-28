# 0. Set WD, load data & required libraries -------------------------------


setwd("~/Egnyte/Private/alexander.liss/0_MarSci_Practice/1_CodingTutorials/psychographic_model/z_model training")

library(tidyverse)
library(caret)
library(readxl)

mydata <- read_csv("mydata_for_model_training.csv")


# 1. set parameters and run initial training model ---------------------------------------------------

# this is a data set with 10 dependent variables
# so i need to set up a test example for a single dependent variable then automate across 10

# Create custom indices:
first.kfolds <- createFolds(mydata$Empathy.and.Humanity, k = 5)

# Create reusable trainControl object: myControl
first.control <- trainControl(
  method = "repeatedcv",
  #summaryFunction = twoClassSummary,
  #classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE,
  savePredictions = TRUE,
  index = first.kfolds
)

set.seed(37)
modeling.data <- mydata[c(2:63,67)] # subset for modeling

# training a GLM net model to start
glmnet.model <- train(
   Empathy.and.Humanity ~ ., 
   modeling.data,
   #metric = 
   method = "glmnet",
   preProcess = c("zv","center","scale","pca"), #adding pre-processing on 3.19 b/c model accuracy was terrible an actual data
   trControl = first.control
)

#assessing model performance
glmnet.model
getTrainPerf(glmnet.model)


#you want RMSE to  be less than SD as a rule
summary(modeling.data$Empathy.and.Humanity)
sd(modeling.data$Empathy.and.Humanity)

#search for more models here:
# https://rdrr.io/cran/caret/man/models.html


# 2 set up a glmnet loop across the 10 variables we need to predict --------

# create the list of columns to model- this is kinda jenky
full_model_data <- mydata[-c(1, 64:66)] #subsetting colums to just x and y vars
modelnames <- colnames(full_model_data[63:72]) #creating the NAMED range to loop through
model_range <- 63:72 #createing the INDEX ranged to loop through
model_list <- vector("list",length(model_range)) #initializing empty list with 10 slots, one for each model
model_list <- list()

# loop through the model for each column
for (i in 1:length(modelnames)){
  # create tempe model in 3 steps: data frame, then formula, then run model
  tempnum <- model_range[i]
  full_model_data_temp <- full_model_data[c(1:62,tempnum)] #  iterate one extra column through each time for the target variable
  target_var <- modelnames[[i]]
  formula <- paste(target_var," ~ .")
  temp_model <- train(as.formula(formula), 
                      full_model_data_temp,  
                      method = "glmnet",  
                      preProcess = c("zv","center","scale","pca"),
                      trControl = first.control)
  #append temp model to list
  model_list <- c(model_list, list(temp_model))
}
#compare  r -square, RMSE for each model
backup <- model_list
names(model_list) <- modelnames
temp_model

model_summary <- data.frame((sapply(model_list, getTrainPerf)))
model_summary <- data.frame(t(model_summary))

# as the last step - SAVE THIS RESULT TO an Rdata file. Then you can use in the 'quick branch'
# be sure to remove all objects EXCEPT the following:
# model_list
# model_summary
# modelnames

save.image("~/Egnyte/Private/alexander.liss/0_MarSci_Practice/1_CodingTutorials/psychographic_model/z_model training/trained_model.Rdata")
