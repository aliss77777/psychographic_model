
# 1. initialize WD and load file -----------------------------------

setwd("~/Egnyte/Private/alexander.liss/0_MarSci_Practice/1_CodingTutorials/psychographic_model/exploratory branch")

#load this data file when starting from the beginning 
mydata <- read.csv("sample data with GUID.csv")
 
set.seed(42) # for reproducible results
options(digits = 2) # personal preference for numeric output format

# 2. dimensionality reduction ---------------------------------------------
FA_dataset <- mydata[complete.cases(mydata),-c(1,64:66)] # removing extraneous columns
library(nFactors) #this masks select from dplyr so i will only attach and detach as i need it
library(semPlot)
library(psych)

# diagnostic plot to determine # of factors
# follow this link for futher tutorials: https://www.statmethods.net/advstats/factor.html
ev <- eigen(cor(FA_dataset)) # get eigenvalues
ap <- parallel(subject=nrow(FA_dataset),var=ncol(FA_dataset),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

# creating polychloric factors, used for categorical data
poly_fit <- fa(FA_dataset, nfactors = 10, rotate = "varimax", cor = "poly")
poly_fit

#visualize the factor & features - note, does not work with a polychoric fa object, bummer
semPlotModel(poly_fit, what = "est", residuals = FALSE,
         cut=0.4, posCol=c("white", "darkblue"), negCol=c("white","red"),
         edge.label.cex = 0.75, nCharNodes = 10, sizeMan = 8)

named_factor_scores <- poly_fit$scores #create a matrix containing the factor  scores

write.csv(poly_fit$loadings, "factor analysis output.csv") # write the matrix to a csv using the polychloric model


factor_column_names <- c("Empathy.and.Humanity", # these are developed after analyzing and interpreting the output in Excel
                         "Anxiety.and.Vulnerability",
                         "Creativity.and.Adventure",
                         "Ambition.and.Curiousity",
                         "Technology.and.Social.Connection",
                         "Spirituality.and.Wellness",
                         "Comraderie.and.Affection",
                         "Culture.and.Religion",
                         "Optimistic.Self.Improvement",
                         "Stability")

colnames(named_factor_scores) <- factor_column_names

# exploring relationship of factors on brand flown
mydata_withFA <- cbind(mydata, named_factor_scores)
write.csv(mydata_withFA, "mydata_withFA_updated.csv", row.names = FALSE)

# i pulled this into tableau to make scatterplots


# 3.  Modeling  ------------------------------------------------------

detach(package:nFactors)
detach(package:psych)
library(tidyverse)
library(caret)
library(effects)

# quick exploration plot of vehicle ownership
barplot(table(mydata_withFA$BrandFlown))

# selecting just a few columns for modeling
data.for.model <- mydata_withFA[,c(66:76)]

# Fit a logistic regression model with training data
data.for.model$BrandFlown <- as.factor(data.for.model$BrandFlown)
backup <- fit_glm
fit_glm <- glm(BrandFlown == "Emirates.Airline.Traveler" ~ ., family = "binomial", data = data.for.model)
summary(fit_glm)
glm_predictions <- predict(fit_glm, type="response")
data.for.model$Prob_Emirates <- glm_predictions

#examining variable importance for the GLM model
GLM_scores <- data.frame(varImp(fit_glm))
GLM_labels <- row.names(GLM_scores)
barplot(GLM_scores$Overall, names.arg = GLM_labels)

#https://www.r-bloggers.com/5-alternatives-to-the-default-r-outputs-for-glms-and-linear-models/
effects_plot <- allEffects(fit_glm)
plot(effects_plot, col = 4, ylab = "Probability of Flying Emirates", type = "response", main = " ")

# now that was a satisfying result! let's export summary statistics and a clean data frame
write.csv(fit_glm$coefficients, " Attitudinal Predictive Model Coefficients Summary.csv")
write.csv(GLM_scores, " Attitudinal Predictive Model Variable Importance Scores.csv")
mydata_withFA$Prob_Emirates <- data.for.model$Prob_Emirates
write.csv(mydata_withFA, "Data with updated FA & Predicted  Ownership.csv", row.names = FALSE)



# 4. creating indexed importance scores for each brand --------------------

data.for.model$BrandFlown <- as.factor(data.for.model$BrandFlown)

# create the list of glm models for each brand and unindexed coefficients
brands <- unique(data.for.model$BrandFlown)
models_each_brand <- vector("list",length(brands))
names(models_each_brand) <- brands
importance_scores_each_brand <- data.frame(matrix(0, ncol = 10, nrow = 0)) # of columns is equal to # of predictors in the model (10)


for (i in 1:length(brands)){
  temp_model <- glm(BrandFlown == brands[i] ~ ., data = data.for.model, family = "binomial")
  models_each_brand[i]<- list(temp_model)
  temp_scores <- t(varImp(models_each_brand[[i]]))
  #importance_scores_each_brand[1,] <- temp_scores
  importance_scores_each_brand <- rbind(importance_scores_each_brand, temp_scores)
  #
  #models <- append(list, temp_model)
}

importance_scores_each_brand$Brand <- brands
importance_scores_each_brand <- data.frame(importance_scores_each_brand[c(11,1:10)], row.names = NULL)
write.csv(importance_scores_each_brand, "importance_scores_each_brand.csv", row.names = FALSE)

# now index the scores and create new columns
importances_column_means <- colMeans(importance_scores_each_brand[-1])
coefficient_names <- names(importance_scores_each_brand[-1])
coefficient_names_indexed <- paste(coefficient_names, "_index", sep = "") # creating the col names for the new data frame of indexed columns
importance_scores_index_each_brand <- data.frame(matrix(0, ncol = 10, nrow = 0)) # of columns is equal to # of predictors in the model (10)
colnames(importance_scores_index_each_brand) <- coefficient_names_indexed

for (i in 1:nrow(importance_scores_each_brand)){
  for (j in 1:length(importance_scores_each_brand[-1])){
    # this populats each cell of a new data frame with an indexed score of the importance of that variable (relative to the other brands)
    importance_scores_index_each_brand[i,j] <- (importance_scores_each_brand[i,j+1] / importances_column_means[j]) * 100 
    # j+1 on the operation above because the first column of importance_scores_each_brand is a factor, need to start with the 2nd column
  }
}

importance_scores_index_each_brand <- bind_cols(importance_scores_each_brand[1], importance_scores_index_each_brand)
write.csv(importance_scores_index_each_brand, "importance_scores_index_each_brand.csv", row.names = FALSE)

# now gather everything into a clean file for tableau exploration
backup <- importance_scores_each_brand   # lol just in case something gets messed up
importance_scores_each_brand <- importance_scores_each_brand %>% #gather into a tidy layout
  gather(key = "Psychographic Dimension", value = "Unscaled_Coefficient", 2:11)
backup <- importance_scores_index_each_brand  
importance_scores_index_each_brand <- importance_scores_index_each_brand %>% #gather into a tidy layout
  gather(key = "Psychographic Dimension", value = "Indexed_Coefficient", 2:11)

final_coefficients_output <- bind_cols(importance_scores_each_brand, importance_scores_index_each_brand[3])
write.csv(final_coefficients_output, "Coefficients by Brand for Analysis.csv", row.names = FALSE)

