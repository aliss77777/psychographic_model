# 0. initialize WD and load file -----------------------------------

# load data file with pre-trained model
setwd("~/Egnyte/Private/alexander.liss/0_MarSci_Practice/1_CodingTutorials/psychographic_model/quick_branch")
load("~/Egnyte/Private/alexander.liss/0_MarSci_Practice/1_CodingTutorials/psychographic_model/quick_branch/psychgraphic_model.RData")

library(tidyverse)
options(digits = 2)
set.seed(42) # for reproducible results

# 1.  Applying Psychographic Model  ------------------------------------------------------

library(caret) # you use the model_list object in the data file to apply the propensities

#load this data file when starting from the beginning 
new.data.full <- read.csv("sample data.csv")

features.for.predictive.model <- new.data.full[2:66] # removing unnecessary columns (velo.id) , brand flown, for predictive model
for (i in 1:length(modelnames)){
  temp_column <- data.frame(predict(model_list[i][[1]], newdata = features.for.predictive.model))
  names(temp_column) <- modelnames[i]
  features.for.predictive.model <- cbind(features.for.predictive.model, temp_column)
  rm(temp_column)
}
data_with_psychographic_propensities <- cbind(new.data.full[c(1,69)], features.for.predictive.model) #this adds back the info that got removed - Brand Flown (target variable)
#write.csv(data_with_psychographic_propensities, "data.with.psychographic.propensities.csv", row.names = FALSE)

# 2. Applying Logistics Regression across brands --------------------------

library(effects)

# quick exploration plot of vehicle ownership
barplot(table(data_with_psychographic_propensities$BrandFlown))

# selecting just a few columns for modeling
modeling.data <- data_with_psychographic_propensities[c(2,68:77)]

unique(modeling.data$BrandFlown)

# Fit a logistic regression model for Emirates ownership
modeling.data$BrandFlown <- as.factor(modeling.data$BrandFlown)
fit_glm <- glm(BrandFlown == "Emirates.Airline.Traveler" ~ ., family = "binomial", data = modeling.data)
summary(fit_glm)
glm_predictions <- predict(fit_glm, type="response")
modeling.data$Probability.of.Flying.Emirates <- glm_predictions


#examining variable importance for the GLM model
GLM_scores <- data.frame(varImp(fit_glm))
GLM_scores$Propensity <- row.names(GLM_scores)
GLM_labels <- row.names(GLM_scores)
row.names(GLM_scores) <- NULL
barplot(GLM_scores$Overall, names.arg = GLM_labels)


# effects plot to see the relative importance of each predictor to being an Emirates flyer
#https://www.r-bloggers.com/5-alternatives-to-the-default-r-outputs-for-glms-and-linear-models/
effects_plot <- allEffects(fit_glm)
plot(effects_plot, col = 4, ylab = "Probability of Flying Emirates", type = "response", main = " ")
# save this to the drive using the options you prefer

# # now that was a satisfying result! let's export summary statistics and a clean data frame
write.csv(fit_glm$coefficients, "Emirates Attitudinal Predictive Model Coefficients Summary.csv", row.names = FALSE)
write.csv(GLM_scores, "Emirates Predictive Model Variable Importance Scores.csv", row.names = FALSE)
data_with_psychographic_propensities$Probability.of.Flying.Emirates <- modeling.data$Probability.of.Flying.Emirates
write.csv(data_with_psychographic_propensities, "Data with updated FA & Predicted Emirates Propensity.csv", row.names = FALSE)

# 3. creating indexed importance scores for each brand --------------------

# this step repeats the model above but for all brands
# allowing you to create really cool scatterplots comparing the propensities for each brand

modeling.data$BrandFlown <- as.factor(modeling.data$BrandFlown)

# create the list of glm models for each brand and unindexed coefficients
brands <- unique(modeling.data$BrandFlown)
models_each_brand <- vector("list",length(brands))
names(models_each_brand) <- brands
importance_scores_each_brand <- data.frame(matrix(0, ncol = 10, nrow = 0)) # of columns is equal to # of predictors in the model (10)

for (i in 1:length(brands)){
  temp_model <- glm(BrandFlown == brands[i] ~ ., data = modeling.data[-12], family = "binomial")
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

# now gather everything into a clean file for tableau exploration
backup <- importance_scores_each_brand   # lol just in case something gets messed up
importance_scores_each_brand <- importance_scores_each_brand %>% #gather into a tidy layout
  gather(key = "Psychographic Dimension", value = "Unscaled_Coefficient", 2:11)
backup <- importance_scores_index_each_brand  
importance_scores_index_each_brand <- importance_scores_index_each_brand %>% #gather into a tidy layout
  gather(key = "Psychographic Dimension", value = "Indexed_Coefficient", 2:11)

# adding one more column - the mean propensity scores for each brand
propensity_by_brand <- data_with_psychographic_propensities %>%
  group_by(BrandFlown) %>%
  select(BrandFlown, 68:77) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) %>%
  gather(key = Propensity, value = Mean_Propensity, 2:11)

final_coefficients_output <- bind_cols(importance_scores_each_brand, importance_scores_index_each_brand[3], propensity_by_brand[3])

write.csv(final_coefficients_output, "Coefficients by Brand for Analysis.csv", row.names = FALSE)


# 4. visualization --------------------------------------------------------

# dropping brands with low base size  
data_for_visualization <- data_with_psychographic_propensities %>% 
  group_by(BrandFlown) %>%
  add_count(BrandFlown) %>%
  filter(n > 50) 

# we'll use this later
filtered_brand_list <- as.character(unique(data_for_visualization$BrandFlown))

# 4.1 creating a scatterplot to show how Emirates differentiates from other brands based on top propensities

# creating a ranking of top two/three propensities for our brand 
GLM_scores <- arrange(GLM_scores, -Overall)
propensity_one <- GLM_scores[1,2]
propensity_two <- GLM_scores[2,2]
propensity_three <- GLM_scores[3,2]

# scatter plot of how Emirates compared to other brands according to our key propensities
data_for_visualization %>%
  group_by(BrandFlown) %>%
  select(propensity_one, propensity_three) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) %>%
  ggplot(aes(x=Spirituality.and.Wellness, 
           y=Stability.1,
           color=BrandFlown)) + 
  geom_point() + scale_y_reverse() #reversing y axis for Emirates specifically

final_coefficients_output$`Psychographic Dimension` <- as.factor(final_coefficients_output$`Psychographic Dimension`)

# the massive matrix of key ways to message to all brands for DCO
final_coefficients_output %>%
  filter(Brand %in% filtered_brand_list) %>%
  group_by(Brand, `Psychographic Dimension`) %>%
  select(Mean_Propensity, Indexed_Coefficient) %>% 
  ggplot(aes(x=Mean_Propensity, 
             y=Indexed_Coefficient,
             color=Brand, shape=`Psychographic Dimension`)) + 
  scale_shape_manual(values=1:nlevels(final_coefficients_output$`Psychographic Dimension`)) +
  geom_point()


