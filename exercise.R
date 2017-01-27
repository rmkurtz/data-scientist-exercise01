#------------------------------------#
#            RTI Exercise            #
#                                    #
#                                    #
#            Rachel Kurtz            #
#------------------------------------#

library(dplyr)
library(reshape2)
library(plyr)
library(randomForest)
library(h2o)
library(ROCR)

####################################
#        Data Preparation          #
####################################

# Upload the data #
Data <- read.csv(file = "C:/Users/Rachel/Documents/Job Applications/RTI/data-scientist-exercise01/data.csv",header=FALSE, stringsAsFactors = FALSE)

# Add names to the columns #
col_names_vector <- c('id','age','workclass_id','education_level_id','education_num','marital_status_id','occupation_id',
                      'relationship_id','race_id','sex_id','capital_gain','capital_loss','hours_week','country_id',
                      'over_50k','sex_name','country_name','education_level_name','marital_status_name','occupation_name',
                      'race_name','relationship_name','workclass_name')
colnames(Data) <- col_names_vector

# Replace anything with a ? as null #
#Data[Data == '?'] <- NA

Data$over_50k_factor <- as.factor(Data$over_50k)

# Look into the proportion that are over 50k #
totals <- table(Data$over_50k)
proportion_under <- totals[as.character(0)]/(totals[as.character(0)]+totals[as.character(1)])
proportion_over <- totals[as.character(1)]/(totals[as.character(0)]+totals[as.character(1)])
proportion_under
proportion_over
# 37155 are under 50k - 76% #
# 11687 are over 50k - 24% #

# Divide the data into train, validation, and test #
set.seed(8675309) # Now that song is stuck in my head #
# Training = 70%, Validation = 20%, Testing = 10% #
perm = sample(1:dim(Data)[1])
Data_randomOrder = Data[perm,]
train = Data_randomOrder[1:floor(0.7*dim(Data)[1]),]
valid = Data_randomOrder[(floor(0.7*dim(Data)[1])+1):floor(0.9*dim(Data)[1]),]
test = Data_randomOrder[(floor(0.9*dim(Data)[1])+1):dim(Data)[1],]

# Look into the proportion that are over 50k in the test set to make sure it's representative #
totals <- table(train$over_50k)
proportion_under <- totals[as.character(0)]/(totals[as.character(0)]+totals[as.character(1)])
proportion_over <- totals[as.character(1)]/(totals[as.character(0)]+totals[as.character(1)])
proportion_under
proportion_over
# It does - yay! #

###################################
#      Exploratory analysis       #
###################################

# Create a data frame that has the numeric variables #
df_numeric <- data.frame(Data$age, Data$education_num, Data$capital_gain, Data$capital_loss, Data$hours_week)
# Look at the correlations #
df_numeric_cor <- as.matrix(cor(df_numeric,Data$over_50k))
df_numeric_cor_melt <- arrange(melt(df_numeric_cor), -abs(value))
# Most correlated to least #
# education_num    0.3326131 #
#           age    0.2303695 #
#    hours_week    0.2276868 #
#  capital_gain    0.2230130 #
#  capital_loss    0.1475545 #


# Create a data frame that has the categorical variables #
df_categorical <- data.frame(Data$workclass_id, Data$education_level_id, Data$marital_status_id, Data$occupation_id,
                             Data$relationship_id, Data$race_id, Data$sex_id, Data$country_id)
df_categorical_cor <- as.matrix(cor(df_categorical,Data$over_50k))
df_categorical_cor_melt <- arrange(melt(df_categorical_cor), -abs(value))
# Most correlated to least #
#    relationship_id    -0.25321363  #
#             sex_id    0.21462803   #
#  marital_status_id    -0.19907178  #
# education_level_id    0.08009053   #
#      occupation_id    0.07672172   #
#            race_id    0.07093429   #
#       workclass_id    0.05267409   #
#         country_id    0.01220996   - this doesn't seem like it gives any kind of information on our target #

# Find the amount missing for each variable #
sum(is.na(Data$age)) 
sum(is.na(Data$education_num))
sum(is.na(Data$capital_gain))
sum(is.na(Data$capital_loss))
sum(is.na(Data$hours_week))

sum(is.na(Data$workclass_name)) # 2799 missing - about 5% #
sum(is.na(Data$education_level_name))
sum(is.na(Data$marital_status_name))
sum(is.na(Data$occupation_name)) # 2809 missing - about 5% #
sum(is.na(Data$relationship_name))
sum(is.na(Data$race_name))
sum(is.na(Data$sex_name))
sum(is.na(Data$country_name)) # 857 missing - about 2% #

sum(is.na(Data$workclass_id)) 
sum(is.na(Data$education_level_id))
sum(is.na(Data$marital_status_id))
sum(is.na(Data$occupation_id)) 
sum(is.na(Data$relationship_id))
sum(is.na(Data$race_id))
sum(is.na(Data$sex_id))
sum(is.na(Data$country_id)) 

# Impute these missing values #
# Since it's categorical, go with the mode of the rest of the data #
# getmode <- function(v) {
#   uniqv <- unique(v)
#   uniqv[which.max(tabulate(match(v, uniqv)))]
# }
# workclass_mode <- getmode(Data$workclass_name[!is.na(Data$workclass_name)])
# occupation_mode <- getmode(Data$occupation_name[!is.na(Data$occupation_name)])
# country_mode <- getmode(Data$country_name[!is.na(Data$country_name)])
# 
# Data$workclass_name[is.na(Data$workclass_name)] <- workclass_mode
# Data$occupation_name[is.na(Data$occupation_name)] <- occupation_mode
# Data$country_name[is.na(Data$country_name)] <- country_mode


# Look at histogram of the numeric variables #
ggplot(Data) + geom_bar(aes(x = age)) # age - there seems to be an odd spike at 90 years old #
ggplot(Data) + geom_bar(aes(x = education_num)) # education number - peaks at 9 (high school grad) #
ggplot(Data) + geom_histogram(aes(x = capital_gain)) # capital gain - most in the 0 category, one maxed out at 100000#
ggplot(Data) + geom_histogram(aes(x = capital_loss)) # capital loss - most in the 0 category, dist. around 2000 #

# Look at histogram of the categorical variables
ggplot(Data) + geom_histogram(aes(x = workclass_name), stat = "count") # majority in "private" work class #
ggplot(Data) + geom_histogram(aes(x = marital_status_name), stat = "count") # most married to a civilian #
ggplot(Data) + geom_histogram(aes(x = occupation_name), stat = "count")
ggplot(Data) + geom_histogram(aes(x = relationship_name), stat = "count") # most are the husband of the family #
ggplot(Data) + geom_histogram(aes(x = race_name), stat = "count") # majority are white #
ggplot(Data) + geom_histogram(aes(x = sex_name, fill = over_50k), stat = "count") # majority are male #
ggplot(Data) + geom_histogram(aes(x = country_name), stat = "count") # majority are from US #

# Compare % of those with over $50k for each variable #
# Numeric #
ggplot(Data, aes(x = age)) + geom_bar(aes(fill = over_50k_factor), binwidth = 5 ,position='fill') 
ggplot(Data, aes(x = education_num)) + geom_bar(aes(fill = over_50k_factor),position='fill')
ggplot(Data, aes(x = capital_gain)) + geom_histogram(aes(fill = over_50k_factor),position='fill')
ggplot(Data, aes(x = capital_loss)) + geom_histogram(aes(fill = over_50k_factor),position='fill')
# Categorical #
ggplot(Data, aes(x = workclass_name)) + geom_bar(aes(fill = over_50k_factor),position='fill')
ggplot(Data, aes(x = marital_status_name)) + geom_bar(aes(fill = over_50k_factor),position='fill')
ggplot(Data, aes(x = occupation_name)) + geom_bar(aes(fill = over_50k_factor),position='fill')
ggplot(Data, aes(x = relationship_name)) + geom_bar(aes(fill = over_50k_factor),position='fill') 
ggplot(Data, aes(x = race_name)) + geom_bar(aes(fill = over_50k_factor),position='fill') 
ggplot(Data, aes(x = sex_name)) + geom_bar(aes(fill = over_50k_factor),position='fill') 
ggplot(Data, aes(x = country_name)) + geom_bar(aes(fill = over_50k_factor),position='fill') 
 

# Try to plot continuous variables in comparison to the target variable #
# Instead of plotting 1 or 0, plot the percentage that are 1 (over 50k) #
plotPercentage <- function(variable, dataframe){
  tot <- as.data.frame.matrix(table(dataframe[,variable],dataframe[,'over_50k']))
  row_ids <- as.numeric(rownames(tot))
  tot_pct <- tot$'1'/(tot$'0' + tot$'1')
  plotting_vector <- cbind(row_ids, tot_pct)
  plot(x=plotting_vector[,1], y = plotting_vector[,2])
  # Add different polynomial lines to determine the order #
  straight <- lm(plotting_vector[,2] ~ poly(plotting_vector[,1],1))
  quadratic <- lm(plotting_vector[,2] ~ poly(plotting_vector[,1],2))
  cubic <- lm(plotting_vector[,2] ~ poly(plotting_vector[,1],3))
  lines(plotting_vector[,1], predict(straight), col = 'blue')
  lines(plotting_vector[,1], predict(quadratic), col= 'red')
  lines(plotting_vector[,1], predict(cubic), col = 'dark green')
}

plotPercentage('age',Data) # seems to be a cubic relationship #
plotPercentage('education_num',Data) # seems to be a cubic relationship #
plotPercentage('hours_week',Data) # cubic - kind of #
plotPercentage('capital_gain',Data) 
plotPercentage('capital_loss',Data)


###################################
#         Model Building          #
###################################

# Random Forest #
# Numeric only #
# Determine the best mtry and ntree #
x <- cbind(Data$age, Data$education_num, Data$hours_week)
y <- Data$over_50k_factor
bestmtry <- tuneRF(x,y,stepFactr=1.5, ntree=100)
print(bestmtry)
# mtry = 1 #

# Create model #
randomForestModel_numeric <- randomForest(over_50k_factor ~ age + education_num + hours_week, 
                                          data = train, ntree = 100, mtry=1 , importance = TRUE)


varImpPlot(randomForestModel_numeric)
# edcuation_num is the variable of most importance, age is the least #
print(randomForestModel_numeric)

# Accuracy #
accuracy <- sum(diag(randomForestModel_numeric$confusion))/sum(randomForestModel_numeric$confusion)
accuracy
# 0.7990713 #

randomForest.pr = predict(randomForestModel_numeric, type = "prob", newdata = valid)[,2]
randomForest.pred = prediction(randomForest.pr, valid$over_50k)
randomForest.perf = performance(randomForest.pred, "tpr", "fpr")
plot(randomForest.perf, main = "ROC Curve for Random Forest", col = 2, lwd = 2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

# Numeric and Categorical #
# Determine the best mtry and ntree #
x <- cbind(Data$age, Data$education_num, Data$hours_week, Data$sex_id, Data$race_id, Data$relationship_id, 
           Data$workclass_id, Data$marital_status_id, Data$country_id)
y <- Data$over_50k_factor
bestmtry <- tuneRF(x,y,stepFactr=1.5, ntree=100)
print(bestmtry)
# mtry = 2 #

# Build the model #
randomForestModel_both <- randomForest(over_50k_factor ~ age + education_num + hours_week + sex_id + race_id + 
                                         relationship_id + workclass_id + marital_status_id + country_id, 
                                       data = train, ntree = 100, mtry = 2, importance = TRUE)

varImpPlot(randomForestModel_both)
# again education_num is the best, country_id is the least #
print(randomForestModel_both)

# Accuracy #
accuracy <- sum(diag(randomForestModel_both$confusion))/sum(randomForestModel_numeric$confusion)
accuracy
# 0.8335261 #

randomForest.pr = predict(randomForestModel_both, type = "prob", newdata = valid)[,2]
randomForest.pred = prediction(randomForest.pr, valid$over_50k)
randomForest.perf = performance(randomForest.pred, "tpr", "fpr")
plot(randomForest.perf, add=TRUE, col = 'green', lwd = 2)


# Logistic Regression #
logistRegression_numeric <- glm(over_50k_factor ~ .-over_50k, data = train, family = binomial)
# We get an error that there must be perfect separation in at least one of our predictor variables #
# We saw this when looking at the % of each level that were over and under 50k #
# Ex: "never worked" workclass is completely under 50k #
# Need to combine these bins with others in a logical way #

# Combine never_worked with without_pay into a new bin #
train_logistic <- train
train_logistic$workclass_id[train_logistic$workclass_id == 4 | train_logistic$workclass_id == 9] <- 10
table(train_logistic$workclass_id)

valid_logistic <- valid
valid_logistic$workclass_id[valid_logistic$workclass_id == 4 | valid_logistic$workclass_id == 9] <- 10
table(valid_logistic$workclass_id)

test_logistic <- test
test_logistic$workclass_id[test_logistic$workclass_id == 4 | test_logistic$workclass_id == 9] <- 10
table(test_logistic$workclass_id)

logistRegression_numeric <- glm(over_50k_factor ~ .-over_50k, data = train_logistic, family = binomial)
# Still coming up with the error because of our numeric/continuous values and binning may not be logical #


pred <- predict(logistRegression_numeric, newdata = valid_logistic, type = "response")

#confusion matrix
conf_matrix <- table(valid$over_50k_factor, pred > 0.5)
accuracy <- sum(diag(conf_matrix))/sum(conf_matrix)
accuracy
# 0.8519656 #

#ROC Curve
ROCRpred <- prediction(pred, valid$over_50k_factor)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf,add=TRUE, col = 'blue', lwd = 2)
legend(0.5, 0.4, c('Logistic Regression', 'Numeric Random Forest', 'Total Random Forest'), 1:3)

#### From this we see logistic seems to be the best ####
# Train on both train and validation set #

# Combine train and validation #
total_train <- rbind(train_logistic, valid_logistic)

# Train #
logistRegression_numeric <- glm(over_50k_factor ~ .-over_50k-workclass_name, data = total_train, family = binomial)

pred <- predict(logistRegression_numeric, newdata = test_logistic, type = "response")

# Score and determine accuracy on test data set #
conf_matrix <- table(test_logistic$over_50k_factor, pred > 0.5)
accuracy <- sum(diag(conf_matrix))/sum(conf_matrix)
accuracy
# 0.8483112 #
