# Define train_values_url
train_values_url <- "http://s3.amazonaws.com/drivendata/data/7/public/4910797b-ee55-40a7-8668-10efd5c1b960.csv"
# import train_values
train_values <- read.csv(train_values_url)

# Define train_labels_url
train_labels_url <- "http://s3.amazonaws.com/drivendata/data/7/public/0bf8bc6e-30d0-4c50-956a-603fc693d966.csv"
# import train_labels
train_labels <- read.csv(train_labels_url)

# Define test_values_url
test_values_url <- "http://s3.amazonaws.com/drivendata/data/7/public/702ddfc5-68cd-4d1d-a0de-f5f566f76d91.csv"
# import test_values
test_values <- read.csv(test_values_url)

# Merge training values and labels
train <- merge(train_labels,train_values)
test <- merge(train_labels,test_values)

# Number of pumps in each functional status_group
table(train$status_group)

# As proportions
prop.table(table(train$status_group))

# Table of the quantity variable vs the status of the pumps
table(train$quantity, train$status_group)

# As row-wise proportions, quantity vs status_group
prop.table(table(train$quantity, train$status_group), margin = 1)

# Table of the payment variable vs the status of the pumps
table(train$payment, train$status_group)
prop.table(table(train$payment, train$status_group), margin = 1)

# Check out the structure of the variables in train with str(). You will see the majority of them are categorical. If there aren't too many categories in a variable, a bar chart can be a great way to visualize and digest your data.
#In the sample code to the right, you have been provided with a command that will produce a plot that shows a breakdown of the quantity variable broken up by status_group. Here are a few variables that you can view with a similar command:

#quality_group - The quality of the water
#extraction_type_class - The kind of extraction the water point uses
#payment - What the water costs
#source_type - The source of the water
#waterpoint_type - The kind of water point

# Load the ggplot package and examine train
library(ggplot2)
str(train)

# Create bar plot for quantity
qplot(quantity, data=train, geom="bar", fill=status_group) + 
  theme(legend.position = "top")

# Create bar plot for quality_group
qplot(quality_group, data=train, geom="bar", fill=status_group) + 
  theme(legend.position = "top")

# Create bar plot for waterpoint_type
qplot(waterpoint_type, data=train, geom="bar", fill=status_group) + 
  theme(legend.position = "top") + 
  theme(axis.text.x=element_text(angle = -20, hjust = 0))

# You just made some great plots that compared some categorical variables 
# based on the well status. Now you can look at some ordinal or continuous 
# variables using ggplot2 and geom_histogram

# Observe the distribution of some of these variables 
#Here is a list of a few variables in train that you could view in this way:
  
#amount_tsh - Total static head (amount water available to water point)
#gps_height - Altitude of the well
#population - Population around the well
#construction_year - Year the water point was constructed

# Create a histogram for `construction_year` grouped by `status_group`
ggplot(train, aes(x = construction_year)) + 
  geom_histogram(bins = 20) + 
  facet_grid( ~ status_group)

# Now subsetting when construction_year is larger than 0
ggplot(subset(train, construction_year > 0), aes(x = construction_year)) +
  geom_histogram(bins = 20) + 
  facet_grid( ~ status_group)

# Mapping well locations
install.packages("googleVis")
library(googleVis)

# Create scatter plot: latitude vs longitude with color as status_group
ggplot(subset(train[1:1000,], latitude < 0 & longitude > 0),
       aes(x = latitude, y = longitude, color = status_group)) + 
  geom_point(shape = 1) + 
  theme(legend.position = "top")

# Create a column 'latlong' to input into gvisGeoChart
train$latlong <- paste(round(train$latitude,2), round(train$longitude, 2), sep = ":")

# Could not get this code to work 
# Use gvisGeoChart to create an interactive map with well locations
wells_map <- gvisGeoChart(train[1:1000,], locationvar = "train$latlong", 
                          colorvar = "train$status_group", sizevar = "train$Size", 
                          options = list(region = "train$TZ"))

# Plot wells_map
wells_map

# Make some predictions
# Load the randomForest library
install.packages("randomForest", dependencies=TRUE)
library(randomForest)

# Set seed and create a random forest classifier
set.seed(42)
model_forest <- randomForest(as.factor(status_group) ~ longitude + latitude + extraction_type_group + quality_group + quantity + waterpoint_type + construction_year, data = train, importance = TRUE, ntree = 5, nodesize = 2)

# Use random forest to predict the values in train
pred_forest_train <- predict(model_forest, train)

# Observe the first few rows of your predictions
head(pred_forest_train)

# Create a confusion matrix to examine the results more closely
install.packages("caret", dependencies=TRUE)
library(caret)

confusionMatrix(pred_forest_train,train$status_group)

# Variable importance
# How important were the variables used in the predictive model?
importance(model_forest) # examine the meandecreaseaccuracy
varImpPlot(model_forest)

# There is an issue with inconsistent data, use a substring of some of the variables for consistency
# Observe the installer variable
summary(train$installer)

# Make installer lowercase, take first 3 letters as a sub string
train$install_3 <- substr(tolower(train$installer),1,3)
train$install_3[train$install_3 %in% c(" ", "", "0", "_", "-")] <- "other"

# Take the top 15 substrings from above by occurance frequency
install_top_15 <- names(summary(as.factor(train$install_3)))[1:15]
train$install_3[!(train$install_3 %in% install_top_15)] <- "other"
train$install_3 <- as.factor(train$install_3)

# Table of the install_3 variable vs the status of the pumps
table(train$install_3, train$status_group)

# As row-wise proportions, install_3 vs status_group
prop.table(table(train$install_3, train$status_group), margin = 1)

# Create install_3 for the test set using same top 15 from above
test_values$install_3 <- substr(tolower(test_values$installer),1,3)
test_values$install_3[test_values$install_3 %in% c(" ", "", "0", "_", "-")] <- "other"
test_values$install_3[!(test_values$install_3 %in% install_top_15)] <- "other"
test_values$install_3 <- as.factor(test_values$install_3)

# Add install_3 variable to the random forest, redo the predictions, examine results
# randomForest and caret packages are pre-loaded
set.seed(42)
model_forest <- randomForest(as.factor(status_group) ~ longitude + latitude + extraction_type_group + quantity + waterpoint_type + construction_year + install_3,
                             data = train, importance = TRUE,
                             ntree = 5, nodesize = 2)

# Predict using the training values
pred_forest_train <- predict(model_forest, train)
importance(model_forest)
confusionMatrix(pred_forest_train, train$status_group)

# Predict using the test values
pred_forest_test <- predict(model_forest, test_values)

# Create submission data frame
submission <- data.frame(test_values$id)
submission$status_group <- pred_forest_test
names(submission)[1] <- "id"

# Output csv
setwd('C:/Users/Sean Ankenbruck/Desktop')
write.csv(submission, file= 'submission.csv')
