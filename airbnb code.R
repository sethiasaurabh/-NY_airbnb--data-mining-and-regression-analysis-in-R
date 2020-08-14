###########################################################################################

###Using libraries:

library(corrplot)
library(cowplot)
library(caret)
library(ggcorrplot)
library(stringr)
library(car)
library(broom)
library(callr)
library(carData)
library(dplyr)
library(ggplot2)

### Load dataset:

ab_nyc <- read.csv("D:\\Analytics\\AB_NYC_2019.csv", header = T, na.strings=c("","NA"))

### checking data:


### Dataset Information
print("***** Dimension *****")
dim(ab_nyc)

print("***** Column Names *****")
names(ab_nyc)  

print("***** Summary *****")
summary(ab_nyc)

print("***** Structure *****")
dplyr::glimpse(ab_nyc)

print("***** Sample *****")
dplyr::sample_n(ab_nyc, 4)

# Checking isNA coulumns
print("***** Null Column Count *****")
sapply(ab_nyc, function(x) sum(is.na(x)))

### Replace blank columns with zeros and other values in name, host_name, last review and reviews_per_month columns;

ab_nyc$reviews_per_month[is.na(ab_nyc$reviews_per_month)]<-"0"

ab_nyc$name[is.na(ab_nyc$name)]<-"NoName"

ab_nyc$last_review[is.na(ab_nyc$last_review)]<-"NotReviewed"

ab_nyc$host_name[is.na(ab_nyc$host_name)]<-"NoName"

###Again checking isNA coulumns
print("***** Null Column Count *****")
sapply(ab_nyc, function(x) sum(is.na(x)))

### adding leading zeroes in ID column to make id column cleaner;

ab_nyc$id <- str_pad(ab_nyc$id, 6, pad = "0")

head(ab_nyc)

distinct(ab_nyc, id)

###checking duplicate id's;

distinct(ab_nyc, id)

###checking some factors for more undertanding of data:

factor(ab_nyc$neighbourhood_group)   # 5 distinct groups

factor(ab_nyc$neighbourhood)         # 221 distinct neighbourhood

factor(ab_nyc$room_type)             # 4 room type levels

###Saving new updated file;

write.csv(ab_nyc,'ab_nyc_updated.csv')

### Cheking avg price to get some useful information;

summary(ab_nyc$price)

###########################################################################################

### Making EDA model:

###checking again some factors to make EDA model:

factor(ab_nyc$neighbourhood_group)   # 5 distinct groups

factor(ab_nyc$neighbourhood)         # 221 distinct neighbourhood

factor(ab_nyc$room_type)             # 4 room type levels


# Creating the 2 way contigency table, which is a useful way to represent the total counts of observations that
# fall into each combination of the levels of categorical variables:

table(ab_nyc$neighbourhood_group, ab_nyc$room_type)  #table 1

table(ab_nyc$neighbourhood_group, ab_nyc$room_type, ab_nyc$availability_365)  #table 2

### Create side-by-side barchart of room_type by alignment:

ggplot(ab_nyc, aes(x = room_type, colour = neighbourhood_group)) +       #### for table 1 created above
  geom_bar(position = "dodge")

ggplot(ab_nyc, aes(x =neighbourhood_group , colour = room_type)) +      #### for table 1 created above
  geom_bar(position = "dodge") 

ggplot(ab_nyc, aes(x = neighbourhood_group, y = availability_365,  color = room_type)) +    #### for table 2 created above
  geom_bar(stat = "identity") 

### Creating hist of rooms in affordable prices according to neighbourhood_group:

total_budget_rooms <- ab_nyc %>%
  filter(price < 200) %>%
  select(neighbourhood_group, price, room_type) 

head(total_budget_rooms)

ggplot(total_budget_rooms, aes(x = neighbourhood_group)) +
  geom_histogram(stat = 'count')  +
  ggtitle("Distribution of total affordable rooms according to neighnourhood_ group")


### check the proportion of above barcharts;

ggplot(ab_nyc, aes(x = room_type, colour = neighbourhood_group)) + 
  geom_bar(position = "fill") + ylab("proportion")

ggplot(ab_nyc, aes(x = neighbourhood_group, colour = room_type)) + 
  geom_bar(position = "fill") + ylab("proportion")


### Making conditional Bar-chart;

ggplot(ab_nyc, aes(x = neighbourhood_group)) + 
  geom_bar() +
  facet_wrap(~ room_type)

### Boxplot for checking outliers;

ab_nyc %>%
  ggplot(aes(x = 1, y = price)) +
  geom_boxplot()

ab_nyc_no_out <- ab_nyc %>%                  # filter data with no outliers and again make boxplot
  filter(price < 3000)

ab_nyc_no_out %>%
  ggplot(aes(x = 1, y = price)) +
  geom_boxplot()

### Checking the price outliers of individual neighbourhood_group;

ab_nyc_by_group <- ab_nyc %>%                  # filter data with groups and again make boxplot
  select(neighbourhood_group, price) %>%
  group_by(neighbourhood_group)%>%
  mutate(mean(price),
         median(price))

ab_nyc_by_group %>%
  ggplot(aes(x = neighbourhood_group, y = price)) +          #shows the price outliers of each neighbourhood_group
  geom_boxplot()


### Compute groupwise measures of spread (Standard Deviation and IQR);

groupwise_measures <- ab_nyc %>%
  group_by(neighbourhood_group) %>%
  mutate(sd(price),
         IQR(price),
         n())

### Generate overlaid density plots

groupwise_measures %>%
  ggplot(aes(x = price, fill = neighbourhood_group)) +
  geom_density(alpha = 0.3)


###########################################################################################

############# Generating linear model;


####### changing more data types to integers:

# to keep the actual date I'm going to make the data into number

ab_nyc$last_review <- as.integer(gsub("-", "", ab_nyc$last_review))

# set the missing values = 0

ab_nyc$last_review[is.na(ab_nyc$last_review)]<-"0"

#converting to integer and other column to factor;

ab_nyc$last_review <- as.integer(ab_nyc$last_review) 

ab_nyc$id <- as.integer(ab_nyc$id) 

ab_nyc$reviews_per_month <- as.integer(ab_nyc$reviews_per_month) 

glimpse(ab_nyc)

######Checking some correlation between some variables:

cor(ab_nyc$calculated_host_listings_count, ab_nyc$availability_365)

cor(ab_nyc$minimum_nights, ab_nyc$availability_365)

cor(ab_nyc$reviews_per_month, ab_nyc$calculated_host_listings_count)

#selecting variables for linear model:

corrr<- ab_nyc %>%
  select(price, latitude, longitude, host_id, 
         minimum_nights, number_of_reviews, reviews_per_month,
         calculated_host_listings_count, availability_365, last_review)

###Again checking isNA coulumns:
print("***** Null Column Count *****")
sapply(corrr, function(x) sum(is.na(x)))

#checking correlation matrix betwwen above selected variables;

cor(corrr)

round(cor.mtest(corrr)$p, 5)  #rounding the values

#Using corrplot library to visualize correlation matrix:

corrplot(cor(corrr), method = "number", type = "lower", bg = "grey",
         title = "Correlation Matrix")

### Making some single linear regression models;


lmmodel.1 <- lm (price ~ availability_365 * room_type, data = ab_nyc)

summary(lmmodel.1)

lmmodel.2 <- lm (price ~ neighbourhood_group, data = ab_nyc)

summary(lm.model.2)

lmmodel.3 <- lm (price ~ neighbourhood, data = ab_nyc)

summary(lmmodel.3)


linearmodel <- lm(price ~ number_of_reviews + availability_365, data = ab_nyc)
summary(linearmodel)

### Making linear model with highly correlated varibales including outliers with price before training
### and testing data;

linear.model <- lm(price ~ reviews_per_month +  minimum_nights + availability_365 + host_id
                   + last_review + calculated_host_listings_count + number_of_reviews
                   + latitude + longitude, data = corrr)
summary(linear.model)

step <- step(linear.model, direction = 'backward')  

### Try to make a training and testing regression model;

set.seed(12345)
TrainingIndex <- createDataPartition(corrr$price, p=0.75, list = FALSE)
TrainingSet <- corrr[TrainingIndex,] # Training Set
TestingSet <- corrr[-TrainingIndex,] # Test Set

# Build Training model
Model <- train(price ~ ., data = corrr,
               method = "lm",
               na.action = na.omit,
               preProcess=c("scale","center"),
               trControl= trainControl(method="none"))


# Apply model for prediction
Model.training <-predict(Model, TrainingSet) # Apply model to make prediction on Training set
Model.testing <-predict(Model, TestingSet) # Apply model to make prediction on Testing set


# Model performance (Displays scatter plot and performance metrics)
# Scatter plot of Training set
plot(TrainingSet$price,Model.training, col = "blue" )  #actual value
plot(TestingSet$price,Model.testing, col = "blue" )    #predicted value

#model performance:

summary(Model)

#calculate Pearson's correlation coefficient:

R.training <- cor(TrainingSet$price,Model.training)
R.testing <- cor(TestingSet$price,Model.testing)

R.training
R.testing

R2.training <- R.training^2
R2.testing <- R.testing^2

R2.training
R2.testing

## checking the trend of linear regression model;

ggplot(data = corrr, mapping = aes(x = reviews_per_month, y = price)) +
  geom_jitter() +
  stat_smooth(method = "lm", se = FALSE)

ggplot(data = corrr, mapping = aes(x = number_of_reviews, y = price)) +
  geom_jitter() +
  stat_smooth(method = "lm", se = FALSE)

####################################################################################


###Selected variables in last week to make initial linear model;

corrr<- ab_nyc %>%
  select(price, latitude, longitude, host_id, 
         minimum_nights, number_of_reviews, reviews_per_month,
         calculated_host_listings_count, availability_365, last_review)

glimpse(corrr)

glimpse(ab_nyc)

#checking correlation matrix betwwen above selected variables;

cor(corrr)

#Using corrplot library to visualize correlation matrix:

corrplot(cor(corrr), method = "number", type = "lower", bg = "grey",
         title = "Correlation Matrix")

### Making linear model with highly correlated varibales including outliers with price before training
### and testing data;

linear.model <- lm(price ~ reviews_per_month +  minimum_nights + availability_365 + host_id
                   + last_review + calculated_host_listings_count + number_of_reviews
                   + latitude + longitude, data = corrr)
summary(linear.model)

### Model Comparision;

MSE.0 <- mean(linear.model$residuals^2)

### Try to make a training and testing regression model;

set.seed(12345)
TrainingIndex <- createDataPartition(corrr$price, p=0.75, list = FALSE)
TrainingSet <- corrr[TrainingIndex,] # Training Set
TestingSet <- corrr[-TrainingIndex,] # Test Set

# Build Training model
Model.1 <- train(price ~ ., data = corrr,
                 method = "lm",
                 na.action = na.omit,
                 preProcess=c("scale","center"),
                 trControl= trainControl(method="none"))

#model performance:

summary(Model.1)

###showing the predicted values;

prediction.1 <- predict(Model.1, corrr)

predicted_df.1 <- data.frame(actual_values = corrr$price, predicted_values = prediction.1)

head(predicted_df.1)

# Apply model for prediction
Model.training <-predict(Model.1, TrainingSet) # Apply model to make prediction on Training set
Model.testing <-predict(Model.1, TestingSet) # Apply model to make prediction on Testing set


# Model performance (Displays scatter plot and performance metrics)
# Scatter plot of Training set
plot(TrainingSet$price,Model.training, col = "blue" )  #actual value
plot(TestingSet$price,Model.testing, col = "blue" )    #predicted value


#calculate Pearson's correlation coefficient:

R.training <- cor(TrainingSet$price,Model.training)
R.testing <- cor(TestingSet$price,Model.testing)

R.training
R.testing

R2.training <- R.training^2
R2.testing <- R.testing^2

R2.training
R2.testing

## checking the trend of linear regression model;

ggplot(data = corrr, mapping = aes(x = reviews_per_month, y = price)) +
  geom_jitter() +
  stat_smooth(method = "lm", se = FALSE)

ggplot(data = corrr, mapping = aes(x = number_of_reviews, y = price)) +
  geom_jitter() +
  stat_smooth(method = "lm", se = FALSE)

###VIF or model validation

vif0 <- lm(price ~., data = corrr); summary(corrr)

vif(vif0)



###########  new dataframe ###########################################################################

corrr.1<- ab_nyc %>%
  select(price, latitude, longitude, host_id, 
         minimum_nights, number_of_reviews, reviews_per_month,
         calculated_host_listings_count, availability_365, last_review, id, neighbourhood_group,
         neighbourhood, room_type)

###converting columns to factor then to integer;

columns <- c("neighbourhood_group","neighbourhood","room_type")
corrr.1[, columns] <- corrr.1 %>% select(all_of(columns)) %>% lapply(as.factor)
# check to make sure it worked
corrr.1 %>% select(all_of(columns)) %>% str()

cols = c("neighbourhood_group", "neighbourhood", "room_type")
corrr.1[, cols] <- corrr.1 %>% select(all_of(cols)) %>% lapply(as.numeric)

glimpse(corrr.1)

###Checking Correlation;

cor(corrr.1)

corrplot(cor(corrr.1), method = "number", type = "lower", bg = "grey",              #Ploting graph
         title = "Correlation Matrix")

###Making linear model;

model.2 <- lm(price ~ ., data = corrr.1)

summary(model.2)

###showing the predicted values;

prediction.2 <- predict(model.2, corrr.1)

predicted_df.2 <- data.frame(actual_values = corrr.1$price, predicted_values = prediction.2)

head(predicted_df.2)

###selecting selection method;

step0 <- step(model.2, direction = 'backward')              ##backward selection

### Model Comparision;


cat("Is the MSE LESS THAN the initial model 1? ",
    mean(linear.model$residuals^2) > mean(model.2$residuals^2), "\n");

MSE.0 <- mean(linear.model$residuals^2)
MSE.1 <- mean(model.2$residuals^2)


###VIF or model validation

vif1 <- lm(price ~., data = corrr.1); summary(corrr.1)
vif(vif1)


################### Remove outliers and applying selection method;###################

Q <- quantile(corrr.1$price, probs=c(.25, .75), na.rm = T)
iqr <- IQR(corrr.1$price, na.rm = T)
corrr.2 <- corrr.1 %>% filter(price > (Q[1] - 1.5*iqr) &     # New dataframe
                                price < (Q[2] + 1.5*iqr))


###Checking Correlation;

cor(corrr.2)

round(cor.mtest(corrr.2)$p, 5)

corrplot(cor(corrr.2), method = "number", type = "lower", bg = "grey",              #Plot
         title = "Correlation Matrix")

###Making linear model;

model.3 <- lm(price ~ ., data = corrr.2)

summary(model.3)

###showing the predicted values;

prediction.3 <- predict(model.3, corrr.2)

predicted_df.3 <- data.frame(actual_values = corrr.2$price, predicted_values = prediction.3)

head(predicted_df.3)

######selecting selection method;

step1 <- step(model.3, direction = 'backward')            ##backward selection
step1

### Model Comparision;

cat("Is the MSE LESS THAN the first model 2? ",
    mean(model.2$residuals^2) > mean(model.3$residuals^2), "\n");

MSE.1 <- mean(model.2$residuals^2)
MSE.2 <- mean(model.3$residuals^2)

###VIF or model validation

vif2 <- lm(price ~., data = corrr.2); summary(corrr.2)
vif(vif2)


##### trying new model after removing non-relevant variable get to know through backward selection #######

corrr.3<- ab_nyc %>%
  select(price, latitude, longitude, 
         minimum_nights, number_of_reviews,
         calculated_host_listings_count, availability_365, last_review, neighbourhood_group,
         neighbourhood, room_type)

corrr.3<- ab_nyc %>%
  select(price, latitude, 
         calculated_host_listings_count, availability_365, neighbourhood_group,
         neighbourhood)


###converting columns to factor then to integer;

columns <- c("neighbourhood_group","neighbourhood")
corrr.3[, columns] <- corrr.3 %>% select(all_of(columns)) %>% lapply(as.factor)
# check to make sure it worked
corrr.3 %>% select(all_of(columns)) %>% str()

cols = c("neighbourhood_group", "neighbourhood", "room_type")
corrr.3[, cols] <- corrr.3 %>% select(all_of(cols)) %>% lapply(as.numeric)

glimpse(corrr.3)

### Remove outliers

Q <- quantile(corrr.3$price, probs=c(.25, .75), na.rm = T)
iqr <- IQR(corrr.3$price, na.rm = T)
df <- corrr.3 %>% filter(price > (Q[1] - 1.5*iqr) & 
                           price < (Q[2] + 1.5*iqr))

### linear model

model.4 <- lm(price ~ ., data = df)

summary(model.4)


###showing the predicted values;

prediction.4 <- predict(model.4, df)

predicted_df.4 <- data.frame(actual_values = df$price, predicted_values = prediction)

head(predicted_df.4)


######selecting selection method;

step1 <- step(model.3, direction = 'backward')            ##backward selection

### Model Comparision;

cat("Is the MSE LESS THAN the first model? ",
    mean(model.3$residuals^2) > mean(model.4$residuals^2), "\n");

MSE.2 <- mean(model.3$residuals^2)
MSE.3 <- mean(model.4$residuals^2)

###VIF or model validation

vif3 <- lm(price ~., data = corrr.3); summary(corrr.3)
vif(vif3)
