#Notes #########
# Author: Joseph A. Stone
# Date: 15/10/2023
# Tips: 
  #Turn on "soft wrap of long lines" in the "code" tab above to easily read comments
  #Open the "document outline" to see workflow and understand headings/subheadings using the button in the top right of this panel
# To Run: 
  # Change the first variable "folder_with_csvs" to the folder containing the 4 provided csv's
#IMPORTANT note regarding creation:
  # This was written in R as I am currently in Europe travelling to Norway to meet a professor. Therefore, as I was unable to access a computer during the required timeframe, this code was written on my iphone which connected to an R server I set up using Saturn cloud... 
#Understandably, I was very limited in the visualisations I could produce as it is difficult coding using an iphone. Additionally, although I can code in both Python and R, I decided to use R as the Rstudio IDE is more mobile friendly, the set up was easier, and I'm a more fluent in R statistics packages I'm anticipating will be required for this project.

# 1. Set up ####
## a. set wd ====
folder_with_csvs <- "/home/jovyan/workspace/indebted_takehome_exercise/"
## b. packages ====
install.packages("ISLR")
install.packages("tree")
library("tidyverse")
install.packages("GGally")
library("GGally")
library("lubridate")
library("hms")
install.packages("caTools")
library("caTools")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
library(ISLR)
library(tree)


## c. data ====
clients <- read.csv(paste0(folder_with_csvs,"clients.csv"))
customers <- read.csv(paste0(folder_with_csvs,"customers.csv"))
messages <- read.csv(paste0(folder_with_csvs,"messages.csv"))
sample_customer_ids <- read.csv(paste0(folder_with_csvs,"sample_customer_ids.csv"))

# 2. clean Data - customers table ####
## a. Add flag for customers requiring prediction ====
sample_customer_ids["validation"]<-TRUE
customers <- left_join(customers,sample_customer_ids,by=c("id"="customer_id"))

## b. Joining data to fix country column based on provided assumption ====
customers <- left_join(customers,clients, by=c("client_id"="id"),suffix=c("","_clienttable"))
customers[customers[["country"]]=="","country"] <- customers[customers[["country"]]=="","country_clienttable"]

## c. Fill missing data ====
### i. check for missing in terms of both NA and blank values ----
sapply(customers, function(x) sum(is.na(x))) 
apply(customers, 2, function(x) sum(x == ""))
### ii. gender (2021 missing), add "missing" as categorical and shouldnt matter ----
customers[customers[,"gender"]=="","gender"] <- "missing"

### iii. age (1425), assign 100 value (higher than max) for now, can remove later ----
customers[is.na(customers[,"age"]),"age"] <- 100

## d. fix date column type ====
customers["created_at"]<-  as.POSIXct(customers[,"created_at"])
customers["created_day"] <- wday(as.Date(customers$created_at),label=TRUE)
customers["created_time"] <- as_hms(customers[,"created_at"])

#3. Clean Data - messages table ####
sapply(messages, function(x) sum(is.na(x))) 
apply(messages, 2, function(x) sum(x == ""))

## a. extract day/time ====
messages["sent_at"]<-  as.POSIXct(messages[,"sent_at"])
messages["sent_day"] <- wday(as.Date(messages$sent_at),label=TRUE)
messages["sent_time"] <- as_hms(messages[,"sent_at"])

## b. convert responses to boolean
messages["converted"]<-as.logical(messages[,"converted"])
messages["clicked"]<-as.logical(messages[,"clicked"])

#4. Quick exploration of data####
## a. summary of messages table ====
sum(messages$clicked)/nrow(messages)
sum(messages$converted)/nrow(messages)
# ... that's rough not many people reply

## b. visualise data ----
provided_sample <- left_join(messages,customers,by=c("customer_id"="id"))
ggpairs(provided_sample[provided_sample["converted"]==TRUE,
        c("converted","sent_day")],
        aes(colour = converted))
ggpairs(provided_sample[provided_sample["converted"]==TRUE, c("converted","clicked","sent_time")],
        aes(colour = converted))
# aaah so in the data provided which I assumed was "sample data", you've provided messages which aren't being randomly sent throughout the day. You're already tending to send them before midday and not on monday... So any model I make with this data will suggest to not send on monday and to send before midday, and will bias towards a specific time. I should probably have a stratified sample which has an even number of messages throughout the day and week.

#5. Obtain a sample of the messages ####
## Remove the validation data (preventing data leakage) ====
provided_sample <- provided_sample[(is.na(provided_sample[,"validation"])),]

## Round to 15 mins ====
#lets assume that the specific minute you send the message doesnt matter and time can be subsetted into 15min increments
provided_sample["sent_time_rounded"] <- round_date(as.POSIXct(provided_sample[,"sent_time"]),
                                         unit="15 mins")

## Find sample for each 15mins ====
min(table(provided_sample["sent_time_rounded"])) #13764 is lowest in weekday
set.seed(1)
stratified_sample <- provided_sample %>%
  group_by(sent_time_rounded) %>%
  sample_n(382)
## Find sample for each day ====
table(stratified_sample["sent_day"]) #196
stratified_sample <- stratified_sample %>%
  group_by(sent_day) %>%
  sample_n(2755)

## Quickly fix data types =====
stratified_sample$converted <- as.factor(stratified_sample$converted)
stratified_sample$sent_day <- factor(stratified_sample$sent_day, ordered = FALSE)
stratified_sample$gender <- as.factor(stratified_sample$gender)
stratified_sample$country <- as.factor(stratified_sample$country)
stratified_sample$product_type <- as.factor(stratified_sample$product_type)
stratified_sample$created_day <- factor(stratified_sample$created_day, ordered = FALSE )

# 6. Split into test/train (80/20) ####
temp_var <- sample.split(stratified_sample$id, SplitRatio = .8)
train = subset(stratified_sample, temp_var == TRUE)
test  = subset(stratified_sample, temp_var == FALSE)

## Check of the spread of levels throughout test/train ====
table(train["sent_day"]) 
table(train["sent_time_rounded"])
table(train["converted"]) 
#not many TRUE values for converted (response variable) made it through... different representation of classes usually requires adjusting the weight argument during model fitting
table(test["sent_day"]) 
table(test["sent_time_rounded"])
table(test["converted"])

## Quickly make filter variables in dataset to list the predictor variables and response ====
predictors <- c("sent_day",
                "sent_time",
                "gender",
                "country",
                "age",
                "product_type",
                "country_clienttable",
                "created_day",
                "created_time",
                "sent_time_rounded",
                "converted")

# 7. Fit models ####
# more complex methods are ruled out as our intention is not to come up with an accurate classification/prediction, but to create a model which is understandable so we can make a recommendation for the time/day to send a message. Therefore, more complex models such as ensemble algorithms, randomforest algorithms, or even the fun timeseries/ensemble timeseries models (outlined in papers such as https://iniria.hal.science/hal-03558165/document) cant be used.
#Instead I'm tending to lean towards more simple models such as decision trees, logistic regression model, naive bayes, or KNN. Again I'd say that logistic regression/bayes would be less useful for recommendation do the the nature of the model (probablistic does not provide distinction for different predictor categories)
#Just took a quick read of a similar journal article (cited below) and they confirmed exactely what I guessed above (they even tested RNN and it wasn't as accurate). KNN and decision trees performed the best on similar data. For this project I could attempt both, but it makes more sense to stick to a single decision tree
#citation: "Araújo, C., Soares, C., Pereira, I., Coelho, D., Rebelo, M. Â., & Madureira, A. (2022). A Novel Approach for Send Time Prediction on Email Marketing. Applied Sciences, 12(16), 8310–. https://doi.org/10.3390/app12168310"
train <- train[,predictors]
test <- test[,predictors]

## Decision tree ====
#Going into this, it's unlikely it will work in this case as there's a large class imbalance. I could look at using the wts argument to create a more accurate model, or undersampling. I've been told to avoid undersampling though by professors in the past. 

tree.model1 <- tree::tree(converted~.,train)
summary(tree.model1)
plot(tree.model1)
text(tree.model1, pretty=0)
#so the model just classified everything as false and only used the product_type and country predictors. I could try adjusting the wts argument to adjust to balancing of classes. But in my experience doing that with randomforest algorithms it's fiddly and doesn't work well. Instead I'm going to shift to the rpart package so I can force a more complex model which overfits the training data. Then reassess from there, possibly they have a trim function based o0n misclass using 10-fold CV which will allow me to trim the overfitted tree.

tree2<- rpart(converted~.,
                        data=train,
                        cp=0)
rpart.plot::rpart.plot(tree2) 
#I can't see the plot on my phone, but I've clearly forced a more complex DT that identifies some relationships but is almost certainly overfitting
summary(tree2)

## note ====
#usually I'd "prune or tune" the tree to reduce the chance of overfitting using something similar to the process below. However, the result of pruning is just a single root node as we have to force this complexity by chanigng the cp parameter. additionally, from what i can interpret from the model summary (again cant really see the visualisations on my phone) the lower nodes contain the times and days splitting observations,. so tuning the cp argument will simply lower model complexity and result in a model which does not use the time or day to make a prediction. i could try changing the predictors selected, but wont for these purposes.Therefore, I'm simply going to check the error rate of the overfitted model on the test data as it is the best we have at the moment (and I dont have the resources right now to trouble shoot) 
#ptree <- prune(tree2, cp=tree2$cptable[which.min(tree2$cptable[,"xerror"]),"CP"])
#rpart.plot(ptree) 

## test accuracy ====
table(predict(tree2,train,type="vector"))
table(predict(tree2,test,type="vector"))
#Although this is not a confusion matrix and is a lazy representation, the results indicate that the model is ineffective at predicting the "2" values in both the training and test sample. This behaviour was expected due to the class imbalance noted initially. However, I'm going to stick with this model for the purposes of this assessment due to time constraints. 

#8. predictions and post process ####

#we can see from the summary that only two terminal nodes were classifying as converted and there were misclassifications in the training data
"Node number 243: 8 observations "
"Node number 7655: 7 observations"

rpart.rules(tree2)
#this shows the rules for the two terminal nodes we care about. i cant copy ans pate this on my phone, bur the rules we care about are essentially that the time is between 58088 and 64763 and the day iswed-fri for the first set of rules. for the second set of rules the time is 39294 to39628 for the second set of rules. The logic i would use as the rules filter out the shared rules to find which set is more specific. i believe that means the last set with age >=19 is the more specific, meaning the second time frame should be applied to observatipns with an age less  than 25.

final <- left_join(sample_customer_ids,customers,by=c("customer_id"="id"))
final["start_day"] <- "Wednesday"
final["end_day"] <- "friday"

final["start_time"] <- new_hms(58088)
final[final["age"]<25,"start_time"] <- new_hms(39294)
final["end_time"] <- new_hms(39628)
final[final["age"]>=25,"end_time"] <- new_hms(64763)
final[final["age"]>=25&final["country"]=="CA","end_time"] <- new_hms(61200)





      
