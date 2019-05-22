#Clean the environment
rm(list = ls())

#Set working directory
setwd("Z:/Edwisor Project 2/Employee Absenteeism")

#Load the librarires
libraries = c("ggplot2","dummies","caret","rpart.plot","plyr","dplyr","rpart","DMwR","randomForest","usdm","corrgram","DataCombine","caret","ISLR")
lapply(X = libraries,require, character.only = TRUE)
rm(libraries)

#Read the csv file
emp_absent = read.csv(file = "Absenteeism_at_work_Project.csv", header = T)

#Check number of rows and columns
dim(emp_absent)

#Observe top 5 rows
head(emp_absent)

#Structure of variables
str(emp_absent)

#Transform data types
emp_absent$ï..ID = as.factor(as.character(emp_absent$ï..ID))
emp_absent$Reason.for.absence[emp_absent$Reason.for.absence %in% 0] = 20
emp_absent$Reason.for.absence = as.factor(as.character(emp_absent$Reason.for.absence))
emp_absent$Month.of.absence[emp_absent$Month.of.absence %in% 0] = NA
emp_absent$Month.of.absence = as.factor(as.character(emp_absent$Month.of.absence))
emp_absent$Day.of.the.week = as.factor(as.character(emp_absent$Day.of.the.week))
emp_absent$Seasons = as.factor(as.character(emp_absent$Seasons))
emp_absent$Disciplinary.failure = as.factor(as.character(emp_absent$Disciplinary.failure))
emp_absent$Education = as.factor(as.character(emp_absent$Education))
emp_absent$Son = as.factor(as.character(emp_absent$Son))
emp_absent$Social.drinker = as.factor(as.character(emp_absent$Social.drinker))
emp_absent$Social.smoker = as.factor(as.character(emp_absent$Social.smoker))
emp_absent$Pet = as.factor(as.character(emp_absent$Pet))

#Structure of variables
str(emp_absent)

#Make a copy of data
df = emp_absent

#Get number of missing values
sapply(df,function(x){sum(is.na(x))})
missing_values = data.frame(sapply(df,function(x){sum(is.na(x))}))

#Get the rownames as new column
missing_values$Variables = row.names(missing_values)

#Reset the row names 
row.names(missing_values) = NULL

#Rename the column
names(missing_values)[1] = "Miss_perc"

#Calculate missing percentage
missing_values$Miss_perc = ((missing_values$Miss_perc/nrow(emp_absent)) *100)

#Reorder the columns
missing_values = missing_values[,c(2,1)]

#Sort the rows according to decreasing missing percentage
missing_values = missing_values[order(-missing_values$Miss_perc),]

#Create a bar plot to visualie top 5 missing values
ggplot(data = missing_values[1:20,], aes(x=reorder(Variables, -Miss_perc),y = Miss_perc))+
  geom_bar(stat = "identity",fill = "grey")+xlab("Parameter")+
  ggtitle("Missing data percentage") + theme_bw()

#Used the copied dataset to predict the value of K.
df = knnImputation(data = df, k = 5)
train_control <- trainControl(method = "repeatedcv", repeats = 30)
knnFit <- train(Absenteeism.time.in.hours ~ ., data = df, method = "knn", trControl = train_control, tuneLength = 10)
knnFit
#Plotting yields Number of Neighbours Vs accuracy (based on repeated cross validation)
plot(knnFit)

emp_absent = knnImputation(data = emp_absent, k = 23)
df=emp_absent
#Check if any missing values
sum(is.na(df))

# Saving output result into excel file
write.csv(missing_values, "Missing_perc_R.csv", row.names = F)
#Get the data with only numeric columns
numeric_index = sapply(df, is.numeric)
numeric_data = df[,numeric_index]
#Get the names of numeric columns
numeric_columns = colnames(numeric_data)


#Distribution of factor data using bar plot
###################

bar1 = ggplot(data = df, aes(x = ID)) + geom_bar() + ggtitle("Count of ID") + theme_bw()
bar2 = ggplot(data = df, aes(x = Reason.for.absence)) + geom_bar() + 
  ggtitle("Count of Reason for absence") + theme_bw()
bar3 = ggplot(data = df, aes(x = Month.of.absence)) + geom_bar() + ggtitle("Count of Month") + theme_bw()
bar4 = ggplot(data = df, aes(x = Disciplinary.failure)) + geom_bar() + 
  ggtitle("Count of Disciplinary failure") + theme_bw()
bar5 = ggplot(data = df, aes(x = Education)) + geom_bar() + ggtitle("Count of Education") + theme_bw()
bar6 = ggplot(data = df, aes(x = Son)) + geom_bar() + ggtitle("Count of Son") + theme_bw()
bar7 = ggplot(data = df, aes(x = Social.smoker)) + geom_bar() + 
  ggtitle("Count of Social smoker") + theme_bw()

gridExtra::grid.arrange(bar2,bar3,bar4,ncol=2)
gridExtra::grid.arrange(bar5,bar6,bar7,ncol=2)



#Check the distribution of numerical data using histogram
########################

hist1 = ggplot(data = numeric_data, aes(x =Transportation.expense)) + 
  ggtitle("Transportation.expense") + geom_histogram(bins = 25)
hist2 = ggplot(data = numeric_data, aes(x =Height)) + 
  ggtitle("Distribution of Height") + geom_histogram(bins = 25)
hist3 = ggplot(data = numeric_data, aes(x =Body.mass.index)) + 
  ggtitle("Distribution of Body.mass.index") + geom_histogram(bins = 25)
hist4 = ggplot(data = numeric_data, aes(x =Absenteeism.time.in.hours)) + 
  ggtitle("Distribution of Absenteeism.time.in.hours") + geom_histogram(bins = 25)

gridExtra::grid.arrange(hist1,hist2,hist3,hist4,ncol=2)



#Check for outliers using boxplots
for(i in 1:ncol(numeric_data)) {
  assign(paste0("box",i), ggplot(data = df, aes_string(y = numeric_data[,i])) +
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour = "red", fill = "grey", outlier.size = 1) +
           labs(y = colnames(numeric_data[i])) +
           ggtitle(paste("Boxplot: ",colnames(numeric_data[i]))))
}
#Arrange the plots in grids
gridExtra::grid.arrange(box1,box2,box3,box4,box5,box6,box7,box8,box9, ncol=4)
#Replace all outlier data with NA
for(i in numeric_columns){
  val = df[,i][df[,i] %in% boxplot.stats(df[,i])$out]
  print(paste(i,length(val)))
  df[,i][df[,i] %in% val] = NA
}
#Check number of missing values
sapply(df,function(x){sum(is.na(x))})
#Get number of missing values after replacing outliers as NA
missing_values_out = data.frame(sapply(df,function(x){sum(is.na(x))}))
missing_values_out$Columns = row.names(missing_values_out)
row.names(missing_values_out) = NULL
names(missing_values_out)[1] = "Miss_perc"
missing_values_out$Miss_perc = ((missing_values_out$Miss_perc/nrow(emp_absent)) *100)
missing_values_out = missing_values_out[,c(2,1)]
missing_values_out = missing_values_out[order(-missing_values_out$Miss_perc),]
missing_values_out
#Compute the NA values using KNN imputation
train_control <- trainControl(method = "repeatedcv", repeats = 30)
knnFit <- train(Absenteeism.time.in.hours ~ ., data = df, 
                method = "knn", trControl = train_control, tuneLength = 10)
knnFit
#Use plots to see optimal number of clusters:
#Plotting yields Number of Neighbours Vs accuracy (based on repeated cross validation)
plot(knnFit)
df = knnImputation(df, k = 23)
#Check if any missing values
sum(is.na(df))
emp_absent = df
#Check for multicollinearity using VIF
vifcor(numeric_data)
#Check for multicollinearity using corelation graph
corrgram(numeric_data, order = F, upper.panel=panel.pie, 
         text.panel=panel.txt, main = "Correlation Plot")

#Variable Reduction
df = subset.data.frame(df, select = -c(Weight))

#Make a copy of Clean Data
clean_data = df

#Normality check for target variable
hist(df$Absenteeism.time.in.hours)

#Remove dependent variable
numeric_index = sapply(df,is.numeric)
numeric_data = df[,numeric_index]
numeric_columns = names(numeric_data)
i<-grep("Absenteeism.time.in.hours", colnames(df))
numeric_columns = numeric_columns[-i]
#Normalization
for(i in numeric_columns){
  print(i)
  df[,i] = (df[,i] - min(df[,i]))/
    (max(df[,i]) - min(df[,i]))
}

#Get the data with only factor columns
factor_data = df[,!numeric_index]

#Get the names of factor variables
factor_columns = names(factor_data)

#Create dummy variables of factor variables
df = dummy.data.frame(df, factor_columns)

rmExcept(keepers = c("df","emp_absent"))

#Setting Train and Test Data
set.seed(1)
train_index = sample(1:nrow(df), 0.8*nrow(df))        
train = df[train_index,]
test = df[-train_index,]

#Decision Tree
dt_model = rpart(Absenteeism.time.in.hours ~ ., data = train, method = "anova")

#Plot the tree
rpart.plot(dt_model)

#Perdict for test cases
i<-grep("Absenteeism.time.in.hours", colnames(df))
dt_predictions = predict(dt_model, test[,-i])

#Create data frame for actual and predicted values
df_pred = data.frame("actual"=test[,i],"dt_pred"=dt_predictions)
head(df_pred)

#Calcuate MAE, RMSE, R-sqaured for testing data 
print(postResample(pred = dt_predictions, obs = test[,i]))

#Plot a graph for actual vs predicted values
plot(test$Absenteeism.time.in.hours,type="l",lty=2,col="green")
lines(dt_predictions,col="blue")

#Build Random Forest
i<-grep("Absenteeism.time.in.hours", colnames(df))
x<- df[,-i]
y<- df[,i]
#Hypertuning choosing the best mtree
set.seed(1)
bestMtry <- tuneRF(x,y, stepFactor = 1.5, improve = 1e-5, ntree = 500)
names(train) <- make.names(names(train))
##Train the model using training data
rf_model = randomForest(Absenteeism.time.in.hours ~ .,data = train, ntree = 500, mtree=11)
names(test) <- make.names(names(test))
#Predict the test cases
i<-grep("Absenteeism.time.in.hours", colnames(test))
rf_predictions = predict(rf_model, test[,-i])

#Create dataframe for actual and predicted values
df_pred = cbind(df_pred,rf_predictions)
head(df_pred)

#Calcuate MAE, RMSE, R-sqaured for testing data 
print(postResample(pred = rf_predictions, obs = test[,i]))

#Plot a graph for actual vs predicted values
plot(test$Absenteeism.time.in.hours,type="l",lty=2,col="green")
lines(rf_predictions,col="blue")


#Linear Regression
#Train the model using training data
lr_model = lm(Absenteeism.time.in.hours ~ ., data = train)
#Get the summary of the model
summary(lr_model)
#Predict the test cases
lr_predictions = predict(lr_model,test)
#Create dataframe for actual and predicted values
df_pred = cbind(df_pred,lr_predictions)
head(df_pred)
#Calcuate MAE, RMSE, R-sqaured for testing data 
print(postResample(pred = lr_predictions, obs =test$Absenteeism.time.in.hours))
#Plot a graph for actual vs predicted values
plot(test$Absenteeism.time.in.hours,type="l",lty=2,col="green")
lines(lr_predictions,col="blue")

#PCA
prin_comp = prcomp(train)
pr_stdev = prin_comp$sdev
#Compute variance
pr_var = pr_stdev^2
#Proportion of variance explained
prop_var = pr_var/sum(pr_var)
plot(cumsum(prop_var), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")
#Add a training set with principal components
train.data = data.frame(Absenteeism.time.in.hours = train$Absenteeism.time.in.hours, prin_comp$x)
# From the above plot selecting 80 components since it explains almost 95+ % data variance
train.data =train[,1:80]
i<-grep("Absenteeism.time.in.hours", colnames(train))
rbind(train.data, train[,i])

#Transform test data into PCA
test.data = predict(prin_comp, newdata = test)
cbind(test$Absenteeism.time.in.hours,test.data)
test.data = as.data.frame(test.data)

#Select the first 80 components
test.data=test[,1:80]

#Decision Tree using rpart

dt_model = rpart(Absenteeism.time.in.hours ~ ., data = train.data, method = "anova")

#Plot the tree
rpart.plot(dt_model)

#Perdict for test cases
i<-grep("Absenteeism.time.in.hours", colnames(test.data))
dt_predictions = predict(dt_model, test.data[,-i])

#Create data frame for actual and predicted values
df_pred = data.frame("actual"=test[,i],"dt_pred"=dt_predictions)
head(df_pred)

#Calcuate MAE, RMSE, R-sqaured for testing data 
print(postResample(pred = dt_predictions, obs = test[,i]))

#Plot a graph for actual vs predicted values
plot(test$Absenteeism.time.in.hours,type="l",lty=2,col="green")
lines(dt_predictions,col="blue")

#Build Random Forest
i<-grep("Absenteeism.time.in.hours", colnames(train.data))
x<- df[,-i]
y<- df[,i]
set.seed(1)
#Choosing the best mtree
bestMtry <- tuneRF(x,y, stepFactor = 1.5, improve = 1e-5, ntree = 500)
##Train the model using training data
names(train.data) <- make.names(names(train.data))
rf_model = randomForest(Absenteeism.time.in.hours ~ .,data = train.data, ntree = 500, mtree=6)
names(test.data) <- make.names(names(test.data))
#Predict the test cases
i<-grep("Absenteeism.time.in.hours", colnames(test))
rf_predictions = predict(rf_model, test[,-i])
#Create dataframe for actual and predicted values
df_pred = cbind(df_pred,rf_predictions)
head(df_pred)
#Calcuate MAE, RMSE, R-sqaured for testing data 
print(postResample(pred = rf_predictions, obs = test[,i]))
#Plot a graph for actual vs predicted values
plot(test$Absenteeism.time.in.hours,type="l",lty=2,col="green")
lines(rf_predictions,col="blue")

##Linear Regression
#Train the model using training data
lr_model = lm(Absenteeism.time.in.hours ~ ., data = train.data)
#Get the summary of the model
summary(lr_model)
#Predict the test cases
lr_predictions = predict(lr_model,test.data)
#Create dataframe for actual and predicted values
df_pred = cbind(df_pred,lr_predictions)
head(df_pred)
#Calcuate MAE, RMSE, R-sqaured for testing data 
print(postResample(pred = lr_predictions, obs =test.data$Absenteeism.time.in.hours))
#Plot a graph for actual vs predicted values
plot(test$Absenteeism.time.in.hours,type="l",lty=2,col="green")
lines(lr_predictions,col="blue")