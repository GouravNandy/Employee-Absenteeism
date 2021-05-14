rm(list= ls())
setwd("E:/My career/Data Science/EDwisor/Projects/Employee Absentism")
getwd()

library(rpart)

library(readxl)
df <- read_excel("Absenteeism_at_work_Project.xls", 
                 sheet = "Absenteeism_at_work")
View(df)
summary(df)

df$`Reason for absence`[df$`Reason for absence`== 0] <-NA
df$`Month of absence`[df$`Month of absence`== 0] <-NA

#****** Datatype Conversion****** 

str(df)
df$ID = as.factor(df$ID)
df$`Reason for absence` = as.factor(df$`Reason for absence`)
df$`Month of absence` = as.factor(df$`Month of absence`)
df$`Day of the week`=as.factor(df$`Day of the week`)
df$Seasons = as.factor(df$Seasons)
df$`Disciplinary failure`=as.factor(df$`Disciplinary failure`)
df$Education=as.factor(df$Education)
df$Son = as.factor(df$Son)
df$`Social drinker`=as.factor(df$`Social drinker`)
df$`Social smoker`=as.factor(df$`Social smoker`)
df$Pet = as.factor(df$Pet)


#*******Missing Value Analysis

colSums(is.na(df))
total_msv = sum(colSums(is.na(df)))
total_value = nrow(df)*ncol(df)
percentage_msv = (total_msv/total_value)*100.00
print(percentage_msv)

library("VIM")
df = kNN(df, k=3, imp_var = FALSE)
total_msv = sum(colSums(is.na(df)))
total_value = nrow(df)*ncol(df)
percentage_msv = (total_msv/total_value)*100.00
print(percentage_msv)


#Outlier Analys
numeric_index = sapply(df, is.numeric)
numeric_data = df[,numeric_index]
numeric_col = colnames(numeric_data)
factor_data = df[,!numeric_index]
factor_col = colnames(factor_data)


for(i in numeric_col){
  val = df[,i][df[,i] %in% boxplot.stats(df[,i])$out]
  print(paste(i,length(val)))
  df[,i][df[,i] %in% val] = NA
}


df = kNN(df, k=3, imp_var = FALSE)
View(df)


#Multicolinearity

library("corrgram")
corrgram(df[,numeric_col], order = F,upper.panel=panel.fill, text.panel=panel.txt, main = "Correlation Plot")
df$`Body mass index`=NULL
View(df)


#Feature Scaling
#Standardization

numeric_col = numeric_col[!numeric_col %in% 'Body mass index'] #Removing colinear variable 
numeric_col = numeric_col[-9]# dropping dependent variable
for(i in numeric_col){
  df[,i] = (df[,i] - mean(df[,i]))/
    sd(df[,i])
}
    
     
View(df)


#Feature Engineering


library('data.table')
library('mltools')

df = one_hot(as.data.table(df))
df = as.data.frame(df)


View(df)
str(df)
#********* Splitting data**********
library("caret")
set.seed(1234)
train.index = createDataPartition(df$`Absenteeism time in hours`, p = .80, list = FALSE)
train = df[ train.index,]
test  = df[-train.index,]

#********* Dimension reduction *******


prin_comp = prcomp(train)
pr_stdev = prin_comp$sdev
pr_var = pr_stdev^2
prop_var = pr_var/sum(pr_var)

#Add a training set with principal components
train.data = data.frame(Absenteeism_time_in_hours = train$`Absenteeism time in hours`, prin_comp$x)

plot(cumsum(prop_var), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

# From the above plot selecting 52 components since it explains almost 97+ % data variance
train.data =train.data[,1:52]

#Transform test data into PCA
test.data = predict(prin_comp, newdata = test)
test.data = data.frame(test.data)

#Select the first 52 components
test.data=test.data[,1:52]
#********* Model Development ******
#Linear Regression
print('Linear Regression')
lr_model = lm(train.data$Absenteeism_time_in_hours~. , data = train.data)
lr_predictions = predict(lr_model, test.data)
df_pred = data.frame("actual"=test[,114], "lr_pred"=lr_predictions)
head(df_pred)
#Calcuate MAE, RMSE, R-sqaured for testing data
print(postResample(pred = lr_predictions, obs = test$`Absenteeism time in hours`))



# Decision Tree
print('Decision Tree')
library(rpart)
dt_model = rpart(Absenteeism_time_in_hours ~., data = train.data, method = "anova")
dt_predictions = predict(dt_model,test.data)
df_pred = data.frame("actual"=test[,114], "dt_pred"=dt_predictions)
head(df_pred)

#Calcuate MAE, RMSE, R-sqaured for testing data 
print(postResample(pred = dt_predictions, obs = test$`Absenteeism time in hours`))



#Random Forest
print('Random Forest')
library(randomForest)
rf_model = randomForest(Absenteeism_time_in_hours~., data = train.data, ntrees = 500)
rf_predictions = predict(rf_model,test.data)
df_pred = data.frame("actual"=test[,114], "rf_pred"=rf_predictions)
head(df_pred)

#Calcuate MAE, RMSE, R-sqaured for testing data 
print(postResample(pred = rf_predictions, obs = test$`Absenteeism time in hours`))



