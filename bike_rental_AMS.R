rm(list=ls())

#set working directory
setwd("S:/Aparajita/edwisor/proj1_bike")

#Loading Libraries
x = c("ggplot2", "corrgram",'DataCombine', "rpart", "randomForest")
lapply(x, require, character.only = TRUE)
rm(x)

#Load data
bike_data = read.csv("day.csv", header = T) 

#Checking the structure of data
str(bike_data)

#Exploratory Data Analysis

bike_data$season=as.factor(bike_data$season)
bike_data$mnth=as.factor(bike_data$mnth)
bike_data$yr=as.factor(bike_data$yr)
bike_data$holiday=as.factor(bike_data$holiday)
bike_data$weekday=as.factor(bike_data$weekday)
bike_data$workingday=as.factor(bike_data$workingday)
bike_data$weathersit=as.factor(bike_data$weathersit)
bike_data$dteday=as.factor(format(as.Date(bike_data$dteday,format="%Y-%m-%d"), "%d"))
bike_data=subset(bike_data,select = -c(instant,casual,registered))

#checking the structure of data after EDA
str(bike_data)

#Missing Value Analysis
missing_val = data.frame(apply(bike_data,2,function(x){sum(is.na(x))}))
names(missing_val)[1] =  "Missing_Value"
#NO missing values

#outlier Analysis
numeric_index = sapply(subset(bike_data,select = -c(cnt)),is.numeric)
numeric_data = bike_data[,numeric_index]
cnames = colnames(numeric_data)

 for (i in 1:length(cnames))
 {
   assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "cnt"), data = subset(bike_data))+ 
            stat_boxplot(geom = "errorbar", width = 0.5) +
            geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                         outlier.size=1, notch=FALSE) +
            theme(legend.position="bottom")+
            labs(y=cnames[i],x="cnt")+
            ggtitle(paste("Box plot for",cnames[i])))
 }

gridExtra::grid.arrange(gn1,gn2,ncol=2)
gridExtra::grid.arrange(gn4,gn3,ncol=2)

#Remove outliers

 for(i in cnames)
   {
   val = bike_data[,i][bike_data[,i] %in% boxplot.stats(bike_data[,i])$out]
   bike_data = bike_data[which(!bike_data[,i] %in% val),]     }

#Feature Selection

# Correlation Plot 
numeric_index = sapply(bike_data,is.numeric)
corrgram(bike_data[,numeric_index], order = F,upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

#Dimension Reduction#

bike_data = subset(bike_data,select = -c(atemp))

#Clean the environment

rmExcept("bike_data")

#getting test and train data'

train.index = sample(1:nrow(bike_data),0.8*nrow(bike_data))
train = bike_data[ train.index,]
test  = bike_data[-train.index,]

#Building Model

#Decision Tree
DT_model = rpart(cnt ~ ., data = train, method = "anova")
predictions_DT = as.integer(predict(DT_model, test[,-12]))

#Random Forest
RF_model = randomForest(cnt ~ ., train, importance = TRUE, ntree = 200)
predictions_RF = as.integer(predict(RF_model, test[,-12]))
plot(RF_model)

#Linear Regression
lm_model = lm(cnt ~., data = train)
summary(lm_model)
predictions_LR = as.integer(predict(lm_model, test[,-12]))

#calculate MAPE
MAPE = function(y, yhat){
   mean(abs((y - yhat)/y)*100)
}

MAPE(test[,12], predictions_DT)

MAPE(test[,12], predictions_RF)

MAPE(test[,12],  predictions_LR)

#saving the output

results <- data.frame(test[-12], Pred_Cnt = predictions_LR)

write.csv(results, file = 'LR_op_R.csv', row.names = FALSE)
