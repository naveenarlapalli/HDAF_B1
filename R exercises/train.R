library("readxl")
ret<-read_excel("Employee_Attrition_Dataset_Problem.xlsx", sheet = "Dataset")
ret
str(ret)
summary(ret)
library(caret)
ret$Over18<-NULL
ret$EmployeeCount<-NULL
ret$EmployeeNumber<-NULL
ret$StandardHours<-NULL
ret$BusinessTravel<-as.numeric(as.factor(ret$BusinessTravel))
ret$Department<-as.numeric(as.factor(ret$Department))
ret$EducationField<-as.numeric(as.factor(ret$EducationField))
ret$Gender<-as.numeric(as.factor(ret$Gender))
ret$JobRole<-as.numeric(as.factor(ret$JobRole))
ret$MaritalStatus<-as.numeric(as.factor(ret$MaritalStatus))
ret$OverTime<-as.numeric(as.factor(ret$OverTime))
#ret$Education <- as.factor(ret$Education)
#ret$EnvironmentSatisfaction <- as.factor(ret$EnvironmentSatisfaction)
#ret$JobInvolvement <- as.factor(ret$JobInvolvement)
#ret$JobLevel <- as.factor(ret$JobLevel)
#ret$JobSatisfaction <- as.factor(ret$JobSatisfaction)
#ret$PerformanceRating <- as.factor(ret$PerformanceRating)
#ret$RelationshipSatisfaction <- as.factor(ret$RelationshipSatisfaction)
#ret$StockOptionLevel <- as.factor(ret$StockOptionLevel)
#ret$TrainingTimesLastYear <- as.factor(ret$TrainingTimesLastYear)
#ret$WorkLifeBalance <- as.factor(ret$WorkLifeBalance)
ret$Attrition<-ifelse(ret$Attrition == "Yes" , 1,0)
ret$Attrition<-as.factor(ret$Attrition)

View(ret)

#library(caTools)
#'%ni%' <- Negate('%in%')
#split = sample.split(ret$Attrition, SplitRatio = 0.7)
#Dataset = subset(ret, split == TRUE)
#Evaluation = subset(ret, split == FALSE)
#table(Dataset$Attrition)
#set.seed(100)

#down_train <- downSample(x = ret[, colnames(ret) %ni% "Attrition"],
# y = ret$Attrition)

#table(down_train$Class)
logitmod <- glm(Attrition ~., family = "binomial",data=Dataset,maxit=100)

summary(logitmod)
glm_prob<- predict(logitmod,type = "response")
res<- predict(logitmod,ret,type = "response")


cut_off <- seq(1:10)*.1
cut_off
accuracy <- numeric()
false_pos <- numeric()
false_neg <- numeric()
Yes<-numeric()
No<-numeric()
for(i in cut_off){
  d1<-table(ActualValue=ret$Attrition,PredictValue=res>i)
  #print(d1)
  if(NCOL(d1)>1){
    accuracy<-c(accuracy,(d1[1,1]+d1[2,2])/(d1[1,1]+d1[1,2]+d1[2,1]+d1[2,2]))
    false_pos<-c(false_pos,(d1[2,1])/(d1[1,1]+d1[1,2]+d1[2,1]+d1[2,2]))
    false_neg<-c(false_neg,(d1[1,2])/(d1[1,1]+d1[1,2]+d1[2,1]+d1[2,2]))
    Yes <- c(Yes,(d1[2,2])/(d1[2,1]+d1[2,2]))
    No <- c(No,(d1[1,1])/(d1[1,1]+d1[1,2]))
    print(No)
  }
  if(NCOL(d1)==1){
    accuracy<-c(accuracy,(d1[1,1])/(d1[1,1]+d1[2,1]))
    false_pos<-c(false_pos,(d1[2,1])/(d1[1,1]+d1[2,1]))
    false_neg<-c(false_neg,0)
    Yes <- c(Yes,0)
    No <- c(No,(d1[1,1])/(d1[1,1]))
  }
    
  }
  result <- data.frame(cut_off,accuracy,false_pos,false_neg,Yes,No)
  
  print(result)
  
  # Recode factors
  #y_pred_num <- ifelse(pred <= 0.9, 0, 1)
  #y_pred <- factor(y_pred_num, levels= c(0,1))
  #y_act <- Evaluation$Attrition
  #table(y_act,y_pred )
  
  # Accuracy
  #mean(y_pred == y_act) 
  write.csv(result,file = "attrition.csv")
  