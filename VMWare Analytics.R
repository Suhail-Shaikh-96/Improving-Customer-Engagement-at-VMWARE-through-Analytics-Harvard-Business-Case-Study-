library(glmnet)
library(psych)
library(mlbench)
library(caret)

Train<- read.csv("E:\\IDS 572\\Case studies\\IMB 623 VMWare- Digital Buyer Journey\\Training.csv", header=TRUE)

#To check if column is factor.(Make a separate dataframe of factor variables and add to main dataframe later at line 144.
Check_factor <-function(x)
{
  ifelse( is.factor(x),TRUE,FALSE)
}
test2 <-sapply(Train[,1:ncol(Train)], Check_factor)
test_new2 <- data.frame(colnames(Train),test2)
#View(test_new2)
factorcolumns <-which(test2==TRUE)
factorarray <- test_new2[factorcolumns,]
factor_columns <-rownames(factorarray)
#View(factor_columns)
Target <- Train[,factor_columns]

Target$target <- as.factor(Train$target)
#View(Target)  #All columns with factor variavbles are stored in Target

#Converting factor to its level
replaceby_Missing<-function(x){
 # Addlevel_NA <-addNA(x)
  #levels(Addlevel_NA)<-c(levels(x),"Missing")- doesnt work
  #doesnot work    ifelse(is.na(x),"Missing",x)
   `levels<-`(addNA(x), c(levels(x), 'Missing'))
#or factor(ifelse(is.na(x), 'Missing', paste(x)), levels = c(levels(x), 'Missing'))
  }

Test_missinng <-data.frame(sapply(Target[,1:ncol(Target)],replaceby_Missing ))
#str(Test_misisinng$db_industry)
#View(Target$db_industry)
#View(Test_missinng)

#--------------------------------------------------------------Removing factor columns having more than 50% Missing values 
Remove_Missing <-function(x)
{
  count_na <- sum(x=='Missing')
  ifelse( count_na >0.5*50000,count_na,NA)
}
test3 <-sapply(Test_missinng[,1:ncol(Test_missinng)], Remove_Missing)
test_new3 <- data.frame(colnames(Test_missinng),test3)
#View(test_new3)
#View(Test_missinng)

Training_rowremove3 <- which(!is.na(test3))
Target_final <- Test_missinng[,-Training_rowremove3]
#View(Target_final)  #Only 9 columns are remaining
#View(Target_final$gu_emp_segment_desc)
#str(Target$db_industry)
#--------------------------------------------------------------------


#--------------------------------------------------------------Removing factor columns having more than 50% Unknown values
Unknown_factor_remove <- function(val){
  count_na <- sum(val=='Unknown')
  ifelse( count_na >0.5*50000,count_na,NA)
  
}

test4 <- sapply(Target_final[,1:ncol(Target_final)], Unknown_factor_remove)
test_new4 <- data.frame(colnames(Target_final),test4)
#View(test4)
Training_rowremove4 <- which(!is.na(test4))
Target_final_new <- Target_final[,-Training_rowremove4]
#View(Target_final_new)
#str(Target_final_new)
#str(Target$db_state)
#summary(Target_final_new$target)
#--------------------------------------------------------------------------------

#Dataset without factor columns
Train <- Train[,-factorcolumns]
Train$target <- NULL   #Removing target column
#ncol(Train1)

#----------------------------------------------- removing numerical coulumns with more than 50% NA values.
Remove_NA <-function(x)
{
  count_na <- sum(is.na(x))
  ifelse( count_na >0.5*50000,count_na,NA)
}

test <-sapply(Train[,1:ncol(Train)], Remove_NA)
test_new <- data.frame(colnames(Train),test)

Training_rowremove <- which(!is.na(test))
Train_final <- Train[,-Training_rowremove]
#Data after remob=ving coulumns which has more than 50% values as NA.
#str(Train_final$db)
#-------------------------------------------------------------------------------------------


#--------------------------------------------------------------Removing numerical columns having more than 50% 'Unknown' or 9999 values
Unknown_remove <- function(val){
  ifelse ( val=='Unknown'| val=='9999', NA, val)
  
}

Training_final<-data.frame(sapply(Train_final[,1:ncol(Train_final)], Unknown_remove))
#Training_final <- data.frame(Training_final)
#str(Train_final$db_city)
#Training_final$gu_emp_segment_desc
#View(Training_final)

Remove_NA_new <-function(x)                    
{
  count_na <- sum(is.na(x))
  ifelse( count_na >0.5*50000,count_na,NA)               #In this first we had given count_na > 0
}

new1 <-sapply(Training_final[,1:ncol(Training_final)], Remove_NA_new)
new2 <- data.frame(colnames(Training_final),new1)
new3 <- which(!is.na(new1))

#rowstoimpute <- rownames(new2[!is.na(new1),])
#Removing unknown
Training_final <- Training_final[,-new3]
#--------------------------------------------------------------------------------



#----------------------------------------Removing coulumns having imbalance Data(like columns having same data 98% of the rows)
Remove_0 <-function(x)
{
  count_na <- sum(x==0)
  ifelse( count_na >49500,count_na,NA)
}

Training_final_new <- sapply(Tratrainining_final[,1:ncol(Training_final)], Remove_0)

Training_final_new1 <- data.frame(colnames(Training_final),Training_final_new)
  
Training_rowremove2 <- which(!is.na(Training_final_new))
final_selected <-Training_final_new1[Training_rowremove2,]

Training_final_265_old <-Training_final[,-Training_rowremove2]
#View(Training_final_265_old)
#Training_final_265$target <- Target
Training_final_265 <- data.frame(Training_final_265_old,Target_final_new)     #Numerical columns and factor columns are combined together
#View(Training_final_265)   ncol(Training_final_265)   #Total 261 columns are there(factor+ Numerical)
#--------------------------------------------------------------------------------




#--------------------------------------*******************VAriable Reduction******************

#boruta method for feature selection
library(Boruta)
set.seed(111)
boruta_output <- Boruta(target~., data=Training_final_265, doTrace=2)

imp_feature <- names(boruta_output)
imp_feature
# Get significant variables including tentatives
boruta_signif <- getSelectedAttributes(boruta_output, withTentative = TRUE)
print(boruta_signif)

# Do a tentative rough fix
roughFixMod <- TentativeRoughFix(boruta_output)
boruta_signif <- getSelectedAttributes(roughFixMod)
print(boruta_signif)

# Variable Importance Scores
imps <- attStats(roughFixMod)
imps2 = imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
head(imps2[order(-imps2$meanImp), ])  # descending sort

# Plot variable importance
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance") 


#caret method for feature selection
library(caret)



class(Training_final_265$target) 
set.seed(100)
rPartMod <- train(target ~ ., data=Training_final_265, method="rpart")
rpartImp <- varImp(rPartMod)
print(rpartImp)    

#View(Training_final_265)
#View(Training_final_265[,-ncol(Training_final_265)])  #for removing target column

#These two columns had one NA row
#View(Training_final_265[which(is.na(Training_final_265$total_employees)),'total_employees'])
#View(Training_final_265[which(is.na(Training_final_265$derived_total_employees)),'derived_total_employees'])
#nrow(Training_final_265)
Training_final_265 <-data.frame(Training_final_265[-c(which(is.na(Training_final_265$total_employees))),])   #Removing only one row with NA value
#nrow(Training_final_265)
#str(Training_final_265$target)


#---------------------------------------------ranger algorithm for feature selection. We selected 92 
#install.packages('ranger')
library(ranger)
ranger_model <- ranger(target~.,data=Training_final_265,importance="permutation")
a=data.frame(colnames(Training_final_265[,-ncol(Training_final_265)]),importance_pvalues(ranger_model))
b=a[a$pvalue < 0.05,c(1,3)]
#View(b)
important_91variables=rownames(b)
#View(important_91variables)

Training_final_92 <-Training_final_265[,important_91variables]
#We have created the data frame here with 92 important variables given by ranger model(selection criteria for variables was p value less than 0.05)

#View(Training_final_92)
#--------------------------------------------------------------------------------------



#---------------------------------------------------SMOTE Classifier fot handelling imbalance(it balances the number of rows for each class)
install.packages('UBL')
library(UBL)
balancedata <- SmoteClassif(Training_final_265.target~., Training_final_92, C.perc="balance", k=5, dist="HEOM", p=2)
#summary(balancedata$Training_final_265.target) 
summary(balancedata$Training_final_265.target)
#-----------------------------------------------------------------------


  #--------------------------Naive Bayes model
  library(e1071)
  NBclassfier=naiveBayes(Training_final_265.target~., data=balancedata)
  Predict_Train_Naivebayes<-predict(NBclassfier,TestData,type = "class")
  #library(caret)
  Confusion_Matrix_Predict_Test_Naivebayes <-confusionMatrix(Predict_Train_Naivebayes,TestData$target)
  Confusion_Matrix_Predict_Test_Naivebayes
  
#   Confusion Matrix and Statistics
#   
#   Reference
#   Prediction     0     1     2     3     4     5
#   0 44064     1     0     4     0     5
#   1   192    71     3     2     5     7
#   2   547   172     1     0     3    11
#   3  2790   437     3     8    20    47
#   4   130    73     1     0    11    17
#   5   878    46     1     2    51   403
#   
#   Overall Statistics
#   
#   Accuracy : 0.8911          
#   95% CI : (0.8883, 0.8938)
#   No Information Rate : 0.9719          
#   P-Value [Acc > NIR] : 1               
#   
#   Kappa : 0.2381          
#   
#   Mcnemar's Test P-Value : <2e-16          
# 
# Statistics by Class:
# 
#                      Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5
# Sensitivity            0.9066 0.088750 0.111111 0.500000 0.122222 0.822449
# Specificity            0.9929 0.995753 0.985339 0.934047 0.995573 0.980249
# Pos Pred Value         0.9998 0.253571 0.001362 0.002421 0.047414 0.291818
# Neg Pred Value         0.2352 0.985340 0.999838 0.999829 0.998413 0.998211
# Prevalence             0.9719 0.015998 0.000180 0.000320 0.001800 0.009799
# Detection Rate         0.8812 0.001420 0.000020 0.000160 0.000220 0.008059
# Detection Prevalence   0.8814 0.005599 0.014678 0.066092 0.004639 0.027617
# Balanced Accuracy      0.9498 0.542251 0.548225 0.717023 0.558897 0.901349
  
  #Average Precision for class 1-5=(0.253571+0.001362+0.002421+0.047414+0.291818)/5=0.1193172,
  #Precision is very less only 12% here.
  #---------End Naive Bayes

  
  #-------------------------------------------------XGBoost Model

  Test<- read.csv("E:\\IDS 572\\Case studies\\IMB 623 VMWare- Digital Buyer Journey\\Validation.csv", header=TRUE)
  Test$target <- as.factor(Test$target)
  TestData <-Test
  
  Balancedata_xgboost <-balancedata
  Balancedata_xgboost$target <-Balancedata_xgboost$Training_final_265.target
  Balancedata_xgboost$Training_final_265.target <- NULL
  Balancedata_xgboost$db_country <- NULL
  Balancedata_xgboost$db_state <- NULL
  Balancedata_xgboost$db_audience <- NULL
  
  library(xgboost)
  library(caret)
  
  target <- Balancedata_xgboost$target
  
  
  XG_Data <- Balancedata_xgboost
  xg_validate <- TestData
  
  target_classes <- XG_Data$target
  target_validate_class <- xg_validate$target
  label <- as.integer(XG_Data$target)-1
  XG_Data$target = NULL
  
  
  n = nrow(XG_Data)
  train.index = sample(n,floor(0.75*n))
  train.data = as.matrix(XG_Data[train.index,])
  train.label = label[train.index]
  
  test.data = as.matrix(XG_Data[-train.index,])
  test.label = label[-train.index]
  
  xgb.train = xgb.DMatrix(data=train.data,label=train.label)
  xgb.test = xgb.DMatrix(data=test.data,label=test.label)
  
  num_class1 = length(levels(target))
  params = list(
    booster="gbtree",
    eta=0.001,
    max_depth=5,
    gamma=3,
    subsample=0.75,
    colsample_bytree=1,
    objective="multi:softprob",
    eval_metric="mlogloss",
    num_class=num_class
  )
  
  # Train the XGBoost classifer
  xgb.fit=xgb.train(
    params=params,
    data=xgb.train,
    nrounds=200,
    early_stopping_rounds=10,
    watchlist=list(val1=xgb.train,val2=xgb.test),
    verbose=1
  )
  
  
  xgb.pred = predict(xgb.fit,test.data,reshape=T)
  xgb.pred
  xgb.pred = as.data.frame(xgb.pred)
  colnames(xgb.pred) = levels(target)
  xgb.pred
  
  xgb.pred$prediction = apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
  xgb.pred$label = levels(target)[test.label+1]
  xgb.pred
  
  data.frame(xgb.pred$prediction , xgb.pred$label)
  ConfusionMatrix_XGB_maxdepth_5 <- confusionMatrix(as.factor(xgb.pred$prediction) , as.factor(xgb.pred$label))
  
  ###----------------Confusion Matrix Maxdepth=5 and eta=0.001
  # Confusion Matrix and Statistics
  # 
  # Reference
  # Prediction    0    1    2    3    4    5
  # 0 2089    0    0    0    0    0
  # 1    0 2017    0   30    0    0
  # 2    0  116 2070   28    0    0
  # 3    7    7    0 1985    0    0
  # 4    0    0    0    0 2126    0
  # 5    0    0    0    0    0 2026
  # 
  # Overall Statistics
  # 
  # Accuracy : 0.985          
  # 95% CI : (0.9827, 0.987)
  # No Information Rate : 0.1712         
  # P-Value [Acc > NIR] : < 2.2e-16      
  # 
  # Kappa : 0.982          
  # 
  # Mcnemar's Test P-Value : NA             
  # 
  # Statistics by Class:
  # 
  #                      Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5
  # Sensitivity            0.9967   0.9425   1.0000   0.9716   1.0000   1.0000
  # Specificity            1.0000   0.9971   0.9862   0.9987   1.0000   1.0000
  # Pos Pred Value         1.0000   0.9853   0.9350   0.9930   1.0000   1.0000
  # Neg Pred Value         0.9993   0.9882   1.0000   0.9945   1.0000   1.0000
  # Prevalence             0.1677   0.1712   0.1656   0.1634   0.1701   0.1621
  # Detection Rate         0.1671   0.1613   0.1656   0.1588   0.1701   0.1621
  # Detection Prevalence   0.1671   0.1637   0.1771   0.1599   0.1701   0.1621
  # Balanced Accuracy      0.9983   0.9698   0.9931   0.9851   1.0000   1.0000
  
  
  #Average Precision for class 1-5=(0.9999+0.9947+1+1+1)/5=0.9989
  
#table(xgb.pred$prediction , xgb.pred$label)
  result = sum(xgb.pred$prediction==xgb.pred$label)/nrow(xgb.pred)
  print(paste("Final Accuracy =",sprintf("%1.2f%%", 100*result)))
  #-------------------------------------------------------------------------------------------
  
  
  #----------------------------------------------------------------changing eta=0.01 from eta=0.01
  
  params = list(
    booster="gbtree",
    eta=0.01,
    max_depth=5,
    gamma=3,
    subsample=0.75,
    colsample_bytree=1,
    objective="multi:softprob",
    eval_metric="mlogloss",
    num_class=num_class
  )
  
  # Train the XGBoost classifer
  xgb.fit=xgb.train(
    params=params,
    data=xgb.train,
    nrounds=200,
    early_stopping_rounds=10,
    watchlist=list(val1=xgb.train,val2=xgb.test),
    verbose=1
  )
  
  
  
  xgb.pred = predict(xgb.fit,test.data,reshape=T)
  xgb.pred
  xgb.pred = as.data.frame(xgb.pred)
  colnames(xgb.pred) = levels(target)
  xgb.pred
  
  xgb.pred$prediction = apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
  xgb.pred$label = levels(target)[test.label+1]
  xgb.pred
  
  data.frame(xgb.pred$prediction , xgb.pred$label)
  ConfusionMatrix_XGB_maxdepth_10 <- confusionMatrix(as.factor(xgb.pred$prediction) , as.factor(xgb.pred$label))
  
  # Confusion Matrix and Statistics
  # 
  # Reference
  # Prediction    0    1    2    3    4    5
  # 0 2078    0    0    0    0    0
  # 1    0 1758    0   71    0    0
  # 2    0  368 2070   75    0    0
  # 3   18   14    0 1897    0    0
  # 4    0    0    0    0 2126    0
  # 5    0    0    0    0    0 2026
  # 
  # Overall Statistics
  # 
  # Accuracy : 0.9563          
  # 95% CI : (0.9526, 0.9598)
  # No Information Rate : 0.1712          
  # P-Value [Acc > NIR] : < 2.2e-16       
  # 
  # Kappa : 0.9476          
  # 
  # Mcnemar's Test P-Value : NA              
  # 
  # Statistics by Class:
  # 
  #                      Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5
  # Sensitivity            0.9914   0.8215   1.0000   0.9285   1.0000   1.0000
  # Specificity            1.0000   0.9931   0.9575   0.9969   1.0000   1.0000
  # Pos Pred Value         1.0000   0.9612   0.8237   0.9834   1.0000   1.0000
  # Neg Pred Value         0.9983   0.9642   1.0000   0.9862   1.0000   1.0000
  # Prevalence             0.1677   0.1712   0.1656   0.1634   0.1701   0.1621
  # Detection Rate         0.1662   0.1406   0.1656   0.1517   0.1701   0.1621
  # Detection Prevalence   0.1662   0.1463   0.2010   0.1543   0.1701   0.1621
  # Balanced Accuracy      0.9957   0.9073   0.9788   0.9627   1.0000   1.0000
  
  
  #Average Precision for class 1-5=(0.9612+0.8237+0.9834+1+1)/5=0.95366
  
  result = sum(xgb.pred$prediction==xgb.pred$label)/nrow(xgb.pred)
  print(paste("Final Accuracy =",sprintf("%1.2f%%", 100*result)))
  
  
  
  
  #---------------------------------------------------------------------------randomForest Model
  
  library(randomForest)
  library(caret)
  
  Balancedata_RF <-balancedata
  
  Balancedata_RF$target <-Balancedata_xgboost$Training_final_265.target
  #Removing factor vRIABLES WITH MORE THAN 53 LEVELS AS r IS NOT ABLE TO PROCESS IT
  Balancedata_RF$Training_final_265.target <- NULL
  Balancedata_RF$db_country <- NULL
  Balancedata_RF$db_state <- NULL
  Balancedata_RF$db_audience <- NULL
  summary(Balancedata_RF$target)
  
  Model_RF<- randomForest(target~.,data=Balancedata_RF,ntree=100,mtry=20,importance=TRUE,verbose=TRUE)#,proximity=TRUE)
  #model1
  summary(Model_RF)
  
  Test<- read.csv("E:\\IDS 572\\Case studies\\IMB 623 VMWare- Digital Buyer Journey\\Validation.csv", header=TRUE)
  Test$target <- as.factor(Test$target)
  TestData_RF <-Test
  
  Predict_Train_RF<-predict(Model_RF,TestData_RF,type = "class")
  #View(upsample)
  
  confusionMatrix_RF <- confusionMatrix(Predict_Train_RF,TestData_RF$target)
  confusionMatrix_RF
  
  # confusionMatrix_RF
  # 
  # 
  # Reference
  # Prediction     0     1     2     3     4     5
  # 0 48565     0     0     9     0     0
  # 1    36   800     9     3     0     0
  # 2     0     0     0     0     0     0
  # 3     0     0     0     4     0     0
  # 4     0     0     0     0    86     0
  # 5     0     0     0     0     4   490
  # 
  # Overall Statistics
  # 
  # Accuracy : 0.9988          
  # 95% CI : (0.9984, 0.9991)
  # No Information Rate : 0.9719          
  # P-Value [Acc > NIR] : < 2.2e-16       
  # 
  # Kappa : 0.978           
  # 
  # Mcnemar's Test P-Value : NA              
  # 
  # Statistics by Class:
  # 
  #                      Class: 0 Class: 1 Class: 2  Class: 3 Class: 4 Class: 5
  # Sensitivity            0.9993  1.00000  0.00000 2.500e-01  0.95556 1.000000
  # Specificity            0.9936  0.99902  1.00000 1.000e+00  1.00000 0.999919
  # Pos Pred Value         0.9998  0.94340      NaN 1.000e+00  1.00000 0.991903
  # Neg Pred Value         0.9749  1.00000  0.99982 9.998e-01  0.99992 1.000000
  # Prevalence             0.9719  0.01600  0.00018 3.200e-04  0.00180 0.009799
  # Detection Rate         0.9712  0.01600  0.00000 7.999e-05  0.00172 0.009799
  # Detection Prevalence   0.9714  0.01696  0.00000 7.999e-05  0.00172 0.009879
  # Balanced Accuracy      0.9964  0.99951  0.50000 6.250e-01  0.97778 0.999960
  # 
  
  
  # #Average Precision for class 1-5 = (  0.9434   +   1   +   1   +   0.991903)/5 =0.7870 
  
  
  #----------------------------------------------------------Random Forest without SMOTE
  
  Balancedata_before_RF <- Training_final_92
  Balancedata_before_RF$db_country <- NULL
  Balancedata_before_RF$db_audience <- NULL
  Balancedata_before_RF$db_state <- NULL
  Balancedata_before_RF$target <- Balancedata_before_RF$Training_final_265.target
  Balancedata_before_RF$Training_final_265.target <- NULL
  summary(Balancedata_before_RF$targe)
  #0         1     2     3     4     5 
  #48669   777     9    19    85   446
  
  Model_RF1<- randomForest(target~.,data=Balancedata_before_RF,ntree=100,mtry=20,importance=TRUE,verbose=TRUE)#,proximity=TRUE)
  #model1
  summary(Model_RF1)
  
  Predict_Train_RF1<-predict(Model_RF1,TestData_RF,type = "class")
  #View(upsample)
  
  confusionMatrix_RF1 <- confusionMatrix(Predict_Train_RF1,TestData_RF$target)
  confusionMatrix_RF1
  
  # confusionMatrix_RF1
  # Confusion Matrix and Statistics
  # 
  # Reference
  # Prediction     0     1     2     3     4     5
  # 0 48601     0     0    13     0     0
  # 1     0   800     9     3     0     0
  # 2     0     0     0     0     0     0
  # 3     0     0     0     0     0     0
  # 4     0     0     0     0    86     0
  # 5     0     0     0     0     4   490
  # 
  # Overall Statistics
  # 
  # Accuracy : 0.9994          
  # 95% CI : (0.9992, 0.9996)
  # No Information Rate : 0.9719          
  # P-Value [Acc > NIR] : < 2.2e-16       
  # 
  # Kappa : 0.9894          
  # 
  # Mcnemar's Test P-Value : NA              
  # 
  # Statistics by Class:
  # 
  #                      Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5
  # Sensitivity            1.0000  1.00000  0.00000  0.00000  0.95556 1.000000
  # Specificity            0.9907  0.99976  1.00000  1.00000  1.00000 0.999919
  # Pos Pred Value         0.9997  0.98522      NaN      NaN  1.00000 0.991903
  # Neg Pred Value         1.0000  1.00000  0.99982  0.99968  0.99992 1.000000
  # Prevalence             0.9719  0.01600  0.00018  0.00032  0.00180 0.009799
  # Detection Rate         0.9719  0.01600  0.00000  0.00000  0.00172 0.009799
  # Detection Prevalence   0.9722  0.01624  0.00000  0.00000  0.00172 0.009879
  # Balanced Accuracy      0.9954  0.99988  0.50000  0.50000  0.97778 0.999960
  
  #Average Precision for class 1-5=(  0.98522  +   1   +   0.991903)/5 =0.5954
  #---------------------------------------------------------------------------------
  
  
  
  #------------------------------------Regularization model using glmnet---------------------------------
  #install.packages("model.Matrix")
  library(model.Matrix)
  #install.packages("Matrix")
  library(Matrix)
  #install.packages("data.table")
  library(data.table)    
  #install.packages("Mtools")
  
  #Creating spares matrix for train data
  #install.packages('glmnet')  
  library(glmnet)
  Balancedata_GLM <- Balancedata
  Balancedata_GLM$Training_final_265.target <-NULL
  SparseMatrixdataTrain <-sparse.model.matrix(~. ,data =Balancedata_GLM)
  #str(Balancedata$Training_final_265.target)
  
  columnNames <- colnames(balancedata)
  Test<- read.csv("E:\\IDS 572\\Case studies\\IMB 623 VMWare- Digital Buyer Journey\\Validation.csv", header=TRUE)
  Test$target <- as.factor(Test$target)
  
  TestData <- Test[,columnNames[c(1:91)]]
  TestData$target <- Test$target
  #str(TestData$target)
  TestData$db_country <- NULL
  TestData$db_state <- NULL
  TestData$db_audience <- NULL
  TestData$target
  #str(TestData)
  TestData1=TestData
  TestData$target <- NULL
  #str(TestData)
  
  str(Balancedata)
  Balancedata <-balancedata
  View(SparseMatrixdataTrain)
  Balancedata$Training_final_265.target <- NULL
  #dt <- data.table(Balancedata)
  #SparseMatrixdataTrain <- sparsify(dt, sparsifyNAs = FALSE)
  
  
  Balancedata$db_country <- NULL
  Balancedata$db_state <- NULL
  Balancedata$db_audience <-NULL
  SparseMatrixdataTest <-sparse.model.matrix(~.,TestData)

  #.............................................Lasso Regularization........................................................................
  #Model_glmNet <- glmnet(SparseMatrixdataTrain,Balancedata$Training_final_265.target,alpha=1,family="multinomial",)
  cv_glmnet <-cv.glmnet(SparseMatrixdataTrain,balancedata$Training_final_265.target,nfolds=5,alpha=1,family="multinomial",type.measure = "class")
  #prediction on train #, trace.it = 1
  
  
  plot(cv_glmnet, xvar="lambda", label=TRUE)
  plot(cv_glmnet$glmnet.fit,xvar="lambda",label=TRUE)
  ##plot(cv_glmnet$glmnet.fit, "norm", label=TRUE)
  
  lm_1 <- cv_glmnet$lambda.min
  
  Glmnet_BestModel <- glmnet(SparseMatrixdataTrain, balancedata$Training_final_265.target, family= "multinomial", alpha = 1, lambda = lm_1)
  
  
  pred_op <- predict(Glmnet_BestModel, SparseMatrixdataTrain)#, s="lambda.min", type = "class")
  TestData$target <- Test$target
  library(caret)
  ConfusionMatrix_GLM_Lasso <- confusionMatrix(pred_op , as.factor(Balancedata_GLM$target))
  ConfusionMatrix_GLM_Lasso
  #-----------------------------------------------------