pacman::p_load("caret","ROCR","lift","glmnet","MASS","e1071",'dplyr',
               "partykit","rpart", "randomForest", "xgboost") #Check, and if needed install the necessary packages

library(dplyr)

creditData <- read.csv(file.choose()) #read the 25,000 credit data
str(creditData)


#------------------------------------

### Data cleaning

creditData$X<- NULL
colnames(creditData)
names(creditData)[names(creditData)=="ï..ID"] <- "ID" #fix the ID header

creditData %>% group_by(EDUCATION) %>% summarise(no_rows=length(ID))

#Combine 4,5,6 to 0 because all 3 doesn't translate to a meaningful difference
creditData$EDUCATION <- as.character(creditData$EDUCATION)
creditData$EDUCATION[creditData$EDUCATION=="5"]<- "0"
creditData$EDUCATION[creditData$EDUCATION=="6"]<- "0"
creditData$EDUCATION[creditData$EDUCATION=="4"]<- "0"
creditData$EDUCATION <- as.factor(creditData$EDUCATION)

#Combine 3 and 0 for MARRIAGE
creditData$MARRIAGE <- as.character(creditData$MARRIAGE)
creditData$MARRIAGE[creditData$MARRIAGE=="3"]<- "0"
creditData$MARRIAGE <- as.factor(creditData$MARRIAGE)

#Remove all PAY_1 values of -1 and -2 because it has inherently different meaning than 0 and above
creditData$PAY_ADJ1<-creditData$PAY_1
creditData$PAY_ADJ2<-creditData$PAY_2
creditData$PAY_ADJ3<-creditData$PAY_3
creditData$PAY_ADJ4<-creditData$PAY_4
creditData$PAY_ADJ5<-creditData$PAY_5
creditData$PAY_ADJ6<-creditData$PAY_6
creditData$PAY_ADJ1[creditData$PAY_ADJ1<0]<-0
creditData$PAY_ADJ2[creditData$PAY_ADJ2<0]<-0
creditData$PAY_ADJ3[creditData$PAY_ADJ3<0]<-0
creditData$PAY_ADJ4[creditData$PAY_ADJ4<0]<-0
creditData$PAY_ADJ5[creditData$PAY_ADJ5<0]<-0
creditData$PAY_ADJ6[creditData$PAY_ADJ6<0]<-0

#Normalise bill amount based on limit balance
creditData$BILL_P1<- creditData$BILL_AMT1 / creditData$LIMIT_BAL
creditData$BILL_P2<- creditData$BILL_AMT2 / creditData$LIMIT_BAL
creditData$BILL_P3<- creditData$BILL_AMT3 / creditData$LIMIT_BAL
creditData$BILL_P4<- creditData$BILL_AMT4 / creditData$LIMIT_BAL
creditData$BILL_P5<- creditData$BILL_AMT5 / creditData$LIMIT_BAL
creditData$BILL_P6<- creditData$BILL_AMT6 / creditData$LIMIT_BAL

#Normalise payment amount based on limit balance
creditData$PAY_P1<- creditData$PAY_AMT1 / creditData$LIMIT_BAL
creditData$PAY_P2<- creditData$PAY_AMT2 / creditData$LIMIT_BAL
creditData$PAY_P3<- creditData$PAY_AMT3 / creditData$LIMIT_BAL
creditData$PAY_P4<- creditData$PAY_AMT4 / creditData$LIMIT_BAL
creditData$PAY_P5<- creditData$PAY_AMT5 / creditData$LIMIT_BAL
creditData$PAY_P6<- creditData$PAY_AMT6 / creditData$LIMIT_BAL

#Get the maximum lateness of the creditor over the past 6 months and for everyone who is never late, put 0
creditData$MAX_LATE<- pmax(creditData$PAY_1,creditData$PAY_2,creditData$PAY_3,creditData$PAY_4,creditData$PAY_5,creditData$PAY_6)
creditData$MAX_LATE[creditData$MAX_LATE<0]<-0

#Separate out all -2 from PAY_1 in a new variable
creditData$PAY_ONTIME_1<-creditData$PAY_1
creditData$PAY_ONTIME_2<-creditData$PAY_2
creditData$PAY_ONTIME_3<-creditData$PAY_3
creditData$PAY_ONTIME_4<-creditData$PAY_4
creditData$PAY_ONTIME_5<-creditData$PAY_5
creditData$PAY_ONTIME_6<-creditData$PAY_6

creditData$PAY_ONTIME_1[creditData$PAY_ONTIME_1!=-2]<-0
creditData$PAY_ONTIME_1[creditData$PAY_ONTIME_1==-2]<-1

creditData$PAY_ONTIME_2[creditData$PAY_ONTIME_2!=-2]<-0
creditData$PAY_ONTIME_2[creditData$PAY_ONTIME_2==-2]<-1

creditData$PAY_ONTIME_3[creditData$PAY_ONTIME_3!=-2]<-0
creditData$PAY_ONTIME_3[creditData$PAY_ONTIME_3==-2]<-1

creditData$PAY_ONTIME_4[creditData$PAY_ONTIME_4!=-2]<-0
creditData$PAY_ONTIME_4[creditData$PAY_ONTIME_4==-2]<-1

creditData$PAY_ONTIME_5[creditData$PAY_ONTIME_5!=-2]<-0
creditData$PAY_ONTIME_5[creditData$PAY_ONTIME_5==-2]<-1

creditData$PAY_ONTIME_6[creditData$PAY_ONTIME_6!=-2]<-0
creditData$PAY_ONTIME_6[creditData$PAY_ONTIME_6==-2]<-1

#Separate out all -1 from PAY_1 in a new variable
creditData$PAY_FULL_1<-creditData$PAY_1
creditData$PAY_FULL_2<-creditData$PAY_2
creditData$PAY_FULL_3<-creditData$PAY_3
creditData$PAY_FULL_4<-creditData$PAY_4
creditData$PAY_FULL_5<-creditData$PAY_5
creditData$PAY_FULL_6<-creditData$PAY_6

creditData$PAY_FULL_1[creditData$PAY_FULL_1!=-1]<-0
creditData$PAY_FULL_1[creditData$PAY_FULL_1==-1]<-1

creditData$PAY_FULL_2[creditData$PAY_FULL_2!=-1]<-0
creditData$PAY_FULL_2[creditData$PAY_FULL_2==-1]<-1

creditData$PAY_FULL_3[creditData$PAY_FULL_3!=-1]<-0
creditData$PAY_FULL_3[creditData$PAY_FULL_3==-1]<-1

creditData$PAY_FULL_4[creditData$PAY_FULL_4!=-1]<-0
creditData$PAY_FULL_4[creditData$PAY_FULL_4==-1]<-1

creditData$PAY_FULL_5[creditData$PAY_FULL_5!=-1]<-0
creditData$PAY_FULL_5[creditData$PAY_FULL_5==-1]<-1

creditData$PAY_FULL_6[creditData$PAY_FULL_6!=-1]<-0
creditData$PAY_FULL_6[creditData$PAY_FULL_6==-1]<-1

#Create features that signifies whether they're late or not, just true or false
creditData$PAY_LATE_1<-creditData$PAY_1
creditData$PAY_LATE_2<-creditData$PAY_2
creditData$PAY_LATE_3<-creditData$PAY_3
creditData$PAY_LATE_4<-creditData$PAY_4
creditData$PAY_LATE_5<-creditData$PAY_5
creditData$PAY_LATE_6<-creditData$PAY_6

creditData$PAY_LATE_1[creditData$PAY_LATE_1>0]<-1
creditData$PAY_LATE_1[creditData$PAY_LATE_1<1]<-0

creditData$PAY_LATE_2[creditData$PAY_LATE_2>0]<-1
creditData$PAY_LATE_2[creditData$PAY_LATE_2<1]<-0

creditData$PAY_LATE_3[creditData$PAY_LATE_3>0]<-1
creditData$PAY_LATE_3[creditData$PAY_LATE_3<1]<-0

creditData$PAY_LATE_4[creditData$PAY_LATE_4>0]<-1
creditData$PAY_LATE_4[creditData$PAY_LATE_4<1]<-0

creditData$PAY_LATE_5[creditData$PAY_LATE_5>0]<-1
creditData$PAY_LATE_5[creditData$PAY_LATE_5<1]<-0

creditData$PAY_LATE_6[creditData$PAY_LATE_6>0]<-1
creditData$PAY_LATE_6[creditData$PAY_LATE_6<1]<-0

#Create normalised total bill and total payment in comparison to limit balance
creditData$TotalBill <- (creditData$BILL_AMT1 + creditData$BILL_AMT2 + creditData$BILL_AMT3 + creditData$BILL_AMT4 + creditData$BILL_AMT5 + creditData$BILL_AMT6) / creditData$LIMIT_BAL
creditData$TotalPay <- (creditData$PAY_AMT1 + creditData$PAY_AMT2 + creditData$PAY_AMT3 + creditData$PAY_AMT4 + creditData$PAY_AMT5 + creditData$PAY_AMT6) / (creditData$LIMIT_BAL)

#Create feature of how much money is left to be paid
creditData$Leftover <- (creditData$TotalBill - creditData$TotalPay)

head(creditData,10)

#Change to factors
creditData$SEX<- as.factor(creditData$SEX)
creditData$EDUCATION<- as.factor(creditData$EDUCATION)
creditData$MARRIAGE<- as.factor(creditData$MARRIAGE)
creditData$PAY_1<- as.factor(creditData$PAY_1)
creditData$PAY_2<- as.factor(creditData$PAY_2)
creditData$PAY_3<- as.factor(creditData$PAY_3)
creditData$PAY_4<- as.factor(creditData$PAY_4)
creditData$PAY_5<- as.factor(creditData$PAY_5)
creditData$PAY_6<- as.factor(creditData$PAY_6)
creditData$default_0<- as.factor(creditData$default_0)

creditData$PAY_ONTIME_1<-as.factor(creditData$PAY_ONTIME_1)
creditData$PAY_ONTIME_2<-as.factor(creditData$PAY_ONTIME_2)
creditData$PAY_ONTIME_3<-as.factor(creditData$PAY_ONTIME_3)
creditData$PAY_ONTIME_4<-as.factor(creditData$PAY_ONTIME_4)
creditData$PAY_ONTIME_5<-as.factor(creditData$PAY_ONTIME_5)
creditData$PAY_ONTIME_6<-as.factor(creditData$PAY_ONTIME_6)
creditData$PAY_FULL_1<-as.factor(creditData$PAY_FULL_1)
creditData$PAY_FULL_2<-as.factor(creditData$PAY_FULL_2)
creditData$PAY_FULL_3<-as.factor(creditData$PAY_FULL_3)
creditData$PAY_FULL_4<-as.factor(creditData$PAY_FULL_4)
creditData$PAY_FULL_5<-as.factor(creditData$PAY_FULL_5)
creditData$PAY_FULL_6<-as.factor(creditData$PAY_FULL_6)
creditData$PAY_LATE_1<-as.factor(creditData$PAY_LATE_1)
creditData$PAY_LATE_2<-as.factor(creditData$PAY_LATE_2)
creditData$PAY_LATE_3<-as.factor(creditData$PAY_LATE_3)
creditData$PAY_LATE_4<-as.factor(creditData$PAY_LATE_4)
creditData$PAY_LATE_5<-as.factor(creditData$PAY_LATE_5)
creditData$PAY_LATE_6<-as.factor(creditData$PAY_LATE_6)
creditData$PAY_ADJ1<-as.numeric(creditData$PAY_ADJ1)
creditData$PAY_ADJ2<-as.numeric(creditData$PAY_ADJ2)
creditData$PAY_ADJ3<-as.numeric(creditData$PAY_ADJ3)
creditData$PAY_ADJ4<-as.numeric(creditData$PAY_ADJ4)
creditData$PAY_ADJ5<-as.numeric(creditData$PAY_ADJ5)
creditData$PAY_ADJ6<-as.numeric(creditData$PAY_ADJ6)


#Remove features no longer relevant
creditData$ID <- NULL
creditData$PAY_1 <- NULL
creditData$PAY_2 <- NULL
creditData$PAY_3 <- NULL
creditData$PAY_4 <- NULL
creditData$PAY_5 <- NULL
creditData$PAY_6 <- NULL

creditData$BILL_AMT1 <- NULL
creditData$BILL_AMT2 <- NULL
creditData$BILL_AMT3 <- NULL
creditData$BILL_AMT4 <- NULL
creditData$BILL_AMT5 <- NULL
creditData$BILL_AMT6 <- NULL

creditData$PAY_AMT1 <- NULL
creditData$PAY_AMT2 <- NULL
creditData$PAY_AMT3 <- NULL
creditData$PAY_AMT4 <- NULL
creditData$PAY_AMT5 <- NULL
creditData$PAY_AMT6 <- NULL

fixNAs<-function(data_frame){
  # Define reactions to NAs
  integer_reac<-0
  factor_reac<-"FIXED_NA"
  character_reac<-"FIXED_NA"
  date_reac<-as.Date("1900-01-01")
  # Loop through columns in the data frame and depending on which class the variable is, apply the defined reaction and create a surrogate
  
  for (i in 1 : ncol(data_frame)){
    if (class(data_frame[,i]) %in% c("numeric","integer")) {
      if (any(is.na(data_frame[,i]))){
        data_frame[,paste0(colnames(data_frame)[i],"_surrogate")]<-
          as.factor(ifelse(is.na(data_frame[,i]),"1","0"))
        data_frame[is.na(data_frame[,i]),i]<-integer_reac
      }
    } else
      if (class(data_frame[,i]) %in% c("factor")) {
        if (any(is.na(data_frame[,i]))){
          data_frame[,i]<-as.character(data_frame[,i])
          data_frame[,paste0(colnames(data_frame)[i],"_surrogate")]<-
            as.factor(ifelse(is.na(data_frame[,i]),"1","0"))
          data_frame[is.na(data_frame[,i]),i]<-factor_reac
          data_frame[,i]<-as.factor(data_frame[,i])
          
        } 
      } else {
        if (class(data_frame[,i]) %in% c("character")) {
          if (any(is.na(data_frame[,i]))){
            data_frame[,paste0(colnames(data_frame)[i],"_surrogate")]<-
              as.factor(ifelse(is.na(data_frame[,i]),"1","0"))
            data_frame[is.na(data_frame[,i]),i]<-character_reac
          }  
        } else {
          if (class(data_frame[,i]) %in% c("Date")) {
            if (any(is.na(data_frame[,i]))){
              data_frame[,paste0(colnames(data_frame)[i],"_surrogate")]<-
                as.factor(ifelse(is.na(data_frame[,i]),"1","0"))
              data_frame[is.na(data_frame[,i]),i]<-date_reac
            }
          }  
        }       
      }
  } 
  return(data_frame) 
}

#Fix NA values
creditData_NA <- fixNAs(creditData)
str(creditData_NA)


#------------------------------------
#Partition training data

set.seed(77850) #set a random number generation seed to ensure that the split is the same everytime
inTrain <- createDataPartition(y = creditData_NA$default_0,
                               p = 19999/24000, list = FALSE) #approximate 80/20 rule
training <- creditData_NA[ inTrain,]
testing <- creditData_NA[ -inTrain,]
str(training)


#------------------------------------
#Logistic Regression

model_logistic<-glm(default_0~., data=training, family="binomial"(link="logit"))
summary(model_logistic)

#Stepwise AIC
model_logistic_stepwiseAIC<-stepAIC(model_logistic,direction = c("both"),trace = 1) #AIC stepwise
summary(model_logistic_stepwiseAIC) 
warnings()

#Plot the model
par(mfrow=c(1,4))
plot(model_logistic_stepwiseAIC) #Error plots: similar nature to lm plots
str(creditData_NA)
par(mfrow=c(1,1))

#Test the model
logistic_probabilities<-predict(model_logistic_stepwiseAIC,newdata=testing,type="response") #Predict probabilities
logistic_classification<-rep("1",4000)
logistic_classification[logistic_probabilities<0.22]="0" #Predict classification using 0.22 threshold. 0.22 is derived from the default rate of the 25,000 sample data
logistic_classification<-as.factor(logistic_classification)

###Confusion matrix  
confusionMatrix(logistic_classification,testing$default_0,positive = "1") #Display confusion matrix

####ROC Curve
logistic_ROC_prediction <- prediction(logistic_probabilities, testing$default_0)
logistic_ROC <- performance(logistic_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(logistic_ROC) #Plot ROC curve

####AUC (area under curve)
auc.tmp <- performance(logistic_ROC_prediction,"auc") #Create AUC data
logistic_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
logistic_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

#AUC yields 0.77

#------------------------------------------
### CTREE 


ctree_tree<-ctree(default_0~.,data=training) #Run ctree on training data
plot(ctree_tree, gp = gpar(fontsize = 8)) #Plotting the tree (adjust fontsize if needed)

ctree_probabilities<-predict(ctree_tree,newdata=testing,type="prob") #Predict probabilities
ctree_classification<-rep("1",4000)
ctree_classification[ctree_probabilities[,2]<0.22]="0"
ctree_classification<-as.factor(ctree_classification)

###Confusion matrix  
confusionMatrix(ctree_classification,testing$default_0,positive = "1")

####ROC Curve
ctree_probabilities_testing <-predict(ctree_tree,newdata=testing,type = "prob") #Predict probabilities
ctree_pred_testing <- prediction(ctree_probabilities_testing[,2], testing$default_0) #Calculate errors
ctree_ROC_testing <- performance(ctree_pred_testing,"tpr","fpr") #Create ROC curve data
plot(ctree_ROC_testing) #Plot ROC curve

####AUC (area under curve)
auc.tmp <- performance(ctree_pred_testing,"auc") #Create AUC data
ctree_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
ctree_auc_testing 

#### Lift chart
plotLift(ctree_probabilities[,2],  testing$default_0, cumulative = TRUE, n.buckets = 10) # Plot Lift chart


#------------------------------------
### RPART
###

CART_cp = rpart.control(cp = 0.0005) #set cp to a small number to "grow" a large tree

rpart_tree<-rpart(default_0~.,data=training, method="class", control=CART_cp) #"Grow" a tree on training data

prunned_rpart_tree<-prune(rpart_tree, cp=0.001) #Prune the tree. Play with cp to see how the resultant tree changes
plot(as.party(prunned_rpart_tree), type = "extended",gp = gpar(fontsize = 7)) #Plotting the tree (adjust fontsize if needed)

printcp(rpart_tree) # Understand the relationship between the cross-validated error, size of the tree and cp
plotcp(rpart_tree) # As a rule of thumb pick up the largest cp which does not give a substantial drop in error

rpart_prediction_class<-predict(prunned_rpart_tree,newdata=testing, type="class") #Predict classification (for confusion matrix)
confusionMatrix(rpart_prediction_class,testing$default_0,positive = "1") #Display confusion matrix

rpart_probabilities_testing <-predict(prunned_rpart_tree,newdata=testing,type = "prob") #Predict probabilities
rpart_pred_testing <- prediction(rpart_probabilities_testing[,2], testing$default_0) #Calculate errors
rpart_ROC_testing <- performance(rpart_pred_testing,"tpr","fpr") #Create ROC curve data
plot(rpart_ROC_testing) #Plot ROC curve

auc.tmp <- performance(rpart_pred_testing,"auc") #Create AUC data
rpart_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
rpart_auc_testing #Display AUC value

plotLift(rpart_prediction_class,  testing$default_0, cumulative = TRUE, n.buckets = 10) # Plot Lift chart


#------------------------------------------
### Random Forest


model_forest <- randomForest(default_0~ ., data=training, 
                             type="classification",
                             importance=TRUE,
                             ntree = 4000,           # hyperparameter: number of trees in the forest
                             mtry = 10,             # hyperparameter: number of random columns to grow each tree
                             nodesize = 10,         # hyperparameter: min number of datapoints on the leaf of each tree
                             maxnodes = 10,         # hyperparameter: maximum number of leafs of a tree
                             cutoff = c(0.5, 0.5)   # hyperparameter: how the voting works; (0.5, 0.5) means majority vote
) 

plot(model_forest)  # plots error as a function of number of trees in the forest; use print(model_forest) to print the values on the plot

varImpPlot(model_forest) # plots variable importances; use importance(model_forest) to print the values

###Finding predicitons: probabilities and classification
forest_probabilities<-predict(model_forest,newdata=testing,type="prob") #Predict probabilities -- an array with 2 columns: for not retained (class 0) and for retained (class 1)
forest_classification<-rep("1",4000)
forest_classification[forest_probabilities[,2]<0.5]="0" #Predict classification using 0.5 threshold.
forest_classification<-as.factor(forest_classification)

confusionMatrix(forest_classification,testing$default_0, positive="1") #Display confusion matrix. Note, confusion matrix actually displays a better accuracy with threshold of 50%

####ROC Curve
forest_ROC_prediction <- prediction(forest_probabilities[,2], testing$default_0) #Calculate errors
forest_ROC <- performance(forest_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(forest_ROC) #Plot ROC curve

####AUC (area under curve)
AUC.tmp <- performance(forest_ROC_prediction,"auc") #Create AUC data
forest_AUC <- as.numeric(AUC.tmp@y.values) #Calculate AUC
forest_AUC #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

#### Lift chart
plotLift(forest_probabilities[,2],  testing$default_0, cumulative = TRUE, n.buckets = 10) # Plot Lift chart

### An alternative way is to plot a Lift curve not by buckets, but on all data points
Lift_forest <- performance(forest_ROC_prediction,"lift","rpp")
plot(Lift_forest)


#------------------------------------------
### XGBoost

training.x <-model.matrix(default_0~ ., data = training)
testing.x <-model.matrix(default_0~ ., data = testing)

model_XGboost<-xgboost(data = data.matrix(training.x[,-1]), 
                       label = as.numeric(as.character(training$default_0)), 
                       eta = 0.1,       # hyperparameter: learning rate 
                       max_depth = 20,  # hyperparameter: size of a tree in each boosting iteration
                       nround=50,       # hyperparameter: number of boosting iterations  
                       objective = "binary:logistic"
)

XGboost_prediction<-predict(model_XGboost,newdata=testing.x[,-1], type="response") #Predict classification (for confusion matrix)
confusionMatrix(as.factor(ifelse(XGboost_prediction>0.22,1,0)),testing$default_0,positive="1") #Display confusion matrix

####ROC Curve
XGboost_pred_testing <- prediction(XGboost_prediction, testing$default_0) #Calculate errors
XGboost_ROC_testing <- performance(XGboost_pred_testing,"tpr","fpr") #Create ROC curve data
plot(XGboost_ROC_testing) #Plot ROC curve

####AUC
auc.tmp <- performance(XGboost_pred_testing,"auc") #Create AUC data
XGboost_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
XGboost_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

#### Lift chart
plotLift(XGboost_prediction, testing$default_0, cumulative = TRUE, n.buckets = 10) # Plot Lift chart


#---------------------------------
#Load new data

creditDataNew <- read.csv("Credit Data New.csv")
str(creditDataNew)

#------------------------------------
#Transform new data
creditDataNew$X<- NULL #Only when there's X column that appears unknown from where
colnames(creditDataNew)
names(creditDataNew)[names(creditDataNew)=="ï..ID"] <- "ID"
#Combine 4,5,6 to 0 because all 3 doesn't translate to a meaningful difference
creditDataNew$EDUCATION <- as.character(creditDataNew$EDUCATION)
creditDataNew$EDUCATION[creditDataNew$EDUCATION=="5"]<- "0"
creditDataNew$EDUCATION[creditDataNew$EDUCATION=="6"]<- "0"
creditDataNew$EDUCATION[creditDataNew$EDUCATION=="4"]<- "0"
creditDataNew$EDUCATION <- as.factor(creditDataNew$EDUCATION)

creditDataNew$MARRIAGE <- as.character(creditDataNew$MARRIAGE)
creditDataNew$MARRIAGE[creditDataNew$MARRIAGE=="3"]<- "0"
creditDataNew$MARRIAGE <- as.factor(creditDataNew$MARRIAGE)

#Remove all PAY_1 values of -1 and -2 because it has inherently different meaning than 0 and above
creditDataNew$PAY_ADJ1<-creditDataNew$PAY_1
creditDataNew$PAY_ADJ2<-creditDataNew$PAY_2
creditDataNew$PAY_ADJ3<-creditDataNew$PAY_3
creditDataNew$PAY_ADJ4<-creditDataNew$PAY_4
creditDataNew$PAY_ADJ5<-creditDataNew$PAY_5
creditDataNew$PAY_ADJ6<-creditDataNew$PAY_6
creditDataNew$PAY_ADJ1[creditDataNew$PAY_ADJ1<0]<-0
creditDataNew$PAY_ADJ2[creditDataNew$PAY_ADJ2<0]<-0
creditDataNew$PAY_ADJ3[creditDataNew$PAY_ADJ3<0]<-0
creditDataNew$PAY_ADJ4[creditDataNew$PAY_ADJ4<0]<-0
creditDataNew$PAY_ADJ5[creditDataNew$PAY_ADJ5<0]<-0
creditDataNew$PAY_ADJ6[creditDataNew$PAY_ADJ6<0]<-0

#Normalise bill amount based on limit balance
creditDataNew$BILL_P1<- creditDataNew$BILL_AMT1 / creditDataNew$LIMIT_BAL
creditDataNew$BILL_P2<- creditDataNew$BILL_AMT2 / creditDataNew$LIMIT_BAL
creditDataNew$BILL_P3<- creditDataNew$BILL_AMT3 / creditDataNew$LIMIT_BAL
creditDataNew$BILL_P4<- creditDataNew$BILL_AMT4 / creditDataNew$LIMIT_BAL
creditDataNew$BILL_P5<- creditDataNew$BILL_AMT5 / creditDataNew$LIMIT_BAL
creditDataNew$BILL_P6<- creditDataNew$BILL_AMT6 / creditDataNew$LIMIT_BAL

#Normalise payment amount based on limit balance
creditDataNew$PAY_P1<- creditDataNew$PAY_AMT1 / creditDataNew$LIMIT_BAL
creditDataNew$PAY_P2<- creditDataNew$PAY_AMT2 / creditDataNew$LIMIT_BAL
creditDataNew$PAY_P3<- creditDataNew$PAY_AMT3 / creditDataNew$LIMIT_BAL
creditDataNew$PAY_P4<- creditDataNew$PAY_AMT4 / creditDataNew$LIMIT_BAL
creditDataNew$PAY_P5<- creditDataNew$PAY_AMT5 / creditDataNew$LIMIT_BAL
creditDataNew$PAY_P6<- creditDataNew$PAY_AMT6 / creditDataNew$LIMIT_BAL

#Get the maximum lateness of the creditor over the past 6 months and for everyone who is never late, put 0
creditDataNew$MAX_LATE<- pmax(creditDataNew$PAY_1,creditDataNew$PAY_2,creditDataNew$PAY_3,creditDataNew$PAY_4,creditDataNew$PAY_5,creditDataNew$PAY_6)
creditDataNew$MAX_LATE[creditDataNew$MAX_LATE<0]<-0

#Separate out all -2 from PAY_1 in a new variable
creditDataNew$PAY_ONTIME_1<-creditDataNew$PAY_1
creditDataNew$PAY_ONTIME_2<-creditDataNew$PAY_2
creditDataNew$PAY_ONTIME_3<-creditDataNew$PAY_3
creditDataNew$PAY_ONTIME_4<-creditDataNew$PAY_4
creditDataNew$PAY_ONTIME_5<-creditDataNew$PAY_5
creditDataNew$PAY_ONTIME_6<-creditDataNew$PAY_6

creditDataNew$PAY_ONTIME_1[creditDataNew$PAY_ONTIME_1!=-2]<-0
creditDataNew$PAY_ONTIME_1[creditDataNew$PAY_ONTIME_1==-2]<-1

creditDataNew$PAY_ONTIME_2[creditDataNew$PAY_ONTIME_2!=-2]<-0
creditDataNew$PAY_ONTIME_2[creditDataNew$PAY_ONTIME_2==-2]<-1

creditDataNew$PAY_ONTIME_3[creditDataNew$PAY_ONTIME_3!=-2]<-0
creditDataNew$PAY_ONTIME_3[creditDataNew$PAY_ONTIME_3==-2]<-1

creditDataNew$PAY_ONTIME_4[creditDataNew$PAY_ONTIME_4!=-2]<-0
creditDataNew$PAY_ONTIME_4[creditDataNew$PAY_ONTIME_4==-2]<-1

creditDataNew$PAY_ONTIME_5[creditDataNew$PAY_ONTIME_5!=-2]<-0
creditDataNew$PAY_ONTIME_5[creditDataNew$PAY_ONTIME_5==-2]<-1

creditDataNew$PAY_ONTIME_6[creditDataNew$PAY_ONTIME_6!=-2]<-0
creditDataNew$PAY_ONTIME_6[creditDataNew$PAY_ONTIME_6==-2]<-1

#Separate out all -1 from PAY_1 in a new variable
creditDataNew$PAY_FULL_1<-creditDataNew$PAY_1
creditDataNew$PAY_FULL_2<-creditDataNew$PAY_2
creditDataNew$PAY_FULL_3<-creditDataNew$PAY_3
creditDataNew$PAY_FULL_4<-creditDataNew$PAY_4
creditDataNew$PAY_FULL_5<-creditDataNew$PAY_5
creditDataNew$PAY_FULL_6<-creditDataNew$PAY_6

creditDataNew$PAY_FULL_1[creditDataNew$PAY_FULL_1!=-1]<-0
creditDataNew$PAY_FULL_1[creditDataNew$PAY_FULL_1==-1]<-1

creditDataNew$PAY_FULL_2[creditDataNew$PAY_FULL_2!=-1]<-0
creditDataNew$PAY_FULL_2[creditDataNew$PAY_FULL_2==-1]<-1

creditDataNew$PAY_FULL_3[creditDataNew$PAY_FULL_3!=-1]<-0
creditDataNew$PAY_FULL_3[creditDataNew$PAY_FULL_3==-1]<-1

creditDataNew$PAY_FULL_4[creditDataNew$PAY_FULL_4!=-1]<-0
creditDataNew$PAY_FULL_4[creditDataNew$PAY_FULL_4==-1]<-1

creditDataNew$PAY_FULL_5[creditDataNew$PAY_FULL_5!=-1]<-0
creditDataNew$PAY_FULL_5[creditDataNew$PAY_FULL_5==-1]<-1

creditDataNew$PAY_FULL_6[creditDataNew$PAY_FULL_6!=-1]<-0
creditDataNew$PAY_FULL_6[creditDataNew$PAY_FULL_6==-1]<-1

#Create features that signifies whether they're late or not, just true or false
creditDataNew$PAY_LATE_1<-creditDataNew$PAY_1
creditDataNew$PAY_LATE_2<-creditDataNew$PAY_2
creditDataNew$PAY_LATE_3<-creditDataNew$PAY_3
creditDataNew$PAY_LATE_4<-creditDataNew$PAY_4
creditDataNew$PAY_LATE_5<-creditDataNew$PAY_5
creditDataNew$PAY_LATE_6<-creditDataNew$PAY_6

creditDataNew$PAY_LATE_1[creditDataNew$PAY_LATE_1>0]<-1
creditDataNew$PAY_LATE_1[creditDataNew$PAY_LATE_1<1]<-0

creditDataNew$PAY_LATE_2[creditDataNew$PAY_LATE_2>0]<-1
creditDataNew$PAY_LATE_2[creditDataNew$PAY_LATE_2<1]<-0

creditDataNew$PAY_LATE_3[creditDataNew$PAY_LATE_3>0]<-1
creditDataNew$PAY_LATE_3[creditDataNew$PAY_LATE_3<1]<-0

creditDataNew$PAY_LATE_4[creditDataNew$PAY_LATE_4>0]<-1
creditDataNew$PAY_LATE_4[creditDataNew$PAY_LATE_4<1]<-0

creditDataNew$PAY_LATE_5[creditDataNew$PAY_LATE_5>0]<-1
creditDataNew$PAY_LATE_5[creditDataNew$PAY_LATE_5<1]<-0

creditDataNew$PAY_LATE_6[creditDataNew$PAY_LATE_6>0]<-1
creditDataNew$PAY_LATE_6[creditDataNew$PAY_LATE_6<1]<-0

#Create normalised total bill and total payment in comparison to limit balance
creditDataNew$TotalBill <- (creditDataNew$BILL_AMT1 + creditDataNew$BILL_AMT2 + creditDataNew$BILL_AMT3 + creditDataNew$BILL_AMT4 + creditDataNew$BILL_AMT5 + creditDataNew$BILL_AMT6) / creditDataNew$LIMIT_BAL
creditDataNew$TotalPay <- (creditDataNew$PAY_AMT1 + creditDataNew$PAY_AMT2 + creditDataNew$PAY_AMT3 + creditDataNew$PAY_AMT4 + creditDataNew$PAY_AMT5 + creditDataNew$PAY_AMT6) / creditDataNew$LIMIT_BAL

#Create feature of how much money is left to be paid
creditDataNew$Leftover <- creditDataNew$TotalBill - creditDataNew$TotalPay

#Change variable type
creditDataNew$SEX<- as.factor(creditDataNew$SEX)
creditDataNew$EDUCATION<- as.factor(creditDataNew$EDUCATION)
creditDataNew$MARRIAGE<- as.factor(creditDataNew$MARRIAGE)
creditDataNew$PAY_1<- as.factor(creditDataNew$PAY_1)
creditDataNew$PAY_2<- as.factor(creditDataNew$PAY_2)
creditDataNew$PAY_3<- as.factor(creditDataNew$PAY_3)
creditDataNew$PAY_4<- as.factor(creditDataNew$PAY_4)
creditDataNew$PAY_5<- as.factor(creditDataNew$PAY_5)
creditDataNew$PAY_6<- as.factor(creditDataNew$PAY_6)

creditDataNew$PAY_ONTIME_1<-as.factor(creditDataNew$PAY_ONTIME_1)
creditDataNew$PAY_ONTIME_2<-as.factor(creditDataNew$PAY_ONTIME_2)
creditDataNew$PAY_ONTIME_3<-as.factor(creditDataNew$PAY_ONTIME_3)
creditDataNew$PAY_ONTIME_4<-as.factor(creditDataNew$PAY_ONTIME_4)
creditDataNew$PAY_ONTIME_5<-as.factor(creditDataNew$PAY_ONTIME_5)
creditDataNew$PAY_ONTIME_6<-as.factor(creditDataNew$PAY_ONTIME_6)
creditDataNew$PAY_FULL_1<-as.factor(creditDataNew$PAY_FULL_1)
creditDataNew$PAY_FULL_2<-as.factor(creditDataNew$PAY_FULL_2)
creditDataNew$PAY_FULL_3<-as.factor(creditDataNew$PAY_FULL_3)
creditDataNew$PAY_FULL_4<-as.factor(creditDataNew$PAY_FULL_4)
creditDataNew$PAY_FULL_5<-as.factor(creditDataNew$PAY_FULL_5)
creditDataNew$PAY_FULL_6<-as.factor(creditDataNew$PAY_FULL_6)
creditDataNew$PAY_LATE_1<-as.factor(creditDataNew$PAY_LATE_1)
creditDataNew$PAY_LATE_2<-as.factor(creditDataNew$PAY_LATE_2)
creditDataNew$PAY_LATE_3<-as.factor(creditDataNew$PAY_LATE_3)
creditDataNew$PAY_LATE_4<-as.factor(creditDataNew$PAY_LATE_4)
creditDataNew$PAY_LATE_5<-as.factor(creditDataNew$PAY_LATE_5)
creditDataNew$PAY_LATE_6<-as.factor(creditDataNew$PAY_LATE_6)
creditDataNew$PAY_ADJ1<-as.numeric(creditDataNew$PAY_ADJ1)
creditDataNew$PAY_ADJ2<-as.numeric(creditDataNew$PAY_ADJ2)
creditDataNew$PAY_ADJ3<-as.numeric(creditDataNew$PAY_ADJ3)
creditDataNew$PAY_ADJ4<-as.numeric(creditDataNew$PAY_ADJ4)
creditDataNew$PAY_ADJ5<-as.numeric(creditDataNew$PAY_ADJ5)
creditDataNew$PAY_ADJ6<-as.numeric(creditDataNew$PAY_ADJ6)

#Remove irrelevant features
creditDataNew$ID <- NULL
creditDataNew$PAY_1 <- NULL
creditDataNew$PAY_2 <- NULL
creditDataNew$PAY_3 <- NULL
creditDataNew$PAY_4 <- NULL
creditDataNew$PAY_5 <- NULL
creditDataNew$PAY_6 <- NULL

creditDataNew$BILL_AMT1 <- NULL
creditDataNew$BILL_AMT2 <- NULL
creditDataNew$BILL_AMT3 <- NULL
creditDataNew$BILL_AMT4 <- NULL
creditDataNew$BILL_AMT5 <- NULL
creditDataNew$BILL_AMT6 <- NULL

creditDataNew$PAY_AMT1 <- NULL
creditDataNew$PAY_AMT2 <- NULL
creditDataNew$PAY_AMT3 <- NULL
creditDataNew$PAY_AMT4 <- NULL
creditDataNew$PAY_AMT5 <- NULL
creditDataNew$PAY_AMT6 <- NULL

#------------------------------------
#Run model on new data

logistic_probabilities<-predict(model_logistic_stepwiseAIC,newdata=creditDataNew,type="response") #Predict probabilities
write.csv2(logistic_probabilities,"Predicted Probabilities.csv")
logistic_classification<-rep("1",1000)
logistic_classification[logistic_probabilities<0.22]="0" #Predict classification using 0.6073 threshold. Why 0.6073 - that's the average probability of being retained in the data. An alternative code: logistic_classification <- as.integer(logistic_probabilities > mean(testing$Retained.in.2012. == "1"))
write.csv2(logistic_classification,"Predicted Default-Log.csv")
export.model(model_logistic_stepwiseAIC, replace=FALSE)

saveRDS(model_logistic_stepwiseAIC,"AIC_Model.rds")
