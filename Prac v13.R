library(dplyr)
library(plyr)
library(gridExtra)
library(corrplot)
library(caret)
library(randomForest)
library(ggplot2)
library(scales)
library(reshape2)
library(GGally)
library(stringr)
library(pROC)
library(ggbiplot)
library(doParallel)
library(parallel)
library(MLmetrics)

# read in the data ####################################################################################

att <- data.frame(read.csv(file = 'IBM_Attrition_Data.csv'))
colnames(att)[1] <- c('Age')
att$BusinessTravel <- as.factor(str_replace_all(att$BusinessTravel, '[ -]', '_'))
att$BusinessTravel <- as.factor(str_replace_all(att$BusinessTravel, '[&]', 'And'))
att$Department <- as.factor(str_replace_all(att$Department, '[ -]', '_'))
att$Department <- as.factor(str_replace_all(att$Department, '[&]', 'And'))
att$EducationField <- as.factor(str_replace_all(att$EducationField, '[ -]', '_'))
att$EducationField <- as.factor(str_replace_all(att$EducationField, '[&]', 'And'))
att$JobRole <- as.factor(str_replace_all(att$JobRole, '[ -]', '_'))
att$JobRole <- as.factor(str_replace_all(att$JobRole, '[&]', 'And'))

glimpse(att, width = 75)

# =====================================================================================================

# any missing values ##################################################################################

sum(is.na(att))

# =====================================================================================================

# find and remove near zero variables ##################################################################

nearZeroVar(att, names = TRUE)
att <- att[,-c(nearZeroVar(att))]
glimpse(att)

# =====================================================================================================

# factorize and order features by type ################################################################

att$Education <- as.factor(att$Education)
att$Education <- revalue(att$Education, c('1' = 'Below_College', '2' = 'College', '3' = 'Bachelor',
                                          '4' = 'Master', '5' = 'Doctor'))

att$EnvironmentSatisfaction <- as.factor(att$EnvironmentSatisfaction)
att$EnvironmentSatisfaction <- revalue(att$EnvironmentSatisfaction, c('1' = 'Low', '2' = 'Medium',
                                                                      '3' = 'High', '4' = 'Very_High'))

att$JobInvolvement <- as.factor(att$JobInvolvement)
att$JobInvolvement <- revalue(att$JobInvolvement, c('1' = 'Low', '2' = 'Medium', '3' = 'High',
                                                    '4' = 'Very_High'))

att$JobLevel <- as.factor(att$JobLevel)

att$JobSatisfaction <- as.factor(att$JobSatisfaction)
att$JobSatisfaction <- revalue(att$JobSatisfaction, c('1' = 'Low', '2' = 'Medium', '3' = 'High',
                                                    '4' = 'Very_High'))

att$PerformanceRating <- as.factor(att$PerformanceRating)
att$PerformanceRating <- revalue(att$PerformanceRating, c('1' = 'Low', '2' = 'Good', '3' = 'Better', 
                                                          '4' = 'Best'))

att$RelationshipSatisfaction <- as.factor(att$RelationshipSatisfaction)
att$RelationshipSatisfaction <- revalue(att$RelationshipSatisfaction, c('1' = 'Low', '2' = 'Medium',
                                                                        '3' = 'High', 
                                                                        '4' = 'Very_High'))

att$StockOptionLevel <- as.factor(att$StockOptionLevel)


att$WorkLifeBalance <- as.factor(att$WorkLifeBalance)
att$WorkLifeBalance <- revalue(att$WorkLifeBalance, c('1' = 'Bad', '2' = 'Good', '3' = 'Better', 
                                                      '4' = 'Best'))

#att$MarStatGender <- as.factor(paste(att$MaritalStatus, att$Gender))
#att$BusTravGender <- as.factor(paste(att$BusinessTravel, att$Gender))
#att$BusTravMarStat <- as.factor(paste(att$BusinessTravel, att$MaritalStatus))

#tempFront <- c('EmployeeNumber', 'Attrition', 'BusinessTravel', 'BusTravGender', 'BusTravMarStat',
#               'Department', 'Education', 'EducationField', 'EnvironmentSatisfaction', 'Gender',
#               'JobInvolvement', 'JobLevel', 'JobRole', 'JobSatisfaction', 'MaritalStatus',
#               'MarStatGender', 'OverTime', 'PerformanceRating', 'RelationshipSatisfaction',
#               'StockOptionLevel', 'WorkLifeBalance')

tempFront <- c('EmployeeNumber', 'Attrition', 'BusinessTravel', 'Department', 'Education',
               'EducationField', 'EnvironmentSatisfaction', 'Gender', 'JobInvolvement', 'JobLevel',
               'JobRole', 'JobSatisfaction', 'MaritalStatus', 'OverTime', 'PerformanceRating',
               'RelationshipSatisfaction', 'StockOptionLevel', 'WorkLifeBalance')

att <- cbind(select(att, tempFront), select(att, -tempFront))

glimpse(att, 75)

# =====================================================================================================

# start multiple cores ################################################################################

cl <- makeCluster(detectCores() - 2)
registerDoParallel(cl)

# =====================================================================================================

# plot categorical ####################################################################################

pList <- list()
for (i in 3:18){
  
  local({
    
    i <- i
    myTab <- table(att$Attrition, att[,i])
    myPropTab <- prop.table(myTab, 2)
    myYes <- myPropTab[2,]
    myDF <- as.data.frame(myYes)
    myDF$Category <- row.names(myDF)
    colnames(myDF)[1] <- 'AttProp'
    row.names(myDF) <- seq.int(nrow(myDF))
    myDF <- myDF[,c(2, 1)]
    p <- ggplot(data = myDF, aes(x = Category, y = AttProp))
    p <- p + theme_classic()
    p <- p + geom_bar(stat = 'identity', fill = 'red3')
    p <- p + labs (title = names(att[i]), y = 'Proportion', caption = paste('Figure ', i - 2))
    p <- p + theme(plot.title = element_text(hjust = 0.5),
                   axis.title.x = element_blank(),
                   axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
                   axis.ticks.x = element_blank(),
                   legend.position = 'bottom')

    pList[[i - 2]] <<- p
    
    if((i-2) %% 4 == 0){
      grid.arrange(grobs = pList[(i-5):(i-2)], nrow = 2, newpage = TRUE)
      #grid.arrange(grobs = pList, nrow = 2, newpage = TRUE)
      #rm(pList)
      #pList <- list()
    }
    
  })
}
rm(pList)

# =====================================================================================================

# plot numerical ######################################################################################

pList <- list()
for (i in 19:32){
  
  local({
    
    i <- i
    p <- ggplot(data = att, aes_string(x = names(att[i])))
    p <- p + theme_classic()
    p <- p + geom_histogram(fill = 'red3')
    p <- p + labs (title = names(att[i]), y = 'Count', caption = paste('Figure ', i - 2))
    p <- p + theme(plot.title = element_text(hjust = 0.5),
                   axis.title.x = element_blank(),
                   axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
    
    pList[[i - 18]] <<- p
    
    if((i-18) %% 4 == 0){
      grid.arrange(grobs = pList[(i-21):(i-18)], nrow = 2, newpage = TRUE)
      #grid.arrange(grobs = pList, nrow = 2, newpage = TRUE)
      #rm(pList)
      #pList <- list()
    }
    
  })
}
rm(pList)


# =====================================================================================================

# plot numerical vs Attrition #########################################################################

pList <- list()
for (i in 19:32){
  
  local({
    
    i <- i
    p <- ggplot(data = att, aes_string(y = names(att[i]), x = 'Attrition', fill = 'Attrition'))
    p <- p + theme_classic()
    p <- p + geom_violin(trim = FALSE)
    p <- p + scale_fill_manual(values = c('green3', 'red'))
    p <- p + geom_boxplot(width = 0.025, fill = c('grey'))
    p <- p + labs (title = names(att[i]), y = 'Value', caption = paste('Figure ', i + 10))
    p <- p + theme(plot.title = element_text(hjust = 0.5),
                   axis.title.x = element_blank(),
                   axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
                   legend.position = 'None')
    
    pList[[i - 18]] <<- p
    
    if((i-18) %% 4 == 0){
      grid.arrange(grobs = pList[(i-21):(i-18)], nrow = 2, newpage = TRUE)
      #grid.arrange(grobs = pList, nrow = 2, newpage = TRUE)
      #rm(pList)
      #pList <- list()
    }
    
  })
}
rm(pList)

# =====================================================================================================

# correlation of numerical data #######################################################################

corrMatP <- cor(select(att, Age:YearsWithCurrManager), method = 'pearson')
diag(corrMatP) <- 0
corrplot(corrMatP, order = 'AOE', tl.col = 'black', col = grey.colors(100), title = 'Pearson',
         mar = c(0, 0, 1, 0))

corrMatK <- cor(select(att, Age:YearsWithCurrManager), method = 'kendall')
diag(corrMatK) <- 0
corrplot(corrMatK, order = 'AOE', tl.col = 'black', col = grey.colors(100), title = 'Kendall',
         mar = c(0, 0, 1, 0))

corrMatS <- cor(select(att, Age:YearsWithCurrManager), method = 'spearman')
diag(corrMatS) <- 0
corrplot(corrMatS, order = 'AOE', tl.col = 'black', col = grey.colors(100), title = 'Spearman',
         mar = c(0, 0, 1, 0))

ggpairs(select(att, c(Age, MonthlyIncome, NumCompaniesWorked, TotalWorkingYears, YearsAtCompany,
                      YearsInCurrentRole, YearsSinceLastPromotion, YearsWithCurrManager)))

#ggpairs(select(att, c(Attrition, Age, MonthlyIncome, NumCompaniesWorked, TotalWorkingYears,
#                      YearsAtCompany, YearsInCurrentRole, YearsSinceLastPromotion,
#                      YearsWithCurrManager)))

# =====================================================================================================

# build yX ############################################################################################

yX <- as.data.frame(select(att, Attrition:YearsWithCurrManager))

#yX <- select(yX, -c(TotalWorkingYears, YearsInCurrentRole, YearsWithCurrManager))
#yX$RelativeIncome <- yX$MonthlyIncome/mean(yX$MonthlyIncome)
#yX <- select(yX, -c(Gender))

#yX <- as.data.frame(select(att, Attrition, MonthlyIncome, Age, OverTime, DailyRate, TotalWorkingYears,
#                           JobRole, DistanceFromHome, HourlyRate, MonthlyRate, YearsAtCompany,
#                           PercentSalaryHike, EducationField, EnvironmentSatisfaction,
#                           StockOptionLevel, NumCompaniesWorked, JobSatisfaction, YearsWithCurrManager,
#                           WorkLifeBalance, YearsInCurrentRole, RelationshipSatisfaction,
#                           TrainingTimesLastYear, JobInvolvement, JobLevel, YearsSinceLastPromotion,
#                           Education, MaritalStatus, BusinessTravel))

# =====================================================================================================

# preprocess and split data ###########################################################################

set.seed(12345)

nrow(yX)

trainSize <- floor(0.75 * nrow(yX))

preProcYX <- preProcess(yX, method = c('BoxCox', 'center', 'scale'), fudge = 0)
newYX <- as.data.frame(predict(preProcYX, yX))
#newYX <- yX

trainSamp <- sample(seq_len(nrow(newYX)), size = trainSize)

trainYX <- as.data.frame(newYX[trainSamp,])
testYX <- as.data.frame(newYX[-trainSamp,])

# =====================================================================================================

# add dummy variables #################################################################################

trainX <- trainYX[,-1]
trainDumVars <- dummyVars(~., data = trainX)
trainX <- as.data.frame(predict(trainDumVars, trainX))

testX <- testYX[,-1]
testDumVars <- dummyVars(~., data = testX)
testX <- as.data.frame(predict(testDumVars, testX))

trainF <- data.frame(trainYX[,1], trainX)
colnames(trainF)[1] = 'Attrition'

testF <- data.frame(testYX[,1], testX)
colnames(testF)[1] = 'Attrition'

# =====================================================================================================

# penalized logistic regression model #################################################################

set.seed(12345)

myCtrlGlmnet <- trainControl(method = 'repeatedcv', number = 10, repeats = 5,
                             summaryFunction = mnLogLoss, classProbs = TRUE,
                             allowParallel = TRUE)
myModelGlmnet <- train(Attrition ~., data = trainF, trControl = myCtrlGlmnet,
                       metric = 'logLoss', method = 'glmnet')

logLossGlmnet <- min(myModelGlmnet$results$logLoss)

myCoef <- coef(myModelGlmnet$finalModel, myModelGlmnet$finalModel$lambdaOpt)

myVarImpGlmnet <- as.data.frame(varImp(myModelGlmnet)$importance, optional = TRUE)
myVarImpGlmnet$Variable <- row.names(myVarImpGlmnet)
row.names(myVarImpGlmnet) <- seq.int(nrow(myVarImpGlmnet))
myVarImpGlmnet <- select(myVarImpGlmnet, c(Variable, Overall))

myVarImpGlmnet[order(myVarImpGlmnet$Overall, decreasing = TRUE),]

probsGlmnet <- predict(myModelGlmnet, testX, type = 'prob')
predsGlmnet <- predict(myModelGlmnet, testX, type = 'raw')

mySamp <- newYX[1,]
mySampDumVars <- dummyVars(~., data = mySamp)
mySamp <- as.data.frame(predict(mySampDumVars, mySamp))
mySamp

mySampProbsGlmnet <- predict(myModelGlmnet, mySamp, type = 'prob')
mySampProbsGlmnet

mySamp2 <- mySamp
mySamp2$OverTime.No <- 1
mySamp2$OverTime.Yes <- 0

mySampProbsGlmnet2 <- predict(myModelGlmnet, mySamp2, type = 'prob')
mySampProbsGlmnet2

resGlmnet <- as.data.frame(testF$Attrition)
colnames(resGlmnet) <- 'obs'
resGlmnet$pred <- predsGlmnet
resGlmnet$No <- probsGlmnet$No
resGlmnet$Yes <- probsGlmnet$Yes

#LogLoss
LogLoss(resGlmnet$Yes, ifelse(resGlmnet$obs == 'Yes', 1, 0))

#ROC, Sens, Spec
glmnetTCS <- twoClassSummary(resGlmnet, lev = c('Yes', 'No'))
glmnetThreshold <- glmnetTCS[1]
glmnetThreshold
resGlmnet$pred <- as.factor(ifelse(resGlmnet$Yes >= glmnetThreshold, 'Yes', 'No'))

#Accuracy, Kappa
postResample(resGlmnet$pred, resGlmnet$obs)

#Confusion matrix
confusionMatrix(resGlmnet$obs, resGlmnet$pred, positive = 'Yes')

#ROC, Sens, Spec
twoClassSummary(resGlmnet, lev = c('Yes', 'No'))

#AUC, Precision, Recall, F
prSummary(resGlmnet, lev = c('Yes', 'No'))

#ROC curve
rocObs <- ifelse(resGlmnet$obs == 'Yes', 1, 0)
plot.roc(rocObs, resGlmnet$Yes)

#Area Under ROC curve
auc(roc(rocObs, resGlmnet$Yes))

# =====================================================================================================

# random forest model #################################################################################

set.seed(12345)

myCtrlRF <- trainControl(method = 'repeatedcv', number = 10, repeats = 5,
                           summaryFunction = mnLogLoss, classProbs = TRUE,
                           allowParallel = TRUE)
myModelRF <- train(Attrition ~., data = trainF, trControl = myCtrlRF,
                     metric = 'logLoss', method = 'rf')

logLossRF <- min(myModelRF$results$logLoss)

myVarImpRF <- as.data.frame(varImp(myModelRF)$importance, optional = TRUE)
myVarImpRF$Variable <- row.names(myVarImpRF)
row.names(myVarImpRF) <- seq.int(nrow(myVarImpRF))
myVarImpRF <- select(myVarImpRF, c(Variable, Overall))

myVarImpRF[order(myVarImpRF$Overall, decreasing = TRUE),]

probsRF <- predict(myModelRF, testX, type = 'prob')
predsRF <- predict(myModelRF, testX, type = 'raw')

resRF <- as.data.frame(testF$Attrition)
colnames(resRF) <- 'obs'
resRF$pred <- predsRF
resRF$No <- probsRF$No
resRF$Yes <- probsRF$Yes

#LogLoss
LogLoss(resRF$Yes, ifelse(resRF$obs == 'Yes', 1, 0))

#ROC, Sens, Spec
rFTCS <- twoClassSummary(resRF, lev = c('Yes', 'No'))
rFThreshold <- rFTCS[1]
rFThreshold
resRF$pred <- as.factor(ifelse(resRF$Yes >= rFThreshold, 'Yes', 'No'))

#Accuracy, Kappa
postResample(resRF$pred, resRF$obs)

#Confusion matrix
confusionMatrix(resRF$obs, resRF$pred, positive = 'Yes')

#ROC, Sens, Spec
twoClassSummary(resRF, lev = c('Yes', 'No'))

#AUC, Precision, Recall, F
prSummary(resRF, lev = c('Yes', 'No'))

#ROC curve
rocObs <- ifelse(resRF$obs == 'Yes', 1, 0)
plot.roc(rocObs, resRF$Yes)

#Area Under ROC curve
auc(roc(rocObs, resRF$Yes))

# =====================================================================================================

# gradient boosted trees ##############################################################################

set.seed(12345)

myCtrlBoost <- trainControl(method = 'repeatedcv', number = 10, repeats = 5,
                         summaryFunction = mnLogLoss, classProbs = TRUE,
                         allowParallel = TRUE)
myModelBoost <- train(Attrition ~., data = trainF, trControl = myCtrlBoost,
                   metric = 'logLoss', method = 'xgbTree')

logLossBoost <- min(myModelBoost$results$logLoss)

myVarImpBoost <- as.data.frame(varImp(myModelBoost)$importance, optional = TRUE)
myVarImpBoost$Variable <- row.names(myVarImpBoost)
row.names(myVarImpBoost) <- seq.int(nrow(myVarImpBoost))
myVarImpBoost <- select(myVarImpBoost, c(Variable, Overall))

myVarImpBoost[order(myVarImpBoost$Overall, decreasing = TRUE),]

probsBoost <- predict(myModelBoost, testX, type = 'prob')
predsBoost <- predict(myModelBoost, testX, type = 'raw')

resBoost <- as.data.frame(testF$Attrition)
colnames(resBoost) <- 'obs'
resBoost$pred <- predsBoost
resBoost$No <- probsBoost$No
resBoost$Yes <- probsBoost$Yes

#LogLoss
LogLoss(resBoost$Yes, ifelse(resBoost$obs == 'Yes', 1, 0))

#ROC, Sens, Spec
boostTCS <- twoClassSummary(resBoost, lev = c('Yes', 'No'))
boostThreshold <- boostTCS[1]
boostThreshold
resBoost$pred <- as.factor(ifelse(resBoost$Yes >= boostThreshold, 'Yes', 'No'))

#Accuracy, Kappa
postResample(resBoost$pred, resBoost$obs)

#Confusion matrix
confusionMatrix(resBoost$obs, resBoost$pred, positive = 'Yes')

#ROC, Sens, Spec
twoClassSummary(resBoost, lev = c('Yes', 'No'))

#AUC, Precision, Recall, F
prSummary(resBoost, lev = c('Yes', 'No'))

#ROC curve
rocObs <- ifelse(resBoost$obs == 'Yes', 1, 0)
plot.roc(rocObs, resBoost$Yes)

#Area Under ROC curve
auc(roc(rocObs, resBoost$Yes))

# =====================================================================================================

# support vector machine ##############################################################################

set.seed(12345)

myCtrlSVM <- trainControl(method = 'repeatedcv', number = 10, repeats = 5,
                            summaryFunction = mnLogLoss, classProbs = TRUE,
                            allowParallel = TRUE)
myModelSVM <- train(Attrition ~., data = trainF, trControl = myCtrlSVM,
                      metric = 'logLoss', method = 'svmLinear')

logLossSVM <- min(myModelSVM$results$logLoss)

myVarImpSVM <- as.data.frame(varImp(myModelSVM)$importance, optional = TRUE)
myVarImpSVM$Variable <- row.names(myVarImpSVM)
row.names(myVarImpSVM) <- seq.int(nrow(myVarImpSVM))
myVarImpSVM <- select(myVarImpSVM, c(Variable, Yes))
colnames(myVarImpSVM)[2] <- 'Overall'

myVarImpSVM[order(myVarImpSVM$Overall, decreasing = TRUE),]

probsSVM <- predict(myModelSVM, testX, type = 'prob')
predsSVM <- predict(myModelSVM, testX, type = 'raw')

resSVM <- as.data.frame(testF$Attrition)
colnames(resSVM) <- 'obs'
resSVM$pred <- predsSVM
resSVM$No <- probsSVM$No
resSVM$Yes <- probsSVM$Yes

#LogLoss
LogLoss(resSVM$Yes, ifelse(resSVM$obs == 'Yes', 1, 0))

#ROC, Sens, Spec
sVMTCS <- twoClassSummary(resSVM, lev = c('Yes', 'No'))
sVMThreshold <- sVMTCS[1]
sVMThreshold
resSVM$pred <- as.factor(ifelse(resSVM$Yes >= sVMThreshold, 'Yes', 'No'))

#Accuracy, Kappa
postResample(resSVM$pred, resSVM$obs)

#Confusion matrix
confusionMatrix(resSVM$obs, resSVM$pred, positive = 'Yes')

#ROC, Sens, Spec
twoClassSummary(resSVM, lev = c('Yes', 'No'))

#AUC, Precision, Recall, F
prSummary(resSVM, lev = c('Yes', 'No'))

#ROC curve
rocObs <- ifelse(resSVM$obs == 'Yes', 1, 0)
plot.roc(rocObs, resSVM$Yes)

#Area Under ROC curve
auc(roc(rocObs, resSVM$Yes))

# =====================================================================================================

# support vector machine radial #######################################################################

set.seed(12345)

myCtrlSVMRad <- trainControl(method = 'repeatedcv', number = 10, repeats = 5,
                          summaryFunction = mnLogLoss, classProbs = TRUE,
                          allowParallel = TRUE)
myModelSVMRad <- train(Attrition ~., data = trainF, trControl = myCtrlSVMRad,
                    metric = 'logLoss', method = 'svmRadial')

logLossSVMRad <- min(myModelSVMRad$results$logLoss)

myVarImpSVMRad <- as.data.frame(varImp(myModelSVMRad)$importance, optional = TRUE)
myVarImpSVMRad$Variable <- row.names(myVarImpSVMRad)
row.names(myVarImpSVMRad) <- seq.int(nrow(myVarImpSVMRad))
myVarImpSVMRad <- select(myVarImpSVMRad, c(Variable, Yes))
colnames(myVarImpSVMRad)[2] <- 'Overall'

myVarImpSVMRad[order(myVarImpSVMRad$Overall, decreasing = TRUE),]

probsSVMRad <- predict(myModelSVMRad, testX, type = 'prob')
predsSVMRad <- predict(myModelSVMRad, testX, type = 'raw')

resSVMRad <- as.data.frame(testF$Attrition)
colnames(resSVMRad) <- 'obs'
resSVMRad$pred <- predsSVMRad
resSVMRad$No <- probsSVMRad$No
resSVMRad$Yes <- probsSVMRad$Yes

#LogLoss
LogLoss(resSVMRad$Yes, ifelse(resSVMRad$obs == 'Yes', 1, 0))

#ROC, Sens, Spec
sVMRadTCS <- twoClassSummary(resSVMRad, lev = c('Yes', 'No'))
sVMRadThreshold <- sVMRadTCS[1]
sVMRadThreshold
resSVMRad$pred <- as.factor(ifelse(resSVMRad$Yes >= sVMRadThreshold, 'Yes', 'No'))

#Accuracy, Kappa
postResample(resSVMRad$pred, resSVMRad$obs)

#Confusion matrix
confusionMatrix(resSVMRad$obs, resSVMRad$pred, positive = 'Yes')

#ROC, Sens, Spec
twoClassSummary(resSVMRad, lev = c('Yes', 'No'))

#AUC, Precision, Recall, F
prSummary(resSVMRad, lev = c('Yes', 'No'))

#ROC curve
rocObs <- ifelse(resSVMRad$obs == 'Yes', 1, 0)
plot.roc(rocObs, resSVMRad$Yes)

#Area Under ROC curve
auc(roc(rocObs, resSVMRad$Yes))

# =====================================================================================================

# end multiple cores ##################################################################################

stopCluster(cl)

# =====================================================================================================