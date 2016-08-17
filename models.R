### This script is to build a linear model for Home Depot
### Metric: RMSE <- sqrt(mean((y-y_pred)^2))

library(randomForest)
library(caret)
library(xgboost)
library(readr)
library(stringr)
library(car)
load(file = 'attrCount.Rda')
load(file = 'termLength.Rda')
load(file = 'termInTitleRatio.Rda')
load(file = 'termInTitleDummy.Rda')
load(file = 'termInDescRatio.Rda')
load(file = 'termInDescDummy.Rda')
load(file = 'termInTitleCount.Rda')
load(file = 'termInDescCount.Rda')
load(file = 'termFirstInTitle.Rda')
load(file = 'termFirstInDesc.Rda')

RMSE <- function(pred, reference) {
      result = sqrt(mean((pred-reference)^2, na.rm = T))
      return(result)
}

data.raw = data.frame(termLength = termLength, 
                      termInTitleRatio = termInTitleRatio, termInTitleDummy = termInTitleDummy, 
                      termInDescRatio = termInDescRatio, termInDescDummy = termInDescDummy,
                      termInTitleCount = termInTitleCount, termInDescCount = termInDescCount,
                      termFirstInTitle = termFirstInTitle, termFirstInDesc = termFirstInDesc,
                      relevance = data.hd$relevance)

inSample = sample(1:nrow(data.raw), 0.8*nrow(data.raw))
data.train = data.raw[inSample,]
data.test = data.raw[-inSample,]

#################### simple Linear Model #######################
data.train2 = data.train
drop.index = which((data.train2$relevance == 1.25 | data.train2$relevance == 1.5 | data.train2$relevance == 1.75 |
                          data.train2$relevance == 2.25 | data.train2$relevance == 2.5 | data.train2$relevance == 2.75))
data.train2 = data.train2[-drop.index,]
set.seed(1)
fit.lm = lm(relevance ~., data = data.train2)
pred = predict(fit.lm, data.test)
RMSE(pred, data.test$relevance) # 0.4937322


#################### simple Generalized Linear Model ########################
## link = 'identity' is the best. So failed
## http://www.statmethods.net/advstats/glm.html
data.train2 = data.train
drop.index = which((data.train2$relevance == 1.25 | data.train2$relevance == 1.5 | data.train2$relevance == 1.75 |
                          data.train2$relevance == 2.25 | data.train2$relevance == 2.5 | data.train2$relevance == 2.75))
data.train2 = data.train2[-drop.index,]
set.seed(1)
fit.glm = glm(relevance ~., data = data.train2, family = 'quasi')
pred = predict(fit.glm, data.test)
RMSE(pred, data.test$relevance) # 0.5013096


####################### Random Forest #######################################
## 1 numeric class variable
data.train2 = data.train
drop.index = which((data.train2$relevance == 1.25 | data.train2$relevance == 1.5 | data.train2$relevance == 1.75 |
                          data.train2$relevance == 2.25 | data.train2$relevance == 2.5 | data.train2$relevance == 2.75))
data.train2 = data.train2[-drop.index,]
## training control
trc <- trainControl(method = "repeatedcv",
                    number = 10,
                    repeats = 1,
                    classProbs = TRUE,
                    allowParallel = T)
grid = expand.grid(mtry = c(2,3,4))  # 2 or 3 seems better
## train model
time.now = proc.time()
set.seed(1)
tune.rf <- train(relevance ~ ., data=data.train2, 
                 method = "rf", 
                 metric = 'rmse',
                 do.trace = T,
                 tuneGrid = grid,
                 trControl = trc)
proc.time() - time.now
fit.rf.num = tune.rf$finalModel
save(fit.rf.num, file = 'fit.rf.num.Rda')

## predict
pred = predict(fit.rf.num, data.test)
RMSE(pred, data.test$relevance)  # 0.4887343

###########################################
## 2 convert into factor
drop.index = which((data.train2$relevance == 1.25 | data.train2$relevance == 1.5 | data.train2$relevance == 1.75 |
                          data.train2$relevance == 2.25 | data.train2$relevance == 2.5 | data.train2$relevance == 2.75))
data.train2 = data.train2[-drop.index,]
data.train2$relevance = as.factor(data.train2$relevance)
levels(data.train2$relevance) <- c('a','b','c','d','e','f','g')
## training control
trc <- trainControl(method = "repeatedcv",
                    number = 5,
                    repeats = 1,
                    classProbs = TRUE,
                    allowParallel = T)
grid = expand.grid(mtry = 3)
## train model
time.now = proc.time()
set.seed(1)
tune.rf2 <- train(relevance ~ ., data=data.train2, 
                  method = "rf", 
                  metric = 'Accuracy',
                  do.trace = T,
                  tuneGrid = grid,
                  trControl = trc)
proc.time() - time.now
fit.rf.fac = tune.rf2$finalModel

## Predict
pred = predict(fit.rf.fac, data.test)
pred.convert = pred
levels(pred.convert) <- c('1','1.33','1.67','2','2.33','2.67','3')
pred.convert = as.numeric(as.character(pred.convert))
RMSE(pred.convert, data.test$relevance) # 0.6057528


########################## xgboost ########################
##
data.train2 = data.train
drop.index = which((data.train2$relevance == 1.25 | data.train2$relevance == 1.5 | data.train2$relevance == 1.75 |
             data.train2$relevance == 2.25 | data.train2$relevance == 2.5 | data.train2$relevance == 2.75))
data.train2 = data.train2[-drop.index,]
data.train2$relevance = as.factor(data.train2$relevance)
levels(data.train2$relevance) <- c('a','b','c','d','e','f','g')
labels <- recode(data.train2[,'relevance'], "'a'=0; 'b'=1; 'c'=2; 'd'=3; 'e'=4; 'f'=5; 'g'=6")
data.labels = as.character(labels)

time.now = proc.time()
set.seed(1)
xgb <- xgboost(data = data.matrix(data.train2[,-9]),
               label = data.labels, 
               eta = 0.1,
               max_depth = 6, 
               nround = 43, 
               subsample = 0.7,
               colsample_bytree = 0.7,
               objective = "multi:softprob",
               num_class = 7
               # nthread = 3
)
proc.time() - time.now

pred <- predict(xgb, data.matrix(data.test[,-9]))
pred <- as.data.frame(matrix(pred, nrow = 7))
rownames(pred) <- c('1','1.33','1.67','2','2.33','2.67','3')
pred.top <- as.vector(apply(pred, 2, function(x) names(sort(x)[7])))
pred.top = as.numeric(pred.top)
RMSE(pred.top, data.test$relevance)
confusionMatrix(pred.top,data.test$relevance)


