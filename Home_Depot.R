### This script is to check data files from Home Depot

## Thoughts:
## 1. Since the evaluating metric is RMSE, cutting possible relevance value
## into small # of factors may help.

library(plyr)
library(dplyr)
library(lubridate)
library(caret)
library(ggplot2)

# RMSE <- sqrt(mean((y-y_pred)^2))

################# Checking Training Data #################
##
## load data and briefly explore
data.hd <- read.csv(file = 'train.csv', stringsAsFactors = F)
# check NAs
sapply(data.hd, function(x) sum(is.na(x))) # no NA
# check near zero variables
nearZeroVar(data.hd, saveMetrics = T) # no NZV
# check class variable
length(unique(data.hd$relevance)) # only 13 possible values
unique(data.hd$relevance)
sort(table(data.hd$relevance), decreasing = T) # imbalanced

## check variables
# id
length(unique(data.hd$id)) # all different, drop it when modeling
# product_uid
length(unique(data.hd$product_uid)) # 54,667 different uid


################# Checking Test Data #################
##
data.test.final <- read.csv(file = 'test.csv')


################# Checking Sample Submission #################
##
sampleSub = read.csv(file = 'sample_submission.csv')


################# Checking Products Attributes  #################
##
data.attr <- read.csv(file = 'attributes.csv', stringsAsFactors = F) # 2,044,803 obs & 3 variables
# name: attribute name

sort(table(data.attr$product_uid), decreasing = T) # imbalanced
unique(table(data.attr$product_uid))

## extract each product's # of attributes
productList = unique(data.attr$product_uid)
attrCount = c()
countIndex = 35000
for (p in productList[35001:86264]) {
      countIndex = countIndex +1
      sub.current = subset(data.attr, data.attr$product_uid == p)
      attrCount[countIndex] = nrow(sub.current)
      print(countIndex) # for tracking
}
# attrCount = data.frame(product_uid = productList, attrCount = attrCount)
# save(attrCount, file = 'attrCount.Rda')
# load(file = 'attrCount.Rda')


################# Checking Products Descriptions #################
##
data.product <- read.csv(file = 'product_descriptions.csv', stringsAsFactors = F) # 124,428 obs & 2 variables


plot.test = data.frame(id = data.hd$product_uid - 100000, score = data.hd$relevance)
ggplot(data = plot.test) +
      geom_point(x = plot.test$id, y = plot.test$score)
plot(plot.test$id, plot.test$score, pch = 20)
