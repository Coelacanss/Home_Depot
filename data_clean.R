### This script is to clean data for Home Depot
## NOTE: CHECK SPELLING USING PACKAGE 'qdap'

library(plyr)
library(dplyr)
library(lubridate)
library(caret)
library(ggplot2)
library(tm)
library(SnowballC)
library(e1071)
library(caret)
data.hd <- read.csv(file = 'train.csv', stringsAsFactors = F)
data.attr <- read.csv(file = 'attributes.csv', stringsAsFactors = F) # 2,044,803 obs & 3 variables
data.product <- read.csv(file = 'product_descriptions.csv', stringsAsFactors = F) # 124,428 obs & 2 variables
load(file = 'attrCount.Rda')
load(file = 'termLength.Rda')
load(file = 'termInTitleRatio.Rda')
load(file = 'termInTitleDummy.Rda')
load(file = 'termInDescRatio.Rda')
load(file = 'termInDescDummy.Rda')
load(file = 'termInTitleCount.Rda')
load(file = 'termInDescCount.Rda')

################### extract word stem of search_term ##################
###
search_term = data.hd$search_term
search.raw = VCorpus(VectorSource(search_term))

time.now = proc.time()
search.new = tm_map(search.raw, content_transformer(tolower))
# remove all numbers
all.reviews = tm_map(all.reviews, content_transformer(removeNumbers))
#remove all punctuation
all.reviews = tm_map(all.reviews, content_transformer(removePunctuation))
#remove all stopwords
all.reviews = tm_map(all.reviews, removeWords, stopwords("english"))
#now let us stem it
#library(SnowballC)
all.reviews = tm_map(all.reviews, stemDocument)
#remove whitespace
all.reviews = tm_map(all.reviews, content_transformer(stripWhitespace))
proc.time() - time.now
# lets take a look
writeLines(as.character(all.reviews[[5]]))
# save(all.reviews, file=file.choose())

# now make the document term matrix
dtm = DocumentTermMatrix(all.reviews)
dtm


dtm.99 = removeSparseTerms(dtm, 0.99)
dtm.99

dtm.df = as.data.frame(as.matrix(dtm.99))

###################################################################

uidList = unique(data.hd$product_uid)
showit <- function(num) {
      for (uid in uidList[num]) {
            print(uid)
            print('***************************************')
            data.sub = data.hd[data.hd$product_uid == uid,]
            for (i in 1:nrow(data.sub)){
                  print(data.sub$product_title[i])
                  print(data.sub$search_term[i])
                  print(data.sub$relevance[i])
                  print('***************************************')   
            }
            desc = data.product[data.product$product_uid == uid, 2]
            print(desc)
            print('***************************************')
      }
}
showit(13)
grepl('grate', desc, ignore.case = T)
#
data.check <- read.csv(file = 'df_all.csv', stringsAsFactors = F)


## check correlation
corr = cor(data.raw, method='spearman', use='pairwise')
corr
findCorrelation(corr, verbose=T, names=T, cutoff=0.6)

data.submit = read.csv(file = 'submission.csv', stringsAsFactors = F)
write.csv(data.submit, file = 'submission3.csv', quote=FALSE, row.names = FALSE)


