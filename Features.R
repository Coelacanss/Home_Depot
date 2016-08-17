### This script is to extract new features from Home Depot data

library(plyr)
library(dplyr)
library(lubridate)
library(caret)
library(ggplot2)
library(stringi)
library(stringr)

data.hd$search_term = stemDocument(tolower(data.hd$search_term))

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


## extract length of search term
termLength = c()
for (i in 1:74067) {
      term.current = data.hd$search_term[i]
      termLength[i] = length(unlist(str_extract_all(term.current, ' '))) + 1
      print(i)
}
# save(termLength, file = 'termLength.Rda')
# load(file = 'termLength.Rda')


## extract the ratio that terms in product_title
termInTitleRatio = c()
for (i in 1:74067) {
      termList = unlist(str_split(data.hd$search_term[i], ' '))
      term.total.current = length(termList)
      title.current = data.hd[i, 'product_title']
      numIn = 0
      for (term.current in termList) {
            if (grepl(term.current, title.current, ignore.case = T) == T) {
                  numIn = numIn + 1
            }
      }
      termInTitleRatio[i] = numIn / term.total.current
      print(i)
}
# BIAS: some term only includes a single letter or number and it still counts, like 'I', '8'
# save(termInTitleRatio, file = 'termInTitleRatio.Rda')
# load(file = 'termInTitleRatio.Rda')


## extract whether search term has any word in title, 1 for yes, 0 for no
termInTitleDummy = c()
for (i in 1:74067) {
      termInTitleDummy[i] = ifelse(termInTitleRatio[i] != 0, 1, 0)
      print(i)
}
# save(termInTitleDummy, file = 'termInTitleDummy.Rda')
# load(file = 'termInTitleDummy.Rda')


## extract the ratio that terms in description
termInDescRatio = c()
for (i in 1:74067) {
      termList = unlist(str_split(data.hd$search_term[i], ' '))
      term.total.current = length(termList)
      id.current = data.hd[i,'product_uid']
      desc.current = data.product[data.product$product_uid == id.current, 'product_description']
      numIn = 0
      for (term.current in termList) {
            if (grepl(term.current, desc.current, ignore.case = T) == T) {
                  numIn = numIn + 1
            }
      }
      termInDescRatio[i] = numIn / term.total.current
      print(i)
}
# BIAS: some term only includes a single letter or number and it still counts, like 'I', '8'
# save(termInDescRatio, file = 'termInDescRatio.Rda')
# load(file = 'termInDescRatio.Rda')


## extract whether search term has any word in desc, 1 for yes, 0 for no
termInDescDummy = c()
for (i in 1:74067) {
      termInDescDummy[i] = ifelse(termInDescRatio[i] != 0, 1, 0)
      print(i)
}
# save(termInDescDummy, file = 'termInDescDummy.Rda')
# load(file = 'termInDescDummy.Rda')


## extract # of term in title
termInTitleCount = c()
for (i in 1:74067) {
      termList = unlist(str_split(data.hd$search_term[i], ' '))
      term.total.current = length(termList)
      title.current = data.hd[i, 'product_title']
      numIn = 0
      for (term.current in termList) {
            if (grepl(term.current, title.current, ignore.case = T) == T) {
                  numIn = numIn + 1
            }
      }
      termInTitleCount[i] = numIn
      print(i)
}
# save(termInTitleCount, file = 'termInTitleCount.Rda')
# load(file = 'termInTitleCount.Rda')


## extract # of terms in product description
termInDescCount = c()
for (i in 1:74067) {
      termList = unlist(str_split(data.hd$search_term[i], ' '))
      term.total.current = length(termList)
      id.current = data.hd[i,'product_uid']
      desc.current = data.product[data.product$product_uid == id.current, 'product_description']
      numIn = 0
      for (term.current in termList) {
            if (grepl(term.current, desc.current, ignore.case = T) == T) {
                  numIn = numIn + 1
            }
      }
      termInDescCount[i] = numIn
      print(i)
}
# save(termInDescCount, file = 'termInDescCount.Rda')
# load(file = 'termInDescCount.Rda')


## extract the index of first term word found in product name
termFirstInTitle = c()
for (i in 1:74067) {
      termList = unlist(str_split(data.hd$search_term[i], ' '))
      term.length = length(termList)
      title.current = data.hd[i, 'product_title']
      termIndex = 0
      termFirstInTitle[i] = 0
      for (termIndex in 1:term.length) {
            term.current = termList[termIndex]
            if (grepl(term.current, title.current, ignore.case = T) == T) {
                  termFirstInTitle[i] = termIndex
                  break()
            }
      }
      print(i)
}
# save(termFirstInTitle, file = 'termFirstInTitle.Rda')
# load(file = 'termFirstInTitle.Rda')


## extract the index of first term word found in product description
termFirstInDesc = c()
for (i in 1:74067) {
      termList = unlist(str_split(data.hd$search_term[i], ' '))
      term.length = length(termList)
      desc.current = data.product[i,'product_description']
      termIndex = 0
      termFirstInDesc[i] = 0
      for (termIndex in 1:term.length) {
            term.current = termList[termIndex]
            if (grepl(term.current, desc.current, ignore.case = T) == T) {
                  termFirstInDesc[i] = termIndex
                  break()
            }
      }
      print(i)
}
# save(termFirstInDesc, file = 'termFirstInDesc.Rda')
# load(file = 'termFirstInDesc.Rda')


## extract brand of product
data.attr = na.omit(data.attr)
uidList = unique(data.attr$product_uid)
brandList = rep(NA, length(uidList))
i = 80000
for (uid in uidList[80001:86263]) {
      i = i + 1
      data.sub = data.attr[data.attr$product_uid == uid,]
      if ('MFG Brand Name' %in% data.sub$name) {
            brandList[i] = data.sub[which(data.sub$name == 'MFG Brand Name'), 'value']
      }
      else {brandList[i] = 'unknown'}
      print(i)
}
# save(brandList, file = 'brandList.Rda')
product_brand <- data.frame(product_uid = uidList, brand = brandList)
# save(product_brand, file = 'product_brand.Rda')
