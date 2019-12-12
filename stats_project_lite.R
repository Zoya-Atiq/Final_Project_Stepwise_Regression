library(tidyverse)
library(mlbench)

data("BostonHousing2")
bostondf = BostonHousing2 %>% select(-c(medv, cmedv, town, tract))
bostondf = data.frame(cmedv = BostonHousing2$cmedv, bostondf)

pvals = data.frame(predictor = NA, pval = NA)

for (i in 2:ncol(bostondf)) {
  model = lm(cmedv ~ bostondf[, i], data = bostondf)
  
  modelsum = summary(model)
  modelsumdf = as.data.frame(modelsum$coefficients)
  pvals[i - 1, 2] = modelsumdf$`Pr(>|t|)`[2]
  pvals[i - 1, 1] = colnames(bostondf[i])
}

winner = grep(pvals[which(pvals[, 2] == min(pvals[,2])), 1], names(bostondf))
winners = data.frame(name = bostondf[winner])
otheroptions = bostondf[, -winner]

names(winners)

thesecondbestfunction = function(depvar, winners, otheroptions) {
  
  pvals = data.frame(predictor = NA, pval = NA)
### The real function
for (i in 1:length(otheroptions)) {
  if (is.null(winners) == TRUE) {
    model = lm(depvar ~ otheroptions[, i], data = otheroptions)
    
    modelsum = summary(model)
    modelsumdf = as.data.frame(modelsum$coefficients)
    pvals[i - 1, 2] = modelsumdf$`Pr(>|t|)`[2]
    pvals[i - 1, 1] = colnames(otheroptions[i])
    
  } else {
    model = lm(depvar ~ . + otheroptions[, i], data = winners)
    modelsum = summary(model)
    modelsumdf = as.data.frame(modelsum$coefficients)
    pvals[i - 1, 2] = modelsumdf$`Pr(>|t|)`[2]
    pvals[i - 1, 1] = colnames(otheroptions[i])
  }
  
}

winner = grep(pvals[which(pvals[, 2] == min(pvals[,2])), 1], pvals$predictor)
if (is.null(winners) == TRUE) {
  winners = data.frame(otheroptions[winner])
  otheroptions = otheroptions[, -winner]
} else {
  winners = data.frame(winners, otheroptions[winner])
  otheroptions = otheroptions[, -winner]
  
}
returnlist = list(winners = winners, 
                  otheroptions = otheroptions, 
                  pval = min(pvals$pval))
return(returnlist)
}

winners = NULL
otheroptions = bostondf
for (i in 1:14) {
  winners_and_otheroptions = thesecondbestfunction(bostondf$cmedv, winners, otheroptions)
  if (winners_and_otheroptions[["pval"]] > .05) break
    
  winners = winners_and_otheroptions[["winners"]]
  otheroptions = winners_and_otheroptions[["otheroptions"]]
}

winners_and_otheroptions[["otheroptions"]]
winners_and_otheroptions[["winners"]]







listj = list(NA)
listj[1] = "var"
for (j in 1:14) {
  list[j] = thesecondbestfunction(bostondf$cmedv, winners, otheroptions)
}

testf = function(x) {
  f = x * 2
  return(f)
}
testf(bostondf$cmedv)

for (j in 1:15) {
  testf(bostondf$cmedv)
}
