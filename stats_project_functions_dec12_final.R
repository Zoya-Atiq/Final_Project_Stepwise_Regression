thesecondbestfunction = function(depvar, winners, otheroptions) {
  pvals = data.frame(predictor = NA, pval = NA)
  ### The real function
  for (i in 1:length(otheroptions)) {
    if (is.null(winners) == TRUE) {
      model = lm(depvar ~ otheroptions[, i], data = otheroptions)
      
      modelsum = summary(model)
      modelsumdf = as.data.frame(modelsum$coefficients)
      pvals[i, 2] = modelsumdf$`Pr(>|t|)`[2]
      pvals[i, 1] = colnames(otheroptions[i])
      
      
    } else {
      model = lm(depvar ~ . + otheroptions[, i], data = winners)
      modelsum = summary(model)
      modelsumdf = as.data.frame(modelsum$coefficients)
      j = nrow(modelsumdf)
      pvals[i, 2] = modelsumdf$`Pr(>|t|)`[j]
      pvals[i, 1] = colnames(otheroptions[i])
      
    }
    
    
  }
  
  
  
  winner = grep(pvals[which(pvals[, 2] == min(pvals[, 2])), 1], names(otheroptions))
  if (is.null(winners) == TRUE) {
    winners = data.frame(otheroptions[winner])
    otheroptions = otheroptions[, -winner]
  } else {
    winners = data.frame(winners, otheroptions[winner])
    otheroptions = as.data.frame(otheroptions[, -winner])
    
  }
  returnlist = list(winners = winners,
                    winner_names = names(winners),
                    otheroptions = otheroptions, 
                    pval = min(pvals$pval))
  print(returnlist$winner_names)
  return(returnlist)
}

steponyournewshoes = function(depvar, data) {
  print('Dear Julia, thank you for the class. We all had a great time. Have a happy holiday season.')
  winners = NULL
  otheroptions = data 
  for (i in 1:length(otheroptions)) {
    
    winners_and_otheroptions = thesecondbestfunction(depvar, winners, otheroptions)
    if (winners_and_otheroptions[["pval"]] > .05) break
    
    winners = winners_and_otheroptions[["winners"]]
    otheroptions = winners_and_otheroptions[["otheroptions"]]
  }
  return(winners_and_otheroptions)
}

library(mlbench)
data("BostonHousing2")
depvar = BostonHousing2$cmedv
df = BostonHousing2 %>% subset(select = -c(cmedv, medv, town, tract))
love = steponyournewshoes(depvar, df)
love$winner_names

df$fake1 = rnorm(506)
df$fake2 = rcauchy(506)
df$fake3 = runif(506)

love = steponyournewshoes(depvar, df)
love$winner_names
