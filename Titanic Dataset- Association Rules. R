library(dplyr)
library(Matrix)
library(arulesViz)
library(arules)
###
titanic<-read.csv("/Users/ishantdhall/Desktop/DEV/Assignment-2/Titanic.csv",na.strings=c("","NA"), stringsAsFactors = TRUE)
View(titanic)

str(titanic)
sapply(titanic, function(x) sum(length(which(is.na(x)))/length(x) * 100))
# There are no missing values in the data set. 
titanic<- titanic %>% select(-X) # Dropping the unnecessary column
# find association rules with the given settings

rules <- apriori(titanic,
                  parameter = list(minlen=2,supp=0.005,conf=0.8),
                  appearance = list(rhs=("Survived=Yes"),
                                     default="lhs"),
                  control = list(verbose=F))
inspect(rules)
# The association rules help us to identify interesting trends within the variables. 
# From the rule table, we can see that the "lhs" is the characteristics of people and rhs refer to their survivial.
# Higher confidence implies that there is a higher probability of the coexistence of the rules.( % of times the rule is True)
# Higher lift values implies that there is more likelhood for the rule to be True. In our case, the rule with a lift value of
# 3.09 implies that the people in Rule 1 are 3.09 times more likely to survive than others. 
# Sorting the rules
sortedRules<- sort(rules, by="lift")
inspect(sortedRules)
###
# finding the redundant rules
subset.matrix <- is.subset(sortedRules, sortedRules, sparse = FALSE)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)
# removing theredundant rules
rulesPruned <- sortedRules[!redundant]
inspect(rulesPruned)
# We see that the highest lift values have been achieved for {Class=2nd, Age = Child}, {Class=1st, Sex= Female} and 
# {Class = 2nd and Sex= Female} rules at 3.09, 3.01 and 2.71 in this particular order. 
# From the result, we see that the "Children travelling in 2nd Class" are 3.09 times more likely to survive than the other population. 
# Therefore, we can conclude that the childrens travelling in the second class have more likelyhood of survival than the children 
# who are travelling the the 1st and 3rd class as they have a higher lift value than the others. 
# Similarly, we can also conclude that the "Females" who are traveling via 1st class have a higher likelyhood of survival than the females
# who are travelling in the 2nd and the Crew class. It is also observed that the females in the crew class have the lowest lift value at 2.69.
# Hence, as these rules fall in our threshold values, we can conclude that the females in 1st, 2nd and Crew class have a higher 
# chance of survival than men.
plot(rulesPruned)
visual <- head(sort(rulesPruned, by="lift"), 3)
plot(visual, method = "graph" ,
     control = list( alpha = 1,main = "Top 3 Rules"))
# Plotting the top 3 rules. 
