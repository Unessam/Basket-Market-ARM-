pacman::p_load(pacman, tidyr, dplyr, shiny, ggplot2, ggthemes, GGally, ggvis, httr,
               lubridate, rmarkdown, stringr)
p_load(rio)
p_load(psych)
#importing data
marketbasket<- import("C://Users//ARAD//Desktop//Data science//Salford//handouts//data mining//w3//marketbasket.csv")
#rm=(list=ls())
# analysing dataset
View(marketbasket)
names(marketbasket)
length(names(marketbasket))
head(marketbasket)
summary(marketbasket)      
str(marketbasket)
#describe(marketbasket)
dim(marketbasket)
yes<-colSums(marketbasket=='Yes')
yes
no<- colSums(marketbasket=='No')
no
purchased<-rbind(yes, no)
purchased
# plotting Item sold frequency
barplot(purchased, legend=rownames(purchased))
barplot(purchased, beside=T, legend=rownames(purchased))
# we can see that cosmetics, avocado and sardines are respectively the most
# frequent items in the basket
p_load(plyr)
# mapping character values to binary integer values(1, 0)
for (i in colnames(marketbasket)){ marketbasket[[i]]<- mapvalues(marketbasket[[i]],
                                                              from=c('Yes', 'No')
                                                              , to=c(1, 0))}
#converting to discrete column types such as factor or logical
for (i in colnames(marketbasket)){ marketbasket[[i]]<- as.factor(marketbasket[[i]])}
head(marketbasket)
# installing Arules package for ARM
install.packages('arules')
library(arules)
?apriori
rules<- apriori(marketbasket)
rules
summary(rules)
inspect(rules)
rules <- apriori(marketbasket, parameter = list(minlen=2, maxlen=3, conf=0.95))
rules 
summary(rules)
inspect(rules)
# since we are not interested in not-purchased items, we should filter them, 
# also the most popular products(most frequent) are more interesting to us.
rules<- apriori(marketbasket, parameter = list(minlen=2, maxlen=3, conf=0.95),
                appearance = list(rhs=c('cosmetics=1'), default='lhs'))
rules
# there is no rule available for this parameter, we can lower the confidence
rules<- apriori(marketbasket, parameter = list(minlen=2, maxlen=3, conf=0.7),
                appearance = list(rhs=c('cosmetics=1'), default='lhs'))
rules
summary(rules)
inspect(rules)
# visualizing rules
install.packages('arulesViz')
library(arulesViz)

plot(rules)
plot(rules, method = "grouped")
plot(rules@quality, col='red', pch=19)
?plotly_arules
rules3 <- apriori(marketbasket,
                  parameter = list(minlen=2,maxlen=4, conf = 0.60),
                  appearance =list(rhs=c("banana=1","apples=1","avocado=1")
                                   ,default="lhs") )
#interactive plot
plot(rules3, engine = "plotly")

#changing measures 
plot(rules3, engine= 'plotly', measure = c("support", "lift"), shading = "confidence")

#filtering only purchased items:
rules2 <- apriori(marketbasket,
                  parameter = list(minlen=2, maxlen=3,conf = 0.7),
                  appearance =list(rhs=c("cosmetics=1"),
                                   lhs=c("apples=1",
                                         "banana=1",
                                         "coke=1",
                                         "turkey=1",
                                         "bourbon=1",
                                         "ice_cream=1",
                                         "baguette=1",
                                         "soda=1",
                                         "choclate=1",
                                         "cracker=1",
                                         "avocado=1",
                                         "sardines=1"),
                                   default="none"))
inspect(rules2)
plot(rules2, engine='plotly')
plot(rules2)
# lowering confident parameter to observe more rules
rules4 <- apriori(marketbasket,
                  parameter = list(minlen=2, maxlen=3,conf = 0.5),
                  appearance =list(rhs=c("cosmetics=1"),
                                   lhs=c("apples=1",
                                         "banana=1",
                                         "coke=1",
                                         "turkey=1",
                                         "bourbon=1",
                                         "ice_cream=1",
                                         "baguette=1",
                                         "soda=1",
                                         "choclate=1",
                                         "cracker=1",
                                         "avocado=1",
                                         "sardines=1"),
                                   default="none"))
inspect(rules4)
plot(rules4, engine='plotly')
#Explore association rules using interactive manipulations and
# visualization using shiny
?ruleExplorer
rules_ex <-apriori(marketbasket,
                   parameter =list(minlen=2,maxlen=4,conf=0.75))

install.packages('shinythemes')
library(shinythemes)
ruleExplorer(rules_ex)

