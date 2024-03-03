
#######################################################
###                 Association Rules         #########
#######################################################



# Define a project called "Association" and store it in a file called "Modelling Techniques"

# Download and store the dataset "market_basket" and the script into the file "Modelling Techniques"


# Import the dataset


suppressWarnings(retail <- read.transactions('/Users/adamdanielgreen/Desktop/Business Statistics/Association Rules-20231215/market_basket.csv', format = 'basket', sep=','))

# Packages

#install.packages('arules')
#install.packages('arulesViz')
    

library('arules', warn.conflicts=F, quietly=T)
library('arulesViz', warn.conflicts=F, quietly=T)
library('readxl', warn.conflicts=F, quietly=T)

#  A summary of the dataset

retail
summary(retail)

class(retail)



# Frequent itemset Generation
 
itemsets1 <- apriori(retail,parameter=list(minlen=1,maxlen=1,support=0.001,target="frequent itemsets"))

summary(itemsets1)


# Top 10 frequent 1-itemsets

inspect(head(sort(itemsets1, by = "support"), 10))


# Frequent 2-itemset Generation
  
itemsets2 <- apriori(retail,parameter=list(minlen=2,maxlen=2,support=0.001,target="frequent itemsets"))

summary(itemsets2)

  
# Top 10 frequent 2-itemsets

inspect(head(sort(itemsets2, by = "support"), 10))

  
# Frequent 3-itemset Generation
  
itemsets3 <- apriori(retail,parameter=list(minlen=3,maxlen=3,support=0.001,target="frequent itemsets"))

summary(itemsets3)


# Top 10 frequent 3-itemsets

inspect(head(sort(itemsets3, by = "support"), 10))

# Create some rules
 
rules <- apriori(retail, parameter = list(supp=0.001, conf=0.8))

rules <- sort(rules, by='confidence', decreasing = TRUE)
  
summary(rules)

plot(rules)

# Finding subsets of rules containing "COFFEE" items

COFFEE_rules <- subset(rules, items %in% "COFFEE")
inspect(head(sort(COFFEE_rules , by="lift") , 10))

# Finding subsets of rules that precede "SUGAR JARS" purchases
SUGAR_rules <- subset(rules, rhs %pin% "SUGAR JARS")
inspect(SUGAR_rules)

  
# The top ten rules sorted by the lift
  
inspect(head(sort(rules , by="lift") , 10))

# Graph visualization of the top ten rules sorted by lift
  
highLiftRules<-head(sort(rules, by="lift"),10)
plot(highLiftRules, method="graph" , control=list (type="items" ))

  