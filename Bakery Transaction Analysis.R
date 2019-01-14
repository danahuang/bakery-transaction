####Bakery Transaction#####
####ref: https://www.kaggle.com/danilodiogo/bakery-transactions-eda-and-association-rules/notebook
#### https://www.kaggle.com/nt19y08u/bakery-transaction-analysis
#### https://www.kaggle.com/behcetsenturk/analysis-of-bakery-transactions
rm(list=ls())

#import libraries
library(tidyverse)
library(data.table)
library(dplyr)
# library(lubridate)
library(cowplot)
library(arules)
library(arulesViz)
library(ggplot2)
library(plotrix)
library(reshape2)

data <-fread("C:/Users/nxf51120/Downloads/Dataset-Transactions-from-a-Bakery-Exploratory-Data-Analysis--master/Dataset-Transactions-from-a-Bakery-Exploratory-Data-Analysis--master/Transactions-from-a-Bakery/BreadBasket_DMS.csv",sep=",",stringsAsFactors=F)
str(data)
summary(data)
head(data)
tail(data)

#Correct date and time column types
data <- data %>%
  mutate(
    Date = as.Date(Date),
    Time = as.ITime(Time)
  )

str(data)

#find how many unique items that we have
data_item <- data%>%
  group_by(Item) %>%
  summarize(
    Count = n()
  ) 

lengths(data_item)[2]

# Count 
# 95 

#clean the data
word_list = c("NaN", "-", "nan", "NAN", "None", "NONE", "none", " ", "_", ".")
found<-c()
 for ( i in c(1:length(word_list))) {
    if (sum(which(data$Item==word_list[i]))>0) {
    found=c(found,i)
    }
 }

# Found word types is 1 so only one of thing in my list founded in our data ("NONE")    
found_words=word_list[found]

# how many of them are "NONE" 
nrow(data[which(data$Item == "NONE"),])
# [1] 786

# Data include 786 missing values let's drop them
found<-c()
for ( i in c(1:length(word_list))) {
  if (sum(which(data$Item==word_list[i]))>0) {
    data_clean=data[-which(data$Item==word_list[i]),]
  }
}
            
# Let's look again unique Item list it must be 94.
data_item_1 <- data_clean%>%
  group_by(Item) %>%
  summarize(
    Count = n()
  ) 

lengths(data_item_1)[2]

# Count 
# 94

# fwrite(data_clean,"C:/Users/nxf51120/Downloads/Dataset-Transactions-from-a-Bakery-Exploratory-Data-Analysis--master/Dataset-Transactions-from-a-Bakery-Exploratory-Data-Analysis--master/Transactions-from-a-Bakery/BreadBasket_DMS_Clean.csv")

## ggplot theme
theme <- theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

# Decrease graph size from standard
options(repr.plot.width = 9, repr.plot.height = 3)

# Graph of transactions per hour
a <- data_clean %>%
  mutate(
    Hour = as.factor(hour(Time))
  ) %>%
  group_by(Hour, Transaction) %>%
  summarize(
    Transactions = n_distinct(Item)
  ) %>%
  ggplot(
    aes(x = Hour, fill = Hour)
  ) +
  geom_histogram(stat="count") +
  theme(
    legend.position="none" 
  ) +
  labs(x = "Hour", y = "Transactions", title = "Transactions per hour")

plot(a)

# 3-Find most popular Items
# Get first 10 items from list
hot_items = data_item_1  %>%  mutate(rank = rank(desc(Count))) %>%  arrange(rank) %>% filter(rank<=10)
hot_items_data<-data.frame()
for ( i in c(1:lengths(hot_items)[2])) {
  if (sum(which(data_clean$Item==hot_items$Item[i]))>0) {
    hot_items_data_i=data_clean[which(data_clean$Item==hot_items$Item[i]),]
    hot_items_data=rbind(hot_items_data,hot_items_data_i)
  }
}

# hot_items_data=data_clean[which(data_clean$Item==hot_items$Item),]
# hot_items_data<-sapply(data_clean, function(x) data_clean$Item[match(x, data_item_1$Name)])

# Find and sum the remaining items and label it as "Others".
other_items = data_item_1  %>%  mutate(rank = rank(desc(Count))) %>%  arrange(rank) %>% filter(rank>10)
other_items_data<-data.frame()
for ( i in c(1:lengths(other_items)[2])) {
  if (sum(which(data_clean$Item==other_items$Item[i]))>0) {
    other_items_data_i=data_clean[which(data_clean$Item==other_items$Item[i]),]
    other_items_data=rbind(other_items_data,other_items_data_i)
  }
}
other_items_data$Item="Others"
# Add two of them in one series.
data_clean_1=rbind(hot_items_data,other_items_data)

# Here the item list. Yes I like coffee too.
data_clean_1

#Items distribution
options(repr.plot.width = 9, repr.plot.height = 5)

# Most popular items sold
data_clean_1 %>%
  group_by(Item) %>%
  summarize(
    Count = n()
  ) %>%
  arrange(desc(Count))  %>%
  head(n = 20) %>%
  ggplot(
    aes(
      x = reorder(Item, Count), y = Count, fill = Item
    )
  ) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(
    legend.position = "none" 
  ) +
  labs(x = "Item", y = "Transactions", title = "Most popular items")

#Find the best item combinations with coffee

  # We get all transaction numbers which contain coffee.
coffee_data=data  %>%  filter(Item=="Coffee")
coffee_transaction_list=coffee_data$Transaction

Coffee_Comb_data= data_clean_1
Coffee_Comb_data= data_clean_1 %>%  filter(Transaction %in% c(coffee_transaction_list))

 # We select the top 10 items and add label to figure out it's with coffee or not
hot_items_data_coffee= hot_items_data %>% mutate(Coffee=ifelse(Transaction %in% c(coffee_transaction_list),"with coffee","wo coffee"))

# We need to drop coffee values beacuse we don't need it when we compare items with coffee or without coffee
hot_items_data_coffee= hot_items_data_coffee %>% filter (Item!="Coffee")

hot_items_data_coffee_group = hot_items_data_coffee %>% group_by(Item,Coffee) %>%   summarize(
  Count = n()
)
hot_items_data_coffee_group_1=dcast(hot_items_data_coffee_group,Item~Coffee,value.var=c("Count"))

hot_items_data_coffee_group_1$Compare=hot_items_data_coffee_group_1$`with coffee`/hot_items_data_coffee_group_1$`wo coffee`
hot_items_data_coffee_group_1$Compare=round(hot_items_data_coffee_group_1$Compare, 2)
hot_items_data_coffee_group_1 = hot_items_data_coffee_group_1 %>% arrange(desc(hot_items_data_coffee_group_1$Compare))
hot_items_data_coffee_group_1

# Item with coffee wo coffee Compare
# 1     Medialuna         345       271    1.27
# 2        Pastry         474       382    1.24
# 3      Sandwich         421       350    1.20
# 4          Cake         540       485    1.11
# 5       Cookies         283       257    1.10
# 6 Hot chocolate         293       297    0.99
# 7       Brownie         186       193    0.96
# 8           Tea         482       953    0.51
# 9         Bread         923      2402    0.38

counts <- table(hot_items_data_coffee$Coffee, hot_items_data_coffee$Item)
b=barplot(counts, main="Sales Information of Top Ten Items With/Without Coffee",
        xlab="Coffee or not", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)






# Supplemment: Item association rules (Market Basket Analysis)

# fwrite(data_clean,"C:/Users/nxf51120/Downloads/Dataset-Transactions-from-a-Bakery-Exploratory-Data-Analysis--master/Dataset-Transactions-from-a-Bakery-Exploratory-Data-Analysis--master/Transactions-from-a-Bakery/BreadBasket_DMS_Clean.csv")

transaction.data <- read.transactions(
  "C:/Users/nxf51120/Downloads/Dataset-Transactions-from-a-Bakery-Exploratory-Data-Analysis--master/Dataset-Transactions-from-a-Bakery-Exploratory-Data-Analysis--master/Transactions-from-a-Bakery/BreadBasket_DMS_Clean.csv", # file path
  format = "single", # single is used because each column has 1 item
  cols = c(3,4), # specifies the item and transaction columns
  sep = "," # csv file has , as separator
)

itemFrequencyPlot(
  transaction.data,
  topN=4, # Show top 4 items
  type="absolute", # absolute frequency instead of relative
  main="Most popular items sold (absolute frequency)"
)

association.rules <- apriori(
  transaction.data,
  parameter = list(
    supp = 0.005, # Minimum support level, frequency of items
    conf = 0.6 # Minimum confidence level
  )
)

association.rules <- sort(association.rules, by = 'support', decreasing = TRUE)
summary(association.rules)

inspect(head(association.rules))

plot(association.rules)

plot(association.rules, method="graph")

