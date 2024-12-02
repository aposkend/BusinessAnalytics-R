library(tidyverse)

#Question 1
stock = read_csv("stock.csv")
stock
stock %>% separate(id_name, into=c("id", "name"), sep=" ") -> stock1
stock1 %>% gather(key = "date", value = "value", -id, -name, -type) -> stock2
stock2 %>% separate(date, into=c("year", "month", "date"), sep="/") -> stock3
stock3 %>% spread(key = type, value = value) -> stock4
print(x =stock4, n=100)

#ANS: stock4

#Question 2
client.df = read_csv("client_list.csv")
prod.df = read_csv("product_list.csv")
sales.df = read_csv("salesdata.csv")

# 2-1
prod.df
prod.df %>% separate(Item, into=c("Product", "Item")) -> prod.df
#ANS: prod.df

# 2-2
#deals with some issue on column type and name
sales.df %>% mutate(Product = as.character(Product)) -> sales.df
client.df %>% select(-...1) -> client.df

sales.df %>% inner_join(prod.df) -> sales.df
sales.df %>% inner_join(client.df) -> full.table
full.table %>% rename(salesID = ...1) -> full.table
#ANS: full.table

# 2-3
full.table %>% mutate(spend = UnitPrice * Quantity) -> full.table
#ANS: full.table

#2-4
full.table %>% mutate(Group = ifelse(Membership %in% c("gold", "diamond"), "Group1", "Group2")) -> temp.table
temp.table %>% group_by(Group) ->temp.table
temp.table %>% summarize(
  Avg_Age = mean(Age),
  Avg_Spend = mean(UnitPrice * Quantity),
  Total_Spend = sum(UnitPrice * Quantity),
  Boy_Count = sum(Gender == "male"),
  Girl_Count = sum(Gender == "female"),
  Total_Count = n(),
) -> summary

summary

summary %>% summarize(
  Age_Difference = Avg_Age[Group == "Group1"] - Avg_Age[Group == "Group2"],
  Avg_Spend_Difference = Avg_Spend[Group == "Group1"] - Avg_Spend[Group == "Group2"],
  Total_Spend_Difference = Total_Spend[Group == "Group1"] - Total_Spend[Group == "Group2"],
  Boy_Count_Difference = Boy_Count[Group == "Group1"] - Boy_Count[Group == "Group2"],
  Girl_Count_Difference = Girl_Count[Group == "Group1"] - Girl_Count[Group == "Group2"],
  Total_Count_Difference = Total_Count[Group == "Group1"] - Total_Count[Group == "Group2"]
) -> difference

difference
#ANS: summary, difference

#2-5

full.table %>% filter(Gender == "female") ->temp.table
temp.table
temp.table %>% summarize(
  Avg_Age = mean(Age),
  Avg_Spend = mean(UnitPrice * Quantity),
  Total_Spend = sum(UnitPrice * Quantity),
  Total_Count = n(),
) -> summary

summary

ggplot(temp.table, aes(x = reorder(Item, spend), y = spend)) +
  geom_bar(stat = "identity") +
  labs(title = "Bar chart",
       x = "Product",
       y = "Total Spend") 

#ANS: summarty

