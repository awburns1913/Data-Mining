install.packages('arules')
library(arules)
library(tidyr)
library(stringr)
library(dplyr)

test = read.csv('orderData.csv', stringsAsFactors = FALSE)

test2 = test %>% 
  group_by(orderNo, seatNo) %>%
  mutate(item = paste0(item, collapse = ", ")) %>%
  slice(which(row_number() %% 3 == 1)) %>%
  ungroup() %>%
  select(item) %>%
  separate(item, c('meat', 'wine', 'side'), ", ")

test3 = test2 %>% select(meat, wine)

write_csv(test2, "orderDataclean.csv")
write_csv(test3, "orderDataclean3.csv")

orders = read.transactions('orderDataclean.csv', sep = ',')
orders2 = read.transactions('orderDataclean3.csv', sep = ',')

image(orders[1:100])

summary(orders)
inspect(orders[1:5])
itemFrequency(orders2[, 1:15])

#Rules for full meal
rules = apriori(orders, parameter = list(support = .0025, confidence = .15, minlen = 3))
inspect(rules)
inspect(sort(rules, by = 'lift')[1:20])

#Rules for meat and wine
rules2 = apriori(orders2, parameter = list(support = .0025, confidence = .15, minlen = 2))
inspect(rules2[1:20])
inspect(sort(rules2, by = 'lift')[1:20])




