

#####
#
# format data to train recurrent deep nets
# p1 p2 ... p5 <EOO> p4 p8 ... p1 <EOO> p2 p5 p8 <EOO>
#
#####

setwd("C:/Users/martin.m/Documents/dataCompetitions/instacart/")

## load data

source("./scripts/load.R")

## also loads data.table, tidyr, dplyr packages


# Reshape -----------------------------------------------------------------

table(orders$eval_set)

## join user_id to each order

orderp <- orderp %>% inner_join(orders %>% select(order_id, user_id, order_number), by = "order_id")

ordert <- ordert %>% inner_join(orders %>% select(order_id, user_id, order_number), by = "order_id")

## combine orderp and ordert
## order by user_id, order_number, ad_to_cart_order
## transform into string of concatenated product_ids

order_ts <- orderp %>%
  #top_n(100, wt = user_id) %>% ## subset
  bind_rows(ordert) %>% 
  arrange(user_id, order_number, add_to_cart_order) %>%
  group_by(user_id, order_number) %>%
  summarise(product_ts_temp = paste(product_id, collapse = " ")) %>%
  arrange(user_id, order_number) %>%
  group_by(user_id) %>%
  summarise(product_ts = paste(product_ts_temp, collapse = " 99999 ")) ## end of order marked with 99999 (0 is already the default for missing in keras)
  

## export

write.table(x = order_ts$product_ts, file = "./data/ts.txt", sep = ";", quote = FALSE, row.names = FALSE, col.names = FALSE)


## select specific products
top5_nbrs <- c(30259, 598, 43677, 44476, 1369)
# c(13379, 15144, 47704, 2188, 36071, 15463)
# c(27845, 38689, 6104, 39180, 22035, 42244) ## milk and yoghurt
# c(196, 48142, 14018, 41406, 15784, 44544) ## soda's


products %>% filter(product_id %in% top5_nbrs)
