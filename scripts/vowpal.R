
## Data in Vowpal Wabbit format
## Label |Product1 Product2 ...ProductN
##
## Label is the reordered product
## products are the products ordered 

## Merge orders and user

user_history <- orders_prior %>%
  sample_frac(size = 0.01) %>%
  inner_join(orders_meta, by = "order_id") %>%
  filter(eval_set == "prior") %>%
  group_by(user_id) %>%
  summarise(products = paste(product_id, collapse = " "))
  #select(product_id, user_id)


## from train data, filter reordered products

products_train <- orders_train %>% 
  inner_join(orders_meta, by = "order_id") %>%
  filter(reordered == 1) %>%
  inner_join(user_history, by = "user_id") %>%
  select(product_id, products)

## determine the 1000 most common reordered items
## and filter only products from the top 1000

top100 <- orders_train %>%
  filter(reordered == 1) %>%
  group_by(product_id) %>%
  summarise(count = n()) %>%
  arrange(-count) %>%
  top_n(100)


## final filtering and vw formatting
set.seed(12345)
train_eval_index <- 1:nrow(products_train) %in% createDataPartition(products_train$id, p = 0.8, list = FALSE)

vw_output <- products_train %>%
  filter(train_eval_index) %>%
  filter(product_id %in% top100$product_id) %>%
  mutate(product_id = match(product_id, unique(product_id))) %>%
  mutate(id = 1:n(), output = paste(product_id,  paste0(id, "|"), products)) %>%
  select(output)

## write

write.table(vw_output, file="vw_test.txt", col.names = FALSE, row.names = FALSE, quote = FALSE,
            sep = ";")

## evaluation set in vw format (for each order_id get userId -> product history)
vw_train <- products_train %>% 
  filter(!train_eval_index) %>% 
  mutate(product_id = match(product_id, unique(product_id))) %>%
  mutate(id = 1:n(), output = paste(product_id,  paste0(id, "|"), products)) %>%
  select(output)


## read in probabilities produced by VW 
## and select top N based on threshold

