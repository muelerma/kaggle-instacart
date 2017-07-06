

# Recommend ---------------------------------------------------------------

library(recommenderlab)
library(tidyr)
library(dplyr)

## 1. Join Customer and Item (instead of customer/order)

products_customer <- orders_prior %>%
  sample_frac(size = 0.001) %>%
  select(order_id, product_id) %>%
  inner_join(orders_meta %>% select(order_id, user_id), by = "order_id")

## 2. create sparse Customer-Item-Matrix

## v1
products_customer_sparse <- products_customer %>% 
  select(-order_id) %>%
  distinct() %>% 
  spread(product_id, user_id, fill = 0)

products_customer$bought <- 1

## v2
products_customer_sparse <- sparseMatrix(i = products_customer$user_id,
                                         j = products_customer$product_id)
## doesn't work with sparseM


## create sparse matrix from user -> product list
m <- as(matrix(data = 0,nrow = length(unique(products_customer$user_id)),
            ncol = length(unique(products_customer$product_id)),
            dimnames = list( users = unique(products_customer$user_id), 
                             products = unique(products_customer$product_id))),
        "binaryRatingMatrix")

m <- as(as(products_customer_sparse, "matrix"), "binaryRatingMatrix")


## recommend

test <- Recommender(data = m, method = "UBCF")




## Submit ------------------------------------------------------------------

## Submission file must be like CustomerID, ProductID_1, ProductID_2, ... , ProductID_n


