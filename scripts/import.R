

## Import ------------------------------------------------------------------

library(dplyr)

products <- read.csv(file = "./data/products.csv")
orders_meta <- read.csv(file = "./data/orders.csv")
departments <- read.csv(file = "./data/departments.csv")
orders_prior <- read.csv(file = "./data/order_products__prior.csv")
orders_train <- read.csv(file = "./data/order_products__train.csv")



## Join --------------------------------------------------------------------


data_full_train <- orders_train %>%
  inner_join(orders_meta, by = "order_id") %>%
  inner_join(products, by = "product_id") %>%
  inner_join(departments, by = "department_id") 

data_full <- orders_prior %>%
  sample_frac(size = 0.1) %>%
  inner_join(orders_meta, by = "order_id") %>%
  inner_join(products, by = "product_id") %>%
  inner_join(departments, by = "department_id") 

object.size(data_full)
