
###########################################################################################################
#
# Kaggle Instacart competition
# Fabien Vavrand, June 2017
# Simple xgboost starter, score 0.3791 on LB
# Products selection is based on product by product binary classification, with a global threshold (0.21)
#
###########################################################################################################

library(data.table)
library(dplyr)
library(tidyr)


# Load Data ---------------------------------------------------------------
path <- "./data"

aisles <- fread(file.path(path, "aisles.csv"))
departments <- fread(file.path(path, "departments.csv"))
orderp <- fread(file.path(path, "order_products__prior.csv"))
ordert <- fread(file.path(path, "order_products__train.csv"))
orders <- fread(file.path(path, "orders.csv"))
products <- fread(file.path(path, "products.csv"))


# Reshape data ------------------------------------------------------------

aisles$aisle <- as.factor(aisles$aisle)
departments$department <- as.factor(departments$department)
orders$eval_set <- as.factor(orders$eval_set)
products$product_name <- as.factor(products$product_name)

products <- products %>% 
  inner_join(aisles) %>% 
  inner_join(departments) %>% 
  select(-aisle_id, -department_id)

rm(aisles, departments)

## attach user_id to train 
ordert$user_id <- orders$user_id[match(ordert$order_id, orders$order_id)]

orders_products <- orders %>% 
  inner_join(orderp, by = "order_id")

rm(orderp)
gc()


# Products ----------------------------------------------------------------

prd <- orders_products %>%
  arrange(user_id, order_number, product_id) %>%
  group_by(user_id, product_id) %>%
  mutate(product_time = row_number()) %>%
  ungroup() %>%
  group_by(product_id) %>%
  summarise(
    prod_orders = n(),
    prod_reorders = sum(reordered),
    prod_first_orders = sum(product_time == 1),
    prod_second_orders = sum(product_time == 2)
  )

prd$prod_reorder_probability <- prd$prod_second_orders / prd$prod_first_orders
prd$prod_reorder_times <- 1 + prd$prod_reorders / prd$prod_first_orders
prd$prod_reorder_ratio <- prd$prod_reorders / prd$prod_orders

prd <- prd %>% 
  select(-prod_reorders, -prod_first_orders, -prod_second_orders)

rm(products)
gc()


# Users -------------------------------------------------------------------

users <- orders %>%
  filter(eval_set == "prior") %>%
  group_by(user_id) %>%
  summarise(
    user_orders = max(order_number),
    user_period = sum(days_since_prior_order, na.rm = T),
    user_mean_days_since_prior = mean(days_since_prior_order, na.rm = T)
  )

us <- orders_products %>%
  group_by(user_id) %>%
  summarise(
    user_total_products = n(),
    user_reorder_ratio = sum(reordered == 1) / sum(order_number > 1),
    user_distinct_products = n_distinct(product_id)
  )

users <- users %>% 
  inner_join(us)

users$user_average_basket <- users$user_total_products / users$user_orders

us <- orders %>%
  filter(eval_set != "prior") %>%
  select(user_id, order_id, eval_set,
         time_since_last_order = days_since_prior_order)

users <- users %>% 
  inner_join(us)

rm(us)
gc()


# Database ----------------------------------------------------------------

data <- orders_products %>%
  group_by(user_id, product_id) %>% 
  summarise(
    up_orders = n(),
    up_first_order = min(order_number),
    up_last_order = max(order_number),
    up_average_cart_position = mean(add_to_cart_order))

#rm(orders_products, orders)

data <- data %>% 
  inner_join(prd, by = "product_id") %>%
  inner_join(users, by = "user_id")

data$up_order_rate <- data$up_orders / data$user_orders
data$up_orders_since_last_order <- data$user_orders - data$up_last_order
data$up_order_rate_since_first_order <- data$up_orders / (data$user_orders - data$up_first_order + 1)

data <- data %>% 
  left_join(ordert %>% select(user_id, product_id, reordered), 
            by = c("user_id", "product_id"))

# rm(ordert, prd, users)
gc()


# Train / Test datasets ---------------------------------------------------

train <- as.data.frame(data[data$eval_set == "train",])
train$eval_set <- NULL
train$user_id <- NULL
train$product_id <- NULL
train$order_id <- NULL
train$reordered[is.na(train$reordered)] <- 0

test <- as.data.frame(data[data$eval_set == "test",])
test$eval_set <- NULL
# test$user_id <- NULL
test$reordered <- NULL

rm(data)
gc()


# Model -------------------------------------------------------------------
library(xgboost)

params <- list(
  "objective"           = "reg:logistic",
  "eval_metric"         = "logloss",
  "eta"                 = 0.1,
  "max_depth"           = 6,
  "min_child_weight"    = 10,
  "gamma"               = 0.70,
  "subsample"           = 0.77,
  "colsample_bytree"    = 0.95,
  "alpha"               = 2e-05,
  "lambda"              = 10
)


subtrain <- train %>%
  sample_frac(0.1)

library(caret)
set.seed(1234)

eval_index <- createDataPartition(subtrain$reordered, p = 0.25, list = FALSE)

subtrain_train <- subtrain[-eval_index[,1] ,]
subtrain_eval <- subtrain[eval_index[,1] ,]

X <- xgb.DMatrix(as.matrix(subtrain_train %>% select(-reordered)), label = subtrain_train$reordered)

model <- xgboost(data = X, params = params, nrounds = 90)

importance <- xgb.importance(colnames(X), model = model)
#xgb.ggplot.importance(importance)

rm(X, importance, subtrain)
gc()


# Score -------------------------------------------------------------------

threshold_reorder <- 0.21

X_eval <- xgb.DMatrix(as.matrix(subtrain_eval %>% select(-reordered)))
subtrain_eval$predicted <- predict(model, X_eval)

subtrain_eval$predicted <- as.numeric(subtrain_eval$predicted > threshold_reorder)

confusionMatrix(subtrain_eval$predicted, subtrain_eval$reordered)

# rec <- recall(as.factor(subtrain_eval$predicted), as.factor(subtrain_eval$reordered), relevant = "1")
# prec <- precision(as.factor(subtrain_eval$predicted), as.factor(subtrain_eval$reordered), relevant = "1")
# 
# 2*( rec*prec / (prec+rec))

F_meas(as.factor(subtrain_eval$predicted), relevant = "1", reference = as.factor(subtrain_eval$reordered))

## base accuracy on p = 0.25 and set.seed(1234): 0.4366785



# Apply model on test -----------------------------------------------------
X_test <- xgb.DMatrix(as.matrix(test %>% select(-order_id, -product_id, -user_id)))
test$reordered <- predict(model, X_test)

# test$reordered <- as.numeric(test$reordered > threshold_reorder)


### VARIABLE THRESHOLD
## get predictions from ReorderCnt.R

test_predicted_cnt

## join via user_id 

## rank reorders by predicted prob

## filter based on reorder count

temp <- test %>% 
  arrange(user_id, -reordered) %>%
  group_by(user_id) %>%
  mutate(rank = row_number()) %>%
  #select(user_id, reordered, rank) %>%
  inner_join(test_predicted_cnt, by = "user_id") %>%
  filter(rank <= reorder_cnt_predict_rnd)

# save(file = "test_data.RData", test)

# Submission --------------------------------------------------------------

submission <- temp %>%
  #filter(reordered == <1) %>%
  group_by(order_id) %>%
  summarise(
    products = paste(product_id, collapse = " ")
  )

missing <- data.frame(
  order_id = unique(temp$order_id[!temp$order_id %in% submission$order_id]),
  products = "None"
)

submission <- submission %>% 
  #bind_rows(missing) %>% 
  arrange(order_id)

write.csv(submission, file = "submit_v2.csv", row.names = F, quote = FALSE)

