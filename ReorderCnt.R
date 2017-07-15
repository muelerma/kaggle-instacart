
## Size of next order prediction


## in order to get away from static threshold, predict how many reordered items will be in next order

## start with data/features about user

View(head(users))

## target variable: from data get orders with eval_set == train and sum(reordered)

## target: train orders only

head(ordert)

train_reorders <- ordert %>%
  group_by(user_id) %>% ## order_id/user_id 1:1 -> use user_id for join
  summarise(reorder_cnt = sum(reordered))

## join with user data

train_reorders <- train_reorders %>%
  inner_join(users %>% select(-order_id, -eval_set), by = "user_id")


# Train -------------------------------------------------------------------


## -> first simple train set to predict reorder count

library(xgboost)
library(caret)
set.seed(1234)

train_idx <- createDataPartition(train_reorders$reorder_cnt, p=0.8, list = FALSE)

train_reorders_train <- train_reorders[ train_idx[,1] ,]
train_reorders_test <- train_reorders[ -train_idx[,1] ,]

params <- list(
  "objective"           = "reg:linear",
  "eval_metric"         = "rmse",
  "eta"                 = 0.1,
  "max_depth"           = 6,
  "min_child_weight"    = 10,
  "gamma"               = 0.70,
  "subsample"           = 0.77,
  "colsample_bytree"    = 0.95,
  "alpha"               = 2e-05,
  "lambda"              = 10
)

X2 <- xgb.DMatrix(as.matrix(train_reorders_train %>% select(-reorder_cnt, -user_id)), label = train_reorders_train$reorder_cnt)

model <- xgboost(data = X2, params = params, nrounds = 90)

importance <- xgb.importance(colnames(X2), model = model)
#xgb.ggplot.importance(importance)

## predict

Y <- xgb.DMatrix(as.matrix(train_reorders_test %>% select(-reorder_cnt, -user_id)))

# train_reorders_test$reorder_cnt <- NULL

train_reorders_test$reorder_cnt_predict <- predict(model, Y)

train_reorders_test$reorder_cnt_predict_rnd <- round(train_reorders_test$reorder_cnt_predict)


## predict test

test <- test %>% distinct(user_id) %>%
  inner_join(users %>% select(-order_id, -eval_set), by = "user_id")

Y_test <- xgb.DMatrix(as.matrix(test %>% select(-user_id)))

test$reorder_cnt_predict <- predict(model, Y_test)
test$reorder_cnt_predict_rnd <- round(test$reorder_cnt_predict)

test_predicted_cnt <- test %>% select(user_id, reorder_cnt_predict_rnd)

## now use user_id - reorder_cnt prediction as threshold xgboost starter
