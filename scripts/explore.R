


## Explore -----------------------------------------------------------------

library(ggplot2)

## prior orders per customer

orders %>% 
  group_by(user_id) %>%
  filter(eval_set == "prior") %>% 
  summarize(total_orders = n()) %>%
  ggplot(aes(x = total_orders)) +
  geom_bar()

## totel distinct products

duplicated(products$product_id) ## 50k

