#Data Manipulation

library(tidyverse)
library(lubridate)

customer <- read_csv(choose.files())
transactions <- read_csv(choose.files())
Products <- read_csv(choose.files())

customer <- customer %>% 
  mutate(city_code = as_factor(city_code),
         city_code = fct_recode(city_code, 
                                Tehran = "1",
                                Khorasan = "2",
                                Isfahan = "3",
                                Fars = "4",
                                Azerbaijan = "5",
                                Mazandaran = "6",
                                Kerman = "7",
                                Alborz = "8",
                                Gilan = "9",
                                Kermanshah = "10"
         ),
         date_of_birth = dmy(date_of_birth)) %>% 
  drop_na()

Products <- Products %>% 
  rename(prod_subcat_code = prod_sub_cat_code)
transactions

transactions1 <- transactions %>% 
  left_join(Products, by = c("prod_cat_code", "prod_subcat_code"))

shop <- transactions1 %>% 
  left_join(customer, by = "customer_id")

View(shop)
glimpse(shop)
shop <- shop %>% 
  mutate(tran_date = dmy(tran_date),
         rate = abs(rate),
         total_amt = abs(total_amt),
         tax = abs(tax),
         store_type = as_factor(store_type),
         prod_cat = as_factor(prod_cat),
         prod_subcat = as_factor(prod_subcat)) %>% 
  select(transaction_id, customer_id, tran_date, 
         rate, tax, total_amt, store_type, prod_cat, prod_subcat,
         date_of_birth, gender, city_code)

shop <- shop %>% 
  drop_na()

#Distinct number of customers
shop %>% 
  distinct(customer_id)

