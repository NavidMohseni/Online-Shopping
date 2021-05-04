#Shopping Rate
shop_rate <- shop %>% 
  group_by(customer_id) %>% 
  mutate(rate = mean(rate)) %>% 
  mutate(rate_scaled = case_when(
    rate < 150 ~ 1,
    rate >= 150 & rate < 300 ~ 2,
    rate >= 300 & rate < 450 ~ 3,
    rate >= 450 & rate < 600 ~ 4,
    rate >= 600 & rate < 750 ~ 5,
    rate >= 750 & rate < 900 ~ 6,
    rate >= 900 & rate < 1050 ~ 7,
    rate >= 1050 & rate < 1200 ~ 8,
    rate >= 1200 & rate < 1350 ~ 9,
    rate >= 1350 & rate <= 1500 ~ 10)) %>% 
  ungroup(customer_id) 


shop_rate %>% 
  ggplot(aes(as_factor(rate_scaled), fill = gender)) +
  geom_bar() +
  facet_wrap(~gender) + 
  labs(x = "rate", y = NULL, fill = "Male Vs Female",
       title = "Rate by gender")


shop_rate %>% 
  ggplot(aes(rate_scaled, color = gender, fill = gender)) + 
  geom_density(adjust = 2, alpha = 0.1)

shop_rate %>% 
  ggplot(aes(total_amt, as_factor(rate_scaled))) + 
  geom_boxplot(fill = "white", outlier.color = "gray60", outlier.alpha = 0.6)

shop_rate %>% 
  group_by(city_code, store_type) %>% 
  summarise(mean = mean(rate_scaled),
            min = min(rate_scaled),
            max = max(rate_scaled)) %>% 
  arrange(desc(mean))

shop_rate %>% 
  group_by(prod_cat,prod_subcat) %>% 
  summarise(mean = mean(rate_scaled)) %>% 
  arrange(desc(mean))


