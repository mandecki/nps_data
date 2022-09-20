# https://monkeylearn.com/blog/sentiment-analysis-for-nps-surveys/
# https://www.kaggle.com/datasets/datafiniti/consumer-reviews-of-amazon-products?select=Datafiniti_Amazon_Consumer_Reviews_of_Amazon_Products.csv

library(tidyverse)

x <- tibble(score = c(10, 6, 7, 3, 4, 9, 1, 8, 7, 3, 2, 10, 5, 4))

x <- x %>%
  mutate(category_cut = cut(score, breaks = c(0, 6, 8, 10), labels=c('detractor', 'passive', 'promotor')),
         category_case = case_when(score <= 6 ~'detractor',
                                   score > 6 & score <= 8 ~ 'passive',
                                   score > 8 ~ 'promotor'),
         category_num = as.numeric(as.character(cut(score, breaks = c(0, 6, 8, 10), labels=c(-1, 0, 1))))) %>%
  mutate(category_recode = case_when(score %in% 0:6 ~ -1,
                                     score %in% 7:8 ~ 0,
                                     score %in% 9:10 ~ 1))

y %>%
  group_by(y) %>%
  summarize(cnt = n(),
            freq = cnt / sum(cnt))
