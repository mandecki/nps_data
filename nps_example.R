# EASY NPS analysis using R
# Written by Michael Mandecki


### LOAD DATA INTO R ############################
library(dplyr)

#nps_raw <- read_csv(file='https://raw.github.com/mandecki/nps_data/main/nps_fake.csv')
nps_raw <- read_csv(file = 'nps_fake.csv')


# date         name                    region          nps
# <date>       <chr>                   <chr>         <dbl>
# 1 2022-01-01 Zena Nolan              Northeast         5
# 2 2022-01-01 Kamari Cruickshank      North Central     8
# 3 2022-01-01 Miss Alivia Ruecker PhD North Central     9


### THE STANDARD WAY ############################

# Assign NPS to groups
nps_prep <- nps_raw %>%
  mutate(nps_group = case_when(nps <= 6 ~'detractor',
                               nps > 6 & nps <= 8 ~ 'passive',
                               nps > 8 ~ 'promotor'))

head(nps_prep)
# date                            name        region nps nps_group
# 1 2022-01-01              Zena Nolan     Northeast   5 detractor
# 2 2022-01-01      Kamari Cruickshank North Central   8   passive
# 3 2022-01-01 Miss Alivia Ruecker PhD North Central   9  promotor

# Calculate NPS
nps_summary <- nps_prep %>%
  group_by(region, nps_group) %>%
  summarize(cnt = n()) %>%
  mutate(prop = cnt / sum(cnt)) %>%
  select(-cnt) %>%
  pivot_wider(names_from = nps_group, values_from = prop) %>%
  mutate(nps_calc = round((promotor - detractor) * 100, 0))

head(nps_summary)
# region          detractor passive promotor nps_calc
# <chr>               <dbl>   <dbl>    <dbl>    <dbl>
# 1 North Central     0.225   0.360    0.416       19
# 2 Northeast         0.242   0.401    0.357       12
# 3 South             0.164   0.325    0.511       35
# 4 West              0.183   0.388    0.429       25
         

### THE BETTER WAY ##############################

# Assign NPS to -1, 0, 1
nps_prep <- nps_raw %>%
  mutate(nps_recode = case_when(nps %in% 0:6 ~ -1,
                                nps %in% 7:8 ~ 0,
                                nps %in% 9:10 ~ 1))

head(nps_prep)
#   date       name                    region          nps nps_recode
#   <date>     <chr>                   <chr>         <dbl>      <dbl>
# 1 2022-01-01 Zena Nolan              Northeast         5         -1
# 2 2022-01-01 Kamari Cruickshank      North Central     8          0
# 3 2022-01-01 Miss Alivia Ruecker PhD North Central     9          1

# Calculate NPS
nps_summary <- nps_prep %>%
  group_by(region) %>%
  summarize(nps_calc = round(mean(nps_recode) * 100, 0))

head(nps_summary)
#   region        nps_calc
#   <chr>            <dbl>
# 1 North Central       19
# 2 Northeast           12
# 3 South               35
# 4 West                25


### RECODE USING CUT ############################

# nps_prep <- nps_raw %>%
#   mutate(nps_cut1 = cut(nps, breaks=c(0, 6, 8, 10), labels=c('detractor', 'passive', 'promotor')),
#          nps_cut2 = as.numeric(as.character(cut(score, breaks = c(0, 6, 8, 10), labels=c(-1, 0, 1)))))

