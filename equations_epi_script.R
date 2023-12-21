library(tidyverse)
options(scipen = 999)

## Define objects

# negate function
'%not_in%' <- Negate('%in%')

eulers_number <- exp(1)

# i = origin (exporting) country
# j = destination (importing) country
# k = commodity

i <- "GBR"
j <- "USA"
k <- "Whisky"

## Import data

country_pop_and_gdp <- read_rds("country_pop_and_gdp.rds")

# modify to make the growth rates +1
country_pop_and_gdp <- country_pop_and_gdp %>%
  mutate(pop_growth = pop_growth + 1,
         gdp_growth = gdp_growth + 1)

bilateral_data <- read_rds("bilateral_data.rds")
# remember that currently value is in USD, not ,000s.

## Population and GDP growth rates
# (looking at the methodology paper, it seems like this should be a ratio around 1 rather than a growth rate around 0):
# "(∆𝐺𝐷𝑃𝑖 is the ratio of expected over current GDP for country 𝑖). Expected GDP growth is computed for a time period of five years."

# ∆GDP_𝑖=> projected change in GDP in origin country i
d_gdp_i <- country_pop_and_gdp %>%
           filter(country_iso == i)%>%
           pull(var = gdp_growth)

# ∆GDP_𝑗=> projected change in GDP destination market j
d_gdp_j <- country_pop_and_gdp %>%
  filter(country_iso == j)%>%
  pull(var = gdp_growth)

# ∆POP_𝑗=> projected change in population in destination market j 
d_pop_j <- country_pop_and_gdp %>%
  filter(country_iso == j)%>%
  pull(var = pop_growth)

## Elasticity
# 𝐸𝑚,𝐺𝐷𝑃_(j,𝑘)=> Elasticity of imports per capita with respect to GDP per capita of commodity k in destination country j. Currently assumed to be 1.
elast_m_gdp_jk <- bilateral_data %>%
  filter (iso3_d == j, FFD == k)%>%
  summarise(first(elast_m_gdp_jk))%>%
  pull()


## Distances

# log distance_(𝑖,𝑗)=> distance between origin i and destination j
log_distance_ij <- bilateral_data %>%
  filter (iso3_d == j,
          iso3_o == i)%>%
  summarise(first(log_distance))%>%
  pull()

# average log distance_(j,k)=> average distance of imports of commodity k by destination market j, logged
average_log_distance_jk <- bilateral_data %>%
                           filter(iso3_d == j, FFD == k)%>%
                           summarise(average_log_distance_jk = weighted.mean(log_distance, w = value_usd))%>%
                           pull()

## Trade flows (current)

# 𝑣_(𝑖,𝑘) => total origin (i) exports of commodity k
v_ik <- bilateral_data %>%
  filter (iso3_o == i,
          FFD == k)%>%
  summarise(sum(value_usd))%>%
  pull()

#𝑣_(𝑖,𝑗)=> total origin (i) exports to destination market j 
v_ij <- bilateral_data %>%
  filter (iso3_o == i,
          iso3_d == j)%>%
  summarise(sum(value_usd))%>%
  pull()

#𝑣_(𝑗,𝑘) => total imports in destination market j of commodity k
v_jk <- bilateral_data %>%
  filter (iso3_d == j,
          FFD == k)%>%
  summarise(sum(value_usd))%>%
  pull()

#𝑣_𝑘 => total global trade in commodity k
v_k <- bilateral_data %>%
  filter (FFD == k)%>%
  summarise(sum(value_usd))%>%
  pull()

#𝑚_(𝑖,𝑘)=> total origin (i) imports of commodity k (confusingly here origin i is the destination!)
m_ik <- bilateral_data %>%
  filter (iso3_d == i,
          FFD == k)%>%
  summarise(sum(value_usd))%>%
  pull()

## Tariffs

# average_tariff_(𝑖,𝑘)=> average tariff faced by exports of commodity k from origin i
average_tariff_ik <- bilateral_data %>%
  filter(iso3_o == i, FFD == k)%>%
  summarise(average_tariff_ik = weighted.mean(MinAve, w = value_usd))%>%
  pull()

# average_tariff_𝑘=> average tariff faced by all trade flows of commodity k 
average_tariff_k <- bilateral_data %>%
  filter(FFD == k)%>%
  summarise(average_tariff_k = weighted.mean(MinAve, w = value_usd))%>%
  pull()

# average_tariff_(j,𝑘) => average tariff faced by all imports of commodity k by market j 
average_tariff_jk <- bilateral_data %>%
  filter(iso3_d == j, FFD == k)%>%
  summarise(average_tariff_jk = weighted.mean(MinAve, w = value_usd))%>%
  pull()

# tariff_(𝑖,𝑗,𝑘) => tariff faced by exports of commodity k from the UK (i) to destination market j
tariff_ijk <- bilateral_data %>%
  filter(iso3_o == i, iso3_d == j, FFD == k)%>%
  pull(MinAve)

# summed terms. For these I will need to use vectors rather than values.
# sum over i (v_ik * d_gdp_i)
#for every origin country i, what is the total volume of k exported, multiplied by the change in i's GDP?
# sum the answer.
sum_over_i_v_ik_times_d_gdp_i <- bilateral_data %>% 
  group_by(iso3_o)%>%
  summarise(v_ik = sum(value_usd, na.rm = TRUE))%>%
  ungroup()%>%
  left_join(., country_pop_and_gdp, by = c("iso3_o" = "country_iso"))%>%
  mutate(v_ik_times_d_gdp_i  = v_ik * gdp_growth)%>%
  summarise(sum(v_ik_times_d_gdp_i, na.rm = TRUE))%>%
  pull()


# sum over k ((v_ik/v_k) * v_jk)
# for every commodity k, what is the volume of k exported by country i; 
# total global trade in k; 
# and total k imported to destination market j?
# make these columns, then perform the division and multiplication.
# sum the result over k.


# Note: the entire denominator, before it is summed over K, behaves in a way that if all trade in K was between countries i and j, its value would be V_k squared.
# this is the maximum value that it could take. The minimum value it could take is 0. Bear in mind this is before we sum over all Ks, and before we use it as the 
# denominator in the overall 'ease of trade second term'.
v_k_df <- bilateral_data %>%
  group_by(FFD)%>%
  summarise(v_k = sum(value_usd, na.rm = TRUE))%>%
  ungroup()
  
v_ik_df <- bilateral_data %>%
  filter(iso3_o == i)%>%
  group_by(FFD)%>%
  summarise(v_ik = sum(value_usd, na.rm = TRUE))%>%
  ungroup()
  
v_jk_df <- bilateral_data %>%
  filter(iso3_d == j)%>%
  group_by(FFD)%>%
  summarise(v_jk = sum(value_usd, na.rm = TRUE))%>%
  ungroup()

sum_over_k_formula_df <- left_join(v_k_df, v_ik_df, by = "FFD")%>%
                         left_join(., v_jk_df, by = "FFD")

sum_over_k_formula_df <- sum_over_k_formula_df %>% mutate(v_ik_over_v_k_times_v_jk = (v_ik / v_k)* v_jk)

sum_over_k_formula <- sum_over_k_formula_df %>%
  summarise(sum_over_k = sum(v_ik_over_v_k_times_v_jk, na.rm = TRUE))%>%
  pull(sum_over_k)



## Supply
# first term
supply_first_term <- (v_ik * d_gdp_i) / sum_over_i_v_ik_times_d_gdp_i

supply_second_term <- if_else((v_ik/m_ik) < 1, (v_ik/m_ik), 1)

supply_third_term <- (1 + average_tariff_ik) / (1 + average_tariff_k)

supply <- supply_first_term * supply_second_term * supply_third_term


## Demand
demand_first_term <- v_jk * ((d_gdp_j / d_pop_j)^elast_m_gdp_jk) * d_pop_j

demand_second_term <- (1+ average_tariff_jk) / (1 + tariff_ijk)

demand <- demand_first_term * demand_second_term # note that this is a very large result because one of the multipliers is v_jk (absolute rather than a ratio or growth rate).


## Ease of trade

abs_val_diff_in_log_distances <- abs(average_log_distance_jk - log_distance_ij)

# this term takes a value of between 0 and 1. When country i is the exact same distance away from the destination  market j as the weighted average
# imaginary country where all its imports of commodity k come from (destinations weighted by import value of k), the value will be 1. It is an adjustment
# factor to account for how much the origin country differs from the average distance of import origins.
ease_of_trade_first_term <- eulers_number ^ -abs_val_diff_in_log_distances

# the total value of trade between origin i and destination k, divided by:
# 
ease_of_trade_second_term <- v_ij / sum_over_k_formula
# Now that we've summed over k and divided v_ij by this, we think that the result will be around 1 becuase the numerator and denominator are likely
# to be of the same magnitude. If v_ij is larger than the denominator, the value will be over 1 and this tells us that it's easier for i and j to trade
# than average / baseline.


ease_of_trade <- ease_of_trade_first_term * ease_of_trade_second_term


## Export Potential Indicator for origin i, destination j, and commodity k
epi_ijk <<- supply * demand * ease_of_trade


# EPI as a percentage of current v_ijk:

v_ijk <- bilateral_data %>%
  filter (iso3_o == i,
          iso3_d == j,
          FFD == k)%>%
  pull(value_usd)
  
export_potential_percentage_change <- (epi_ijk / v_ijk)*100


