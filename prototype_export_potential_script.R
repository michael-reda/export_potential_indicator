library(tidyverse)

# create dummy data

set.seed(10)

country_data <- tibble::tibble(country = c("Kathrynland", "Peoples Democratic Republic of Mike", "Michaelia"))
                               
country_data <- country_data %>%
                  mutate(gdp_growth = sample(0:7, size = nrow(country_data), replace = TRUE)/100,
                         population_growth = sample(0:4, size = nrow(country_data), replace = TRUE)/100
                        )
                            

commodity_data <- tibble(country = rep(c("Kathrynland", "Peoples Democratic Republic of Mike", "Michaelia"), 2),
                         commodity = rep(c("wheat", "sugar"),3)
)

commodity_data <- commodity_data %>%
                  mutate(inbound_tariff = sample(0:20, size = nrow(commodity_data), replace = TRUE)/100,
                         elast_imp_per_cap_wrt_gdp_per_cap = sample(50:90, size = nrow(commodity_data), replace = TRUE)/100,
                         armington_elast = case_when(commodity == "wheat" ~ 2.5,
                                                     commodity == "sugar" ~ 4)
                        
                           )

trade_data <- expand_grid(country = country_data$country, trade_partner = country_data$country)%>%
  filter(trade_partner != country)

trade_data  <- trade_data %>% mutate(wheat = sample(0:1000, size = nrow(trade_data), replace = TRUE),
                                     sugar = sample(0:600, size = nrow(trade_data), replace = TRUE),
                                     distance = c(1000, 650, 1000, 2300, 650, 2300)
                                    )%>%
  pivot_longer(cols = c(wheat, sugar), names_to = "commodity", values_to = "tonnes")

trade_data <- left_join(trade_data, country_data, by = "country")%>%
              left_join(., commodity_data, by = c("country", "commodity"))%>%
              select(1,6,7,4,8,9,10,2,3,5)

rm(commodity_data, country_data)

country_sums <- trade_data %>%
  select(country, gdp_growth, commodity, tonnes)%>%
  group_by(country, gdp_growth, commodity)%>%
  summarise(tonnes = sum(tonnes))%>%
  ungroup()%>%
  mutate(tonnes_d_gdp = gdp_growth * tonnes)

# create variables

#function
epi_model <- function(i,j,k){
  
eulers_number <- exp(1)

## SUPPLY

v_ik <<- trade_data %>% 
  filter(country == i, commodity == k) %>% 
  summarise(tonnes = sum(tonnes))%>%
  pull() # total country i exports of commodity k

m_ik <<- trade_data %>%
  filter(trade_partner == i, commodity == k) %>%
  summarise(tonnes = sum(tonnes))%>%
  pull() # total country i imports of commodity k

d_gdp_i <<- trade_data %>% filter(country == i) %>% summarise(gdp_growth = first(gdp_growth)) %>% pull() # growth rate of GDP in exporting country i

d_gdp_j <<- trade_data %>% filter(trade_partner == j) %>% summarise(gdp_growth = first(gdp_growth)) %>% pull() # growth rate of GDP in destination country j

d_pop_j <<- trade_data %>% filter(trade_partner == j) %>% summarise(population_growth = first(population_growth)) %>% pull() # growth rate of population in destination country j

average_tariff_ik <<- trade_data %>%
  filter(country != i, commodity == k)%>%
  summarise(tariff = weighted.mean(inbound_tariff, w = tonnes))%>%
  pull(tariff) # average tariff faced by country i's exports of commodity k. 
# I filter out country i becuase tariffs are listed as 'inbound tariff' by the respective country. 
# So this leaves us with the destination countries' tariffs on commodity j. 
# This average is weighted by tonnes.

average_tariff_k <<- trade_data %>%
  filter(commodity == k)%>%
  summarise(tariff = weighted.mean(inbound_tariff, w = tonnes))%>%
  pull(tariff) # as above but this is a tonnes-weighted average for 
# all tariffs applied to the good, including by country i.

average_tariff_jk <<- trade_data %>%
  filter(trade_partner == j, commodity == k)%>%
  summarise(tariff = weighted.mean(inbound_tariff, w = tonnes))%>%
  pull(inbound_tariff) # the average tariff faced by all imports of commodity k by destination market j.

tariff_ijk <<- trade_data %>%
  filter(country == j, trade_partner == i, commodity == k)%>%
  pull(inbound_tariff) # the tariff faced by commodity k when destination country j imports it from source country i.

v_ik_times_d_gdp_i <- country_sums %>%
  filter(country == i, commodity == k)%>%
  pull(tonnes_d_gdp) # the total volume of commodity k exported by the exporting country,
# multiplied by the predicted growth rate of GDP

sum_over_i_v_ik_times_d_gdp_i <- country_sums %>%
  filter(commodity == k)%>%
  summarise(sum_tonnes_d_gdp = sum(tonnes_d_gdp))%>%
  pull() # the above term but summed over exporting countries i.


supply_first_term <<-v_ik_times_d_gdp_i / sum_over_i_v_ik_times_d_gdp_i
 
supply_second_term <<- if_else(v_ik/m_ik < 1, v_ik/m_ik, 1)

supply_third_term <<- (1+average_tariff_ik)/(1+average_tariff_k)

supply <<- supply_first_term * supply_second_term * supply_third_term


## DEMAND

v_jk <<- trade_data %>%
    filter(trade_partner == j, commodity == k)%>%
    summarise(tonnes = sum(tonnes))%>%
    pull()
      #total imports in destination market j of commodity k

em_jk_gdp <<- trade_data %>% 
  filter(country = j, commodity = k)%>%
  first(elast_imp_per_cap_wrt_gdp_per_cap)%>%
  pull()

demand_first_term <<- v_jk * ((d_gdp_j / d_pop_j)^em_jk_gdp) * d_pop_j

demand_second_term <<- (1+ average_tariff_jk) / (1 + tariff_ijk)

demand <<- demand_first_term * demand_second_term


## EASE OF TRADE

average_log_distance_jk <<- trade_data %>%
  filter(trade_partner == j, commodity = k)%>%
  mutate(log_distance = log(distance))%>%
  summarise(average_log_distance_jk = mean(log_distance))%>%
  pull()
  
log_distance_ij <<- trade_data %>%
  filter(country == i, trade_partner = j)%>%
  mutate(log_distance = log(distance))%>%
  first()%>%
  pull()
  
abs_val_diff_in_log_distances <<- abs(average_log_distance_jk - log_distance_ij)
 
ease_of_trade_first_term <- eulers_number ^ -abs_val_diff_in_log_distances


v_ik_df <<- trade_data %>%
  select(country, trade_partner, commodity, tonnes)%>%
  group_by(country, commodity)%>%
  summarise(v_ik = sum(tonnes))%>%
  ungroup()

v_jk_df <<- trade_data %>%
  select(country, trade_partner, commodity, tonnes)%>%
  group_by(trade_partner, commodity)%>%
  summarise(v_jk = sum(tonnes))%>%
  ungroup()

vk_df <<- trade_data %>%
  select(country, trade_partner, commodity, tonnes)%>%
  group_by(commodity)%>%
  summarise(v_k = sum(tonnes))%>%
  ungroup()

commodity_sums <<- left_join(v_ik_df, vk_df, by = "commodity")%>%
  left_join(., v_jk_df, by = "commodity")

sum_over_k_v_ik_over_vk_then_times_v_jk <<- commodity_sums %>%
  mutate(v_ik_over_vk_then_times_v_jk = (v_ik / v_k)* v_jk)%>%
  filter(country == "i", trade_partner == "j")%>%
  summarise(sum_over_k_v_ik_over_vk_then_times_v_jk = sum(v_ik_over_vk_then_times_v_jk))%>%
  pull(sum_over_k_v_ik_over_vk_then_times_v_jk)

v_ij <<- trade_data %>% 
  filter(country == "i", trade_partner == "j") %>% 
  summarise(v_ij = sum(tonnes))%>%
  pull() # total country i exports to destination country j

ease_of_trade_second_term <<- v_ij / sum_over_k_v_ik_over_vk_then_times_v_jk

ease_of_trade <<- ease_of_trade_first_term * ease_of_trade_second_term


ep_ijk <<- supply * demand * ease_of_trade

}

epi_model("Michaelia", "Kathrynland", "wheat")




                        
                               
                               
                               