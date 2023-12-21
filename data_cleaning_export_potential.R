library(tidyverse)
options(scipen = 999)

# negate function
'%not_in%' <- Negate('%in%')

### Data cleaning for export potential model

## 1) import the comtrade data: imports of FFD by reporting country (destination) and partner country (origin)

comtrade1 <- readRDS("finaltablecomtradeFFD.RDS")

# comtrade FFD remaining countries
comtrade2 <- readRDS("finaltablecomtraderemainingcountries2017to2021.RDS")

# stack the two together
# filter for the most recent 5 years skipping 2020, and then average across time
comtrade <- rbind(comtrade1, comtrade2)%>%
  filter(Year %in%c(2016, 2017, 2018, 2019, 2021))%>%
  group_by(ReporterISO3numeric, Reportername, PartnerISO3numeric, Partnername, FFD)%>%
  summarise(across(.cols = c(WeightinKG, ValueinUSdollars), ~ mean(.x, na.rm = TRUE)))


## 2) macmap tariffs (minimum AVEs per FFD)
macmap1 <- readRDS("finaltablemacmapprocessedFFD.RDS")

# macmap remaining countries
macmap2 <- readRDS("finaltablemacmapprocessedFFDremainingcountries2017to2021.RDS")


#stack them together, filter for most recent 5 years and then average across time
# macmap <- rbind(macmap1, macmap2)%>%
#   filter(Year %in% c(2016, 2017, 2018, 2019, 2021))%>%
#   group_by(ReportingCountry, PartnerCountry, FFD)%>%
#   summarise(MinAve = mean(MinAve, na.rm = TRUE))%>%
#   ungroup()

# number of unique reporter partner commodity combinations: 5,432,011

# try a different method: select the most recent year for each combination
macmap <- rbind(macmap1, macmap2)

macmap <- macmap %>%
  group_by(ReportingCountry, PartnerCountry, FFD)%>%
  filter(Year == max(Year))%>%
  ungroup()

# na_macmap <- macmap %>% filter(is.na(MinAve))
# #no NAs.
# 
# # what is the profile of latest year available for each combination?
# count_years <- macmap %>%
#   group_by(Year)%>%
#   count()%>%
#   ungroup()
# 
# zero_count_years <- macmap %>%
#   filter(MinAve  < 0.0001)%>%
#   group_by(Year)%>%
#   count()%>%
#   ungroup()
# 
# count_years <- left_join(count_years, zero_count_years, by = "Year")
# 
# count_years <- mutate(count_years, percent_zero = (n.y / n.x) * 100)
# 
# count_years <- count_years %>% rename(latest_year = Year, count = n.x, of_which_AVE_zero = n.y)
# 
# write_csv(count_years, "year_breakdown_of_macmap_ave_data.csv")

# filtering for only the latest available year, most data comes from the most recent years.

#number of unique reporter partner commodity combinations: 6,420,404

macmap <- select(macmap, -Year)

# remove the original datasets
rm(comtrade1, comtrade2, macmap1, macmap2)


## 3) import concordance table
concordance_table <- readxl::read_xlsx("Comtrade_vs_Macmap_vs_FAO_vs_Worldbank.xlsx")%>%
  dplyr::mutate(ID = row_number())

#select relevant rows to join to macmap.
comtrade_macmap_concordance <- concordance_table %>%
  select(ID, 
         comtrade_country_num = `ComtradeCode (from ComtradePlus - data items - 17 Mar 2020.xlsx)`, 
         comtrade_country_name = ComtradeWords,
         macmap_country_num = `MacmapCode (from countrycodesfromSharepoint.csv)`,
         macmap_country_name = MacmapWords,
         macmap_country_iso = Macmap3Letters,
         world_bank_country_iso = WorldBankWebsite3Letters
  )
         

# note code variables ('comtrade_country_num' and 'macmap_country_num') are numeric. The number of digits varies
# according to the number (eg. '7' or '25'. We need everything to be in a three digit format. Adding the 0s at the beginning turns these variables
# in to character variables. The sprintf function can do this.

comtrade_macmap_concordance$macmap_country_num <- sprintf("%03d", comtrade_macmap_concordance$macmap_country_num)
comtrade_macmap_concordance$comtrade_country_num <- sprintf("%03d", comtrade_macmap_concordance$comtrade_country_num)

# join the concordance table to macmap to give each row the corresponding comtrade country code for reporter country.
macmap <- left_join(macmap, comtrade_macmap_concordance, by = c("ReportingCountry" = "macmap_country_num"))

#rename these new columns as destination_country_[name]. The reason for this is that we are next going to join the concordance table again, but this time
# by origin country. We need to differentiate these identifier columns.
macmap <- rename(macmap, c("destination_country_comtrade_num" = "comtrade_country_num",
                            "destination_country_comtrade_name" = "comtrade_country_name",
                            "destination_country_macmap_num" = "ReportingCountry",
                            "destination_country_macmap_name" = "macmap_country_name",
                            "destination_country_macmap_iso" = "macmap_country_iso",
                            "destination_country_world_bank_iso" = "world_bank_country_iso",
                            "destination_country_id" = "ID"
))

# repeat this, but this time join by origin country
macmap <- left_join(macmap, comtrade_macmap_concordance, by = c("PartnerCountry" = "macmap_country_num"))

#rename these new variables to specify that they are for the origin country
macmap <- rename(macmap, c("origin_country_comtrade_num" = "comtrade_country_num",
                           "origin_country_comtrade_name" = "comtrade_country_name",
                           "origin_country_macmap_num" = "PartnerCountry",
                           "origin_country_macmap_name" = "macmap_country_name",
                           "origin_country_macmap_iso" = "macmap_country_iso",
                           "origin_country_world_bank_iso" = "world_bank_country_iso",
                           "origin_country_id" = "ID"
))



#join the ID in comtrade_macmap_concordance to comtrade destination country
concordance_for_comtrade <- comtrade_macmap_concordance %>% select(comtrade_country_num, ID)

comtrade <- left_join(comtrade, concordance_for_comtrade, by = c("ReporterISO3numeric" = "comtrade_country_num"))%>%
  rename("destination_country_id" = "ID")

# join the ID in comtrade_macmap_concordance to origin country
comtrade <- left_join(comtrade, concordance_for_comtrade, by = c("PartnerISO3numeric" = "comtrade_country_num"))%>%
  rename("origin_country_id" = "ID")


## 4) join the macmap data to the comtrade data by destination country ID, origin country ID and FFD
combined_data <- left_join(comtrade, macmap, by = c("destination_country_id",
                                                    "origin_country_id",
                                                    "FFD"))

# find the unique combinations of country IDs for combined_data
colnames(combined_data)

country_codes <- combined_data %>% ungroup()%>%
                                   select("destination_country_id",
                                          "ReporterISO3numeric",
                                          "destination_country_comtrade_num",   
                                          "destination_country_comtrade_name",
                                          "Reportername",
                                          "destination_country_macmap_num",
                                          "destination_country_macmap_name",
                                          "destination_country_macmap_iso",
                                          "destination_country_world_bank_iso"
                                          ) %>%
                                    unique()

# # test for concordance on a random country which was showing a bad join before.
# test_macmap <- filter(macmap, destination_country_id == 129)
# test_comtrade <- filter(comtrade, destination_country_id == 129)
# test_combined_data <- filter(combined_data, destination_country_id == 129)

# filter for the NA rows in combined data to inspect them. It currently seems that they are
# being caused by origins that aren't in the macmap data because they aren't countries,
# eg. 'asia - other'. If this is the only cause, then it's safe to delete them because
# we have world totals in the separate comtrade world dataset.

# na_combined_data <- filter(combined_data, is.na(MinAve))
# 
# na_combined_data_unique <- select(na_combined_data, c(2,4))%>%
#   unique()
# 
# count_na_importer_country <- na_combined_data_unique %>%
#   group_by(Reportername)%>%
#   count()

# There are 52,508 NA values for tariff AVEs, of which 1933 are for unique origin-destination combinations. 
# These are mostly for small countries or small economies. Remove them.

combined_data <- filter(combined_data, !is.na(MinAve))%>%
  ungroup()

combined_data <- combined_data %>% mutate(volume_tonnes = WeightinKG / 1000)

# for the combined data, save only the 3 number, 3 letter and ID identifiers.
combined_data <- combined_data %>%
  select(iso3num_d = destination_country_comtrade_num, 
         iso3_d = destination_country_world_bank_iso,
         destination_country_id,
         iso3num_o = origin_country_comtrade_num, 
         iso3_o = origin_country_world_bank_iso, 
         origin_country_id,
         FFD,
         volume_tonnes,
         value_usd = ValueinUSdollars,
         MinAve
  )

# I need to change the letters for Montenegro  499 MNE, Serbia 688 SRB, Switzerland 757 CHE, India 699 IND
combined_data <- combined_data %>% mutate(iso3_o = case_when(iso3num_o == "499" ~ "MNE",
                                                             iso3num_o == "688" ~ "SRB",
                                                             iso3num_o == "757" ~ "CHE",
                                                             iso3num_o == "699" ~ "IND",
                                                             TRUE ~ iso3_o),
                                          iso3_d = case_when(iso3num_d == "499" ~ "MNE",
                                                             iso3num_d == "688" ~ "SRB",
                                                             iso3num_d == "757" ~ "CHE",
                                                             iso3num_d == "699" ~ "IND",
                                                             TRUE ~ iso3_d))

## 5) Gravity data

# gravity_data <- readRDS("Gravity_V202211.rds")
# 
# # filter for years 2015 - 2021 (2015 because we need t-1 to calculate growth rates)
# # filter for only countries that exist now
# # remove the self-paired rows (o = d)
# 
# gravity_data <- gravity_data %>% 
#                 filter(year %in% c(2015:2021), 
#                        country_exists_o == 1,
#                        country_exists_d == 1,
#                        country_id_o != country_id_d
#                       )
# 
# # drop country_id and keep only these country codes. The suffixes _d and _o stand for destination and origin.
# # iso3, iso3num
# # distw_arithmetic: Population-weighted distance between most populated cities (arithmetic mean)
# # pop_o, pop_d, gdp_o, gdp_d,
# # tradeflow_comtrade_d, tradeflow_baci, tradeflow_imf_d
# 
# gravity_data <- gravity_data %>%
#                      select (year, 
#                              iso3_o, 
#                              iso3_d, 
#                              iso3num_o, 
#                              iso3num_d, 
#                              distw_arithmetic, 
#                              pop_o, 
#                              pop_d, 
#                              gdp_o, 
#                              gdp_d
#                              # tradeflow_comtrade_d, 
#                              # tradeflow_baci, 
#                              # tradeflow_imf_d
#                      )
# 
# 
# # create growth rates and log variables
# gravity_data <- gravity_data %>%
#   group_by(iso3_o, iso3_d)%>%
#   mutate(pop_growth_o = (pop_o - dplyr::lag(pop_o)) / dplyr::lag(pop_o),
#          pop_growth_d = (pop_d - dplyr::lag(pop_d)) / dplyr::lag(pop_d),
#          gdp_growth_o = (gdp_o - dplyr::lag(gdp_o)) / dplyr::lag(gdp_o),
#          gdp_growth_d = (gdp_d - dplyr::lag(gdp_d)) / dplyr::lag(gdp_d),
#          log_distance = log(distw_arithmetic)
#       ) %>%
#   ungroup()
#   
# # drop 2015. Create averages across time.
# gravity_data <- gravity_data %>%
#   filter(year != 2015)%>%
#   group_by(iso3_o, iso3_d, iso3num_o, iso3num_d)%>%
#   summarise(across(c(distw_arithmetic:log_distance), ~ mean(.x, na.rm = TRUE)))%>%
#   ungroup()
# 
# ## several sources are given for estimates of the total value of bilateral trade flows. All of them have a lot of 0 or missing values.
# ## take the mean of the trade value estimates per country pair.
# # 
# # gravity_data <- gravity_data %>%
# #                 mutate(total_imports_from_o_to_d = rowMeans(select(., c(tradeflow_comtrade_d, 
# #                                                                         tradeflow_baci, 
# #                                                                         tradeflow_imf_d)),
# #                                                                        na.rm = TRUE)
# #                       )
# 
# # # remove the original estimates to leave only the mean column.
# # gravity_data <- gravity_data %>% select(-tradeflow_comtrade_d, 
# #                                         -tradeflow_baci, 
# #                                         -tradeflow_imf_d)
# 
# # # replace the NA values in total imports with 0.
# # gravity_data$total_imports_from_o_to_d[is.na(gravity_data$total_imports_from_o_to_d)] <- 0
# 
# # make iso3_num_o and iso3_num_d characters with three digits.
# gravity_data$iso3num_o <- sprintf("%03d", gravity_data$iso3num_o)
# gravity_data$iso3num_d <- sprintf("%03d", gravity_data$iso3num_d)
# 
# # In gravity data, I need to change India 356 to 699, Switzerland 756 to 757 and Romania from ROU to ROM.
# gravity_data <- gravity_data %>% mutate(iso3num_o = case_when(iso3_o == "IND" ~ "699",
#                                                               iso3_o == "CHE" ~ "757",
#                                                               TRUE ~ iso3num_o),
#                                         iso3num_d = case_when(iso3_d == "IND" ~ "699",
#                                                               iso3_d == "CHE" ~ "757",
#                                                               TRUE ~ iso3num_d),
#                                         iso3_o = case_when(iso3num_o == "642" ~ "ROM",
#                                                            TRUE ~ iso3_o),
#                                         iso3_d = case_when(iso3num_d == "642" ~ "ROM",
#                                                            TRUE ~ iso3_d))
# 
# 
# # put the country fixed characteristics (not bilateral) in a separate table, then remove them from the gravity dataframe. 
# # also remove the non-log-transformed distance.
# 
# country_pop_and_gdp <- gravity_data %>%
#   select(country_iso = iso3_o, 
#          country_iso3_num = iso3num_o, 
#          pop = pop_o,
#          gdp = gdp_o,
#          pop_growth = pop_growth_o,
#          gdp_growth = gdp_growth_o)%>%
#   unique()%>%
#   filter(!is.na(gdp))
# 
# gravity_data <- gravity_data %>% select(-c(pop_o,
#                                            gdp_o,
#                                            pop_growth_o,
#                                            gdp_growth_o,
#                                            pop_d,
#                                            gdp_d,
#                                            pop_growth_d,
#                                            gdp_growth_d,
#                                            distw_arithmetic))

# # save the processed datasets
# 
# write_rds(combined_data, "combined_data_pre_join.rds") # this is an intermediate step in case I need to re-do the join
# write_rds(gravity_data, "gravity_data.rds")
# write_rds(country_pop_and_gdp, "country_pop_and_gdp.rds")
# 
# 
# # read the combined data pre-join
# combined_data <- readRDS("combined_data_pre_join.rds")

# read the gravity data in. I've commented out the data wrangling for this variable so that in future we can skip to the clean file.
# this is because the original file is >100mb and I want to use git and github. I'll omit the large raw file using git ignore.
# Also because cleaning the gravity data is straightforward and I can be more confident that I've done what I need to do correctly.

gravity_data <- readRDS("gravity_data.rds")

# combine them both
combined_data <- left_join(combined_data, select(gravity_data, -c(iso3num_o, iso3num_d)), by = c("iso3_o", "iso3_d"))

# # test for NAs; 
# combined_data_na <- combined_data %>%
#   filter(is.na(log_distance),
#          iso3_d != iso3_o,
#          iso3_d %not_in% c("ZAR", "PCE", "TMP", "ANT", "ATF") & iso3_o %not_in% c("ZAR", "PCE", "TMP", "ANT", "ATF"))
# 
combined_data <- combined_data %>%
  filter(!is.na(log_distance))
# 
# colSums(is.na(combined_data))
#all clear: no more NA values. It was only observations where a country was paired with itself
# or the group of five unusual small economies or not fully recognised countries.

# Find all rows where total trade is lower than FFD trade.
# combined_data_wrong <- combined_data %>%
#   group_by(iso3num_d, iso3num_o, iso3_d, iso3_o, destination_country_id, origin_country_id)%>%
#   summarise(value_usd = sum(value_usd),  total_imports_from_o_to_d = first(total_imports_from_o_to_d))%>%
#   mutate(total_imports_from_o_to_d = total_imports_from_o_to_d * 1000)%>%
#   ungroup()
# #25,592 observations
# 
# combined_data_wrong <- combined_data_wrong %>%
#   filter(value_usd >= total_imports_from_o_to_d)
# #7614 observations
# 
# # almost a third of the country pairs have FFD totals which are higher than the total trade given by CEPII; this can't be right. This is after I've adjusted by
# # multiplying the CEPII estimates by 1000 to make the units the same.
# 
# # going back, I can see that the three estimates of value I used didn't just contain NAs. They also contained extremely small values (approx 0) 
# # in some columns with large values in others, suggesting that the mean which I used has effectively halved or 'thirded' the value. 
# # Maybe if I take the maximum the value will be very different.
# 
# # re-load the original gravity dataset and then do this:
# test_trade_totals <- gravity_data %>% summarise(comtrade = sum(tradeflow_comtrade_d, na.rm = TRUE), 
#                                                 baci = sum(tradeflow_baci, na.rm = TRUE), 
#                                                 imf = sum(tradeflow_imf_d, na.rm = TRUE))
# 
# # need to remove all rows for which all three have NA values.
# test_trade_max <- gravity_data %>%
#   rowwise()%>%
#   mutate(max_total_imports_from_o_to_d = max(c(tradeflow_comtrade_d, 
#                                                           tradeflow_baci, 
#                                                           tradeflow_imf_d), na.rm = TRUE))
#   
#   test_trade_numbers_gravity <- gravity_data %>% select(tradeflow_comtrade_d, 
#                                                         tradeflow_baci, 
#                                                         tradeflow_imf_d)%>%
#   summarise(count_comtrade = length(tradeflow_comtrade_d[tradeflow_comtrade_d > 1]),
#             count_baci = length(tradeflow_baci[tradeflow_baci > 1]),
#             count_imf = length(tradeflow_imf_d[tradeflow_imf_d > 1])
#   )
  
# None of these tests help us very much. I still don't know a methodologically sound way of getting the most accurate tradeflow total estimates 
# out of the CEPII data. For now I will have to remove this variable and treat 'all FFD' as total trade - which is wrong - until 
# we find a reliable 'total trade' figure.


combined_data <- mutate(combined_data, elast_m_gdp_jk = 1)

combined_data <- distinct(combined_data)

# save the combined data ready for use to construct the model. Give it a new name: bilateral_data.
write_rds(combined_data, "bilateral_data.rds")
