## IMPACT MFS maps
## Author: Sirimon Thomas - sirimon.thomas@impact-initiatives.org
## Date: 01/05/2023

library(tidyverse)
library(here)
rm(list=ls())


#####
#import clean jmmi data
jmmi <- read.csv('input/JMMI_data.csv', na.strings = '')


#### ACCESSIBILITY #### -----------------------------------------

#AC.1
#calculate '% of vendors selecting an option other than "Hazardous, damaged, or unsafe buildings in the marketplace," 
#"Hazards or damage on roads leading to the marketplace," "No issues," "Don't know," or "Prefer not to answer"'

mfs_access_physical <- jmmi %>%
  mutate(physical_access_true = if_else(rowSums(select(.,contains('accessibility_physical.') 
                                                       & -contains('no_issues') 
                                                       & -contains('dont_know')
                                                       & -contains('hazardous_buildings')
                                                       & -contains('hazardous_roads'))) > 0,1,0)) %>% 
  group_by(state,county,location) %>%
  summarise(physical_access = round(sum(physical_access_true)/n()*100,1)) %>%
  mutate(physical_access_score = case_when(physical_access<5 ~ 8,
                                           physical_access>=5 & physical_access<10 ~ 6,
                                           physical_access>=10 & physical_access<25 ~ 4,
                                           physical_access>=25 & physical_access<50 ~ 2,
                                           physical_access>=50 ~ 0))

#AC.2
#calculate '% of vendors selecting "Hazards or damage on roads leading to the marketplace"'

mfs_access_roads <- jmmi %>% 
  group_by(state,county,location) %>%
  summarise(physical_roads_access = round(sum(accessibility_physical.hazardous_roads)/n()*100,1)) %>%
  mutate(physical_roads_access_score = case_when(physical_roads_access<5 ~ 4,
                                                 physical_roads_access>=5 & physical_roads_access<10 ~ 3,
                                                 physical_roads_access>=10 & physical_roads_access<25 ~ 2,
                                                 physical_roads_access>=25 & physical_roads_access<50 ~ 1,
                                                 physical_roads_access>=50 ~ 0))

#AC.3
#if any vendor responds "Yes," the market is coded as a "Yes"

mfs_access_social <- jmmi %>%
  mutate(accessibility_social_access = if_else(accessibility_social_access == 'yes',1,0)) %>%
  group_by(state,county,location) %>%
  summarise(social_access_score = if_else(sum(accessibility_social_access)>0,0,2))

#AC.4
#calculate % of vendors selecting an option other than "No issues" or "Prefer not to answer"

mfs_access_safety <- jmmi %>% 
  mutate(safety_access_true = if_else(rowSums(select(.,contains('accessibility_safety.') 
                                                     & -contains('no_issues') 
                                                     & -contains('dont_know'))) > 0,1,0)) %>%
  group_by(state,county,location) %>%
  summarise(safety_access = round(sum(safety_access_true)/n()*100,1)) %>%
  mutate(safety_access_score = case_when(safety_access<5 ~ 3,
                                         safety_access>=5 & safety_access<10 ~ 2,
                                         safety_access>=10 & safety_access<20 ~ 1,
                                         safety_access>=20 ~ 0))


#### AVAILABILITY #### -------------------------------------

#AV.1
#Mode of vendor responses for each item

# function to take most common answer (statistical mode). This favours available over limited over unavailable i.e. if one trader says 'available' and one says 'limited', the location will be given 'available'
mode_fun <- function(x) {
  #remove NAs and sort alphabetically - this priotises available over limited over unavailable
  x <- x[!is.na(x)] %>% sort()
  
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

mfs_available <- jmmi %>%
  select(state, county, location, ends_with('_available') & !contains(c('summary','wholesale','usd_available','ssp_available','sdg_available','etb_available','ugx_available','kes_available','cdf_available','xaf_available'))) %>% #NB excludes currencies
  group_by(state, county, location) %>%
  summarise(across(ends_with('_available'), mode_fun)) %>% #use mode_fun to extract mode value
  mutate(across(ends_with('_available'), ~ case_when( #convert to scores
    . == 'available' ~ 3,
    . == 'limited' ~ 2,
    . == 'unavailable' ~ 0))) %>%
  ungroup() %>%
  mutate(availability_score = rowSums(across(ends_with('_available')), na.rm = T)) %>%#sum rows to give overall score - max score is 105 (35 items x 3)
  select(state, county, location, availability_score)


#### AFORDABILITY #### -------------------------------------

#AF.1
#Median of vendor responses for each item

# function to score prices based on national median price
price_score_fun <- function(item_median){
  item <- deparse(substitute(item_median))
  result = case_when(
    {{item_median}} > median_national[[item]] * 1.5 ~ -2,
    {{item_median}} > median_national[[item]] * 1.25 ~ -1.5,
    {{item_median}} > median_national[[item]] * 1.1 ~ -1,
    {{item_median}} > median_national[[item]] * 0.90 ~ 0,
    {{item_median}} > median_national[[item]] * 0.75 ~ 1,
    {{item_median}} > median_national[[item]] * 0.5 ~ 1.5,
    {{item_median}} <= median_national[[item]] * 0.5 ~ 2,
    TRUE ~ NA_real_
  )
  return (result)
}

#create national median dataset
median_national <- jmmi%>%
  select(state,county,location,contains('_price_unit_ssp'),-contains('wholesale')) %>% # NB does not include currency prices
  lapply(median, na.rm=T) %>% #ignore warning messages- these are from trying to calculate the median on the state, county and location columns
  as.data.frame() %>%
  mutate(across(c(state, county, location), as.character))

n_items <- ncol(median_national) - 3 #also used later in final pillar score calculations

#calculate scores
mfs_afford_price <- jmmi%>%
  select(state,county,location,contains('_price_unit_ssp'),-contains('wholesale')) %>%
  group_by(state, county, location)%>% 
  summarise(across(everything(), ~median(., na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(across(contains('_price_unit_ssp'), price_score_fun), #apply function to score each item
         afford_price_sum = rowSums(across(contains('_price_unit_ssp') | contains('_price_ind')), na.rm = T), # row sums to give total score - in SSD the range is -70 to 70 (2x35 items)
         afford_price_score = ((afford_price_sum - (-2*n_items)) * (12-0)) / ((2*n_items) - (-2*n_items) + 0) # use scaling formula to scale values to 0-12 - optional
  ) %>% 
  select(state, county, location, afford_price_sum, afford_price_score)

#AF.2
#% of vendors selecting an option other than "No issues," "Don't know," or "Prefer not to answer"

mfs_afford_finance <- jmmi %>% 
  mutate(afford_finance_true = if_else(rowSums(select(.,contains('affordability_financial.') 
                                                     & -contains('no_issues') 
                                                     & -contains('dont_know'))) > 0,1,0)) %>%
  group_by(state,county,location) %>%
  summarise(afford_finance = round(sum(afford_finance_true)/n()*100,1)) %>%
  mutate(afford_finance_score = case_when(afford_finance<10 ~ 9,
                                          afford_finance>=10 & afford_finance<25 ~ 6,
                                          afford_finance>=25 & afford_finance<50 ~ 3,
                                          afford_finance>=50 ~ 0))

#AF.3
#% of vendors selecting "Yes"

mfs_afford_price_vol <- jmmi %>% 
  mutate(affordability_price_volatility = if_else(affordability_price_volatility == 'yes',1,0)) %>%
  group_by(state,county,location) %>%
  summarise(afford_price_vol = round(sum(affordability_price_volatility)/n()*100,1)) %>%
  mutate(afford_price_vol_score = case_when(afford_price_vol<10 ~ 6,
                                            afford_price_vol>=10 & afford_price_vol<25 ~ 4,
                                            afford_price_vol>=25 & afford_price_vol<50 ~ 2,
                                            afford_price_vol>=50 ~ 0))


#### RESILIENCE #### -------------------------------------

#RE.1
# For each vendor, subtract # restocking days from # days of remaining stock for each item or category; aggregate by taking the median of these vendor-level calculations

#in SSD, there are 3 categories - local food (sorghum and maize), imported food (all other food) and non-food items. This should be changed to match your context. You can either use categories or each item individually, depending on the data you have available.

mfs_resil_restock <- jmmi %>%
  select(state, county, location, ends_with(c('_stock_current', '_duration')) & -contains('wholesale')) %>% #select stock and restock duration columns
  mutate_if(is.logical, as.numeric) %>%
  rowwise() %>%
  mutate(
    #stock calculations
    local_food_stock_current = median(c(sorghum_grain_stock_current, maize_grain_stock_current), na.rm = T), #create median local cereal stock
    import_food_stock_current = median(c(wheat_flour_stock_current,rice_stock_current,groundnuts_stock_current,beans_stock_current,sugar_stock_current,salt_stock_current,cooking_oil_stock_current), na.rm = T), #create imported food stock median
    nfi_stock_current = median(c(soap_stock_current,jerrycan_stock_current,mosquito_net_stock_current,exercise_book_stock_current, #create median nfi stock
                                 blanket_stock_current,cooking_pot_stock_current,plastic_sheet_stock_current,pen_available_stock_current,
                                 pencil_available_stock_current,rubber_available_stock_current,sharpener_available_stock_current,rubber_rope_available_stock_current,
                                 kanga_available_stock_current,solar_lamp_available_stock_current,plastic_bucket_available_stock_current,
                                 sanitary_pads_available_stock_current), na.rm = T),
    
    #resilience days calculations
    local_food_stock = local_food_stock_current - food_supplier_local_duration,
    import_food_stock = import_food_stock_current - food_supplier_imported_duration,
    nfi_stock = nfi_stock_current - nfi_supplier_duration,
    
    #resilience score calculations
    local_food_stock = case_when(
      local_food_stock > 3 ~ 3,
      local_food_stock > 0 ~ 2,
      local_food_stock == 0 ~ 1,
      local_food_stock < 0 ~ 0),
    import_food_stock = case_when(
      import_food_stock > 3 ~ 3,
      import_food_stock > 0 ~ 2,
      import_food_stock == 0 ~ 1,
      import_food_stock < 0 ~ 0),
    nfi_stock = case_when(
      nfi_stock > 3 ~ 3,
      nfi_stock > 0 ~ 2,
      nfi_stock == 0 ~ 1,
      nfi_stock < 0 ~ 0)) %>%
  ungroup() %>%
  select(state, county, location, ends_with('_stock')) %>% #select relevant columns
  group_by(state, county, location) %>% 
  summarise(across(everything(), ~median(., na.rm = TRUE))) %>% #aggregate by location
  mutate(resil_restock_score = rowSums(across(ends_with('_stock')), na.rm = T))
  
#RE.2
#### see above
  
#RE.3
#% of vendors selecting "Yes"

mfs_resil_supply_diverse <- jmmi %>% 
  select(state, county, location, ends_with('_single')) %>%
  mutate(across(ends_with('_single'), ~if_else(. == 'yes',1,0))) %>%
  group_by(state,county,location) %>%
  summarise(across(ends_with('_single'), ~round(sum(., na.rm=T)/sum(!is.na(.))*100,1))) %>%
  ungroup() %>%
  mutate(food_supplier_local_single = case_when(food_supplier_local_single > 75 ~ 0,
                                             food_supplier_local_single > 50 ~ 1,
                                             food_supplier_local_single > 25 ~ 2,
                                             food_supplier_local_single <= 25 ~ 3),
         food_supplier_imported_single = case_when(food_supplier_imported_single  > 75 ~ 0,
                                              food_supplier_imported_single  > 50 ~ 1,
                                              food_supplier_imported_single  > 25 ~ 2,
                                              food_supplier_imported_single  <= 25 ~ 3),
         nfi_supplier_single = case_when(nfi_supplier_single > 75 ~ 0,
                                      nfi_supplier_single > 50 ~ 1,
                                      nfi_supplier_single > 25 ~ 2,
                                      nfi_supplier_single <= 25 ~ 3),
         
         resil_supply_diverse_score = rowSums(across(ends_with('_single')), na.rm = T)) #sum rows to give final score - in SSD max score is 9 (3 x 3 categories)

#RE.4
#% of vendors selecting an option other than "No difficulties" or "Prefer not to answer"

mfs_resil_supply <- jmmi %>% 
  mutate(resilience_supply_true = if_else(rowSums(select(.,contains('resilience_supply_chain.') 
                                                      & -contains('no_difficulties') 
                                                      & -contains('dont_know'))) > 0,1,0)) %>%
  group_by(state,county,location) %>%
  summarise(resil_supply = round(sum(resilience_supply_true)/n()*100,1)) %>%
  mutate(resil_supply_score = case_when(resil_supply<5 ~ 12,
                                        resil_supply>=5 & resil_supply<10 ~ 9,
                                        resil_supply>=10 & resil_supply<25 ~ 6,
                                        resil_supply>=25 & resil_supply<50 ~ 3,
                                        resil_supply>=50 ~ 0))

#### INFRASTRUCTURE #### -------------------------------------

#IN.1
#% of vendors selecting "Hazardous, damaged, or unsafe buildings in the marketplace"

mfs_infra_facilities <- jmmi %>% 
  group_by(state, county, location) %>%
  summarise(infra_facilities = round(sum(accessibility_physical.hazardous_buildings)/n()*100,1)) %>%
  mutate(infra_facilities_score = case_when(infra_facilities<5 ~ 4,
                                            infra_facilities>=5 & infra_facilities<10 ~ 3,
                                            infra_facilities>=10 & infra_facilities<25 ~ 2,
                                            infra_facilities>=25 & infra_facilities<50 ~ 1,
                                            infra_facilities>=50 ~ 0))

#IN.2
#% of vendors selecting an option other than "Yes, within my own business facilities" or "Yes, elsewhere within the marketplace"

mfs_infra_storage <- jmmi %>% 
  mutate(infrastructure_storage = if_else(infrastructure_storage == 'no_store_facility_outside' |
                                            infrastructure_storage == 'no_store_at_home' |
                                            infrastructure_storage == 'other' |
                                            infrastructure_storage == 'prefer_not_answer', 1,0)) %>%
  group_by(state, county, location) %>%
  summarise(infra_storage = round(sum(infrastructure_storage)/n()*100,1)) %>%
  mutate(infra_storage_score = case_when(infra_storage<10 ~ 3,
                                         infra_storage>=10 & infra_storage<25 ~ 2,
                                         infra_storage>=25 & infra_storage<50 ~ 1,
                                         infra_storage>=50 ~ 0))

#IN.3
#% of vendors selecting an option other than "Cash (local currency)", "Cash (foreign currencies)", or "Prefer not to answer"

mfs_infra_payment <- jmmi %>% 
  mutate(infra_payment_true = if_else(rowSums(select(.,contains('modalities_which.') 
                                                         & -contains('cash') 
                                                         & -contains('dont_know'))) > 0,1,0)) %>%
  group_by(state, county, location) %>%
  summarise(infra_payment = round(sum(infra_payment_true, na.rm = T)/n()*100,1)) %>%
  mutate(infra_payment_score = case_when(infra_payment>75 ~ 3,
                                         infra_payment>50 ~ 2,
                                         infra_payment>25 ~ 1,
                                         infra_payment<=25 ~ 0))


#======================================== CALCULATE MFS =============================================

mfs_data_list <- list(mfs_access_physical,mfs_access_roads,mfs_access_social,mfs_access_safety, #accessibility pillar - max 17
                      mfs_available,                                                            #availability pillar - max depends on number of items or categories
                      mfs_afford_price,mfs_afford_finance,mfs_afford_price_vol,                 #affordability pillar - max depends on number of items
                      mfs_resil_restock,mfs_resil_supply_diverse,mfs_resil_supply,              #resilience pillar - max depends on number of items or categories
                      mfs_infra_facilities,mfs_infra_storage,mfs_infra_payment)                 #infrastructure pillar - max 10

#create full dataset
mfs <- mfs_data_list %>% reduce(full_join, by=c('state','county','location'))

#calculate final mfs
mfs <- mfs %>%
  select(state, county, location, contains('_score'))%>%
  #calculate pillar scores, scale to 0-1 by dividing by the max score for that pillar, then apply weights
  mutate(mfs_accessibility_pillar_score = (rowSums(across(contains('access_score'))) /17) *25,
         mfs_availability_pillar_score = (availability_score /(n_items*3)) *30,
         mfs_affordability_pillar_score = (rowSums(across(contains('afford'))) /27) *15,
         mfs_resilience_pillar_score = (rowSums(across(contains('resil'))) /30) *20,
         mfs_infrastructure_pillar_score = (rowSums(across(contains('infra'))) /10) *10,
         
         #calculate final score
         mfs_score = rowSums(across(contains('_pillar_score')))) %>%
  select(state, county, location, contains(c('_pillar_score', 'mfs_score')))

#export

write.csv(mfs,
          file = 'output/mfs_ssd.csv',
          row.names = F)









