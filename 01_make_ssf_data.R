source("00_setup.R") #setup libraries etc.


# download data -----------------------------------------------------------

if (!dir.exists(here("data"))) {
  dir.create(here("data"))
  download.file(
    "https://zenodo.org/api/records/13887065/files-archive",
    file.path("data","tmp.zip"),
    mode = "wb" # for windows users
  )
  
  
  unzip(file.path("data","tmp.zip"), exdir = file.path("data"))
  
  unlink( file.path("data","tmp.zip"))
  
  zips <- list.files(here("data"))
  
  zips <- zips[str_detect(zips, "\\.zip")]
  
  for (z in zips){
    
    unzip(file.path("data",z), exdir = file.path("data"))
    
    unlink( file.path("data",z))
    
  }
  
}

# safely create union countries within each UN region to get just the UN region; would be better to just get the actual shapefile

# load data ---------------------------------------------------------------

# mike_global <- readxl::read_xlsx(here("data","clean_mike_data.xlsx"), sheet = "regional") |> 
#   janitor::clean_names()

mike_global <-read_csv(here("data","3_anon_figures","anon_figures_catch","anon_clean_mike_data_regional.csv")) |> 
  janitor::clean_names()

# mike_country <- readxl::read_xlsx(here("data","clean_mike_data.xlsx"), sheet = "country") |> 
#   janitor::clean_names() |> 
#   rename(catch_ssf = ssf_catch_preferred_mix) |> 
#   mutate(catch_ssf = catch_ssf / 1e6) |> 
#   pivot_wider(names_from = "source", values_from = "catch_ssf", 
#               names_prefix = "ssf_")

mike_country <- read_csv(here("data", "3_anon_figures","anon_figures_catch", "anon_clean_mike_data_country.csv")) |>
  janitor::clean_names() |>
  rename(catch_ssf = ssf_catch_preferred_mix) |>
  mutate(catch_ssf = catch_ssf / 1e6) |>
  pivot_wider(names_from = "source",
              values_from = "catch_ssf",
              names_prefix = "ssf_")


# should_i_stay_or_should_i_go_now <-
#   read_csv(here("data", "mike_catch_estimates_MM.csv")) |>
#   select(country_name, contains("is_na"))

should_i_stay_or_should_i_go_now <-
  read_csv(here("data","3_anon_figures","anon_figures_catch", "anon_mike_catch_estimates_MM.csv")) |>
  select(anon_country_name, contains("is_na"))

mike_country <- mike_country |> 
  left_join(should_i_stay_or_should_i_go_now, by = "anon_country_name") |> 
  mutate(ssf_marine = if_else(is.na(ssf_marine) & !marine_na_is_na, 0,ssf_marine),
         ssf_inland = if_else(is.na(ssf_inland) & !inland_na_is_na, 0,ssf_inland)) |> 
  rowwise() |> 
  mutate(catch_ssf = altsum(ssf_marine, ssf_inland, na.rm = FALSE)) |> 
  ungroup() 

# 
# fishstat_catch <- readr::read_csv(here("data", "20240207_FAO_FishStat_Catch.csv")) %>%
#   janitor::clean_names() |> 
#   select(-country_name,-region)


fishstat_catch <- readr::read_csv(here("data","3_anon_figures","anon_figures_catch", "anon_20240207_FAO_FishStat_Catch.csv")) %>%
  janitor::clean_names() 

# data <- readr::read_csv(here("data", "20240201_Employ_Livelih_Landvalue.csv")) %>%
#   janitor::clean_names() |> 
#   left_join(mike_country, by = c("iso3code"  = "country_name")) |> 
#   left_join(fishstat_catch, by = "iso3code")

data <- readr::read_csv(
  here(
    "data",
    "3_anon_figures",
    "anon_figures_employment_landed_value",
    "anon_20240201_Employ_Livelih_Landvalue.csv"
  )
) %>%
  janitor::clean_names() |>
  left_join(mike_country,
            by = c("anon_iso3code", "anon_country_name", "region")) |>
  left_join(fishstat_catch,
            by = c("anon_iso3code", "anon_country_name", "region"))

region_lookup <- data %>%
  select(anon_country_name, region) %>%
  unique()

nutrition_name <- "Figure1_nutrition_data.xlsx"

# portions_marine_inland <-
#   read_xlsx(here("data", nutrition_name),
#             sheet = "Fig1a",
#             na = "NA") %>%
#   rename(country_name = country) %>%
#   janitor::clean_names()

portions_marine_inland <-
  read_csv(here("data","3_anon_figures","anon_figures_nutrition", "anon_Figure1_nutrition_data_fig1a.csv")) %>%
  janitor::clean_names()


# which_is_which <-
#   read_xlsx(here("data", "pred obs catch by country table.xlsx"), skip = 1) |> 
#   janitor::clean_names() |> 
#   select(1:2) |> 
#   rename(country_name = country_1, case_study = x2) |> 
#   mutate(case_study = case_study == "CCS" ) 

which_is_which <-
  read_csv(here("data","3_anon_figures","anon_figures_catch" ,"anon_pred_obs_catch_by_country_table.csv")) |> 
  janitor::clean_names() |> 
  mutate(case_study = case_study == "CCS" ) 

portions_marine_inland <- portions_marine_inland |> 
  left_join(which_is_which, by = c("anon_country_name", "anon_iso3code", "region"))

cs <- portions_marine_inland |> 
  filter(case_study) |> 
  group_by(anon_country_name) |> 
  mutate(country_count = 1 / n_distinct(marine_inland_char)) |> 
  ungroup() |> 
  mutate(extrapolated = FALSE)



extrap <- portions_marine_inland |> 
  filter(!case_study) |> 
  group_by(anon_country_name) |> 
  mutate(country_count = 1 / n_distinct(marine_inland_char)) |> 
  group_by(marine_inland_char,region) |> 
  summarise(pop = sum(pop),
            country_count = sum(country_count)) |> 
  mutate(extrapolated = TRUE)
  
portions_marine_inland <- cs |> 
  bind_rows(extrap) |> 
  select(-case_study)

write_csv(portions_marine_inland, file = file.path(fig_dir, "portions_marine_inland.csv"))


# portions_lsf_ssf <-
#   read_xlsx(here("data", nutrition_name),
#             sheet = "Fig1b",
#             na = "NA") %>%
#   rename(country_name = country) %>%
#   janitor::clean_names() |> 
#   mutate(lsf_yield = pmax(0,ssf_lsf_yield - ssf_yield))

portions_lsf_ssf <-
  read_csv(here("data","3_anon_figures","anon_figures_nutrition", "anon_Figure1_nutrition_data_fig1b.csv")) %>%
  janitor::clean_names() |> 
  mutate(lsf_yield = pmax(0,ssf_lsf_yield - ssf_yield))

portions_region_lookup <- portions_lsf_ssf %>%
  select(anon_country_name, region) %>%
  unique()


zeroish_subsistence_europe <-
  FALSE # treat missing as zero just for subsistence-based metrics in Europe

if (zeroish_subsistence_europe) {
  subsistence_columns <- str_detect(colnames(data), "subsistence")
  
  data[data$region == "Europe" &
         !is.na(data$region), subsistence_columns] <-  0
  
}


# need to get the portions data wrangled into the same format
# portions_ssf_metrics <- portions_lsf_ssf %>%
#   pivot_wider(names_from = ssf_lsf, values_from = daily_portions)

data <- data %>%
  left_join(portions_lsf_ssf, by = c("anon_iso3code","anon_country_name" , "region"))



# generate metrics used in report
metrics <-  data %>%
  rowwise() %>%
  mutate(
    ssf_v_portions = ssf_yield / altsum(ssf_yield, lsf_yield, na.rm = remove_na),
    ssf_employment = altsum(
      preharvest_ssf,
      harvest_inland_ssf,
      harvest_marine_ssf,
      processing_ssf,
      trading_ssf,
      na.rm = remove_na
    ),
    ssf_employment_marine = altsum(
      preharvest_ssf ,
      processing_ssf ,
      trading_ssf ,
      harvest_marine_ssf,
      na.rm = remove_na
    ),
    ssf_employment_inland = altsum(
      preharvest_ssf ,
      processing_ssf ,
      trading_ssf ,
      harvest_inland_ssf,
      na.rm = remove_na
    ),
    lsf_employment = altsum(
      preharvest_lsf ,
      harvest_inland_lsf ,
      harvest_marine_lsf ,
      processing_lsf ,
      trading_lsf,
      na.rm = remove_na
    ),
    ssf_employment_w = altsum(
      preharvest_ssf_w,
      harvest_inland_ssf_w ,
      harvest_marine_ssf_w ,
      processing_ssf_w ,
      trading_ssf_w,
      na.rm = remove_na
    ),
    lsf_employment_w = altsum(
      preharvest_lsf_w ,
      harvest_inland_lsf_w ,
      harvest_marine_lsf_w ,
      processing_lsf_w ,
      trading_lsf_w,
      na.rm = remove_na
    ),
    ssf_v_total_employment =  ssf_employment / altsum(ssf_employment, lsf_employment, na.rm = remove_na),
    ssf_v_women_employment = ssf_employment_w / altsum(ssf_employment_w, lsf_employment_w, na.rm = remove_na),
    ssf_v_livelihood = altsum(dependent_subsistence_ssf_part, dependent_ssf_part, na.rm = remove_na) / altsum(
      dependent_subsistence_ssf_part ,
      dependent_ssf_part,
      dependent_lsf_part,
      na.rm = remove_na
    ),
    ssf_v_total_catch  =  pmin(1, catch_ssf / catch),
    ssf_subsistance = altsum(subsistence_inland , subsistence_marine, na.rm = remove_na),
    ssf_employment_w = altsum(
      preharvest_ssf_w ,
      harvest_inland_ssf_w ,
      harvest_marine_ssf_w ,
      processing_ssf_w ,
      trading_ssf_w,
      na.rm = remove_na
    ),
    ssf_subsistance_w = altsum(subsistence_inland_w , subsistence_marine_w, na.rm = remove_na),
    lsf_employment_w = altsum(
      preharvest_lsf_w ,
      harvest_inland_lsf_w ,
      harvest_marine_lsf_w ,
      processing_lsf_w ,
      trading_lsf_w,
      na.rm = remove_na
    ),
    ssf_livelihoods = altsum(dependent_ssf_part , dependent_subsistence_ssf_part, na.rm = remove_na),
    lsf_livelihoods = altsum(dependent_lsf_part, na.rm = remove_na),
    non_fish_employment = altsum(emp_agr_2016, emp_ind_2016, emp_ser_2016, na.rm = remove_na)
  ) %>%
  ungroup()

# get the portions data in the right shape. sigh.

# as long as remove_na = TRUE, this tells you whether each country had data in the numerator and denominator
has_top_and_bottom <- metrics %>%
  mutate(across(where(is.numeric), ~ !is.na(.x)))

has_top_and_bottom <- metrics %>%
  mutate(across(where(is.numeric), ~ TRUE))

metric_names <-
  names(metrics %>%  select(starts_with("name_") |
                              starts_with("ssf_v")))



# get totals
get_consistent_totals <-
  function(metric, data, has_top_and_bottom) {
    candidates <-
      has_top_and_bottom[[metric]] # this step ensures that you include countries that have the top and bottom for metrics
    total_metrics <-  data[candidates, ] %>%
      summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%  # has to be remove NA = TRUE here otherwise it doesn't work
      mutate(
        ssf_employment = altsum(
          preharvest_ssf,
          harvest_inland_ssf,
          harvest_marine_ssf,
          processing_ssf,
          trading_ssf,
          na.rm = remove_na
        ),
        ssf_employment_marine = altsum(
          preharvest_ssf ,
          processing_ssf ,
          trading_ssf ,
          harvest_marine_ssf,
          na.rm = remove_na
        ),
        ssf_employment_inland = altsum(
          preharvest_ssf ,
          processing_ssf ,
          trading_ssf ,
          harvest_inland_ssf,
          na.rm = remove_na
        ),
        lsf_employment = altsum(
          preharvest_lsf ,
          harvest_inland_lsf ,
          harvest_marine_lsf ,
          processing_lsf ,
          trading_lsf,
          na.rm = remove_na
        ),
        ssf_employment_w = altsum(
          preharvest_ssf_w,
          harvest_inland_ssf_w ,
          harvest_marine_ssf_w ,
          processing_ssf_w ,
          trading_ssf_w,
          na.rm = remove_na
        ),
        lsf_employment_w = altsum(
          preharvest_lsf_w ,
          harvest_inland_lsf_w ,
          harvest_marine_lsf_w ,
          processing_lsf_w ,
          trading_lsf_w,
          na.rm = remove_na
        ),
        ssf_v_portions = ssf_yield / altsum(ssf_yield, lsf_yield), # calculate LSF and set to 0 when negative
        ssf_v_total_employment =  ssf_employment / altsum(ssf_employment, lsf_employment),
        ssf_v_women_employment = ssf_employment_w / altsum(ssf_employment_w, lsf_employment_w),
        ssf_v_livelihood = altsum(dependent_subsistence_ssf_part ,  dependent_ssf_part) / sum(
          dependent_subsistence_ssf_part ,
          dependent_ssf_part,
          dependent_lsf_part,
          na.rm = remove_na
        ),
        ssf_v_total_catch  =  pmin(1, catch_ssf / catch),
        ssf_subsistance = altsum(subsistence_inland , subsistence_marine, na.rm = remove_na),
        ssf_employment_w = altsum(
          preharvest_ssf_w ,
          harvest_inland_ssf_w ,
          harvest_marine_ssf_w ,
          processing_ssf_w ,
          trading_ssf_w,
          na.rm = remove_na
        ),
        ssf_subsistance_w = altsum(subsistence_inland_w , subsistence_marine_w, na.rm = remove_na),
        ssf_livelihoods = altsum(dependent_ssf_part , dependent_subsistence_ssf_part, na.rm = remove_na),
        lsf_livelihoods = altsum(dependent_lsf_part, na.rm = remove_na),
        non_fish_employment = altsum(emp_agr_2016, emp_ind_2016 , emp_ser_2016, na.rm = remove_na)
      ) %>%
      ungroup() %>%
      select(all_of(metric))
  }


regional_total_metrics <-
  tibble(region = unique(data$region),
         data = vector("list", n_distinct(data$region))) |>
  filter(!is.na(region))

for (i in 1:nrow(regional_total_metrics)) {
  regional_total_metrics$data[[i]] <-   map_dfc(
    metric_names,
    get_consistent_totals,
    data = data |> filter(region == regional_total_metrics$region[i]),
    has_top_and_bottom = has_top_and_bottom |> filter(region == regional_total_metrics$region[i])
  )
}

regional_total_metrics <- regional_total_metrics |>
  unnest(cols = data)

total_metrics <-
  map_dfc(
    metric_names,
    get_consistent_totals,
    data = data |> mutate(region = "global"),
    has_top_and_bottom = has_top_and_bottom
  )  |>
  mutate(region = "global")


# note crop_volume_tons reported as tons, per code book catch_ssf is in millions of MT, but looking at the data crop_volume_tons has to be in MMT

long_metrics <- metrics %>%
  select(anon_country_name, region, starts_with("ssf_v")) %>%
  pivot_longer(contains("ssf_v"), names_to = "metric", values_to = "value") %>%
  filter(!is.na(value), !is.na(region)) %>%
  mutate(metric = str_remove_all(metric, "ssf_v_")) %>%
  mutate(metric = fct_relevel(metric, "total_catch", "total_employment", "women_employment"))

metric_means <- long_metrics %>%
  group_by(metric) %>%
  summarise(mean_value = mean(value, na.rm = TRUE))


# write processed data to results folder for figures ----------------------



write_csv(long_metrics
          , file = file.path(fig_dir, "long_metrics.csv"))

metric_means <- long_metrics %>%
  group_by(metric) %>%
  summarise(mean_value = mean(value, na.rm = TRUE))

# write_csv(
#   metrics |>  select(country_name, region, starts_with("ssf_v"), contains("ssf_"),-catch_ssf_inland,-catch_ssf_marine)
#   ,
#   file = file.path(fig_dir, "metrics.csv")
# )



write_csv(regional_total_metrics,
          file = file.path(fig_dir, "regional_total_metrics.csv"))


write_csv(total_metrics, file = file.path(fig_dir, "total_metrics.csv"))

catch_data <- data |>
  select(anon_country_name, region, catch, contains("catch_ssf"))


sigh <- data |> 
  select(anon_iso3code,region) |> 
  unique()

n_countries_per_region <- mike_country |> 
  left_join(sigh, by = c("anon_iso3code", "region")) |> 
  filter(!is.na(catch_ssf)) |> 
  group_by(region) |> 
  count()

un_region_ssf_catch <- mike_global %>%
  mutate(summed_pred_catch = summed_pred_catch / 1e6) |> 
  filter(region != "global") |>
  select(region, source, summed_pred_catch) |>
  pivot_wider(names_from = source,
              values_from = summed_pred_catch,
              names_prefix = "catch_ssf_") |>
  mutate(catch_ssf = catch_ssf_marine + catch_ssf_inland) |>
  group_by(region) %>%
  # mutate(across(starts_with("catch_ssf"), ~ ifelse(is.na(.x), 0, .x))) |> # this is needed to calculate p_marine
  summarise(
    ssf_catch = sum(catch_ssf, na.rm = TRUE),
    ssf_catch_marine =  sum(catch_ssf_marine, na.rm = TRUE),
    ssf_catch_inland =  sum(catch_ssf_inland, na.rm = TRUE)
  )  |>
  mutate(p_marine = ssf_catch_marine / ssf_catch) |> 
  left_join(n_countries_per_region, by = "region")


# un_region_ssf_catch <- catch_data %>%
#   group_by(region) %>%
#   mutate(has_data = !is.na(catch_ssf)) |>
#   mutate(across(starts_with("catch_ssf"), ~ ifelse(is.na(.x), 0, .x))) |> # this is needed to calculate p_marine
#   summarise(
#     ssf_catch = sum(catch_ssf, na.rm = TRUE),
#     ssf_catch_marine =  sum(catch_ssf_marine, na.rm = TRUE),
#     ssf_catch_inland =  sum(catch_ssf_inland, na.rm = TRUE),
#     p_marine = mean(
#       catch_ssf_marine / (catch_ssf_inland + catch_ssf_marine + 1e-9),
#       na.rm = TRUE
#     ),
#     n = n_distinct(country_name[has_data])
#   )  |>
#   filter(!is.na(region))


# write_csv(catch_data, file = file.path(fig_dir, "catch_data.csv"))

write_csv(un_region_ssf_catch, file = file.path(fig_dir, "un_region_ssf_catch.csv"))


employment_w_un_region_totals <- metrics %>%
  group_by(region) %>%
  mutate(has_data = !is.na(ssf_employment_w)) |>
  summarise(
    ssf_employment_w = sum(ssf_employment_w, na.rm = TRUE),
    ssf_harvest_marine_w =  sum(harvest_marine_ssf_w, na.rm = TRUE),
    ssf_harvest_inland_w =  sum(harvest_inland_ssf_w, na.rm = TRUE),
    n = n_distinct(anon_country_name[has_data])
  ) %>%
  filter(!is.na(region))

write_csv(employment_w_un_region_totals, file = file.path(fig_dir, "employment_w_un_region_totals.csv"))


emp_un_region_totals <- metrics %>%
  group_by(region) %>%
  mutate(has_data = !is.na(ssf_employment)) |>
  summarise(
    ssf_emp = sum(ssf_employment, na.rm  = TRUE),
    ssf_emp_inland =  sum(ssf_employment_inland, na.rm = TRUE),
    ssf_emp_marine =  sum(ssf_employment_marine, na.rm = TRUE),
    n = n_distinct(anon_country_name[has_data])
  ) %>%
  filter(!is.na(region))

write_csv(emp_un_region_totals, file = file.path(fig_dir, "emp_un_region_totals.csv"))

live_un_region_totals <- metrics %>%
  group_by(region) %>%
  mutate(has_data = !is.na(ssf_livelihoods)) |>
  summarise(
    ssf_live = sum(ssf_livelihoods, na.rm = TRUE),
    ssf_live_inland =  sum(ssf_livelihoods, na.rm = TRUE),
    ssf_live_marine =  sum(ssf_livelihoods, na.rm = TRUE),
    n = n_distinct(anon_country_name[has_data])
  ) %>%
  filter(!is.na(region)) |> 
  mutate(message = "ignore _inland and _marine values, they are a coding artifact")

write_csv(live_un_region_totals, file = file.path(fig_dir, "live_un_region_totals.csv"))

countries_per_region <- data %>%
  group_by(region) %>%
  count(name = "pool")

write_csv(countries_per_region, file = file.path(fig_dir, "countries_per_region.csv"))

catches <- data |>
  select(region, anon_country_name, catch)

# write_csv(catches, file = file.path(fig_dir, "catches.csv"))


# regional_catches <- data |>
#   group_by(region) |>
#   summarise(total_catch = sum(catch, na.rm = TRUE))
# 
# write_csv(regional_catches, file = file.path(fig_dir, "regional_catches.csv"))



