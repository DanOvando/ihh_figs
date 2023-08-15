source("00_setup.R") #setup libraries etc.


# load data ---------------------------------------------------------------
message("built using data https://doi.org/10.5281/zenodo.8243546")

countries <-
  rnaturalearth::ne_download(
    category = "cultural",
    type = "map_units",
    scale = 50,
    returnclass = "sf",
    destdir = "data"
  ) %>%  # download cultural map units
  sf::st_make_valid()

countries <- countries %>%
  filter(st_is_valid(countries)) # remove invalid countries for now

land = rnaturalearth::ne_download(
  category = "physical",
  type = "land",
  scale = 50,
  returnclass = "sf"
) # get borderless land

ssu <- purrr::safely(sf::st_union)

# safely create union countries within each UN region to get just the UN region; would be better to just get the actual shapefile
un_regions <- countries %>%
  group_by(REGION_UN) %>%
  nest() %>%
  mutate(tmp = map(data, ssu)) %>%
  mutate(worked = map_lgl(map(tmp, "error"), is.null)) %>%
  filter(worked) %>%
  mutate(geometry = map(tmp, "result")) %>%
  select(-tmp) %>%
  unnest(cols = geometry) %>%
  ungroup() %>%
  sf::st_as_sf() %>%
  filter(sf::st_is_valid(.)) %>%
  janitor::clean_names()

#| label: load-country-data

data <- readr::read_csv(here("data", "20230731IHH_global.csv")) %>%
  janitor::clean_names() # read in data

berhman <-
  st_crs(
    "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"
  ) # flat projected global distribution

region_lookup <- data %>%
  select(country_name, region) %>%
  unique()

nutrition_name <- "20230811 Nutrition Dan corrected_AT+XB.xlsx"

portions_marine_inland <-
  read_xlsx(here("data", nutrition_name),
            sheet = "Sheet1",
            na = "NA") %>%
  rename(country_name = country) %>%
  janitor::clean_names()

portions_lsf_ssf <-
  read_xlsx(here("data", nutrition_name),
            sheet = "Sheet2",
            na = "NA") %>%
  rename(country_name = country) %>%
  janitor::clean_names()

portions_region_lookup <- portions_lsf_ssf %>%
  select(country_name, region) %>%
  unique()


zeroish_subsistence_europe <-
  TRUE # treat missing as zero just for subsistence-based metrics in Europe

if (zeroish_subsistence_europe) {
  subsistence_columns <- str_detect(colnames(data), "subsistence")
  
  data[data$region == "Europe" &
         !is.na(data$region), subsistence_columns] <-  0
  
}


#| label: calc-metrics

# need to get the portions data wrangled into the same format
portions_ssf_metrics <- portions_lsf_ssf %>%
  pivot_wider(names_from = ssf_lsf, values_from = daily_portions)

data <- data %>%
  left_join(portions_ssf_metrics, by = c("country_name", "region"))


# an alternative to `sum` where returns NA if all elements of sum are NA
altsum <- function(..., na.rm = TRUE) {
  components <- list(...)
  
  components <- map_dbl(components,  ~ .x)
  
  if (all(is.na(components))) {
    out <- NA
  } else {
    out <- sum(components, na.rm = na.rm)
  }
  
  return(out)
  
}

# generate metrics used in report
metrics <-  data %>%
  rowwise() %>%
  mutate(
    ssf_v_portions = SSF / altsum(LSF, SSF, na.rm = remove_na),
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
        ssf_v_portions = SSF / altsum(LSF, SSF, na.rm = remove_na),
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
  select(country_name, region, starts_with("ssf_v")) %>%
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

write_csv(
  metrics |>  select(
    country_name,
    region,
    catch,
    starts_with("ssf_v"),
    ssf_employment_w,
    harvest_marine_ssf_w,
    harvest_inland_ssf_w,
    ssf_employment,
    ssf_livelihoods
  )
  ,
  file = file.path(fig_dir, "metrics.csv")
)

write_csv(regional_total_metrics,
          file = file.path(fig_dir, "regional_total_metrics.csv"))


write_csv(total_metrics, file = file.path(fig_dir, "total_metrics.csv"))

catch_data <- data |>
  ungroup() |> 
  mutate(country_name = forcats::fct_anon(country_name)) |> 
  select(country_name, region, catch, contains("catch_ssf"))

regional_catch_data <- catch_data %>%
  group_by(region) %>%
  mutate(has_data = !is.na(catch_ssf)) |>
  mutate(across(starts_with("catch_ssf"), ~ ifelse(is.na(.x), 0, .x))) |> # this is needed to calculate p_marine
  summarise(
    ssf_catch = sum(catch_ssf, na.rm = TRUE),
    ssf_catch_marine =  sum(catch_ssf_marine, na.rm = TRUE),
    ssf_catch_inland =  sum(catch_ssf_inland, na.rm = TRUE),
    p_marine = mean(
      catch_ssf_marine / (catch_ssf_inland + catch_ssf_marine + 1e-9),
      na.rm = TRUE
    ),
    n = n_distinct(country_name[has_data])
  )  |>
  filter(!is.na(region))

write_csv(regional_catch_data, file = file.path(fig_dir, "regional_catch_data.csv"))


un_region_portions <- portions_marine_inland %>%
  mutate(
    marine = tolower(marine_inland) == "marine",
    daily_portions_domestic = daily_portions_domestic / 1e6
  ) %>%
  group_by(region) %>%
  mutate(has_data = !is.na(daily_portions_domestic)) |>
  summarise(
    regional_portions = sum(daily_portions_domestic, na.rm = TRUE),
    regional_portions_marine =  sum(daily_portions_domestic[marine], na.rm = TRUE),
    regional_portions_inland =  sum(daily_portions_domestic[!marine], na.rm = TRUE),
    n = n_distinct(country_name[has_data])
  )

write_csv(un_region_portions,
          file.path(fig_dir, "regional_portions_data.csv"))
