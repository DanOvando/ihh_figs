source("00_setup.R")

# load data ---------------------------------------------------------------

files <- list.files(fig_dir)

result_files <-
  str_remove_all(files[str_detect(files, ".csv$")], ".csv") # read in CSVs


walk(result_files, ~ assign(.x, read_csv(file.path(
  fig_dir, paste0(.x, ".csv")
)), envir = .GlobalEnv))


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


# functions for maps ------------------------------------------------------


# function to make pie charts (or bar charts later as desired) of proportion of catch from inland vs. marine
make_pies <-
  function(un_region,
           data,
           pie_filling,
           slice_colors =  c("salmon3", "skyblue")) {
    pie <- data %>%
      filter(region_un == un_region) %>%
      pivot_longer(starts_with(pie_filling),
                   names_to = "subgroup",
                   values_to = "value") %>%
      ggplot() +
      geom_col(aes(x = "", y = value, fill = subgroup),
               show.legend = FALSE) +
      coord_polar(theta = "y") +
      scale_fill_manual(values = slice_colors) +
      theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent", color = "transparent")
      ) +
      ggpubr::theme_transparent()
    
    # ggsave(filename = here("pies",paste0(un_region,"_pie.png")), pie)
    
  }





#| label: map_foo

map_foo <-
  function(fill_var,
           title,
           subtitle,
           base_map,
           carto_map,
           centroids,
           low_fill = "salmon3",
           high_fill = "skyblue",
           pies = FALSE,
           show_legend = TRUE,
           carto_fill = "gray90",
           units = "Somethings",
           legend_position = c(0.65, 1.1)) {
    centroids <- centroids %>%
      mutate(X = ifelse(region_un == "Europe", X / 3, X),
             metric = subtitle)
    
    carto_map$metric <- subtitle
    
    base_map$metric <- subtitle
    
    n = sum(centroids$n)
    
    total <- centroids %>%
      summarise(total = sum(({
        {
          fill_var
        }
      })))
    
    metric_map <- carto_map %>%
      ggplot() +
      geom_sf(data = base_map,
              alpha = 0.5,
              color = "transparent")
    
    total_label <-
      as.character(plyr::round_any(sum(total$total), 1))
    
    # if_else(plyr::round_any(sum(total$total), 1) == 0,'<0.1',as.character(plyr::round_any(sum(total$total), 1)))
    
    if (pies) {
      metric_map <- metric_map +
        geom_sf(
          fill = carto_fill,
          size = .5,
          alpha = 1,
          show.legend = FALSE,
          color = "transparent"
        ) +
        ggimage::geom_subview(data = centroids, aes(
          X,
          Y,
          subview = pies,
          width = wh,
          height = wh
        ))  +
        geom_text(
          data = centroids,
          aes(
            X + 2,
            Y + 2,
            label = ifelse(
              plyr::round_any({
                {
                  fill_var
                }
              }, 0.1) == 0,
              "<0.1",
              plyr::round_any({
                {
                  fill_var
                }
              }, 0.1)
            )
          ),
          color = "black",
          size = 3
        ) +
        geom_text(
          data = data.frame(
            x = 50,
            y = -55,
            label = glue::glue("N={n}"),
            metric = subtitle
          ),
          aes(x = x, y = y, label = label),
          size = 3
        ) +
        geom_richtext(
          data = data.frame(
            x = -225,
            y = 5,
            label = glue(
              "Total<br>~<span style='font-size:14pt; color:black'>{total_label}</span><br>{units}"
            ),
            metric = subtitle
          ),
          aes(x, y, label = label),
          fill = NA,
          label.color = NA,
          size = 3,
          hjust = 0
        ) +
        scale_fill_gradient(low = low_fill,
                            high = high_fill,
                            guide = "none")
      
      
    } else {
      metric_map <-  metric_map +
        geom_sf(
          fill = carto_fill,
          size = .5,
          alpha = 1,
          show.legend = FALSE
        ) +
        geom_label(
          data = centroids,
          aes(X, Y, label = ceiling({
            {
              fill_var
            }
          }), fill = p_marine),
          color = "black",
          size = 4,
          show.legend = show_legend
        ) +
        scale_fill_binned(
          low = low_fill,
          high = high_fill,
          limits = c(0, 1),
          labels = scales::label_percent(accuracy = 1),
          name = "% Marine",
          guide = guide_colorbar(
            frame.colour = "black",
            ticks.colour = "black",
            barwidth = unit(75, "points"),
            title.position = "right"
            
          )
        ) +
        scale_color_viridis_d() +
        guides(color = "none")
      
      
    }
    
    metric_map <- metric_map  +
      scale_size(trans = "sqrt") +
      coord_sf(clip = "off") +
      ggpubr::theme_transparent() +
      theme(
        legend.position = legend_position,
        legend.direction = "horizontal",
        plot.margin = ggplot2::unit(c(5, 5, 5, 5), units = "points"),
        legend.title.align = 1,
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6),
        plot.subtitle = element_text(size = 12)
      ) +
      facet_wrap(~ metric, strip.position = "left") +
      theme(
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_text(size = 11, color = "black")
      )
    
    
    return(metric_map)
    
  }



# make maps ---------------------------------------------------------------



#| label: catch-map


# calculate ssf catch statistics
un_region_ssf_catch <- catch_data %>%
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
#join together
un_region_ssf_catch <- un_regions %>%
  left_join(un_region_ssf_catch, by = c("region_un" = "region")) %>%
  filter(!is.na(ssf_catch))

berhman <-
  st_crs(
    "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"
  )

base_map <-  un_region_ssf_catch %>%
  sf::st_transform(crs = berhman)

if (cartoify) {
  un_region_ssf_catch_carto <-
    cartogram_ncont(base_map, "ssf_catch") %>%
    sf::st_transform(crs = st_crs(un_regions))
  
} else {
  un_region_ssf_catch_carto <- base_map %>%
    sf::st_transform(crs = st_crs(un_regions))
  
}


# find centroids of regions to locate pie chart insets
un_region_centroids <-
  sf::st_centroid(un_region_ssf_catch_carto) %>%
  bind_cols(st_coordinates(.)) %>%
  mutate(factor_region = as.factor(as.numeric(as.factor(region_un)))) %>%
  sf::st_drop_geometry() %>%
  select(-data)


catch_centroids <- un_region_centroids %>%
  mutate(
    pies = map(
      region_un,
      make_pies,
      data = un_region_centroids %>% select(-ssf_catch),
      pie_filling = "ssf_catch"
    )
  ) %>%
  mutate(wh = pie_size) # circle size scalar proportional to total amount of SSF catch

simple_catch_centroids <- catch_centroids |>
  select(region_un, X, Y, ssf_catch, p_marine, n)

catch_map_plot <-
  map_foo(
    fill_var = ssf_catch,
    subtitle = "Catch",
    centroids = catch_centroids,
    base_map = base_map,
    carto_map = un_region_ssf_catch_carto,
    pies = plot_pies,
    show_legend = FALSE,
    units = "million tonnes"
  )


catch_map_plot




#| label: women-data

un_region_totals <- metrics %>%
  group_by(region) %>%
  mutate(has_data = !is.na(ssf_employment_w)) |>
  summarise(
    ssf_employment_w = sum(ssf_employment_w, na.rm = TRUE),
    ssf_harvest_marine_w =  sum(harvest_marine_ssf_w, na.rm = TRUE),
    ssf_harvest_inland_w =  sum(harvest_inland_ssf_w, na.rm = TRUE),
    n = n_distinct(country_name[has_data])
  ) %>%
  filter(!is.na(region))


#join together
ssf_w_employment <- un_regions %>%
  left_join(un_region_totals, by = c("region_un" = "region")) %>%
  filter(!is.na(ssf_employment_w))


tmp <-  ssf_w_employment %>%
  sf::st_transform(crs = berhman)


if (cartoify) {
  carto_map_w <- cartogram_ncont(tmp, "ssf_employment_w") %>%
    sf::st_transform(crs = st_crs(un_regions))
} else {
  carto_map_w <- tmp %>%
    sf::st_transform(crs = st_crs(un_regions))
}


# find centroids of regions to locate pie chart insets
centroids_w <- sf::st_centroid(carto_map_w) %>%
  bind_cols(st_coordinates(.)) %>%
  mutate(factor_region = as.factor(as.numeric(as.factor(region_un)))) %>%
  sf::st_drop_geometry() %>%
  select(-data)

w_pies <- centroids_w %>%
  mutate(
    pies = map(
      region_un,
      make_pies,
      data = centroids_w,
      pie_filling = "ssf_employment_w",
      slice_colors = c(unknown_color, unknown_color)
    )
  ) %>%
  mutate(wh = pie_size,
         p_marine = NA) # circle size scalar proportional to total amount of SSF catch


simple_women_employed_centroids <- w_pies |>
  select(region_un, X, Y, ssf_employment_w, p_marine, n)


w_map_plot <-
  map_foo(
    fill_var = ssf_employment_w,
    subtitle = "Women employed",
    centroids = w_pies,
    base_map = base_map,
    carto_map = carto_map_w,
    pies = plot_pies,
    show_legend = FALSE,
    units = "million people",
    low_fill = unknown_color,
    high_fill = unknown_color
  )


w_map_plot


#| label: fig-emp-map
#| eval: true


un_region_totals <- metrics %>%
  group_by(region) %>%
  mutate(has_data = !is.na(ssf_employment)) |>
  summarise(
    ssf_emp = sum(ssf_employment, na.rm  = TRUE),
    ssf_emp_inland =  sum(ssf_employment_inland, na.rm = TRUE),
    ssf_emp_marine =  sum(ssf_employment_marine, na.rm = TRUE),
    n = n_distinct(country_name[has_data])
  ) %>%
  filter(!is.na(region))

#join together
ssf_emp <- un_regions %>%
  left_join(un_region_totals, by = c("region_un" = "region")) %>%
  filter(!is.na(ssf_emp))


tmp <-  ssf_emp %>%
  sf::st_transform(crs = berhman)

if (cartoify) {
  carto_map <- cartogram_ncont(tmp, "ssf_emp") %>%
    sf::st_transform(crs = st_crs(un_regions))
} else {
  carto_map <- tmp %>%
    sf::st_transform(crs = st_crs(un_regions))
}

# find centroids of regions to locate pie chart insets
centroids <- sf::st_centroid(carto_map) %>%
  bind_cols(st_coordinates(.)) %>%
  mutate(factor_region = as.factor(as.numeric(as.factor(region_un)))) %>%
  sf::st_drop_geometry() %>%
  select(-data)


pies <- centroids %>%
  mutate(
    pies = map(
      region_un,
      make_pies,
      data = centroids %>% select(-ssf_emp),
      pie_filling = "ssf_emp",
      slice_colors = c(unknown_color, unknown_color)
    )
  ) %>%
  mutate(wh = pie_size,
         p_marine = NA) # circle size scalar proportional to total amount of SSF catch

simple_employed_centroids <- pies |>
  select(region_un, X, Y, ssf_emp, p_marine, n)


emp_map_plot <-
  map_foo(
    fill_var = ssf_emp,
    subtitle = "People employed",
    centroids = pies,
    base_map = base_map,
    carto_map = carto_map,
    pies = plot_pies,
    show_legend = FALSE,
    units = "million people",
    low_fill = unknown_color,
    high_fill = unknown_color
  )


emp_map_plot



#| label: fig-live-map
#| eval: true


un_region_totals <- metrics %>%
  group_by(region) %>%
  mutate(has_data = !is.na(ssf_livelihoods)) |>
  summarise(
    ssf_live = sum(ssf_livelihoods, na.rm = TRUE),
    ssf_live_inland =  sum(ssf_livelihoods, na.rm = TRUE),
    ssf_live_marine =  sum(ssf_livelihoods, na.rm = TRUE),
    n = n_distinct(country_name[has_data])
  ) %>%
  filter(!is.na(region))

#join together
ssf_live <- un_regions %>%
  left_join(un_region_totals, by = c("region_un" = "region")) %>%
  filter(!is.na(ssf_live))


tmp <-  ssf_live %>%
  sf::st_transform(crs = berhman)

if (cartoify) {
  carto_map <- cartogram_ncont(tmp, "ssf_live") %>%
    sf::st_transform(crs = st_crs(un_regions))
} else {
  carto_map <- tmp %>%
    sf::st_transform(crs = st_crs(un_regions))
  
}


# find centroids of regions to locate pie chart insets
centroids <- sf::st_centroid(carto_map) %>%
  bind_cols(st_coordinates(.)) %>%
  mutate(factor_region = as.factor(as.numeric(as.factor(region_un)))) %>%
  sf::st_drop_geometry() %>%
  select(-data)


pies <- centroids %>%
  mutate(
    pies = map(
      region_un,
      make_pies,
      data = centroids %>% select(-ssf_live),
      pie_filling = "ssf_live",
      slice_colors = c(unknown_color, unknown_color)
    )
  ) %>%
  mutate(wh = pie_size,
         p_marine = NA) # circle size scalar proportional to total amount of SSF catch

simple_livelihoods_centroids <- pies |>
  select(region_un, X, Y, ssf_live, p_marine, n)


live_map_plot <-
  map_foo(
    fill_var = ssf_live,
    subtitle = "Livelihoods",
    centroids = pies,
    base_map = base_map,
    carto_map = carto_map,
    low_fill = unknown_color,
    high_fill = unknown_color,
    pies = plot_pies,
    show_legend = FALSE,
    units = "million people"
  )


# make nutrition maps ---------------------------------------------------------------

# calculate total portions per region split out by inland vs marine

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

#join together
un_region_portions <- un_regions %>%
  left_join(un_region_portions, by = c("region_un" = "region")) %>%
  filter(!is.na(regional_portions))

berhman <-
  st_crs(
    "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"
  )

base_map <-  un_region_portions %>%
  sf::st_transform(crs = berhman)

if (cartoify) {
  un_region_portions_carto <-
    cartogram_ncont(base_map, "regional_portions") %>%
    sf::st_transform(crs = st_crs(un_regions))
  
} else {
  un_region_portions_carto <- base_map %>%
    sf::st_transform(crs = st_crs(un_regions))
  
}

# find centroids of regions to locate pie chart insets

un_region_centroids <-
  sf::st_centroid(un_region_portions_carto) %>%
  bind_cols(st_coordinates(.)) %>%
  mutate(factor_region = as.factor(as.numeric(as.factor(region_un)))) %>%
  sf::st_drop_geometry() %>%
  select(-data)


portion_centroids <- un_region_centroids %>%
  mutate(
    pies = map(
      region_un,
      make_pies,
      data = un_region_centroids %>% select(-regional_portions),
      pie_filling = "regional_portions"
    )
  ) %>%
  mutate(wh = pie_size,
         p_marine = regional_portions_marine / regional_portions) # circle size scalar proportional to total amount of SSF catch


simple_portion_centroids <- portion_centroids |>
  select(region_un, X, Y, regional_portions, p_marine, n) |>
  rename(ssf_portions = regional_portions)

nuts_map_plot <-
  map_foo(
    fill_var = regional_portions,
    subtitle = "Nutrient supply",
    centroids = portion_centroids,
    base_map = base_map,
    carto_map = un_region_portions_carto,
    pies = plot_pies,
    show_legend = TRUE,
    units = "million people",
    legend_position = c(0.15,-0.05)
  ) +
  theme(plot.title = element_text(hjust = 0, vjust = 1))


# make SSF contribution plots ---------------------------------------------



ssf_labeler <- c(
  catch = "Catch",
  employment = "People employed in fisheries",
  livelihood = "People with livelihoods in fisheries",
  women_employment = "Women employed in fisheries",
  portions = "Portions"
)

total_metric_lines <- total_metrics %>%
  select(starts_with("ssf_v")) %>%
  pivot_longer(starts_with("ssf_v"),
               names_to = "metric",
               values_to = "total") %>%
  mutate(metric = str_remove_all(metric, "(ssf_v_)|total_")) %>%
  mutate(metric = fct_relevel(metric, "catch", "employment", "women_employment"))
metric_means <- long_metrics %>%
  group_by(metric) %>%
  summarise(mv = mean(value, na.rm = TRUE)) %>%
  ungroup()

regional_total_metrics <- regional_total_metrics %>%
  select(region, starts_with("ssf_v")) %>%
  pivot_longer(starts_with("ssf_v"),
               names_to = "metric",
               values_to = "value") %>%
  mutate(metric = str_remove_all(metric, "(ssf_v_)"))

metric_means <- long_metrics %>%
  group_by(metric, region) %>%
  summarise(mv = mean(value, na.rm = TRUE)) %>%
  ungroup()

region_metric_means <- long_metrics %>%
  group_by(region, metric) %>%
  summarise(mv = mean(value, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(metric)

countries_per_region <- data %>%
  group_by(region) %>%
  count(name = "pool")


ssf_contribution_countries <- long_metrics |>
  select(country_name, region) |>
  unique()


ssf_contributions_catch_comp <- data |>
  mutate(in_ssf_contrib = country_name %in% ssf_contribution_countries$country_name) |>
  group_by(region) |>
  summarise(
    ssf_contrib_catch = sum(catch[in_ssf_contrib], na.rm = TRUE),
    total_catch = sum(catch, na.rm = TRUE)
  )


catches <- data |>
  select(region, country_name, catch)

regional_catches <- data |>
  group_by(region) |>
  summarise(total_catch = sum(catch, na.rm = TRUE))

counts <- long_metrics %>%
  left_join(catches, by = c("country_name", "region")) |>
  group_by(region, metric) %>%
  summarise(n = n_distinct(country_name),
            sampled_catch = sum(catch, na.rm = TRUE)) |>
  ungroup() |>
  left_join(countries_per_region, by = "region") |>
  left_join(regional_catches, by = "region") |>
  mutate(prop_catch_represented = sampled_catch / total_catch) |>
  mutate(p = scales::percent(n / pool)) %>%
  mutate(label = p) |>
  mutate(label2 = paste0("N=", n))

counts |>
  ungroup() |>
  mutate(metric = str_remove_all(metric, "total_")) |>
  select(region, metric, n, p, prop_catch_represented) |>
  arrange(metric, region) |>
  rename(
    "number_countries_with_data" = n,
    "percent_countries_with_data" = p,
    ssf_metric = metric
  ) |>
  write_csv(file.path(fig_dir, "ssf_contribution_sample_sizes.csv"))

ssf_hists_data <-  long_metrics %>%
  left_join(counts, by = c("region", "metric")) |>
  mutate(metric = str_remove_all(metric, "total_")) |>
  mutate(metric = fct_relevel(metric, "catch", "portions", "employment", "livelihood")) |>
  mutate(basic_region = region,
         region = glue::glue("{region} {label}")) |>
  mutate(region = (region),
         basic_region = fct_rev(basic_region)) |>
  mutate(value = pmin(1, value))

ssf_regional_total_data <-  regional_total_metrics %>%
  left_join(counts, by = c("region", "metric")) |>
  mutate(metric = str_remove_all(metric, "total_")) |>
  mutate(metric = fct_relevel(metric, "catch", "portions", "employment", "livelihood")) |>
  mutate(basic_region = region,
         region = glue::glue("{region} {label}")) |>
  mutate(region = (region),
         basic_region = fct_rev(basic_region)) |>
  mutate(value = pmin(1, value))

ssf_hists_labels <- ssf_regional_total_data |>
  group_by(region, basic_region, metric) |>
  summarise(mv = mean(value)) |>
  arrange(metric, (mv)) |>
  ungroup() |>
  mutate(plot_order = factor(1:length(mv))) |>
  mutate(ordered_region = fct_inorder(region))

ssf_hists_data <- ssf_hists_data |>
  left_join(ssf_hists_labels, by = c("metric", "region", "basic_region"))

ssf_regional_total_data <- ssf_regional_total_data |>
  left_join(ssf_hists_labels, by = c("metric", "region", "basic_region"))

ssf_points_plot <- ssf_regional_total_data %>%
  ggplot(aes(plot_order, value)) +
  geom_hline(
    data = total_metric_lines,
    aes(yintercept = total),
    linetype = 1,
    color = "tomato",
    linewidth = 3
  ) +
  geom_point(color = "grey55",
             size = 4) +
  labs(title = bquote( ~ bold("b") ~ "Relative contributions of SSF")) +
  scale_y_continuous(
    name = "SSF share of total fisheries sector",
    labels = scales::label_percent(accuracy = 1),
    breaks = seq(0, 1, by = .2)
  ) +
  scale_x_discrete(breaks = ssf_hists_labels$plot_order,
                   labels = as.character(ssf_hists_labels$region)) +
  scale_fill_manual(values = viridisLite::cividis(n = 5)[1:5]) +
  facet_wrap(
    ~ metric,
    ncol = 1,
    strip.position = "top",
    scales = "free_y",
    labeller = labeller(metric = ssf_labeler)
  ) +
  coord_flip() +
  theme(
    axis.title.y  = element_blank(),
    plot.margin = ggplot2::unit(c(5, 5, 0, 0), "pt"),
    strip.background = element_blank(),
    axis.text.y = element_text(size = 8),
    axis.line = element_line(color = "black", linetype = 1),
    strip.text = element_blank()
  )


ssf_bar_plot <- ssf_regional_total_data %>%
  mutate(not_ssf = 1 - value)  |>
  rename(ssf = value) |>
  pivot_longer(c(not_ssf, ssf), names_to = "thing", values_to = "value") |>
  ggplot() +
  geom_col(aes(plot_order, value, fill = thing)) +
  geom_hline(
    data = total_metric_lines,
    aes(yintercept = total),
    linetype = 1,
    color = "tomato",
    linewidth = 3
  ) +
  labs(title = bquote( ~ bold("b") ~ "Relative contributions of SSF")) +
  scale_y_continuous(
    name = "SSF share of total fisheries sector",
    labels = scales::label_percent(accuracy = 1),
    breaks = seq(0, 1, by = .2),
    expand = ggplot2::expansion(mult = c(0, .025))
  ) +
  scale_x_discrete(breaks = ssf_hists_labels$plot_order,
                   labels = as.character(ssf_hists_labels$region)) +
  facet_wrap(
    ~ metric,
    ncol = 1,
    strip.position = "top",
    scales = "free_y",
    labeller = labeller(metric = ssf_labeler)
  ) +
  coord_flip() +
  theme(
    axis.title.y  = element_blank(),
    plot.margin = ggplot2::unit(c(5, 5, 0, 0), "pt"),
    strip.background = element_blank(),
    axis.text.y = element_text(size = 8),
    axis.line = element_line(color = "black", linetype = 1),
    strip.text = element_blank()
  ) +
  scale_fill_manual(name = "", values = c("darkgreen", "steelblue"))

hist_legend <-
  data.frame(label = c("National", "Regional", "Global"),
             thing = 1) |>
  mutate(label = fct_relevel(label, "Global", "Regional"))

country_color <- "darkcyan"

region_color <- "gray30"


hist_legend <- hist_legend |>
  ggplot(aes(label, thing, fill = label)) +
  geom_col() +
  scale_fill_manual(name = element_blank(),
                    values = c("tomato", region_color, country_color)) +
  theme(
    legend.key = element_rect(color = "transparent"),
    legend.text = element_text(size = 9),
    legend.key.size = unit(c(0.75), "lines"),
    legend.background = element_rect(fill = "transparent")
  )


hist_legend <- as_ggplot(ggpubr::get_legend(hist_legend))


ssf_hists_plot <- ssf_hists_data %>%
  ggplot(aes(plot_order, value)) +
  stat_histinterval(
    scale = 3,
    show.legend = FALSE,
    point_interval = mean_qi,
    point_color = "transparent",
    point_fill = "transparent",
    point_size = 0,
    size = 1,
    color = "transparent",
    shape = 21,
    slab_fill = country_color,
    slab_color = "transparent",
    slab_alpha = 0.5,
    slab_size = 0,
    .width = c(0.5)
  ) +
  geom_hline(
    data = total_metric_lines,
    aes(yintercept = total),
    linetype = 1,
    color = "tomato",
    linewidth = 3
  ) +
  geom_point(data = ssf_regional_total_data,
             color = region_color,
             size = 4) +
  labs(title = bquote( ~ bold("b") ~ "Relative contributions of SSF")) +
  scale_y_continuous(
    name = "SSF share of total fisheries sector",
    labels = scales::label_percent(accuracy = 1),
    breaks = seq(0, 1, by = .2)
  ) +
  scale_x_discrete(breaks = ssf_hists_labels$plot_order,
                   labels = as.character(ssf_hists_labels$basic_region)) +
  scale_fill_manual(values = viridisLite::cividis(n = 5)[1:5]) +
  facet_wrap(
    ~ metric,
    ncol = 1,
    strip.position = "top",
    scales = "free_y",
    labeller = labeller(metric = ssf_labeler)
  ) +
  coord_flip() +
  theme(
    axis.title.y  = element_blank(),
    plot.margin = ggplot2::unit(c(5, 5, 0, 0), "pt"),
    strip.background = element_blank(),
    axis.text.y = element_text(size = 8),
    axis.line = element_line(color = "black", linetype = 1),
    strip.text = element_blank()
  ) +
  inset_element(
    hist_legend,
    left = 0,
    bottom = 1,
    right = 1,
    top = 1.05,
    on_top = TRUE
  )

a = data |>
  filter(!is.na(region)) |>
  group_by(region) |>
  summarise(catch = sum(catch, na.rm = TRUE)) |>
  arrange(catch)


ssf_hists_fixed_order_plot <- ssf_hists_data %>%
  ggplot(aes(fct_rev(
    fct_relevel(basic_region, "Africa", "Asia", "Americas", "Oceania")
  ), value)) +
  stat_histinterval(
    scale = 4,
    show.legend = FALSE,
    point_interval = mean_qi,
    point_color = "transparent",
    point_fill = "transparent",
    point_size = 0,
    size = 1,
    color = "transparent",
    shape = 21,
    slab_fill = c("cadetblue2"),
    slab_color = "transparent",
    slab_alpha = 1,
    slab_size = 0.25,
    .width = c(0.5)
  ) +
  geom_hline(
    data = total_metric_lines,
    aes(yintercept = total),
    linetype = 1,
    color = "tomato",
    linewidth = 3
  ) +
  geom_point(data = ssf_regional_total_data,
             color = "gray60",
             size = 4) +
  labs(title = bquote( ~ bold("b") ~ "Relative contributions of SSF")) +
  scale_y_continuous(
    name = "SSF share of total fisheries sector",
    labels = scales::label_percent(accuracy = 1),
    breaks = seq(0, 1, by = .2)
  ) +
  facet_wrap(
    ~ metric,
    ncol = 1,
    strip.position = "top",
    scales = "free_y",
    labeller = labeller(metric = ssf_labeler)
  ) +
  coord_flip() +
  theme(
    axis.title.y  = element_blank(),
    plot.margin = ggplot2::unit(c(5, 5, 0, 0), "pt"),
    strip.background = element_blank(),
    axis.text.y = element_text(size = 8),
    axis.line = element_line(color = "black", linetype = 1),
    strip.text = element_blank()
  ) +
  inset_element(
    hist_legend,
    left = 0,
    bottom = 1,
    right = 1,
    top = 1.05,
    on_top = TRUE
  )





# make main figure --------------------------------------------------------


ssf_regional_totals <-
  list_rbind(map(ls()[str_detect(ls(), "simple_")], ~ get(.x)))


if (plot_pies) {
  pie_slices <-
    data.frame(label = c("Marine", "Inland", "Undifferentiated"),
               thing = 1) |>
    mutate(label = fct_relevel(label, "Marine", "Inland"))
  
  pie_legend <- pie_slices |>
    ggplot(aes(label, thing, fill = label)) +
    geom_col() +
    scale_fill_manual(
      name = element_blank(),
      values = c("skyblue", "salmon3", "tan"),
      guide = guide_legend()
    ) +
    theme(
      legend.key = element_rect(color = "transparent"),
      legend.text = element_text(size = 9),
      legend.key.size = unit(c(0.75), "lines"),
      legend.background = element_rect(fill = "transparent")
    )
  
  
  pie_legend <- as_ggplot(ggpubr::get_legend(pie_legend))
  
} else {
  pie_slices <- data.frame(label = c("Unknown"), thing = 1)
  
  pie_legend <- pie_slices |>
    ggplot(aes(label, thing, fill = label), color = "black") +
    geom_col() +
    scale_fill_manual(name = element_blank(), values = c("cornsilk")) +
    theme(
      legend.key = element_rect(color = "black"),
      legend.text = element_text(size = 8),
      legend.key.size = unit(c(1), "lines"),
      legend.background = element_rect(fill = "transparent")
    )
  
  
  
  pie_legend <- as_ggplot(ggpubr::get_legend(pie_legend))
  
}

map_side <-
  (catch_map_plot + labs(title = bquote(
    ~ bold("a") ~ "Absolute contributions of SSF"
  )) + theme(plot.title = element_text(hjust = 0, vjust = 1))) / nuts_map_plot / emp_map_plot / live_map_plot / w_map_plot


hist_side = ssf_hists_plot



total_plot <-  (map_side |
                  hist_side) + plot_layout(widths = c(1, 1)) &
  theme(plot.title = element_text(size = 13, margin = margin(b = 20)))

if (!plot_pies) {
  total_plot = total_plot + inset_element(
    pie_legend,
    left = 0,
    bottom = 0.09,
    right = -0.75,
    top = 0,
    align_to = "full",
    on_top = FALSE
  )
} else {
  total_plot = total_plot + inset_element(
    pie_legend,
    left = 0,
    bottom = 1.875,
    right = -1,
    top = 0,
    align_to = "full",
    on_top = FALSE
  )
  
}


ggsave(
  file.path(fig_dir, glue::glue("ssf_fig_1.pdf")),
  total_plot,
  width = 180,
  height = 170,
  units = "mm"
)


plots <- ls()[str_detect(ls(), "_plot$")]

purrr::walk(
  plots,
  \(x) ggsave(
    filename = file.path(fig_dir, glue::glue("{x}.png")),
    plot = get(x),
    width = plot_width,
    height = plot_height
  )
)
