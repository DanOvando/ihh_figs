library(tidyverse)

library(sf)

library(rnaturalearth)

# library(rnaturalearthhires)

library(rnaturalearthdata)

library(countrycode)

library(readxl)

library(readr)

library(stringr)

library(janitor)

library(here)

library(png)

library(ggpubr)

library(ggimage)

library(ggridges)

library(ggdist)

library(patchwork)

library(cartogram)

library(glue)

library(ggtext)

theme_set(ggpubr::theme_pubclean(base_size = 10))

if (!dir.exists("results")) {
  dir.create("results", recursive = TRUE)
}

sf::sf_use_s2(FALSE)

run_name <- "v1.1"

remove_na <- TRUE

cartoify <- FALSE

plot_pies <- TRUE

pie_size <- 100

plot_width = 6

plot_height = 8

unknown_color <- c("tan")

fig_dir <-
  here(
    "results",
    run_name ,
    ifelse(remove_na, "NAs are treated as zero", "NAs treated as missing")
  )

if (!dir.exists(fig_dir)) {
  dir.create(fig_dir, recursive = TRUE)
}

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
