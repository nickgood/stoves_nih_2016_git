
df <- readRDS("data/emissions_long.RDS")
samples <- readRDS("data/samples.RDS")
unit_list <- unique(df$units)
inst_list <- unique(df$inst)

# pollutant lists
  pol_carb <- unique(dplyr::filter(df, inst == "carbs")$pol)
  pol_ecoc <- unique(dplyr::filter(df, inst == "ecoc")$pol)
  pol_fivegas <- unique(dplyr::filter(df, inst == "fivegas")$pol)
  pol_grav <- unique(dplyr::filter(df, inst == "grav")$pol)
  pol_ions <- unique(dplyr::filter(df, inst == "ions")$pol)
  pol_voc <- unique(dplyr::filter(df, inst == "voc")$pol)
  
# grouping variables
  by_group <- c("none", "fuel", "stove", "stove + fuel")
  by_facet <- c("none", "id", "fuel", "stove", "stove + fuel")
  by_color <- c("none", "id", "fuel", "stove", "stove + fuel")
  by_sin_group <- c("none", "id", "fuel", "stove", "stovecat", "fuelcat")


