#* Title: rscript01
#* 
#* Code function: This script will be executed using the terminal
#* 
#* Creation date: September 03 2026
#* Author: David U. Garibay Treviño

# 01 Initial Setup --------------------------------------------------------

## 01.01 Clean environment ------------------------------------------------
remove(list = ls())

#* Refresh environment memory
gc()

## 01.02 Load libraries ----------------------------------------------------
library(ggplot2)
library(ggpubr)
library(dplyr)


# 02 Define global variables ---------------------------------------------
date_time <- format(Sys.time(), "%Y_%m_%d_%Hh%Mm")

# Use inherited arguments from terminal
args <- commandArgs(trailingOnly = TRUE)

# Check if arguments were provided
if (length(args) == 0) {
  stop("At least one argument must be supplied (input file).\n")
}

# Number of observations to be used in the code
# min: 1, max: 32
n_obs <- as.numeric(args[1])

print(paste0("The inherited value is: ", n_obs))
print(paste0("The class of the value in the Rscript is: ", class(n_obs)))

# Add safety check
if (n_obs <= 0 | n_obs > 32) {
  stop("The inherited value has to be greater than 0 and lower than or equal to 32")
}

# 03 Wrangle data --------------------------------------------------------

# We will load the `mtcars` data set from the `datasets` package preloaded in R
df_cars_00 <- datasets::mtcars

df_cars_01 <- df_cars_00 %>%
  mutate(
    wt_kg = round(wt*0.453592, 3), # calculate weight in kg
    km_lt = round(mpg*0.425144, 3) # Obtain km per litre
  ) %>% 
  # Arrange data putting the lighter cars in the first rows
  arrange(wt_kg) %>% 
  # Keep the first `n_obs` rows in the data
  slice_head(n = n_obs)

# 04 Plot data -----------------------------------------------------------


plt_cars <- ggplot(data    = df_cars_01,
                         mapping = aes(x = wt_kg, 
                                       y = km_lt)) +
  theme_bw() +
  geom_smooth(method = "lm", formula = y ~ x, color = "red", linetype = "dashed") +
  ggpubr::stat_regline_equation(
    label.x= quantile(x = df_cars_01$wt_kg, probs = 0.75), 
    label.y= quantile(x = df_cars_01$km_lt, probs = 0.80)) +
  geom_point()

# plt_cars


# 05 Save outputs --------------------------------------------------------

# Save plot
ggsave(
  plot = plt_cars, 
  filename = paste0("figs/plt_cars_",date_time,".png"), 
  width = 10, 
  height = 6)


# Save updated data set
saveRDS(object = df_cars_01, file = paste0("data/df_cars_", date_time,".png"))
