# load packages -----------------------------------------------------------

library(sf)
library(vroom)
library(dplyr)
library(tidycensus)
library(bayesplot)
library(rstanarm)
library(tidybayes)
library(broom.mixed)
library(janitor)
library(brms)
library(ggplot2)
library(ggspatial)
library(ggrepel)
library(scales)
library(ggokabeito)
options(tigris_use_cache = TRUE)



# load data ---------------------------------------------------------------

model_df_ma <- readr::read_csv("data/12_16.csv") %>%
  dplyr::filter(abbr == "OH")

points_blocks_df <- readRDS(file = "data/replication/points_blocks_df.RDS")

points_blocks_df_no_geo <- readr::read_csv(file = "data/replication/points_blocks_df_no_geo.csv")

# data desctiption ----------------------------------------------------------


model_df_ma %>%
  dplyr::select(NAMELSAD, prop) %>%
  left_join(county_shp, by = c("NAMELSAD" = "county_name")) %>%
  sf::st_as_sf() -> ohio_descriptive_data


cities_data <- data.frame(
  city = c("Columbus", "Cleveland", "Cincinnati"),
  lon = c(-82.9988, -81.6944, -84.5120),
  lat = c(39.9612, 41.4993, 39.1031)
)

cities_sf <- st_as_sf(cities_data, coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(st_crs(ohio_descriptive_data))

cities_coords <- st_coordinates(cities_sf)
cities_sf$longitude <- cities_coords[,1]
cities_sf$latitude <- cities_coords[,2]

# Create a color palette that reflects the range of changes, with more color variation near zero
color_palette <- c("darkgreen", "lightgreen", "white", "pink", "darkred")
names(color_palette) <- c(-10, -5, 0, 5, 166) # Set these based on your range of data


erased_water_ohio <- tigris::erase_water(ohio_descriptive_data, area_threshold = 0.99)

# Plotting
ggplot(data = ohio_descriptive_data) +
  geom_sf(fill = "antiquewhite1") +
  geom_sf(aes(fill = prop), color = NA) + # Fill counties based on 'prop' value
  geom_sf(data = cities_sf, color = "black", size = 3, shape = 23, fill = NA) + # Custom city markers, smaller size, no fill
  geom_text_repel(
    aes(x = longitude, y = latitude, label = city), # Use longitude and latitude for positioning text
    data = cities_sf,
    fontface = "bold",
    size = 3.5,
    box.padding = unit(0.35, "lines"),
    point.padding = unit(1, "lines"), # Increased distance from the point
    color = "black"
  ) +
  scale_fill_gradientn(colors = color_palette, values = rescale(c(-10, -5, 0, 5, 166))) +
  labs(
    fill = "Percentage Change"
  ) +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
  ) -> county_change_poll_place_svg


ggsave(filename = "county_change_poll_place.svg",
       plot = county_change_poll_place_svg,
       device = "svg",
       path = "figures/",
       width = 7, height = 5, units = "in")


#### graph 3 polling places change
points_blocks_df$longitude <- st_coordinates(points_blocks_df)[, 1]
points_blocks_df$latitude <- st_coordinates(points_blocks_df)[, 2]

ggplot(data = ohio_descriptive_data) +
  geom_sf(fill = "antiquewhite1", color = NA) + # Base map fill
  geom_point(data = points_blocks_df,
             aes(x = longitude, y = latitude, color = ifelse(moved_or_closed == 1, "#B40F20", "#02401B")),
             size = 1.3, alpha = 0.6) + # Polling places
  scale_color_identity() +  # Use direct color mapping
  geom_sf(data = cities_sf,
          aes(geometry = geometry),
          color = "deepskyblue4", size = 4, shape = 23, fill = "white") + # City markers
  geom_text_repel(data = cities_sf,
                  aes(x = longitude, y = latitude, label = city),
                  fontface = "bold", size = 3.5,
                  box.padding = unit(0.35, "lines"),
                  point.padding = unit(1, "lines"),
                  color = "#46ACC8") + # City labels
  labs(title = "Polling Places in Ohio (2012 to 2016)",
       subtitle = "Red dots represent closed polling places; green dots represent active ones") +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Center the title
    plot.subtitle = element_text(size = 12, hjust = 0.5),  # Center the subtitle
    legend.position = "none"  # Hide the legend as colors are self-explanatory
  ) -> poll_places_ohio_svg


ggsave(filename = "poll_places_changes.svg",
       plot = poll_places_ohio_svg,
       device = "svg",
       path = "figures/",
       width = 7, height = 5, units = "in")


median(subset_bg_data$bg_population)

# Now create the complete plot
ggplot(data = subset_bg_data) +
  geom_sf(fill = "antiquewhite1", color = NA) + # Base map fill
  geom_sf(aes(fill = bg_pct_nonwhite), color = NA) + # Fill block groups based on non-white percentage
  geom_point(data = cities_sf, aes(x = longitude, y = latitude), color = "blue", size = 3, shape = 21) + # Add city markers
  geom_text_repel(data = cities_sf,
                  aes(x = longitude, y = latitude, label = city),
                  fontface = "bold", size = 4, color = "black",
                  box.padding = unit(0.35, "lines"),
                  point.padding = unit(1, "lines")) + # Add city labels with a halo effect
  scale_fill_viridis_c(
    name = "Percentage Non-White",
    labels = percent_format(accuracy = 1) # Use a continuous color scale with percent labels
  )  +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5), # Center the title
    plot.subtitle = element_text(size = 12, hjust = 0.5), # Center the subtitle
    legend.position = "bottom" # Position the legend at the bottom
  ) -> bg_svg_black

ggsave(filename = "bg_svg_black.svg",
       plot = bg_svg_black,
       device = "svg",
       path = "figures/",
       width = 7, height = 5, units = "in")

# analysis 2 ----------------------------------------------------------------
points_blocks_df_no_geo %>%
  dplyr::filter(!is.na(Total)) -> points_blocks_df_no_geo

polling_place_model <- stan_glmer(
  moved_or_closed ~ bg_pct_nonwhite + pop_change + (1 | cnty_nm),
  data = points_blocks_df_no_geo, family = binomial,
  prior_intercept = normal(log(1/3), 1, autoscale = TRUE), # log(1/3) reflects the known closure rate
  prior = normal(0, c(0.24, 5.51), autoscale = TRUE), # Adjusted based on the provided scales for β1 and β2
  prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 0.5), # Adjusted for between-county variability
  chains = 4, iter = 4000, seed = 12345
)



polling_place_budget_model <- stan_glmer(
  moved_or_closed ~ bg_pct_nonwhite + pop_change + (1 + log(Total) | cnty_nm),
  data = points_blocks_df_no_geo,
  family = binomial,
  prior_intercept = normal(log(1/3), 1, autoscale = TRUE), # Specific prior for intercept
  prior = normal(0, 2.5, autoscale = TRUE), # General prior for all coefficients
  prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 0.5), # For random effects
  chains = 4,
  iter = 4000,
  seed = 12345
)

# Confirm prior specifications
prior_summary(polling_place_model)

# MCMC diagnostics
mcmc_trace(polling_place_model, size = 0.1)
mcmc_dens_overlay(polling_place_model)
mcmc_acf(polling_place_model)
neff_ratio(polling_place_model)
rhat(polling_place_model)

tidy(polling_place_model, effects = "fixed", conf.int = TRUE, conf.level = 0.80)




points_blocks_df_no_geo$moved_or_closed <- as.factor(points_blocks_df_no_geo$moved_or_closed)

# Fitting the model with brms
polling_place_budget_model_brms <- brm(
  moved_or_closed ~ bg_pct_nonwhite + pop_change + (1 + Total | cnty_nm),
  data = points_blocks_df_no_geo,
  family = bernoulli(),
  prior = c(
    prior(normal(log(1/3), 1), class = Intercept),
    prior(normal(0, 0.24), class = b, coef = bg_pct_nonwhite),
    prior(normal(0, 5.51), class = b, coef = pop_change),
    prior(normal(0, 1), class = sd),
    prior(normal(0, 1), class = b, coef = Total)
  ),
  chains = 4,
  iter = 4000,
  seed = 12345,
  control = list(adapt_delta = 0.95) # Adjust this for convergence issues
)





# Adding the 'inits' argument to the brm() call
polling_place_model_brms <- brm(
  formula = moved_or_closed ~ bg_pct_nonwhite + pop_change + (1 | cnty_nm),
  data = points_blocks_df_no_geo,
  family = bernoulli(),
  prior = c(
    prior(normal(-1.098612, 1), class = Intercept),
    prior(normal(0, 0.24), class = b, coef = "bg_pct_nonwhite"),
    prior(normal(0, 5.51), class = b, coef = "pop_change"),
    prior(normal(0, 0.5), class = sd)
  ),
  chains = 4,
  iter = 8000,
  seed = 12345
)

saveRDS(polling_place_model_brms, file = "models/polling_place_model_brms.RDS")

get_prior(formula = moved_or_closed ~ bg_pct_nonwhite + pop_change + (1 + log(Total) | cnty_nm),
          family = bernoulli(), data = points_blocks_df_no_geo)

polling_place_budget_model_brms <- brm(
  formula = moved_or_closed ~ bg_pct_nonwhite + pop_change + log(total) + (1 | cnty_nm),
  data = points_blocks_df_no_geo,
  family = bernoulli(),
  prior = c(
    prior(normal(-1.098612, 1), class = Intercept),
    prior(normal(0, 0.24), class = b, coef = "bg_pct_nonwhite"),
    prior(normal(0, 5.51), class = b, coef = "pop_change"),
    prior(normal(0, 0.5), class = sd, group = "cnty_nm"),  # Prior for group-level standard deviation
    prior(normal(0, 0.5), class = sd, coef = "logTotal", group = "cnty_nm")  # Correct prior specification for the slope of log-transformed Total within cnty_nm
  ),
  chains = 4,
  iter = 8000,
  seed = 420
)

saveRDS(polling_place_budget_model_brms, file = "models/polling_place_budget_model_brms.RDS")



# plots with results


polling_place_model_brms %>%
  gather_draws(c(b_bg_pct_nonwhite, b_pop_change, `(1 | cnty_nm) Intercept`)) %>%
  ggplot(aes(x = .value, y = .variable, fill = .variable)) +
  geom_vline(xintercept = 0) +
  stat_halfeye(.width = c(0.8, 0.95), alpha = 0.8, point_interval = "median_hdi") +
  scale_fill_okabe_ito() +
  guides(fill = "none") +
  theme_minimal() +
  labs(x = "Effect Size (Log Odds)",
       y = "") +
  theme(text = element_text(size = 16),
        axis.title.x = element_text(size = 12, family = "Arial")) +
  scale_y_discrete(labels = c("Percent Non-White", "Population Change")) -> base_model_viz


ggsave(filename = "base_model_viz.svg",
       plot = base_model_viz,
       device = "svg",
       path = "figures/",
       width = 7, height = 5, units = "in")




polling_place_budget_model_brms_2 <- brm(
  formula = moved_or_closed ~ bg_pct_nonwhite + pop_change + log(Total) + (1 | cnty_nm),
  data = points_blocks_df_no_geo,
  family = bernoulli(),
  prior = c(
    prior(normal(-1.098612, 1), class = Intercept),
    prior(normal(0, 0.24), class = b, coef = "bg_pct_nonwhite"),
    prior(normal(0, 5.51), class = b, coef = "pop_change"),
    prior(normal(0, 0.5), class = sd, group = "cnty_nm")  # Prior for group-level standard deviation
  ),
  chains = 4,
  iter = 8000,
  seed = 420
)


# Get relationship summaries for both models
model_1_mean <- tidy(polling_place_model_brms, effects = "fixed")
model_2_mean <- tidy(polling_place_budget_model_brms_2, effects = "fixed")


model_1_mean %>%
  right_join(., model_2_mean, by = "term",
             suffix = c("_model_1", "_model_2")) %>%
  dplyr::select(term, estimate_model_1, estimate_model_2)


# Get variance summaries for both models
model_1_var <- tidy(polling_place_model_brms, effects = "ran_pars") %>% View()
model_2_var <- tidy(polling_place_budget_model_brms_2, effects = "ran_pars")



model_1_var %>%
  right_join(., model_2_var, by = "term",
             suffix = c("_model_1", "_model_2")) %>%
  select(term, estimate_model_1, estimate_model_2)





polling_place_budget_model <- stan_glmer(
  moved_or_closed ~ bg_pct_nonwhite + pop_change + log(Total) + (1 | cnty_nm),
  data = points_blocks_df_no_geo,
  family = binomial,
  prior_intercept = normal(log(1/3), 1, autoscale = TRUE), # Specific prior for intercept
  prior = normal(0, 2.5, autoscale = TRUE), # General prior for all coefficients
  prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 0.5), # For random effects
  chains = 4,
  iter = 8000,
  seed = 420
)

polling_place_model_simple <- stan_glmer(
  moved_or_closed ~ bg_pct_nonwhite + pop_change + (1 | cnty_nm),
  data = points_blocks_df_no_geo,
  family = binomial,
  prior_intercept = normal(log(1/3), 1, autoscale = TRUE), # Specific prior for intercept
  prior = normal(0, 2.5, autoscale = TRUE), # General prior for all coefficients
  prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 0.5), # For random effects
  chains = 4,
  iter = 8000,
  seed = 420
)

# Get relationship summaries for both models
model_1_mean <- tidy(polling_place_model_simple, effects = "fixed")
model_2_mean <- tidy(polling_place_budget_model, effects = "fixed")

# Combine the summaries for both models
combined_summaries <- model_1_mean %>%
  right_join(., model_2_mean, by = "term",
             suffix = c("_model_1", "_model_2")) %>%
  select(-starts_with("std.error"))

combined_summaries
# Get variance summaries for both models
model_1_var <- tidy(polling_place_model_simple, effects = "ran_pars")
model_2_var <- tidy(polling_place_budget_model, effects = "ran_pars")

# Combine the summaries for both models
model_1_var %>%
  right_join(., model_2_var, by = "term",
             suffix = c("_model_1", "_model_2")) %>%
  select(-starts_with("group"))



