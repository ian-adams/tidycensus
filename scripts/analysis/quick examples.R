library(tidycensus)
library(tidyverse)
library(tigris)
options(tigris_use_cache = TRUE)
library(here)


#census_api_key("cd5009854d5b0c175bf11ee896e8ae9f31c7dc1b", install = T) ## load if need api, but should be installed



ut <- get_acs(geography = "county", 
              variables = c(medincome = "B19013_001"), 
              state = "UT", 
              year = 2020)

## HHI for whole state
ut %>%
  mutate(NAME = gsub(" County, Utah", "", NAME)) %>%
  ggplot(aes(x = estimate, y = reorder(NAME, estimate))) +
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(color = "red", size = 3) +
  labs(title = "Household income by county in Utah",
       subtitle = "2014-2020 American Community Survey",
       y = "",
       x = "ACS estimate (bars represent margin of error)")

ggsave(filename = here("output", "figures", "utahHHI.png"))


## Median household income Salt Lake County -- 
saltlake <- get_acs(state = "UT", county = "Salt Lake", geography = "tract", 
                           variables = "B19013_001", geometry = TRUE,
                           year = 2020)
head(saltlake)


saltlake %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "magma") 

ggsave(filename = here("output", "figures", "SLCoHHIheat.png"))


## Facet by race % - 2010 data (2020 not avail yet?)
racevars <- c(White = "P005003", 
              Black = "P005004", 
              Asian = "P005006", 
              Hispanic = "P004003")

saltlake2 <- get_decennial(geography = "tract", variables = racevars, 
                        state = "UT", county = "Salt Lake", geometry = TRUE,
                        summary_var = "P001001", year = 2010) 

saltlake2 %>%
  mutate(pct = 100 * (value / summary_value)) %>%
  ggplot(aes(fill = pct)) +
  facet_wrap(~variable) +
  geom_sf(color = NA) +
  scale_fill_viridis_c()

ggsave(filename = here("output", "figures", "SLCo_race.png"))

## Net migration estimates

us_components <- get_estimates(geography = "state", product = "components")

unique(us_components$variable)

net_migration <- get_estimates(
  geography = "county",
  variables = "RNETMIG",
  year = 2019,
  geometry = TRUE,
  resolution = "20m"
) %>%
  shift_geometry()

net_migration


order = c("-15 and below", "-15 to -5", "-5 to +5", "+5 to +15", "+15 and up")

net_migration <- net_migration %>%
  mutate(groups = case_when(
    value > 15 ~ "+15 and up",
    value > 5 ~ "+5 to +15",
    value > -5 ~ "-5 to +5",
    value > -15 ~ "-15 to -5",
    TRUE ~ "-15 and below"
  )) %>%
  mutate(groups = factor(groups, levels = order))

state_overlay <- states(
  cb = TRUE,
  resolution = "20m"
) %>%
  filter(GEOID != "72") %>%
  shift_geometry()

ggplot() +
  geom_sf(data = net_migration, aes(fill = groups, color = groups), size = 0.1) +
  geom_sf(data = state_overlay, fill = NA, color = "black", size = 0.1) +
  scale_fill_brewer(palette = "PuOr", direction = -1) +
  scale_color_brewer(palette = "PuOr", direction = -1, guide = FALSE) +
  coord_sf(datum = NA) +
  theme_minimal(base_family = "Roboto") +
  labs(title = "Net migration per 1000 residents by county",
       subtitle = "US Census Bureau 2020 Population Estimates",
       fill = "Rate",
       caption = "Data acquired with the R tidycensus package | @kyle_e_walker")


## Estimates of population characteristics

sl_age_hisp <- get_estimates(
  geography = "county",
  product = "characteristics",
  breakdown = c("SEX", "AGEGROUP", "HISP"),
  breakdown_labels = TRUE,
  state = "UT",
  county = "Salt Lake"
)

compare <- filter(
  sl_age_hisp,
  str_detect(AGEGROUP, "^Age"),
  HISP != "Both Hispanic Origins",
  SEX != "Both sexes"
) %>%
  mutate(value = ifelse(SEX == "Male",-value, value))

ggplot(compare, aes(x = AGEGROUP, y = value, fill = SEX)) + 
  geom_bar(stat = "identity", width = 1) + 
  theme_minimal(base_family = "Roboto") + 
  scale_y_continuous(labels = function(y) paste0(abs(y / 1000), "k")) + 
  scale_x_discrete(labels = function(x) gsub("Age | years", "", x)) + 
  scale_fill_manual(values = c("darkred", "navy")) + 
  coord_flip() + 
  facet_wrap(~HISP) + 
  labs(x = "", 
       y = "2020 Census Bureau population estimate", 
       title = "Population structure by Hispanic origin", 
       subtitle = "Salt Lake County, UT", 
       fill = "", 
       caption = "Data source: US Census Bureau population estimates & tidycensus R package")


## Mapping migration flows
library(mapdeck)

key <- 'pk.eyJ1IjoiaWFkYW1zNzgiLCJhIjoiY2wxd3lwdGhjMDVsMDNwc2pqZndweTZocyJ9.aLfSgEAjHUs1Clti_MWDNw'
mapdeck(token = key)
set_token(key)
mapdeck_tokens()

slc_flows <- get_flows(
  geography = "county",
  state = "UT",
  county = "Salt Lake",
  year = 2019,
  geometry = TRUE
)

slc_flows %>% 
  head()

top_move_in <- slc_flows %>% 
  filter(!is.na(GEOID2), variable == "MOVEDIN") %>% 
  slice_max(n = 25, order_by = estimate) %>% 
  mutate(
    width = estimate / 500,
    tooltip = paste0(
      scales::comma(estimate * 5, 1),
      " people moved from ", str_remove(FULL2_NAME, "Metro Area"),
      " to ", str_remove(FULL1_NAME, "Metro Area"), " in 2019"
    )
  )

top_move_in %>% 
  mapdeck(style = mapdeck_style("dark"), pitch = 45) %>% 
  add_arc(
    origin = "centroid1",
    destination = "centroid2",
    stroke_width = "width",
    auto_highlight = TRUE,
    highlight_colour = "#8c43facc",
    tooltip = "tooltip"
  )
