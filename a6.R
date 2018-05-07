# Questions:
# Is "most recent" 2015? Can I change the label to 2015?
# Total gdp is different online than from what I summed up
# Changing column names of years
# breaks don't equal labels, no +10 million kt label

# To Do:
# 1. Detailed caption for first visual
# 2. Second Visual
# 3. Detailed caption for second visual
# 4. Map tiers

library("dplyr")
library("ggplot2")
library("tidyr")
library("maps")
library("plotly")

setwd("~/Desktop/a6-data-visualization-HanaStrickland")
emissions_data <- read.csv("data/WDI_emissions_Data.csv", stringsAsFactors = FALSE)
country_data <- read.csv("data/WDI_selected_Data.csv", stringsAsFactors = FALSE, na.strings = "..")

filter_co2_emissions_kt <- function() {
  filter_data <-
    emissions_data %>%
    filter(Series.Code == "EN.ATM.CO2E.KT") %>%
    select(-Most_Recent)

  filter_data
}


filter_gdp <- function() {
  filter_data <-
    country_data %>%
    filter(Series.Code == "NY.GDP.MKTP.CD") %>%
    select(-Most_Recent)

  filter_data
}




### Question: How has co2 emissions changed between 1998 and 2014  in relation to GDP? ###

get_gdp_co2_year_graphic <- function() {
  # Filter for co2 emissions and gdp
  filter_co2_emissions_kt()

  filter_gdp()

  # sum columns of data by year
  total_co2_emissions_kt <- colSums(filter_co2_emissions_kt()[, -1:-2], na.rm = TRUE)
  total_gdp <- colSums(filter_gdp()[, -1:-4], na.rm = TRUE)

  # Make summed info into dataframes
  total_co2_df <- as.data.frame(total_co2_emissions_kt)
  total_gdp_df <- as.data.frame(total_gdp)

  # change year row name into "year"
  total_co2_df <- total_co2_df %>%
    mutate(year = rownames(total_co2_df))
  total_gdp_df <- total_gdp_df %>%
    mutate(year = rownames(total_gdp_df))

  # Change gdp years to be the same as co2 years
  total_gdp_df$year <- c(
    "YR1998", "YR1999", "YR2000", "YR2001", "YR2002", "YR2003", "YR2004", "YR2005", "YR2006",
    "YR2007", "YR2008", "YR2009", "YR2010", "YR2011", "YR2012", "YR2013", "YR2014"
  )


  # join co2 and gdp data
  both_co2_gdp <- left_join(total_co2_df, total_gdp_df, by = "year")

  # organize columns
  both_co2_gdp <- both_co2_gdp %>%
    select(year, total_co2_emissions_kt, total_gdp)

  # Plot data
  plot_graphic <- ggplot(data = both_co2_gdp) +
    geom_point(mapping = aes(x = year, y = total_co2_emissions_kt, color = total_gdp)) +
    scale_color_gradientn(colors = c("blue", "orange")) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    labs(title = "Change in CO2 Emissions Over Time in Relation to GDP")

  plot_graphic
}

### Question: Regionally, how has co2 emissions changed between 2004 and 2014? ###

get_regional_co2_emissions <- function() {
  regional_data <- read.csv("data/WDI_regional.csv")

  regional_emissions <- regional_data %>%
    filter(Series.Code == "EN.ATM.CO2E.KT") %>%
    filter(Country.Name != "World") %>%
    select(-Country.Code, -Series.Name)

  # Change names to make them shorter and more relevant
  names(regional_emissions) <- c(
    "Region", "Series_Code", "YR2004", "YR2005", "YR2006", "YR2007",
    "YR2008", "YR2009", "YR2010", "YR2011",
    "YR2012", "YR2013", "YR2014"
  )


  regional_long <- gather(regional_emissions,
    key = year, value = co2_emitted,
    "YR2004", "YR2005", "YR2006", "YR2007",
    "YR2008", "YR2009", "YR2010", "YR2011",
    "YR2012", "YR2013", "YR2014"
  )

  ggplot(data = regional_long) +
    geom_point(mapping = aes(x = year, y = co2_emitted, color = Region)) +
    facet_wrap(~ Region) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    labs(title = "Change in CO2 Emissions Over Time in Relation By Region")
}
get_regional_co2_emissions()
### Question: How do co2 emissions compare between different countries in 2014? ###

### World CO2 Emissions ###

get_world_map <- function() {
  world_map <- map_data("world")

  iso_data <- iso.alpha(world_map$region, n = 3)

  world_map$iso <- iso_data

  # Join map and co2 data
  world_map_and_co2 <- left_join(world_map, filter_co2_emissions_kt(),
    by = c("iso" = "Country.Code")
  )

  world_map_and_co2 <-
    world_map_and_co2 %>%
    select(long, lat, group, order, region, iso, YR2014)

  scale_breaks <- c(100, 1000, 10000, 100000, 1000000, 10000000, 100000000) # log10

  factors_of_emissions <- cut(world_map_and_co2$YR2014,
    breaks = scale_breaks,
    labels = c("less than 100kt", "100 - 1,000kt", "1,000 - 10,000kt", "10,000 - 100,000kt", "100,000 - 1,000,000kt", "1,000,000 - 10,000,000"),
    ordered_result = TRUE
  )
  
  factors_of_emissions <- as.data.frame(factors_of_emissions) #ordered factor to data frame

  world_map_and_co2$emission_level <- factors_of_emissions # new column for emissions level

  plot_graphic <- ggplot(data = world_map_and_co2) +
    geom_polygon(aes(x = long, y = lat, group = group, fill = emission_level)) +
    coord_quickmap() +
    scale_fill_brewer(type = div, palette = "Set2") +
    theme(legend.position = "bottom") + 
    labs(title = "World Map of CO2 Emissions", 
         caption = "(Based on data from the World Bank)",
         x = "Longitude", y = "Latitude")
    
  
  plot_graphic
}

get_world_map()

# YR2014
# Min.   :      11
# 1st Qu.:    1335
# Median :    9109
# Mean   :  165114
# 3rd Qu.:   59864
# Max.   :10291927


### Interactive Visualization ###

# GDP and CO2 in 2014 

# Filter for co2 emissions and gdp
co2_interactive <- filter_co2_emissions_kt() %>% 
  select(-Series.Code)  

gdp_interactive <- filter_gdp() %>% 
  select(-Series.Code, -Series.Name)

# Change year names on gdp data frame
colnames(gdp_interactive) <- c("Country.Name", "Country.Code", 
                               "YR1998", "YR1999", "YR2000", "YR2001", "YR2002", "YR2003", 
                               "YR2004", "YR2005", "YR2006", "YR2007", 
                               "YR2008", "YR2009", "YR2010", "YR2011",
                                       "YR2012", "YR2013", "YR2014")
# make them long form
co2_long <- gather(co2_interactive,
                        key = year, value = co2_emitted, 
                   "YR1998", "YR1999", "YR2000", "YR2001", "YR2002", "YR2003",
                   "YR2004", "YR2005", "YR2006", "YR2007",
                   "YR2008", "YR2009", "YR2010", "YR2011",
                   "YR2012", "YR2013", "YR2014")

gdp_long <- gather(gdp_interactive,
                  key = year, value = GDP, 
                  "YR1998", "YR1999", "YR2000", "YR2001", "YR2002", "YR2003",
                  "YR2004", "YR2005", "YR2006", "YR2007",
                  "YR2008", "YR2009", "YR2010", "YR2011",
                  "YR2012", "YR2013", "YR2014")


gdp_co2_interactive <- left_join(co2_long, gdp_long, by = c("Country.Code", "year"))

# organize columns
gdp_co2_interactive <- gdp_co2_interactive %>%
  select(Country.Code, Country.Name, year, co2_emitted, GDP)



# gdp_co2_interactive_2014 <- gdp_co2_interactive %>% 
#   filter(year == "YR2014")

# Get regional info (from assignment 3 data folder)
world_region_info <- read.csv("data/life_expectancy.csv", stringsAsFactors = FALSE)

world_region_info <- world_region_info %>% 
  select(country, region, income_group)

gdp_co2_interactive <- left_join(gdp_co2_interactive, world_region_info, 
                                      by = c("Country.Name" = "country"))
colnames(gdp_co2_interactive)
View(gdp_co2_interactive)

usa_and_china <- gdp_co2_interactive %>% 
  filter(Country.Name %in% c("United States", "China"))
usa_and_china
interactive_plot <- gdp_co2_interactive %>%
  plot_ly(
    x = ~GDP,
    y = ~co2_emitted,
    frame = ~year,
    type = "scatter",
    mode = "markers",
    color = ~region,
    text = ~Country.Name,
    showlegend = F
  ) %>%
  layout(
    xaxis = list(type = "log"),
    yaxis = list(type = "log")
    )

interactive_plot

plot_gdp_by_co2 <- ggplot(data = gdp_co2_interactive) +
  geom_point(mapping = aes(x = GDP, y = co2_emitted, color = region)) +
  scale_color_brewer(palette = "Set3") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(title = "Scatter Plot of Country CO2 Emissions and GDP on a Log Scale") +
  scale_x_log10() +
  scale_y_log10()

plot_gdp_by_co2




# Resources Used:
# http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
# https://plot.ly/r/line-and-scatter/
# https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf 
# https://blog.exploratory.io/filter-data-with-dplyr-76cf5f1a258e 
# https://plot.ly/r/animations/ 