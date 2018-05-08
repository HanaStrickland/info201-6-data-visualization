# Questions:
# Total gdp is different online than from what I summed up
# Changing column names of years
# breaks don't equal labels, no +10 million kt label
# Changing label to trillions
# Paragraph in .Rmd

# To Do:
# 1. Detailed captions
# 2. Graphic 1 key needs to change to accending


library("dplyr")
library("ggplot2")
library("tidyr")
library("maps")
library("plotly")

setwd("~/Desktop/a6-data-visualization-HanaStrickland")
emissions_data <- read.csv("data/WDI_emissions_Data.csv", stringsAsFactors = FALSE)
country_data <- read.csv("data/WDI_selected_Data.csv", stringsAsFactors = FALSE, na.strings = "..")

# Change years to match co2 years format

years_in_YRxxxx_form <- c(
  "YR1998", "YR1999", "YR2000", "YR2001", "YR2002", "YR2003", "YR2004",
  "YR2005", "YR2006", "YR2007", "YR2008", "YR2009", "YR2010", "YR2011",
  "YR2012", "YR2013", "YR2014"
)


colnames(country_data) <- c(
  "Country.Name", "Country.Code", "Series.Name", "Series.Code", years_in_YRxxxx_form, "Most_Recent"
)


filter_co2_emissions_kt <- function() {
  filter_data <-
    emissions_data %>%
    filter(Series.Code == "EN.ATM.CO2E.KT") %>%
    select(-Most_Recent)

  filter_data
}

filtered_co2 <- filter_co2_emissions_kt()

filter_gdp <- function() {
  filter_data <-
    country_data %>%
    filter(Series.Code == "NY.GDP.MKTP.CD") %>%
    select(-Most_Recent)

  filter_data
}

filtered_gdp <- filter_gdp()


filter_life_expectancy <- function() {
  filter_data <-
    country_data %>%
    filter(Series.Code == "SP.DYN.LE00.IN") %>%
    select(-Most_Recent)

  filter_data_long <- gather(filter_data,
    key = year, value = life_expectancy, years_in_YRxxxx_form
  )
}

filtered_life_expectancy <- filter_life_expectancy()

###
### Question: How has co2 emissions changed between 1998 and 2014  in relation to GDP? ###
###

# sum columns of data by year
total_co2 <- colSums(filtered_co2[, -1:-2], na.rm = TRUE)
total_gdp <- colSums(filtered_gdp[, -1:-4], na.rm = TRUE)

# Make summed info into dataframes
total_co2_df <- as.data.frame(total_co2)
total_gdp_df <- as.data.frame(total_gdp)

# change year row name into "year"
total_co2_df <- total_co2_df %>%
  mutate(year = rownames(total_co2_df))
total_gdp_df <- total_gdp_df %>%
  mutate(year = rownames(total_gdp_df))


# join co2 and gdp data
both_co2_gdp <- left_join(total_co2_df, total_gdp_df, by = "year")

# organize columns
both_co2_gdp <- both_co2_gdp %>%
  select(year, total_co2, total_gdp)

# Plot data
plot_co2_gdp_graphic <- ggplot(data = both_co2_gdp) +
  geom_point(mapping = aes(x = year, y = total_co2, color = total_gdp)) +
  scale_color_gradientn(
    colors = c("dark blue", "orange", "dark green"),
    guide = guide_legend(title = "Total GDP", reverse = TRUE),
    breaks = c(4e+13, 4.5e+13, 5e+13, 5.5e+13, 6e+13, 6.5e+13, 7e+13),
    labels = c("40 Trillion", "45 Trillion", "50 Trillion", "55 Trillion", "60 Trillion", "65 Trillion", "70 Trillion")
  ) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(title = "Change in CO2 Emissions Over Time", x = "Year", y = "CO2 (kt)")

# For .Rmd File
co2_2008 <- both_co2_gdp %>% 
  filter(year == "YR2008") %>% 
  select(total_co2)

co2_2008 <- co2_2008[1,1]

co2_2008 <- paste(format(round(co2_2008 / 1e6, 2), trim = TRUE), "Million")


gdp_2008 <- both_co2_gdp %>% 
  filter(year == "YR2008") %>% 
  select(total_gdp)

gdp_2008 <- gdp_2008[1,1]

gdp_2008 <- paste(format(round(gdp_2008 / 1e12, 2), trim = TRUE), "Trillion")

co2_2009 <- both_co2_gdp %>% 
  filter(year == "YR2009") %>% 
  select(total_co2)

co2_2009 <- co2_2009[1,1]

co2_2009 <- paste(format(round(co2_2009 / 1e6, 2), trim = TRUE), "Million")

gdp_2009 <- both_co2_gdp %>% 
  filter(year == "YR2009") %>% 
  select(total_gdp)

gdp_2009 <- gdp_2009[1,1]

gdp_2009 <- paste(format(round(gdp_2009 / 1e12, 2), trim = TRUE), "Trillion")

###
### Question: Regionally, how has co2 emissions changed between 2004 and 2014? ###
###
regional_data <- read.csv("data/WDI_regional.csv")

regional_change_years_form <- c(
  "2004", "2005", "2006", "2007",
  "2008", "2009", "2010", "2011",
  "2012", "2013", "2014"
)

regional_emissions <- regional_data %>%
  filter(Series.Code == "EN.ATM.CO2E.KT") %>%
  filter(Country.Name != "World") %>%
  select(-Country.Code, -Series.Name)

# Change names to make them shorter and more relevant
names(regional_emissions) <- c(
  "Region", "Series_Code", regional_change_years_form
)


regional_emissions_long <- gather(regional_emissions,
  key = year, value = co2_emitted, regional_change_years_form
)

regional_gdp <- regional_data %>%
  filter(Series.Code == "NY.GDP.MKTP.CD") %>%
  filter(Country.Name != "World") %>%
  select(-Country.Code, -Series.Name)

names(regional_gdp) <- c(
  "Region", "Series_Code", regional_change_years_form
)

regional_gdp_long <- gather(regional_gdp,
  key = year, value = GDP, regional_change_years_form
)

regional_data <- left_join(regional_emissions_long, regional_gdp_long, by = c("Region", "year"))


regional_data$year <- as.numeric(regional_data$year)
regional_data[regional_data$Region, ]

plot_regional <- ggplot(regional_data, aes(year, co2_emitted, color = factor(Region))) + 
  geom_point() 

# plot_regional_graphic <- plot_regional + facet_grid(. ~ Region) +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1))



###
### Question: How do co2 emissions compare between different countries in 2014? ###
###

### World CO2 Emissions ###


world_map <- map_data("world")

iso_data <- iso.alpha(world_map$region, n = 3)

world_map$iso <- iso_data

# Join map and co2 data
world_map_and_co2 <- left_join(world_map, filtered_co2,
  by = c("iso" = "Country.Code")
)

world_map_and_co2 <-
  world_map_and_co2 %>%
  select(long, lat, group, order, region, iso, YR2014)

scale_breaks <- c(100, 1000, 10000, 100000, 1000000, 10000000, 100000000) # log10

factors_of_emissions <- cut(world_map_and_co2$YR2014,
  breaks = scale_breaks,
  labels = c(
    "less than 100kt", "100 - 1,000kt", "1,000 - 10,000kt", "10,000 - 100,000kt",
    "100,000 - 1,000,000kt", "1,000,000 - 10,000,000"
  ),
  ordered_result = TRUE
)

factors_of_emissions <- as.data.frame(factors_of_emissions) # ordered factor to data frame

world_map_and_co2$emission_level <- factors_of_emissions # new column for emissions level

plot_world_graphic <- ggplot(data = world_map_and_co2) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = emission_level)) +
  coord_quickmap() +
  scale_fill_brewer(type = div, palette = "Set2") +
  theme(legend.position = "bottom") +
  labs(
    title = "World Map of CO2 Emissions",
    caption = "(Based on data from the World Bank)",
    x = "Longitude", y = "Latitude"
  )






### Interactive Visualization ###

# Filter for co2 emissions and gdp
co2_interactive <- filtered_co2 %>%
  select(-Series.Code)

gdp_interactive <- filtered_gdp %>%
  select(-Series.Code, -Series.Name)


# make them long form
co2_long <- gather(co2_interactive,
  key = year, value = co2_emitted, years_in_YRxxxx_form
)

gdp_long <- gather(gdp_interactive,
  key = year, value = GDP, years_in_YRxxxx_form
)


gdp_co2_interactive <- left_join(co2_long, gdp_long, by = c("Country.Code", "year"))

# organize columns
gdp_co2_interactive <- gdp_co2_interactive %>%
  select(Country.Code, Country.Name, year, co2_emitted, GDP)

# Get regional info (from assignment 3 data folder)
world_region_info <- read.csv("data/life_expectancy.csv", stringsAsFactors = FALSE)

world_region_info <- world_region_info %>%
  select(country, region, income_group)

gdp_co2_interactive <- left_join(gdp_co2_interactive, world_region_info,
  by = c("Country.Name" = "country")
)

# add life expectancy info to data frame
gdp_co2_interactive <- left_join(gdp_co2_interactive, filtered_life_expectancy,
  by = c("Country.Code", "Country.Name", "year")
)

gdp_co2_interactive <- gdp_co2_interactive %>%
  filter(Country.Code %in% c("USA", "CHN", "JPN", "DEU", "GBR", "FRA", "IND", "ITA", "BRA", "CAN"))


co2_1dim <- gdp_co2_interactive$co2_emitted

plot_interactive <- gdp_co2_interactive %>%
  plot_ly(
    x = ~ gdp_co2_interactive$GDP,
    y = ~ co2_1dim,
    frame = gdp_co2_interactive$year,
    text = gdp_co2_interactive$Country.Name,
    color = ~ gdp_co2_interactive$region
  ) %>%
  add_markers() %>%
  add_text(textposition = "left") %>%
  layout(showlegend = FALSE) %>%
  layout(
    title = "Co2 Emissions and GDP of Ten Largest Economies Over Time",
    xaxis = list(title = "GDP (in Trillions of Dollars)", type = "log"),
    yaxis = list(title = "CO2 Emitted in kt", type = "log")
  )










# Resources Used:
# http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
# https://plot.ly/r/line-and-scatter/
# https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf
# https://blog.exploratory.io/filter-data-with-dplyr-76cf5f1a258e
# https://plot.ly/r/animations/
