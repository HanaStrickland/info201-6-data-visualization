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
  labs(title = "Change in Global CO2 Emissions Over Time", x = "Year", y = "CO2 (kt)")

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


co2_2014 <- both_co2_gdp %>% 
  filter(year == "YR2014") %>% 
  select(total_co2)

co2_2014 <- co2_2014[1,1]
co2_2014 <- paste(format(round(co2_2014 / 1e6, 2), trim = TRUE), "Million")


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

plot_regional <- ggplot(regional_data, aes(year, co2_emitted, color = factor(Region))) + 
  geom_point()  +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  guides(color = guide_legend(title = "Region")) +
  labs(title = "Regional Breakdown of CO2 Emissions", x = "Year", y = "CO2 Emissions (kt)") +
  scale_y_continuous(breaks = c(2.5e06, 5e06, 7.5e06, 1e07, 1.25e07), 
                     labels = c("2.5 Million", "5 Million", "7.5 Million", "10 Million", "12.5 Million") )

plot_regional_graphic <- plot_regional + facet_grid(. ~ Region)



# For .Rmd File


display_regional_co2 <- paste(format(round(regional_data$co2_emitted / 1e6, 2), trim = TRUE), "Million")

display_regional_gdp <- paste(format(round(regional_data$GDP / 1e12, 2), trim = TRUE), "Trillion")

regional_data$co2_emitted <- display_regional_co2

regional_data$GDP <- display_regional_gdp

africa_2014 <- regional_data %>% 
  filter(year == "2014", Region == "Sub-Saharan Africa") %>% 
  select(GDP, co2_emitted)

africa_gdp_2014 <- africa_2014[1, "GDP"]
africa_co2_2014 <- africa_2014[1, "co2_emitted"]


latin_2014 <- regional_data %>% 
  filter(year == "2014", Region == "Latin America & Caribbean") %>% 
  select(GDP, co2_emitted)

latin_gdp_2014 <- latin_2014[1, "GDP"]
latin_co2_2014 <- latin_2014[1, "co2_emitted"]


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

world_map_and_co2 <- na.omit(world_map_and_co2)

scale_breaks <- c(0, 1000, 10000, 100000, 1000000, 10000000, 100000000) # log10

factors_of_emissions <- cut(world_map_and_co2$YR2014,
  breaks = scale_breaks,
  labels = c(
    "less than 1000kt", 
    #"100 - 1,000kt", 
    "1,000 - 10,000kt", 
    "10,000 - 100,000kt",
    "100,000 - 1,000,000kt", 
    "1,000,000 - 10,000,000kt",
    "+10,000,000kt"
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
    x = "", y = ""
  )  +
  guides(fill = guide_legend(title = "Emission Level"))


# For .Rmd File
china_co2_2014 <- filtered_co2 %>% 
  filter(Country.Code == "CHN") %>% 
  select(YR2014)

china_co2_2014_map <- paste(format(round(china_co2_2014 / 1e6, 2), trim = TRUE), "Million")

usa_co2_2014 <- filtered_co2 %>% 
  filter(Country.Code == "USA") %>% 
  select(YR2014)

usa_co2_2014 <- paste(format(round(usa_co2_2014 / 1e6, 2), trim = TRUE), "Million")

###
### Interactive Visualization ###
###

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


top_ten_interactive <- gdp_co2_interactive %>%
  filter(Country.Code %in% c("USA", "CHN", "JPN", "DEU", "GBR", "FRA", "BRA", "ITA", "RUS", "IND"))

co2_1dim <- top_ten_interactive$co2_emitted

plot_interactive <- top_ten_interactive %>%
  plot_ly(
    x = ~ top_ten_interactive$GDP,
    y = ~ co2_1dim,
    frame = top_ten_interactive$year,
    text = top_ten_interactive$Country.Name
  ) %>%
  add_markers() %>%
  add_text(textposition = "left") %>% 
  layout(showlegend = FALSE) %>%
  layout(
    title = "Co2 Emissions and GDP of Ten Largest Economies Over Time",
    xaxis = list(title = "GDP (in Trillions of Dollars)", type = "log"),
    yaxis = list(title = "CO2 Emitted (in kt)", type = "log")
  ) 


# For .Rmd File
top_economies <- top_ten_interactive %>% 
  filter(year == "YR2014") %>% 
  select(Country.Name, co2_emitted, GDP) %>% 
  arrange(-co2_emitted)

 
top_co2 <- paste(format(round(top_economies$co2_emitted / 1e6, 2), trim = TRUE), "Million")

top_gdp <- paste(format(round(top_economies$GDP / 1e12, 2), trim = TRUE), "Trillion")

top_economies$co2_emitted <- top_co2

top_economies$GDP <- top_gdp

east_asia <- gdp_co2_interactive %>% 
  filter(region == "East Asia & Pacific", year == "YR2014") %>% 
  arrange(-co2_emitted)

east_asia_top <- sum(east_asia[1:3, "co2_emitted" ])

china_top <- as.numeric(china_co2_2014)

china_pct_ea <- china_top / east_asia_top

china_pct_ea <- paste(round(100*china_pct_ea, 2), "%", sep ="")

# Resources Used:
# http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
# https://plot.ly/r/line-and-scatter/
# https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf
# https://blog.exploratory.io/filter-data-with-dplyr-76cf5f1a258e
# https://plot.ly/r/animations/
