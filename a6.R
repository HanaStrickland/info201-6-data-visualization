# Questions:
# Is "most recent" 2015? Can I change the label to 2015?

emissions_data <- read.csv("data/WDI_emissions_Data.csv", stringsAsFactors = FALSE)
country_data <- read.csv("data/WDI_selected_Data.csv", stringsAsFactors = FALSE, na.strings = "..")




### Question: How has co2 emissions changed over time in relation to GDP? ###
filter_co2_emissions_kt <- 
  emissions_data %>% 
  filter(Series.Code == "EN.ATM.CO2E.KT") 
  
filter_gdp <- 
  country_data %>% 
  filter(Series.Code == "NY.GDP.MKTP.CD")

co2_and_gdp <- left_join(filter_co2_emissions_kt, filter_gdp, by = c("Country.Code"))

total_co2_emissions_kt <- colSums(filter_co2_emissions_kt[,-1:-2], na.rm = TRUE) 

total_co2_df <- as.data.frame(total_co2_emissions_kt) # makes total co2 a data frame


total_co2_df <- total_co2_df %>% 
  mutate(year = rownames(total_co2_df))



total_gdp <- colSums(filter_gdp[,-1:-4], na.rm = TRUE) 


total_gdp_df <- as.data.frame(total_gdp)

total_gdp_df <- total_gdp_df %>% 
  mutate(year = rownames(total_gdp_df))


total_gdp_df$year <- c("YR1998", "YR1999", "YR2000", "YR2001", "YR2002", "YR2003", "YR2004", "YR2005", "YR2006",
                       "YR2007", "YR2008", "YR2009", "YR2010", "YR2011", "YR2012", "YR2013", "YR2014", "Most_Recent")


both_co2_gdp <- left_join(total_co2_df, total_gdp_df, by = "year")

change_most_recent <- both_co2_gdp[both_co2_gdp$year == "Most_Recent", "year"] <- "YR2015"

both_co2_gdp <- both_co2_gdp %>% 
  select(year, total_co2_emissions_kt, total_gdp)


ggplot(data = both_co2_gdp) +
  geom_point(mapping = aes(x = year , y = total_co2_emissions_kt, color = total_gdp ))
