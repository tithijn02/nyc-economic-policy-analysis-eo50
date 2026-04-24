# Install the required the libraries
install.packages(c("socviz", "maps", "sf", "scales", "viridis"))

# Load the required libraries for data manipulation, visualization, including map data etc.
library(tidyverse)
library(socviz)
library(maps)
library(ggthemes)
library(ggplot2)
library(sf)
library(scales)
library(viridis)

# In this step the data is loaded and preprocessed, then convert the approval date to Date format. 
#After the conversion, clean the data by removing NA values and create Year and Month columns.
data <- read.csv("e050.csv")
data$EO50.Approval.Date <- as.Date(data$EO50.Approval.Date, format="%m/%d/%Y %I:%M:%S %p")
data <- data %>% 
  drop_na(EO50.Approval.Date, Latitude, Longitude, Borough) %>%
  mutate(Year = format(EO50.Approval.Date, "%Y"),
         Month = format(EO50.Approval.Date, "%Y-%m"))

# View the updated EO50.csv dataset
View(data)

# Load supporting dataset: nyc_boroughs.geojson and nyc_census_tracts.csv and read them
nyc_boroughs <- st_read("Borough Boundaries.geojson")
census <- read.csv("nyc_census_tracts.csv")

#Display the supporting datasets
View(nyc_boroughs)
View(census)

# Standardize borough names for all datasets, in order to merge the 3 datasets 
# Since, Borough was found common amongst the datasets.
data$Borough <- toupper(data$Borough)
census$Borough <- toupper(census$Borough)
nyc_boroughs$boro_name <- toupper(nyc_boroughs$boro_name)

# Aggregate census data by borough
census_borough <- census %>%
  group_by(Borough) %>%
  summarise(
    median_income = mean(Income, na.rm = TRUE),
    employment_rate = mean(Employed/TotalPop, na.rm = TRUE),
    poverty_rate = mean(Poverty, na.rm = TRUE)
  )

# View census_borough
View(census_borough)

# Create final merged dataset
final_data <- data %>%
  left_join(census_borough, by = "Borough") %>%
  left_join(nyc_boroughs, by = c("Borough" = "boro_name"))

# Check the count of the rows 
print(nrow(final_data))

# View the final data
View(final_data)

# Drop rows with NA values
cleaned_data <- final_data %>%
  drop_na()

# Check if the data is cleaned or not with the final_data 
print(nrow(cleaned_data))

# View the cleaned data
View(cleaned_data)


# Visualization 1: Spatial Distribution
spatial_plot <- ggplot() +
  # Adding borough boundaries
  geom_sf(data = st_as_sf(nyc_boroughs)) +
  # Adding points for EO50 certificates
  geom_point(data = cleaned_data, 
             aes(x = Longitude, y = Latitude, 
                 color = employment_rate,
                 size = poverty_rate),
             alpha = 0.6) +
  # Adding labels for the boroughs
  geom_sf_text(data = st_as_sf(nyc_boroughs),
               aes(label = boro_name),
               size = 2.5,
               fontface = "bold",
               check_overlap = TRUE) +
  # Setting the colour scheme for employment rate
  scale_color_viridis_c(name = "Employment Rate") +
  # Setting the size scale for poverty rate
  scale_size_continuous(name = "Poverty Rate") +
  # Use minimal theme(ggthemes)
  theme_minimal() +
  # Adding title and subtitle to the graph
  labs(
    title = "Spatial Distribution of EO50 Certificates",
    subtitle = "Color indicates employment rate, point-size indicates poverty rate"
  ) +
  # Adjusting theme elements
  theme(
    legend.position = "right",
    text = element_text(size = 10)
  )

# Printing the plot 
print(spatial_plot)


# Visualization 2: Time Series 

# Calculate year-over-year growth rates using reframe
growth_data <- cleaned_data %>%
  group_by(Year, Borough) %>%
  reframe(
    EO50_Count = n(),
    MedianIncome = median_income
  ) %>%
  group_by(Borough) %>%
  arrange(Year) %>%
  mutate(
    Growth_Rate = (EO50_Count - lag(EO50_Count))/lag(EO50_Count) * 100
  )

# Create the enhanced time-series plot
ggplot(growth_data) +
  # Adding lines for EO50 approval counts
  geom_line(aes(x = Year, 
                y = EO50_Count, 
                color = Borough,
                group = Borough),
            size = 1.2) +
  # Adding points for EO50 counts
  geom_point(aes(x = Year,
                 y = EO50_Count,
                 color = Borough,
                 size = MedianIncome),
             alpha = 0.7) +
  # Add milestone markers
  geom_vline(xintercept = "2022",
             linetype = "dotted",
             alpha = 0.5) +
  annotate("text",
           x = "2022",
           y = max(growth_data$EO50_Count),
           label = "Policy Update",
           angle = 90,
           vjust = 1.5,
           size = 3) +
  # Setting size scale for median income
  scale_size_continuous(
    name = "Median Income ($)",
    range = c(3, 8)
  ) +
  # Setting Theme and labels
  theme_minimal() +
  scale_color_viridis_d() +
  labs(
    title = "Temporal Diffusion of EO50 Certificates by Borough",
    subtitle = "Point size indicates Median Income\nPolicy update marker coincides with peak period",
    x = "Year",
    y = "Number of EO50 Approvals",
    color = "Borough"
  ) +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    legend.position = "right",
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95")
  )

