


# Install the ggplot2 package.
install.packages('ggplot2')

# Import the ggplot2 library.
library(ggplot2)
# Load the tidyverse library.
library(tidyverse)

install.packages("openxlsx")
library(openxlsx)
install.packages("xlsx")
library(xlsx)

##### Import 4 spead sheets

# Ensure openxlsx is installed
install.packages("openxlsx")

# Load the package
library(openxlsx)

# Use the full function call to avoid ambiguity
israelim <- openxlsx::read.xlsx("Israel_Missile_Capabilities.xlsx", sheet = 1)


# View top 6 rows.
head(israelim)
summary(israelim)
head(israelim)


# Import the data set (Iran_Missile_Capabilities.xlsx)
# Set your working directory.
# Use the full function call to avoid ambiguity
iranm <- openxlsx::read.xlsx("Iran_Missile_Capabilities.xlsx", sheet = 1)

# View top 6 rows.
head(iranm)
summary(iranm)
head(iranm)

# Import the data set (Russia_Missile_Capabilities.xlsx)
# Set your working directory.
russiam <- openxlsx::read.xlsx('Russia_Missile_Capabilities.xlsx', sheet = 1)

# View top 6 rows.
head(russiam)
summary(russiam)
head(russiam)

# Import the data set (China_Missile_Capabilities.xlsx)
# Set your working directory.
chinam <- openxlsx::read.xlsx('China_Missile_Capabilities.xlsx', sheet = 1)

# View top 6 rows.
head(chinam)
summary(chinam)
head(chinam)

# Import the data set (USA_Missile_Capabilities.xlsx)
# Set your working directory.
usam  <- openxlsx::read.xlsx('USA_Missile_Capabilities.xlsx', sheet = 1)

# View top 6 rows.
head(usam)
summary(usam)
head(usam)

####

# Load necessary package
library(ggplot2)

# Create dataset
missile_data <- data.frame(
  Country = c("USA", "Russia", "China", "Iran", "Israel", "South Korea"),
  AvgPayloadKG = c(1200, 1500, 1300, 700, 1000, 500),
  AvgRangeKM = c(10000, 9500, 8500, 1000, 2000, 800)
)

# Plot using ggplot2
ggplot(missile_data, aes(x = AvgPayloadKG, y = AvgRangeKM, label = Country)) +
  geom_point(color = "purple", size = 4) +
  geom_text(vjust = -0.5, hjust = 0.5) +
  labs(
    title = "Missile Capabilities by Country: Payload vs. Range",
    x = "Average Payload (kg)",
    y = "Average Range (km)"
  ) +
  theme_minimal()
#################

# Create dataset
missile_data <- data.frame(
  Country = c("USA", "Russia", "China", "Iran", "Israel", "South Korea",
              "USA", "Russia", "Israel"),
  AvgPayloadKG = c(1200, 1500, 1300, 700, 1000, 500, 0, 0, 0),  # 0 payload for interceptors
  AvgRangeKM = c(10000, 9500, 8500, 1000, 2000, 800, 200, 2400, 300),  # Example ranges
  Type = c("Ballistic", "Ballistic", "Ballistic", "Ballistic", "Ballistic", "Ballistic",
           "Interceptor", "Interceptor", "Interceptor")
)


# Create dataset
usa_missiles <- data.frame(
  Missile = c("Minuteman III", "Trident II", "ATACMS", "Sentinel"),
  Payload_KG = c(1100, 2800, 500, 1100),
  Range_KM = c(12000, 12000, 300, 12000)
)

# Melt the data
usa_long <- melt(usa_missiles, 
                 id.vars = "Missile", 
                 variable.name = "Metric", 
                 value.name = "Value")


##########



# Load necessary package
library(ggplot2)

# Create dataset
missile_data <- data.frame(
  Country = c("USA", "Russia", "China", "Iran", "Israel", "South Korea",
              "USA", "Russia", "Israel"),
  AvgPayloadKG = c(1200, 1500, 1300, 700, 1000, 500, 0, 0, 0),  # 0 payload for interceptors
  AvgRangeKM = c(10000, 9500, 8500, 1000, 2000, 800, 200, 2400, 300),  # Example ranges
  Type = c("Ballistic", "Ballistic", "Ballistic", "Ballistic", "Ballistic", "Ballistic",
           "Interceptor", "Interceptor", "Interceptor")
)


# Filter ballistic missiles
ballistic_data <- subset(missile_data, Type == "Ballistic")

ggplot(ballistic_data, aes(x = AvgPayloadKG, y = AvgRangeKM, label = Country)) +
  geom_point(color = "darkred", size = 4) +
  geom_text(vjust = -0.5) +
  labs(
    title = "Ballistic Missiles: Payload vs. Range",
    x = "Average Payload (kg)",
    y = "Average Range (km)"
  ) +
  theme_minimal()

####


# Filter interceptors
interceptor_data <- subset(missile_data, Type == "Interceptor")

ggplot(interceptor_data, aes(x = AvgPayloadKG, y = AvgRangeKM, label = Country)) +
  geom_point(color = "blue", size = 4) +
  geom_text(vjust = -0.5) +
  labs(
    title = "Interceptors: Payload vs. Range",
    x = "Average Payload (kg)",
    y = "Average Range (km)"
  ) +
  theme_minimal()

#### iran 
library(tidyverse)

# Define the Israel missile data
israel <- data.frame(
  Missile = c("Jericho I", "Jericho II", "Jericho III", "LORA", "Delilah", "Rampage"),
  Payload = c(500, 1000, 1300, 570, 50, 570),
  Range = c(500, 1500, 6500, 400, 250, 300)
)

# Convert to long format
israel_long <- israel %>%
  pivot_longer(cols = c(Payload, Range),
               names_to = "Metric",
               values_to = "Value")

# Create grouped bar chart
ggplot(israel_long, aes(x = Missile, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Israel Ballistic Missiles: Payload and Range",
    x = "Missile System",
    y = "Value (kg or km)",
    fill = "Metric"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --- USA Ballistic Missiles ---

library(tidyverse)

library(tidyverse)

# Define USA missile data
usa <- data.frame(
  Missile = c("Minuteman III", "Trident II D5", "Atlas", "Polaris A-3", "Pershing II", "Peacekeeper"),
  Payload = c(1150, 2800, 1500, 800, 400, 4000),
  Range = c(13000, 12000, 14500, 4600, 1800, 14800)
)

# Convert to long format
usa_long <- usa %>%
  pivot_longer(cols = c(Payload, Range),
               names_to = "Metric",
               values_to = "Value")

# Create grouped bar chart
ggplot(usa_long, aes(x = Missile, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "U.S. Ballistic Missiles: Payload and Range",
    x = "Missile System",
    y = "Value (kg or km)",
    fill = "Metric"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# --- Russia Ballistic Missiles ---
russia_missiles <- data.frame(
  Missile = c("Iskander", "RS-24 Yars", "RS-28 Sarmat", "DF-41", "Topol-M"),
  Payload_KG = c(700, 1500, 2000, 1500, 1200),
  Range_KM = c(500, 10500, 10000, 12000, 11000)
)
russia_long <- melt(russia_missiles, id.vars = "Missile", variable.name = "Metric", value.name = "Value")

# --- South Korea Ballistic Missiles ---
skorea_missiles <- data.frame(
  Missile = c("Hyunmoo-2A", "Hyunmoo-2B", "Hyunmoo-2C"),
  Payload_KG = c(500, 700, 1000),
  Range_KM = c(300, 500, 800)
)
skorea_long <- melt(skorea_missiles, id.vars = "Missile", variable.name = "Metric", value.name = "Value")

############
# USA Plot
ggplot(usa_long, aes(x = Missile, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "USA Ballistic Missiles: Payload and Range", x = "Missile", y = "Value (kg or km)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Russia Plot
ggplot(russia_long, aes(x = Missile, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Russia Ballistic Missiles: Payload and Range", x = "Missile", y = "Value (kg or km)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# South Korea Plot
ggplot(skorea_long, aes(x = Missile, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "South Korea Ballistic Missiles: Payload and Range", x = "Missile", y = "Value (kg or km)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

######
####### Created a bar plot for the countries involved.
# it is important to understand the population desity of a country one wants to war with.

# Sample data
countries <- c("Israel", "USA", "Iran", "Russia", "China", "North Korea")
values <- c(85, 300, 92, 150, 280, 70)  # Replace these numbers with your actual data

# Create the bar plot
barplot(
  height = values,
  names.arg = countries,
  col = "skyblue",
  main = "Comparison of Countries",
  xlab = "Country",
  ylab = "Value",
  ylim = c(0, max(values) + 50)
)

# Add value labels on top of bars
text(x = seq_along(values), y = values, label = values, pos = 3)

### Fixed plot

# Country names
countries <- c("Israel", "USA", "Iran", "Russia", "China", "North Korea")

# Approximate population values in millions (as of 2024 estimates)
population <- c(9.8, 332, 89, 143, 1412, 26)

# Create the barplot
barplot(height = population,
        names.arg = countries,
        col = "skyblue",
        main = "Population Comparison by Country",
        ylab = "Population (in millions)",
        las = 2,           # Rotate axis labels for readability
        cex.names = 0.8)   # Adjust label size

#### Removed China
# Country names (without China)
countries <- c("Israel", "USA", "Iran", "Russia", "North Korea")

# Corresponding population values in millions
population <- c(9.8, 332, 89, 143, 26)

# Create the barplot with vertical labels
barplot(height = population,
        names.arg = countries,
        col = "skyblue",
        main = "Population Comparison by Country",
        ylab = "Population (in millions)",
        las = 2,           # Rotate x-axis labels to vertical
        cex.names = 0.8)   # Optional: adjust label size for fit



#####################################
### Animated version of the barplot

install.packages("gifski")
install.packages("gganimate", dependencies = TRUE)

library(ggplot2)
library(gganimate)
library(dplyr)

# Sample dataset with time progression
data <- data.frame(
  country = rep(c("Israel", "USA", "Russia", "China", "North Korea"), each = 5),
  year = rep(1:5, times = 5),
  value = c(
    seq(10, 85, length.out = 5),  # Israel
    seq(20, 300, length.out = 5), # USA
    seq(5, 150, length.out = 5),  # Russia
    seq(15, 280, length.out = 5), # China
    seq(8, 70, length.out = 5)    # North Korea
  )
)

# Assign colors
data$color <- ifelse(data$country %in% c("USA", "Israel"), "skyblue", "orange")

# Animated bar plot
p <- ggplot(data, aes(x = country, y = value, fill = color)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +
  labs(title = 'Year: {frame_time}', y = "Value", x = "Country") +
  transition_time(year) +
  ease_aes('linear')

# Render the animation
animate(p, width = 800, height = 600, fps = 10, duration = 5, renderer = gifski_renderer())


###########


# Define the data
country <- c("Iran", "Israel")
tertiary_pct <- c(44.9, 50.3)

df <- data.frame(country, tertiary_pct)

# Create bar plot
ggplot(df, aes(x = country, y = tertiary_pct, fill = country)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = c("Iran" = "red", "Israel" = "blue")) +
  labs(
    title = "Tertiary Education Attainment (25–64 years old)",
    x = "Country",
    y = "Percentage of Adults with Tertiary Education"
  ) +
  theme_minimal() +
  geom_text(aes(label = paste0(tertiary_pct, "%")), vjust = -0.5)

#

# Load packages
library(ggplot2)

# Create data frame
country <- c("Iran - Arts", "Iran - Engineering", "Israel - Arts", "Israel - ICT (STEM)")
percentage <- c(8.2, 31.0, 6.4, 6.9)
df <- data.frame(country, percentage = percentage)

# Define colors: Arts = red, STEM = blue
df$color <- ifelse(grepl("Arts", df$country), "firebrick", "steelblue")

# Plot
ggplot(df, aes(x = country, y = percentage, fill = color)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +
  labs(
    title = "Tertiary Graduates by Field: Arts vs STEM",
    x = "",
    y = "Percentage of Graduates"
  ) +
  theme_minimal() +
  geom_text(aes(label = paste0(percentage, "%")), vjust = -0.5)

###

library(ggplot2)
library(dplyr)

df <- data.frame(
  country = c("Iran", "Iran", "Iran", "Iran", "Israel", "Israel"),
  field = c("Engineering & Sciences", "Medical & Health", 
            "Humanities & Arts", "Education", "STEM", "Other"),
  percentage = c(25.5, 24.2, 8.2, 13.4, 28, 72)
)

# Plot grouped bar chart
ggplot(df, aes(x = country, y = percentage, fill = field)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Tertiary Education Field Distribution – Iran vs Israel",
       x = "Country", y = "Percentage of Graduates") +
  theme_minimal()
####

library(ggplot2)

# Data frame
df <- data.frame(
  country = rep(c("Iran", "Israel"), each = 2),
  field = rep(c("STEM", "Non-STEM"), times = 2),
  percentage = c(49.7, 50.3, 28, 72)
)

# Plot
ggplot(df, aes(x = country, y = percentage, fill = field)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("STEM" = "steelblue", "Non-STEM" = "firebrick")) +
  labs(
    title = "STEM vs Non-STEM Tertiary Education (Iran vs Israel)",
    y = "Percentage of Graduates",
    x = "Country"
  ) +
  theme_minimal() +
  geom_text(aes(label = paste0(percentage, "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white", size = 4)

######### COMBINED GRAPHS




# 1. Install reshape2 (only once)
install.packages("reshape2")

# 2. Load the package
library(reshape2)

# 3. Create your dataset
usa_missiles <- data.frame(
  Missile = c("Minuteman III", "Trident II", "ATACMS", "Sentinel"),
  Payload_KG = c(1100, 2800, 500, 1100),
  Range_KM = c(12000, 12000, 300, 12000)
)

# 4. Melt the data
usa_long <- melt(usa_missiles, 
                 id.vars = "Missile", 
                 variable.name = "Metric", 
                 value.name = "Value")

# 5. View the result
print(usa_long)

# 1. Install tidyverse
install.packages("tidyverse")

# 2. Load tidyverse (includes tidyr)
library(tidyverse)

# 3. Reshape the data
usa_long <- usa_missiles %>%
  pivot_longer(cols = -Missile,
               names_to = "Metric",
               values_to = "Value")

# 4. View result
print(usa_long)


# Install if needed (only once)
install.packages("reshape2")

# Load the package quietly
suppressPackageStartupMessages(library(reshape2))

# Install and load required packages
install.packages("tidyverse")  # Only once
suppressPackageStartupMessages(library(tidyverse))

# Create dataset
usa_missiles <- data.frame(
  Missile = c("Minuteman III", "Trident II", "ATACMS", "Sentinel"),
  Payload_KG = c(1100, 2800, 500, 1100),
  Range_KM = c(12000, 12000, 300, 12000)
)

# Reshape data to long format
usa_long <- usa_missiles %>%
  pivot_longer(cols = c(Payload_KG, Range_KM), 
               names_to = "Metric", 
               values_to = "Value")

# Create grouped bar chart
ggplot(usa_long, aes(x = Missile, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "USA Missile Payload and Range",
       x = "Missile",
       y = "Value",
       fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#####
# Install and load required packages
install.packages("tidyverse")  # Only once
suppressPackageStartupMessages(library(tidyverse))

# Create dataset
usa_missiles <- data.frame(
  Missile = c("Minuteman III", "Trident II", "ATACMS", "Sentinel"),
  Payload_KG = c(1100, 2800, 500, 1100),
  Range_KM = c(12000, 12000, 300, 12000)
)

# Reshape data to long format
usa_long <- usa_missiles %>%
  pivot_longer(cols = c(Payload_KG, Range_KM), 
               names_to = "Metric", 
               values_to = "Value")

# Create grouped bar chart
ggplot(usa_long, aes(x = Missile, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "USA Missile Payload and Range",
       x = "Missile",
       y = "Value",
       fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####

# Install and load required packages
install.packages("tidyverse")  # Only once
suppressPackageStartupMessages(library(tidyverse))

# Create dataset for Russian missiles
russia_missiles <- data.frame(
  Missile = c("Topol-M", "RS-28 Sarmat", "Iskander-M", "Kalibr"),
  Payload_KG = c(1200, 10000, 700, 450),
  Range_KM = c(11000, 18000, 500, 2500)
)

# Reshape data to long format
russia_long <- russia_missiles %>%
  pivot_longer(cols = c(Payload_KG, Range_KM), 
               names_to = "Metric", 
               values_to = "Value")

# Create grouped bar chart
ggplot(russia_long, aes(x = Missile, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Russian Missile Payload and Range",
       x = "Missile",
       y = "Value",
       fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angl
                                   
######
install.packages("tidyverse")
suppressPackageStartupMessages(library(tidyverse))


# Create dataset for Russian missiles
russia_missiles <- data.frame(
  Missile = c("Topol-M", "RS-28 Sarmat", "Iskander-M", "Kalibr"),
  Payload_KG = c(1200, 10000, 700, 450),
  Range_KM = c(11000, 18000, 500, 2500)
)

# Reshape data to long format using pipes
russia_long <- russia_missiles %>%
  pivot_longer(cols = c(Payload_KG, Range_KM), 
               names_to = "Metric", 
               values_to = "Value")

# Create grouped bar chart
ggplot(russia_long, aes(x = Missile, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Russian Missile Payload and Range",
       x = "Missile",
       y = "Value",
       fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### russia and USA

# Install and load tidyverse
install.packages("tidyverse")  # Run only once
suppressPackageStartupMessages(library(tidyverse))

# USA missile data
usa_missiles <- data.frame(
  Missile = c("Minuteman III", "Trident II", "ATACMS", "Sentinel"),
  Payload_KG = c(1100, 2800, 500, 1100),
  Range_KM = c(12000, 12000, 300, 12000),
  Country = "USA"
)

# Russia missile data
russia_missiles <- data.frame(
  Missile = c("Topol-M", "RS-28 Sarmat", "Iskander-M", "Kalibr"),
  Payload_KG = c(1200, 10000, 700, 450),
  Range_KM = c(11000, 18000, 500, 2500),
  Country = "Russia"
)

# Combine both datasets
all_missiles <- bind_rows(usa_missiles, russia_missiles)

# Reshape for plotting
missiles_long <- all_missiles %>%
  pivot_longer(cols = c(Payload_KG, Range_KM),
               names_to = "Metric",
               values_to = "Value")

# Plot: grouped bar chart with facet by Country
ggplot(missiles_long, aes(x = Missile, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Country, scales = "free_x") +
  labs(title = "Comparison of USA and Russia Missile Payload & Range",
       x = "Missile",
       y = "Value",
       fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#########


# Install and load tidyverse
install.packages("tidyverse")  # Only once
suppressPackageStartupMessages(library(tidyverse))

# Iran missile data
iran_missiles <- data.frame(
  Missile = c("Shahab-3", "Sejjil", "Fateh-110"),
  Payload_KG = c(1000, 650, 500),
  Range_KM = c(2000, 2500, 300),
  Country = "Iran"
)

# Israel missile data
israel_missiles <- data.frame(
  Missile = c("Jericho II", "Jericho III", "LORA"),
  Payload_KG = c(1000, 1000, 570),
  Range_KM = c(1500, 6500, 400),
  Country = "Israel"
)

# Combine both datasets
all_missiles <- bind_rows(iran_missiles, israel_missiles)

# Reshape to long format for ggplot
missiles_long <- all_missiles %>%
  pivot_longer(cols = c(Payload_KG, Range_KM),
               names_to = "Metric",
               values_to = "Value")

# Plot: grouped bar chart with facet by Country
ggplot(missiles_long, aes(x = Missile, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Country, scales = "free_x") +
  labs(title = "Comparison of Iran and Israel Missile Payload & Range",
       x = "Missile",
       y = "Value",
       fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))







# 1. Install reshape2 (only once)
install.packages("reshape2")

# 2. Load the package
library(reshape2)

# 3. Create your dataset
usa_missiles <- data.frame(
  Missile = c("Minuteman III", "Trident II", "ATACMS", "Sentinel"),
  Payload_KG = c(1100, 2800, 500, 1100),
  Range_KM = c(12000, 12000, 300, 12000)
)

# 4. Melt the data
usa_long <- melt(usa_missiles, 
                 id.vars = "Missile", 
                 variable.name = "Metric", 
                 value.name = "Value")

# 5. View the result
print(usa_long)

# 1. Install tidyverse
install.packages("tidyverse")

# 2. Load tidyverse (includes tidyr)
library(tidyverse)

# 3. Reshape the data
usa_long <- usa_missiles %>%
  pivot_longer(cols = -Missile,
               names_to = "Metric",
               values_to = "Value")

# 4. View result
print(usa_long)


# Install if needed (only once)
install.packages("reshape2")

# Load the package quietly
suppressPackageStartupMessages(library(reshape2))

# Install and load required packages
install.packages("tidyverse")  # Only once
suppressPackageStartupMessages(library(tidyverse))

# Create dataset
usa_missiles <- data.frame(
  Missile = c("Minuteman III", "Trident II", "ATACMS", "Sentinel"),
  Payload_KG = c(1100, 2800, 500, 1100),
  Range_KM = c(12000, 12000, 300, 12000)
)

# Reshape data to long format
usa_long <- usa_missiles %>%
  pivot_longer(cols = c(Payload_KG, Range_KM), 
               names_to = "Metric", 
               values_to = "Value")

# Create grouped bar chart
ggplot(usa_long, aes(x = Missile, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "USA Missile Payload and Range",
       x = "Missile",
       y = "Value",
       fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#####
# Install and load required packages
install.packages("tidyverse")  # Only once
suppressPackageStartupMessages(library(tidyverse))

# Create dataset
usa_missiles <- data.frame(
  Missile = c("Minuteman III", "Trident II", "ATACMS", "Sentinel"),
  Payload_KG = c(1100, 2800, 500, 1100),
  Range_KM = c(12000, 12000, 300, 12000)
)

# Reshape data to long format
usa_long <- usa_missiles %>%
  pivot_longer(cols = c(Payload_KG, Range_KM), 
               names_to = "Metric", 
               values_to = "Value")

# Create grouped bar chart
ggplot(usa_long, aes(x = Missile, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "USA Missile Payload and Range",
       x = "Missile",
       y = "Value",
       fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####

# Install and load required packages
install.packages("tidyverse")  # Only once
suppressPackageStartupMessages(library(tidyverse))

# Create dataset for Russian missiles
russia_missiles <- data.frame(
  Missile = c("Topol-M", "RS-28 Sarmat", "Iskander-M", "Kalibr"),
  Payload_KG = c(1200, 10000, 700, 450),
  Range_KM = c(11000, 18000, 500, 2500)
)

# Reshape data to long format
russia_long <- russia_missiles %>%
  pivot_longer(cols = c(Payload_KG, Range_KM), 
               names_to = "Metric", 
               values_to = "Value")

# Create grouped bar chart
ggplot(russia_long, aes(x = Missile, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Russian Missile Payload and Range",
       x = "Missile",
       y = "Value",
       fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angl
                                   
######
install.packages("tidyverse")
suppressPackageStartupMessages(library(tidyverse))


# Create dataset for Russian missiles
russia_missiles <- data.frame(
  Missile = c("Topol-M", "RS-28 Sarmat", "Iskander-M", "Kalibr"),
  Payload_KG = c(1200, 10000, 700, 450),
  Range_KM = c(11000, 18000, 500, 2500)
)

# Reshape data to long format using pipes
russia_long <- russia_missiles %>%
  pivot_longer(cols = c(Payload_KG, Range_KM), 
               names_to = "Metric", 
               values_to = "Value")

# Create grouped bar chart
ggplot(russia_long, aes(x = Missile, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Russian Missile Payload and Range",
       x = "Missile",
       y = "Value",
       fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### russia and USA

# Install and load tidyverse
install.packages("tidyverse")  # Run only once
suppressPackageStartupMessages(library(tidyverse))

# USA missile data
usa_missiles <- data.frame(
  Missile = c("Minuteman III", "Trident II", "ATACMS", "Sentinel"),
  Payload_KG = c(1100, 2800, 500, 1100),
  Range_KM = c(12000, 12000, 300, 12000),
  Country = "USA"
)

# Russia missile data
russia_missiles <- data.frame(
  Missile = c("Topol-M", "RS-28 Sarmat", "Iskander-M", "Kalibr"),
  Payload_KG = c(1200, 10000, 700, 450),
  Range_KM = c(11000, 18000, 500, 2500),
  Country = "Russia"
)

# Combine both datasets
all_missiles <- bind_rows(usa_missiles, russia_missiles)

# Reshape for plotting
missiles_long <- all_missiles %>%
  pivot_longer(cols = c(Payload_KG, Range_KM),
               names_to = "Metric",
               values_to = "Value")

# Plot: grouped bar chart with facet by Country
ggplot(missiles_long, aes(x = Missile, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Country, scales = "free_x") +
  labs(title = "Comparison of USA and Russia Missile Payload & Range",
       x = "Missile",
       y = "Value",
       fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#########


# Install and load tidyverse
install.packages("tidyverse")  # Only once
suppressPackageStartupMessages(library(tidyverse))

# Iran missile data
iran_missiles <- data.frame(
  Missile = c("Shahab-3", "Sejjil", "Fateh-110"),
  Payload_KG = c(1000, 650, 500),
  Range_KM = c(2000, 2500, 300),
  Country = "Iran"
)

# Israel missile data
israel_missiles <- data.frame(
  Missile = c("Jericho II", "Jericho III", "LORA"),
  Payload_KG = c(1000, 1000, 570),
  Range_KM = c(1500, 6500, 400),
  Country = "Israel"
)

# Combine both datasets
all_missiles <- bind_rows(iran_missiles, israel_missiles)

# Reshape to long format for ggplot
missiles_long <- all_missiles %>%
  pivot_longer(cols = c(Payload_KG, Range_KM),
               names_to = "Metric",
               values_to = "Value")

# Plot: grouped bar chart with facet by Country
ggplot(missiles_long, aes(x = Missile, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Country, scales = "free_x") +
  labs(title = "Comparison of Iran and Israel Missile Payload & Range",
       x = "Missile",
       y = "Value",
       fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





# 1. Install reshape2 (only once)
install.packages("reshape2")

# 2. Load the package
library(reshape2)

# 3. Create your dataset
usa_missiles <- data.frame(
  Missile = c("Minuteman III", "Trident II", "ATACMS", "Sentinel"),
  Payload_KG = c(1100, 2800, 500, 1100),
  Range_KM = c(12000, 12000, 300, 12000)
)

# 4. Melt the data
usa_long <- melt(usa_missiles, 
                 id.vars = "Missile", 
                 variable.name = "Metric", 
                 value.name = "Value")

# 5. View the result
print(usa_long)

# 1. Install tidyverse
install.packages("tidyverse")

# 2. Load tidyverse (includes tidyr)
library(tidyverse)

# 3. Reshape the data
usa_long <- usa_missiles %>%
  pivot_longer(cols = -Missile,
               names_to = "Metric",
               values_to = "Value")

# 4. View result
print(usa_long)


# Install if needed (only once)
install.packages("reshape2")

# Load the package quietly
suppressPackageStartupMessages(library(reshape2))

# Install and load required packages
install.packages("tidyverse")  # Only once
suppressPackageStartupMessages(library(tidyverse))

# Create dataset
usa_missiles <- data.frame(
  Missile = c("Minuteman III", "Trident II", "ATACMS", "Sentinel"),
  Payload_KG = c(1100, 2800, 500, 1100),
  Range_KM = c(12000, 12000, 300, 12000)
)

# Reshape data to long format
usa_long <- usa_missiles %>%
  pivot_longer(cols = c(Payload_KG, Range_KM), 
               names_to = "Metric", 
               values_to = "Value")

# Create grouped bar chart
ggplot(usa_long, aes(x = Missile, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "USA Missile Payload and Range",
       x = "Missile",
       y = "Value",
       fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#####
# Install and load required packages
install.packages("tidyverse")  # Only once
suppressPackageStartupMessages(library(tidyverse))

# Create dataset
usa_missiles <- data.frame(
  Missile = c("Minuteman III", "Trident II", "ATACMS", "Sentinel"),
  Payload_KG = c(1100, 2800, 500, 1100),
  Range_KM = c(12000, 12000, 300, 12000)
)

# Reshape data to long format
usa_long <- usa_missiles %>%
  pivot_longer(cols = c(Payload_KG, Range_KM), 
               names_to = "Metric", 
               values_to = "Value")

# Create grouped bar chart
ggplot(usa_long, aes(x = Missile, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "USA Missile Payload and Range",
       x = "Missile",
       y = "Value",
       fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####

# Install and load required packages
install.packages("tidyverse")  # Only once
suppressPackageStartupMessages(library(tidyverse))

# Create dataset for Russian missiles
russia_missiles <- data.frame(
  Missile = c("Topol-M", "RS-28 Sarmat", "Iskander-M", "Kalibr"),
  Payload_KG = c(1200, 10000, 700, 450),
  Range_KM = c(11000, 18000, 500, 2500)
)

# Reshape data to long format
russia_long <- russia_missiles %>%
  pivot_longer(cols = c(Payload_KG, Range_KM), 
               names_to = "Metric", 
               values_to = "Value")

# Create grouped bar chart
ggplot(russia_long, aes(x = Missile, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Russian Missile Payload and Range",
       x = "Missile",
       y = "Value",
       fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angl
                                   
######
install.packages("tidyverse")
suppressPackageStartupMessages(library(tidyverse))


# Create dataset for Russian missiles
russia_missiles <- data.frame(
  Missile = c("Topol-M", "RS-28 Sarmat", "Iskander-M", "Kalibr"),
  Payload_KG = c(1200, 10000, 700, 450),
  Range_KM = c(11000, 18000, 500, 2500)
)

# Reshape data to long format using pipes
russia_long <- russia_missiles %>%
  pivot_longer(cols = c(Payload_KG, Range_KM), 
               names_to = "Metric", 
               values_to = "Value")

# Create grouped bar chart
ggplot(russia_long, aes(x = Missile, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Russian Missile Payload and Range",
       x = "Missile",
       y = "Value",
       fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### russia and USA

# Install and load tidyverse
install.packages("tidyverse")  # Run only once
suppressPackageStartupMessages(library(tidyverse))

# USA missile data
usa_missiles <- data.frame(
  Missile = c("Minuteman III", "Trident II", "ATACMS", "Sentinel"),
  Payload_KG = c(1100, 2800, 500, 1100),
  Range_KM = c(12000, 12000, 300, 12000),
  Country = "USA"
)

# Russia missile data
russia_missiles <- data.frame(
  Missile = c("Topol-M", "RS-28 Sarmat", "Iskander-M", "Kalibr"),
  Payload_KG = c(1200, 10000, 700, 450),
  Range_KM = c(11000, 18000, 500, 2500),
  Country = "Russia"
)

# Combine both datasets
all_missiles <- bind_rows(usa_missiles, russia_missiles)

# Reshape for plotting
missiles_long <- all_missiles %>%
  pivot_longer(cols = c(Payload_KG, Range_KM),
               names_to = "Metric",
               values_to = "Value")

# Plot: grouped bar chart with facet by Country
ggplot(missiles_long, aes(x = Missile, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Country, scales = "free_x") +
  labs(title = "Comparison of USA and Russia Missile Payload & Range",
       x = "Missile",
       y = "Value",
       fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#########


# Install and load tidyverse
install.packages("tidyverse")  # Only once
suppressPackageStartupMessages(library(tidyverse))

# Iran missile data
iran_missiles <- data.frame(
  Missile = c("Shahab-3", "Sejjil", "Fateh-110"),
  Payload_KG = c(1000, 650, 500),
  Range_KM = c(2000, 2500, 300),
  Country = "Iran"
)

# Israel missile data
israel_missiles <- data.frame(
  Missile = c("Jericho II", "Jericho III", "LORA"),
  Payload_KG = c(1000, 1000, 570),
  Range_KM = c(1500, 6500, 400),
  Country = "Israel"
)

# Combine both datasets
all_missiles <- bind_rows(iran_missiles, israel_missiles)

# Reshape to long format for ggplot
missiles_long <- all_missiles %>%
  pivot_longer(cols = c(Payload_KG, Range_KM),
               names_to = "Metric",
               values_to = "Value")

# Plot: grouped bar chart with facet by Country
ggplot(missiles_long, aes(x = Missile, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Country, scales = "free_x") +
  labs(title = "Comparison of Iran and Israel Missile Payload & Range",
       x = "Missile",
       y = "Value",
       fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#####




# 1. Install reshape2 (only once)
install.packages("reshape2")

# 2. Load the package
library(reshape2)

# 3. Create your dataset
usa_missiles <- data.frame(
  Missile = c("Minuteman III", "Trident II", "ATACMS", "Sentinel"),
  Payload_KG = c(1100, 2800, 500, 1100),
  Range_KM = c(12000, 12000, 300, 12000)
)

# 4. Melt the data
usa_long <- melt(usa_missiles, 
                 id.vars = "Missile", 
                 variable.name = "Metric", 
                 value.name = "Value")

# 5. View the result
print(usa_long)

# 1. Install tidyverse
install.packages("tidyverse")

# 2. Load tidyverse (includes tidyr)
library(tidyverse)

# 3. Reshape the data
usa_long <- usa_missiles %>%
  pivot_longer(cols = -Missile,
               names_to = "Metric",
               values_to = "Value")

# 4. View result
print(usa_long)


# Install if needed (only once)
install.packages("reshape2")

# Load the package quietly
suppressPackageStartupMessages(library(reshape2))

# Install and load required packages
install.packages("tidyverse")  # Only once
suppressPackageStartupMessages(library(tidyverse))

# Create dataset
usa_missiles <- data.frame(
  Missile = c("Minuteman III", "Trident II", "ATACMS", "Sentinel"),
  Payload_KG = c(1100, 2800, 500, 1100),
  Range_KM = c(12000, 12000, 300, 12000)
)

# Reshape data to long format
usa_long <- usa_missiles %>%
  pivot_longer(cols = c(Payload_KG, Range_KM), 
               names_to = "Metric", 
               values_to = "Value")

# Create grouped bar chart
ggplot(usa_long, aes(x = Missile, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "USA Missile Payload and Range",
       x = "Missile",
       y = "Value",
       fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#####
# Install and load required packages
install.packages("tidyverse")  # Only once
suppressPackageStartupMessages(library(tidyverse))

# Create dataset
usa_missiles <- data.frame(
  Missile = c("Minuteman III", "Trident II", "ATACMS", "Sentinel"),
  Payload_KG = c(1100, 2800, 500, 1100),
  Range_KM = c(12000, 12000, 300, 12000)
)

# Reshape data to long format
usa_long <- usa_missiles %>%
  pivot_longer(cols = c(Payload_KG, Range_KM), 
               names_to = "Metric", 
               values_to = "Value")

# Create grouped bar chart
ggplot(usa_long, aes(x = Missile, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "USA Missile Payload and Range",
       x = "Missile",
       y = "Value",
       fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####

# Install and load required packages
install.packages("tidyverse")  # Only once
suppressPackageStartupMessages(library(tidyverse))

# Create dataset for Russian missiles
russia_missiles <- data.frame(
  Missile = c("Topol-M", "RS-28 Sarmat", "Iskander-M", "Kalibr"),
  Payload_KG = c(1200, 10000, 700, 450),
  Range_KM = c(11000, 18000, 500, 2500)
)

# Reshape data to long format
russia_long <- russia_missiles %>%
  pivot_longer(cols = c(Payload_KG, Range_KM), 
               names_to = "Metric", 
               values_to = "Value")

# Create grouped bar chart
ggplot(russia_long, aes(x = Missile, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Russian Missile Payload and Range",
       x = "Missile",
       y = "Value",
       fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angl
                                   
                                   ######
                                   install.packages("tidyverse")
                                   suppressPackageStartupMessages(library(tidyverse))
                                   
                                   
                                   # Create dataset for Russian missiles
                                   russia_missiles <- data.frame(
                                     Missile = c("Topol-M", "RS-28 Sarmat", "Iskander-M", "Kalibr"),
                                     Payload_KG = c(1200, 10000, 700, 450),
                                     Range_KM = c(11000, 18000, 500, 2500)
                                   )
                                   
                                   # Reshape data to long format using pipes
                                   russia_long <- russia_missiles %>%
                                     pivot_longer(cols = c(Payload_KG, Range_KM), 
                                                  names_to = "Metric", 
                                                  values_to = "Value")
                                   
                                   # Create grouped bar chart
                                   ggplot(russia_long, aes(x = Missile, y = Value, fill = Metric)) +
                                     geom_bar(stat = "identity", position = "dodge") +
                                     labs(title = "Russian Missile Payload and Range",
                                          x = "Missile",
                                          y = "Value",
                                          fill = "Metric") +
                                     theme_minimal() +
                                     theme(axis.text.x = element_text(angle = 45, hjust = 1))
                                   
                                   #### russia and USA
                                   
                                   # Install and load tidyverse
                                   install.packages("tidyverse")  # Run only once
                                   suppressPackageStartupMessages(library(tidyverse))
                                   
                                   # USA missile data
                                   usa_missiles <- data.frame(
                                     Missile = c("Minuteman III", "Trident II", "ATACMS", "Sentinel"),
                                     Payload_KG = c(1100, 2800, 500, 1100),
                                     Range_KM = c(12000, 12000, 300, 12000),
                                     Country = "USA"
                                   )
                                   
                                   # Russia missile data
                                   russia_missiles <- data.frame(
                                     Missile = c("Topol-M", "RS-28 Sarmat", "Iskander-M", "Kalibr"),
                                     Payload_KG = c(1200, 10000, 700, 450),
                                     Range_KM = c(11000, 18000, 500, 2500),
                                     Country = "Russia"
                                   )
                                   
                                   # Combine both datasets
                                   all_missiles <- bind_rows(usa_missiles, russia_missiles)
                                   
                                   # Reshape for plotting
                                   missiles_long <- all_missiles %>%
                                     pivot_longer(cols = c(Payload_KG, Range_KM),
                                                  names_to = "Metric",
                                                  values_to = "Value")
                                   
                                   # Plot: grouped bar chart with facet by Country
                                   ggplot(missiles_long, aes(x = Missile, y = Value, fill = Metric)) +
                                     geom_bar(stat = "identity", position = "dodge") +
                                     facet_wrap(~ Country, scales = "free_x") +
                                     labs(title = "Comparison of USA and Russia Missile Payload & Range",
                                          x = "Missile",
                                          y = "Value",
                                          fill = "Metric") +
                                     theme_minimal() +
                                     theme(axis.text.x = element_text(angle = 45, hjust = 1))
                                   #########
                                   
                                   
                                   # Install and load tidyverse
                                   install.packages("tidyverse")  # Only once
                                   suppressPackageStartupMessages(library(tidyverse))
                                   
                                   # Iran missile data
                                   iran_missiles <- data.frame(
                                     Missile = c("Shahab-3", "Sejjil", "Fateh-110"),
                                     Payload_KG = c(1000, 650, 500),
                                     Range_KM = c(2000, 2500, 300),
                                     Country = "Iran"
                                   )
                                   
                                   # Israel missile data
                                   israel_missiles <- data.frame(
                                     Missile = c("Jericho II", "Jericho III", "LORA"),
                                     Payload_KG = c(1000, 1000, 570),
                                     Range_KM = c(1500, 6500, 400),
                                     Country = "Israel"
                                   )
                                   
                                   # Combine both datasets
                                   all_missiles <- bind_rows(iran_missiles, israel_missiles)
                                   
                                   # Reshape to long format for ggplot
                                   missiles_long <- all_missiles %>%
                                     pivot_longer(cols = c(Payload_KG, Range_KM),
                                                  names_to = "Metric",
                                                  values_to = "Value")
                                   
                                   # Plot: grouped bar chart with facet by Country
                                   ggplot(missiles_long, aes(x = Missile, y = Value, fill = Metric)) +
                                     geom_bar(stat = "identity", position = "dodge") +
                                     facet_wrap(~ Country, scales = "free_x") +
                                     labs(title = "Comparison of Iran and Israel Missile Payload & Range",
                                          x = "Missile",
                                          y = "Value",
                                          fill = "Metric") +
                                     theme_minimal() +
                                     theme(axis.text.x = element_text(angle = 45, hjust = 1))
                                   
                                   



# 1. Install reshape2 (only once)
install.packages("reshape2")

# 2. Load the package
library(reshape2)

# 3. Create your dataset
usa_missiles <- data.frame(
  Missile = c("Minuteman III", "Trident II", "ATACMS", "Sentinel"),
  Payload_KG = c(1100, 2800, 500, 1100),
  Range_KM = c(12000, 12000, 300, 12000)
)

# 4. Melt the data
usa_long <- melt(usa_missiles, 
                 id.vars = "Missile", 
                 variable.name = "Metric", 
                 value.name = "Value")

# 5. View the result
print(usa_long)

# 1. Install tidyverse
install.packages("tidyverse")

# 2. Load tidyverse (includes tidyr)
library(tidyverse)

# 3. Reshape the data
usa_long <- usa_missiles %>%
  pivot_longer(cols = -Missile,
               names_to = "Metric",
               values_to = "Value")

# 4. View result
print(usa_long)


# Install if needed (only once)
install.packages("reshape2")

# Load the package quietly
suppressPackageStartupMessages(library(reshape2))

# Install and load required packages
install.packages("tidyverse")  # Only once
suppressPackageStartupMessages(library(tidyverse))

# Create dataset
usa_missiles <- data.frame(
  Missile = c("Minuteman III", "Trident II", "ATACMS", "Sentinel"),
  Payload_KG = c(1100, 2800, 500, 1100),
  Range_KM = c(12000, 12000, 300, 12000)
)

# Reshape data to long format
usa_long <- usa_missiles %>%
  pivot_longer(cols = c(Payload_KG, Range_KM), 
               names_to = "Metric", 
               values_to = "Value")

# Create grouped bar chart
ggplot(usa_long, aes(x = Missile, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "USA Missile Payload and Range",
       x = "Missile",
       y = "Value",
       fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#####
# Install and load required packages
install.packages("tidyverse")  # Only once
suppressPackageStartupMessages(library(tidyverse))

# Create dataset
usa_missiles <- data.frame(
  Missile = c("Minuteman III", "Trident II", "ATACMS", "Sentinel"),
  Payload_KG = c(1100, 2800, 500, 1100),
  Range_KM = c(12000, 12000, 300, 12000)
)

# Reshape data to long format
usa_long <- usa_missiles %>%
  pivot_longer(cols = c(Payload_KG, Range_KM), 
               names_to = "Metric", 
               values_to = "Value")

# Create grouped bar chart
ggplot(usa_long, aes(x = Missile, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "USA Missile Payload and Range",
       x = "Missile",
       y = "Value",
       fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####

# Install and load required packages
install.packages("tidyverse")  # Only once
suppressPackageStartupMessages(library(tidyverse))

# Create dataset for Russian missiles
russia_missiles <- data.frame(
  Missile = c("Topol-M", "RS-28 Sarmat", "Iskander-M", "Kalibr"),
  Payload_KG = c(1200, 10000, 700, 450),
  Range_KM = c(11000, 18000, 500, 2500)
)

# Reshape data to long format
russia_long <- russia_missiles %>%
  pivot_longer(cols = c(Payload_KG, Range_KM), 
               names_to = "Metric", 
               values_to = "Value")

# Create grouped bar chart
ggplot(russia_long, aes(x = Missile, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Russian Missile Payload and Range",
       x = "Missile",
       y = "Value",
       fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angl
                                   
######
install.packages("tidyverse")
suppressPackageStartupMessages(library(tidyverse))


# Create dataset for Russian missiles
russia_missiles <- data.frame(
  Missile = c("Topol-M", "RS-28 Sarmat", "Iskander-M", "Kalibr"),
  Payload_KG = c(1200, 10000, 700, 450),
  Range_KM = c(11000, 18000, 500, 2500)
)

# Reshape data to long format using pipes
russia_long <- russia_missiles %>%
  pivot_longer(cols = c(Payload_KG, Range_KM), 
               names_to = "Metric", 
               values_to = "Value")

# Create grouped bar chart
ggplot(russia_long, aes(x = Missile, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Russian Missile Payload and Range",
       x = "Missile",
       y = "Value",
       fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### russia and USA

# Install and load tidyverse
install.packages("tidyverse")  # Run only once
suppressPackageStartupMessages(library(tidyverse))

# USA missile data
usa_missiles <- data.frame(
  Missile = c("Minuteman III", "Trident II", "ATACMS", "Sentinel"),
  Payload_KG = c(1100, 2800, 500, 1100),
  Range_KM = c(12000, 12000, 300, 12000),
  Country = "USA"
)

# Russia missile data
russia_missiles <- data.frame(
  Missile = c("Topol-M", "RS-28 Sarmat", "Iskander-M", "Kalibr"),
  Payload_KG = c(1200, 10000, 700, 450),
  Range_KM = c(11000, 18000, 500, 2500),
  Country = "Russia"
)

# Combine both datasets
all_missiles <- bind_rows(usa_missiles, russia_missiles)

# Reshape for plotting
missiles_long <- all_missiles %>%
  pivot_longer(cols = c(Payload_KG, Range_KM),
               names_to = "Metric",
               values_to = "Value")

# Plot: grouped bar chart with facet by Country
ggplot(missiles_long, aes(x = Missile, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Country, scales = "free_x") +
  labs(title = "Comparison of USA and Russia Missile Payload & Range",
       x = "Missile",
       y = "Value",
       fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#########


# Install and load tidyverse
install.packages("tidyverse")  # Only once
suppressPackageStartupMessages(library(tidyverse))

# Iran missile data
iran_missiles <- data.frame(
  Missile = c("Shahab-3", "Sejjil", "Fateh-110"),
  Payload_KG = c(1000, 650, 500),
  Range_KM = c(2000, 2500, 300),
  Country = "Iran"
)

# Israel missile data
israel_missiles <- data.frame(
  Missile = c("Jericho II", "Jericho III", "LORA"),
  Payload_KG = c(1000, 1000, 570),
  Range_KM = c(1500, 6500, 400),
  Country = "Israel"
)

# Combine both datasets
all_missiles <- bind_rows(iran_missiles, israel_missiles)

# Reshape to long format for ggplot
missiles_long <- all_missiles %>%
  pivot_longer(cols = c(Payload_KG, Range_KM),
               names_to = "Metric",
               values_to = "Value")

# Plot: grouped bar chart with facet by Country
ggplot(missiles_long, aes(x = Missile, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Country, scales = "free_x") +
  labs(title = "Comparison of Iran and Israel Missile Payload & Range",
       x = "Missile",
       y = "Value",
       fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##### CHINA AND NK

# 1. Install reshape2 (only once)
install.packages("reshape2")

# 2. Load the package
library(reshape2)

# 3. Create your dataset
usa_missiles <- data.frame(
  Missile = c("Minuteman III", "Trident II", "ATACMS", "Sentinel"),
  Payload_KG = c(1100, 2800, 500, 1100),
  Range_KM = c(12000, 12000, 300, 12000)
)

# 4. Melt the data
usa_long <- melt(usa_missiles, 
                 id.vars = "Missile", 
                 variable.name = "Metric", 
                 value.name = "Value")

# 5. View the result
print(usa_long)

# 1. Install tidyverse
install.packages("tidyverse")

# 2. Load tidyverse (includes tidyr)
library(tidyverse)

# 3. Reshape the data
usa_long <- usa_missiles %>%
  pivot_longer(cols = -Missile,
               names_to = "Metric",
               values_to = "Value")

# 4. View result
print(usa_long)


# Install if needed (only once)
install.packages("reshape2")

# Load the package quietly
suppressPackageStartupMessages(library(reshape2))

# Install and load required packages
install.packages("tidyverse")  # Only once
suppressPackageStartupMessages(library(tidyverse))

# Create dataset
usa_missiles <- data.frame(
  Missile = c("Minuteman III", "Trident II", "ATACMS", "Sentinel"),
  Payload_KG = c(1100, 2800, 500, 1100),
  Range_KM = c(12000, 12000, 300, 12000)
)

# Reshape data to long format
usa_long <- usa_missiles %>%
  pivot_longer(cols = c(Payload_KG, Range_KM), 
               names_to = "Metric", 
               values_to = "Value")

# Create grouped bar chart
ggplot(usa_long, aes(x = Missile, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "USA Missile Payload and Range",
       x = "Missile",
       y = "Value",
       fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#####
# Install and load required packages
install.packages("tidyverse")  # Only once
suppressPackageStartupMessages(library(tidyverse))

# Create dataset
usa_missiles <- data.frame(
  Missile = c("Minuteman III", "Trident II", "ATACMS", "Sentinel"),
  Payload_KG = c(1100, 2800, 500, 1100),
  Range_KM = c(12000, 12000, 300, 12000)
)

# Reshape data to long format
usa_long <- usa_missiles %>%
  pivot_longer(cols = c(Payload_KG, Range_KM), 
               names_to = "Metric", 
               values_to = "Value")

# Create grouped bar chart
ggplot(usa_long, aes(x = Missile, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "USA Missile Payload and Range",
       x = "Missile",
       y = "Value",
       fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####

# Install and load required packages
install.packages("tidyverse")  # Only once
suppressPackageStartupMessages(library(tidyverse))

# Create dataset for Russian missiles
russia_missiles <- data.frame(
  Missile = c("Topol-M", "RS-28 Sarmat", "Iskander-M", "Kalibr"),
  Payload_KG = c(1200, 10000, 700, 450),
  Range_KM = c(11000, 18000, 500, 2500)
)

# Reshape data to long format
russia_long <- russia_missiles %>%
  pivot_longer(cols = c(Payload_KG, Range_KM), 
               names_to = "Metric", 
               values_to = "Value")

# Create grouped bar chart
ggplot(russia_long, aes(x = Missile, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Russian Missile Payload and Range",
       x = "Missile",
       y = "Value",
       fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angl
                                   
######
install.packages("tidyverse")
suppressPackageStartupMessages(library(tidyverse))


# Create dataset for Russian missiles
russia_missiles <- data.frame(
  Missile = c("Topol-M", "RS-28 Sarmat", "Iskander-M", "Kalibr"),
  Payload_KG = c(1200, 10000, 700, 450),
  Range_KM = c(11000, 18000, 500, 2500)
)

# Reshape data to long format using pipes
russia_long <- russia_missiles %>%
  pivot_longer(cols = c(Payload_KG, Range_KM), 
               names_to = "Metric", 
               values_to = "Value")

# Create grouped bar chart
ggplot(russia_long, aes(x = Missile, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Russian Missile Payload and Range",
       x = "Missile",
       y = "Value",
       fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### russia and USA

# Install and load tidyverse
install.packages("tidyverse")  # Run only once
suppressPackageStartupMessages(library(tidyverse))

# USA missile data
usa_missiles <- data.frame(
  Missile = c("Minuteman III", "Trident II", "ATACMS", "Sentinel"),
  Payload_KG = c(1100, 2800, 500, 1100),
  Range_KM = c(12000, 12000, 300, 12000),
  Country = "USA"
)

# Russia missile data
russia_missiles <- data.frame(
  Missile = c("Topol-M", "RS-28 Sarmat", "Iskander-M", "Kalibr"),
  Payload_KG = c(1200, 10000, 700, 450),
  Range_KM = c(11000, 18000, 500, 2500),
  Country = "Russia"
)

# Combine both datasets
all_missiles <- bind_rows(usa_missiles, russia_missiles)

# Reshape for plotting
missiles_long <- all_missiles %>%
  pivot_longer(cols = c(Payload_KG, Range_KM),
               names_to = "Metric",
               values_to = "Value")

# Plot: grouped bar chart with facet by Country
ggplot(missiles_long, aes(x = Missile, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Country, scales = "free_x") +
  labs(title = "Comparison of USA and Russia Missile Payload & Range",
       x = "Missile",
       y = "Value",
       fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#########


# Install and load tidyverse
install.packages("tidyverse")  # Only once
suppressPackageStartupMessages(library(tidyverse))

# Iran missile data
iran_missiles <- data.frame(
  Missile = c("Shahab-3", "Sejjil", "Fateh-110"),
  Payload_KG = c(1000, 650, 500),
  Range_KM = c(2000, 2500, 300),
  Country = "Iran"
)

# Israel missile data
israel_missiles <- data.frame(
  Missile = c("Jericho II", "Jericho III", "LORA"),
  Payload_KG = c(1000, 1000, 570),
  Range_KM = c(1500, 6500, 400),
  Country = "Israel"
)

# Combine both datasets
all_missiles <- bind_rows(iran_missiles, israel_missiles)

# Reshape to long format for ggplot
missiles_long <- all_missiles %>%
  pivot_longer(cols = c(Payload_KG, Range_KM),
               names_to = "Metric",
               values_to = "Value")

# Plot: grouped bar chart with facet by Country
ggplot(missiles_long, aes(x = Missile, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Country, scales = "free_x") +
  labs(title = "Comparison of Iran and Israel Missile Payload & Range",
       x = "Missile",
       y = "Value",
       fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##### CHINA /NORTH KOREA #####

library(tidyverse)

# Step 1: Define missile data for China
china <- data.frame(
  Country = "China",
  Missile = c("DF-11", "DF-15", "DF-21", "DF-26", "DF-31", "DF-41"),
  Payload = c(500, 600, 600, 1800, 1050, 2500),
  Range = c(600, 900, 1750, 4000, 11200, 15000)
)

# Step 2: Define missile data for North Korea
north_korea <- data.frame(
  Country = "North Korea",
  Missile = c("Hwasong-5", "Hwasong-7", "Hwasong-12", "Hwasong-14", "Hwasong-15", "Hwasong-17"),
  Payload = c(1000, 1200, 500, 500, 1000, 2000),
  Range = c(300, 700, 4500, 10000, 13000, 15000)
)

# Step 3: Combine both datasets
missiles_combined <- bind_rows(china, north_korea)

# Step 4: Reshape into long format
missiles_long <- missiles_combined %>%
  pivot_longer(cols = c(Payload, Range),
               names_to = "Metric",
               values_to = "Value")

# Step 5: Plot grouped bar chart
ggplot(missiles_long, aes(x = Missile, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Country, scales = "free_x") +
  labs(
    title = "Chinese and North Korean Missile Capabilities: Payload vs Range",
    x = "Missile System",
    y = "Value (kg or km)",
    fill = "Metric"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






##### the amount of missles that each country have (aproximate)

# Load necessary libraries
library(tidyverse)

# Step 1: Define missile counts for each country
missile_counts <- data.frame(
  Country = c("Israel", "Iran", "USA", "Russia", "China", "North Korea"),
  Number_of_Missiles = c(6, 10, 15, 20, 12, 8)  # You can update these with real figures if needed
)

# Step 2: View the dataset
print(missile_counts)

# Step 3: Plot a bar chart
ggplot(missile_counts, aes(x = reorder(Country, -Number_of_Missiles), y = Number_of_Missiles, fill = Country)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Number of Known Missile Systems by Country",
    x = "Country",
    y = "Number of Missile Systems"
  ) +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))




