# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Your code here.


# Install the necessary packages.
# If you have installed it previously, you can ignore this step.
install.packages('tidyverse')

# Import the ggplot2 library.
library(ggplot2)

# The whole tidyverse package.
library(tidyverse)
# Import and read CSV file.
library(readr)
# Data wrangling.
library(dplyr)
# Data wrangling.
library(tidyr)
# Create statistical summaries.
library(skimr)
# Create a report as an HTML file.
library(DataExplorer)

# Load data (assuming you're loading your CSV again)
reviews <- read.csv("turtle_reviews.csv")

# First, rename the 3 specific columns
names(reviews)[names(reviews) == "renumeration"] <- "Renumeration"
names(reviews)[names(reviews) == "spending_score"] <- "Spending Score"
names(reviews)[names(reviews) == "loyalty_points"] <- "Loyalty Points"

# Now capitalize the first letter of all remaining column names
names(reviews) <- sapply(names(reviews), function(name) {
  paste0(toupper(substring(name, 1, 1)), substring(name, 2))
})

# Check final column names
print(names(reviews))

# View top 10 rows.
head(reviews)

# View top 10 rows.
summary(reviews)

# Return a frequency table for the 'language' column.
table(reviews$language)

# Return a frequency table for the 'platform' column.
table(reviews$platform)

## What we want to do:
# Get rid of 'language' and 'platform' columns.
# Check why 'age' is not numeric.
# Convert 'race' to factor (categorical variable).

## What we want to do:
# Get rid of 'language' and 'platform' columns.
# remove missing data not picked up in excel clean up.


# Remove the 'language' and 'platform' columns.
reviews <- dplyr::select(reviews, -c(language, platform))

# To search for missing values in a specific column of a data set.
is.na(reviews$loyalty_points)

# To search for missing values in a data set.
sum(is.na(reviews))

# To search for missing values in a specific column of a data set.
sum(is.na(reviews$loyalty_points))

# To search for missing values in a data set.
reviews[is.na(reviews)]

# Use the summary(reviews) function.
summary(reviews)

# No missing, data cleaned up
##############################################################################

# Convert data frame to a tibble.
# Import the ggplot2 library.
install.packages('ggplot2')
library(ggplot2)

-----------
### Descriptive statistics.

# These functions provide summary statistics of the data set.
summary(reviews)

# This function creates a downloadable HTML file containing summary stats of
# the data set.
DataExplorer::create_report(reviews)


# 2. Explore the data set

# Determine descriptive statistics of the data set.
summary(reviews)
summary(reviews$`Loyalty Points`)


# Measure central tendencies of loyalty points with mean and median.
mean(reviews$`Loyalty Points`)
median(reviews$`Loyalty Points`)

# Statistics of extreme values (max and min).
min (reviews$`Loyalty Points`)
max (reviews$`Loyalty Points`)

# Measure the variability of loyalty points values.
# Range = Maximum - Minimum.
max(reviews$`Loyalty Points`)- min(reviews$`Loyalty Points`)  

# Function to calculate Q1.
quantile(reviews$`Loyalty Points`, 0.25)  

# Function to calculate Q2.
quantile(reviews$`Loyalty Points`, 0.75)   

# Function to calculate IQR.
IQR(reviews$`Loyalty Points`)    

# Function to determine the variance.
var(reviews$`Loyalty Points`)

# Function to return the standard deviation.
sd(reviews$`Loyalty Points`) 


###############################################################################

# 3. Determine if data is normally distributed

# Measure normality in loyalty points values.
# Q-Q plot:
qqnorm(reviews$`Loyalty Points`)
# Add a reference line:
qqline(reviews$`Loyalty Points`, col='red')


# Shapiro-Wilk test:
shapiro.test((reviews$`Loyalty Points`))
# Our p-value is <2.2,so the data is not normally distributed.

# Check for skewness.
# First import the moments package and library.
library(moments)

# Now we can check for skewness.
skewness(reviews$`Loyalty Points`)
# Our output suggests a positive skewness.


#Check for kurtosis.
kurtosis(reviews$`Loyalty Points`)
# Our kurtosis value is less than 3, suggesting our data is platykurtic.

###
# Install the ggplot2 package.
install.packages('ggplot2')
# Import the ggplot2 library.
library(ggplot2)

# Examine a variable (loyalty_points) through a visualisation.
ggplot(reviews, aes(x = `Loyalty Points`)) + geom_histogram()


# Examine a variable (renumeration) through a visualisation.
qplot(Renumeration, data=reviews)

# Examine two variables (renumeration and loyalty_points) through a visualisation.
qplot(Renumeration, `Loyalty Points`, data = reviews)

# Examine two variables (spending_score and loyalty_points) through a visualisation.
qplot('Spending Score', 'Loyalty Points', data = reviews)

# Examine two variables (renumeration and spending_score) through a visualisation.
qplot(Renumeration, 'Spending Score', data = reviews)

#### In depth visualualisations 

# 3c) Scatterplot
# Start with a simple scatterplot.
ggplot(reviews, aes(x=Renumeration, y='Loyalty Points')) + 
  geom_point()


## Scatterplot with a line-of-best-fit from linear regression model.
ggplot(reviews, aes(x=`Spending Score`, Renumeration, y=`Loyalty Points`)) + 
  geom_point() + 
  geom_smooth(method=lm)

# Scatterplot with no method in geom_smooth() (spline).
ggplot(reviews, aes(x=Renumeration, y=`Loyalty Points`)) + 
  geom_point() + 
  geom_smooth()

# Add a further/third variable as a colour and remove the smoothing line.
ggplot(reviews, aes(x=`Spending Score`, y=`Loyalty Points`, color=renumeration)) + 
  geom_point()


# Add a further/third variable as a colour with a smoothing line (spline).
ggplot(reviews, aes(x=`Spending Score`, y=`Loyalty Points`, col=Renumeration)) + 
  geom_point() + 
  geom_smooth()

# Add a further/third variable and no standard error.
ggplot(reviews, aes(x=`Spending Score`, y=`Loyalty Points`, col=Renumeration)) + geom_point() + 
  geom_smooth(se=FALSE)

# third variable and no standard error.
ggplot(reviews, aes(x=Renumeration, y=`Loyalty Points`, col=`Spending Score`)) + geom_point() + 
  geom_smooth(se=FALSE)
###

# 5. Descriptive statistics.
# These functions provide summary statistics of the data set.
summary(reviews)

# Comparing spending score and loyalty points with colour (fill and side-by-side).
ggplot(reviews, aes(x=`Spending Score`, fill=`Loyalty Points`)) + 
  geom_bar(position='dodge')

# This function creates a downloadable HTML file containing summary stats of
# the data set.
DataExplorer::create_report(reviews)

################################################################################
## Assignment 6 assignment: Making recommendations to the business.

################################################################################

# Your code here.

# View the data frame.
head(reviews)
str(reviews)

# Determine descriptive statistics of the data set.
summary(reviews)
summary(reviews$`Loyalty Points`)

# Measure central tendencies of loyalty_points with mean and median.
mean(reviews$`Loyalty Points`)
median(reviews$`Loyalty Points`)

# Statistics of extreme values (max and min).
min (reviews$`Loyalty Points`)
max (reviews$`Loyalty Points`)

# Measure the variability of loyalty_points values.
# Range = Maximum - Minimum.
max(reviews$`Loyalty Points`)- min(reviews$`Loyalty Points`)  

-----
# Function to calculate Q1.
quantile(reviews$`Loyalty Points`, 0.25)  

# Function to calculate Q2.
quantile(reviews$`Loyalty Points`, 0.75)   

# Function to calculate IQR.
IQR(reviews$`Loyalty Points`)    

# Function to determine the variance.
var(reviews$`Loyalty Points`)

# Function to return the standard deviation.
sd(reviews$`Loyalty Points`) 

###############################################################################

# 3. Determine if data is normally distributed

# Measure normality in loyalty point values.
# Q-Q plot:
qqnorm(reviews$`Loyalty Points`)
# Add a reference line:
qqline(reviews$`Loyalty Points`, col='red')

# Shapiro-Wilk test:
shapiro.test((reviews$`Loyalty Points`))
# Our p-value is <2.2,so the data is not normally distributed.

# Check for skewness.
# First import the moments package and library.
library(moments)

# Now we can check for skewness.
skewness(reviews$`Loyalty Points`)
# Output suggests a positive skewness.

#Check for kurtosis.
kurtosis(reviews$`Loyalty Points`)
# Our kurtosis value is more than 3, suggesting our data is leptokurtic.
###############################################################################

# 4. Determine if there are any correlation(s)
# Check correlation between loyalty_points and client age.
# Let's first check for normality in the client age values.
shapiro.test(reviews$`Loyalty Points`)
# Our output is less than 0.05, so the data is not normally distributed.

# Check correlation between loyalty_points and age using Pearson's correlation.
cor(reviews$`Loyalty Points`, reviews$Renumeration)

# Comparing age with wage and change the line width.
ggplot(reviews, aes(x=Renumeration, y=`Loyalty Points`, col=`Spending Score`)) + 
  geom_point() + 
  geom_smooth(lwd=2, se=FALSE) +
  theme_classic()
  ggtitle("Turtle Reviews education level versus spending score")

###############################################################################
# Visualisation and data wrangling.
# Import tidyverse.
library(tidyverse)
# For k-means clustering and visualisation
library(factoextra) 

# Set your working directory.
# Import the CSV file.
reviews <- read.csv('turtle_reviews.csv', header=TRUE)

# Create a scatterplot with ggplot().
# Assign an object 'plot', specify the data frame,
# specify the variables and the geom type.
plot <- ggplot(data=reviews,
               mapping=aes(x=Renumeration, y=`Loyalty Points`)) +
  geom_point()


# Import the libraries.
library(plotly)
library(tidyverse)

# Assign a data frame name to the data set (reviews_df).
reviews <- reviews

# View the object (data) type.
typeof(reviews)

# Create the plot.
# This a basic plot with a built-in data set.
fig <- plot_ly(reviews,
               x=~Renumeration,
               color=~`Spending Score`,
               type='box')

# View the plot.
fig 

# Install the factoextra package for k-means clustering. 
install.packages('factoextra')


# Explore the data set.
summary(reviews)
dim(reviews)
-
  

# Plot the relationship with base R graphics.
plot(reviews$`Loyalty Points`, reviews$Renumeration)

# Install the psych package.
install.packages('psych')

# Import the psych package.
library(psych)

summary(reviews) 

# Remove the 'Language', 'Platform'.
reviews <- dplyr::select(reviews, -c(Language, Platform))

# Use the corPlot() function.
# Specify the data frame (reviews) and set 
# character size (cex=2).
corPlot(reviews, cex=2)


# Create a new object and 
# specify the lm function and the variables.
modela = lm(`Loyalty Points`~`Spending Score`+Renumeration+Age, 
            data=reviews)

# Print the summary statistics.
summary(modela)


# New model.
modelc = lm(`Loyalty Points`~`Spending Score`+Renumeration, 
            data=reviews) 

summary(modelc)


# View the data.
str(reviews)

# Create a new object and specify the predict function.
predictTest = predict(modelc, newdata=reviews,
                      interval='confidence')

# Print the object.
predictTest 


#############

# 4. Fit a simple linear regression model.

# Create a model with only one x variable.
model1 <- lm(`Loyalty Points`~Renumeration,
             data=reviews)


# View the model.
model1

# View more outputs for the model - the full regression table.
summary(model1)

# Renumeration is a low value, explaining over 37.95 of the variability while spending score is 45.20% of the variablity.

### VISUALISATIONS
# Import the libraries.
library(plotly)
library(tidyverse)

# Convert the data set to a data frame.
reviews_df <- as.data.frame(reviews)

# View the object (data) type.
# cannot run plot_ly without running this code.
typeof(reviews)

# View the data frame.
head(reviews)
str(reviews)
summary(reviews)

# Use plotly to create a plot with one variable.
# Note the spelling of the function (plot_ly).
# Specify the data frame and the x variable.
plot_ly(reviews_df,
        x = ~Renumeration)

# Create a scatterplot with ggplot().
# Assign an object 'plot', specify the data frame,
# specify the variables and the geom type.
plot <- ggplot(data=reviews_df,
               mapping=aes(x=Renumeration, y=`Loyalty Points`)) +
  geom_point()
  ggtitle("Turtle Reviews education level versus spending score")

# 2.a) Create a scatterplot
# Compare age (x-variable) and charges (y-variable).
ggplot(reviews,
       mapping=aes(x=Renumeration, y=`Loyalty Points`)) +
  geom_point()

# Examine a variable (reviews) through a visualisation.
qplot(`Loyalty Points`, data=reviews)
=================
# 3a) Histogram
# Start with a simple histogram.
ggplot(reviews, aes(x=`Loyalty Points`)) + 
  geom_histogram()

# Comparing marital and education with colour (fill and side-by-side).
ggplot(reviews, aes(x=`Spending Score`, fill=Education)) + 
  geom_bar(position='dodge') +
  ggtitle("Turtle Reviews education level versus spending score")

# Use plotly to create a plot with one variable.
# Note the spelling of the function (plot_ly).
# Specify the data frame and the x variable.
plot_ly(reviews_df,
        x = ~Renumeration)

+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# View the object (data) type.
typeof(reviews)

# Sense-check the data viewing the top six rows.
head(reviews_df)

reviews_df <- reviews

# Convert the data set to a data frame.
reviews_df <- as.data.frame(reviews)

# What if we don't specify the plot type?
# With two variables, allow plotly to select the chart type.
plot_ly(reviews_df,
        x = ~Renumeration,
        y = ~`Loyalty Points`)

# Add a third variable as colour to the scatterplot - spending score.
# Specify the mode as markers and color as cyl.
plot_ly(reviews_df,
        x = ~Renumeration,
        y = ~`Loyalty Points`,
        type = 'scatter',
        mode = 'markers',
        color = ~`Spending Score`)


# Create an animated scatter plot using spending score in the frame parameter.
plot_ly(reviews_df,
        x = ~`Spending Score`,
        y = ~`Loyalty Points`,
        type = 'scatter',
        mode = 'markers',
        frame = ~Renumeration,
        showlegend = TRUE)

# Assign the animated plot to the object viz.
viz <- plot_ly(reviews_df,
               x = ~`Spending Score`,
               y = ~`Loyalty Points`,
               type = 'scatter',
               mode = 'markers',
               frame = ~Renumeration,
               showlegend = TRUE)

viz %>%
  animation_button(x = 1, xanchor = 'right',
                   y = 1, yanchor = 'bottom') %>% 
  animation_slider(currentvalue = list(prefix = 'Renumeration',
                                       font = list(color = 'purple'))) %>%
  animation_opts(frame = 10000, easing = "circle-in")


# Create a 3D plot with an x, y, and z-axis.
plot_ly(reviews_df,
        x = ~`Spending Score`,
        y = ~`Loyalty Points`,
        z = ~Renumeration,
        color = ~factor(Renumeration))


# Edit and alter animation features, such as the button,
# slider, and transitions.
viz %>%
  animation_button(x =1 , xanchor = 'right',
                   y = 1, yanchor = 'bottom')%>% 
  animation_slider(currentvalue = list(prefix = 'Renumeration',
                                       font = list(colors = 'blue', 'purple', 'pink''))) %>% 
  animation_opts(frame = 10000,
                 easing = 'circle-in')
                 

library(plotly)

viz <- plot_ly(reviews_df,
               x = ~`Spending Score`,
               y = ~`Loyalty Points`,
               color = ~categorical_variable,
               frame = ~animation_variable,
               type = 'scatter',
               mode = 'markers') %>%
  animation_button(x = 1, xanchor = 'right',
                   y = 1, yanchor = 'bottom') %>% 
  animation_slider(currentvalue = list(prefix = 'Renumeration',
                                       font = list(color = 'purple'))) %>%
  animation_opts(frame = 1000, easing = 'circle-in')

# Install required packages (only once)
install.packages("plotly")
install.packages("htmlwidgets")
install.packages("webshot2")
install.packages("magick")

# Load libraries
library(plotly)
library(htmlwidgets)
library(webshot2)
library(magick)

# Create your plotly object
viz <- plot_ly(reviews_df,
               x = ~spending_score,
               y = ~loyalty_points,
               color = ~gender,
               frame = ~renumeration,
               type = 'scatter',
               mode = 'markers') %>%
  animation_button(x = 1, xanchor = 'right',
                   y = 1, yanchor = 'bottom') %>% 
  animation_slider(currentvalue = list(prefix = 'Renumeration',
                                       font = list(color = 'purple'))) %>%
  animation_opts(frame = 10000, easing = "circle-in")

# Save as interactive HTML first
htmlwidgets::saveWidget(viz, "viz_animation.html", selfcontained = TRUE)

# Use external screen recorder OR convert HTML to GIF:
# (Note: plotly to GIF is tricky — requires browser rendering)
# You can record screen manually OR:
# Use online HTML to GIF converter after generating viz_animation.html



library(plotly)

viz <- plot_ly(reviews_df,
               x = ~spending_score,
               y = ~loyalty_points,
               color = ~gender,           # Categorical color variable
               frame = ~renumeration,     # Variable to animate over
               type = 'scatter',
               mode = 'markers') %>%
  animation_button(x = 1, xanchor = 'right',
                   y = 1, yanchor = 'bottom') %>% 
  animation_slider(currentvalue = list(prefix = 'renumeration: ',
                                       font = list(color = 'purple'))) %>%
  animation_opts(frame = 1000, easing = 'circle-in')


###############################################################################
###############################################################################




