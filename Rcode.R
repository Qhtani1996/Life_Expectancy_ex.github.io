# Clean the environment
rm(list = ls())           # Clear all objects
if (!is.null(dev.list())) dev.off()  # Clear plots
cat("\014")               # Clear console

# Define your folder structure
dropbox_path <- "C:\\Users\\Admin\\Dropbox\\R_new practice\\Rstudio_projects"

# Load necessary libraries
install.packages("devtools")
devtools::install_github("hrbrmstr/streamgraph")

library(streamgraph)
library(dplyr)
library(readr)

# Set the file path
file_path <- "C:/Users/Admin/Dropbox/R_new practice/Rstudio_projects/data/Long_Format_Life_Expectancy_Data_for_Streamgraph.csv"

# Read the data
data <- read_csv(file_path)

# Create the streamgraph
data %>%
  streamgraph(key = "Country", value = "Life_Expectancy", date = "year", width = 700, height = 400) %>%
  sg_fill_tableau() %>%  # Optional: Use Tableau-like colors
  sg_legend(show = TRUE) %>%
  sg_axis_x(tick_interval = 2)  # Optional: Set x-axis tick interval

#####__________________________________________###

# Load necessary libraries
library(ggplot2)
library(ggrepel)
library(dplyr)
library(readr)

# Load your data
file_path <- "C:/Users/Admin/Dropbox/R_new practice/streamgraph/Long_Format_Life_Expectancy_Data_for_Streamgraph.csv"
data <- read_csv(file_path)

# Rank countries by Life Expectancy for each year
data_ranked <- data %>%
  group_by(year) %>%
  mutate(Rank = rank(-Life_Expectancy)) %>%  # Rank descending
  ungroup()

# Highlight specific countries
highlight_countries <- c("Saudi Arabia", "United States", "France")
data_ranked <- data_ranked %>%
  mutate(Highlight = ifelse(Country %in% highlight_countries, Country, "Other"))

# Create the bump chart
p <- ggplot(data_ranked, aes(x = year, y = Rank, group = Country, color = Highlight)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_y_reverse(breaks = 1:nrow(data_ranked), minor_breaks = NULL) +  # Reverse ranks
  theme_minimal() +
  scale_color_manual(values = c("Saudi Arabia" = "red", "United States" = "blue", "France" = "green", "Other" = "grey")) +
  labs(
    title = "Life Expectancy Rankings Over Time",
    x = "Year",
    y = "Rank",
    color = "Country"
  )

# Display with ggrepel for labeling (optional)
p <- p + geom_text_repel(data = filter(data_ranked, year == max(year)), 
                         aes(label = Country),
                         nudge_x = 0.5, size = 3)

# Make it interactive with plotly
library(plotly)
ggplotly(p)

##2nd graph________________
library(ggplot2)
library(dplyr)

# Select two years (e.g., first and last available years)
selected_years <- data %>%
  filter(year %in% c(min(year), max(year))) %>%
  mutate(year = as.factor(year))

ggplot(selected_years, aes(x = year, y = Life_Expectancy, group = Country, color = Country)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(
    title = "Change in Life Expectancy Over Time",
    x = "Year",
    y = "Life Expectancy"
  ) +
  theme(legend.position = "none")

##end_____________________

##3rd graph
library(ggplot2)
library(ggrepel)

ggplot(data, aes(x = year, y = Life_Expectancy, group = Country)) +
  geom_line() +
  facet_wrap(~Country, scales = "free_y") +
  theme_minimal() +
  labs(title = "Life Expectancy Trends by Country", x = "Year", y = "Life Expectancy")

##end___________
library(ggridges)

ggplot(data, aes(x = Life_Expectancy, y = as.factor(year), fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "Life Expectancy Distribution Over Time", x = "Life Expectancy", y = "Year")


####______________________________________________________________________###

library(ggplot2)
library(dplyr)
library(readr)
library(plotly)  # For interactivity

# Load data
file_path <- "C:/Users/Admin/Dropbox/R_new practice/Rstudio_projects/data/Long_Format_Life_Expectancy_Data_for_Streamgraph.csv"
data <- read_csv(file_path)

# Filter for 2000 and 2020
selected_years <- data %>%
  filter(year %in% c(2000, 2020)) %>%
  mutate(year = as.factor(year),
         Life_Expectancy = ifelse(year == "2000", -Life_Expectancy, Life_Expectancy))  # Mirror 2000 values for alignment

# Create face-to-face bar chart
p <- ggplot(selected_years, 
            aes(x = reorder(Country, -Life_Expectancy), 
                y = Life_Expectancy, 
                fill = year, 
                text = paste("Country:", Country, "<br>Year:", year, "<br>Life Expectancy:", sprintf("%.1f", abs(Life_Expectancy))))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.1f", abs(Life_Expectancy))),  # Format to 1 decimal place
            position = position_stack(vjust = 0.5), 
            size = 3, color = "white") +  # Display LE inside bars
  coord_flip() +
  theme_minimal() +
  labs(title = "Life Expectancy Comparison: 2000 vs 2020", 
       x = "", 
       y = "Life Expectancy") +
  scale_fill_manual(values = c("2000" = "steelblue", "2020" = "dodgerblue")) +  # Blue shades
  theme(legend.position = "top") +
  scale_y_continuous(labels = function(x) sprintf("%.1f", abs(x)))  # Format y-axis labels to 1 decimal place

# Convert to interactive plot
interactive_plot <- ggplotly(p, tooltip = "text")

# Display
interactive_plot
