---
title: "Life Expectancy Example"
author: "Meshal AlQahtani"
format: pdf
knitr:
  opts_chunk:
    fig.width: 10
    fig.height: 6
---



## Introduction

Below are example graphs created using RStudio and published with Quarto. Over the next month, I will frequently share simple and easy-to-create visualizations using RStudio.

## Loading My data

You can find the data I used here: [Global Health Observatory](https://www.who.int/data/gho)



::: {.cell}

```{.r .cell-code}
install.packages("devtools", repos = "https://cloud.r-project.org")
```

::: {.cell-output .cell-output-stdout}

```
package 'devtools' successfully unpacked and MD5 sums checked

The downloaded binary packages are in
	C:\Users\Admin\AppData\Local\Temp\RtmpW8FMij\downloaded_packages
```


:::

```{.r .cell-code}
library(streamgraph)
library(dplyr)
library(readr)

# Set the file path
library(here)
file_path <- here("data", "Long_Format_Life_Expectancy_Data_for_Streamgraph.csv")
data <- read_csv(file_path)
```
:::



## the LE over the years among G20 countries



::: {.cell}

```{.r .cell-code}
library(ggplot2)
library(ggrepel)
library(dplyr)
library(readr)

# Load your data
library(here)
file_path <- here("data", "Long_Format_Life_Expectancy_Data_for_Streamgraph.csv")
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
```

::: {.cell-output-display}
![](LE_Quarto_files/figure-pdf/unnamed-chunk-2-1.pdf){fig-pos='H'}
:::
:::

::: {.cell}

```{.r .cell-code}
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
```

::: {.cell-output-display}
![](LE_Quarto_files/figure-pdf/unnamed-chunk-3-1.pdf){fig-pos='H'}
:::
:::

::: {.cell}

```{.r .cell-code}
library(ggplot2)
library(ggrepel)

ggplot(data, aes(x = year, y = Life_Expectancy, group = Country)) +
  geom_line() +
  facet_wrap(~Country, scales = "free_y") +
  theme_minimal() +
  labs(title = "Life Expectancy Trends by Country", x = "Year", y = "Life Expectancy")
```

::: {.cell-output-display}
![](LE_Quarto_files/figure-pdf/unnamed-chunk-4-1.pdf){fig-pos='H'}
:::
:::

::: {.cell}

```{.r .cell-code}
library(ggridges)

ggplot(data, aes(x = Life_Expectancy, y = as.factor(year), fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "Life Expectancy Distribution Over Time", x = "Life Expectancy", y = "Year")
```

::: {.cell-output-display}
![](LE_Quarto_files/figure-pdf/unnamed-chunk-5-1.pdf){fig-pos='H'}
:::
:::

::: {.cell}

```{.r .cell-code}
library(ggplot2)
library(dplyr)
library(readr)
library(plotly)  # For interactivity

# Load data
library(here)
file_path <- here("data", "Long_Format_Life_Expectancy_Data_for_Streamgraph.csv")

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
```

::: {.cell-output-display}
![](LE_Quarto_files/figure-pdf/unnamed-chunk-6-1.pdf){fig-pos='H'}
:::
:::



## GDPR Visualization from : <https://github.com/davidsjoberg/ggbump?tab=readme-ov-file>



::: {.cell}

```{.r .cell-code}
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Load necessary packages
install.packages("pacman")
```

::: {.cell-output .cell-output-stdout}

```
package 'pacman' successfully unpacked and MD5 sums checked

The downloaded binary packages are in
	C:\Users\Admin\AppData\Local\Temp\RtmpW8FMij\downloaded_packages
```


:::

```{.r .cell-code}
install.packages("rnaturalearthdata")
```

::: {.cell-output .cell-output-stdout}

```
package 'rnaturalearthdata' successfully unpacked and MD5 sums checked

The downloaded binary packages are in
	C:\Users\Admin\AppData\Local\Temp\RtmpW8FMij\downloaded_packages
```


:::

```{.r .cell-code}
pacman::p_load(BBmisc, tidyverse, hablar, ggbump, sf, rnaturalearth, feather, janitor, lubridate)
options(stringsAsFactors = F)

# Load GDPR violations data
gdpr_violations <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_violations.tsv')

# Process data
df <- gdpr_violations %>% 
  group_by(name) %>% 
  summarise(price = sum(price)) %>% 
  ungroup()

sdf <- rnaturalearthdata::countries50 %>% 
  st_as_sf() %>% 
  st_crop(xmin = -24, xmax = 31, ymin = 33, ymax = 73) %>% 
  filter(admin %in% df$name) %>% 
  left_join(df, by = c("admin" = "name")) %>% 
  mutate(price_cap  = price / pop_est,
         admin = case_when(admin == "United Kingdom" ~ "UK",
                           admin == "Czech Republic" ~ "Czech",
                           TRUE ~ admin))

# Create ranking data
ranking <- st_geometry(sdf) %>% 
  st_point_on_surface() %>% 
  st_coordinates() %>% 
  as_tibble() %>% 
  bind_cols(tibble(fine_cap = normalize(rank(sdf$price_cap), range = c(40.12161, 66.12161), method = "range"),
                   country = sdf$admin,
                   xend = 60,
                   x_axis_start = xend + 10,
                   fine_cap_x = normalize(sdf$price_cap, range = c(first(x_axis_start), 100), method = "range"),
                   val_txt = paste0(format(sdf$price_cap, digits = 2, nsmall = 2)),
                   val_txt2 = if_else(country == "Austria", paste0(val_txt, "€ per capita"), val_txt)))

sdf <- sdf %>% 
  bind_cols(ranking %>% select(fine_cap))

# Create the plot
ggplot() + 
  geom_sf(data = sdf, size = .3, fill = "transparent", color = "gray17") +
  geom_sigmoid(data = ranking, 
               aes(x = X, y = Y, xend = x_axis_start - .2, yend = fine_cap, group = country, color = fine_cap), 
               alpha = .6, smooth = 10, size = 1) + 
  geom_segment(data = ranking, 
               aes(x = x_axis_start, y = fine_cap, xend = fine_cap_x, yend = fine_cap, color = fine_cap), alpha = .6, size = 1, 
               lineend = "round") + 
  geom_segment(data = ranking, 
               aes(x = x_axis_start, y = 40, xend = x_axis_start, yend = 67), alpha = .6, size = 1.3, color = "black") +
  geom_point(data = ranking, 
             aes(x = X, y = Y, color = fine_cap), size = 2) +
  geom_text(data = ranking, aes(x = x_axis_start-.5, y = fine_cap, label = country, color = fine_cap), hjust = 1, size = 2.5, nudge_y = .5) +
  geom_text(data = ranking, aes(x = fine_cap_x, y = fine_cap, label = val_txt2, color = fine_cap), hjust = 0, size = 2, nudge_x = .4) +
  coord_sf(clip = "off") +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  theme_void() +
  labs(title = "GDPR fines per capita",
       subtitle = str_wrap("The General Data Protection Regulation (EU) 2016/679 (GDPR) is a regulation in EU law on data protection and privacy in the European Union (EU) and the European Economic Area (EEA).", 100),
       caption = "Source: TidyTuesday & Wikipedia") + 
  theme(plot.margin = margin(.5, 1, .5, .5, "cm"),
        legend.position = "none",
        plot.background = element_rect(fill = "black"),
        plot.caption = element_text(color = "gray40"),
        plot.title = element_text(color = "gray40", size = 16, family = "Helvetica", face = "bold"),
        plot.subtitle = element_text(color = "gray40", size = 8))
```

::: {.cell-output-display}
![](LE_Quarto_files/figure-pdf/unnamed-chunk-7-1.pdf){fig-pos='H'}
:::
:::

