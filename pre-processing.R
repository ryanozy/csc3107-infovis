## -----------------------------------------------------------------------------
#| label: load-libraries
#| message: false

library(readxl)
library(dplyr)
library(gt)
library(ggplot2)
library(tidyverse)
library(ggnewscale)
library(scales)
library(patchwork)


## -----------------------------------------------------------------------------
#| label: load-dataset
#| message: false

current_us <- read_excel(
  "SIPRI-Milex-data-2018-2023.xlsx",
  sheet = "Current US$",
  skip = 5)

share_gdp <- read_excel(
  "SIPRI-Milex-data-2018-2023.xlsx",
  sheet = "Share of GDP",
  skip = 5)


## -----------------------------------------------------------------------------
current_us
share_gdp


## -----------------------------------------------------------------------------
#| label: data-cleaning

# Drop rows that the first column is NA
current_us <- current_us |> 
  filter(!is.na(`Country`))

# Drop Notes Column
current_us <- current_us |> 
  select(-Notes)

# Change column names from '2018.0' to '2018'
current_us <- current_us |> 
  rename_with(~sub("\\.0$", "", .))

# Drop rows that the second column is NA
current_us <- current_us |> 
  filter(!is.na(`2018`))

# Mutate '...' values to empty
current_us <- current_us |> 
  mutate(across(everything(), ~replace(., . == "...", NA)))

# Convert all columns to numeric
current_us <- current_us |> 
  mutate(across(-Country, as.numeric))

# Retain Top 10 Countries in 2023
current_us <- current_us |> 
  slice_max(`2023`, n = 10)

current_us


## -----------------------------------------------------------------------------
#| label: data-transformation

# Convert Military Spending from Millions to Billions
current_us <- current_us |> 
  mutate(across(-Country, ~. / 1000))

# Convert Data to Integer
current_us <- current_us |> 
  mutate(across(-Country, as.integer))

current_us


## -----------------------------------------------------------------------------
#| label: share-of-gdp

# Drop rows that the first column is NA
share_gdp <- share_gdp |> 
  filter(!is.na(`Country`))

# Drop Notes Column
share_gdp <- share_gdp |> 
  select(-Notes)

# Change column names from '2018.0' to '2018'
share_gdp <- share_gdp |> 
  rename_with(~sub("\\.0$", "", .))

# Drop rows that the second column is NA
share_gdp <- share_gdp |> 
  filter(!is.na(`2018`))

# Mutate '...' values to empty
share_gdp <- share_gdp |> 
  mutate(across(everything(), ~replace(., . == "...", NA)))

# Convert all columns to numeric
share_gdp <- share_gdp |> 
  mutate(across(-Country, as.numeric))

# Retain 10 Countries from current_us
share_gdp <- share_gdp |>
  filter(Country %in% current_us$Country)

# Convert to percentage
share_gdp <- share_gdp |>
  mutate(across(-Country, ~. * 100))

# Change to 2.dp
share_gdp <- share_gdp |>
  mutate(across(-Country, ~round(., 2)))

share_gdp


## -----------------------------------------------------------------------------
#| label: after-consultation
#| fig.width: 10
#| fig.height: 5

# Restructure Data for Bar Chart
top10_long <- current_us |> 
  select(Country, `2022`, `2023`) |>
  pivot_longer(cols = `2022`:`2023`, names_to = "Year", values_to = "Spending")

# Share of GDP Bar Chart
share_gdp_long <- share_gdp |> 
  select(Country, `2022`, `2023`) |>
  pivot_longer(cols = `2022`:`2023`, names_to = "Year", values_to = "Share")

# Merge Data
merged_data <- top10_long |>
  left_join(share_gdp_long, by = c("Country", "Year"))

scale_factor <- max(merged_data$Spending) /
  max(merged_data$Share)

merged_data <- merged_data |>
  mutate(Country = factor(Country, levels = c("Japan", "France", "Ukraine", "Germany", "United Kingdom", "Saudi Arabia", "India", "Russia", "China", "United States of America")))

# Bar plot
bar_plot <- ggplot(merged_data, aes(x = Country, y = Spending, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(Spending, 1)), 
            position = position_dodge(width = 0.9), 
            vjust = 0.3, hjust = -0.5,
            size = 3) +
  scale_fill_manual(values = c("2022" = "#3182bd", "2023" = "#de2d26")) +  # Manually specify colors
  guides(
    fill = guide_legend(reverse = TRUE, title = "Military Spending")  # Reverse legend order
  ) +
  labs(
    x = "Country",
    y = "Military Spending in US$ (Billions)"
  ) +
  ylim(0, 1000) +  # Extend x-axis limits from 0 to 40
  coord_flip() +
  theme_minimal() +
  theme(legend.position = c(.8, .15))


# Separate the data for each year
data_2022 <- merged_data |>
  filter(Year == "2022")

data_2023 <- merged_data |>
  filter(Year == "2023") |>
  arrange(desc(Spending))

point_plot <- ggplot() +
  geom_point(data = data_2023, aes(x = reorder(Country, Spending), y = Share, color = Share), shape = 16, position = position_nudge(x=0.3), size = 3.5) +
  scale_color_gradientn(colors = c("#fcbba1", "#a50f15")) +
  geom_point(data = data_2022, aes(x = Country, y = Share, fill = Share), shape = 24, position = position_nudge(x=-0.3), size = 3.5) +
  scale_fill_gradientn(colors = c("#c6dbef", "#08519c")) + 
  geom_text(data = data_2023, aes(x = Country, y = Share, label = round(Share, 2)), position = position_dodge(width = 0.5), vjust = -1, hjust = -0.4, size = 3) +
  geom_text(data = data_2022, aes(x = Country, y = Share, label = round(Share, 2)), position = position_dodge(width = 0.5), vjust = 1.5, hjust = -0.6, size = 3) +
  coord_flip() +
  guides(
    fill = guide_colorbar(title = "Share of GDP 2022", order = 2),
    color = guide_colorbar(title = "Share of GDP 2023", order = 1)
  ) + 
  labs(
    y = "Share of GDP (%)"
  ) +
  ylim(0, 45) +  # Extend x-axis limits from 0 to 40
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),  # Remove y-axis text
    axis.ticks.y = element_blank(), # Remove y-axis ticks
    axis.title.y = element_blank()  # Remove y-axis title
  )

# Combine plots with a shared legend at the bottom
combined_plot <- 
  (bar_plot + point_plot) + 
    plot_annotation(
    title = "Top 10 Countries by Military Spending and Share of GDP (2022-2023)",
    caption = "Source: SIPRI Military Expenditure Database",
    theme = theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),  # Center the title
    )
  )

# Display the combined plot
combined_plot


## -----------------------------------------------------------------------------
#| label: export
ggsave("images/top10_countries.png", combined_plot, width = 15, height = 12)

