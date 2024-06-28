## -----------------------------------------------------------------------------
#| label: load-libraries
#| message: false

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(patchwork)


## -----------------------------------------------------------------------------
#| label: load-dataset
#| message: false

# Loading of current military spending sheet based on current USD exchange rate
current_us <- read_excel(
  "SIPRI-Milex-data-2018-2023.xlsx", sheet = "Current US$", skip = 5
)

# Loading of percentage of spending based on GDP
share_gdp <- read_excel(
  "SIPRI-Milex-data-2018-2023.xlsx", sheet = "Share of GDP", skip = 5
)


## -----------------------------------------------------------------------------
current_us
share_gdp


## -----------------------------------------------------------------------------
#| label: data-cleaning-current-us

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
#| label: data-cleaning-share-gdp

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

# Change to 2 decimal point
share_gdp <- share_gdp |>
  mutate(across(-Country, ~round(., 2)))

share_gdp


## -----------------------------------------------------------------------------
#| label: data-transformation-plot

# Pivot Long for Top 10 Military Spending
top10_long <- current_us |>
  select(Country, `2022`, `2023`) |>
  pivot_longer(cols = `2022`:`2023`, names_to = "Year", values_to = "Spending")

top10_long

# Pivot Long for Share of GDP
share_gdp_long <- share_gdp |>
  select(Country, `2022`, `2023`) |>
  pivot_longer(cols = `2022`:`2023`, names_to = "Year", values_to = "Share")

share_gdp_long

# Merge Data using left_join
merged_data <- top10_long |>
  left_join(share_gdp_long, by = c("Country", "Year"))

merged_data

# Ordering of Countries within the bar chart
merged_data <- merged_data |>
  mutate(
    Country = factor(
      Country, levels = c(
        "Japan",
        "France",
        "Ukraine",
        "Germany",
        "United Kingdom",
        "Saudi Arabia",
        "India",
        "Russia",
        "China",
        "United States of America"
      )
    )
  )

merged_data



# Separate the data for each year
data_2022 <- merged_data |>
  filter(Year == "2022")

data_2023 <- merged_data |>
  filter(Year == "2023") |>
  arrange(desc(Spending))

data_2022
data_2023

