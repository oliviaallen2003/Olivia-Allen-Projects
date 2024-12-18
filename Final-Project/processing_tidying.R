# Load libraries
library(tidyverse)
library(readr)
library(stringr)
library(knitr)

# Load dataset
spotify_data <- read_csv("data/raw/spotify.csv")

# Looking through dataset
str(spotify_data)
summary(spotify_data)

# Summarize missingness
missing_before <- colSums(is.na(spotify_data))
kable(
  as.data.frame(missing_before),
  caption = "Missingness in Dataset Before Cleaning"
)

# Identify non-ASCII characters and proportion within dataset
non_ascii_counts_one_or_more <- tibble(
  column = c("artists", "album_name", "track_name"),
  `Values with 1+ non-ASCII characters` = c(
    sum(str_detect(spotify_data$artists, "[^[:ascii:]]"), na.rm = TRUE),
    sum(str_detect(spotify_data$album_name, "[^[:ascii:]]"), na.rm = TRUE),
    sum(str_detect(spotify_data$track_name, "[^[:ascii:]]"), na.rm = TRUE)
  ),
  `Total Values in Column` = c(
    nrow(spotify_data),
    nrow(spotify_data),
    nrow(spotify_data)
  ),
  proportion = c(
    sum(str_detect(spotify_data$artists, "[^[:ascii:]]"), na.rm = TRUE) / nrow(spotify_data),
    sum(str_detect(spotify_data$album_name, "[^[:ascii:]]"), na.rm = TRUE) / nrow(spotify_data),
    sum(str_detect(spotify_data$track_name, "[^[:ascii:]]"), na.rm = TRUE) / nrow(spotify_data)
  )
)

non_ascii_counts_one_or_more |> 
  kable(caption = "Values with 1 or more non-ASCII characters")

# Create a function to show sample non-ASCII values
extract_non_ascii_values <- function(column, n) {
  pattern <- paste0("^(?:[^[:ascii:]]){", n, "}[[:ascii:]]*$|^(?:[[:ascii:]]*[^[:ascii:]]){", n, "}[[:ascii:]]*$")
  na.omit(column[str_detect(column, pattern)])[1:10]
}

# Examples of non-ASCII values for specific counts
non_ascii_1 <-extract_non_ascii_values(spotify_data$artists, 1)
non_ascii_1 |>
  kable(caption = "Artists' names with 1 non-ASCII character")

non_ascii_2 <- extract_non_ascii_values(spotify_data$artists, 2)
non_ascii_2 |>
  kable(caption = "Artists' names with 2 non-ASCII characters")

non_ascii_3 <- extract_non_ascii_values(spotify_data$artists, 3)
non_ascii_3 |>
  kable(caption = "Artists' names with 3 non-ASCII characters")

# Clean non-ASCII characters
spotify_data$artists <- str_remove_all(spotify_data$artists, "[^[:ascii:]]")
spotify_data$artists[spotify_data$artists == ""] <- NA

spotify_data$album_name <- str_remove_all(spotify_data$album_name, "[^[:ascii:]]")
spotify_data$album_name[spotify_data$album_name == ""] <- NA

spotify_data$track_name <- str_remove_all(spotify_data$track_name, "[^[:ascii:]]")
spotify_data$track_name[spotify_data$track_name == ""] <- NA

# Creating new csv file with cleaned data
data_clean <- na.omit(spotify_data)
write.csv(data_clean, "spotify_clean.csv", row.names = FALSE)

# Showing cleaned dataset and number of non-ASCII values after cleaning
non_ascii_counts <- tibble(
  column = c("artists", "album_name", "track_name"),
  `Values with 1+ non-ASCII characters` = c(
    sum(str_detect(spotify_cleaned$artists, "[^[:ascii:]]"), na.rm = TRUE),
    sum(str_detect(spotify_cleaned$album_name, "[^[:ascii:]]"), na.rm = TRUE),
    sum(str_detect(spotify_cleaned$track_name, "[^[:ascii:]]"), na.rm = TRUE)
  )
)
non_ascii_counts |> 
  kable(caption = "Values with 1 or more non-ASCII characters (Post-Cleaning)")

# Showing missingness after cleaning 
missing_values_per_column <- colSums(is.na(data_clean))
missing_values_per_column |> kable(caption = "Spotify Data Missingness (Post-Cleaning)")
