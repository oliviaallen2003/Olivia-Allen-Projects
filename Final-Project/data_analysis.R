# Analysis and Visualizations

# Load libraries
library(tidyverse)
library(knitr)

# Load dataset
spotify_cleaned <- read_csv("data/spotify_clean.csv")

## Tempo

# Filter for valid tempos
spotify_filtered <- spotify_cleaned |>
  filter(tempo > 0 & tempo < 230)

# Distribution of Tempo
ggplot(spotify_filtered, aes(x = tempo)) +
  geom_density(fill = "lightblue", alpha = 0.7) +
  labs(
    title = "Distribution of Tempo Across All Songs",
    x = "Tempo (BPM)",
    y = "Density"
  ) +
  theme_minimal()

# Tempo Statistics
tempo_stats <- spotify_filtered |>
  summarise(
    Mean = mean(tempo, na.rm = TRUE),
    Median = median(tempo, na.rm = TRUE),
    Min = min(tempo, na.rm = TRUE),
    Max = max(tempo, na.rm = TRUE),
    SD = sd(tempo, na.rm = TRUE)
  )

kable(
  tempo_stats,
  caption = "Tempo Statistics Across All Songs",
  col.names = c("Mean", "Median", "Min", "Max", "SD")
)

# Popularity vs. Tempo
spotify_filtered <- spotify_filtered |>
  mutate(tempo_bin = cut(tempo, breaks = seq(0, 250, by = 10), include.lowest = TRUE))

ggplot(spotify_filtered, aes(x = tempo_bin, y = popularity)) +
  geom_boxplot(fill = "lightblue", color = "darkblue", alpha = 0.7) +
  labs(
    title = "Popularity Across Tempo",
    x = "Tempo (BPM)",
    y = "Popularity"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Tempo vs. Popularity by Genre

# Filter for 3 chosen genres
tempo_genre_summary <- spotify_filtered |>
  filter(track_genre %in% c("hip-hop", "pop", "rock")) |>
  mutate(tempo_bin = cut(tempo, breaks = seq(0, 250, by = 10), include.lowest = TRUE)) |>
  group_by(track_genre, tempo_bin) |>
  summarise(mean_popularity = mean(popularity, na.rm = TRUE), .groups = "drop") |>
  na.omit()

ggplot(tempo_genre_summary, aes(x = tempo_bin, y = mean_popularity, color = track_genre, group = track_genre)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_grid(track_genre ~ .) +
  labs(
    title = "Mean Popularity by Tempo Bins Across Genres",
    x = "Tempo (BPM)",
    y = "Mean Popularity"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none")

# Linear Model of Tempo and Popularity by Genre

# Creating for loop to make linear model for chosen genres
for (genre in c("hip-hop", "pop", "rock")) {
  genre_data <- spotify_cleaned |>
    filter(track_genre == genre)
  model <- lm(popularity ~ tempo, data = genre_data)
  coef_summary <- as.data.frame(summary(model)$coefficients)
  colnames(coef_summary) <- c("Estimate", "SE", "t-value", "p-value")
  
  kable(
    coef_summary,
    caption = paste("Linear Model Results Tempo predicting Popularity in", genre),
    col.names = c("Estimate", "SE", "t-value", "p-value"),
    digits = 3
  )
}

## Danceability

# Distribution of Danceability
ggplot(spotify_cleaned, aes(x = danceability)) +
  geom_histogram(bins = 20, color = "black", fill = "blue", alpha = 0.7) +
  labs(
    title = "Distribution of Song Danceability",
    x = "Danceability",
    y = "Frequency"
  ) +
  theme_minimal()

# Danceability Statistics
danceability_stats <- spotify_cleaned |>
  summarise(
    Mean = mean(danceability, na.rm = TRUE),
    Median = median(danceability, na.rm = TRUE),
    Min = min(danceability, na.rm = TRUE),
    Max = max(danceability, na.rm = TRUE),
    SD = sd(danceability, na.rm = TRUE)
  )

kable(
  danceability_stats,
  caption = "Danceability Statistics",
  col.names = c("Mean", "Median", "Min", "Max", "SD")
)

# Popularity by Danceability 

# Creating bins for danceability
spotify_cleaned <- spotify_cleaned |>
  mutate(danceability_bin = cut(danceability, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE))

ggplot(spotify_cleaned, aes(x = danceability_bin, y = popularity)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen") +
  labs(
    title = "Popularity by Danceability Bin",
    x = "Danceability",
    y = "Popularity"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Heatmap of Popularity by Danceability and Energy

# Creating bins for energy
spotify_cleaned <- spotify_cleaned |>
  mutate(
    energy_bin = cut(energy, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE)
  )

heatmap_data <- spotify_cleaned |>
  group_by(danceability_bin, energy_bin) |>
  summarize(mean_popularity = mean(popularity, na.rm = TRUE), .groups = "drop")

ggplot(heatmap_data, aes(x = danceability_bin, y = energy_bin, fill = mean_popularity)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "white") +
  labs(
    title = "Mean Popularity by Danceability and Energy",
    x = "Danceability",
    y = "Energy",
    fill = "Mean Popularity"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Linear Model of Danceability and Energy
lm_danceability_energy <- lm(popularity ~ danceability * energy, data = spotify_cleaned)

kable(
  summary(lm_danceability_energy)$coefficients,
  caption = "Linear Model: Impact of Danceability and Energy on Popularity",
  col.names = c("Coefficient", "SE", "t-value", "p-value"),
  digits = 3
)

## Song Titles

# Distribution of Song Title Lengths

# Creating new variable to count characters in song name
spotify_cleaned <- spotify_cleaned |>
  mutate(title_length = nchar(track_name))

ggplot(spotify_cleaned, aes(x = title_length)) +
  geom_histogram(binwidth = 10, fill = "dodgerblue", color = "white", alpha = 0.8) +
  labs(
    title = "Distribution of Song Title Lengths",
    x = "Title Length (Characters)",
    y = "Frequency"
  ) +
  xlim(0, 100)

# Summarize statistics for title length
title_stats <- spotify_cleaned |> 
  summarise(
    Mean = mean(title_length, na.rm = TRUE),
    Median = median(title_length, na.rm = TRUE),
    Min = min(title_length, na.rm = TRUE),
    Max = max(title_length, na.rm = TRUE),
    SD = sd(title_length, na.rm = TRUE)
  )

title_stats |> 
  kable(caption = "Table 8: Song Title Length Statistics")


# Title lengths vs. popularity

# Creating title length bins
spotify_cleaned <- spotify_cleaned |>
  mutate(title_length_bin = cut(title_length, breaks = seq(0, 100, by = 5), include.lowest = TRUE))

ggplot(spotify_cleaned, aes(x = title_length_bin, y = popularity)) +
  geom_boxplot(fill = "lightblue", color = "darkblue", alpha = 0.7) +
  labs(title = "Popularity Across Song Title Lengths",
       x = "Title Length (Characters)",
       y = "Popularity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


spotify_cleaned <- spotify_cleaned |> 
  mutate(
    title_length = nchar(track_name),
    artist_name_length = nchar(artists)
  )

# Calculating mean popularity and creating bins for graph
spotify_lengths <- spotify_cleaned |> 
  mutate(
    title_length_bin = cut(title_length, breaks = seq(0, 100, by = 10), include.lowest = TRUE),
    artist_length_bin = cut(artist_name_length, breaks = seq(0, 50, by = 5), include.lowest = TRUE)
  ) |> 
  filter(!is.na(title_length_bin), !is.na(artist_length_bin)) |> 
  group_by(title_length_bin, artist_length_bin) |> 
  summarise(mean_popularity = mean(popularity, na.rm = TRUE), .groups = "drop")

# Heatmap for title vs. artist name lengths by Popularity
ggplot(spotify_lengths, aes(x = title_length_bin, y = artist_length_bin, fill = mean_popularity)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(name = "Mean Popularity") +
  labs(
    title = "Popularity by Title and Artist Name Length",
    x = "Song Title Length",
    y = "Artist Name Length"
  ) +
  theme_minimal()

# Linear Model of Title and Artist Name Lengths
lm_model <- lm(popularity ~ title_length * artist_name_length, data = spotify_cleaned)

kable(
  summary(lm_model)$coefficients,
  caption = "Linear Model: Popularity as a Function of Title & Artist Name Length",
  col.names = c("Estimate", "SE", "t-value", "p-value"),
  digits = 3
)
