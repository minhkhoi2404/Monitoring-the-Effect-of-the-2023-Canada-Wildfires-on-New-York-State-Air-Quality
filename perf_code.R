library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

df <- June_2023_Whiteface %>%
  rename_with(~ gsub(" ", "_", .)) %>%
  rename_with(~ gsub("\\.", "_", .))

df <- df %>%
  mutate(
    Time_fixed = format(as.POSIXct(Time, origin = "1899-12-30"), "%H:%M:%S"),
    datetime = as.POSIXct(paste(Date, Time_fixed), format = "%Y-%m-%d %H:%M:%S"),
    Day = day(datetime),
    Hour = hour(datetime),
    Hour_Bin = cut(Hour, breaks = seq(0, 24, by = 4), include.lowest = TRUE, right = FALSE, labels = c("0-4", "4-8", "8-12", "12-16", "16-20", "20-24"))
  ) %>%
  rename(PM25 = PM_2_5)

heatmap_data <- df %>%
  group_by(Day, Hour_Bin) %>%
  summarise(PM25 = mean(PM25, na.rm = TRUE)) %>%
  ungroup()

# Custom diverging scale at 50
ggplot(heatmap_data, aes(x = Hour_Bin, y = factor(Day), fill = PM25)) +
  geom_tile(color = "white", linewidth = 1.2, width = 0.95, height = 0.95) +
  scale_fill_gradientn(
    colors = c("blue", "lightblue", "yellow", "orange", "red"),
    values = scales::rescale(c(0, 25, 50, 100, 200)),  # make 50 a key breakpoint
    breaks = c(0, 25, 50, 100, 150, 200),
    limits = c(0, 200),
    name = "PM2.5 (µg/m³)"
  ) +
  labs(title = "Heatmap of PM2.5 Concentration (June 2023 - Whiteface)",
       x = "Time of Day (4-hour bins)", y = "Day of June") +
  theme_minimal(base_size = 16) +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    plot.title = element_text(size = 18, face = "bold"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )
