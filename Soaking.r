#==============================================================================#
#                                                                              #
#                    PREY-SOAKING BEHAVIOR IN IBERIAN LYNX                     #
#        José Jiménez, Rafael Finat, Mario Fernández-Tizón, Pedro Peiró,       #
#         Javier Hernández-Hernández, Antoni Margalida, Emilio Virgós          #
#                            17:34 27/09/2025                                  #
#                                                                              #
#==============================================================================#

# Load required libraries
library(ggplot2)
library(viridis)
library(png)
library(grid)
library(patchwork)

setwd('...')

# Read data
temp <- read.table("Temp.txt", header=TRUE)
temp2 <- read.table("Hum.txt", header=TRUE)

# Filter data by panel
temp_sun <- subset(temp, Sun == "Sun")
temp_sun$Soak[temp_sun$Soak == "Y"] <- "Soaking in water, 15 seconds"
temp_sun$Soak[temp_sun$Soak == "N"] <- "Dry"
temp_shadow <- subset(temp, Sun == "Shadow")
temp_shadow$Soak[temp_shadow$Soak == "Y"] <- "Soaking in water, 30 seconds"
temp_shadow$Soak[temp_shadow$Soak == "N"] <- "Dry"

# Legends
soak_levels <- unique(temp$Soak)
soak_colors <- viridis::viridis(length(soak_levels), option = "H")
names(soak_colors) <- soak_levels

legend_labels <- expand.grid(
  Soak = soak_levels
)
legend_labels$time <- 30.5  # Adjust according to your x-axis
legend_labels$Temp <- 30.2  # Adjust according to your y-axis
legend_labels$label <- legend_labels$Soak


# Plot for "Sun"
ggplot(temp_sun, aes(x = time, y = Temp, color = Soak, fill = Soak)) +
  geom_smooth(method = "loess", span = 1, se = TRUE, alpha = 0.2, linewidth = 1) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  coord_cartesian(ylim = c(30, 41)) +
  scale_y_continuous(breaks = seq(30, 41, by = 2)) +
  labs(
    title = "Temperature over Time (Sun)",
    subtitle = "By Soak Duration",
    x = "Time (minutes)", y = "Temperature (°C)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = c(0.05, 0.05),  # Bottom-left corner
    legend.justification = c(0, 0),
    legend.background = element_rect(fill = "transparent", color = "white", linewidth = 0),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "white", color = NA)
  )

# Plot for "Shadow"
ggplot(temp_shadow, aes(x = time, y = Temp, color = Soak, fill = Soak)) +
  geom_smooth(method = "loess", span = 1, se = TRUE, alpha = 0.2, linewidth = 1) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  coord_cartesian(ylim = c(30, 41)) +
  scale_y_continuous(breaks = seq(30, 41, by = 2)) +
  labs(
    title = "Temperature over Time (Shadow)",
    subtitle = "By Soak Duration",
    x = "Time (minutes)", y = "Temperature (°C)", color = "Soak"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = c(0.05, 0.05),  # Bottom-left corner
    legend.justification = c(0, 0),
    legend.background = element_rect(fill = "transparent", color = "white", linewidth = 0),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "white", color = NA)
  )

# Humidity plot
# You can change the palette here if desired
ggplot(temp2, aes(x = time, y = rate, color = Condition, fill = Condition)) +
  geom_smooth(method = "loess", span = 1, se = TRUE, alpha = 0.2, linewidth = 1) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Water Retention in Rabbit Hair",
    subtitle = "Percentage over Time by Condition",
    x = "Time (minutes)", y = "Water as Percentage of Rabbit Weight", color = "Condition"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = c(0.95, 0.4), # Top-right corner
    legend.justification = c(1, 0),
    legend.background = element_rect(fill = "transparent", color = "white", linewidth = 0),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, color = "black"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "white", color = NA)
  )
