# Required libraries
library(dplyr) 
library(writexl)
library(ggplot2)
library(tidyr)
library(dplyr)
data <- read.csv("C:/Users/tanji/OneDrive/Desktop/preprocess/Nov21_8to1.csv")
View(data)

###############################################################################################
#Date_November_21 yielding percentage: Corner vs Median
  
#Data
data <- data.frame(
  Time = c("8AM-9AM", "9AM-10AM", "10AM-11AM", "11AM-12PM", 
           "12PM-1PM", "Overall"),
  Yielding_Percent_Corner = c(0.00, 38.89, 70.97, 17.86, 43.26, 34.19),
  Yielding_Percent_Median = c(37.50,53.33, 86.90, 67.86, 67.71, 62.66))

# Set time order with "Overall" at the end
data <- data %>%
  mutate(Time = factor(Time, levels = c("8AM-9AM", "9AM-10AM", "10AM-11AM",
                                        "11AM-12PM", "12PM-1PM","Overall")))

# Reshape data to long format
data_long <- data %>%
  pivot_longer(
    cols = c(Yielding_Percent_Corner, Yielding_Percent_Median),
    names_to = "Metric",
    values_to = "Percentage"
  )

# Create the plot
ggplot(data_long, aes(x = Time, y = Percentage, group = Metric, color = Metric)) +
  geom_line(size = 2) +
  geom_point(size = 3.5) +
  # Blue labels above points
  geom_text(
    data = subset(data_long, Metric == "Yielding_Percent_Corner"),
    aes(label = sprintf("%.1f%%", Percentage)),
    position = position_nudge(y = -13),
    color = "#1f77b4",
    size = 3.5,
    show.legend = FALSE
  ) +
  # Orange labels below lines
  geom_text(
    data = subset(data_long, Metric == "Yielding_Percent_Median"),
    aes(label = sprintf("%.1f%%", Percentage)),
    position = position_nudge(y = 13),
    color = "#ff7f0e",
    size = 3.5,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c("Yielding_Percent_Corner" = "#1f77b4", "Yielding_Percent_Median" = "#ff7f0e"),
    labels = c("Corner Yielding %", "Median Yielding %")
  ) +
  labs(
    title = "Yielding Percentage Comparison: Corner vs Median",
    x = "Time Interval",
    y = "Yielding Percentage (%)",
    color = "Metric"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 12),
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, size = 16)
  ) +
  scale_y_continuous(limits = c(0, 100)) 

##########################################################################################
# Average wait time comparison for all pedestrians:Corner vs Median on November 21
# Data
data <- data.frame(
  Time = c("8AM-9AM", "9AM-10AM", "10AM-11AM", "11AM-12PM", 
           "12PM-1PM", "Overall"),
  Avg_Wait_Corner = c(6,4.17,2.31, 3.79,5.97,4.45),
  Avg_Wait_Median = c(3.00,2.11,1.71,3.00,2.5,2.46
  )
                      
)

# Set time order with "Overall" at the end
data <- data %>%
  mutate(Time = factor(Time, levels = c("8AM-9AM", "9AM-10AM", "10AM-11AM", "11AM-12PM", 
                                        "12PM-1PM", "Overall")))

# Reshape data to long format
data_long <- data %>%
  pivot_longer(
    cols = c(Avg_Wait_Corner, Avg_Wait_Median),
    names_to = "Metric",
    values_to = "Wait_Time"
  )

# Create the plot
ggplot(data_long, aes(x = Time, y = Wait_Time, group = Metric, color = Metric)) +
  geom_line(size = 2) +
  geom_point(size = 3.5) +
  # Blue labels above points
  geom_text(
    data = subset(data_long, Metric == "Avg_Wait_Corner"),
    aes(label = sprintf("%.1f", Wait_Time)),
    position = position_nudge(y = 1.3),
    color = "#1f77b4",
    size = 4,
    show.legend = FALSE
  ) +
  # Orange labels below lines
  geom_text(
    data = subset(data_long, Metric == "Avg_Wait_Median"),
    aes(label = sprintf("%.1f", Wait_Time)),
    position = position_nudge(y = -1.3),
    color = "#ff7f0e",
    size = 4,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c("Avg_Wait_Corner" = "#1f77b4", "Avg_Wait_Median" = "#ff7f0e"),
    labels = c("Corner (All Pedestrians)", "Median (All Pedestrians)")
  ) +
  labs(
    title = "Average Wait Time Comparison: Corner vs Median",
    x = "Time Interval",
    y = "Wait Time (seconds)",
    color = "Location"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, size = 16)
  ) +
  scale_y_continuous(limits = c(0, 15))

###############################################################################################
# Average wait time comparison for drivers with opportunity:Corner vs Median on November 21
# Data
data <- data.frame(
  Time = c("8AM-9AM", "9AM-10AM", "10AM-11AM", "11AM-12PM", 
           "12PM-1PM", "Overall"),
  Avg_Wait_Corner_Drivers = c(7.20,6.82, 4.47,6.55,8.41,6.69),
                          
  Avg_Wait_Median_Car = c(6.00,3.17,3.12, 4.75,4.19,4.24 
  )
  
)

library(ggthemes)
# Set time order with "Overall" at the end
data <- data %>%
  mutate(Time = factor(Time, levels = c("8AM-9AM", "9AM-10AM", "10AM-11AM", "11AM-12PM", 
                                        "12PM-1PM", "Overall")))

# Reshape data to long format
data_long <- data %>%
  pivot_longer(
    cols = c(Avg_Wait_Corner_Drivers, Avg_Wait_Median_Car),
    names_to = "Metric",
    values_to = "Wait_Time"
  )

ggplot(data_long, aes(x = Time, y = Wait_Time, group = Metric, color = Metric)) +
  geom_line(size = 2) +
  geom_point(size = 3.5) +
  # Blue labels above points
  geom_text(
    data = subset(data_long, Metric == "Avg_Wait_Corner_Drivers"),
    aes(label = sprintf("%.1f", Wait_Time)),
    position = position_nudge(y = 0.7),
    color = "#1f77b4",  # Force blue color
    size = 4,
    show.legend = FALSE
  ) +
  # Orange labels below lines
  geom_text(
    data = subset(data_long, Metric == "Avg_Wait_Median_Car"),
    aes(label = sprintf("%.1f", Wait_Time)),
    position = position_nudge(y = -0.7),
    color = "#ff7f0e",  # Force orange color
    size = 4,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c("Avg_Wait_Corner_Drivers" = "#1f77b4", "Avg_Wait_Median_Car" = "#ff7f0e"),
    labels = c("Corner (Drivers with Opportunity)", "Median (Drivers with Opportunity)")
  ) +
  labs(
    title = "Average Wait Time Comparison: Corner vs Median",
    x = "Time Interval",
    y = "Wait Time (seconds)",
    color = "Metric"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size=12),
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, size = 16)
  ) +
  scale_y_continuous(limits = c(0, 15))
#######################################################################################################
  #Date_August_28 yielding percentage: Corner vs Median
  
  # Input data (including Overall)
  data <- data.frame(
    Time = c("8AM-9AM", "9AM-10AM", "10AM-11AM", "11AM-12PM", 
             "12PM-1PM", "Overall"),
    Yielding_Percent_Corner = c(0.00, 30.56, 0.00, 24.69, 9.78, 25.84),
    Yielding_Percent_Median = c(14.29, 6.90, 55.56, 64.81, 45.16, 65.98)
  )

# Set time order with "Overall" at the end
data <- data %>%
  mutate(Time = factor(Time, levels = c("8AM-9AM", "9AM-10AM", "10AM-11AM", 
                                        "11AM-12PM", "12PM-1PM", "Overall")))

# Reshape data to long format
data_long <- data %>%
  pivot_longer(
    cols = c(Yielding_Percent_Corner, Yielding_Percent_Median),
    names_to = "Metric",
    values_to = "Percentage"
  )

# Create the plot
ggplot(data_long, aes(x = Time, y = Percentage, group = Metric, color = Metric)) +
  geom_line(size = 2) +
  geom_point(size = 3.5) +
  
  # Blue labels for Corner (move slightly above points)
  geom_text(
    data = subset(data_long, Metric == "Yielding_Percent_Corner"),
    aes(label = sprintf("%.1f%%", Percentage)),
    vjust = 1.8,  # Move slightly below point
    color = "#1f77b4",
    size = 3.5,
    show.legend = FALSE
  ) +
  
  # Orange labels for Median (move slightly above points)
  geom_text(
    data = subset(data_long, Metric == "Yielding_Percent_Median"),
    aes(label = sprintf("%.1f%%", Percentage)),
    vjust = -1.8,  # Move slightly above point
    color = "#ff7f0e",
    size = 3.5,
    show.legend = FALSE
  ) +
  
  scale_color_manual(
    values = c("Yielding_Percent_Corner" = "#1f77b4", 
               "Yielding_Percent_Median" = "#ff7f0e"),
    labels = c("Corner Yielding %", "Median Yielding %")
  ) +
  
  labs(
    title = "Yielding Percentage Comparison: Corner vs Median",
    x = "Time Interval",
    y = "Yielding Percentage (%)",
    color = "Metric"
  ) +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 12),
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, size = 16)
  ) +
  
  scale_y_continuous(limits = c(0, 85)) 
#############################################################################################
  # Average wait time comparison for all pedestrians:Corner vs Median on August 28
  # Input data (including Overall)
  data <- data.frame(
    Time = c("8AM-9AM", "9AM-10AM", "10AM-11AM", "11AM-12PM", 
             "12PM-1PM", "Overall"),
    Avg_Wait_Corner = c(0.86, 5.15, 4.46, 6.52, 6.45, 5.05),
    Avg_Wait_Median = c(1.00, 3.12, 1.31, 1.81, 2.21, 1.54)
  )

# Set time order with "Overall" at the end
data <- data %>%
  mutate(Time = factor(Time, levels = c("8AM-9AM", "9AM-10AM", "10AM-11AM", "11AM-12PM", 
                                        "12PM-1PM", "Overall")))

# Reshape data to long format
data_long <- data %>%
  pivot_longer(
    cols = c(Avg_Wait_Corner, Avg_Wait_Median),
    names_to = "Metric",
    values_to = "Wait_Time"
  )

# Create the plot
ggplot(data_long, aes(x = Time, y = Wait_Time, group = Metric, color = Metric)) +
  geom_line(size = 2) +
  geom_point(size = 3.5) +
  
  # Blue labels (Corner)
  geom_text(
    data = subset(data_long, Metric == "Avg_Wait_Corner"),
    aes(label = sprintf("%.1f", Wait_Time)),
    vjust = -1.2,  # Move slightly above point
    color = "#1f77b4",
    size = 4,
    show.legend = FALSE
  ) +
  
  # Orange labels (Median)
  geom_text(
    data = subset(data_long, Metric == "Avg_Wait_Median"),
    aes(label = sprintf("%.1f", Wait_Time)),
    vjust = 1.8,   # Move slightly below point
    color = "#ff7f0e",
    size = 4,
    show.legend = FALSE
  ) +
  
  scale_color_manual(
    values = c("Avg_Wait_Corner" = "#1f77b4", "Avg_Wait_Median" = "#ff7f0e"),
    labels = c("Corner (All Pedestrians)", "Median (All Pedestrians)")
  ) +
  
  labs(
    title = "Average Wait Time Comparison: Corner vs Median",
    x = "Time Interval",
    y = "Wait Time (seconds)",
    color = "Location"
  ) +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, size = 16)
  ) +
  
  scale_y_continuous(limits = c(0, 15))

################################################################################################
  # Average wait time comparison for drivers with opportunity:Corner vs Median on August 28
  # Data
  data <- data.frame(
    Time = c("8AM-9AM", "9AM-10AM", "10AM-11AM", "11AM-12PM", 
             "12PM-1PM", "Overall"),
    Avg_Wait_Corner_Drivers = c(6.00, 10.95, 9.67, 10.10, 9.84, 7.45),
    Avg_Wait_Median_Car = c(3.50, 7.57, 3.40, 2.67, 7.11, 2.02)
  )

data <- data %>%
  mutate(Time = factor(Time, levels = c("8AM-9AM", "9AM-10AM", "10AM-11AM", "11AM-12PM", 
                                        "12PM-1PM", "Overall")))

# Reshape data
data_long <- data %>%
  pivot_longer(
    cols = c(Avg_Wait_Corner_Drivers, Avg_Wait_Median_Car),
    names_to = "Metric",
    values_to = "Wait_Time"
  )

ggplot(data_long, aes(x = Time, y = Wait_Time, group = Metric, color = Metric)) +
  geom_line(size = 2) +
  geom_point(size = 3.5) +
  # Blue labels above points
  geom_text(
    data = subset(data_long, Metric == "Avg_Wait_Corner_Drivers"),
    aes(label = sprintf("%.1f", Wait_Time)),
    position = position_nudge(y = 0.7),
    color = "#1f77b4",  # Force blue color
    size = 4,
    show.legend = FALSE
  ) +
 
  geom_text(
    data = subset(data_long, Metric == "Avg_Wait_Median_Car"),
    aes(label = sprintf("%.1f", Wait_Time)),
    position = position_nudge(y = -0.7),
    color = "#ff7f0e",  # Force orange color
    size = 4,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c("Avg_Wait_Corner_Drivers" = "#1f77b4", "Avg_Wait_Median_Car" = "#ff7f0e"),
    labels = c("Corner (Drivers with Opportunity)", "Median (Drivers with Opportunity)")
  ) +
  labs(
    title = "Average Wait Time Comparison: Corner vs Median",
    x = "Time Interval",
    y = "Wait Time (seconds)",
    color = "Metric"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size=12),
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, size = 16)
  ) +
  scale_y_continuous(limits = c(0, 15)) 
##########################################################################################
  # BOXPLOT(YIELDING RATE on both August 28 and November 21)

# --- August 28 Data ---
data_august <- data.frame(
  Date = "August 28",
  Time = c("8AM-9AM", "9AM-10AM", "10AM-11AM", "11AM-12PM", "12PM-1PM", "Overall"),
  Yielding_Corner = c(0.00, 30.56, 0.00, 24.69, 9.78, 25.84),
  Yielding_Median = c(14.29, 6.90, 55.56, 64.81, 45.16, 65.98)
)

# --- November 21 Data ---
data_november <- data.frame(
  Date = "November 21",
  Time = c("8AM-9AM", "9AM-10AM", "10AM-11AM", "11AM-12PM", "12PM-1PM", "Overall"),
  Yielding_Corner = c(0.00, 38.89, 70.97, 17.86, 43.26, 34.19),
  Yielding_Median = c(37.50, 53.33, 86.90, 67.86, 67.71, 62.66)
)

# --- Combine both dates ---
combined_data <- bind_rows(data_august, data_november)

# --- Reshape into long format ---
combined_long <- combined_data %>%
  pivot_longer(
    cols = c(Yielding_Corner, Yielding_Median),
    names_to = "Location",
    values_to = "Yielding_Percentage"
  )

# --- Create Boxplot ---
ggplot(combined_long, aes(x = Time, y = Yielding_Percentage, fill = Location)) +
  geom_boxplot(outlier.shape = 21, outlier.fill = "red", outlier.color = "black") +
  labs(
    title = "Distribution of Yielding Percentage by Time Interval",
    subtitle = "Comparison between Corner and Median (Aug 28 & Nov 21)",
    x = "Time Interval",
    y = "Yielding Percentage (%)",
    fill = "Crossing Location"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    plot.title = element_text(hjust = 0.5, size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 14)
  ) +
  scale_fill_manual(values = c("Yielding_Corner" = "#1f77b4", "Yielding_Median" = "#ff7f0e")) +
  scale_y_continuous(limits = c(0, 100))

#####################################################################################################
#ANOVA Test
# Data
data_august <- data.frame(
  Date = "August 28",
  Time = c("8AM-9AM", "9AM-10AM", "10AM-11AM", "11AM-12PM", "12PM-1PM", "Overall"),
  Yielding_Corner = c(0.00, 30.56, 0.00, 24.69, 9.78, 25.84),
  Yielding_Median = c(14.29, 6.90, 55.56, 64.81, 45.16, 65.98)
)

data_november <- data.frame(
  Date = "November 21",
  Time = c("8AM-9AM", "9AM-10AM", "10AM-11AM", "11AM-12PM", "12PM-1PM", "Overall"),
  Yielding_Corner = c(0.00, 38.89, 70.97, 17.86, 43.26, 34.19),
  Yielding_Median = c(37.50, 53.33, 86.90, 67.86, 67.71, 62.66)
)

# Combine
combined_data <- bind_rows(data_august, data_november)

combined_long <- combined_data %>%
  pivot_longer(
    cols = c(Yielding_Corner, Yielding_Median),
    names_to = "Location",
    values_to = "Yielding_Percentage"
  ) %>%
  filter(Time != "Overall")

# ANOVA (interaction between Time and Location)
anova_model <- aov(Yielding_Percentage ~ Time * Location, data = combined_long)
summary(anova_model)

anova_with_date <- aov(Yielding_Percentage ~ Date + Time + Location, data = combined_long)
summary(anova_with_date)
###################################################################################################
# Average yielding percentage by date and location
# Group data 
mean_data <- combined_long %>%
  group_by(Date, Location) %>%
  summarise(
    Mean_Yield = mean(Yielding_Percentage),
    SD_Yield = sd(Yielding_Percentage),
    .groups = "drop"
  )

# Plot
ggplot(mean_data, aes(x = Date, y = Mean_Yield, fill = Location)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = Mean_Yield - SD_Yield, ymax = Mean_Yield + SD_Yield),
                position = position_dodge(0.9), width = 0.2) +
  labs(
    title = "Average Yielding Percentage by Date and Location",
    x = "Date",
    y = "Average Yielding %",
    fill = "Crossing Location"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 13)
  )
######################################################################################################
  ### 1st map, part 1###
  
  # Libraries
library(tigris)
library(sf)
library(leaflet)

# Boundary of Kent city
kent_city <- tigris::places(state = "OH", cb = TRUE)
kent_city <- kent_city[kent_city$NAME == "Kent", ]

map_kent <- ggplot() +
  geom_sf(data = kent_city, fill = "transparent", color = "blue", lwd = 2) +
  geom_text(aes(x = -81.3574, y = 41.1480, label = "Kent"), color = "red", size = 5) +
  
  coord_sf(xlim = c(-81.4, -81.3), ylim = c(41.1, 41.2), expand = FALSE) +  # Adjusted zoom on Kent city
  theme_minimal() +
  labs(title = "Zoomed-In View of Kent, Ohio") +
  theme(plot.title = element_text(hjust = 0.5))

print(map_kent)

### 1st map, part 2###

kent_city <- tigris::places(state = "OH", cb = TRUE)
kent_city <- kent_city[kent_city$NAME == "Kent", ]
kent_coords <- c(-81.3574, 41.1480)

map_kent <- leaflet() %>%
  addTiles() %>%  
  setView(lng = kent_coords[1], lat = kent_coords[2], zoom = 13.3) %>%
  
  addPolygons(data = kent_city, color = "blue", weight = 3, fillOpacity = 0, label = "Kent City Boundary") %>%
  
  addLabelOnlyMarkers(lng = kent_coords[1], lat = kent_coords[2], 
                      label = "Kent", 
                      labelOptions = labelOptions(noHide = TRUE, direction = "center"))
map_kent

#################################################################################################

### 2nd map###

# Coordinates for the camera and midblock crossing
camera_coords <- c(-81.349764, 41.153882) 
crossing_coords <- c(-81.349767, 41.153745)


map_intersection <- leaflet() %>%
  addTiles() %>%  
  setView(lng = -81.349765, lat = 41.153815, zoom = 18) %>%  
  
  addMarkers(lng = camera_coords[1], lat = camera_coords[2], popup = "Camera Location") %>%
  
  addMarkers(lng = crossing_coords[1], lat = crossing_coords[2], popup = "Midblock Crossing") %>%
  
  addLabelOnlyMarkers(lng = crossing_coords[1], lat = crossing_coords[2] + -0.00002,
                      label = "Midblock Crossing", 
                      labelOptions = labelOptions(noHide = TRUE, direction = "bottom", textsize = "12px")) %>%
  
  addLabelOnlyMarkers(lng = camera_coords[1], lat = camera_coords[2] + 0.00009,
                      label = "Camera", 
                      labelOptions = labelOptions(noHide = TRUE, direction = "top", textsize = "12px"))

# Display the map with the camera and midblock crossing markers
map_intersection
