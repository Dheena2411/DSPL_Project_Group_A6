# EDA - Test Dataset | Hotel Chain A
# 5DATA004C Data Science Project Lifecycle

# Load required libraries
library(ggplot2)
library(dplyr)
library(scales)

# STEP 2: Age Distribution

ggplot(test, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white", alpha = 0.9) +
  scale_x_continuous(breaks = seq(20, 70, by = 5)) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Age Distribution of Hotel Guests (Test Set)",
    x = "Age (years)",
    y = "Number of Guests"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 16),
    axis.text = element_text(size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

# STEP 3: Room Rate Distribution
ggplot(test, aes(x = Room_Rate)) +
  geom_histogram(binwidth = 25, fill = "forestgreen", color = "white", alpha = 0.9) +
  scale_x_continuous(breaks = seq(100, 250, by = 25)) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Room Rate Distribution (Test Set)",
    x = "Room Rate ($ per night)",
    y = "Number of Bookings"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 16),
    axis.text = element_text(size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

# STEP 4: Distribution by Hotel Type
hotel_counts <- as.data.frame(table(test$Hotel_Type))
names(hotel_counts) <- c("Hotel_Type", "Count")

ggplot(hotel_counts, aes(x = Hotel_Type, y = Count)) +
  geom_bar(stat = "identity", fill = "coral", color = "black", alpha = 0.9, width = 0.6) +
  geom_text(aes(label = comma(Count)), vjust = -0.5, size = 7, fontface = "bold") +
  scale_y_continuous(labels = comma, limits = c(0, max(hotel_counts$Count) * 1.15)) +
  labs(
    title = "Distribution by Hotel Type (Test Set)",
    x = "Hotel Type",
    y = "Number of Bookings"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 16),
    axis.text = element_text(size = 14),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid.major.x = element_blank()
  )

# STEP 5: Distribution by Deposit Type
deposit_counts <- as.data.frame(table(test$Deposit_type))
names(deposit_counts) <- c("Deposit_Type", "Count")

ggplot(deposit_counts, aes(x = Deposit_Type, y = Count)) +
  geom_bar(stat = "identity", fill = "gold", color = "black", alpha = 0.9, width = 0.6) +
  geom_text(aes(label = comma(Count)), vjust = -0.5, size = 7, fontface = "bold") +
  scale_y_continuous(labels = comma, limits = c(0, max(deposit_counts$Count) * 1.15)) +
  labs(
    title = "Distribution by Deposit Type (Test Set)",
    x = "Deposit Type",
    y = "Number of Bookings"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 16),
    axis.text = element_text(size = 14),
    panel.grid.major.x = element_blank()
  )

# STEP 6: Distribution by Booking Channel
channel_counts <- as.data.frame(table(test$Booking_channel))
names(channel_counts) <- c("Channel", "Count")

ggplot(channel_counts, aes(x = Channel, y = Count)) +
  geom_bar(stat = "identity", fill = "mediumpurple", color = "black", alpha = 0.9, width = 0.6) +
  geom_text(aes(label = comma(Count)), vjust = -0.5, size = 7, fontface = "bold") +
  scale_y_continuous(labels = comma, limits = c(0, max(channel_counts$Count) * 1.15)) +
  labs(
    title = "Distribution by Booking Channel (Test Set)",
    x = "Booking Channel",
    y = "Number of Bookings"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 16),
    axis.text = element_text(size = 14),
    panel.grid.major.x = element_blank()
  )

# STEP 7: Guest Distribution by Country Region (Pie Chart)
region_counts <- as.data.frame(table(test$Country_region))
names(region_counts) <- c("Region", "Count")
region_counts$Percentage <- round(region_counts$Count / sum(region_counts$Count) * 100, 1)

ggplot(region_counts, aes(x = "", y = Count, fill = Region)) +
  geom_bar(stat = "identity", width = 1, color = "white", size = 2) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(Percentage, "%")),
            position = position_stack(vjust = 0.5),
            size = 7, fontface = "bold", color = "white") +
  scale_fill_manual(values = c("#e74c3c", "#3498db", "#2ecc71", "#f39c12")) +
  labs(
    title = "Guest Distribution by Country Region (Test Set)",
    fill = "Region"
  ) +
  theme_void(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5, margin = margin(b = 20)),
    legend.title = element_text(face = "bold", size = 14),
    legend.text = element_text(size = 12),
    legend.position = "right"
  )

# STEP 8: Guest Distribution by Ethnicity (Horizontal Bar)
ethnicity_counts <- as.data.frame(table(test$Ethnicity))
names(ethnicity_counts) <- c("Ethnicity", "Count")
ethnicity_counts <- ethnicity_counts[order(ethnicity_counts$Count), ]

ggplot(ethnicity_counts, aes(x = reorder(Ethnicity, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "brown", color = "black", alpha = 0.9) +
  geom_text(aes(label = comma(Count)), hjust = -0.2, size = 6, fontface = "bold") +
  scale_y_continuous(labels = comma, limits = c(0, max(ethnicity_counts$Count) * 1.15)) +
  coord_flip() +
  labs(
    title = "Guest Distribution by Ethnicity (Test Set)",
    x = "Ethnicity",
    y = "Number of Bookings"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 16),
    axis.text = element_text(size = 14),
    panel.grid.major.y = element_blank()
  )

# STEP 9: Booking Volume by Month (Line Plot)
test$Booking_date <- as.Date(test$Booking_date, format = "%m/%d/%Y")
test$Booking_Month <- as.numeric(format(test$Booking_date, "%m"))

bookings_by_month <- test %>%
  group_by(Booking_Month) %>%
  summarise(Count = n(), .groups = "drop")

ggplot(bookings_by_month, aes(x = Booking_Month, y = Count)) +
  geom_area(fill = "lightblue", alpha = 0.5) +
  geom_line(color = "darkblue", linewidth = 2.5) +
  geom_point(color = "red", size = 5) +
  scale_x_continuous(breaks = 1:12,
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Booking Volume by Month (Test Set)",
    subtitle = "Seasonal Pattern in Hotel Reservations",
    x = "Month",
    y = "Number of Bookings"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5, color = "gray30"),
    axis.title = element_text(face = "bold", size = 16),
    axis.text = element_text(size = 14),
    panel.grid.minor = element_blank()
  )

# STEP 10: Average Room Rate by Month (Line Plot)
rate_by_month <- test %>%
  group_by(Booking_Month) %>%
  summarise(Avg_Rate = mean(Room_Rate), .groups = "drop")

ggplot(rate_by_month, aes(x = Booking_Month, y = Avg_Rate)) +
  geom_area(fill = "lightgreen", alpha = 0.5) +
  geom_line(color = "darkgreen", linewidth = 2.5) +
  geom_point(color = "red", size = 5) +
  scale_x_continuous(breaks = 1:12,
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  scale_y_continuous(labels = dollar) +
  labs(
    title = "Average Room Rate by Month (Test Set)",
    subtitle = "Seasonal Pricing Patterns",
    x = "Month",
    y = "Average Room Rate"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5, color = "gray30"),
    axis.title = element_text(face = "bold", size = 16),
    axis.text = element_text(size = 14),
    panel.grid.minor = element_blank()
  )
