install.packages("dplyr")
install.packages("ggplot2")
library(ggplot2)
library(dplyr)
# Load data 
train <- read.csv("C:/Users/pc/Desktop/Uni/Data Science/Group CW/Hotel-A-train.csv")
# Prepare dates for line plots
train$Booking_date <- as.Date(train$Booking_date, format = "%m/%d/%Y")
train$Booking_Month <- as.numeric(format(train$Booking_date, "%m"))
#Plot 1-Age Distribution
library(scales)

library(ggplot2)
ggplot(train, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white", alpha = 0.9) +
  scale_x_continuous(breaks = seq(20, 70, by = 5)) +  # FIXED: Show every 5 years!
  scale_y_continuous(labels = comma) +
  labs(title = "Age Distribution of Hotel Guests",
       x = "Age (years)", 
       y = "Number of Guests") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
        axis.title = element_text(face = "bold", size = 16),
        axis.text = element_text(size = 14),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())
#Plot 2 -Room Rate Distribution
ggplot(train, aes(x = Room_Rate)) +
  geom_histogram(binwidth = 25, fill = "forestgreen", color = "white", alpha = 0.9) +
  scale_x_continuous(breaks = seq(100, 250, by = 25)) +
  scale_y_continuous(labels = comma) +
  labs(title = "Room Rate Distribution",
       x = "Room Rate ($ per night)", 
       y = "Number of Bookings") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
        axis.title = element_text(face = "bold", size = 16),
        axis.text = element_text(size = 14),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())
#Plot 3-Bar Chart/Hotel Type
hotel_counts <- as.data.frame(table(train$Hotel_Type))
names(hotel_counts) <- c("Hotel_Type", "Count")

ggplot(hotel_counts, aes(x = Hotel_Type, y = Count)) +
  geom_bar(stat = "identity", fill = "coral", color = "black", alpha = 0.9, width = 0.6) +
  geom_text(aes(label = comma(Count)), 
            vjust = -0.5, size = 7, fontface = "bold") +
  scale_y_continuous(labels = comma, limits = c(0, max(hotel_counts$Count) * 1.15)) +
  labs(title = "Distribution by Hotel Type",
       x = "Hotel Type", 
       y = "Number of Bookings") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
        axis.title = element_text(face = "bold", size = 16),
        axis.text = element_text(size = 14),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        panel.grid.major.x = element_blank())
#Plot 4-Bar Char/Deposit Type
deposit_counts <- as.data.frame(table(train$Deposit_type))
names(deposit_counts) <- c("Deposit_Type", "Count")

ggplot(deposit_counts, aes(x = Deposit_Type, y = Count)) +
  geom_bar(stat = "identity", fill = "gold", color = "black", alpha = 0.9, width = 0.6) +
  geom_text(aes(label = comma(Count)), 
            vjust = -0.5, size = 7, fontface = "bold") +
  scale_y_continuous(labels = comma, limits = c(0, max(deposit_counts$Count) * 1.15)) +
  labs(title = "Distribution by Deposit Type",
       x = "Deposit Type", 
       y = "Number of Bookings") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
        axis.title = element_text(face = "bold", size = 16),
        axis.text = element_text(size = 14),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        panel.grid.major.x = element_blank())
#Plot 5-Line Plot
bookings_by_month <- train %>%
  group_by(Booking_Month) %>%
  summarise(Count = n(), .groups = 'drop')

ggplot(bookings_by_month, aes(x = Booking_Month, y = Count)) +
  geom_area(fill = "lightblue", alpha = 0.5) +
  geom_line(color = "darkblue", size = 2.5) +
  geom_point(color = "red", size = 5) +
  scale_x_continuous(breaks = 1:12, 
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  scale_y_continuous(labels = comma) +
  labs(title = "Booking Volume by Month",
       subtitle = "Seasonal Pattern in Hotel Reservations",
       x = "Month", 
       y = "Number of Bookings") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
        plot.subtitle = element_text(size = 16, hjust = 0.5, color = "gray30"),
        axis.title = element_text(face = "bold", size = 16),
        axis.text = element_text(size = 14),
        panel.grid.minor = element_blank())

#Plot 6- Line Plot Room rate by month
rate_by_month <- train %>%
  group_by(Booking_Month) %>%
  summarise(Avg_Rate = mean(Room_Rate), .groups = 'drop')

ggplot(rate_by_month, aes(x = Booking_Month, y = Avg_Rate)) +
  geom_area(fill = "lightgreen", alpha = 0.5) +
  geom_line(color = "darkgreen", size = 2.5) +
  geom_point(color = "red", size = 5) +
  scale_x_continuous(breaks = 1:12, 
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  scale_y_continuous(labels = dollar) +
  labs(title = "Average Room Rate by Month",
       subtitle = "Seasonal Pricing Patterns",
       x = "Month", 
       y = "Average Room Rate") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
        plot.subtitle = element_text(size = 16, hjust = 0.5, color = "gray30"),
        axis.title = element_text(face = "bold", size = 16),
        axis.text = element_text(size = 14),
        panel.grid.minor = element_blank())

# Plot 7 - Booking Channel Distribution
channel_counts <- as.data.frame(table(train$Booking_channel))
names(channel_counts) <- c("Channel", "Count")

ggplot(channel_counts, aes(x = Channel, y = Count)) +
  geom_bar(stat = "identity", fill = "mediumpurple", color = "black", alpha = 0.9, width = 0.6) +
  geom_text(aes(label = comma(Count)), 
            vjust = -0.5, size = 7, fontface = "bold") +
  scale_y_continuous(labels = comma, limits = c(0, max(channel_counts$Count) * 1.15)) +
  labs(title = "Distribution by Booking Channel",
       x = "Booking Channel", 
       y = "Number of Bookings") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
        axis.title = element_text(face = "bold", size = 16),
        axis.text = element_text(size = 14),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        panel.grid.major.x = element_blank())


# Plot 8 - Pie Chart: Country Region Distribution
region_counts <- as.data.frame(table(train$Country_region))
names(region_counts) <- c("Region", "Count")
region_counts$Percentage <- round(region_counts$Count / sum(region_counts$Count) * 100, 1)

ggplot(region_counts, aes(x = "", y = Count, fill = Region)) +
  geom_bar(stat = "identity", width = 1, color = "white", size = 2) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("#e74c3c", "#3498db", "#2ecc71", "#f39c12")) +
  geom_text(aes(label = paste0(Percentage, "%")), 
            position = position_stack(vjust = 0.5),
            size = 7, fontface = "bold", color = "white") +
  labs(title = "Guest Distribution by Country Region",
       fill = "Region") +
  theme_void(base_size = 16) +
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5, margin = margin(b = 20)),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 12),
        legend.position = "right")

# Plot 9 - Horizontal Bar Chart: Ethnicity Distribution
ethnicity_counts <- as.data.frame(table(train$Ethnicity))
names(ethnicity_counts) <- c("Ethnicity", "Count")
ethnicity_counts <- ethnicity_counts[order(ethnicity_counts$Count), ]  # Sort for better visualization

ggplot(ethnicity_counts, aes(x = reorder(Ethnicity, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "brown", color = "black", alpha = 0.9) +
  geom_text(aes(label = comma(Count)), 
            hjust = -0.2, size = 6, fontface = "bold") +
  scale_y_continuous(labels = comma, limits = c(0, max(ethnicity_counts$Count) * 1.15)) +
  coord_flip() +  # Makes it horizontal
  labs(title = "Guest Distribution by Ethnicity",
       x = "Ethnicity", 
       y = "Number of Bookings") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
        axis.title = element_text(face = "bold", size = 16),
        axis.text = element_text(size = 14),
        panel.grid.major.y = element_blank())


#Further EDA
# Prepare dates and derived variables
train$Booking_date <- as.Date(train$Booking_date, format = "%m/%d/%Y")
train$Expected_checkin <- as.Date(train$Expected_checkin, format = "%m/%d/%Y")
train$Expected_checkout <- as.Date(train$Expected_checkout, format = "%m/%d/%Y")

train$Lead_Time <- as.numeric(train$Expected_checkin - train$Booking_date)
train$Length_of_Stay <- as.numeric(train$Expected_checkout - train$Expected_checkin)
train$Is_Cancelled <- ifelse(grepl("cancel", tolower(train$Reservation_Status)), 1, 0)
#Univariate Analysis
# 1. Lead Time Distribution Across Hotel Types
library(scales)
ggplot(train, aes(x = Lead_Time, fill = Hotel_Type)) +
  geom_histogram(binwidth = 15, color = "white", alpha = 0.9) +
  facet_wrap(~Hotel_Type, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = seq(0, 350, by = 50)) +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("#e74c3c", "#3498db", "#2ecc71")) +
  labs(title = "Lead Time Distribution by Hotel Type",
       x = "Lead Time (days)", 
       y = "Number of Bookings") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        axis.title = element_text(face = "bold", size = 16),
        axis.text = element_text(size = 14),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        strip.text = element_text(face = "bold", size = 14))


# 2. Length of Stay Distribution
ggplot(train, aes(x = Length_of_Stay)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white", alpha = 0.9) +
  scale_y_continuous(labels = comma) +  # Let R auto-scale x-axis
  labs(title = "Length of Stay Distribution",
       subtitle = "Number of Nights Booked",
       x = "Nights", 
       y = "Number of Bookings") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        axis.title = element_text(face = "bold", size = 16),
        axis.text = element_text(size = 14),
        panel.grid.minor = element_blank())

#Bivariate Analysis
# 3. Deposit Type vs Cancellation Rate
cancel_by_deposit <- train %>%
  group_by(Deposit_type) %>%
  summarise(
    Total = n(),
    Cancelled = sum(Is_Cancelled),
    Cancel_Rate = (Cancelled / Total) * 100,
    .groups = 'drop'
  )

ggplot(cancel_by_deposit, aes(x = Deposit_type, y = Cancel_Rate)) +
  geom_bar(stat = "identity",
           fill = c("#27ae60", "#c0392b", "#e67e22"),
           color = "black",
           alpha = 0.9,
           width = 0.5) +
  geom_text(aes(label = paste0(round(Cancel_Rate,1), "%")),
            vjust = -0.2,
            size = 3.5) +
  labs(title = "Cancellation Rate by Deposit Type",
       subtitle = "Key Predictor of Cancellation Behavior",
       x = "Deposit Type",
       y = "Cancellation Rate (%)") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 11),
    axis.text = element_text(size = 9),
    axis.text.x = element_text(angle = 30, hjust = 1),
    panel.grid.major.x = element_blank()
  )



# 4. Lead Time vs Cancellation Rate
train$Lead_Category <- cut(train$Lead_Time, 
                           breaks = c(0, 30, 90, 180, 365),
                           labels = c("0-30 days", "31-90 days", "91-180 days", "180+ days"))

cancel_by_lead <- train %>%
  group_by(Lead_Category) %>%
  summarise(
    Total = n(),
    Cancelled = sum(Is_Cancelled),
    Cancel_Rate = (Cancelled / Total) * 100,
    .groups = 'drop'
  )

ggplot(cancel_by_lead, aes(x = Lead_Category, y = Cancel_Rate)) +
  geom_bar(stat = "identity", fill = "yellow", color = "black", alpha = 0.9, width = 0.6) +
  geom_text(aes(label = paste0(round(Cancel_Rate, 1), "%")), 
            vjust = -0.3, size = 5, fontface = "bold") +  # slightly smaller text
  labs(title = "Cancellation Rate by Lead Time",
       subtitle = "Does Booking in Advance Increase Cancellation Risk?",
       x = "Lead Time Category", 
       y = "Cancellation Rate (%)") +
  theme_minimal(base_size = 14) +  # slightly smaller base size
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank())


# 5. Revenue Distribution: Cancelled vs Confirmed Bookings
train$Booking_Revenue <- train$Room_Rate * train$Length_of_Stay
train$Status_Label <- ifelse(train$Is_Cancelled == 1, "Cancelled", "Confirmed")

ggplot(train, aes(x = Booking_Revenue, fill = Status_Label)) +
  geom_histogram(binwidth = 100, alpha = 0.7, position = "identity", color = "white") +
  scale_fill_manual(values = c("Cancelled" = "#e74c3c", "Confirmed" = "#2ecc71")) +
  scale_x_continuous(labels = dollar, limits = c(0, 2500)) +
  scale_y_continuous(labels = comma) +
  labs(title = "Revenue Distribution: Cancelled vs Confirmed Bookings",
       subtitle = "Are High-Value Bookings More Likely to Cancel?",
       x = "Booking Revenue ($)", 
       y = "Number of Bookings",
       fill = "Status") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        axis.title = element_text(face = "bold", size = 16),
        axis.text = element_text(size = 14),
        panel.grid.minor = element_blank(),
        legend.position = "right")

# Multivariate  analysis
# 6. Lead Time + Deposit Type → Cancellation (Heatmap)
cancel_lead_deposit <- train %>%
  group_by(Lead_Category, Deposit_type) %>%
  summarise(Cancel_Rate = mean(Is_Cancelled) * 100, .groups = 'drop')

ggplot(cancel_lead_deposit, aes(x = Deposit_type, y = Lead_Category, fill = Cancel_Rate)) +
  geom_tile(color = "white", size = 1.5) +
  geom_text(aes(label = paste0(round(Cancel_Rate, 1), "%")), 
            color = "white", size = 7, fontface = "bold") +
  scale_fill_gradient2(low = "#2ecc71", mid = "#f39c12", high = "#e74c3c",
                       midpoint = 20, name = "Cancel\nRate (%)") +
  labs(title = "Cancellation Rate: Lead Time × Deposit Type",
       subtitle = "Identifying Highest Risk Booking Combinations",
       x = "Deposit Type", 
       y = "Lead Time Category") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        axis.title = element_text(face = "bold", size = 16),
        axis.text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1))

# 7. Booking Month × Deposit Type → Cancellation (Seasonal Pattern)
train$Booking_Month <- as.numeric(format(train$Booking_date, "%m"))

cancel_season_deposit <- train %>%
  group_by(Booking_Month, Deposit_type) %>%
  summarise(Cancel_Rate = mean(Is_Cancelled) * 100, .groups = 'drop')

ggplot(cancel_season_deposit, aes(x = Booking_Month, y = Cancel_Rate, color = Deposit_type, group = Deposit_type)) +
  geom_line(size = 2) +
  geom_point(size = 4) +
  scale_x_continuous(breaks = 1:12, 
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  scale_color_manual(values = c("#27ae60", "#c0392b", "#e67e22")) +
  labs(title = "Cancellation Rate: Seasonal Pattern by Deposit Type",
       x = "Booking Month", 
       y = "Cancellation Rate (%)",
       color = "Deposit Type") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        axis.title = element_text(face = "bold", size = 16),
        axis.text = element_text(size = 14),
        legend.position = "right")

# 8. Room Rate × Length of Stay → Revenue Risk
train$Rate_Category <- cut(train$Room_Rate,
                           breaks = c(0, 150, 200, 300),
                           labels = c("Budget (<$150)", "Mid ($150-$200)", "Premium (>$200)"))

train$Stay_Category <- cut(train$Length_of_Stay,
                           breaks = c(0, 2, 4, 10),
                           labels = c("Short (1-2)", "Medium (3-4)", "Long (5+)"))
cancel_revenue_risk <- train %>%
  group_by(Rate_Category, Stay_Category) %>%
  summarise(
    Cancel_Rate = mean(Is_Cancelled) * 100,
    Avg_Revenue = mean(Room_Rate * Length_of_Stay),
    .groups = 'drop'
  )
ggplot(cancel_revenue_risk, aes(x = Rate_Category, y = Stay_Category, fill = Cancel_Rate)) +
  geom_tile(color = "white", size = 1.5) +
  geom_text(aes(label = paste0(round(Cancel_Rate, 1), "%\n$", round(Avg_Revenue, 0))), 
            color = "white", size = 6, fontface = "bold") +
  scale_fill_gradient2(low = "#2ecc71", mid = "#f39c12", high = "#e74c3c",
                       midpoint = 15, name = "Cancel\nRate (%)") +
  labs(title = "Cancellation Rate & Revenue at Risk",
       subtitle = "Price Category × Length of Stay (Shows: Cancel % and Avg Revenue per Booking)",
       x = "Room Rate Category", 
       y = "Length of Stay (nights)") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
        plot.subtitle = element_text(size = 13, hjust = 0.5),
        axis.title = element_text(face = "bold", size = 16),
        axis.text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1))
# 9. Correlation Heatmap
install.packages("reshape2")
library(reshape2)
library(dplyr) 
library(ggplot2)
numeric_vars <- train %>%
  select(Age, Lead_Time, Length_of_Stay, Room_Rate, Discount_Rate, 
         Adults, Children, Babies, Is_Cancelled)

cor_matrix <- cor(numeric_vars, use = "complete.obs")
cor_melted <- melt(cor_matrix)

ggplot(cor_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white", size = 1) +
  geom_text(aes(label = round(value, 2)), size = 4, fontface = "bold") +
  scale_fill_gradient2(low = "#3498db", mid = "white", high = "#e74c3c",
                       midpoint = 0, limit = c(-1, 1), name = "Correlation") +
  labs(title = "Correlation Matrix: Numerical Variables",
       subtitle = "Relationships Between Key Predictors",
       x = "", y = "") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        panel.grid = element_blank())
# 10. Lead Time × Deposit Type × Hotel Type 
cancel_3way <- train %>%
  group_by(Lead_Category, Deposit_type, Hotel_Type) %>%
  summarise(Cancel_Rate = mean(Is_Cancelled) * 100, .groups = 'drop')

ggplot(cancel_3way, aes(x = Deposit_type, y = Lead_Category, fill = Cancel_Rate)) +
  geom_tile(color = "white", size = 1) +
  geom_text(aes(label = paste0(round(Cancel_Rate, 1), "%")), 
            color = "white", size = 5, fontface = "bold") +
  scale_fill_gradient2(low = "#2ecc71", mid = "#f39c12", high = "#e74c3c",
                       midpoint = 20, name = "Cancel\nRate (%)") +
  facet_wrap(~Hotel_Type, ncol = 3) +
  labs(title = "Cancellation Rate: Lead Time × Deposit Type × Hotel Type",
       x = "Deposit Type", 
       y = "Lead Time Category") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 13, hjust = 0.5),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 11),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(face = "bold", size = 14))
