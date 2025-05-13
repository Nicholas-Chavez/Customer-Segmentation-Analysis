#load libraries
library(tidyverse)
library(lubridate)

#load data
setwd("C:/Users/Nicho/OneDrive/Desktop/Projects")
df <- read.csv("Online_Retail.csv")

# Data Cleaning
df2 <- df |> 
  drop_na(CustomerID) |> # removing columns with no customer ID
  filter(Quantity >0) |> # isolating positive and non-zero purchase quantities
  mutate(TotalPurchase = Quantity*UnitPrice) |> # adding total purchase column
  mutate(InvoiceDate = mdy_hm(InvoiceDate)) # Transform into proper date format
  

#RFM Analysis
snapshot_date <- max(df2$InvoiceDate) + days(1) # creating a reference date to be the most recent

rfm <- df2 |> 
  group_by(CustomerID) |> # Aggregating data by Customer ID
  summarise(
    Recency = as.numeric(snapshot_date - max(InvoiceDate), units = "days"),
                         Frequency = n_distinct(InvoiceNo),
                         Monetary  = sum(TotalPurchase)
  ) # Creating three summary columns for each Customer ID
# Recency: Shows how recent the latest purchase is from the snapshot_date
# Frequency: Shows how frequently an individual shops based on how many distinct invoices they have
# Monetary: Shows how much they are purchased in total

# Standardize data method 1: 

Standardize <- function(x){
  (x - min(x))/(max(x)-min(x))
} # Creating a function to standardize data

# Standardize all values for comparability
rfm_stz <- rfm |> 
  select(Recency, Frequency, Monetary) |> 
  mutate(Recency = Standardize(Recency)) |> 
  mutate(Frequency = Standardize(Frequency)) |> 
  mutate(Monetary = Standardize(Monetary))

# Standardize method 2: outliers are less influential
rfm_stz <- rfm |> mutate(across(c(Recency, Frequency, Monetary), ~ scale(.x)[, 1]))

# I chose to proceed with method 2 as not to be influenced by outliers
summary(rfm_stz)

# Assuming 'data' is your scaled numeric dataset (e.g., rfm_scaled)
wcss <- map_dbl(1:10, ~{
  kmeans(rfm_stz[, c("Recency", "Frequency", "Monetary")], centers = ., nstart = 25)$tot.withinss
})

# Create a tibble for plotting
wcss_data <- tibble(
  k = 1:10,
  wcss = wcss
)

ggplot(wcss_data, aes(k, wcss)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(color = "red", size = 3) +
  labs(
    title = "Elbow Method for Optimal k",
    x = "Number of Clusters (k)",
    y = "Within-Cluster Sum of Squares (WCSS)"
  ) +
  scale_x_continuous(breaks = 1:10) +
  theme_minimal()

# K-Mean clustering
set.seed(123)
km_out <- kmeans(rfm_stz, 4, nstart = 42)

km_out$cluster

km_out$centers

# Add cluster grouping to data
df3 <- rfm |> 
  mutate(cluster = factor(km_out$cluster))

# Altering data you see cluster characteristics
df4 <- df3 |> 
  group_by(cluster) |> 
  summarise(
    Recency = round(mean(Recency), 1),
    Frequency = round(mean(Frequency), 1),
    Monetary = round(mean(Monetary), 1)
  )

# Identify types
ggplot(data = df4, aes(x = Frequency, y = Monetary, color = cluster))+
  labs(title = "Customer Segments") +
  geom_point()


# Customer Types based of frequency and monetary spending

df5 <- df3 |> 
  mutate(
    Segment = case_when(
      Recency < 30 & Monetary > median(Monetary) ~ "High-Value",
      Recency > 90 ~ "Inactive",
      Frequency > median(Frequency) ~ "Loyal",
      TRUE ~ "Occasional"
    )
  )

# Determine engagement
ggplot(data = df5, aes(Frequency, Recency, color = Segment))+
  geom_point()

# Bar Chart: Average RFM values per cluster.
ggplot(df5, aes(Segment, Recency, fill=Segment)) + geom_boxplot()

# High-Value: Recently active and higher engagement frequency
# Loyal: Moderately active, with mid-range frequency
# Inactive: Not active with, mid-range to low frequency
# Occasional: Moderately active, with low frequency

Countries <- df |> 
  distinct(CustomerID, Country) 
  
enhanced_data <- df5 |> 
  left_join(Countries, by = "CustomerID")

# Double Check Duplicates
total_rows <- nrow(enhanced_data)
unique_row <- nrow(distinct(enhanced_data))
duplicate_count <- total_rows - unique_row
print(duplicate_count)

# Export Data 

write.csv(enhanced_data, "RFM_Analysis_Results_2.csv", row.names = FALSE)

