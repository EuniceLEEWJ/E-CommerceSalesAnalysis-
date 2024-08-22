
# Set Working Directory
setwd("C:/Users/User/OneDrive - Monash University/Desktop/Projects/E-Cormmerces Sales Trends")


# Remove/Clean the environment
rm(list=ls()) 

# Load required libraries
library(tidyverse)
library(ggplot2)
library(tidyr)
library(dplyr)

# Load the provided datasets
amazon_sales <- read.csv("Amazon Sale Report.csv")
sale_report <- read.csv("Sale Report.csv")
may_2022 <- read.csv("May-2022.csv")
pl_march_2021 <- read.csv("P  L March 2021.csv")

################################################################################
#                            Inspect datasets                                  #
#------------------------------------------------------------------------------#
################################################################################
# Understand the column variables datatypes and dimensions of the datasets
str(amazon_sales)
str(sale_report)
str(may_2022)
str(pl_march_2021)

# View the columns name
names(amazon_sales)
names(sale_report)
names(may_2022)
names(pl_march_2021)

# View the datasets to visually inspect the datasets
View(amazon_sales) 
View(sale_report)
View(may_2022)
View(pl_march_2021)

# Get Summary Statistics of each datasets
summary(amazon_sales)
summary(sale_report)
summary(pl_march_2021)
summary(may_2022)

# 1. Explain the size, structure, and distribution of the dataset.
# 2. summary() function provides a statistical summary of the datasets
# ^^^ minimum, maximum, median, mean, and quartiles for numerical data, and frequency counts for factors
# 3. Can assess the quality of the data: 
# identify missing values, and detect possible outliers or anomalies that may require data cleaning
# 4. By knowing the data types and structures I have information on I have to handle date-time data before any plotting or analysis
# 5. By reviewing the column names and the data types, I can know which columns might be relevant for the plotting or analysis 
# 6. I wanted to plot boxplot to look at the distribution of numerical attributes but 
# from str() it is known that most attributes are of chr datatype so I didn't plot

################################################################################
#                              Data cleaning                                   #
#------------------------------------------------------------------------------#
################################################################################
################# (1) Convert selected columns to appropriate data type 
# Format required columns from character to numerical formats'
pl_march_2021$TP.1 <- as.numeric(pl_march_2021$TP.1)
pl_march_2021$TP.2 <- as.numeric(pl_march_2021$TP.2)
may_2022$TP <- as.numeric(may_2022$TP)

# compile all MRP columns to call later during reshaping
mrp_columns <- c("Ajio.MRP", "Amazon.MRP", "Amazon.FBA.MRP", "Flipkart.MRP", 
                 "Limeroad.MRP", "Myntra.MRP", "Paytm.MRP", "Snapdeal.MRP")

# Ensure MRP columns are numeric and gather them into a single column
pl_march_2021 <- pl_march_2021 %>%
  mutate(across(all_of(mrp_columns), as.numeric))
may_2022 <- may_2022 %>%
  mutate(across(all_of(mrp_columns), as.numeric))

# Convert Date column of character data type to Date data types for amazon_sales
amazon_sales$Date <- as.Date(amazon_sales$Date, format = "%m-%d-%y")

#################(2) Check for missing values for each datasets
# Check for amazon_sales
colSums(is.na(amazon_sales))

# Check for sale_report
colSums(is.na(sale_report))

# Check for pl_march_2021
colSums(is.na(pl_march_2021))

# Check for may_2022
colSums(is.na(may_2022))

#### Remove rows with missing values for each datasets
amazon_sales_cleaned <- na.omit(amazon_sales)
sale_report_cleaned <- na.omit(sale_report)
pl_march_2021_cleaned <- na.omit(pl_march_2021)
may_2022_cleaned <- na.omit(may_2022)

#################(3) Check for duplicates for each datasets
# Count the number of duplicate rows
# sale_report_cleaned
if (sum(duplicated(sale_report_cleaned)) == 0) {
  paste("There are no duplicate values in sale_report_cleaned")
} else {
  paste("Number of duplicate values in sale_report_cleaned:", sum(duplicated(sale_report_cleaned)))
}
# amazon_sales_cleaned
if (sum(duplicated(amazon_sales_cleaned)) == 0) {
  paste("There are no duplicate values in amazon_sales_cleaned")
} else {
  paste("Number of duplicate values in amazon_sales_cleaned:", sum(duplicated(amazon_sales_cleaned)))
}
# pl_march_2021_cleaned
if (sum(duplicated(pl_march_2021_cleaned)) == 0) {
  paste("There are no duplicate values in pl_march_2021_cleaned")
} else {
  paste("Number of duplicate values in pl_march_2021_cleaned:", sum(duplicated(pl_march_2021_cleaned)))
}
# may_2022_cleaned
if (sum(duplicated(may_2022_cleaned)) == 0) {
  paste("There are no duplicate values in may_2022_cleaned")
} else {
  paste("Number of duplicate values in may_2022_cleaned:", sum(duplicated(may_2022_cleaned)))
}

### Conclusion: NO duplicates row so no 'remove duplicate row' action is done

# Export cleaned dataset
write.csv(amazon_sales_cleaned, file = "amazon_sales_cleaned.csv", row.names = FALSE)
write.csv(sale_report_cleaned, file = "sale_report_cleaned.csv", row.names = FALSE)
write.csv(pl_march_2021_cleaned, file = "pl_march_2021_cleaned.csv", row.names = FALSE)
write.csv(may_2022_cleaned, file = "may_2022_cleaned.csv", row.names = FALSE)

################################################################################
#                               Visualizations                                 #
#------------------------------------------------------------------------------#
################################################################################

##------------------------- FOR PROBLEM STATEMENT 1 --------------------------##
################################################################################
#------------------------------ Histogram -------------------------------------#
##----DATA MANIPULATION-----##
# Histogram 1: Inventory Distribution of MRPs Across Different Platforms throughout May 2022 and March 2021
# Remove the 'TP.1' and 'TP.2' columns as its not needed
pl_march_2021_cleaned <- pl_march_2021_cleaned[, !(names(pl_march_2021_cleaned) %in% c("TP.1", "TP.2"))]

# Remove the 'TP' column as its not needed
may_2022_cleaned <- may_2022_cleaned[, !(names(may_2022_cleaned) %in% c("TP"))]

# Perform full outer join
full_out_dataset <- full_join(pl_march_2021_cleaned, may_2022_cleaned)

# Inspect dataset after data joining
names(full_out_dataset)

# compile all MRP columns to call later during reshaping
mrp_columns <- c("Ajio.MRP", "Amazon.MRP", "Amazon.FBA.MRP", "Flipkart.MRP", 
                 "Limeroad.MRP", "Myntra.MRP", "Paytm.MRP", "Snapdeal.MRP")

# Reshaping the MRP columns into a long format
histogram_dataset <- full_out_dataset %>%
  pivot_longer(                       # Transform data from wide format to long format
    cols = all_of(mrp_columns),       # Select all MRP columns to pivot
    names_to = "Platform_MRP",        # New column for the MRP platform names
    values_to = "MRP_Value"           # New column for the MRP values
  )         # Filter to select rows where the MRP_Value is not missing values

# Check outcome
head(histogram_dataset)

##--------PLOTTING---------##
# Plotting the histogram of MRPs
ggplot(histogram_dataset, aes(x = MRP_Value)) +
  geom_histogram(binwidth = 70, fill = "royalblue", color = "black") +
  facet_wrap(~ Platform_MRP, scales = "free_y") +
  labs(title = "Inventory Distribution of MRPs Across Different Platforms",
       x = "MRP Value",
       y = "Frequency") +
  theme_minimal()

#---------------------------- Scatter Plot ------------------------------------#
# Scatter Plot 1: Relationship Between Sales Volume and Inventory Levels
##----DATA MANIPULATION-----##
# Summarize the sales data to get total sales quantity per SKU
total_sales_per_sku <- amazon_sales_cleaned %>%
  group_by(SKU) %>%
  summarise(Total_Sales_Qty = sum(Qty, na.rm = TRUE))

# Left Join the sales data with the inventory data
sales_inventory_data <- total_sales_per_sku %>%
  left_join(sale_report_cleaned, by = c("SKU" = "SKU.Code"))

# Check Missing value 
colSums(is.na(sales_inventory_data))

# Remove Rows with Missing values
sales_inventory_data = na.omit(sales_inventory_data)

##--------PLOTTING---------##
# Plot the scatter plot
ggplot(sales_inventory_data, aes(x = Stock, y = Total_Sales_Qty)) +
  geom_point(color = "skyblue3") +
  labs(title = "Relationship Between Sales Volume and Inventory Levels",
       x = "Inventory Level (Stock)",
       y = "Sales Volume (Total Sales Quantity)") +
  theme_minimal()

##------------------------- FOR PROBLEM STATEMENT 2 --------------------------##
################################################################################
#------------------------------ Bar Chart -------------------------------------#
##### Bar Chart 1: Category Distribution
##----DATA MANIPULATION-----##
# Get number of counts for product categories
category_counts <- sale_report_cleaned %>%
  count(Category) %>%
  arrange(desc(n))

# Rename the column name n to Counts
names(category_counts)[names(category_counts) == "n"] <- "Counts"

# Delete row category with empty string in category
# Delete row for duplicate category Kurta Set
category_counts = category_counts %>% 
  filter(Category != "" & Category != "KURTA SET")

##--------PLOTTING---------##
# Plot bar chart
ggplot(category_counts, aes(x = reorder(Category, -Counts), y = Counts)) +
  geom_bar(stat = "identity", fill = "steelblue2") +
  labs(title = "Frequency of Product Categories",
       x = "Category",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

##### Bar Chart 2: Color Preference by Size
##----DATA MANIPULATION-----##
# Check out the color column values for data inconsistency and inaccurate data by 
# using unique function on sales report color column
unique(sale_report_cleaned$Color)

#! GOAL: Find if there is any trends in color preferences for different sizes
# Convert all strings in the 'Color' column to uppercase for data format consistency
sale_report_cleaned$Color <- toupper(sale_report_cleaned$Color)

# Filter out unrelated and nonspecific color values such as empty string, “NO REFERENCE”  and "MULTICOLOR".
sale_report_cleaned <- sale_report_cleaned %>% filter(Color != "" & Color != "NO REFERENCE" & Color != "MULTICOLOR")

# Grouping specific colors under a general color category to get overall main colour genre
sale_report_cleaned <- sale_report_cleaned %>%
  mutate(Color_Group = case_when(
    Color %in% c("RED", "MAROON", "RUST", "WINE", "BURGUNDY") ~ "RED",
    Color %in% c("PINK", "PEACH", "LIGHT PINK", "CORAL PINK", "CORAL ", "CORAL") ~ "PINK",
    Color %in% c("ORANGE", "BROWN", "KHAKI", "TAUPE", "CORAL ORANGE", "LIGHT BROWN") ~ "ORANGE",
    Color %in% c("YELLOW", "BEIGE", "GOLD", "CHIKU", "MUSTARD", "CREAM", "LEMON YELLOW", "LEMON ", "LEMON", "LIGHT YELLOW") ~ "YELLOW",
    Color %in% c("GREEN", "OLIVE", "DARK GREEN", "TEAL", "OLIVE GREEN", "LIGHT GREEN", "SEA GREEN", "TEAL GREEN", "LIME GREEN", "AQUA GREEN", "MINT", "TEAL GREEN ", "MINT GREEN") ~ "GREEN",
    Color %in% c("BLUE", "NAVY BLUE", "TURQUOISE BLUE", "TEAL BLUE ", "SKY BLUE", "LIGHT BLUE", "DARK BLUE", "POWDER BLUE", "NAVY", "TURQUOISE GREEN", "TURQUOISE") ~ "BLUE",
    Color %in% c("PURPLE", "TEAL GREEN","INDIGO", "MAUVE", "MAGENTA") ~ "PURPLE",
    Color %in% c("BLACK", "WHITE", "GREY", "OFF WHITE", "CHARCOAL") ~ "GREYSCALE",
    TRUE ~ as.character(Color) # Keeps the original color if it doesn't match the above 
  ))
View(sale_report_cleaned)

##--------PLOTTING---------##
# Custom color palette that matches the color names 
custom_colors <- c(
  "BLUE" = "blue",
  "GREEN" = "darkgreen",
  "GREYSCALE" = "grey",
  "ORANGE" = "orange",
  "PINK" = "pink",
  "PURPLE" = "purple",
  "RED" = "red",
  "YELLOW" = "palegoldenrod"# darker shade of yellow as yellow is too bright
)

# Plot bar chart with custom color 
ggplot(sale_report_cleaned, aes(x = Color_Group, fill = Color_Group)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ Size, scales = "free_y") +  # Adjusting the layout
  scale_fill_manual(values = custom_colors) +  # Apply custom color palette
  labs(title = "Color Preference by Size",
       x = "Color Group",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))  

#------------------------------ Pie Chart -------------------------------------#
##### Pie Chart 1: Orders Fulfillment Type Distribution
##----DATA MANIPULATION-----##
fulfilment_distribution <- table(amazon_sales_cleaned$Fulfilment)
fulfilment_distribution

##--------PLOTTING---------##
# Plot the pie chart on Orders Fulfillment Type Distribution
pie(fulfilment_distribution,
    main = "Fulfilment Type Distribution",
    col = c("lightblue", "pink1"),
    labels = names(fulfilment_distribution))

##------------------------- FOR PROBLEM STATEMENT 3 --------------------------##
################################################################################
#----------------------------- Time Series ------------------------------------#
##----DATA MANIPULATION-----##
# Select required column variables that is Order.ID and Date
amazon_sales_ts <- amazon_sales_cleaned %>%
  select(Order.ID, Date)

# Summarize the number of sales by date
amazon_sales_summary <- amazon_sales_ts %>%
  group_by(Date) %>%
  summarise(Sales_Count = n())
amazon_sales_summary

##--------PLOTTING---------##
# Get max and min point value to be display on the plot
max_point <- amazon_sales_summary %>% filter(Sales_Count == max(Sales_Count))
min_point <- amazon_sales_summary %>% filter(Sales_Count == min(Sales_Count))
max_point
min_point

# Plotting the Time Series for Monthly Sales Trend with max and min labelled
ggplot(amazon_sales_summary, aes(x = Date, y = Sales_Count, group = 1)) +
  geom_line(color = "midnightblue") +
  geom_point() +
  geom_text(data = max_point, aes(label = Sales_Count), vjust = -0.45) +
  geom_text(data = min_point, aes(label = Sales_Count), vjust = 1.45) +
  labs(title = "Monthly Sales Trend",
       x = "Month",
       y = "Sales Volume") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20))

#----------------------------- Line Graph -------------------------------------#
##----DATA MANIPULATION-----##
# Line Graph 1: Quantity Sold by Category Over Time
# Summarize the sales by category and date
sales_by_category <- amazon_sales %>%
  group_by(Date, Category) %>%
  summarise(Total_Qty = sum(Qty, na.rm = TRUE))

# Convert to all uppercase so it shows nicely in the legend as originally it is a combination of uppercase and lowercase characters
sales_by_category$Category <- toupper(sales_by_category$Category)
sales_by_category

##--------PLOTTING---------##
# Plot Line graph for Quantity Sold by Category
ggplot(sales_by_category, aes(x = Date, y = Total_Qty, color = Category, group = Category)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("darkslateblue", "royalblue", "slategray2", "aquamarine4", "darkseagreen3", "thistle", "rosybrown1", "coral1", "palegoldenrod")) +
  labs(title = "Quantity Sold by Category Over Time",
       x = "Date",
       y = "Quantity Sold") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
