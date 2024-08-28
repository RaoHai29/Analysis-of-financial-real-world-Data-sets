# List of required libraries
required_libraries <- c("ggplot2", "dplyr")

# Install missing libraries
for (lib in required_libraries) {
  if (!require(lib, character.only = TRUE)) {
    install.packages(lib, dependencies = TRUE)
    library(lib, character.only = TRUE)
  }
}

# Load the libraries
library(ggplot2)
library(dplyr)

# Load the Bitcoin dataset
btc_data <- read.csv("BTC-USD-1.csv", stringsAsFactors = FALSE)
btc_data$Date <- as.Date(btc_data$Date, format = "%d/%m/%Y")

# Load the S&P 500 dataset
sp500_data <- read.csv("S&P 500-1.csv", stringsAsFactors = FALSE)
sp500_data$Date <- as.Date(sp500_data$Date, format = "%d/%m/%Y")
sp500_data$Price <- as.numeric(gsub(",", "", sp500_data$Price))

# 1. Descriptive Statistics for Bitcoin
btc_summary <- btc_data %>%
  summarize(
    Mean = mean(Adj.Close, na.rm = TRUE),
    Median = median(Adj.Close, na.rm = TRUE),
    Mode = as.numeric(names(sort(table(Adj.Close), decreasing = TRUE))[1]),
    Range = range(Adj.Close, na.rm = TRUE),
    Std_Dev = sd(Adj.Close, na.rm = TRUE)
  )

# 1. Descriptive Statistics for S&P 500
sp500_summary <- sp500_data %>%
  summarize(
    Mean = mean(Price, na.rm = TRUE),
    Median = median(Price, na.rm = TRUE),
    Mode = as.numeric(names(sort(table(Price), decreasing = TRUE))[1]),
    Range = range(Price, na.rm = TRUE),
    Std_Dev = sd(Price, na.rm = TRUE)
  )

# Print descriptive statistics
print("Bitcoin Summary:")
print(btc_summary)
print("S&P 500 Summary:")
print(sp500_summary)

# 2. Plotting Bitcoin Price Trend (2018-2024)
ggplot(btc_data, aes(x = Date, y = Adj.Close)) +
  geom_line(color = "blue") +
  ggtitle("Bitcoin Price Trend (2018-2024)") +
  xlab("Date") +
  ylab("Adjusted Close Price (USD)")

# 2. Plotting S&P 500 Price Trend (2018-2024)
ggplot(sp500_data, aes(x = Date, y = Price)) +
  geom_line(color = "red") +
  ggtitle("S&P 500 Price Trend (2018-2024)") +
  xlab("Date") +
  ylab("Price (USD)")

# 3. Calculate 6-month averages and correlation
# Aggregating data by 6-month periods
btc_6m <- btc_data %>%
  mutate(YearMonth = format(Date, "%Y-%m")) %>%
  group_by(YearMonth) %>%
  summarize(AvgPrice = mean(Adj.Close, na.rm = TRUE))

sp500_6m <- sp500_data %>%
  mutate(YearMonth = format(Date, "%Y-%m")) %>%
  group_by(YearMonth) %>%
  summarize(AvgPrice = mean(Price, na.rm = TRUE))

# Merging the datasets
merged_6m <- merge(btc_6m, sp500_6m, by = "YearMonth", all = TRUE)

# Removing rows with missing values
merged_6m <- na.omit(merged_6m)

# Calculate the correlation between the two datasets
merged_6m$Correlation <- cor(merged_6m$AvgPrice.x, merged_6m$AvgPrice.y)



# 3. Scatter plot to visualize the overall correlation
correlation <- cor(merged_6m$AvgPrice.x, merged_6m$AvgPrice.y)
ggplot(merged_6m, aes(x = AvgPrice.x, y = AvgPrice.y)) +
  geom_point(color = "purple") +
  ggtitle(paste("Correlation between Bitcoin and S&P 500 Prices (Correlation:", round(correlation, 2), ")")) +
  xlab("Bitcoin Average Price (USD)") +
  ylab("S&P 500 Average Price (USD)") +
  geom_smooth(method = "lm", color = "red")

# 4. Assessing Normality

# Histogram for Bitcoin Prices
hist(btc_data$Adj.Close, main = "Histogram of Bitcoin Prices", xlab = "Bitcoin Adjusted Close Price (USD)", col = "lightblue")

# Histogram for S&P 500 Prices
hist(sp500_data$Price, main = "Histogram of S&P 500 Prices", xlab = "S&P 500 Price (USD)", col = "lightcoral")

# QQ Plot for Bitcoin Prices
qqnorm(btc_data$Adj.Close)
qqline(btc_data$Adj.Close, col = "red")

# QQ Plot for S&P 500 Prices
qqnorm(sp500_data$Price)
qqline(sp500_data$Price, col = "blue")

# Shapiro-Wilk test for normality (Bitcoin)
shapiro.test(btc_data$Adj.Close)

# Shapiro-Wilk test for normality (S&P 500)
shapiro.test(sp500_data$Price)

