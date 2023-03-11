# Load dplyr libary
library("dplyr")
# Assign .csv file to dataframe haddix_df
setwd("C://Users//andre//Desktop//INFO201//a3-spl-checkouts-megabig11111")
haddix_df <- read.csv("Checkouts_by_Title.csv", stringsAsFactors = FALSE)

# 1. Total number of books documented from checkouts from Haddix from
# April 2005 to January 2023
total_books <- nrow(haddix_df)


# 2. Total number of years Haddix books were published from
# April 2005 to January 2023
years <- haddix_df$PublicationYear
year_only <- gsub(".*?([0-9]+).*", "\\1", years)
total_years <- length(unique(year_only))


# 3. The most number of checkouts per book
max_checkouts <- haddix_df %>%
  summarize(Max_Checkouts = max(Checkouts, na.rm = TRUE)) %>%
  pull(Max_Checkouts)

# 4. The year with the most number of Haddix book checkouts

# A table of the total checkouts per checkout year
checkouts_by_year <- haddix_df %>%
  group_by(CheckoutYear) %>%
  summarize(Total_Checkouts = sum(Checkouts, na.rm = TRUE))

max_checkouts_per_year <- checkouts_by_year %>%
  filter(Total_Checkouts == max(Total_Checkouts, na.rm = TRUE)) %>%
  pull(CheckoutYear)


# 5. The year with the least number of Haddix book checkouts
min_checkouts_per_year <- checkouts_by_year %>%
  filter(Total_Checkouts == min(Total_Checkouts, na.rm = TRUE)) %>%
  pull(CheckoutYear)


# 6. The average number of Haddix book checkouts per year (rounded to 1s place)
avg_checkouts_per_year <- checkouts_by_year %>%
  summarize(Mean = mean(Total_Checkouts, na.rm = TRUE)) %>%
  pull(Mean)
avg_checkouts_per_year <- round(avg_checkouts_per_year, 0)


# Table that displays these summary statistics
haddix_summary <- data.frame(
  Name = c(
    "Total number of books documented from checkouts from Haddix from
April 2005 to January 2023",
    "Total number of years Haddix books published from April 2005 to January 2023",
    "The most number of checkouts per book",
    "The year with the most number of Haddix book checkouts",
    "The year with the least number of Haddix book checkouts",
    "The average number of Haddix book checkouts per year"
  ),
  Value = c(total_books, total_years, max_checkouts, max_checkouts_per_year,
            min_checkouts_per_year, avg_checkouts_per_year)
)
