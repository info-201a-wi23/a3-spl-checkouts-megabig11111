# Question: How did the number of books published as Sci-Fi and Juvenile
# Fiction change over the years?

# Load libraries
library("dplyr")
library("ggplot2")
library("tidyverse")

# Assign .csv file to dataframe haddix_df
setwd("C://Users//andre//Desktop//INFO201//a3-spl-checkouts-megabig11111")
haddix_df <- read.csv("Checkouts_by_Title.csv", stringsAsFactors = FALSE)

# Filter only checkouts that contain Science Fiction in the Subjects,
# and find the total Science Fiction checkouts per year 
scifi_checkouts <- haddix_df %>%
  filter(str_detect(Subjects, "Science fiction")) %>%
  group_by(CheckoutYear) %>%
  summarize(scifi_total = length(Title))

# Filter only checkouts that contain Juvenile Fiction in the subjects,
# and find the total Juvenile Fiction checkouts per year 
juvenile_checkouts <- haddix_df %>%
  filter(str_detect(Subjects, "Juvenile fiction")) %>%
  group_by(CheckoutYear) %>%
  summarize(juvenile_total = length(Title))

# Combine the two data frames
combined_data <- left_join(scifi_checkouts, juvenile_checkouts, by = "CheckoutYear")

#create line chart
chart2 <- ggplot(combined_data, aes(x = CheckoutYear, color = Subjects)) +
  geom_line(aes(y = scifi_total, color = "Science Fiction")) +
  geom_line(aes(y = juvenile_total, color = "Juvenile Fiction")) +
  scale_x_continuous(breaks = seq(2005, 2023, 1)) +
  labs(title = "Number of Sci-Fi & Juvenile Fiction Books Published by Margaret Peterson Haddix from Apr '05 - Jan '23",
       x = "Year",
       y = "Number of Books Published",
       color = "Subjects") 


