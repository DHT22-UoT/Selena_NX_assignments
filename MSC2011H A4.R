# MSC2011H A4
# Ning Xu

setwd("/Users/ningxu/Downloads/UTM/UTM Summer 2022/MSC2011H/")

# Read the data into a data frame by importing file from the working directory
# By importing this way, readr checks for column names to see if there is space and make sure we have valid column names by automatically changing them.
library(dplyr)
df <- ufo_subset 
df$country[df$country == ""] <- NA
df$country[df$shape == ""] <- NA

final_solution <- df %>%
  filter(!is.na(country)) %>% #excluding rows with missing country info
  filter(!is.na(shape))  %>% #excluding rows with missing shape info
  mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H%M%S")) %>% #Convert datetime column into appropriate formats
  mutate(date.posted = as.POSIXct(date.posted, format = "%Y-%m-%d")) %>% #Convert date.posted column into appropriate formats
  filter(!grepl("HOAX", comments, ignore.case = T)) %>% # (filters out comments containing "HOAX") NUFORC officials comment on sightings that may be hoax. Figure out a way (go through the Comments and decide how a proper filter should look like) and remove these sightings from the dataset.
  filter(!grepl("NUFORC Note", comments, ignore.case = T)) %>% # (filters out comments containing "NUFORC Note") NUFORC officials comment on sightings that may be hoax. Figure out a way (go through the Comments and decide how a proper filter should look like) and remove these sightings from the dataset.
  mutate(report_delay = as.Date(date.posted) - as.Date(datetime)) %>% # Add another column to the dataset (report_delay) and populate with the time difference in days, between the date of the sighting and the date it was reported.
  filter(report_delay >= 0) # Filter out the rows where the sighting was reported before it happened.
  
avg_report_delay_table <- final_solution %>%
  group_by(country) %>%
  summarise(avg_report_delay = mean(report_delay, na.rm = T)) # Create a table with the average report_delay per country

# Check the data quality (missingness, format, range etc) of the duration(seconds) column. Explain what kinds of problems you have identified and how you chose to deal with them, in your comments.
final_solution$duration..seconds.[final_solution$duration..seconds. == ""] <- NA
summary(final_solution$duration..seconds.)
class(final_solution$duration..seconds.) # duration(seconds) is not in the numeric format (currently consists of strings), so I need to use as.numeric() to change it
final_solution$duration..seconds. <- as.numeric(final_solution$duration..seconds.)
# There is no missingness in the duration(seconds) column (from summary), however, there is a very large range of numbers, ranging from 0.02 sec to 52623200 sec (in the final_solution table) or 82800000 sec in the original data.
# Therefore, I plan to use filter() to remove the potential outliers. The condition would be to remove the data points > Q3+1.5*interquartile range(IQR) = Q3+1.5*(Q3-Q1) = 600+1.5*(600-45) = 1432.5
#  (and theoretically data points < Q1-1.5*IQR = Q1-1.5*(Q3-Q1) = 45 - 1.5*(600-45) = -787.5, but they do not exist in this data set)
final_solution <- final_solution %>%
  filter(duration..seconds. <= 1432.5) %>%
  mutate(duration..seconds. = sprintf("%1.2f", duration..seconds.)) # Some numbers are in the decimal format with 2 decimal places while others are in the integer format with no decimal place. I will apply sprintf("%1.2f", x) to change all numbers to the decimal format with 2 decimal places.
 
# Create a histogram using the duration(seconds) column, I used log10 because otherwise the histogram is very skewed
hist(log10(as.numeric(final_solution$duration..seconds.)), main= "Histogram of Duration of UFO Sighting",xlab = "Log10(Duration(seconds))",ylab="Frequency", xlim = c(-2, 4), ylim = c(0, 8000))

                                                                                                                                                      