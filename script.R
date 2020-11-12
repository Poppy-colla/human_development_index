#######################################
#              TASK 1                 #
#######################################

 library(tidyverse)
 
 # QUESTION 1 #
 
 # Load data
 
 data <- "Human-development-index.csv "

 hdi <- read_csv(data) %>%
   janitor::clean_names()

 # Tidy data 
 
 hdi <- hdi %>%
 pivot_longer(names_to = "year",
              values_to = "index",
              cols = -c(hdi_rank_2018, country)) 

# Here i have tidied the year and index value columns to a tidy format and left the country and rank as they are 
 
# Remove the x from the year 

  hdi <- hdi %>%
   mutate(year =  str_replace(year, "x", "") %>% as.numeric())


# Here i have replaced the X values with nothing i.e "" and changed year to numeric 
  
# QUESTION 2 #
  
hdi_no_na <- hdi %>%
  filter(!is.na(index))

# is.na() to remove the missing values in the string index 
# ! is...


# Summarizing data 

hdi_summary <- hdi_no_na %>%
  group_by(country) %>%
  summarise(mean_index = mean(index))

# Adding summary columns 

hdi_summary <- hdi_no_na %>%
  group_by(country) %>%
  summarise(mean_index = mean(index),
            n = length(index))
hdi_summary

# QUESTION 3 #

hdi_summary <- hdi_no_na %>%
  group_by(country) %>%
  summarise(mean_index = mean(index),
            n = length(index),
            sd_index = sd(index),
            se_index = sd_index/sqrt(n))

# Standard error worked out by doing standard deviation divided by square root of n

# We can filter the summary to get just the 10 countries with the lowest mean HDI:

hdi_summary_low <- hdi_summary %>%
  filter(rank(mean_index)<11)

# Then plot this 

hdi_summary_low %>%
  ggplot() +
  geom_point(aes(x = country,
                 y = mean_index)) +
  geom_errorbar(aes(x = country,
                    ymin = mean_index - se_index,
                ymax = mean_index + se_index)) +
   scale_y_continuous(limits = c(0, 0.5),
                      expand = c(0, 0),
                      name = "HDI") +
  scale_x_discrete(expand = c(0, 0),
                   name = "") +
  theme_classic() +
  coord_flip()

#######################################
#              TASK 2                 #
#######################################

file <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=44025h2011.txt.gz&dir=data/historical/stdmet/"

# Viewing the first line to help decide how to read in the data 
readLines(file, n = 1)
#[1] "#YY  MM DD hh mm WDIR WSPD GST  #WVHT   DPD   APD MWD   PRES  ATMP  #WTMP  DEWP  VIS  TIDE"

readLines(file, n = 2)
#[2] "#yr  mo dy hr mn degT m/s  m/s   #m   sec   sec degT   hPa  degC  degC  # degC   mi    ft"
readLines(file, n = 3)
#[3] "2010 12 31 23 50 222  7.2  8.5  0.75  4.55  3.72 203 1022.2   6.9   6.7   3.5 99.0 99.00"

# Shows that the first line gives the column name, the second the units and the third is where the data begins 

# So, read in the data as shown:

buoy44025 <- read_table(file, 
                        col_names = FALSE,
                        skip = 2)
# col = FALSE means don't use the given column names (why just have x1, x2...) skip = 2 means skip first two rows, so starting with data 

# QUESTION 2 #

# Read in the variable names from the first line, removing the #

measure <- scan(file,
                nlines = 1,
                what = character()) %>%
  str_remove("#")

# Read in the units from the second line and remove the #
# Replace the / with _per_ as / is a special character 

units <- scan(file,
              skip = 1,
              nlines = 1,
              what = character()) %>%
  str_remove("#") %>%
  str_replace("/", "_per_")

# Paste the variable name and its units together for the column name 

names(buoy44025) <- paste(measure, units, sep = "_")

names(buoy44025)

#######################################
#              TASK 3                 #
#######################################

# ON PROTID SCRIPT #



 
 
 
 
 
 
  