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








 
 
 
 
 
 
  