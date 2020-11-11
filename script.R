#-------------------------------------#
#              TASK 1                 #
#-------------------------------------#

 library(tidyverse)
 
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


# Here i have replaced the X values with nothin ie.e "" and changed year to numeric 
  
  

 
 
 
 
 
 
  