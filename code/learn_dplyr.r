# learning dplyr
# walkthrough of how to clean up data using dplyr
# ben-williams
# ben.williams@alaska.gov
# 2019-03-01

# load ----
library(tidyverse)
library(lubridate)

# data ----
df <- read_csv('data/practice_data_1.csv') 

glimpse(df)
View(df)

# explore data ----
unique(df$Year)
unique(df$SPECIES)

df %>% 
  rename_all(tolower) %>% 
  filter(!is.na(date)) %>% 
  rename(catch = `catch number`,
         length = `length (mm)`,
         weight = `weight (lbs)`,
         age_code = `age code`) %>% 
  mutate(date = mdy(date),
         # year = as.numeric(str_replace(year, "[[*]]", ""))) 
         year = year(date),
         month = month(date),
         julian = yday(date)) -> df1

# eda ----

df1 %>% glimpse()
  
ggplot(df1, aes(age, length)) +
  geom_point()
  
ggplot(dat = df1, aes(x = age, y = length)) +
  geom_point()

df1 %>% 
  mutate(length = ifelse(length>100, length / 10, length),
         weight = ifelse(weight>100, weight / 1000, weight),
         age = ifelse(age<0, NA, age)) -> df1 

df1 %>% 
  ggplot(aes(age, weight)) +
  geom_point()

mean(df1$age, na.rm = TRUE)

df1 %>% 
  group_by(year) %>% 
  summarise(mean_age = mean(age, na.rm = T),
            mean_length = mean(length),
            mean_weight = mean(weight)) -> means 

write.csv(means, 'output/means.csv')
  


means %>% 
  ggplot(aes(year, mean_age)) +
  geom_point() +
  geom_line() +
  stat_smooth(method = 'lm', color = 'red', alpha = .1, fill = 'red') + 
  theme_bw() +
  stat_smooth(aes(y = mean_length), method = 'lm', color = 'blue', alpha = .1, fill = 4)

means %>% 
  gather(measure, value, -year) %>% 
  ggplot(aes(year, value, group = measure, color = measure, fill = measure)) +
  geom_point() +
  # geom_line() +
  stat_smooth(method = 'glm', alpha = .3) + 
  theme_classic() +
  xlab('\nYear') +
  ylab('Measure \n') +
  ggtitle('This is a title \n and a subtitle')



  


  