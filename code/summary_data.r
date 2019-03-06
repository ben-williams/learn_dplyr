library(tidyverse)

read_csv('output/means.csv') %>% 
  dplyr::select(-X1) -> means

means %>% 
  ggplot(aes(year, mean_age)) +
  geom_point() +
  geom_line() +
  stat_smooth(method = 'lm', color = 'red', alpha = .1, fill = 'red') + 
  theme_bw() +
  stat_smooth(aes(y = mean_length), method = 'lm', 
              color = 'blue', alpha = .1, fill = 4)

# nls example ----

df1 %>% 
  ggplot(aes(length, weight)) +
  geom_point() +
  stat_smooth(method = 'nls', formula = y ~ a * x^2 + b * x + c,
              method.args = list(start = list(a = .1, b = .5, c = .2)), 
              se = FALSE, color = 4)

fit <- nls(weight ~ a * length^2 + b * length + c,
           start = list(a = 0.1, b = 0.5, c = 0.2),
           data = df1)


a <- 0.0001
b <- 0.0005
c <- 0.02

df1 %>% 
  ggplot(aes(length, weight)) +
  geom_point() +
  geom_line(aes(length, a * length^2 + b * length + c))
