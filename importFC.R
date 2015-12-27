library(dplyr)
library(tidyr)
library(lubridate)
library(ggvis)
library(ggplot2)
library(stringr)
library(readr)
library(RColorBrewer)

# Import and clean data ---------------------------------------------------

fc = readr::read_tsv(file = '~/GitHub/FoodCourt/data/foodcourt.tsv', 
                col_types = 'icciiciicc') 

# Remove duplicate month column name.
colnames(fc)[8] = 'Month_num'

# Convert times from strings to times using lubridate 
fc = fc %>% 
  mutate(Date = mdy(Date),
       time = hm(Time))

fc = separate(fc, Time, into = c('hr', 'min'), sep = 2) %>% 
  mutate(hr = as.numeric(str_replace(hr, ':', '')),
         min = as.numeric(str_replace(min, ':', '')))

# Reorder the days of the week.
fc$day = factor(fc$day, rev(c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 
                          'Thursday', 'Friday', 'Saturday')))


# Aggregate up ------------------------------------------------------------

# By month
fcMon = fc %>% 
  group_by(Month) %>% 
  summarise(avg = mean(Visitors), 
            tot = sum(Visitors),
            std = sd(Visitors),
            num = n())

# By day
fcDay = fc %>% 
  group_by(day) %>% 
  summarise(avg = mean(Visitors), 
            tot = sum(Visitors),
            std = sd(Visitors),
            num = n())


# By date
fcDate = fc %>% 
  group_by(Day) %>% 
  summarise(avg = mean(Visitors), 
            tot = sum(Visitors),
            std = sd(Visitors),
            num = n())

# By time
fcTime = fc %>% 
  group_by(hr) %>%
  summarise(avg = mean(Visitors), 
            tot = sum(Visitors),
            std = sd(Visitors),
            num = n())

# By day and time
# fcDayTime = fc %>% 
#   group_by(day, hr) %>% 
#   summarise(avg = mean(Visitors), 
#             tot = sum(Visitors),
#             std = sd(Visitors),
#             num = n())

fcDayTime = fc %>% 
  group_by(day, hour = hr) %>% 
  summarise(value = round(mean(Visitors)))


# export ------------------------------------------------------------------
write_tsv(fcDayTime, '~/GitHub/FoodCourt/D3/data.tsv')



# simple plots ------------------------------------------------------------

ggplot(fc, aes(x = Date, y = 1, fill = Visitors)) +
  geom_bar(stat = 'identity') +
  coord_polar()




ggplot(fcDay, aes(x = day, y = avg,
                  ymin = avg - std,
                  ymax = avg + std)) +
  geom_pointrange()

ggplot(fcDate, aes(x = Day, y = avg)) +
  geom_line()

ggplot(fcTime, aes(x = hr, y = avg, group = 1)) +
  geom_line()

ggplot(fcDayTime, aes(y = day, x = hr, fill = avg)) +
  geom_tile() +
  scale_fill_gradientn(colours = brewer.pal(9, 'YlGnBu')) +
  theme_bw()


ggplot(fcDayTime, aes(y = avg, x = day, fill = avg, group = hr)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_gradientn(colours = brewer.pal(9, 'YlGnBu')) +
  theme_bw()
