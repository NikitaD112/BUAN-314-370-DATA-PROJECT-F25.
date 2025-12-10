##CLEANING DATA 

library(tidyverse)
library(sqldf)
library(tidyr)
library(knitr)
library(stringr)

mh <- read.csv('https://raw.githubusercontent.com/audreyallen2324/BUAN-314-370-DATA-PROJECT-F25./refs/heads/main/Mental_Health_and_Social_Media_Balance_Dataset.csv')
view(mh)

#RENAME HEADERS
mh <- mh %>%
  rename(Daily_Screen_Time = Daily_Screen_Time.hrs.,
         Sleep_Quality = Sleep_Quality.1.10.,
         Stress_Level = Stress_Level.1.10.,
         Days_With_Exercise = Exercise_Frequency.week.,
         Happiness_Index = Happiness_Index.1.10.)

#FIND ANY NULL OR MISSING VALUES AND CONVERT DAILY SCREEN TIME TO MINUTES
sum(is.na(mh))

mh <- mh %>%
  mutate(Daily_Screen_Time_Minutes = Daily_Screen_Time * 60)
glimpse(mh)


#Max QUERY 5 How much time do different age groups spend on social media? 
QUERY5 <-"SELECT
  CASE 
     WHEN Age BETWEEN 16 AND 21 THEN '16-21'
     WHEN Age BETWEEN 22 AND 27 THEN '22–27'
     WHEN Age BETWEEN 28 AND 33 THEN '28–33'
     WHEN Age BETWEEN 34 AND 39 THEN '34–39'
     WHEN Age >= 40 THEN '40+'
  END AS age_group,
  AVG(Daily_Screen_Time_Minutes) AS avg_time_spent
FROM mh
GROUP BY age_group
ORDER BY age_group;
"
sqldf(QUERY5)

#Visualization 5  Which type of social media is used most by which age group? 

mh_age <- mh %>%
  mutate(age_group = case_when(
    Age >= 16 & Age <= 21 ~ "16-21",
    Age >= 22 & Age <= 27 ~ "22-27",
    Age >= 28 & Age <= 33 ~ "28-33",
    Age >= 34 & Age <= 39 ~ "34-39",
    Age >= 40            ~ "40+"
  ))


freq_mh <- mh_age %>%
  count(Social_Media_Platform, age_group)

ggplot(freq_mh, aes(x = "", y = n, fill = age_group)) +
  geom_col(width = 1, color = "white") +
  coord_polar("y") +
  facet_wrap(~ Social_Media_Platform) +
  labs(
    title = "Distribution of Age Groups for Each Social Media Platform",
    fill = "Age Group",
    y = "",
    x = ""
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

#Visualization 6
mh_age <- mh %>%
  mutate(age_group = case_when(
    Age >= 16 & Age <= 21 ~ "16-21",
    Age >= 22 & Age <= 27 ~ "22-27",
    Age >= 28 & Age <= 33 ~ "28-33",
    Age >= 34 & Age <= 39 ~ "34-39",
    Age >= 40            ~ "40+"
  ))

heat_df <- mh_age %>%
  group_by(Social_Media_Platform, age_group) %>%
  summarise(avg_minutes = mean(Daily_Screen_Time_Minutes, na.rm = TRUE))
library(tidyr)

heat_df <- heat_df %>%
  complete(Social_Media_Platform, age_group, fill = list(avg_minutes = 0))

ggplot(heat_df, aes(x = age_group, y = Social_Media_Platform, fill = avg_minutes)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightyellow", high = "darkred") +
  labs(
    title = "Average Screen Time by Age Group and Social Media Platform",
    x = "Age Group",
    y = "Social Media Platform",
    fill = "Avg Minutes"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

platform_avg <- mh_age %>%
  group_by(age_group, Social_Media_Platform) %>%
  summarize(avg_minutes = mean(Daily_Screen_Time_Minutes, na.rm = TRUE))

ggplot(platform_avg, aes(x = Social_Media_Platform,
                         y = age_group,
                         fill = avg_minutes)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "Average Daily Screen Time by Platform and Age Group",
    x = "Social Media Platform",
    y = "Age Group",
    fill = "Avg Minutes"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

sqldf("SELECT
CORR(Daily_Screen_Time_Minutes, Sleep_Quality) AS corr_screen_sleep,
CORR(Daily_Screen_Time_Minutes, Stress_Level) AS corr_screen_stress,
CORR(Daily_Screen_Time_Minutes, Happiness_Index) AS corr_screen_happiness
FROM mh;")
CORR

write.csv(mh, "/Users/maxreyes/Desktop/tidy_dataset.csv", row.names = FALSE)
