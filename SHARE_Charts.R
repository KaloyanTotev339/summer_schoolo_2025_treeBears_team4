# Getting the location of the current script and setting the working directory to it

fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(fileloc)
rm(fileloc)

# Clearing workspace and plots

rm(list = ls())
dev.off(dev.list()["RStudioGD"])

# Loading the necessary packages

library(ggplot2)
library(googleVis)
library(dplyr)

# Creating a data frame from the csv file 

df <- read.csv2("statisticsexp.csv")

# Excluding the missing values

dfc <- df
dfc[dfc < 0] <- NA

# Creating new versions of some variables

dfc$sphus_num <- as.numeric(factor(dfc$sphus, ordered = TRUE))

dfc$sphus_group <- as.character(dfc$sphus, 
                                levels = c("Poor", "Fair", "Good", "Very Good", "Excellent"), 
                                ordered = TRUE)

dfc$age_group <- cut(dfc$age_int, breaks = c(30, 55, 70, 85, Inf), 
                     labels = c("<55", "55-70", "70-85", "85+"), right = FALSE)

dfc$gender_group <- as.character(dfc$gender, ordered = TRUE)

country_factor = factor(dfc$country, 
                        levels = c("11", "12", "13", "14", "15", "16", "17", "18", "19", 
                                   "20", "23", "25", "28", "29", "31", "32", "34", "35", 
                                   "47", "48", "51", "53", "55", "57", "59", "61", "63"), 
                        labels = c("Austria", "Germany", "Sweden", "Netherlands", "Spain", "Italy", "France", "Denmark", "Greece", 
                                   "Switzerland", "Belgium", "Israel", "Czech Republic", "Poland", "Luxembourg", "Hungary", "Slovenia", "Estonia", 
                                   "Croatia", "Lithuania", "Bulgaria", "Cyprus", "Finland", "Latvia", "Malta", "Romania", "Slovakia"))

dfc$country_name <- country_factor
rm(country_factor)

dfc$br001_label <- recode(as.character(dfc$br001_), `5` = "No", `1` = "Yes")

dfc$ac035d4_label <- recode(as.character(dfc$ac035d4), `0` = "No", `1` = "Yes")

dfc$phactiv_label <- recode(as.character(dfc$phactiv), `0` = "Active", `1` = "Inactive")
dfc2 <- dfc %>% filter(!is.na(phactiv))

# CHARTS

# Age Distribution

h <- hist(dfc$age_int, breaks = 30, plot = FALSE)
breaks <- h$breaks

ggplot(dfc, aes(x = age_int)) +
  geom_histogram(breaks = breaks, fill = "skyblue", color = "white") +
  theme_minimal() +
  labs(title = "Age Distribution", 
       x = "Age", y = "Frequency")

rm(h, breaks)

# Self-Perceived Health Distribution

ggplot(dfc, aes(x = sphus)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Self-perceived health Distribution", 
       x = "Self-Perceived Health", y = "Frequency")

# Age Gradient in Self-Perceived Health

ggplot(dfc, aes(x = age_int, y = sphus_num)) +
  geom_jitter(alpha = 0.3) +
  theme_minimal() +
  labs(title = "Age gradient in Self-perceived health", 
       x = "Age", y = "Self-Perceived Health")

# By Country: Self-Perceived Health

ggplot(dfc[dfc$sphus_group %in% c("1", "2", "3", "4", "5"), ], aes(x = country_name, fill = sphus_group)) +
  geom_bar(position = "fill") +
  facet_wrap(~ country, scales = "free", nrow = 3) +
  scale_fill_discrete(labels = c("Excellent", "Very Good", "Good", "Fair", "Poor")) +
  theme_minimal() +
  labs(title = "Self-perceived health distribution by Country", 
       x = "Country", y = "Proportion", 
       fill = "Self-\nPerceived\nHealth") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_blank(),
        axis.title = element_blank())

# By country: Physical inactivity

phactiv_ratio <- dfc2 %>%
  group_by(country_name, phactiv_label) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(country_name) %>%
  mutate(prop = count / sum(count))

ggplot(phactiv_ratio, aes(x = reorder(country_name, -prop), y = prop, fill = phactiv_label)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Physical activity by Country",
       x = "Country", y = "Proportion",
       fill = "Physically\n(in)active") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# By Age Group: Gender

ggplot(dfc[dfc$gender_group %in% c("1", "2"), ], aes(x = age_group, fill = gender_group)) +
  geom_bar(position = "fill") +
  scale_fill_discrete(labels = c("Male", "Female")) +
  theme_minimal() +
  labs(title = "Female survival advantage by Age group", 
       x = "Age Group", y = "Proportion", 
       fill = "Gender\nGroup") +
  theme(strip.text = element_blank())

# By Age Group: Ever smoked daily

ggplot(data = subset(dfc, age_group %in% c("<55", "55-70", "70-85", "85+") & !is.na(br001_label)), 
       aes(x = br001_label, fill = age_group)) +
  geom_bar(position = "fill") +
  facet_wrap(~ br001_label, scales = "free", strip.position = "bottom") +
  labs(title = "Smoking by Age group", 
       x = "Ever smoked daily", y = "Proportion", 
       fill = "Age\nGroup") +
  theme_minimal() +
  theme(axis.text.x = element_blank())

# By Age Group: Activities in last year: attended an educational or training course

ggplot(data = subset(dfc, age_group %in% c("<55", "55-70", "70-85", "85+") & !is.na(ac035d4_label)), 
       aes(x = ac035d4_label, fill = age_group)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  facet_wrap(~ ac035d4_label, scales = "free", strip.position = "bottom") +
  labs(title = "Education patterns across Age groups", 
       x = "Activities in last year: attended an educational or training course", y = "Proportion", 
       fill = "Age\nGroup") +
  theme(axis.text.x = element_blank())

# By country: Self-Perceived Health (mean)

sphus_mean <- dfc %>%
  group_by(country_name) %>%
  summarise(Perceived_Health = mean(sphus_num, na.rm = TRUE))

sphus_map <- gvisGeoChart(sphus_mean,
                          locationvar = "country_name", colorvar = "Perceived_Health",
                          options = list(region = "150", colors = "green"))

# By country: Age (mean)

age_mean <- dfc %>%
  group_by(country_name) %>%
  summarise(Age = mean(age_int, na.rm = TRUE))

age_map <- gvisGeoChart(age_mean,
                        locationvar = "country_name", colorvar = "Age",
                        options = list(region = "150", colorAxis = "{colors: ['#e7711c', '#4374e0']}"))

# Save to an HTML file
write(sphus_map$html$chart, file = "sphus_map.html")
write(age_map$html$chart, file = "age_map.html")
