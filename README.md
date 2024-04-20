---
title: "STAT295 HW1"
author: "Zeynep Sevim KANAR/Hüseyin  Çağatay BAYRAK/ Bülent COŞKUN"
date: "2024-04-20"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Question 1
i.
#cat MERGED_2017_2018_cleaned.csv

ii.
#mv MERGED_2017_2018_cleaned.csv college_score.csv
#grep "Public,Montgomery" college_score.csv > subsample.csv

iii.
#awk -F ',' '{print $3}' college_score.csv | sort | uniq -c

## Question 2
```{r}
library(dplyr)
#i
chocolate <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv")
head(chocolate)
str(chocolate) 
## This dataset seems to cover many details about different chocolate bars. Data have  various columns such as company manufacturer, company location, rewiev date etc. 
##ii
chocolate <- chocolate %>%
  mutate(across(where(is.character), .fns = as.factor))
str(chocolate)
## iii
##It can be  said than some of the standart deviations are 0.
chocolate %>%
  group_by(company_location) %>%
  summarize(
    mean_rating = mean(as.numeric(as.character(rating)), na.rm = TRUE),
    sd_rating = sd(as.numeric(as.character(rating)), na.rm = TRUE),
    median_rating = median(as.numeric(as.character(rating)), na.rm = TRUE),
    range_rating = diff(range(as.numeric(as.character(rating)), na.rm = TRUE))
  ) %>%
  head(10)
## iv
##the ref numbers of chocolates review date is 2020 and originating  from Colombia
result <- chocolate %>%
  filter(review_date== 2020 & country_of_bean_origin=="Colombia")
result$ref

# v
chocolate$cocoa_percent <- as.numeric(sub("%", "", chocolate$cocoa_percent))
chocolate %>% 
  group_by(company_location) %>% 
  summarise(meanRating=mean(rating), meanPercent=mean(cocoa_percent))

## vi
## they all start with "co". but we dont want cocoa_percent

chocolate %>%
  select(starts_with("co"), -starts_with("cocoa_percent")) %>%
  head(.,10)

## vii
chocolate %>%
  filter(company_location== "Switzerland", between(as.numeric(as.character(chocolate$rating)), 3.25,3.5)) %>%
  head(5)

## viii
chocolate %>% 
  group_by(company_location) %>% 
  summarise(meanRating=mean(rating)) %>% 
  arrange(desc(meanRating))
## ix
chocolate %>%
  group_by(country_of_bean_origin) %>%
  filter(company_manufacturer == "Bonnat") %>%
  count()
## v

new_chocolate <- chocolate %>%
  mutate(Rating_Percantage = as.numeric(rating)*25) %>%
  mutate(Class= ifelse(Rating_Percantage<25,"Low",
                       ifelse(Rating_Percantage>=25 & Rating_Percantage<50, "Medium",
                              ifelse(Rating_Percantage>=50 & Rating_Percantage<70, "Tasty","Excellent"))))
head(new_chocolate)


```

## Question 3

```{r}
##i
## It can be seen that the temperature seems rising in begining of the year and decreasing end of the year.
nmmaps <- read.csv("https://www.cedricscherer.com/data/chicago-nmmaps-custom.csv")
head(nmmaps)
library(ggplot2)
ggplot(nmmaps, aes(x = date, y = temp)) +
  geom_point() + 
  facet_wrap(~ year) 


## ii
## The peak of temperature occurs in summer and the lowest temperature happens in autumn and winter.
ggplot(nmmaps, aes(x = date, y = temp, color=season)) +
  geom_point() 


## iii
## There is a strong relationship between temperature  and dewpoint.
ggplot(nmmaps, aes(x=dewpoint, y=temp))+
  geom_point()


```

## QUESTION 4

```{r}

#library
library(rvest)
library(stringr)
library(leaflet)
library(dplyr)
library(magrittr)

# The URL 
url <- "https://acikveri.konya.bel.tr/tr/datastore/dump/b932e10b-0e36-481e-a0a1-bd5448ce5a9e?bom=True"
data <- read.csv(url)



# data manipulation
head(data)
colnames(data) <- c("id","isim","no","latitude","longitude","adres")
data$latitude <- as.numeric(data$latitude)
data$longitude <- as.numeric(data$longitude)
data$id <- as.character(data$id)
data <- na.omit(data)
sum(is.na(data))
data <- subset(data, !(id %in% c(356, 104, 202, 187, 184)))



#visualizing
data |> 
  leaflet() |> 
  addTiles() |> 
  addCircles(~longitude, ~latitude,
             weight = 10,
             radius = 10,
             label = ~isim,
  ) |> 
  setView(lng = median(data$longitude),
          lat = median(data$latitude),
          zoom = 6)

```



```{r}
rmarkdown::render("dosya.R", output_format = "html_document")



library(knitr)
knit("dosya.R", output = "dosya.html")

```
