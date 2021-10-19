library(tidyverse)
library(ggbump)

# load data
tuesdata <- tidytuesdayR::tt_load(2021, week = 43)
pumpkins <- tuesdata$pumpkins 

# data prep
pumpkins <- pumpkins %>%
  filter(!str_detect(country, "Entries")) %>%
  separate(id, into = c("year", "type")) %>%
  mutate(
    type = case_when(
      type == "F" ~ "Field Pumpkin",
      type == "P" ~ "Giant Pumplin",
      type == "S" ~ "Giant Squash",
      type == "W" ~ "Giant Watermelon",
      type == "L" ~ "Long Gourd (length)",
      type == "T" ~ "Tomato"
    ),
    place = as.numeric(place),
    weight_lbs = as.numeric(weight_lbs),
    ott = as.numeric(ott),
    est_weight = as.numeric(est_weight),
    pct_chart = as.numeric(pct_chart)
  )

# bump chart, ehh too many still
pumpkins %>%
  filter(type == "Field Pumpkin", place <= 3) %>%
  ggplot(aes(x = year, y = place, color = variety, group = variety)) + 
  geom_line() +
  geom_point()

# something weird with estimated weights
pumpkins %>%
  filter(type == "Field Pumpkin",
         est_weight > 0,
         ott > 0) %>%
  ggplot(aes(x = est_weight, y = ott, color = weight_lbs)) + 
  geom_point(alpha = 0.4)

pumpkins %>%
  filter(type == "Field Pumpkin",
         ott > 0) %>%
  ggplot(aes(x = est_weight, y = ott, color = year)) + 
  geom_point(alpha = 0.2)

lm(log(ott) ~ est_weight, data = pumpkins) %>% 
  broom::tidy()
