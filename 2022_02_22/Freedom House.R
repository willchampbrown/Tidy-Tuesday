# Loading needed libraries

library(tidytuesdayR)
library(tidyverse)
library(patchwork)
library(ggtext) 
library(sysfonts)
library(showtext)


# Loading in fonts

font_add_google(name = "Lora",family = "Lora")
font_add_google("Fjalla One", "Fjalla One")
showtext_auto()


# Loading data

tuesdata <- tidytuesdayR::tt_load('2022-02-22')
freedom_df <- tuesdata$freedom


# Quick name transformations
# Data is already very clean

head(freedom_df)

freedom <- freedom_df %>%
  select(country,
         year,
         "civil_liberties" = CL,
         "political_rights" = PR,
         "freedom_status" = Status,
         "continent" = Region_Name,
         "least_developed_countries" = is_ldc)
     
     
# Basic graph of civil liberties over time

cl_plot <- freedom %>%
  group_by(year, continent) %>%
  summarise(mean_cl_score = mean(civil_liberties)) %>%
  ggplot(aes(x = year, y = mean_cl_score)) + 
  geom_line(aes(color = continent)) + 
  theme_minimal() + 
  labs(title = "Civil Liberties Over Time",
       subtitle = "Data is parsed by continent",
       x = "Year",
       y = "Average Civil Liberties Score")


# Basic graph of political rights over time

pr_plot <- freedom %>%
  group_by(year, continent) %>%
  summarise(mean_pr_score = mean(political_rights)) %>%
  ggplot(aes(x = year, y = mean_pr_score)) + 
  geom_line(aes(color = continent)) + 
  theme_minimal() + 
  labs(title = "Political Rights Over Time",
       subtitle = "Data is parsed by continent",
       x = "Year",
       y = "Average Political Rights Score")

cl_plot + pr_plot + plot_layout(nrow = 2)


# Pivoting the data into a long format

freedom_pivot <- freedom %>%
  group_by(year, continent) %>%
  summarise(civil_liberties = mean(civil_liberties),
            political_rights = mean(political_rights)) %>%
  pivot_longer(cols = c(civil_liberties, political_rights))

fplot <- freedom_pivot %>%
  ggplot(aes(x = year, y = value, group = continent)) + 
  geom_line() +
  geom_area(aes(fill = name)) +
  facet_grid(rows = vars(name), cols = vars(continent), switch = "y") +
  scale_y_reverse() +
  labs(title = "Civil and Political Rights Over Time",
       subtitle = 
       "A review of  regional <span style='color:#6A322A;'>**Civil Liberties**</span> and <span style='color:#EDAE49;'>**Political Rights**</span> over time (1996-2020).",
       caption = "Data provided by Freedom House",
       x = "Year",
       y = "Score") +
  theme(plot.title = element_text(family = "Fjalla One", size = 38, margin = margin(t = 0, b = 2, unit = "mm")),
        plot.subtitle = element_markdown(family = "Lato", size = 14, color = "#191F24"),
        plot.caption = element_text(family = "Lato", size = 8, hjust = 1, margin = margin(0, 0, 1, 0 ,"mm")),
        plot.margin = margin(10, 5, 0, 5, unit = "mm"),
        axis.text.x = element_text(angle = 90, family = "Lato", size = 10),
        axis.text.y = element_text(family = "Lato", size = 10),
        axis.title.x = element_text(family = "Lato", size = 10, margin = margin(3, 3, 3, 3, unit = "mm")),
        axis.title.y = element_text(family = "Lato", size = 10, margin = margin(3, 3, 3, 3, unit = "mm")),
        plot.background = element_rect(fill = "#FDF9ED"), 
        panel.grid = element_blank(), panel.background = element_rect(fill = "#FDF9ED"),
        strip.text.y = element_blank(),
        strip.background = element_blank(),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.position = "none",
        legend.text = element_text(size = 9, family = "Lato"),
        strip.text = element_text(size = 11, family = "Lato", face = "bold")) +
  scale_fill_manual(labels = c("Civil Liberties", "Political Rights"), values = c("#6A322A", "#EDAE49"))

ggsave(plot = fplot, filename = "testplot.png", device = "png", width = 10, height = 7.5, units = "in", dpi = 300, scale = 1)
