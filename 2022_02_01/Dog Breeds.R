# Loading needed libraries

library(tidyverse)
library(ggimage)
library(janitor)
library(extrafont)
library(sysfonts)

# Reading in all data

breed_traits <- read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv')
trait_description <- read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/trait_description.csv')
breed_rank_all <- read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv')

# Cleaning the column names w. janitor

breed_rank_all %>% 
  clean_names() ->
    ranks

# Selecting the top 10 (from 2020)

ranks %>%
  filter(x2020_rank <= 10) %>%
  rename_with(~str_remove(., 'x')) ->
  top_ten

# Cleaning col names, pivoting

top_ten %>%
  select(-links) %>%
  pivot_longer(cols = c(`2013_rank`:`2020_rank`), names_to = 'year', values_to = 'rank') %>%
  mutate(year = str_replace(string = year, pattern = '_rank', replacement = '')) ->
  top_ten_long

# Adding in the image mapping

as.data.frame(unique(top_ten$breed)) ->
  breed_list

breed_list$image_file <- c(
  'Dog Breeds/Labrador-Retriever-illustration.png',
  'Dog Breeds/French-Bulldog-Illo-2.png',
  'Dog Breeds/German-Shepherd-Dog-Illo-2.png',
  'Dog Breeds/Golden-Retriever-Illo-2.png',
  'Dog Breeds/Rottweiler-Illo-2.png',
  'Dog Breeds/Bulldog-Illo-2.png', 
  'Dog Breeds/Standard-Poodle-illustration.png',
  'Dog Breeds/Beagle-Illo-2.png',
  'Dog Breeds/German-Shorthaired-Pointer-Illo-2.png',
  'Dog Breeds/Dachshund-Illo-2.png'
)

# Quick renaming for my own sanity

colnames(breed_list) <- c('breed', 'image_file')

# Joining the image df to the rank df

top_ten_long <- left_join(top_ten_long, breed_list, c('breed' = 'breed'))

# Creating the plot

ttplot <- top_ten_long %>%
  filter(year == '2013' | year == '2020') %>%
  ggplot(aes(x = year, y = rank, group = breed)) + 
  geom_line(size = .6, color = '#797A9E', linetype = 3) +
  geom_point(aes(color = year),size = 15) +
  geom_image(aes(image = image_file), size = .07, by = 'width') + 
  scale_color_manual(values = c('#BBBDF6', '#BBBDF6'), guide = 'none') + 
  scale_y_continuous(name = '', n.breaks = 13, labels = NULL) +
  scale_x_discrete(name = 'Year',expand = c(.2, .01)) +
  scale_size_identity() +
  theme_minimal() + 
  labs(title = 'Top Dog Breeds, 2013 - 2020',
       subtitle = 'Examining which dog breeds had the most and least mobility between 2013-2020. 
Dogs are rated by the American Kennel Club on an annual basis.',
       caption = 'Data provided by the American Kennel Club') +
  theme(aspect.ratio = 1,
        plot.title = element_text(family = 'Arial Black', size = 26, colour = '#2B061E'),
        plot.subtitle = element_text(family = 'Arial', size = 11, color = '#52405E', margin = margin(0, 3, 2, 0, 'line')),
        panel.grid = element_line(colour = 'light grey', size = .05),
        plot.background = element_rect(fill = '#FBF5EE', colour = '#FBF5EE'),
        axis.title.y = element_text(family = 'Arial', colour = '#2B061E'),
        axis.text.x = element_text(family = 'Arial Black', colour = '#2B061E', margin = margin(10,0,0,0, unit = 'pt')),
        panel.background = element_rect(fill = '#FBF5EE', colour = '#FBF5EE'),
        plot.caption = element_text(family = 'Arial', , color = '#797A9E', size = 8))

# Saving

ggsave(filename = 'ttplot.png', plot = ttplot, device = 'png', dpi = 320, width = 6.3, height = 7.75, units = 'in')        
