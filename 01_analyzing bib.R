library(bib2df)
library(dplyr)
library(ggplot2)

# Parsing .bib file

path <- "/Users/sp/Desktop/philosophy-hse.global.bib"
df <- bib2df(path)

# Analyzing the number of articles and books published per year

# Sorting  data by year

year_counts <- df %>%
  filter(!is.na(YEAR) & grepl("^\\d+$", YEAR) &
           (CATEGORY == "ARTICLE" | CATEGORY == "BOOK")) %>%
  mutate(YEAR = as.numeric(YEAR)) %>%  # Convert YEAR to numeric
  group_by(YEAR) %>%
  summarise(count = n()) %>%
  slice(1:289) %>% 
  filter(count > 10) %>% 
  ungroup() 

# Creating a plot

custom_theme <- function() {
  theme_minimal() +
    theme(
      text = element_text(family = "Marion", size = 12), 
      plot.title = element_text(size = 16, face = "bold"),  
      axis.title = element_text(size = 14),  
      axis.text = element_text(size = 12), 
      panel.background = element_rect(fill = "white"),  
      panel.grid.major = element_line(color = "lightgray", linetype = "dotted"), 
      panel.grid.minor = element_blank(),  
      legend.position = "none"
    )
}

plot_main <- ggplot(data = year_counts, aes(x = YEAR, y = count)) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Number of Articles or Books") +
  ggtitle("Number of Articles or Books published per Year") + 
  custom_theme() +
  scale_x_continuous(breaks = seq(min(year_counts$YEAR), max(year_counts$YEAR), by = 25))

plot_zoom <- plot_main +
  coord_cartesian(xlim = c(1980, 2023)) +
  scale_x_continuous(breaks = seq(1980, 2023, by = 5))
