library(readr)
library(magrittr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(Cairo)
library(extrafont)

# We're going to use some fonts from the system; make sure they're loaded
extrafont::loadfonts()

# Read in the data from \url{https://wonder.cdc.gov/TB-v2013.html}
df_tb <- read_tsv("data/OTIS 2013 TB Data.txt", n_max = 1069, col_types = "-ciiii?di")

# df_tb %>% 
#   group_by(Year) %>% 
#   summarise(n_cases = sum(Count), pop = sum(Population), us_rate = (n_cases / pop * 100000)) %>% 
#   ggplot(aes(x = Year, y = us_rate)) +
#   geom_line() +
#   labs(x = "Year Reported",
#        y = "TB Cases per 100,000 residents",
#        title = "Reported Active Tuberculosis Cases in the U.S.") +
#   theme_minimal()

# Get the top 6 states in the last year of data
top_states <- df_tb %>% 
  filter(Year == 2013) %>% 
  arrange(desc(Rate)) %>% 
  slice(1:6) %>% 
  select(State)

# Create a new factor for labelling with top 6 states named and others labelled "Other"
df_tb$top_state <- factor(df_tb$State, levels = c(top_states$State, "Other"))
df_tb$top_state[is.na(df_tb$top_state)] <- "Other"

# Plot it
# Color the top six and increase the line weight
# All other states will be grey and lighter line weight
# Make sure there's only one legend, and not separate legends for line weight and color
p_states <- df_tb %>% 
  ggplot(aes(x = Year, y = Rate, group = State)) +
  geom_line(aes(colour = top_state, size = top_state)) +
  scale_colour_manual(values = c(brewer.pal(n = 6, "Paired"), "grey"), guide = guide_legend(title = "State")) +
  scale_size_manual(values = c(rep(1,6), 0.5), guide = guide_legend(title = "State")) +
  #guide_legend(label = "State") +
  labs(x = "Year reported",
       y = "TB Cases per 100,000 residents",
       title = "Reported Active Tuberculosis Cases in the U.S.") +
  theme_minimal() +
  theme(text = element_text(face = "plain", family = "Arial"))

# display the plot
p_states
# save the plot
ggsave(plot = p_states, filename = "figs/plot_all_states.png", dpi = 96, width = 6, height = 4, units = "in", type="cairo-png")
