#allow multiple users to access
if (Sys.getenv("USER") == 'brianlibgober'){
  setwd("~/Downloads/")
} else {
  setwd("/Users/jamiegall/Desktop/")
}

#libraries
library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)

#load data
count_of_acts <- read_excel("count_of_acts.xlsx")

#rename
count_of_acts <- count_of_acts %>%
  rename(galloway_count = "Galloway Counts")
count_of_acts <- count_of_acts %>%
  rename(lib_count = "n")

#plot of data
pl <- ggplot(data = count_of_acts, aes(x = congress_number)) +
  geom_line(aes(y = lib_count, color = "Lib Count"), size = 0.5) +
  geom_path(aes(y = galloway_count, color = "Galloway Count"), size = 1, linetype = "22") +  # Dashed line
  theme_classic() +
  ylab("Count") +
  scale_color_manual(values = c("Lib Count" = "blue", "Galloway Count" = "red")) +
  labs(color = "Variable") +  # Legend title
  theme(legend.position = "bottom",  # Move the legend to the bottom
        plot.margin = margin(t = 20, r = 20, b = 50, l = 20, unit = "pt"))+  # Adjust margins
  scale_y_log10()  # Apply logarithmic scale
print(pl)

#make difference column
count_of_acts$difference <-  count_of_acts$galloway_count - count_of_acts$lib_count

#plot difference data
pl2 <- ggplot(data = count_of_acts, aes(x = congress_number)) +
  geom_line(aes(y = difference)) +
  theme_classic() +
  ylab("Difference")
print(pl2)

#make a percentage error column
count_of_acts$pctdifference <-  (count_of_acts$galloway_count / count_of_acts$lib_count) * 100

#percentage error ggplot
pl3 <- ggplot(data = count_of_acts, aes(x = congress_number)) +
  geom_line(aes(y = pctdifference)) +
  theme_classic() +
  coord_cartesian(ylim = c(95, 105)) +
  ylab("Percent Ratio Galloway/Libgober")

print(pl3)

 