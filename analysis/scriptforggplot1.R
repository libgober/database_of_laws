#libraries
library(here)
library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)

here::i_am("analysis/scriptforggplot1.R")

#load data
count_of_acts <- read_excel(here("build","count_of_acts.xlsx"))

master_count = read_csv(here("build","master.csv")) %>%
  group_by(congress_number) %>%
  count()
#rename
count_of_acts <- count_of_acts %>%
  rename(galloway_count = "Galloway Counts")
count_of_acts <- count_of_acts %>%
  rename(lib_count = "n")

#plot of data
pl <- ggplot(data = count_of_acts, aes(x = congress_number)) +
  geom_line(aes(y = lib_count, color = "Lib Count"), size = 0.5) +
  geom_path(aes(y = galloway_count, color = "Galloway & Wise"), size = 1, linetype = "22") +  # Dashed line
  theme_classic() +
  ylab("Number of Acts of Congress") +
  scale_x_continuous('Congress',breaks=c(1,20,40,60,76)) + 
  scale_color_manual(NULL,values = c("Author's Count" = "blue", "Galloway & Wise" = "red")) +
  labs() +  # Legend title
  theme(legend.position = "bottom") 
print(pl)

ggsave(pl,file = here("draft","figures","counts_of_laws.png"))

pl_revised = pl +
  geom_area(data=master_count,aes(y=n,x=congress_number),
            alpha=0.2) + 
  scale_x_continuous('Congress',breaks=c(1,20,40,60,76,100,117)) 

ggsave(pl_revised,file = here("draft","figures","counts_of_laws_revised.png"))
ggsave(pl_revised,file = here("draft","figures","counts_of_laws_revised.pdf"))



#make difference column
count_of_acts$difference <-  count_of_acts$galloway_count - count_of_acts$lib_count

#plot difference data
pl2 <- ggplot(data = count_of_acts, aes(x = congress_number)) +
  geom_line(aes(y = difference)) +
  theme_classic() +
  ylab("Difference")
print(pl2)

#make a percentage error column
count_of_acts$pctdifference <-  (count_of_acts$lib_count / count_of_acts$galloway_count) - 1

#percentage error ggplot
pl3 <- ggplot(data = count_of_acts, aes(x = congress_number)) +
  geom_line(aes(y = pctdifference)) +
  theme_classic() +
  scale_y_continuous(limits=c(-0.025, .025),labels = scales::percent_format()) + 
  ylab("Percentage Discrepancy Relative to Galloway & Wise") + 
  scale_x_continuous('Congress',breaks=c(1,20,40,60,76)) 

ggsave(pl3,file = here("draft","figures","discrpency.png"))

print(pl3)


count_of_acts %>%
  summarise(sum(galloway_count),sum(lib_count),sum(abs(lib_count-galloway_count)),
            sum(abs(lib_count-galloway_count)>0))

 