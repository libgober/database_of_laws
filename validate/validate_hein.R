library(tidyverse)
library(lubridate)
all_laws = read_csv("heinonline/hein_data_raw.csv")
errors = read_csv("heinonline/hein_errors.csv") %>%
  mutate(date=parse_date_time(date,orders=c('%Y-%m-%d','mdy')))

tmp1 = all_laws %>%
  left_join(errors) %>%
  mutate(chapter=ifelse(is.na(new_chapter),chapter,new_chapter)) %>%
  select(-new_chapter)


acts = tmp1 %>%
  filter(action %in% c("An Act"))



acts %>%
  filter(is.na(sal_page_start)) %>% 
  write.csv("heinonline/missing_sal_page.csv")

# Validation

pre.57 = acts %>%
  filter(congress_number<57) 

#
number.of.non.unique.entries = pre.57 %>%
  group_by(congress_number,
           session_number,
           chapter) %>%
  count() %>%
  filter(n>1) %>%
  nrow()

pre.57.chapter.counts = pre.57 %>%
  group_by(congress_number,
           session_number)  %>%
  summarise(n.chapters = max(chapter))

middle.


pre.57.expected.chapters = pre.57.chapter.counts %>%
  cross_join(data.frame(
    chapter=1:(pre.57.chapter.counts$n.chapters %>% max())
  )) %>%
  filter(chapter<=n.chapters) %>%
  select(-n.chapters)

gaps = pre.57.expected.chapters %>%
  left_join(pre.57) %>%
  filter(is.na(sal_page_start)) %>%
  select(congress_number:chapter) 

gaps %>%
  group_by(congress_number,session_number) %>%
  mutate(lead(chapter))

gaps %>%
  mutate(some.follows=lead(chapter)==chapter+1,
         some.precedes=lag(chapter)==chapter-1) %>%
  filter(!(some.follows & some.precedes))


volumes.of.congress.sessions = pre.57 %>%
  select(congress_number,session_number,sal_volume) %>%
  distinct()

#https://mattboegner.com/improve-your-sql-skills-master-the-gaps-islands-problem/
gaps.to.check = pre.57.expected.chapters %>%
  left_join(pre.57)  %>%
  select(-date,-hol_id,-action,-title,-pl_no) %>%
  group_by(congress_number,
           session_number,
           law.missing=is.na(sal_page_start)) %>%
  group_by(congress_number,
           session_number,
           law.missing) %>%
  mutate(values_rank=row_number()) %>% 
  mutate(sequence_grouping=chapter-values_rank) %>%
  group_by(congress_number,session_number,law.missing,sequence_grouping) %>%
  summarise(gap_chapter_start=min(chapter),gap_chapter_end=max(chapter)) %>%
  select(-sequence_grouping) %>%
  filter(law.missing) %>%
  left_join(volumes.of.congress.sessions)

gaps.to.check %>%
  filter(gap_chapter_start!=gap_chapter_end) %>%
  arrange(desc(gap_chapter_end-gap_chapter_start)) %>%
  write.csv("heinonline//gaps_checks.csv")
         

pre.57 %>%
  group_by(congress_number,action) %>%
  count() %>%
  write.csv("heinonline/counts_of_acts.csv")

from.57.to.85 = acts %>%
  filter(congress_number>=57 & congress_number <= 85)  %>%
  unique()


#
number.of.non.unique.entries = from.57.to.85 %>%
  group_by(congress_number,
           session_number,
           chapter,
           pl_no) %>%
  count() %>%
  filter(n>1) %>%
  nrow()

from.57.to.85 %>%
  unique() %>%
  group_by(congress_number,
           session_number,
           chapter,
           pl_no) %>%
  count() %>%
  filter(n>1) 

from.57.to.85 %>%
  group_by(pl_no,congress_number,session_number) %>%
  count() %>%
  filter(n>1) %>%
  write.csv("heinonline/nonunique_pl_nums_btwn_57_85.csv")


recycled_pls = from.57.to.85 %>%
  group_by(pl_no,congress_number,session_number) %>%
  count() %>%
  filter(n>1) %>%
  pull(pl_no)

from.57.to.85 %>% filter(pl_no %in% recycled_pls) %>% write.csv("tmp1.csv")
from.57.to.85 %>% filter(pl_no %in% recycled_pls) %>%  select(-hol_id) %>% View()

from.57.to.85 %>% 
  group_by(congress_number,session_number,chapter) %>%
  count() %>%
  filter(n>1) 



count.pre.57 = pre.57 %>%
  group_by(congress_number,action) %>%
  count() 

count57.to.85= from.57.to.85 %>%
  group_by(sal_volume,congress_number,session_number,pl_no,chapter) %>%
  count() %>%
  group_by(congress_number) %>%
  count()

#%%
bind_rows(count.pre.57,count57.to.85) %>%
  ungroup() %>%
  select(-action) %>% 
  write.csv("heinonline/counts_of_acts.csv")

#%% The last chunk

post85 = acts %>%
  filter(congress_number > 85)  %>%
  unique()


post85 %>% 
  group_by(pl_no) %>%
  count() %>%
  filter(n>1)


x = post85 %>%
  select(congress_number,pl_no) %>%
  unique() %>%
  group_by(congress_number) %>%
  count()

write.csv(x,"heinonline/counts_last_chunk.csv")
