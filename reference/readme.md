# Introduction

This git contains files associated with the analysis of links between public laws and, eventually, a paper about how the dynamics of legislative attention explain ex post lobbying.

For now, the major goal of these set of files is to be able to rigorously estimate the time it takes for Congress to revisit a given legislative topic.

# More detailed introduction

We take three major approaches to identifying recurrence. 

- Topic-area codings for legislative acts
- Affecting common sections of the US Code
- Explicit reference to bill titles

Each approach may have different operationalizations. In the former case, the unit of analysis is the legislative topic. In the latter, the unit of analysis is the section.

For purposes of data analysis, we often prefer to start with tables where the units are laws, then aggregate up as needed. 

## Titles

Substantial effort has been made to enumerate all the laws and their titles enacted by the US Congress. The major source for this data is HEIN Online's Statutes at Large database. This covers laws enacted since the dawn fo the republic to 2012. To bring to currency, we supplement with data from Congress.gov. 

Laws have been identified across time in somewhat different ways, mostly but not always uniquely or consistently. 

- After the 85th Congress, public law numbers look pretty reliable and have the fomrat Cong No.-Law No.
- Between the 57th and 85th congress, public law numbers may be recycled across Congresional sessions, but chapters are commonly used. 
- Prior to the 57th session, laws are described as chapters within the statutes at large.

We construct two unique identifiers that may be used in linking data sources. Generally, these are close to more usual citations, but because those citations are not unique they are not really ideal for data analysis. The first unique identifer ("sol_id") disambiguates the statutes at large citation, where available (1789-2012). The format strings is as follows:

    sol_id_format='/stat/{volume:0.0f}/{page_no:0.0f}/{law_within_page:0.0f}'
    
The second unique identifier is based on the public law

    pl_id = /pl/{cong_no:0.0f}/{session:0.0f}/{law_in_cong_no:0.0f}/{pl_tie}
    
Public law is traditionally CongNo-LawInCongNo, however given the recycling of citations across sessions this is not unique. The pl_tie part is usually empty, however 79-160-A is an example law and so is 74-770.5. 


Generally, I recommend using PL post 1957/85th Congress, and using citation pre 1957, with some attention to the multiple laws per page problem or with chapter as well as citation. The Office of Revision Counsel's Table III tool, for example, identifies acts by year and chapter until August of 1956.


## Topic

We operationalize legislative topic using several hand-curated systems.

Library of Congress:

- 32 policy areas (https://www.congress.gov/help/faq/find-bills-by-subject)
- 1,004 legislative subjects (2009-Present)
- 5,500 legislative indexing terms (1973-2008)

Policy Agendas Project

- 20 major policy areas
- 217 pap subtopics

## US Code

We use various documents used in producing the US Code to construct networks of relationships between laws, in particular when law Y amends or embellishes a US Code section created by law X. The unit of analysis could be the public law or the public law section. 

The two main approaches we adopt for construction are the Table III tool and the source credit slug in the US Code. 

Table III

- Table III is a document that describes for each Public Law which part of the US Code it influences. 
- A key difficulty is that the US Code is rearranged from time to time, therefore information from Table 1 is imported to update the location of sections created by Public Law X at the time when Public Law Y i enacted.

Source Credits

- Each section of the US Code includes a slug describing the legislative history of how this particular piece of text came to be part of statutory law.
- We read through the entire US Code and parse these source credits for edit histories.

# Notes on Implementation: Very Technical and Subject to Amendment

## Identifying Laws

It might make sense to follow the approach of Office of Revision Counsel to identify laws by year and chapter prior to the 85th Congress and public law afterwards. 


## Source Credit Network

The source credit network currently makes no effort to deal with laws that lack a public law number. Indeed, the earliest law we observe in this data comes from the 85th Congress, exactly the previously identified cutoff where PL no got reliable. 

## Table III

Lots of cites in Table III are to removed sections or to notes. Code section "101 nt" occurs 662 times for example. 


# A User Guide

Step 1 - Clone the Git Repo
Step 2 - Run setup.sh to ensure that you have the right python3 packages.
Step 3 - Run get_files.sh
Step 4 - python master/make_master_csv.py makes the main csv, which has descriptions on the laws.
Step 5 - python master/make_networks.py makes a csv which contains information about connections between laws.

## Things You Don't Need to Do, But Might Want to Do

Recrawl the congress.gov site: scrapy runspider -o crawl_2021_06_22.jl public_laws/spiders/laws_and_subjects.py
Reacquire the popular names: python bill_titles/pop_names_scrape.py

# Notes for Improvement

- HeinOnline has a huge database of public laws. Looks very comprehensive between the beginning of time and 2012. Presumably after 2012 things are finally ratoinal from a data standpoint.



