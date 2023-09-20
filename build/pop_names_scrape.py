#!/usr/bin/env python3

# File: pop_names_scrape.py
# Author: Christina Leung and Brian Libgober
# Date: Sept 11, 2023
# Description: Scrapes the Popular Names Tool to map popular names for laws to their PL number.

#%%

from requests import get
from parsel import Selector
import regex
import pandas as pd
import tqdm
#%%

# checks if curr is a valid PL
def valid_pl(curr):
    t = curr.replace("-", "").strip() # remove the dash
    try:
        t = float(t) # should be able to convert to float
        return curr
    except:
        return None
    
    
    
#%%
def pull_data(container):
    sel = container 
    name = sel.xpath("p[@class='popular-name']/text()").extract()[0]
    pop_name_info = sel.xpath("p[@class='popular-name-information']") # date
    
    out = {'popular_name' : name } # write information to file
    
    law = pop_name_info.xpath('a[contains(@title,"Table III")]/text()').extract_first()
    
    # three forms the law thing might take
    if law is None:
        pass
    elif regex.match("\d+\\-\d+", law):
        out['public_law'] = law
    elif regex.match("\d{4},\s+ch\.\s+\d+",law):
        out['year_and_chapter'] = law
    else:
        raise ValueError("Unexpected law citation")

    stat = pop_name_info.xpath('.//a[contains(@title,"Stat")]/text()').extract_first()

    if stat is None:
        pass
    elif regex.match('\d+\s+Stat\.\s+\d+',stat):
        out['citation'] = stat
    else:
        pass
    # attempt to get the USC cite
    text_bits = pop_name_info.xpath(".//text()").extract()
     
    code = None
    for bit in text_bits:
         if regex.search('U\.?S\.?C\.?',bit):
             code = bit
         else:
             continue
     
    try:
        # Date for law
        date = pop_name_info.xpath('@datekey').extract_first()
    except:
        date = None
        
    try:
        # Case: no relevant info in entry, instead contains a reference to another row
        see_para = sel.xpath(".//p[@class='popular-name-information'][@content-type='see']")
        dup = see_para.xpath("./text()").extract_first()
        duplicate = dup[(dup.find(" ") + 1):]
    except:
        duplicate = None
        
    cite_text_bits = sel.xpath(".//p[@content-type='cite']//text()").extract()   
    title = False
    for bit in cite_text_bits:
        if 'title' in bit or 'Title' in bit:
            title=True
        
    
    out['item_no'] = sel.xpath("@item").extract_first()
    out['public_law'] = law
    out['date'] = date
    out['us_code'] = code
    out['duplicate'] = duplicate
    out['cite_contains_title'] = title
    return out
    
#%%


# try: 
#     # Look for PL number
#     val = sel.xpath("//p[@class='popular-name-information']/a[1]/text()").extract_first().rstrip()
#     if len(val.split(',')) > 1:
#         tmp = val.split(',')[1].strip()
#         law = valid_pl(tmp)
#     if "ch." in val:
#         law = None
# except:
#     law = None
    
    
# try:
#     # Look for Stat. citation
#     citation = sel.xpath("//p[@class='popular-name-information']/a[2]/text()").extract_first().rstrip()
# except:
#     citation = None


# try:
#     # Look for USC citation
#     code = sel.xpath("//p[@class='popular-name-information']/text()").extract()
#     found = 0
#     for c in code:
#         if "U.S.C" in c:
#             code = c[:-1]
#             found = 1
#     if found == 0:
#         code = None
# except:
#     code = None

# try:
#     # Case: no relevant info in entry, instead contains a reference to another row
#     dup = sel.xpath("//p[@class='popular-name-information']/text()").extract_first().rstrip()
#     if "See" in dup:
#         dup = dup[(dup.find(" ") + 1):]
#     else:
#         dup = None
# except:
#     dup = None

# try:
#     # Date for law
#     date = pop_name_info.xpath('@datekey').extract_first()
# except:
#     date = None
    
    
    

#%%
if __name__ == '__main__':
    # tool site
    response = get("https://uscode.house.gov/popularnames/popularnames.htm") 
    if (response.status_code != 200):
        raise Warning("Din't get the site data")    
    root = Selector(response.text)
    containers  = root.xpath("//div[@class='popular-name-table-entry']") # arr of container
    df_raw = []
    for container in tqdm.tqdm(containers):
        df_raw.append(pull_data(container))
    df=pd.DataFrame(df_raw)
    df.to_json(path_or_buf="build/popnames2.jsonl",orient='records',lines=True)
    