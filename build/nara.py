#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Sep 22 16:08:51 2023

@author: brianlibgober
"""

from requests import get
import re
import pandas as pd
from parsel import Selector
from os.path import split,exists,join
#%%
def parse_block(block):    
    block = block.strip()
    elements = re.split("[\r\n]+",block)
    bill_law = elements[0]
    misc = elements[-1]
    title = ' '.join(elements[1:-1])
    bill, law = re.split("\s*/\s*",bill_law)
    date,sal_citation,pages = re.split(";\s*",misc.strip("()"))
    out = {}
    out['bill'] =bill
    out['public_law'] = law
    out['date'] = date
    out['sal_citation'] = sal_citation
    out['pages'] = pages
    out['title'] = title
    return(out)




def parse_summary(text,congress,session):
    blocks = re.findall("[HS]\..*?(?=\n\s*\n|\Z)", text, re.DOTALL)
    storage = []
    for block in blocks:
        parsed = parse_block(block)
        parsed['congress'] =congress
        parsed['session'] = 1 if session == 'first' else 2
        storage.append(parsed)
    return(storage)



#%%
template_url = 'https://www.archives.gov/files/federal-register/laws/{}-{}-session.txt'

all_laws = []
for congress in [115,116,117]:
    for session in ['first','second']:
        if (congress==117) and (session=='second'):
            continue
        url = template_url.format(congress,session)
        _, path = split(url)
        fpath = join("NARA",path)
        if not exists(fpath):
            response = get(url)
            with open(fpath,"w+") as f:
                f.write(response.text)
        with open(fpath) as f:
            text = f.read()
        info = parse_summary(text,congress,session)  
        all_laws =  all_laws + info
        
#%% now the more modern stuff
url = 'https://www.archives.gov/federal-register/laws/117-second-session'
response = get(url)
s = Selector(response.text)
rows = s.xpath("//table/tbody/tr")
info = []
for row in rows:
    out = {}
    out['bill'] = row.xpath("td[2]/text()").extract_first()
    out['public_law'] = row.xpath("td[1]/text()").extract_first()
    out['date'] = row.xpath("td[4]/text()").extract_first()
    out['sal_citation'] = row.xpath("td[5]/text()").extract_first()
    out['pages'] = row.xpath("td[6]/text()").extract_first()
    out['title'] = row.xpath("td[3]/text()").extract_first()
    out['congress'] = 117
    out['session'] = 2
    info.append(out)
    
all_laws = all_laws + info
#%%
d=pd.DataFrame(all_laws)
d.to_json("nara.jl",orient='records',lines=True)
