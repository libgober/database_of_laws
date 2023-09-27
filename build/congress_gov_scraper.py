#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Sep 22 15:51:41 2023

@author: brianlibgober
"""

from requests import get
from selenium import webdriver
from parsel import Selector

#%%
driver = webdriver.Firefox()
#%%
def gather_congress(i):
    url = "https://www.congress.gov/public-laws/{}th-congress".format(i)
    driver.get(url)
    s = Selector(driver.page_source)
    rows = s.xpath("//table[contains(@class,'item_table')]/tbody/tr")
    
    collection = []
    for row in rows:    
        meta = {}
        meta['pl_no'] = row.xpath('td[1]//text()').extract_first()
        meta['bill_number'] = row.xpath("td[2]/a/text()").extract_first()
        meta['bill_url'] = row.xpath("td[2]/a/@href").extract_first()
        meta['bill_title'] =row.xpath('td[2]/text()').extract()
        meta['date'] = row.xpath('td[3]/text()').extract_first()
        collection.append(meta)
    
    return(collection)
        
#%%
blob = []
for i in range(115,118):
    blob =  gather_congress(i)