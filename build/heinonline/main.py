# -*- coding: utf-8 -*-
"""
@Author Brian Libgober
@Date August 6, 2023

"""

#%%
import requests
import tqdm
import subprocess
from parsel import Selector
import urllib
import regex as re
import pandas as pd
import pendulum
#%%

def parse_toc(i):
    """
    Takes a volume number of the Statutes at Large and extracts directory data    

    Parameters
    ----------
    i : integer
        Volume number.

    Returns
    -------
    directory_data : list of dicts.
        A list of directory data
    """
    with open("heinonline_tocs/" + str(i) + '.html',"r") as o:
        raw  = o.read()
    s = Selector(raw)
    law_descriptors = s.xpath("//ul[@class='dropdown-submenu']//li")
    directory_data = []
    for law in law_descriptors:
        directory_data.append(parse_law_html(law,i))
    return(directory_data)

def parse_law_html(law,volume):
    """
    Extracts relevant metadata from a table of contents entry.

    Parameters
    ----------
    law : parsel.selector.Selector
        An entry from the table of contents from HeinOnline.
    volume : int 
        Volume of Statutes at Large, since citation style depends on era

    Returns
    -------
    out : dict
        Values of the meta data

    """
    #create a container for output
    out = {'sal_volume' : volume}
    #extract the raw components
    raw_date = law.xpath("./div/i/text()").\
            extract_first().\
                replace(".","").\
                strip().\
                strip("-").\
                strip()
    raw_description = law.xpath("./div/text()").extract_first().strip()
    #more meta data for additoinal scraping, i.e. page length
    page_handle_url = law.xpath(
        "./div/div[@class='page_line']/a[@class='contents_page']/@href").\
        extract_first()
    try:
        out['hol_id'] = urllib.parse.parse_qs(page_handle_url)['id'][0]
    except:
        pass
    
    out['sal_page_start'] = law.xpath(
        "./div/div[@class='page_line']/a[@class='contents_page']/i/text()").\
        extract_first().\
        strip().\
        strip("Page").\
        strip()
    #parse the date
    try:
        out['date'] = pendulum.\
            from_format(raw_date,"MMM D, YYYY").\
            format("YYYY-MM-DD")
    except:
        return(out)
    #parse the description, depending on Congressional era
    if volume<32:
        PATTERN = """Chapter.+?(?P<chapter>\d+).+?
            (?P<congress_number>\d+).+Congress
            .+Session.+?(?P<session_number>\d+),
            (?P<action>.+?):(?P<title>.*)"""
        match = re.search(PATTERN,raw_description,flags=re.X)
    elif volume>70:
        PATTERN = """Public\s+Law\s+(?P<pl_no>\d+-\d+)[,\s]+ 
            (?P<congress_number>\d+).+Congress,
            .+Session.+?(?P<session_number>\d+),
            (?P<action>.+?):(?P<title>.+)
            """
        match = re.search(PATTERN,raw_description,flags=re.X)
    else: #i.e. between 57th and 84th Congress inclusive
        #includes chapter
        PATTERN = """
        Public\s+Law\s+(?P<pl_no>\d+-\d+).+
        Chapter.+?(?P<chapter>\d+).+?
        (?P<congress_number>\d+).+Congress
        .+Session.+?(?P<session_number>\d+),
        (?P<action>.+?):(?P<title>.+)
        """
        match = re.search(PATTERN,raw_description,flags=re.X)
    #clean whatever the regex got
    try: 
        cleaned_groups = {key : match.groupdict()[key].strip() for key in match.groupdict()}
    except: 
        return(out)
    out.update(cleaned_groups)

    return(out)
        


def format_url(i):
    """
    For each volume of the statutes at large, formats the table of contents
    request from HeinOnline
    
    Parameters
    ----------
    i : integer
        The volume number for the statutes at large.

    Returns
    -------
    url : string

    """
    url = "https://heinonline.org/HOL/Contents?handle=hein.statute/sal{:03}".format(i) + \
          "&id=1&size=2&index=statdocs&collection=statute"
    return(url)


def download_heinonline_tocs():
    #loop over the statutes at larget
    for i in tqdm.trange(1,131):
        url = format_url(i)
        response = subprocess.run(["curl", url], capture_output=True, text=True)
        with open("heinonline_tocs/" + str(i) + '.html','w+') as o:
            o.write(response.stdout)


def process_tocs():
    """
    Iterates over the volumes of SAL and combines toc data 

    Parameters
    ----------
    i : integer
        Volume number.

    Returns
    -------
    directory_data : list of dicts.
        A list of directory data
    """
    directory_data = []
    for i in tqdm.trange(1,131):
        #skip certain weird compilations of non-laws
        if (i>=6) and (i<=8):
            continue
        directory_data = directory_data + parse_toc(i)
    return(directory_data)


def main(download_heinonline=False):
    """
    Runs the script, redownloading from HeinOnline if necessary

    Returns
    -------
    None.

    """      
    if download_heinonline: 
        download_heinonline_tocs()
    directory_data = process_tocs()
    hein_data_all = pd.DataFrame(directory_data)
    out = hein_data_all[~hein_data_all.duplicated()]
    out.to_csv("hein_data_raw.csv",index=False)
#%%
    
main()