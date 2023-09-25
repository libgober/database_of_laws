import scrapy
from parsel import Selector 

class LawsAndSubjectsSpider(scrapy.Spider):
    name = 'laws_and_subjects'
    allowed_domains = ['congress.gov']
    
    def clean_output(out):
        for key in out:
            if type(out[key]) == list:
                out[key] = [i.strip() for i in out[key]]
            else:
                out[key] = out[key].strip()
        return(out)
    
    def start_requests(self):
        from requests import get
        response = get("https://www.congress.gov/public-laws/93rd-congress")
        root = Selector(response.text)
        pls = root.xpath("//select[@id='congresses']/option/@value").extract()
        for url in pls:
            yield scrapy.Request(url='https://www.congress.gov'+url,callback=self.parse)
        
    def parse(self, response):
        rows= response.xpath("""
                        //table[contains(@class,'item_table')]/tbody/tr
                        """)
        for row in rows:
            meta = {}
            meta['public_law_number'] = row.xpath(
                                'td[1]//text()').extract_first()
            meta['bill_number'] = row.xpath("td[2]/a/text()").extract_first()
            meta['bill_url'] = row.xpath("td[2]/a/@href").extract_first()
            meta['bill_title'] =row.xpath('td[2]/text()').extract()
            meta['date'] = row.xpath('td[3]/text()').extract_first()
            url=meta['bill_url'] + '/subjects?overview=closed'
            yield response.follow(url=url,
                                 meta=meta,
                                 callback=self.parse_bill_page)
            
    def parse_bill_page(self,response):
        out = response.meta
        out['policy_area'] = response.xpath(
            "//div[@class='column-aside']/ul/li/a/text()").extract_first()
        out['legislative_subjects'] = response.xpath(
            "//div[@class='column-main']//li/a/text()").extract()
        yield out
