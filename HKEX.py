import requests
import pandas as pd
import numpy as np
from datetime import datetime
from bs4 import BeautifulSoup

def get_equities():
    url = ("https://www.hkex.com.hk/eng/services/trading/" + 
    "securities/securitieslists/ListOfSecurities.xlsx")
    data = pd.read_excel(url, header = 2)
    data = data[data['Category'] == 'Equity']
    data = data[['Stock Code', 'Name of Securities']]
    data.columns = ['code', 'name']
    data['code'] = data.code.astype(str).str.zfill(4)
    return data

def date_to_int(date):
    org = 1594166400
    intv = 3600 * 24
    if type(date) == str:
        date = datetime.strptime(date, "%Y-%m-%d").date()
    elif type(date) in [datetime, pd._libs.tslibs.timestamps.Timestamp]:
        date = date.date()
    org_date = datetime.strptime("2020-07-08", "%Y-%m-%d").date()
    dif = (date - org_date).days
    return org + intv * dif

def get_stock(stock, start, end):
    base = 'https://query1.finance.yahoo.com/v7/finance/download/'
    tail = 'interval=1d&events=history'
    s = '%s.HK?' % (stock)
    p1 = 'period1=%s&' % (date_to_int(start))
    p2 = 'period2=%s&' % (date_to_int(end))
    url = base + s + p1 + p2 + tail
    data = pd.read_csv(url, parse_dates=[0])
    data = data.rename(str.lower, axis = 'columns')
    data = data.rename({'adj close': 'adj_close'}, axis = 'columns')
    return data

class HKEX:
    def __init__(self):
        self.equities = get_equities()
        self.today = datetime.now().date()
    
    def getStock(self, stock, start = None, end = None):
        if end is None:
            end = self.today
        if start is None:
            start = '2000-01-01'
        return get_stock(stock, start, end)
