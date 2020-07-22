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

def make_request(stock, start = "", end = ""):
    base = "https://finance.yahoo.com/quote"
    tail = "&interval=1d&filter=history&frequency=1d"
    if start == "" or end == "":
        s = ""
    else:
        p1 = date_to_int(start)
        p2 = date_to_int(end)
        s = "?period1=%s&period2=%s%s" % (p1, p2, tail)
    url = '%s/%s.HK/history%s' % (base, stock, s)
    return requests.get(url)

def parse_table(res):
    soup = BeautifulSoup(res.text)
    table = soup.find('table')
    heading = ['date', 'open', 'high', 'low', 'close', 'adj_close', 'volume']
    sdata = []
    for row in table.find_all("tr")[1:]:
        if len(row) != 7: 
            continue
        sdata.append([d.text for d in row])
    sdata = pd.DataFrame(sdata, columns = heading)
    sdata['date'] = pd.to_datetime(sdata['date'], format = '%b %d, %Y')
    for col in sdata.columns.values[1:]:
        sdata[col] = pd.to_numeric(
            sdata[col].str.replace(',', ''), errors = 'coerce')
    return sdata

def get_stock(stock, start = "", end = ""):
    res = make_request(stock, start, end)
    data = parse_table(res)
    return data

class HKEX:
    def __init__(self):
        self.equities = get_equities()
        self.today = datetime.now().date()
