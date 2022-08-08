#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Aug  5 18:54:15 2022

@author: svenmaurice
"""

from bs4 import BeautifulSoup
import pandas as pd
import urllib3
import requests

url_scrape = "https://theclergydatabase.org.uk/jsp/locations/DisplayLocation.jsp?locKey=3"

webpage = requests.get(url_scrape)

soup = BeautifulSoup(webpage.text, features="html.parser")
