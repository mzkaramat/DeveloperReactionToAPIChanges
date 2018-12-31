import pandas as pd
import re
import string
def load_csv_to_df(file_name):
    df=pd.DataFrame.from_csv(file_name)
    df=df.dropna(how="all")
    return df

cleanr = re.compile('<.*?>')
def cleanhtml(raw_html):
  cleantext = re.sub(cleanr, '', raw_html)
  return cleantext

def onlyASCII(text):
    printable = set(string.printable)
    return ''.join(filter(lambda x: x in printable, text))

def cleanText(text):
    clean_text=cleanhtml(text)
    return onlyASCII(clean_text)