import json
from collections import namedtuple
import requests
import argparse
import urllib
import pandas as pd 

RESOURCE_URL="https://hn.algolia.com/api/v1/search_by_date?"
columns=["created_at","title","post"]
DATA_ITERATION=1000
FILE_NAME=""

def connect_to_media(url):
    resp = requests.get(url)
    return resp

def get_query(query):
    query_data={}
    query_data["query"]=query
    query_data["tags"]="comment"
    query_data["page"]="page_no"
    return urllib.parse.urlencode(query_data)

def parse_data(response,header):
    json_data=response.json()["hits"]
    no_records=response.json()["hitsPerPage"]
    current_page_no=response.json()["page"]
    df=pd.DataFrame(columns=columns,index=range(no_records))
    i=0
    for data in json_data:
        df.ix[i][columns[0]]=data["created_at"]
        df.ix[i][columns[1]]=data["story_title"]
        df.ix[i][columns[2]]=data["comment_text"]
        i+=1
    with open(FILE_NAME, 'a') as f:
        df.to_csv(f, header=header, index=False)
    return current_page_no

def call_api_iteration():
    header=True
    current_page_no=-1
    for i in range(DATA_ITERATION):
        url=RESOURCE_URL.replace("page_no",str(current_page_no+1))
        response=connect_to_media(url)
        if i==0:
            header=True
        else:
            header=False
        current_page_no= parse_data(response,header)
     
if __name__== "__main__":
    parser=argparse.ArgumentParser()
    parser.add_argument("--query",help="Add query string")
    parser.add_argument("--file",help="Add tags")
    args=parser.parse_args()
    RESOURCE_URL=RESOURCE_URL+get_query(args.query)
    FILE_NAME=args.file
    call_api_iteration()