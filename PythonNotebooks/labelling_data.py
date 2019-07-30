import re
import string
import pandas as pd
from nltk.corpus import stopwords
from textblob import TextBlob
import nltk

emoticons_str = r"""
    (?:
        [:=;] # Eyes
        [oO\-]? # Nose (optional)
        [D\)\]\(\]/\\OpP] # Mouth
    )"""

regex_str = [
    emoticons_str,
    r'<[^>]+>',  # HTML tags
    r'(?:@[\w_]+)',  # @-mentions
    r"(?:\#+[\w_]+[\w\'_\-]*[\w_]+)",  # hash-tags
    r'http[s]?://(?:[a-z]|[0-9]|[$-_@.&amp;+]|[!*\(\),]|(?:%[0-9a-f][0-9a-f]))+',  # URLs

    r'(?:(?:\d+,?)+(?:\.?\d+)?)',  # numbers
    r"(?:[a-z][a-z'\-_]+[a-z])",  # words with - and '
    r'(?:[\w_]+)',  # other words
    r'(?:\S)'  # anything else
]

tokens_re = re.compile(r'(' + '|'.join(regex_str) + ')', re.VERBOSE | re.IGNORECASE)
emoticon_re = re.compile(r'^' + emoticons_str + '$', re.VERBOSE | re.IGNORECASE)


# tokenize the text called by preprocess
def tokenize(s):
    return tokens_re.findall(s)


# stopwords removal
punctuation = list(string.punctuation)
stop = stopwords.words('english') + punctuation + ['rt', 'via']


# remove stopwords called by preprocess
def removeStopWords(s):
    notStopword = [word for word in s if word not in stop]
    return notStopword


# Porter Stemmer
ps = nltk.PorterStemmer()


def stemming(s):
    text = [ps.stem(word) for word in s]
    return text


# preprocess the text
def preprocess(s, lowercase=False):
    tokens = tokenize(s)
    if lowercase:
        tokens = [token if emoticon_re.search(token) else token.lower() for token in tokens]
    stop_word_removed = removeStopWords(tokens)
    stemmed_text = stemming(stop_word_removed)
    return " ".join(stemmed_text)


# sentiment extract
def sentiment_value(s):
    return TextBlob(s).sentiment.polarity
def sentiment_subject(s):
    return TextBlob(s).sentiment.subjectivity


def load_csv_to_df(file_name):
    df=pd.DataFrame.from_csv(file_name)
    df=df.dropna(how="all")
    return df

if __name__== "__main__":
    df = load_csv_to_df("ycombi_all.csv")
    dataset = df.filter(['post','topic'], axis=1)
    dataset['processed_post'] = dataset['post'].apply(lambda x: preprocess(x, True))
    dataset['sentiment'] = dataset['processed_post'].apply(lambda x: sentiment_value(x))
    dataset['subject'] = dataset['processed_post'].apply(lambda x: sentiment_subject(x))
    dataset.to_csv("labelled_ycombi_v2.csv")
    print(dataset.head(10))

