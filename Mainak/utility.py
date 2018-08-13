import pandas as pd
def load_csv_to_df(file_name):
    df=pd.DataFrame.from_csv(file_name)
    df=df.dropna(how="all")
    return df