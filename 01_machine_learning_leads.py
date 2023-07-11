

import pandas as pd
import sqlalchemy as sql
import pycaret.classification as clf

# 1.0 INSPECT THE DATABASE

sql_engine = sql.create_engine("sqlite:///database/leads_scored.db")

conn = sql_engine.connect()

# Table Names

metadata = sql.MetaData()

metadata.reflect(bind=sql_engine)

metadata.tables.keys()

# Read the tables

# This table contains our predictions
pd.read_sql_table('leads_scored', conn)

pd.read_sql_table('products', conn)

pd.read_sql_table('transactions', conn)

# 2.0 QUICK MACHINE LEARNING MODEL WITH PYCARET ----

df = pd.read_sql_table('leads_scored', conn) \
    .drop(columns=['predict', 'p0', 'p1', 'mailchimp_id'])

target = pd.read_sql_table('transactions', conn)['user_email'].unique()

df['purchased'] = df['user_email'].isin(target).astype(int)

clf.setup(
    data = df, 
    target = "purchased",
    session_id = 123,  
)

model_xgb = clf.create_model('xgboost')

clf.predict_model(model_xgb, data = df, raw_score=True)

# WE'D SAVE THIS TO OUR DATABASE ----

# Note - These predictions made with an H2O model and a lot of feature engineering
pd.read_sql_table('leads_scored', conn) 
