

# RESOURCE: 
# https://python.langchain.com/docs/modules/chains/popular/sqlite
# https://python.langchain.com/docs/modules/agents/toolkits/sql_database

# LIBRARIES ----
import pandas as pd
import sqlalchemy as sql
import os

from langchain import OpenAI
from langchain.sql_database import SQLDatabase
from langchain.chains import SQLDatabaseChain

# SET YOUR OPENAI API KEY ----
# os.environ["OPENAI_API_KEY"] = "YOUR_API_KEY"



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

# 2.0 SETTING UP AN LLM TO GENERATE SQL QUERIES

llm = OpenAI(
    temperature    = 0, 
    max_tokens     = 256,
    openai_api_key = os.getenv("OPENAI_API_KEY")
)

db = SQLDatabase(
    engine=sql_engine, 
    metadata=metadata
)

sql_chain = SQLDatabaseChain.from_llm(
    llm                       = llm, 
    db                        = db, 
    verbose                   = True,
    use_query_checker         = True,
    return_intermediate_steps = False,
)

# MAKING QUERIES ---- 

sql_chain.run("What tables are in the database?")

pd.read_sql(sql.text("SELECT name FROM sqlite_master WHERE type='table';"), con=conn)

res = sql_chain.run("What are the top 5 customers based on p1 score?")

res = sql_chain.run("What are the top 5 customers based on p1 score located in the us?")

# EXTRACTING THE SQL COMMAND ----

sql_chain_with_steps = SQLDatabaseChain.from_llm(
    llm                       = llm, 
    db                        = db, 
    verbose                   = True,
    use_query_checker         = True,
    return_intermediate_steps = True,
)

res = sql_chain_with_steps("What are the top 5 customers based on p1 score located in the us?")

sql_text = res['intermediate_steps'][1]

pd.read_sql_query(sql.text(sql_text), conn)

pd.read_sql(sql.text("SELECT user_full_name, user_email, p1 FROM leads_scored WHERE country_code = 'us' AND p1 IS NOT NULL ORDER BY p1 DESC LIMIT 5;"), con=conn)


