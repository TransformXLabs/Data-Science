# BUSINESS SCIENCE UNIVERSITY
# LEARNING LAB 84: AI-POWERED LEAD SCORING APP
# PART 2: LANGCHAIN AND OPENAI
# ----

# GOAL: RUN A GENERATIVE AI MODEL TO GENERATE SQL QUERIES ON A COMPANY DATABASE

# SECURITY NOTE:
# Check with your company before running any AI models on your company's database.
# This is a demo proof-of-concept. We are using my anonymized data to show what you can do.
# OpenAI's API does not store any data per it's privacy policy.
# But you should still check with your company before running this code on your company's data.

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


# 2.0 SETTING UP AN LLM TO GENERATE SQL QUERIES

llm = OpenAI(
    temperature    = 0, 
    max_tokens     = 256,
    openai_api_key = os.getenv("OPENAI_API_KEY")
)

db = SQLDatabase(engine=sql_engine)

sql_chain = SQLDatabaseChain.from_llm(
    llm                       = llm, 
    db                        = db, 
    verbose                   = True,
    use_query_checker         = True,
    return_intermediate_steps = False,
)

# 3.0 MAKING QUERIES ---- 

sql_chain.run("What tables are in the database?")

pd.read_sql(sql.text("SELECT name FROM sqlite_master WHERE type='table';"), con=conn)

res = sql_chain.run("What are the top 5 customers based on p1 score?")

res = sql_chain.run("What are the top 5 customers based on p1 score located in the us?")

# 4.0 EXTRACTING THE AI-GENERATED SQL COMMAND ----

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

pd.read_sql(
    sql.text("SELECT user_full_name, user_email, p1 FROM leads_scored WHERE country_code = 'us' AND p1 IS NOT NULL ORDER BY p1 DESC LIMIT 5;"),
    con=conn
)

# CONCLUSION ----
# We can use AI to generate SQL queries on any SQL database.
# This is a powerful tool for data scientists and analysts because it will allow users to more easily query data 
# and automate powerful business insights.
