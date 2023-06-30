import os

import pandas as pd
import sqlalchemy as sql

from langchain import OpenAI
from langchain.sql_database import SQLDatabase
from langchain.chains import SQLDatabaseChain
from langchain.agents import Tool, load_tools, initialize_agent

# os.environ["OPENAI_API_KEY"] = "YOUR_API_KEY"

# 1.0 INSPECT THE DATABASE

sql_engine = sql.create_engine("sqlite:///database/bike_orders_database.sqlite")

conn = sql_engine.connect()

# Table Names

metadata = sql.MetaData()

metadata.reflect(bind=sql_engine)

metadata.tables.keys()

# Read the tables

pd.read_sql_table('bikes', conn)

pd.read_sql_table('bikeshops', conn)

pd.read_sql_table('orderlines', conn)


# 2.0 SETTING UP AN LLM TO GENERATE SQL QUERIES & FIND BUSINESS INSIGHTS
# https://langchain-langchain.vercel.app/docs/modules/chains/popular/sqlite

# Create an LLM, a database, and a SQL language chain tool
llm = OpenAI(
    temperature    = 0, 
    max_tokens     = 256,
    model          = 'gpt-3.5-turbo',
    openai_api_key = os.getenv("OPENAI_API_KEY")
)

db = SQLDatabase(engine=sql_engine, metadata=metadata)

sql_chain = SQLDatabaseChain.from_llm(
    llm                       = llm, 
    db                        = db, 
    verbose                   = True,
    use_query_checker         = True,
    return_intermediate_steps = False,
)

sql_chain.run("What are the top 5 most popular bikes?")

# Using intermediate steps

sql_chain_with_steps = SQLDatabaseChain.from_llm(
    llm                       = llm, 
    db                        = db, 
    verbose                   = True,
    use_query_checker         = True,
    return_intermediate_steps = True,
)

result_chain = sql_chain_with_steps("What are the top 5 most popular bikes?")

len(result_chain)

result_chain.keys()

result_chain["intermediate_steps"][1]

# 3.0 RUN THE AGENT USING A PROMPT THAT GENERATES SQL CODE

prompt = """
What are the top 5 most popular bikes?
"""

ret = zero_shot_agent.run(prompt)

ret

prompt = """
What are the top 5 most popular bikes?

Use the list to create a table with the top 5 most popular bikes and both the quantity sold and the sales total for each bike.
"""


ret = zero_shot_agent.run(prompt)

ret

# USING THE SQL TO PRODUCE A TABLE

pd.read_sql_query(sql.text("SELECT * FROM bikes"), conn)


sql_text = 'SELECT model, COUNT(*) AS popularity FROM bikes INNER JOIN orderlines ON bikes."bike.id" = orderlines."product.id" WHERE orderlines."product.id" IS NOT NULL GROUP BY model ORDER BY popularity DESC LIMIT 5;'

pd.read_sql_query(sql.text(sql_text), conn)




# MAKE AN AGENT TO RUN THE SQL CHAIN

sql_tool = Tool(
    name        = "Bike Orders Database",
    func        = sql_chain.run,
    description = "Query bikes orders database.",
)

# Add math to improve GPT's ability to do math
tools = load_tools(["llm-math"], llm=llm)

tools.append(sql_tool)

# https://langchain-langchain.vercel.app/docs/modules/agents/agent_types/

zero_shot_agent = initialize_agent(
    agent          = "zero-shot-react-description",
    tools          = tools,
    llm            = llm,
    verbose        = True,
    max_iterations = 5,
)
