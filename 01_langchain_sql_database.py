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

# Default Response

sql_chain = SQLDatabaseChain.from_llm(
    llm                       = llm, 
    db                        = db, 
    verbose                   = True,
    use_query_checker         = True,
    return_intermediate_steps = False,
)

sql_chain.run("What are the top 5 most popular bikes?")

# Response using intermediate steps

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

# 3.0 USING THE SQL TO PRODUCE A TABLE

pd.read_sql_query(sql.text("SELECT * FROM bikes"), conn)

sql_text = 'SELECT model, COUNT(*) AS popularity FROM bikes INNER JOIN orderlines ON bikes."bike.id" = orderlines."product.id" WHERE orderlines."product.id" IS NOT NULL GROUP BY model ORDER BY popularity DESC LIMIT 5;'

pd.read_sql_query(sql.text(sql_text), conn)

# 4.0 RUN THE AGENT USING A PROMPT THAT GENERATES SQL CODE

# * Example 1: What are the top 5 most popular bikes?

prompt = """
What are the top 5 most popular bikes?
"""

ret = sql_chain_with_steps(prompt)

ret['result']

ret['intermediate_steps'][1]

pd.read_sql_query(sql.text(ret['intermediate_steps'][1]), conn)

# * Example 2: total sales by unit and value?

prompt = """
What are the total sales in units and by total value (unit price times quantity)?
"""

ret = sql_chain_with_steps(prompt)

ret

ret['result']

ret['intermediate_steps'][1]

pd.read_sql_query(sql.text(ret['intermediate_steps'][1]), conn)

# * Example 3: total sales for each bike shop?

prompt = """
What are the total sales in units and by total value (unit price times quantity) by bikeshop for for the top 10 bike shops?
"""

ret = sql_chain_with_steps(prompt)

ret

ret['result']

ret['intermediate_steps'][1]

pd.read_sql_query(sql.text(ret['intermediate_steps'][1]), conn)

# * Example 4: Sales value (unit price times quantity) by order date for the top 5 bike shops?

prompt = """
Multiply the unit price by the quantity of bikes sold for each order.
"""

ret = sql_chain_with_steps(prompt)

ret

ret['result']

ret['intermediate_steps'][1]

pd.read_sql_query(sql.text(ret['intermediate_steps'][1]), conn)







# 3.0 CREATE AN AGENT TO RUN THE SQL CHAIN

sql_tool = Tool(
    name        = "Bike Orders Database",
    func        = sql_chain_with_steps,
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
    
    # Important to specify a max number of iterations or could run for a long time
    max_iterations = 5,  
)

