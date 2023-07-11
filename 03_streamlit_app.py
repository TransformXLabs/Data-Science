
# RUN APP:
# streamlit run 03_streamlit_app.py

# LIBRARIES ----

import streamlit as st
from streamlit_chat import message

import pandas as pd
import sqlalchemy as sql
import os

from langchain import OpenAI
from langchain.sql_database import SQLDatabase
from langchain.chains import SQLDatabaseChain

# SET YOUR OPENAI API KEY ----
# os.environ["OPENAI_API_KEY"] = "YOUR_API_KEY"

# CONNECT TO DATABASE ----

sql_engine = sql.create_engine("sqlite:///database/leads_scored.db")

conn = sql_engine.connect()

metadata = sql.MetaData()

metadata.reflect(bind=sql_engine)

metadata.tables.keys()

# LLMS ----

llm = OpenAI(
    temperature    = 0, 
    max_tokens     = 256,
    openai_api_key = os.getenv("OPENAI_API_KEY")
)

db = SQLDatabase(
    engine=sql_engine, 
    metadata=metadata
)

sql_chain_with_steps = SQLDatabaseChain.from_llm(
    llm                       = llm, 
    db                        = db, 
    verbose                   = True,
    use_query_checker         = True,
    return_intermediate_steps = True,
)

# FUNCTIONS ----

# We will get the user's input by calling the get_text function
def get_text():
    input_text = st.text_input("You: ","Hello, how can I help?", key="input")
    return input_text

# We will generate the response by calling the generate_response function
def generate_chat_response(prompt):
    
    # Generate the response
    res = sql_chain_with_steps(prompt)
    
    # chat_response = f"""
    # {res['result']}
    # """
    
    sql_query_df = pd.read_sql_query(sql.text(res['intermediate_steps'][1]), conn)
    
    print(sql_query_df)
    
    chat_response = f'''
    {sql_query_df.to_markdown()}
    '''
    
    return chat_response

def on_btn_click():
    del st.session_state.past[:]
    del st.session_state.generated[:]
    
def on_input_change():
    user_input = st.session_state.user_input
    st.session_state.past.append(user_input)
    st.session_state.generated.append("The messages from Bot\nWith new line")

# APP ----

st.title("Lead Scoring Query Chatbot")

# col1, col2 = st.columns(2)

# Storing the chat
if 'generated' not in st.session_state:
    st.session_state['generated'] = []

if 'past' not in st.session_state:
    st.session_state['past'] = []
    
user_input = get_text()

if user_input:
    output = generate_chat_response(user_input)
    # store the output 
    st.session_state.past.append(user_input)
    st.session_state.generated.append(output)

if st.session_state['generated']:
    
    for i in range(len(st.session_state['generated'])-1, -1, -1):
        message(
            st.session_state["generated"][i], key=str(i),
            is_table=True
        )
        message(
            st.session_state['past'][i], is_user=True, 
            key=str(i) + '_user'
        )
    st.button("Clear message", on_click=on_btn_click)

