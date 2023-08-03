# BUSINESS SCIENCE UNIVERSITY
# LEARNING LAB 84: AI-POWERED LEAD SCORING APP
# PART 3: STREAMLIT APP
# ----

# RUN APP:
# streamlit run 03_streamlit_app_rev_1.py

# LIBRARIES ----​
import streamlit as st
import pandas as pd
import sqlalchemy as sql
import os

from langchain import OpenAI
from langchain.sql_database import SQLDatabase
from langchain.chains import SQLDatabaseChain

# CONNECT TO DATABASE ----
sql_engine = sql.create_engine("sqlite:///database/leads_scored.db")
conn = sql_engine.connect()
metadata = sql.MetaData()
metadata.reflect(bind=sql_engine)

# LLMS ----
llm = OpenAI(
    temperature    = 0, 
    max_tokens     = 256, 
    openai_api_key = os.getenv("OPENAI_API_KEY")
)
db = SQLDatabase(engine=sql_engine, metadata=metadata)
sql_chain_with_steps = SQLDatabaseChain.from_llm(llm, db, verbose=True, use_query_checker=True, return_intermediate_steps=True)

# FUNCTIONS ----
def get_text():
    return st.text_input("You: ", "Hello, how can I help?", key="input")

def generate_chat_response(prompt):
    res = sql_chain_with_steps(prompt)
    chatbot_response = res['result']
    chatbot_sql_code = res['intermediate_steps'][1]
    chatbot_sql_query_df = pd.read_sql_query(sql.text(chatbot_sql_code), conn)
    return chatbot_response, chatbot_sql_query_df, chatbot_sql_code

# APP ----
st.set_page_config(layout="wide")
st.title("Lead Scoring Analyzer")
col1, col2 = st.columns(2)

with col1:
    st.header("Leads Scoring Data")
    tab1, tab2, tab3 = st.tabs(["Leads Scored", "Products", "Transactions"])
    with tab1:
        df = pd.read_sql_table('leads_scored', conn)
        st.dataframe(df)
    with tab2:
        df = pd.read_sql_table('products', conn)
        st.dataframe(df)
    with tab3:
        df = pd.read_sql_table('transactions', conn)
        st.dataframe(df)
        
with col2:
    st.header("Ask me anything about the lead scoring data")
    if 'chat_history' not in st.session_state:
        st.session_state['chat_history'] = []
        
    user_input = get_text()
    send_button_clicked = st.button('Apply')
    
    if send_button_clicked:
        chat_response, sql_query_df, sql_code = generate_chat_response(user_input)
        new_chat_entry = (user_input, chat_response, sql_query_df, sql_code)
        # Insert the new chat entry at the beginning of the chat history
        st.session_state['chat_history'].insert(0, new_chat_entry)
    
    for user_text, bot_text, query_df, query_code in st.session_state['chat_history']:
        st.text(f"You: {user_text}")
        st.text(f"Bot: {bot_text}")
        st.dataframe(query_df)
        st.code(query_code, language='sql', line_numbers=True)
        
    if st.button('Clear Chat History'):
        st.session_state['chat_history'] = []