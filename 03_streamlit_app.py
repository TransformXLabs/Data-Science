# BUSINESS SCIENCE UNIVERSITY
# LEARNING LAB 84: AI-POWERED LEAD SCORING APP
# PART 3: STREAMLIT APP
# ----

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
    
    chatbot_response = res['result']
    chatbot_sql_code = res['intermediate_steps'][1]
    chatbot_sql_query_df = pd.read_sql_query(sql.text(chatbot_sql_code), conn)
    
    chat_response = [
        st.text(chatbot_response), 
        st.dataframe(chatbot_sql_query_df),
        st.code(chatbot_sql_code, language='sql', line_numbers=True),
    ]
    
    print(chatbot_response)
    
    return chat_response

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
                st.session_state["generated"][i], 
                key=str(i),
                is_table=False,
                allow_html=True
            )
            message(
                st.session_state['past'][i], 
                is_user=True, 
                key=str(i) + '_user',
                allow_html=True
            )

