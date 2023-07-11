# How To Get Started

## Create The Environment

``` bash
conda create -n lab_84_langchain python=3.9
```

## Activate the environment

``` bash
conda activate lab_84_langchain
```

## Install the requirements

### Option 1: Install the requirements from the requirements.txt file

``` bash
pip install -r requirements.txt
```

### Option 2: Use pip to install the requirements

``` bash
pip install langchain
pip install openai
pip install pycaret
pip install xgboost
pip install streamlit
pip install streamlit-chat
```


# VSCode Instructions (Optional)

## Required Extensions:

Python, Jupyter, Pylance, Pylance-Extension-Pack

## Python Select Interpreter

Select: lab_84_langchain

## Open a New Terminal Window

Verify lab_84_langchain is active


# OpenAI API Key

## IMPORTANT: COST MANAGEMENT!!!

The openai api is NOT a free service. There are costs associated with using the API.


## Create an OpenAI Account & Get an API Key

https://platform.openai.com/account/api-keys

## Cost Management


https://platform.openai.com/account/usage

## Set the API Key as an Environment Variable

Follow the OpenAI API Key best practices: https://help.openai.com/en/articles/5112595-best-practices-for-api-key-safety

### Windows: 

``` bash
setx OPENAI_API_KEY "<yourkey>"

echo %OPENAI_API_KEY%
```

### Mac/Linux: 

``` bash
echo "export OPENAI_API_KEY='yourkey'" >> ~/.zshrc

source ~/.zshrc

echo $OPENAI_API_KEY
```

# Troubleshooting

Make sure to quit VSCODE Application so the environment variables are set correctly and recognized.

Then you are ready to go!
