library(politeness)
library(DTMtools)
library(data.table)
library(quanteda)

PYTHON_PATH <- "/Users/alejandrokantor/anaconda2/bin/python"
spacyr::spacy_initialize(python_executable = PYTHON_PATH)

data("phone_offers")
texts <- phone_offers$message
l_token_pos <- countTokensByPos(texts)

df_token_pos_text <- l_token_pos$df_token_pos_text
df_token_multiple_pos <- l_token_pos$df_token_multiple_pos

View(df_token_multiple_pos)

lookUpTokenPos(df_token_pos_text, s_token_pos = "much_ADJ")

lookUpTokenPos(df_token_pos_text, s_token_pos = "purchase_NOUN")

lookUpTokenPos(df_token_pos_text, s_token_pos = "plus_ADV")

