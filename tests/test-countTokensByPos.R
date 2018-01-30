library(DTMtools)
library(data.table)
library(quanteda)

data("phone_offers")
texts <- phone_offers$message
l_token_pos <- countTokensByPos(texts)

df_token_pos_text <- l_token_pos$df_token_pos_text
df_token_multiple_pos <- l_token_pos$df_token_multiple_pos
df_token_multiple_pos <- l_token_pos$df_token_single_pos

#View(df_token_multiple_pos)
lookUpTokenPos(df_token_pos_text, s_token_pos = "much_ADJ")


lookUpTokenPos(df_token_pos_text, s_token_pos = "purchase_NOUN")

lookUpTokenPos(df_token_pos_text, s_token_pos = "plus_ADV")

