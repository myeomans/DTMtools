library(DTMtools)
library(data.table)
library(quanteda)

data("phone_offers")

texts=phone_offers$message[1:100]
texts = c("",texts)
texts[20] = ""
wstem="all"
language="english"
punct=FALSE
stop.words=TRUE
overlap=1
verbose=FALSE
sparse=0.99
dependency=TRUE

debug(posTokens)
undebug(posTokens)
length(texts)

texts <- texts[1:3]
m_out <- posTokens(texts=texts, sparse=0.99, dependency=TRUE)
nrow(m_out)


rowSums(m_out)

dt_tokens <- data.table(m_out)
dt_tokens[ , texts := texts]
#View(dt_tokens)

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

