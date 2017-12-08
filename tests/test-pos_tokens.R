library(politeness)
library(DTMtools)
library(data.table)
library(quanteda)

PYTHON_PATH <- "/Users/alejandrokantor/anaconda2/bin/python"
spacyr::spacy_initialize(python_executable = PYTHON_PATH)


ptxt<-read.csv("data/practice_plans.csv",stringsAsFactors = F)

texts=ptxt$planSPELL[1:100]
texts = c("",texts)
texts[20] = ""
wstem="all"
language="english"
punct=FALSE
stop.words=TRUE
overlap=1
verbose=FALSE
sparse=0.99
include_word_dependency=TRUE

debug(pos_tokens)
undebug(pos_tokens)
length(texts)
m_out <- pos_tokens(texts=texts, sparse=0.99, include_word_dependency=TRUE)
nrow(m_out)

v_s_names <- colnames(m_out)
v_s_names <- v_s_names[order(v_s_names)]
v_s_names

apply(m_out, sum,MARGIN = 1)

data("phone_offers")
texts <- phone_offers$message
l_token_pos <- countTokensByPos(texts)

df_token_pos_text <- l_token_pos$df_token_pos_text
df_token_multiple_pos <- l_token_pos$df_token_multiple_pos
df_token_multiple_pos <- l_token_pos$df_token_single_pos

View(df_token_multiple_pos)
lookUpTokenPos(df_token_pos_text, s_token_pos = "much_ADJ")


lookUpTokenPos(df_token_pos_text, s_token_pos = "purchase_NOUN")

lookUpTokenPos(df_token_pos_text, s_token_pos = "plus_ADV")

