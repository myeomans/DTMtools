
################################################################
pos_tokens <- function(texts,
                            wstem="all",
                            language="english",
                            punct=FALSE,
                            stop.words=TRUE,
                            overlap=1,
                            sparse=0.99,
                            include_word_dependency=FALSE,
                            verbose=FALSE){



  texts <- textformat(texts, FALSE)
  texts <- ctxpand(texts)

  names(texts) <- 1:length(texts)
  texts[texts==""] <- " "
  parsedtxt <- spacyr::spacy_parse(texts, dependency=T,lemma=T,pos=T,tag=T,entity=T)
  dt_parsedtxt <- data.table(parsedtxt)
  dt_parsedtxt <- dt_parsedtxt[! pos %in% c("PUNCT","SPACE","SYM")]
  ######
  dt_parsedtxt[ , cleanlemma := lemma]
  dt_parsedtxt[cleanlemma=="-PRON-" , cleanlemma := token]
  dt_parsedtxt[,cleanlemma := tolower(cleanlemma)]
  dt_parsedtxt <-  dt_parsedtxt[!grepl("^\\s*$",cleanlemma)]
  ######
  if(is.character(stop.words)){
    dt_parsedtxt <- dt_parsedtxt[! cleanlemma %in% stop.words]
  } else if(stop.words){
    dt_parsedtxt <- dt_parsedtxt[! cleanlemma %in% tm::stopwords(language)]
  }
  ######
  dt_parsedtxt[ , clean_pos:= paste0(cleanlemma,"_",pos)]

  if(include_word_dependency){
    dt_parsedtxt <- word_dependency_parser(dt_parsedtxt)
    dt_clean_pos_by_id <- dt_parsedtxt[ , .(l_clean_pos = list(c(clean_pos,parses[!is.na(parses)]))), by = "doc_id"]

  } else {
    dt_clean_pos_by_id <- dt_parsedtxt[ , .(l_clean_pos = list(clean_pos)), by = "doc_id"]
  }
  l_pos_words <- dt_clean_pos_by_id[,l_clean_pos]
  names(l_pos_words) <- dt_clean_pos_by_id[ , as.character(doc_id)]

  orig_unique_ids <- names(texts)
  l_pos_words <- l_pos_words[orig_unique_ids]
  names(l_pos_words) <- orig_unique_ids
  dpm <- as.matrix(quanteda::dfm(unlist(lapply(l_pos_words, ngrammer, 1)),tolower=FALSE))
  dpm <- dpm[,colSums(dpm)>1]
  if ((sparse<1)) dpm<-dpm[,colMeans(dpm>0)>=(1-sparse)]


  # make sure that tokens with only one POS dont have pos tag
  col_names <- colnames(dpm)
  colnames(dpm) <- remove_redundant_pos_tags(col_names)
  #

  return(dpm)
}

remove_redundant_pos_tags <- function(col_names){
  v_s_allowed_pos <- c("NOUN", "VERB" , "ADV" , "DET" ,"ADJ", "PROPN" , "NUM", "ADP", "CCONJ"  )
  dt_col_names <- data.table(orig_name = col_names)
  dt_col_names[ , token := gsub("_[A-Z]+$","",orig_name)]
  dt_col_names[ , pos := gsub("^.*_","",orig_name)]
  dt_col_names[ ! pos %in% v_s_allowed_pos, pos:= ""]

  dt_token_pos <- unique(dt_col_names[ , .(token,pos)])
  dt_count_lemma_by_pos <- dt_token_pos[ , count := length(pos), by=.(token)]
  multi_pos_lemmas <- dt_count_lemma_by_pos[count>1 , unique(token)]

  dt_col_names[ , final_name := ifelse(token %in% multi_pos_lemmas,
                                       orig_name,
                                       token)]
  return(dt_col_names$final_name)
}
