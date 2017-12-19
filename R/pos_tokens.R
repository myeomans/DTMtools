
################################################################
pos_tokens<-function(texts,
                     wstem="all",
                     language="english",
                     punct=FALSE,
                     stop.words=TRUE,
                     overlap=1,
                     sparse=0.99,
                     include_word_dependency=FALSE,
                     verbose=FALSE){




  texts<-textformat(texts, FALSE)
  texts<-ctxpand(texts)

  names(texts)<-1:length(texts)
  texts[texts==""]<-" "
  parsedtxt <- spacyr::spacy_parse(texts, dependency=T,lemma=T,pos=T,tag=T,entity=T)
  parsedtxt_copy <- parsedtxt
  parsedtxt<-parsedtxt[!(parsedtxt$pos%in%c("PUNCT","SPACE","SYM")),]
  ######
  parsedtxt$cleanlemma<-parsedtxt$lemma
  parsedtxt$cleanlemma[parsedtxt$cleanlemma=="-PRON-"]<-parsedtxt$token[parsedtxt$cleanlemma=="-PRON-"]
  parsedtxt$cleanlemma<-tolower(parsedtxt$cleanlemma)
  parsedtxt<-parsedtxt[!(parsedtxt$cleanlemma%in%c(""," ","  ")),]
  ######
  if((length(stop.words)>1)){
    parsedtxt<-parsedtxt[!(parsedtxt$cleanlemma%in%stop.words),]
  } else if(stop.words){
    parsedtxt<-parsedtxt[!(parsedtxt$cleanlemma%in%tm::stopwords(language)),]
  }
  ######

  parsedtxt$clean_pos<-paste0(parsedtxt$cleanlemma,"_",parsedtxt$pos)

  ######
  unique_ids <- unique(parsedtxt$doc_id)
  pos_words<-parallel::mclapply(unique_ids,
                                function(x) unlist(parsedtxt[parsedtxt$doc_id==x,"clean_pos"]),
                                mc.cores= parallel::detectCores())
  #####


  names(pos_words) <- unique_ids

  #
  if(include_word_dependency){
    parsed_dependency <- word_dependency_parser(parsedtxt)
    l_nonums <- parsed_dependency$nonums
    names(l_nonums) <- unique_ids
    #TODO optimize
    pos_words <- lapply(unique_ids, function(s_id) c(pos_words[[s_id]], l_nonums[[s_id]]))
  }
  # the following garantees that final matrix will be as long as length(texts)
  names(pos_words) <- unique_ids
  all_ids <- names(texts)
  ids_not_in_unique_ids <- setdiff(all_ids, unique_ids)
  if(length(ids_not_in_unique_ids)>0){
    for(s_id in ids_not_in_unique_ids){
      pos_words[[s_id]] = ""
    }
    pos_words <- pos_words[order(as.numeric(names(pos_words)))]
  }

  ## remove for loop and overaps and doubletacker

  dpm <- as.matrix(quanteda::dfm(unlist(lapply(pos_words, ngrammer, 1)),tolower=FALSE))
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
  df_col_names <- data.frame(orig_name = col_names,stringsAsFactors = FALSE)
  df_col_names$token = gsub("_[A-Z]+$","",df_col_names$orig_name)
  df_col_names$pos = gsub("^.*_","",df_col_names$orig_name)
  df_col_names$pos[ ! df_col_names$pos %in% v_s_allowed_pos] <- ""

  df_count_lemma_by_pos <- aggregate(df_col_names$pos, by=df_col_names["token"], FUN=length)
  multi_pos_lemmas <- df_count_lemma_by_pos[df_count_lemma_by_pos$x>1 , "token"]

  df_col_names$final_name = ifelse(df_col_names$token %in% multi_pos_lemmas,
                                   df_col_names$orig_name,
                                   df_col_names$token)
  return(df_col_names$final_name)
}
