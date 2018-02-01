#' POS Tagger
#' @description Tally parse-dependent features
#' @param texts a character vector of texts.
#' @return a matrix of feature counts
#' @import data.table
posTokens <- function(texts,
                      ngrams=1,
                      language="english",
                      punct=FALSE,
                      stop.words=TRUE,
                      overlap=1,
                      sparse=0.99,
                      dependency=FALSE,
                      verbose=FALSE){

  names(texts) <- 1:length(texts)
  texts[texts==""] <- " "
  parsedtxt <- spacyr::spacy_parse(texts, dependency=T,lemma=T,pos=T,tag=T,entity=F)
  dt_parsedtxt <- data.table::data.table(parsedtxt)
  if(punct){
    dt_parsedtxt <- dt_parsedtxt[(! pos %in% c("PUNCT","SPACE","SYM"))|(lemma %in% c("!","?"))]
  } else {
    dt_parsedtxt <- dt_parsedtxt[! pos %in% c("PUNCT","SPACE","SYM")]
  }
  ######
  dt_parsedtxt[ , cleanlemma := lemma]
  dt_parsedtxt[cleanlemma=="-PRON-" , cleanlemma := token]
  dt_parsedtxt[,cleanlemma := tolower(cleanlemma)]
  dt_parsedtxt <-  dt_parsedtxt[!grepl("^\\s*$",cleanlemma)]
  dt_parsedtxt[cleanlemma=="?" , cleanlemma := "qmark"]
  dt_parsedtxt[cleanlemma=="!" , cleanlemma := "xmark"]
  ######
  if(is.character(stop.words)){
    dt_parsedtxt <- dt_parsedtxt[! cleanlemma %in% stop.words]
  } else if(stop.words){
    dt_parsedtxt <- dt_parsedtxt[! cleanlemma %in% tm::stopwords(language)]
  }
  ######
  dt_parsedtxt[ , clean_pos:= paste0(cleanlemma,"_",pos)]

  if(dependency){
    dt_parsedtxt <- tagDependency(dt_parsedtxt)
    dt_clean_pos_by_id <- dt_parsedtxt[ , .(l_clean_pos = list(c(clean_pos,parses[!is.na(parses)]))), by = "doc_id"]

  } else {
    dt_clean_pos_by_id <- dt_parsedtxt[ , .(l_clean_pos = list(clean_pos)), by = "doc_id"]
  }
  l_pos_words <- dt_clean_pos_by_id[,l_clean_pos]
  names(l_pos_words) <- dt_clean_pos_by_id[ , as.character(doc_id)]

  orig_unique_ids <- names(texts)
  l_pos_words <- l_pos_words[orig_unique_ids]
  names(l_pos_words) <- orig_unique_ids

  l_pos_words <- dropRedundantTags(l_pos_words, sparse=sparse)

  dgm<-list()
  for (ng in 1:length(ngrams)){
    dgm[[ng]] <- as.matrix(quanteda::dfm(unlist(lapply(l_pos_words, ngrammer, ngrams[ng])),tolower=FALSE))
    dgm[[ng]]<-dgm[[ng]][,colSums(dgm[[ng]])>1]

    if ((sparse<1)) dgm[[ng]]<-dgm[[ng]][,colMeans(dgm[[ng]]>0)>=(1-sparse)]
    if (ng==1) dpm<-dgm[[1]]
    if ((ng>1)&(!is.null(dim(dgm[[ng]])))) dpm<-DTMtools::overlaps(dpm, dgm[[ng]], overlap)

    if (verbose) print(paste(c(ng, dim(dpm),dim(dgm[[ng]]))))
  }
  return(dpm)
}