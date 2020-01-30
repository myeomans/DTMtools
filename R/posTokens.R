#' POS Tagger
#' @description Tally parse-dependent features
#' @param texts a character vector of texts.
#' @param ngrams numeric vector of ngram sizes (max = 1:3)
#' @param language character what language are you parsing?
#' @param punct logical should exclamation points and question marks be included as features?
#' @param stop.words logical should stop words be included? default is TRUE
#' @param POS logical should features have part of speech tags appended? default is FALSE
#' @param overlap numeric How dissimilar (in cossine distance) must an ngram be from all (n-1)grams to be added to feature set?
#' @param sparse maximum feature sparsity for inclusion (1 = include all features)
#' @param dependency logical should features have dependency relations appended? default is FALSE
#' @param tag.sub numeric what fraction of features should be replaced by POS tags? default is 0 (no features), fractions not supported yet.
#' @param verbose logical - report interim steps during processing
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
                      tag.sub=0,
                      verbose=FALSE){
  if(!((tag.sub>=0)&(tag.sub<=1))) tag.sub<-0

  names(texts) <- 1:length(texts)
  texts[texts==""] <- " "
  parsedtxt <- spacyr::spacy_parse(texts, dependency=dependency,
                                   lemma=T,pos=T,tag=T,entity=F)
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
  } else if(!stop.words){
    dt_parsedtxt <- dt_parsedtxt[! cleanlemma %in% tm::stopwords(language)]
  }
  ######

  if (tag.sub==1){
    dt_clean_pos_by_id <- dt_parsedtxt[ , .(l_clean_pos = list(pos)), by = "doc_id"]
  } else if (tag.sub==0){
    dt_parsedtxt[ , clean_pos:= paste0(cleanlemma,"_",pos)]
    dt_clean_pos_by_id <- dt_parsedtxt[ , .(l_clean_pos = list(clean_pos)), by = "doc_id"]
  } else {
    stop("not supported")
  }


  l_pos_words <- dt_clean_pos_by_id[,l_clean_pos]
  names(l_pos_words) <- dt_clean_pos_by_id[ , as.character(doc_id)]

  orig_unique_ids <- names(texts)
  l_pos_words <- l_pos_words[orig_unique_ids]
  names(l_pos_words) <- orig_unique_ids

  if(tag.sub==0){
    l_pos_words <- dropRedundantTags(l_pos_words, sparse=sparse)
  }

  dgm<-list()
  for (ng in 1:length(ngrams)){
    dgm[[ng]] <- as.matrix(quanteda::dfm(unlist(lapply(l_pos_words, ngrammer, ngrams[ng])),tolower=FALSE))
    dgm[[ng]]<-dgm[[ng]][,colSums(dgm[[ng]])>1]

    if ((sparse<1)) dgm[[ng]]<-dgm[[ng]][,colMeans(dgm[[ng]]>0)>=(1-sparse)]
    if (ng==1) dpm<-dgm[[1]]
    if ((ng>1)&(!is.null(dim(dgm[[ng]])))) dpm<-DTMtools::overlaps(dpm, dgm[[ng]], overlap)

    if (verbose) print(paste(c(ng, dim(dpm),dim(dgm[[ng]]))))
  }
  if(dependency){
    dt_parsedtxt <- tagDependency(dt_parsedtxt)
    dt_parse_by_id <- dt_parsedtxt[ , .(l_parses = list(parses[!is.na(parses)])), by = "doc_id"]
    l_parses <- dt_parse_by_id[,l_parses]
    names(l_parses) <- dt_parse_by_id[ , as.character(doc_id)]

    depm<-as.matrix(quanteda::dfm(unlist(lapply(l_parses, ngrammer, 1)),tolower=FALSE))
    if ((sparse<1)) depm<-depm[,colMeans(depm>0)>=(1-sparse)]
    dpm<-cbind(depm,dpm)
  }
  return(dpm)
}
