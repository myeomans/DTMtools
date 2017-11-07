############################################################################
# THE BIG WRAPPER FUNCTION
############################################################################
DTM<-function(texts,
              sparse=0.99,
              wstem="all",
              ngrams=1,
              language="english",
              vocabmatch=NULL,
              stop.words=TRUE,
              POS=FALSE,
              punct=FALSE,
              overlap=.8,
              group.conc=NULL,
              group.conc.cutoff=0.8,
              TPformat=FALSE,
              verbose=FALSE){

  #######################################################
  if(!is.null(vocabmatch)){
    if(is.null(dim(vocabmatch))){
      stop("Vocabulary match must be a matrix")
    } else{
      overlap = 1
      sparse = 1
    }
  }
  if(!is.null(group.conc)){
    if(length(group.conc)!=length(texts)){
      stop("Group IDs must have same length as texts")
    }
  }
  #######################################################
  if(POS){
    dtm<-pos_tokens(texts,wstem,ngrams,language,punct,stop.words, overlap, sparse, verbose)
  } else {
    dtm<-ngram_tokens(texts,wstem,ngrams,language,punct,stop.words, overlap, sparse, verbose)
  }
  #######################################################
  #if(!TPformat) return(dtm)
  if(!is.null(group.conc)) dtm<-group.max.conc(dtm, group.conc, cutoff=group.conc.cutoff)
  if(!is.null(vocabmatch)) dtm<-DTMmatch(vocabmatch, dtm)
  # #######################################################
  if(!TPformat) return(dtm)
  if(TPformat){
    documents<-lapply(1:nrow(dtm), function(x) (rbind(which(dtm[x,]>0), dtm[x,][dtm[x,]>0])))
    vocab <- as.character(colnames(dtm))
    return(list(documents=documents,
                vocab=vocab,
                meta=NULL,
                docs.removed=0))
  }
}
############################################################################
