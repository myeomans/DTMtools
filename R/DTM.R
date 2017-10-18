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
              overlap=1,
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
    dtm<-pos_tokens(texts,wstem,ngrams,language,punct,stop.words, overlap, verbose)
  } else {
    dtm<-ngram_tokens(texts,wstem,ngrams,language,punct,stop.words, overlap, verbose)
  }
  #######################################################
  sdtm<-Matrix::Matrix(dtm, sparse=T)
  #if(!TPformat) return(sdtm)
  if ((sparse<1)) sdtm<-sdtm[,colMeans(sdtm>0)>=(1-sparse)]
  if(!is.null(group.conc)) sdtm<-group.max.conc(sdtm, group.conc, cutoff=group.conc.cutoff)
  if(!is.null(vocabmatch)) sdtm<-DTMmatch(vocabmatch, sdtm)
  # #######################################################
  if(!TPformat) return(sdtm)
  if(TPformat){
    documents<-lapply(1:nrow(sdtm), function(x) (rbind(which(sdtm[x,]>0), sdtm[x,][sdtm[x,]>0])))
    vocab <- as.character(colnames(sdtm))
    return(list(documents=documents,
                vocab=vocab,
                meta=NULL,
                docs.removed=0))
  }
}
############################################################################
