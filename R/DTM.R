require(tm)
require(qdap)
require(Matrix)
require(SnowballC)
############################################################################
# THE BIG WRAPPER FUNCTION
############################################################################
DTM<-function(texts, sparse=0.99, wstem="all",
              ngrams=1, overlap=1,
              vocabmatch=NULL,
              language="english",
              punct=F,
              TPformat=FALSE,
              stop.words=TRUE,
              group.conc=NA,
              group.conc.cutoff=0.8,
              POS=F,
              verbose=FALSE){
  if(POS){
    dtm<-pos.tokens(texts,wstem,ngrams,language,punct,stop.words, verbose)
  } else {
    dtm<-ngram.tokens(texts,wstem,ngrams,language,punct,stop.words, verbose)
  }
  #######################################################
  dtm<-doublestacker(dtm)
  if (sparse<1) dtm<-dtm[,colMeans(dtm>0)>=(1-sparse)]
  sdtm<-Matrix::Matrix(dtm, sparse=T)
  if(!is.null(vocabmatch)) sdtm<-DTMmatch(vocabmatch, sdtm)
  # if(length(group.conc)==nrow(sdtm)){
  #   sdtm<-group.max.conc(sdtm, group.conc, cutoff=group.conc.cutoff)
  # }
  #######################################################
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
