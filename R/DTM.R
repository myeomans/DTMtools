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
              verbose=FALSE){

  cleanertext<-unlist(sapply(texts, cleantext, language, stop.words, punct))
  gtm<-list()
  for (ng in 1:length(ngrams)){
    tokens<-unlist(sapply(cleanertext, function(x) gramstem(x, wstem, ngrams[ng], language)))
    gtm[[ng]] <- as.matrix(quanteda::dfm(tokens))
    if (ng==1) dtm<-gtm[[1]]
    if (ng>1){
      gtm[[ng]]<-gtm[[ng]][,colSums(gtm[[ng]])>1]
      if (overlap!=1) dtm<-overlaps(dtm, gtm[[ng]], overlap)
      if (overlap==1) dtm<-Matrix::cBind(dtm, as.matrix(gtm[[ng]]))
    }
    if (verbose) print(paste(c(ng, dim(dtm),dim(gtm[[ng]]))))
  }
  #######################################################
  dtm<-doublestacker(dtm)
  if (sparse<1) dtm<-dtm[,colMeans(dtm>0)>=(1-sparse)]
  DSM<-Matrix::Matrix(dtm, sparse=T)
  if(!is.null(vocabmatch)) DSM<-DTMmatch(vocabmatch, DSM)
  # if(length(group.conc)==nrow(DSM)){
  #   DSM<-group.max.conc(DSM, group.conc, cutoff=group.conc.cutoff)
  # }
  #######################################################
  if(!TPformat) return(DSM)
  if(TPformat){
    documents<-lapply(1:nrow(DSM), function(x) (rbind(which(DSM[x,]>0), DSM[x,][DSM[x,]>0])))
    #kept <- 1*(rowMeans(DSM)>0)
    vocab <- as.character(colnames(DSM))
    return(list(documents=documents,
                vocab=vocab,
                meta=NULL,
                docs.removed=0))
  }
}
############################################################################
