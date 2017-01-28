# require(tm)
# require(qdap)
# require(Matrix)
# require(SnowballC)
############################################################################
# THE BIG WRAPPER FUNCTION
############################################################################
DTM<-function(exps, sparse=0.99, wstem="all",
              ngrams=1, overlap=1,
              vocabmatch=NULL,
              language="english",
              TPformat=FALSE,
              stop.words=TRUE,
              verbose=FALSE){
  cleanertext<-unlist(sapply(exps, cleantext, language, stop.words))
  gtm<-list()
  for (ng in 1:length(ngrams)){
    tokens<-unlist(sapply(cleanertext, function(x) gramstem(x, wstem, ngrams[ng], language)))
    gtm[[ng]] <- tm::DocumentTermMatrix(tm::Corpus(tm::VectorSource(tokens)))
    if (sparse<1) gtm[[ng]]<-tm::removeSparseTerms(gtm[[ng]], sparse=sparse)
    if (ng==1) dtm<-gtm[[1]]
    if ((overlap!=1)&(ng>1)) dtm<-overlaps(dtm, gtm[[ng]], overlap)
    if ((overlap==1)&(ng>1)) dtm<-Matrix::cBind(dtm, gtm[[ng]])
    if (verbose) print(paste(c(ng, dim(dtm),dim(gtm[[ng]]))))
  }
  #######################################################
  DSM<-Matrix::sparseMatrix(i=dtm$i, j=dtm$j, x=dtm$v,
                    dims=c(dtm$nrow, dtm$ncol),
                    dimnames=list(NULL, colnames(dtm)))
  if (length(ngrams)>1) DSM<-doublestacker(DSM)
  if(!is.null(vocabmatch)) DSM<-DTMmatch(vocabmatch, DSM)
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
