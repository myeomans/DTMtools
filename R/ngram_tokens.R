ngram_tokens<-function(texts,
                       wstem="all",
                       ngrams=1,
                       language="english",
                       punct=F,
                       TPformat=FALSE,
                       stop.words=TRUE,
                       overlap=1,
                       verbose=FALSE){
  cleanertext<-unlist(sapply(texts, cleantext, language, stop.words))
  dgm<-list()
  for (ng in 1:length(ngrams)){
    tokens<-unlist(sapply(cleanertext, function(x) gramstem(x, wstem, ngrams[ng], language)))
    dgm[[ng]] <- as.matrix(quanteda::dfm(tokens))
    dgm[[ng]]<-dgm[[ng]][,colSums(dgm[[ng]])>1]
    if (ng==1) dtm<-dgm[[1]]
    if (ng>1){
      if (overlap!=1) dtm<-overlaps(dtm, dgm[[ng]], overlap)
      if (overlap==1) dtm<-Matrix::cBind(dtm, as.matrix(dgm[[ng]]))
    }
    if (verbose) print(paste(c(ng, dim(dtm),dim(dgm[[ng]]))))
  }
  return(dtm)
}
