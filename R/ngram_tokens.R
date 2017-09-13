ngram_tokens<-function(texts,
                       wstem="all",
                       ngrams=1,
                       language="english",
                       punct=TRUE,
                       stop.words=TRUE,
                       overlap=1,
                       verbose=FALSE){

  cleanertext<-unlist(parallel::mclapply(texts, cleantext, language, stop.words,
                                         mc.cores = parallel::detectCores()))

  dgm<-list(as.matrix(array(NA, c(length(texts),100))))
  for (ng in 1:length(ngrams)){
    tokens<-unlist(parallel::mclapply(cleanertext, gramstem, wstem=wstem, ngrams=ngrams[ng], language=language,
                                      mc.cores= parallel::detectCores()))
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
