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

  dgm<-lapply(ngrams, function(x) as.matrix(array(NA, c(length(texts),100))))
  for (ng in 1:length(ngrams)){
    tokens<-unlist(parallel::mclapply(cleanertext, gramstem, wstem=wstem, ngrams=ngrams[ng], language=language,
                                      mc.cores= parallel::detectCores()))
    dgm[[ng]] <- as.matrix(quanteda::dfm(tokens))
    dgm[[ng]]<-dgm[[ng]][,colSums(dgm[[ng]])>1]
    if (ng==1) dtm<-dgm[[1]]
    if (ng>1) dtm<-overlaps(dtm, dgm[[ng]], overlap)

    if (verbose) print(paste(c(ng, dim(dtm),dim(dgm[[ng]]))))
  }
  dtm<-doublestacker(dtm)
  return(dtm)
}
