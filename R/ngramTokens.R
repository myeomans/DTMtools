#' Ngram Tagger
#' @description Tally bag-of-words ngram features
#' @param texts a character vector of texts.
#' @return a matrix of feature counts
#' @import data.table
ngramTokens<-function(texts,
                       wstem="all",
                       ngrams=1,
                       language="english",
                       punct=TRUE,
                       stop.words=TRUE,
                       overlap=1,
                       sparse=0.99,
                       verbose=FALSE){

  cleanertext<-unlist(parallel::mclapply(texts, cleantext, language, stop.words, punct,
                                         mc.cores = parallel::detectCores()))

  dgm<-lapply(ngrams, function(x) as.matrix(array(NA, c(length(texts),100))))
  token.list<-list()
  for (ng in 1:length(ngrams)){
    tokens<-unlist(parallel::mclapply(cleanertext, gramstem, wstem=wstem, ngrams=ngrams[ng], language=language,
                                      mc.cores= parallel::detectCores()))
    dgm[[ng]] <- as.matrix(quanteda::dfm(tokens))
    dgm[[ng]]<-dgm[[ng]][,colSums(dgm[[ng]])>1]
    if ((sparse<1)) dgm[[ng]]<-dgm[[ng]][,colMeans(dgm[[ng]]>0)>=(1-sparse)]
    if (ng==1) dtm<-dgm[[1]]
    if ((ng>1)&(!is.null(dim(dgm[[ng]])))) dtm<-DTMtools::overlaps(dtm, dgm[[ng]], overlap)

    if (verbose) print(paste(c(ng, dim(dtm),dim(dgm[[ng]]))))
  }
  dtm<-doublestacker(dtm)
  return(dtm)
}
