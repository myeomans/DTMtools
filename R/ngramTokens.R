#' Ngram Tagger
#' @description Tally bag-of-words ngram features
#' @param texts a character vector of texts.
#' @param wstem character what words should be stemmed?
#' @param ngrams numeric vector of ngram sizes (max = 1:3)
#' @param language character what language are you parsing?
#' @param punct logical should exclamation points and question marks be included as features?
#' @param stop.words logical should stop words be included? default is TRUE
#' @param overlap numeric How dissimilar (in cossine distance) must an ngram be from all (n-1)grams to be added to feature set?
#' @param sparse maximum feature sparsity for inclusion (1 = include all features)
#' @param verbose logical - report interim steps during processing
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
                       verbose=FALSE,
                      mc.cores=1){

  cleanertext<-unlist(parallel::mclapply(texts, cleantext, language, stop.words, punct,
                                         mc.cores = mc.cores))

  dgm<-lapply(ngrams, function(x) as.matrix(array(NA, c(length(texts),100))))
  token.list<-list()
  for (ng in 1:length(ngrams)){
    tokens<-unlist(parallel::mclapply(cleanertext, gramstem, wstem=wstem, ngrams=ngrams[ng], language=language,
                                      mc.cores= mc.cores))
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
