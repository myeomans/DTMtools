#' Document Term Matricizer
#' @description Turns text into data.
#' @param texts a character vector of texts.
#' @param sparse maximum feature sparsity for inclusion (1 = include all features)
#' @param wstem character what words should be stemmed?
#' @param ngrams numeric vector of ngram sizes (max = 1:3)
#' @param language character what language are you parsing?
#' @param vocabmatch matrix used to create a new matrix with features that are identical to a previous one
#' @param stop.words logical should stop words be included? default is TRUE
#' @param punct logical should exclamation points and question marks be included as features?
#' @param POS logical should features have part of speech tags appended? default is FALSE
#' @param dependency logical should features have dependency relations appended? default is FALSE
#' @param tag.sub numeric what fraction of features should be replaced by POS tags? default is 0 (no features), fractions not supported yet.
#' @param overlap numeric How dissimilar (in cossine distance) must an ngram be from all (n-1)grams to be added to feature set?
#' @param group.conc character group IDs for removing group-specific words
#' @param group.conc.cutoff numeric threshold for group-specificity of words, as proportion of occurences in the main group.
#' @param TPformat logical - return in stm::textProcessor() format?
#' @param verbose logical - report interim steps during processing
#' @return Feature counts, as a matrix (or in stm format)
DTM<-function(texts,
              sparse=0.99,
              wstem="all",
              ngrams=1,
              language="english",
              vocabmatch=NULL,
              stop.words=TRUE,
              punct=FALSE,
              POS=FALSE,
              dependency=FALSE,
              tag.sub=0,
              overlap=.8,
              group.conc=NULL,
              group.conc.cutoff=0.8,
              TPformat=FALSE,
              verbose=FALSE,
              mc.cores=1){

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
  if(POS|dependency|tag.sub>0){
    dtm<-posTokens(texts=texts,
                   ngrams=ngrams,
                   language=language,
                   punct=punct,
                   stop.words=stop.words,
                   overlap=overlap,
                   sparse=sparse,
                   dependency=dependency,
                   tag.sub=tag.sub,
                   verbose=verbose)
  } else {
    dtm<-ngramTokens(texts=texts,
                     wstem=wstem,
                     ngrams=ngrams,
                     language=language,
                     punct=punct,
                     stop.words=stop.words,
                     overlap=overlap,
                     sparse=sparse,
                     verbose=verbose,
                     mc.cores=mc.cores)
  }
  #######################################################
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
