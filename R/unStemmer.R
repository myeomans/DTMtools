#' The Un-Stemmer
#' @description Returns root words that match the stems
#' @param vocab a character vector of stemmed words.
#' @param texts a character vector of texts.
#' @return a matrix of feature counts
#' @import data.table
unStemmer<-function(vocab, texts, wstem="all",
                   ngrams=1,language="english", stop.words=TRUE){

  if(mean(ngrams==1)!=1){
    cleanertext<-unlist(sapply(texts, cleantext, language, stop.words))
    xfull<-unlist(sapply(cleanertext, gramstem, "none",ngrams, language))
  }else{
    xfull<-unlist(sapply(texts, cleantext, language, stop.words))
  }
  xes<-c("")
  for (ct in xfull){
    xes<-c(xes,strsplit(ct, split=" ")[[1]])
  }
  xes<-xes[xes!=""]
  xes<-unlist(sapply(xes, gsub, pattern=".",replacement=" ",fixed=TRUE))
  excepts<-ifelse(wstem=="all", "", wstem)
  xstem<-sapply(xes, function(x) stemexcept(x, excepts, language), USE.NAMES=F)
  xstem<-unlist(sapply(xstem, gsub, pattern=" ",replacement=".",fixed=TRUE))
  vcounts<-list()
  for (v in vocab){
    vcounts[[v]]<-table(xes[xstem==v])
  }
  return(vcounts)
}
############################################################################
