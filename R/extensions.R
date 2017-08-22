############################################################################
# Count word lists
############################################################################
textcounter<-function (counted, texts, fixed=T) {
  counts<-rep(0,length(texts))
  for (x in counted){
    counts<-counts+sapply(gregexpr(x, texts, fixed = fixed), function(z) ifelse(z[1] == (-1), 0, length(z)))
  }
  return(counts)
}
############################################################################

############################################################################
# FIT OUT-OF-SAMPLE TEXT TO TRAINING FEATURES
############################################################################
DTMmatch<-function(hole, peg){
  #peg<-doublestacker(peg)
  newpeg<-array(0, c(nrow(peg), ncol(hole)))
  for (i in 1:ncol(newpeg)){
    if(colnames(hole)[i] %in% colnames(peg)){
      newpeg[,i]<-peg[,which(colnames(peg)==colnames(hole)[i])]
    }
  }
  dimnames(newpeg)<-list(rownames(peg), colnames(hole))
  return(as.matrix(newpeg))
}

############################################################################
# Recover Stemming
############################################################################
stemlist<-function(vocab, texts, wstem="all",
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
