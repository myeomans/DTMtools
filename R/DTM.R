#
# require(parallel)
# require(tm)
# require(qdap)
# require(Matrix)
# require(SnowballC)
############################################################################
# THE BIG WRAPPER FOR SUBCOMPONENTS
############################################################################
DTM<-function(exps, sparse=0.99, wstem="all",
              ngrams=1, overlap=1,
              vocabmatch=NULL,
              language="english",
              stopwords=TRUE,
              verbose=FALSE){
  cleanertext<-unlist(sapply(exps, cleantext, language, stopwords))
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
  return(DSM)
  #######################################################
}
############################################################################
############################################################################
#DTM COMPONENTS
############################################################################
############################################################################
cleantext<-function(ex, language="english", stopwords=TRUE){
  #PUTS ALL LETTERS IN LOWER CASE
  ex<-tolower(ex)
  #EXPANDS CONTRACTIONS
  if(language=="english"){
    ex<-ctxpand(ex)
  }
  #DELETES PUNCTUATION & HTML JUNK
  ex<-gsub("[[:punct:]]", " ", ex)
  ex<-gsub("[[:cntrl:]]", " ", ex)
  #DELETES STOP WORDS
  if(stopwords){
    ex<-tm::removeWords(ex, tm::stopwords(language))
  }
  #DELETES NUMBERS
  ex<-tm::removeNumbers(ex)
  ex<-tm::stripWhitespace(ex)
  return(as.character(ex))
}
############################################################################
ctxpand<-function(CTX2){
  CTX2<-sapply(CTX2, function(x) gsub("let's", "let us", x))
  CTX2<-sapply(CTX2, function(x) gsub("i'm", "i am", x))
  CTX2<-sapply(CTX2, function(x) gsub("won't", "will not", x))
  CTX2<-sapply(CTX2, function(x) gsub("can't", "cannot", x))
  CTX2<-sapply(CTX2, function(x) gsub("shan't", "shall not", x))
  CTX2<-sapply(CTX2, function(x) gsub("'d", " would", x))
  CTX2<-sapply(CTX2, function(x) gsub("'ve", " have", x))
  CTX2<-sapply(CTX2, function(x) gsub("'s", " is", x))
  CTX2<-sapply(CTX2, function(x) gsub("'ll", " will", x))
  CTX2<-sapply(CTX2, function(x) gsub("'re", " are", x))
  CTX2<-sapply(CTX2, function(x) gsub("n't", " not", x))
  return(CTX2)}
############################################################################
gramstem<-function(text, wstem="all", ngrams=1, language="english"){
  if(is.na(qdap::word_count(text,missing=0))|qdap::word_count(text,missing=0)<min(ngrams))return(" ") else{
    xes<-(strsplit(text, split=" ")[[1]])
    xes<-xes[which(nchar(xes)>0)]
    if(length(wstem)>1) xes<-sapply(xes, function(x) stemexcept(x, wstem, language), USE.NAMES=F)
    if(wstem=="all") xes<-sapply(xes, SnowballC::wordStem, language=language, USE.NAMES=F)
    xret<-" "
    if (1 %in% ngrams) xret<-paste(c(xret, xes), collapse=" ")
    if (2 %in% ngrams) xret<-paste(c(xret, ngrammer(xes, 2)), collapse=" ")
    if (3 %in% ngrams) xret<-paste(c(xret, ngrammer(xes, 3)), collapse=" ")
    return(xret)
  }
}
############################################################################
ngrammer <- function (onewords, ngram){
  len<-length(onewords)
  if(len<ngram){
    return(" ")
  }else{
  if (ngram==2){
    twowords<-cbind(onewords[1:(len-1)], onewords[2:len])
    return(apply(twowords, 1, function(x) paste0(x, collapse=".")))
  }
  if (ngram==3){
    threewords<-cbind(onewords[1:(len-2)], onewords[2:(len-1)], onewords[3:len])
    return(apply(threewords, 1, function(x) paste0(x, collapse=".")))
  }
  }
}
############################################################################
stemexcept<-function(sentence, excepts, language="english"){
  words<-strsplit(sentence, split=" ")[[1]]
  SS<-which(!(words %in% excepts))
  words[SS]<-SnowballC::wordStem(words[SS], language)
  return(paste(words, collapse=" "))
}
############################################################################
overlaps<-function(high, low, cutoff=.8){
  #if(sum(colnames(low) %in% colnames(high))>0){
  # for(CN in colnames(low)[colnames(low) %in% colnames(high)]){
  # low[,CN]<-low[,CN]+high[,CN]
  #high[,CN]<-high[,-CN]
  #}
  #}
  peaks<-apply(high, 2, function(x) max(apply(low, 2,
                                              function(y) cosdist(x, y))))
  remaining<-high[,peaks<=cutoff]
  return(Matrix::cBind(remaining,low))
}
############################################################################
doublestacker<-function (WDCTX){
  WDCTX<-as.matrix(WDCTX)
  WORDS <- colnames(WDCTX)
  for (Q in WORDS[duplicated(WORDS)]) {
    WDCTX[, (WORDS == Q) & (!duplicated(WORDS))] <- as.numeric(rowSums(WDCTX[,(WORDS == Q)]))
    WDCTX[, ((WORDS == Q) & (duplicated(WORDS)))] <- NA
  }
  return(WDCTX[, !is.na(colMeans(WDCTX))])
}

############################################################################
#EXTENSION FOR OUT-OF-SAMPLE
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
                   ngrams=1,language="english", stopwords=TRUE){
  vocab=colnames(TEXT)
  texts<-SLIM$cleantext3[1:200]
  ngrams<-1:3
  language="spanish"
  stopwords=TRUE
  wstem="all"

  if(mean(ngrams==1)!=1){
    cleanertext<-unlist(sapply(texts, cleantext, language, stopwords))
    xfull<-unlist(sapply(cleanertext, gramstem, "none",ngrams, "spanish"))
  }else{
    xfull<-unlist(sapply(texts, cleantext, language, stopwords))
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
