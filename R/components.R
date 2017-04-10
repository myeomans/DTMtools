############################################################################
# Underlying functions for DTM
############################################################################
cleantext<-function(ex, language="english", stop.words=TRUE){
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
  if(length(stop.words)>1){
    ex<-tm::removeWords(ex, stop.words)
  }else if(stop.words){
    ex<-tm::removeWords(ex, tm::stopwords(language))
  }
  #DELETES NUMBERS
  ex<-tm::removeNumbers(ex)
  ex<-tm::stripWhitespace(ex)
  return(as.character(ex))
}
############################################################################
ctxpand<-function(CTX2){
  CTX2<-sapply(CTX2, function(x) gsub(" ?(f|ht)tp(s?)://(.*)[.][a-z]+", "", x))
  CTX2<-gsub("ha ha"," haha ",CTX2,fixed=T)
  CTX2<-gsub("lol"," haha ",CTX2,fixed=T)
  CTX2<-gsub("LOL"," haha ",CTX2,fixed=T)
  CTX2<-gsub("LOl"," haha ",CTX2,fixed=T)
  CTX2<-gsub("Lol"," haha ",CTX2,fixed=T)
  for (x in 1:5){
    CTX2<-gsub(".?","?",CTX2,fixed=T)
    CTX2<-gsub("?.","?",CTX2,fixed=T)
    CTX2<-gsub("!?","?",CTX2,fixed=T)
    CTX2<-gsub("?!","?",CTX2,fixed=T)
    CTX2<-gsub("??","?",CTX2,fixed=T)
    CTX2<-gsub("!!","!",CTX2,fixed=T)
  }
  CTX2<-gsub("!"," xmark.",CTX2,fixed=T)
  CTX2<-gsub("?"," qmark.",CTX2,fixed=T)
  ################################################################
  CTX2<-sapply(CTX2, function(x) gsub("”", "\"", x))
  CTX2<-sapply(CTX2, function(x) gsub("“", "\"", x))
  CTX2<-sapply(CTX2, function(x) gsub("’", "\'", x))
  ################################################################
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
  CTX2<-sapply(CTX2, function(x) gsub("u.s.", "us", x))
  CTX2<-sapply(CTX2, function(x) gsub("e.g.", "eg", x))
  CTX2<-sapply(CTX2, function(x) gsub("i.e.", "ie", x))
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
      return(apply(twowords, 1, function(x) paste0(x, collapse="_")))
    }
    if (ngram==3){
      threewords<-cbind(onewords[1:(len-2)], onewords[2:(len-1)], onewords[3:len])
      return(apply(threewords, 1, function(x) paste0(x, collapse="_")))
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
  peaks<-apply(high, 2, function(x) max(apply(low, 2, function(y) cosdist(x, y))))
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

# filters out group-specific words
# based on concentration (i.e. % of occurrences that fall in most common group)
group.max.conc<-function(texts,groups, cutoff=0.8){
  group.id<-unique(groups)
  cts<-array(NA, c(ncol(texts),length(group.id)))
  for(g in 1:length(group.id)){
    cts[,g]<-colSums(texts[groups==group.id[g],])
  }
  max.conc<-apply(cts,1,max)/rowSums(cts)
  return(texts[,max.conc<cutoff])
  #return(list(conc=max.conc,
  #            cts=cts))
}
############################################################################
