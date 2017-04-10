############################################################################
# Underlying functions for DTM
############################################################################
cleantext<-function(ex, language="english", stop.words=TRUE, punct=F){
  #PUTS ALL LETTERS IN LOWER CASE
  ex<-tolower(ex)
  ex<-textformat(ex, punct)
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
textformat<-function(text, punct=F){
text<-sapply(text, function(x) gsub(" ?(f|ht)tp(s?)://(.*)[.][a-z]+", "", x))
text<-sapply(text, function(x) gsub("www.(.*)[.][a-z]+", "", x))
text<-gsub("ha ha"," haha ",text,fixed=T)
text<-gsub("lol"," haha ",text,fixed=T)
text<-gsub("LOL"," haha ",text,fixed=T)
text<-gsub("LOl"," haha ",text,fixed=T)
text<-gsub("Lol"," haha ",text,fixed=T)
for (x in 1:5){
  text<-gsub(".?","?",text,fixed=T)
  text<-gsub("?.","?",text,fixed=T)
  text<-gsub("!?","?",text,fixed=T)
  text<-gsub("?!","?",text,fixed=T)
  text<-gsub("??","?",text,fixed=T)
  text<-gsub("!!","!",text,fixed=T)
}
text<-gsub("!"," xmark.",text,fixed=T)
text<-gsub("?"," qmark.",text,fixed=T)
text<-sapply(text, function(x) gsub("”", "\"", x))
text<-sapply(text, function(x) gsub("“", "\"", x))
text<-sapply(text, function(x) gsub("’", "\'", x))
}
ctxpand<-function(text){
  text<-sapply(text, function(x) gsub("let's", "let us", x))
  text<-sapply(text, function(x) gsub("i'm", "i am", x))
  text<-sapply(text, function(x) gsub("won't", "will not", x))
  text<-sapply(text, function(x) gsub("can't", "cannot", x))
  text<-sapply(text, function(x) gsub("shan't", "shall not", x))
  text<-sapply(text, function(x) gsub("'d", " would", x))
  text<-sapply(text, function(x) gsub("'ve", " have", x))
  text<-sapply(text, function(x) gsub("'s", " is", x))
  text<-sapply(text, function(x) gsub("'ll", " will", x))
  text<-sapply(text, function(x) gsub("'re", " are", x))
  text<-sapply(text, function(x) gsub("n't", " not", x))
  text<-sapply(text, function(x) gsub("u.s.", "us", x))
  text<-sapply(text, function(x) gsub("e.g.", "eg", x))
  text<-sapply(text, function(x) gsub("i.e.", "ie", x))
  return(text)}
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
doublestacker<-function (wdcts){
  wdcts<-as.matrix(wdcts)
  words<- colnames(wdcts)
  for (Q in words[duplicated(words)]) {
    wdcts[, (words== Q) & (!duplicated(words))] <- as.numeric(rowSums(wdcts[,(words== Q)]))
    wdcts[, ((words== Q) & (duplicated(words)))] <- NA
  }
  return(wdcts[, !is.na(colMeans(wdcts))])
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
