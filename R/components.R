############################################################################
# Underlying functions for DTM
############################################################################
cleantext<-function(text, language="english", stop.words=TRUE, punct=FALSE, nums=TRUE, pos_tag=FALSE){
  #PUTS ALL LETTERS IN LOWER CASE
  if(!pos_tag) text<-tolower(text)
  text<-textformat(text, punct)
  #EXPANDS CONTRACTIONS
  if(language=="english"){
    text<-ctxpand(text)
  }
  #DELETES PUNCTUATION & HTML JUNK
  if(!pos_tag) text<-gsub("[[:punct:]]", " ", text)
  #DELETES STOP WORDS
  if((length(stop.words)>1)&(!pos_tag)){
    text<-tm::removeWords(text, stop.words)
  }else if((stop.words)&(!pos_tag)){
    text<-tm::removeWords(text, tm::stopwords(language))
  }
  #DELETES NUMBERS
  if(nums) text<-tm::removeNumbers(text)
  text<-tm::stripWhitespace(text)
  return(as.character(text))
}
############################################################################
textformat<-function(text, punct=FALSE){
  text <- gsub(" ?(f|ht)tp(s?)://(.*)[.][a-z]+", "", text)
  text <- gsub("www.(.*)[.][a-z]+", "", text)
  text <- gsub("\u201D", "\"", text)
  text <- gsub("\u201C", "\"", text)
  text <- gsub("\u2019", "\'", text)

  text<-gsub("ha ha"," haha ",text,fixed=T)
  text<-gsub("lol "," haha ",text,fixed=T)
  text<-gsub("lol."," haha.",text,fixed=T)
  text<-gsub("lol!"," haha!",text,fixed=T)
  text<-gsub("Lol "," haha ",text,fixed=T)
  text<-gsub("Lol."," haha.",text,fixed=T)
  text<-gsub("Lol!"," haha!",text,fixed=T)
  text<-gsub("LOL"," haha ",text,fixed=T)
  text<-gsub("LOl"," haha ",text,fixed=T)
  text<-gsub("LOl"," haha ",text,fixed=T)
  text<-gsub("LoL"," haha ",text,fixed=T)
  text<-gsub("ROFL"," haha ",text,fixed=T)
  text<-gsub("rofl"," haha ",text,fixed=T)
  for (x in 1:8){
    text<-gsub(".?","?",text,fixed=T)
    text<-gsub("?.","?",text,fixed=T)
    text<-gsub("!?","?",text,fixed=T)
    text<-gsub("?!","?",text,fixed=T)
    text<-gsub("??","?",text,fixed=T)
    text<-gsub("!!","!",text,fixed=T)
  }
  if(punct){
    text<-gsub("!"," xmark.",text,fixed=T)
    text<-gsub("?"," qmark.",text,fixed=T)
  }
  text<-gsub("||",". ",text,fixed=T)
  text<-gsub("|",". ",text,fixed=T)
  text<-gsub("[[:cntrl:]]", " ", text)
  return(text)
}

ctxpand<-function(text){
  text <- gsub("let's", "let us", text, fixed=T)
  text <- gsub("i'm", "i am", text, fixed=T)
  text <- gsub("won't", "will not", text, fixed=T)
  text <- gsub("can't", "cannot", text, fixed=T)
  text <- gsub("Let's", "Let us", text, fixed=T)
  text <- gsub("I'm", "I am", text, fixed=T)
  text <- gsub("Won't", "Will not", text, fixed=T)
  text <- gsub("Can't", "Cannot", text, fixed=T)
  text <- gsub("shan't", "shall not", text, fixed=T)
  text <- gsub("'d", " would", text, fixed=T)
  text <- gsub("'ve", " have", text, fixed=T)
  text <- gsub("'s", " is", text, fixed=T)
  text <- gsub("'ll", " will", text, fixed=T)
  text <- gsub("'re", " are", text, fixed=T)
  text <- gsub("n't", " not", text, fixed=T)
  text <- gsub("u.s.", "US", text, fixed=T)
  text <- gsub("U.S.", "US", text, fixed=T)
  text <- gsub("e.g.", "eg", text, fixed=T)
  text <- gsub("i.e.", "ie", text, fixed=T)

  return(text)
}
############################################################################
gramstem<-function(text, wstem="all", ngrams=1, language="english"){
  if(nchar(text)%in%c(NA,NULL,0:2)){
    return(text)
  }else{
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
    if (ngram==1){
      words<-onewords
    }
    if (ngram==2){
      twowords<-cbind(onewords[1:(len-1)], onewords[2:len])
      words<-apply(twowords, 1, function(x) paste0(x, collapse="_"))
    }
    if (ngram==3){
      threewords<-cbind(onewords[1:(len-2)], onewords[2:(len-1)], onewords[3:len])
      words<-apply(threewords, 1, function(x) paste0(x, collapse="_"))
    }
    return(paste(words, collapse=" "))
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
overlaps<-function(high, low, cutoff=.9){
  if(cutoff==1){
    combined<-cbind(as.matrix(high),as.matrix(low))
  } else {
    high<-as.matrix(high)
    low_l<-data.frame(lapply(colnames(low),function(x) as.vector(low[,x])))
    colnames(low_l)<-colnames(low)
    #low_max <- unlist(parallel::mclapply(low_l,function(x) max(unlist(apply(high, 2, function(y) cor(x,y)))), mc.cores=parallel::detectCores()))
    #combined<-cbind(high,low[,low_max<cutoff])

    #tmp <- cor(as.matrix(combined))
    #tmp[!upper.tri(tmp)] <- 0
    #combined <- combined[,apply(tmp,2,function(x) all(abs(x) < cutoff))]

    # Should do PMI at some point...

    peaks<-apply(low_l, 2, function(x) max(apply(high, 2, function(y) cosdist(x, y))))
    remaining<-low_l[,peaks<=cutoff]
    combined<-Matrix::cBind(remaining,high)
  }
  return(combined)
}

cosdist<-function(x,y) return(x %*% y / sqrt(x%*%x * y%*%y))
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
group.max.conc<-function(dtm,groups, cutoff=0.8){
  group.id<-unique(groups)
  cts<-array(NA, c(ncol(dtm),length(group.id)))
  for(g in 1:length(group.id)){
    cts[,g]<-colSums(dtm[groups==group.id[g],])
  }
  max.conc<-apply(cts,1,max)/rowSums(cts)
  return(dtm[,max.conc<cutoff])
  #return(list(conc=max.conc,
  #            cts=cts))
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
