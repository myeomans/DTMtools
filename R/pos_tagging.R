library(spacyr)
spacyr::spacy_initialize(python_executable = "/anaconda/bin/python")

# pt<-read.csv("plan_text.csv",stringsAsFactors = F)
# # pt<-pt[(pt$anyplans==1)&(qdap::word_count(pt$planSPELL,missing=0)>5),]
# texts<-pt$planSPELL[20:30]
# wstem="all"
# ngrams=1:3
# language="english"
# punct=F
# stop.words=TRUE
# overlap=1
# verbose=TRUE
################################################################
pos.tokens<-function(texts,
                     wstem="all",
                     ngrams=1,
                     language="english",
                     punct=F,
                     stop.words=TRUE,
                     overlap=1,
                     verbose=FALSE){
  texts<-textformat(texts, punct)
  texts<-ctxpand(texts)
  texts<-gsub("||",".",texts,fixed=T)
  names(texts)<-1:length(texts)
  parsedtxt <- spacyr::spacy_parse(texts, dependency=T,lemma=T,pos=T,tag=T,entity=T)
  cleantoken<-unlist(sapply(parsedtxt$token, cleantext, language, stop.words, punct, USE.NAMES=F))
  if(length(wstem)>1) cleantoken<-sapply(cleantoken, function(x) stemexcept(x, wstem, language), USE.NAMES=F)
  if(wstem=="all") cleantoken<-sapply(cleantoken, SnowballC::wordStem, language=language, USE.NAMES=F)
  parsedtxt$cleantoken<-cleantoken
  parsedtxt$clean_pos<-paste0(parsedtxt$cleantoken,"_",parsedtxt$tag)
  parsedtxt<-parsedtxt[!(parsedtxt$pos%in%c("PUNCT","SPACE","SYM")),]
  parsedtxt<-parsedtxt[!(parsedtxt$cleantoken%in%c("")),]
  pos_words<-lapply(unique(parsedtxt$doc_id),function(x) unlist(parsedtxt[parsedtxt$doc_id==x,"clean_pos"]))
  dgm<-list()
  for (ng in 1:length(ngrams)){
    dgm[[ng]] <- as.matrix(quanteda::dfm(unlist(lapply(pos_words, ngrammer, ngrams[ng])),tolower=F))
    dgm[[ng]]<-dgm[[ng]][,colSums(dgm[[ng]])>1]
    if (ng==1) dpm<-dgm[[1]]
    if (ng>1){
      if (overlap!=1) dpm<-overlaps(dpm, dgm[[ng]], overlap)
      if (overlap==1) dpm<-Matrix::cBind(dpm, as.matrix(dgm[[ng]]))
    }
    if (verbose) print(paste(c(ng, dim(dpm),dim(dgm[[ng]]))))
  }
  return(dpm)
}
head_token_grab<-function(x, data){
  return(data[(data$doc_id==data[x,]$doc_id)&(data$sentence_id==data[x,]$sentence_id)&(data$token_id==data[x,"head_token_id"]),"token"])
}
