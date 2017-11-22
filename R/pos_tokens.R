library(spacyr)
spacyr::spacy_initialize(python_executable = "/Users/mikeyeomans/anaconda3/bin/python")

################################################################
pos_tokens<-function(texts,
                     wstem="all",
                     ngrams=1,
                     language="english",
                     punct=FALSE,
                     stop.words=TRUE,
                     overlap=1,
                     sparse=0.99,
                     verbose=FALSE){

  ptxt<-read.csv("data/practice_plans.csv",stringsAsFactors = F)

  texts=ptxt$planSPELL[1:100]
  wstem="all"
  ngrams=1
  language="english"
  punct=FALSE
  stop.words=TRUE
  overlap=1
  verbose=FALSE


  texts<-textformat(texts, FALSE)
  texts<-ctxpand(texts)

  names(texts)<-1:length(texts)
  texts[texts==""]<-" "
  parsedtxt <- spacyr::spacy_parse(texts, dependency=T,lemma=T,pos=T,tag=T,entity=T)
  parsedtxt<-parsedtxt[!(parsedtxt$pos%in%c("PUNCT","SPACE","SYM")),]
  ######
  parsedtxt$cleanlemma<-parsedtxt$lemma
  parsedtxt[parsedtxt$cleanlemma=="-PRON-",]$cleanlemma<-parsedtxt[parsedtxt$cleanlemma=="-PRON-",]$token
  parsedtxt$cleanlemma<-tolower(parsedtxt$cleanlemma)
  parsedtxt<-parsedtxt[!(parsedtxt$cleanlemma%in%c(""," ","  ")),]
  ######
  if((length(stop.words)>1)){
    parsedtxt<-parsedtxt[!(parsedtxt$cleanlemma%in%stop.words),]
  }else if(stop.words){
    parsedtxt<-parsedtxt[!(parsedtxt$cleanlemma%in%tm::stopwords(language)),]
  }
  ######



  parsedtxt$clean_pos<-paste0(parsedtxt$cleanlemma,"_",parsedtxt$pos)
  ######
  pos_words<-parallel::mclapply(unique(parsedtxt$doc_id),
                                function(x) unlist(parsedtxt[parsedtxt$doc_id==x,"clean_pos"]),
                                mc.cores= parallel::detectCores())
  dgm<-list()
  for (ng in 1:length(ngrams)){
    dgm[[ng]] <- as.matrix(quanteda::dfm(unlist(lapply(pos_words, ngrammer, ngrams[ng])),tolower=F))
    dgm[[ng]]<-dgm[[ng]][,colSums(dgm[[ng]])>1]
    if ((sparse<1)) dgm[[ng]]<-dgm[[ng]][,colMeans(dgm[[ng]]>0)>=(1-sparse)]
    if (ng==1) dpm<-dgm[[1]]
    if (ng>1) dpm<-overlaps(dpm, dgm[[ng]], overlap)

    if (verbose) print(paste(c(ng, dim(dpm),dim(dgm[[ng]]))))
  }
  dpm<-doublestacker(dpm)
  return(dpm)
}
