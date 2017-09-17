#pt<-read.csv("plan_text.csv",stringsAsFactors = F)
#texts<-pt$planSPELL
# wstem="all"
# ngrams=2:3
# language="english"
# punct=TRUE
# stop.words=TRUE
# overlap=1
# verbose=FALSE
#
# # cleanertext<-unlist(parallel::mclapply(texts, cleantext, language, stop.words,
# #                                        mc.cores = parallel::detectCores()))
#

#dtm.text<-DTM(texts)


#dtm.text<-DTM(texts,ngrams=2)
#
# p.t<-proc.time()[3]
# high<-DTM(phone_offers$message,ngrams=1:2,overlap=.9)
# dim(high)
# proc.time()[3]-p.t
