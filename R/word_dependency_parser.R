# Used to avoid incorrect notes of "no visible binding"
utils::globalVariables(c("l_parses","parses",
                         "l_pos_nums","pos.nums",
                         "l_w_nums","w.nums",
                         "doc_id","sentence_id",
                         "token_id","token", "."))


################################################################
# Workflow for SpaCy
################################################################
################################################################

#' Word Dependency Parser
#' @description Return POS tags from natural language.
#' @param txt a character vector of texts.
#' @param num_mc_cores integer Number of cores for parallelization. Default is parallel::detectCores().
#' @return list of compiled POS-tagged items.
#' @import data.table
word_dependency_parser <-function(parsedtxt, num_mc_cores=parallel::detectCores()){
  # parsedtxt <- spacyr::spacy_parse(txt, dependency=TRUE,lemma=FALSE,pos=TRUE,tag=TRUE,entity=TRUE)
  parsedtxt$pos.nums<-paste0("(",parsedtxt$token_id,"-",parsedtxt$token,"-",parsedtxt$tag,")")
  parsedtxt$head_token<-parallel::mclapply(1:nrow(parsedtxt),headTokenGrab, data=parsedtxt, mc.cores=num_mc_cores)
  parsedtxt[parsedtxt$dep_rel=="ROOT",c("dep_rel","head_token","head_token_id")]<-c("root","ROOT",0)
  parsedtxt$pos.nums<-paste0("(",parsedtxt$token_id,"-",parsedtxt$token,"-",parsedtxt$tag,")")
  parsedtxt$parses<-paste0(toupper(parsedtxt$dep_rel), "_",parsedtxt$head_token,"_",parsedtxt$head_token_id,"_",parsedtxt$token,"_",parsedtxt$token_id)
  parsedtxt$w.nums<-paste0(parsedtxt$token,"-",parsedtxt$token_id)

  dt_parsedtxt <- data.table::data.table(parsedtxt)
  all.parses <- dt_parsedtxt[ , .(l_parses = list(parses)), by = "doc_id"][ , l_parses]
  all.pos.nums <- dt_parsedtxt[ , .(l_pos_nums = list(pos.nums)), by = "doc_id"][ , l_pos_nums]
  nonums=parallel::mclapply(all.parses,gsub, pattern="_[0-9][0-9][0-9]",replacement="", mc.cores=num_mc_cores)
  nonums=parallel::mclapply(nonums,gsub, pattern="_[0-9][0-9]",replacement="", mc.cores=num_mc_cores)
  nonums=parallel::mclapply(nonums,gsub, pattern="_[0-9]",replacement="", mc.cores=num_mc_cores)
  nonums=parallel::mclapply(nonums, function(v_s_parse) v_s_parse[!grepl("_character\\(0\\)_",v_s_parse)], mc.cores=num_mc_cores)
  w.nums <- dt_parsedtxt[ , .(l_w_nums = list(w.nums)), by = "doc_id"][ , l_w_nums]
  return(list(parses=all.parses,
              pos.nums=all.pos.nums,
              nonums=nonums,
              w.nums=w.nums))
}

#' Head Token Grab
#' @description Compile dependency relation for each row of a SpaCy parse table
#' @param x row in SpaCy parse table
#' @param data data.frame SpaCy parse table
#' @return word tagged as referent from dependencey
#' @keywords internal
#'
headTokenGrab <- function(x, data){
  data <- data.table(data)
  doc_id_local <- data$doc_id[x]
  sentence_id_local <- data$sentence_id[x]
  head_token_id_local <- data$head_token_id[x]
  data <- data[(doc_id==doc_id_local)&
                 (sentence_id==sentence_id_local)&
                 (token_id==head_token_id_local), ]
  return(data[,token])
}
