# Used to avoid incorrect notes of "no visible binding"
utils::globalVariables(c("dep_rel","head_token",
                         "head_token_id","pos.nums",
                         "parses",
                         "doc_id","sentence_id",
                         "token_id","token", "."))


#' Dependency Tagger
#' @description Return dependency tags from natural language.
#' @param txt a character vector of texts.
#' @return list of dependency-tagged items.
#' @import data.table
tagDependency <-function(dt_parsedtxt){

  dt_head_token <- dt_parsedtxt[ , .(doc_id, sentence_id, token_id,token)]
  setnames(dt_head_token, c("token_id","token"), c("head_token_id","head_token"))
  v_s_keys <-  c("doc_id", "sentence_id", "head_token_id" )
  setkeyv(dt_head_token, v_s_keys)
  setkeyv(dt_parsedtxt, v_s_keys)
  dt_parsedtxt <- dt_head_token[dt_parsedtxt] # left merge on dt_parsedtxt
  dt_parsedtxt[ , parses := ifelse(is.na(head_token)|(dep_rel=="ROOT"), NA_character_,
                                   paste0(toupper(dep_rel), "_",tolower(head_token),"_",cleanlemma))]

  return(dt_parsedtxt)
}

