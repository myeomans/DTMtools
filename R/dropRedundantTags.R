dropRedundantTags <- function(col_names){
  v_s_allowed_pos <- c("NOUN", "VERB" , "ADV" , "DET" ,"ADJ", "PROPN" , "NUM", "ADP", "CCONJ"  )
  dt_col_names <- data.table(orig_name = col_names)
  dt_col_names[ , token := gsub("_[A-Z]+$","",orig_name)]
  dt_col_names[ , pos := gsub("^.*_","",orig_name)]
  dt_col_names[ ! pos %in% v_s_allowed_pos, pos:= ""]

  dt_token_pos <- unique(dt_col_names[ , .(token,pos)])
  dt_count_lemma_by_pos <- dt_token_pos[ , count := length(pos), by=.(token)]
  multi_pos_lemmas <- dt_count_lemma_by_pos[count>1 , unique(token)]

  dt_col_names[ , final_name := ifelse(token %in% multi_pos_lemmas,
                                       orig_name,
                                       token)]
  return(dt_col_names$final_name)
}
