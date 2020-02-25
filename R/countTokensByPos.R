
#' Count Tokens by POS
#'
#' @param texts character. A vector of texts.
#' @param ... parameters to be pased to \code{pos_tokens}
#' @return list.
#' @import data.table

countTokensByPos <- function(texts, ... ){ #=> list
  texts<-iconv(textclean::replace_non_ascii(texts),to="ASCII",sub=" ")
  len_text <- length(texts)

  m_pos_token_one_hot <- posTokens(texts, ...)

  dt_token_one_hot <- data.table(m_pos_token_one_hot)

  num_row <- nrow(dt_token_one_hot)

  if(len_text!=num_row){
    warning("pos_tokens has reduced the number of texts this may result in incorrect df_token_pos_text")
  }

  v_s_texts_in_one_hot <- rownames(m_pos_token_one_hot)
  v_i_texts_in_one_hot <- as.integer(gsub("\\D+","",v_s_texts_in_one_hot))
  i_nrow <- nrow(dt_token_one_hot)
  dt_token_one_hot[ , id := v_i_texts_in_one_hot]
  dt_token_one_hot_long <- melt.data.table(dt_token_one_hot, id.vars="id",
                                           variable.name = "token",
                                           variable.factor = FALSE)
  dt_token_one_hot_long <- dt_token_one_hot_long[ value > 0]
  dt_token_one_hot_long[ , orig_token := gsub("_.*","",token) ]
  dt_token_one_hot_long[ , pos := gsub(".*_","",token) ]

  dt_token_count <- dt_token_one_hot_long[ , .(count = .N), by = c("token","orig_token", "pos")]

  dt_token_count[ , num_token_type := .N , by= "orig_token" ]
  dt_token_count <- dt_token_count[ orig_token !=""]

  setorder(dt_token_count, token)

  dt_token_count_single_pos <- dt_token_count[  num_token_type==1]

  dt_token_count_several_pos <- dt_token_count[  num_token_type>1]
  dt_token_count_several_pos[ , total_orig_token_count := sum(count) , by= "orig_token" ]
  dt_token_count_several_pos[ , percent_pos := count/total_orig_token_count *100]


  dt_token_one_hot[ , text := texts[v_i_texts_in_one_hot]]

  l_token_pos <- list(df_token_single_pos = as.data.frame(dt_token_count_single_pos) ,
                      df_token_multiple_pos = as.data.frame(dt_token_count_several_pos) ,
                      df_token_pos_text =  as.data.frame(dt_token_one_hot)

  )
  return(l_token_pos)

}

#' Look up texts by token
#' @param df_token_pos_text
#' @param s_token_pos
#' @return character vector of texts that include \code{s_token_pos}
#'

lookUpTokenPos <- function(df_token_pos_text, s_token_pos){
  v_s_texts_with_token_pos <- df_token_pos_text$text[df_token_pos_text[[s_token_pos]]==1]
  return(v_s_texts_with_token_pos)
}

