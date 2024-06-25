# FUNCTIONS

# this function receives a list indices of rows of special kind
# e.g. rows that contain intros for questions
print_intro_rows <- function(rows_of_current_special_rows){
  if (length(rows_of_current_special_rows) == 0) return()
  
  if (length(rows_of_current_special_rows) == 1) rows_of_current_special_rows = c(rows_of_current_special_rows)
  
  # go through list of special rows and handle them one by one
  for (i in 1:length(rows_of_current_special_rows)) {
    # if the row is an intro row, print the intro
    if (grepl("intro", df_meta[["Variable name"]][rows_of_current_special_rows[i]])) {
      # THIS IS AN INTRO ROW
      cat(paste0(df_meta[[paste0("Question text ("+TRANSLATION_LANGUAGE_CODE+")")]][rows_of_current_special_rows[i]], "\n\n"))
    }
  }
}


# e.g. rows that contain intros for questions
# this is intended for pages that belong to a question but only describe the open text box option of the question
print_text_box_pages <- function(rows_of_current_special_rows){
  if (length(rows_of_current_special_rows) == 0) return()
  
  if (length(rows_of_current_special_rows) == 1) rows_of_current_special_rows = c(rows_of_current_special_rows)
  
  # go through list of special rows and handle them one by one
  for (i in 1:length(rows_of_current_special_rows)) {
    # if the row is an text box row, print the page
    if (grepl("_txt", df_meta[["Variable name"]][rows_of_current_special_rows[i]])) {
      # THIS IS A TEXT BOX QUESTION ROW
      
      cb_pages(metadata = df_meta, multi.var = c(row_of_single_page_question))
    }
  }
}

# assumes row_index exists in df
# n is the number of replicas the new df should contain
replicate_rows <- function(df, row_index, n) {
  row_to_replicate <- df[row_index, , drop = FALSE] # drop = FALSE to keep it as a data frame
  
  # Replicate the row n times
  replicated_rows <- do.call(rbind, replicate(n, row_to_replicate, simplify = FALSE))
  
  # Combine everything and return
  return(df <- rbind(df[1:(row_index-1), ], replicated_rows, df[(row_index+1):nrow(df), ]))
}







cb_mistable <- function (metadata, num.var = 1, .miscodes = miscodes) 
{
  getinfo = metadata[num.var, .miscodes]
  df = data.frame(names = colnames(getinfo), values = as.character(getinfo[1, 
  ]))
  mistable = df %>% kableExtra::kable("latex", col.names = NULL, 
                                      booktabs = T) %>% kableExtra::kable_styling(latex_options = c("striped", 
                                                                                                    "hold_position")) %>% kableExtra::column_spec(2, width = "35em")
  print(mistable)
}




cb_pages <- function (metadata, multi.vars, comment = "", lbl.space = "1em", 
          lblen.space = "1em", mis.space = "1em", escape = TRUE, add_sumplots = FALSE, 
          response = NULL, stats = "") 
{
  for (var in multi.vars) {
    cb_table(metadata = metadata, num.var = var, lbl.space = lbl.space, 
             lblen.space = lblen.space, mis.space = mis.space, 
             escape = escape)
    cat("\n")
    cat(comment, sep = "\n")
    cat("\n")
    cat("\\newpage")
    cat("\n")
    cat("\n")
  }
}


cb_table <- function (metadata, num.var, .meta = meta, .codes_original_language = codes_original_language, 
          .codes_translated_language = codes_translated_language, .miscodes = miscodes, .languages = languages, 
          lbl.space = "1em", lblen.space = "1em", mis.space = "1em", 
          escape = TRUE) 
{
  values = c(.codes_original_language[!is.na(metadata[num.var, .codes_original_language])], 
             .codes_translated_language[!is.na(metadata[num.var, .codes_translated_language])])
  getinfo = metadata[num.var, sort(c(.meta, values, .miscodes))]
  df = data.frame(names = colnames(getinfo), values = as.character(getinfo[1, 
  ]))
  name = as.character(metadata[num.var, "Variable name"])
  if (length(values) == 0) {
    cbtable = df %>% kableExtra::kable("latex", col.names = NULL, 
                                       booktabs = T, longtable = T, escape = escape) %>% 
      #kableExtra::pack_rows("Missing Labels", length(.meta) + 
      #                        1, c(length(.meta) + length(.miscodes)), latex_gap_space = mis.space, 
      #                      bold = F, italic = T) %>% 
      kableExtra::kable_styling(latex_options = c("striped", "hold_position")) %>% kableExtra::column_spec(2, width = "35em")
    cat("###", name, sep = " ")
    print(cbtable)
  }
  else {
    cbtable = df %>% kableExtra::kable("latex", 
                                       col.names = NULL, 
                                       booktabs = T, 
                                       longtable = T, 
                                       escape = escape) %>% 
      kableExtra::pack_rows("Value Labels", 
                            length(.meta) + 1, 
                            c(length(.meta) + c(length(values)/2)), 
                            latex_gap_space = lbl.space) %>% 
      kableExtra::pack_rows(paste0("Value Labels (", 
                                   .languages[2], 
                                   ")"), 
                            c(length(.meta) + c(length(values)/2) + 1), 
                            c(length(.meta) + length(values)), 
                            latex_gap_space = lblen.space) %>% 
      #kableExtra::pack_rows("Missing Labels", 
      #                      c(length(.meta) + length(values) + 1), 
      #                      c(length(.meta) + length(values) + length(.miscodes)), 
      #                      latex_gap_space = mis.space, 
      #                      bold = F, 
      #                      italic = T) %>% 
      kableExtra::kable_styling(latex_options = c("striped", "hold_position")) %>% 
      kableExtra::column_spec(2,width = "35em")
    cat("###", name, sep = " ")
    print(cbtable)
  }
}




