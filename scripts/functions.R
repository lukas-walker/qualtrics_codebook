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
      cat(paste0(df_meta[["Question text (EN)"]][rows_of_current_special_rows[i]], "\n\n"))
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
      
      cb_page(metadata = df_meta, num.var = row_of_single_page_question)
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
