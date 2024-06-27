# FUNCTIONS

# Function to remove all html content from the dataframe
remove_html <- function(text) {
  if (is.na(text)) return(NA)
  # Parse the text as HTML
  doc <- read_html(paste0("<div>", text, "</div>"))
  # Extract the text content, which removes the HTML tags
  cleaned_text <- xml_text(xml_find_all(doc, ".//text()"))
  # Join the text nodes and remove non-breaking spaces
  cleaned_text <- paste(cleaned_text, collapse = " ")
  cleaned_text <- gsub("&nbsp;", "", cleaned_text)
  return(cleaned_text)
}

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
      cat(paste0(df_meta[[paste0("Question text ("+TRANSLATED_LANGUAGE_CODE+")")]][rows_of_current_special_rows[i]], "\n\n"))
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
  mistable = df %>% kableExtra::kable("latex", 
                                      col.names = NULL, 
                                      booktabs = T) %>% 
                    kableExtra::kable_styling(latex_options =  c("striped", "hold_position")) %>% 
                    kableExtra::column_spec(2, width = "35em")
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
          escape = TRUE, print_miscodes = FALSE) 
{
  non_NA_original_language_codes = .codes_original_language[!is.na(metadata[num.var, .codes_original_language])]
  non_NA_translated_language_codes = .codes_translated_language[!is.na(metadata[num.var, .codes_translated_language])]
  
  num_rows_original_language = length(non_NA_original_language_codes)
  num_rows_translated_language = length(non_NA_translated_language_codes)
  
  metadata[num.var, ] = metadata[num.var, ] %>% mutate_all(~ ifelse(is.na(.), " ", .)) %>%  # Replace all NA values with empty strings
    mutate(across(all_of(c(non_NA_original_language_codes, non_NA_translated_language_codes)), ~ gsub("\r\n|\n", " ", .)))
  
  # gets the information from row num.var and from the columns in .meta, values and .miscodes
  if (print_miscodes) {
    getinfo = metadata[num.var, sort(c(.meta, non_NA_original_language_codes, non_NA_translated_language_codes, .miscodes))]
  } else {
    getinfo = metadata[num.var, sort(c(.meta, non_NA_original_language_codes, non_NA_translated_language_codes))]
  }
  
  # creates dataframe from getinfo, but only with the first row
  df = data.frame(names = colnames(getinfo), values = as.character(getinfo[1, ]))
  
  
    #mutate(across(c(non_NA_original_language_codes, non_NA_translated_language_codes)), ~ gsub("\n", "", .))
  
  # gets question name (= Variable name)
  name = as.character(metadata[num.var, "Variable name"])
  
  # if there are no language code values (e.g. in a pure info text "question")
  if (num_rows_original_language + num_rows_translated_language == 0) {
    cbtable = df %>%  kableExtra::kable("latex", 
                                       col.names = NULL, # omit column names in of the table
                                       booktabs = TRUE, # use booktabs package
                                       longtable = TRUE, # enables multiple pages 
                                       escape = escape) %>% # controls whether special characters are escaped in the table. (TRUE or FALSE)
                      # Missing Labels on every page if print_miscodes == TRUE
                      {if (print_miscodes) kableExtra::pack_rows(., "Missing Labels", 
                                                 length(.meta) + 1, 
                                                 length(.meta) + length(.miscodes), 
                                                 latex_gap_space = mis.space, 
                                                 bold = F, 
                                                 italic = T) else .} %>% 
                      kableExtra::kable_styling(latex_options = c("striped", "hold_position")) %>% 
                      kableExtra::column_spec(2, width = "35em")
    
    
  }
  # else (= there are code values
  else {
    cbtable = df %>% kableExtra::kable("latex", 
                         col.names = NULL, # omit column names in of the table
                         booktabs = TRUE, # use booktabs package
                         longtable = TRUE, # enables multiple pages
                         escape = escape) %>%  # controls whether special characters are escaped in the table. (TRUE or FALSE)
      # add number of meta variables + title rows
      kableExtra::pack_rows(group_label = "Value Labels",  #The label for the grouped rows
                            start_row = length(.meta) + 1, #The starting row index for this grouping.
                            end_row = length(.meta) + num_rows_original_language, #The ending row index for this grouping.
                            latex_gap_space = lbl.space) %>% # Specifies the vertical space to insert before the group label, controlled by the variable lbl.space
      {if (num_rows_translated_language > 0) kableExtra::pack_rows(., # same for translated value labels, if there are any
                                   group_label = paste0("Value Labels (", 
                                      .languages[2], 
                                      ")"), 
                                   start_row = length(.meta) + num_rows_original_language + 1, 
                                   end_row = length(.meta) + num_rows_original_language + num_rows_translated_language, 
                                   latex_gap_space = lbl.space) else .} %>% 
      # Missing Labels on every page if print_miscodes == TRUE
      {if (print_miscodes) kableExtra::pack_rows(., "Missing Labels", 
                            length(.meta) + num_rows_original_language + num_rows_translated_language + 1, 
                            length(.meta) + num_rows_original_language + num_rows_translated_language + length(.miscodes), 
                            latex_gap_space = mis.space, 
                            bold = F, 
                            italic = T) else .} %>% 
      kableExtra::kable_styling(latex_options = c("striped", "hold_position")) %>% 
      kableExtra::column_spec(2,width = "35em")
  }
  # print the question name as a header
  cat("###", name, sep = " ")
  
  # print the title
  print(cbtable)
}




