# Assumtions:
# - Each question appears as a block of rows. (i.e. questions are not mixed between rows)
# - The Variable name is unique
# - intro rows are before the question to which they belong


# INSERT PAGES
current_row = 1
current_question = 1
current_chapter = 1 # contains row of question where this chapter begins

if ("Subchapter" %in% names(df_meta)) {
  current_sub_chapter = 1 # contains row of question where this chapter begins
  uses_subchapters = TRUE
} else {
  uses_subchapters = FALSE
}

default_comments = "The code scheme for missing values can be found on page \\pageref{missingcodes}."

# iterate through all rows until the end of the metadata dataframe
# prints all pages for each question
while (current_row <= nrow(df_meta)) {
  if (df_meta$Chapter[current_chapter] != df_meta$Chapter[current_row] || current_row == 1) {
    # NEW CHAPTER REACHED
    current_chapter = current_row
    
    # PRINT CHAPTER
    cat(paste0("# ", df_meta$Chapter[current_chapter], "\n\n"))
  }
  
  # if the meta file also has subchapters
  if (uses_subchapters) {
    if (df_meta$Subchapter[current_sub_chapter] != df_meta$Subchapter[current_row] || current_row == 1) {
      # NEW SUB CHAPTER REACHED
      current_sub_chapter = current_row
      
      # PRINT CHAPTER
      cat(paste0("## ", df_meta$Subchapter[current_sub_chapter], "\n\n"))
    }
  }
  
  # get question name identifier (e.g. "w12_q3x1")
  row_identifier = df_meta[["Variable name"]][current_row]
  
  # get question number (e.g. "3")
  question_number = str_extract(gsub(paste0(WAVE_PREFIX, "q"), "", row_identifier), "^[0-9]+")
  
  # if subchapters are used, the title hierarchies should be moved down by adding one # before the title
  if (uses_subchapters) {
    cat("#")
  }
  
  # HANDLE SPECIAL CASES (where there is no question number)
  if (is.na(question_number) || TRUE) {
    if (is.na(df_meta[current_row, ]$`Title`)
        || 
        df_meta[current_row, ]$`Title` == " ") {
      cat(paste0("## ", df_meta[current_row, ]$`Variable name`,"\n\n"))
    } else {
      cat(paste0("## ", df_meta[current_row, ]$`Title`,"\n\n"))
    }
    
    # if subchapters are used, the title hierarchies should be moved down by adding one # before the question as well
    if (uses_subchapters) {
      cat("#")
    }
    cb_pages(metadata = df_meta, multi.var = c(current_row), comment = paste0(if (!is.na(df_meta[current_row, ]$`Comment`)) df_meta[current_row, ]$`Comment` else "","\n",default_comments))
    
    # go to next question
    current_row = current_row + 1
    next
  } 
  
  # HANDLE REGULAR CASES (NORMAL QUESTION )
  else  {
    
    if (current_question != question_number || current_row == 1) {
      # PRINT TITLE
      # if subchapters are used, the title hierarchies should be moved down by adding one # before the title
      if (uses_subchapters) {
        cat("#")
      }
      # print the title for this page or these pages if there are multiple subquestions
      cat(paste0("## Question ", question_number,": ",df_meta[current_row, ]$`Title`,"\n\n"))
      
      current_question = question_number
    }
    
    # if it's just an intro text, just print the german and english version
    if (any(grepl(paste0("^.+_intro$"), row_identifier))) {
      cat("DE: ")
      cat(df_meta[current_row, ]$`Question text`)
      cat("\n\n")
      cat("EN: ")
      cat(df_meta[current_row, ]$`Question text (EN)`)
      cat("\n\n")
      
      # go to next question
      current_row = current_row + 1
      next
    }
    
    # check if current row is a regular question that can just be printed
    if ( 
      any(grepl(paste0("^", WAVE_PREFIX, "q", question_number, "(_txt)?$"), row_identifier)) # single page question (possibly with text box)
      ||
      any(grepl(paste0("^", WAVE_PREFIX, "q", question_number, "x", "[0-9]+(_txt)?$"), row_identifier)) # multi page question (possibly with text box)
    ) {
      
      if (any(grepl(paste0("^", WAVE_PREFIX, "q", question_number, "$"), row_identifier))
          &&
          !is.na(df_meta[current_row, ]$`Question type`)
          &&
          df_meta[current_row, ]$`Question type` == "Multiple Choice") {
        # single page question is MULTIPLE CHOICE
        # --> this question has to be expanded
        
        # get number of choices including -8
        n = length(c(codes_de[!is.na(df_meta[current_row, codes_de])]))
        
        df_meta <- replicate_rows(df = df_meta, row_index = current_row, n = n)
        
        # all without -8
        for (i in 0:(n-2)) {
          df_meta[current_row+i,]$`Item text` = df_meta[[current_row+i,codes_de[2]+i]] # value for 1 to last posittive german labels
          df_meta[current_row+i,]$`Item text (EN)` = df_meta[[current_row+i,codes_en[2]+i]] # value for 1 to last posittive english labels
        }
        df_meta[current_row+n-1,]$`Item text` = df_meta[[current_row+i,codes_de[length(codes_de)-1]]] # value for -8
        df_meta[current_row+n-1,]$`Item text (EN)` = df_meta[[current_row+i,codes_en[length(codes_en)-1]]] # value for -8
        
        for (i in 0:(n-1)) {
          for (j in codes_de) {
            df_meta[current_row+i,j] = NA
          }
          for (j in codes_en) {
            df_meta[current_row+i,j] = NA
          }
          df_meta[current_row+i,codes_de[1]] = "Micht Markiert" # value for 0
          df_meta[current_row+i,codes_de[2]] = "Markiert" # value for 0
          df_meta[current_row+i,codes_en[1]] = "Not Selected" # value for 0
          df_meta[current_row+i,codes_en[2]] = "Selected" # value for 0
          
          df_meta[current_row+i,]$`Variable name` = paste0(df_meta[current_row+i,]$`Variable name`, "x", (i+1))
          df_meta[current_row+i,]$`Question type` = "Single Choice"
        }
      }
      
      
      cb_pages(metadata = df_meta, multi.var = c(current_row), comment = paste0(if (!is.na(df_meta[current_row, ]$`Comment`)) df_meta[current_row, ]$`Comment` else "","\n",default_comments))
    }
    
    # go to next question
    current_row = current_row + 1
    next
  }
  
  
}
