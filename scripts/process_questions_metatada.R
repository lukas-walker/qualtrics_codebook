
# ------------------------------------------------------------------------------
# SORT QUESTIONS ACCORDING TO SURVEY
# ------------------------------------------------------------------------------

# Contains the ordered structure of the survey (blocks > questions)
blocks = cont$result$Blocks

# Sort blocks according to survey flow

# Recursive function that goes through the flow and finds all Block IDs in the right order
collect_block_ids <- function(obj_list) {
  result <- c()  # Initialize an empty vector to store IDs
  
  for (obj in obj_list) {
    # if we find a block, we return the ID and stop
    if ("Type" %in% names(obj) && (obj$Type == "Block" || obj$Type == "Standard")) {
      result <- c(result, obj$ID)
    }
    
    # if we find something else than a block (either doesn't have a Type or isn't a Block)
    # then we call this function recursively on that item
    if ("Flow" %in% names(obj)) {
      result <- c(result, collect_block_ids(obj$Flow))
    }
  }
  
  return(result)
}

# call the function to collect all block ids recursively in order
sorted_block_ids <- collect_block_ids(cont$result$SurveyFlow$Flow)

# get the current (unsorted) list of ids from block
unsorted_block_ids = sapply(blocks, function(x) x$ID)

# Get the order of the IDs according to the sorted list
order_indices = match(unsorted_block_ids, sorted_block_ids)

# Sort the objects based on this order
blocks = blocks[order(order_indices)]

# Initialize an empty list to store questions in order
ordered_question_ids = list()

# Loop through blocks to extract question IDs in order
for (block in blocks) {
  if (block$Type == "Trash") next
  # Loop through block elements to find question IDs
  for (element in block$BlockElements) {
    if (element$Type == "Question") {
      ordered_question_ids = append(ordered_question_ids, element$QuestionID)
    }
  }
}

# get the ids from the unordered questions
unordered_question_ids <- sapply(questions, function(x) x$QuestionID)

# Get the order indices
order_indices <- match(ordered_question_ids, unordered_question_ids)

questions = questions[order_indices]



# ------------------------------------------------------------------------------
# FILTER QUESTIONS
# ------------------------------------------------------------------------------



# Remove all Timing questions
questions <- Filter(function(obj) {
  is.list(obj) && "QuestionType" %in% names(obj) && !grepl(paste0("^", "Timing"), obj$QuestionType)
}, questions)

# Remove all DB questions
questions <- Filter(function(obj) {
  is.list(obj) && "QuestionType" %in% names(obj) && !grepl(paste0("^", "DB"), obj$QuestionType)
}, questions)

# Remove all questions without the prefix, if a prefix is used
if (USING_QUESTION_PREFIX) {
  questions <- Filter(function(obj) {
    is.list(obj) && "DataExportTag" %in% names(obj) && grepl(paste0("^", QUESTION_PREFIX), obj$DataExportTag)
  }, questions)
}




# ------------------------------------------------------------------------------
# FIND ALL POSSIBLE CHOICE CODES IN ALL QUESTIONS (e.g. 0, 1, 2, ..., 100, 101, ..., -8, -9)
# ------------------------------------------------------------------------------

# These will then become columns each, for all languages each
# 0 and 1 should always be included for binary questions
codes = list(0, 1)

for (question in questions) {
  # Go through recoded values if the question hase recoded values
  if ("RecodeValues" %in% names(question)) {
    for (recodeValue in question$RecodeValues) {
      # add the recoded value to the list of value codes if it's not in there yet
      if (!(as.integer(recodeValue) %in% unlist(codes))) {
        codes = append(codes, as.integer(recodeValue))
      }
    }
  }
  # else go through the default choices 
  else {
    for (choiceCode in names(question$Choices)) {
      # add the value to the list of value codes if it's not in there yet
      if (!(as.integer(choiceCode) %in% unlist(codes))) {
        codes = append(codes, as.integer(choiceCode))
      }
    }
  }
}

# make a vector so it can be sorted
codes = unlist(codes)

# Sort the elements so it's best readable
# First: All non-negative elements in ascending order
# Then: All negative elements in descending order
non_negative_elements <- codes[codes >= 0]
negative_elements <- codes[codes < 0]

# Sort non-negative elements in ascending order
sorted_non_negative <- sort(non_negative_elements)

# Sort negative elements in descending order
sorted_negative <- sort(negative_elements, decreasing = TRUE)

# Combine the sorted non-negative and negative elements
codes <- c(sorted_non_negative, sorted_negative)


# ------------------------------------------------------------------------------
# CREATE METADATA DATAFRAME WITH ALL ITS COLUMNS
# ------------------------------------------------------------------------------

# Add the Language code to the translated columns
translated_columns_expanded = list()

# Iterate over each element in the input list
for (element in translated_columns) {
  # Append the original element
  translated_columns_expanded = append(translated_columns_expanded, element)
  
  # Create the modified copy with the language code suffix and append it
  translated_element = paste0(element, " (", TRANSLATION_LANGUAGE_CODE, ")")
  translated_columns_expanded = append(translated_columns_expanded, translated_element)
}




# translate the code columns, i.e. add the Language code to the translated columns
codes_translation = list()

# Iterate over each element in the input list
for (element in codes) {
  # Append the original element
  codes_translation = append(codes_translation, paste0(element, " (", TRANSLATION_LANGUAGE_CODE, ")"))
}


all_columns = c(default_columns, "hasDisplayLogic", translated_columns_expanded, default_columns_not_in_table, codes, codes_translation, NON_RESPONSE_COLUMNS)

df <- data.frame(matrix(ncol = length(all_columns), nrow = 0), check.names = FALSE)

# Set the column names
colnames(df) <- all_columns

# make sure all entries are treated as text for the excel
df[] <- lapply(df, as.character)


# ------------------------------------------------------------------------------
# FILL THE DATAFRAME FOR EACH QUESTION
# ------------------------------------------------------------------------------


for (question in questions) {
  question_is_translated = ("Language" %in% names(question) && TRANSLATION_LANGUAGE_CODE %in% names(question$Language))
  
  # Handle questions that are not matrices and not MultipleChoice with multiple selection
  # (Matrix question have a special structure in the API response)
  # (MultipleChoice with multiple selection will be split into binary answers)
  if (question$QuestionType != "Matrix" && (question$QuestionType != "MC" || question$Selector != "MAVR") && question$QuestionType != "RO"){
    # fill the row
    new_row = list( `Dataset` = DATAFRAME_NAME, # same for every row
                    `Item source` = DATA_SOURCE_NAME,  # same for every row
                    `Question type` = question$QuestionType,
                    `Chapter` = DEFAULT_CHAPTER_TEXT,
                    `Title` = DEFAULT_TITLE_TEXT) 
    
    # add variable name, i.e. the question name given in qualtrics
    new_row = append(new_row, list(`Variable name` = question$DataExportTag))
    
    # add non response values
    new_row = append(new_row, NON_RESPONSE_COLUMNS_VALUES)
    
    # figure out if this question has associated Display Logic
    new_row[["hasDisplayLogic"]] = ("DisplayLogic" %in% names(question)) 
    
    # add question text
    new_row[["Question text"]] = question$QuestionText
    # add translated question text if it exists
    if (question_is_translated) {
      new_row[[paste0("Question text (", TRANSLATION_LANGUAGE_CODE, ")")]] = question[["Language"]][[TRANSLATION_LANGUAGE_CODE]]$QuestionText
    }
    
    # Ensure the new row has all columns in the dataframe, filling missing ones with NA
    new_row <- lapply(colnames(df), function(col) if (col %in% names(new_row)) new_row[[col]] else NA)
    names(new_row) <- colnames(df)
    
    # Iterate over all choices in the question
    for (i in names(question$Choices)) {
      # don't mistake keys as integers. They are characters (e.g. "8")
      i = as.character(i)
      
      # get the choice text (the statement text)
      choice_text = question[["Choices"]][[i]]$Display
      # get the translated choice text if it exists
      if (question_is_translated) {
        choice_text_translated = question[["Language"]][[TRANSLATION_LANGUAGE_CODE]][["Choices"]][[i]]$Display
      }
      
      # if the question has recoded values, the texts need to be put into the column according to those values
      if ("RecodeValues" %in% names(question)) {
        # use the recoded values and the corresponding translation column
        new_row[[as.character(question$RecodeValues[[i]])]] = choice_text
        if (question_is_translated) {
          new_row[[paste0(question$RecodeValues[[i]], " (", TRANSLATION_LANGUAGE_CODE,")")]] = choice_text_translated
        }
      } 
      # otherwise they just go into their respective columns
      else {
        # use the default values and the corresponding translation column
        new_row[[as.character(i)]] = choice_text
        new_row[[paste0(i, " (", TRANSLATION_LANGUAGE_CODE,")")]] = choice_text_translated
      }
    }
    
    
    # turn the row into a dataframe so we can add it to the output dataframe df
    new_row <- as.data.frame(new_row, stringsAsFactors = FALSE, check.names = FALSE)
    
    # make sure all entries are treated as text for the excel
    new_row[] <- lapply(new_row, as.character)
    
    # Add the new row to the dataframe
    df <- bind_rows(df, new_row)
    
    # If this choice allows text entries (e.g. "other: ...") then we add another line with _txt at the end
    # almost everything else will be empty
    if (any(sapply(question$Choices, function(choice) {
      "TextEntry" %in% names(choice) && choice$TextEntry == "true"
    }))) {
      # fill the row
      new_row = list( `Dataset` = DATAFRAME_NAME, # same for every row
                      `Item source` = DATA_SOURCE_NAME,  # same for every row
                      `Question type` = question$QuestionType,
                      `Chapter` = DEFAULT_CHAPTER_TEXT,
                      `Title` = DEFAULT_TITLE_TEXT) 
      
      new_row[["Variable name"]] = paste0(question$DataExportTag,"_txt")
      
      # figure out if this question has associated Display Logic
      new_row[["hasDisplayLogic"]]  = ("DisplayLogic" %in% names(question)) 
      
      # add non response values
      new_row = append(new_row, NON_RESPONSE_COLUMNS_VALUES)
      
      new_row[["Question text"]] = question$QuestionText
      # add translated question text if it exists
      if (question_is_translated) {
        new_row[[paste0("Question text (", TRANSLATION_LANGUAGE_CODE, ")")]] = question[["Language"]][[TRANSLATION_LANGUAGE_CODE]]$QuestionText
      }
      
      # turn the row into a dataframe so we can add it to the output dataframe df
      new_row <- as.data.frame(new_row, stringsAsFactors = FALSE, check.names = FALSE)
      
      # make sure all entries are treated as text for the excel
      new_row[] <- lapply(new_row, as.character)
      
      # Add the new row to the dataframe
      df <- bind_rows(df, new_row)
    }
  }
  
  
  # Handle questions that are matrices 
  # (Matrix question have a special structure in the API response)
  # (The Matrix question will be split into subquestions with their own row for each statement)
  else if (question$QuestionType == "Matrix") {
    counter = 0
    
    for (subquestion in names(question$Choices)) {
      # a subquestion represents one statement. Each statement has answers that are the same for each statement (agree, don't agree, etc..)
      subquestion = as.character(subquestion)
      
      # fill the row
      new_row = list( `Dataset` = DATAFRAME_NAME, # same for every row
                      `Item source` = DATA_SOURCE_NAME,  # same for every row
                      `Question type` = question$QuestionType,
                      `Chapter` = DEFAULT_CHAPTER_TEXT,
                      `Title` = DEFAULT_TITLE_TEXT) 
      
      # add variable name, i.e. the question name given in qualtrics
      # if the subquestions in the matrixs question already have tags
      if ("ChoiceDataExportTags" %in% question) {
        # use the existing tags
        new_row[["Variable name"]] = question$ChoiceDataExportTags[[subquestion]]
      } else {
        # else make them with a suffix x1, x2, ..
        counter = counter + 1
        new_row[["Variable name"]] = paste0(question$DataExportTag,"x",counter)
      }
      
      # add non response values
      new_row = append(new_row, NON_RESPONSE_COLUMNS_VALUES)
      
      # figure out if this question has associated Display Logic
      new_row[["hasDisplayLogic"]] = ("DisplayLogic" %in% names(question)) 
      
      # add question text
      new_row[["Question text"]] = question$QuestionText
      new_row[["Item text"]] = question$Choices[[subquestion]]$Display
      # add translated question text if it exists
      if (question_is_translated) {
        new_row[[paste0("Question text (", TRANSLATION_LANGUAGE_CODE, ")")]] = question[["Language"]][[TRANSLATION_LANGUAGE_CODE]]$QuestionText
        new_row[[paste0("Item text (", TRANSLATION_LANGUAGE_CODE, ")")]] = question[["Language"]][[TRANSLATION_LANGUAGE_CODE]]$Choices[[subquestion]]$Display
      }
      
      # Ensure the new row has all columns, filling missing ones with NA
      new_row <- lapply(colnames(df), function(col) if (col %in% names(new_row)) new_row[[col]] else NA)
      names(new_row) <- colnames(df)
      
      # Iterate over answers for the subquestion
      for (i in names(question$Answers)) {
        # don't mistake keys as integers. They are characters (e.g. "8")
        i = as.character(i)
        
        # get the answer text (the statement text)
        choice_text = question[["Answers"]][[i]]$Display
        # get the translated answer text if it exists
        if (question_is_translated) {
          choice_text_translated = question[["Language"]][[TRANSLATION_LANGUAGE_CODE]][["Answers"]][[i]]$Display
        }
        
        # if the question has recoded values, the texts need to be put into the column according to those values
        if ("RecodeValues" %in% names(question)) {
          # use the recoded values and the corresponding translation column
          new_row[[as.character(question$RecodeValues[[i]])]] = choice_text
          
          if (question_is_translated) {
            new_row[[paste0(question$RecodeValues[[i]], " (", TRANSLATION_LANGUAGE_CODE,")")]] = choice_text_translated
          }
        } 
        # otherwise they just go into their respective columns
        else {
          # use the default values and the corresponding translation column
          new_row[[as.character(i)]] = choice_text
          new_row[[paste0(i, " (", TRANSLATION_LANGUAGE_CODE,")")]] = choice_text_translated
        }
      }
      
      
      # turn the row into a dataframe so we can add it to the output dataframe df
      new_row <- as.data.frame(new_row, stringsAsFactors = FALSE, check.names = FALSE)
      
      # make sure all entries are treated as text for the excel
      new_row[] <- lapply(new_row, as.character)
      
      # Add the new row to the dataframe
      df <- bind_rows(df, new_row)
      
      # If this choice allows text entries (e.g. "other: ...") then we add another line with _txt at the end
      # almost everything else will be empty
      if ("TextEntry" %in% names(question$Choices[[subquestion]]) && question$Choices[[subquestion]]$TextEntry == "true") {
        # fill the row
        new_row = list( `Dataset` = DATAFRAME_NAME, # same for every row
                        `Item source` = DATA_SOURCE_NAME,  # same for every row
                        `Question type` = question$QuestionType,
                        `Chapter` = DEFAULT_CHAPTER_TEXT,
                        `Title` = DEFAULT_TITLE_TEXT) 
        
        new_row[["Variable name"]] = paste0(question$DataExportTag,"x",counter,"_txt")
        
        # figure out if this question has associated Display Logic
        new_row[["hasDisplayLogic"]] = ("DisplayLogic" %in% names(question)) 
        
        # add non response values
        new_row = append(new_row, NON_RESPONSE_COLUMNS_VALUES)
        
        # add question text
        new_row[["Question text"]] = question$QuestionText
        new_row[["Item text"]] = question$Choices[[subquestion]]$Display
        # add translated question text if it exists
        if (question_is_translated) {
          new_row[[paste0("Question text (", TRANSLATION_LANGUAGE_CODE, ")")]] = question[["Language"]][[TRANSLATION_LANGUAGE_CODE]]$QuestionText
          new_row[[paste0("Item text (", TRANSLATION_LANGUAGE_CODE, ")")]] = question[["Language"]][[TRANSLATION_LANGUAGE_CODE]]$Choices[[subquestion]]$Display
        }
        
        # turn the row into a dataframe so we can add it to the output dataframe df
        new_row <- as.data.frame(new_row, stringsAsFactors = FALSE, check.names = FALSE)
        
        # make sure all entries are treated as text for the excel
        new_row[] <- lapply(new_row, as.character)
        
        # Add the new row to the dataframe
        df <- bind_rows(df, new_row)
      }
    }
  }
  
  
  # Handle questions that are MultipleChoice with multiple selection
  # (The Matrix question will be split into subquestions with their own row for each statement)
  else if (question$QuestionType == "MC" && question$Selector == "MAVR") {
    counter = 0
    
    for (subquestion in names(question$Choices)) {
      subquestion = as.character(subquestion)
      
      # fill the row
      new_row = list( `Dataset` = DATAFRAME_NAME, # same for every row
                      `Item source` = DATA_SOURCE_NAME,  # same for every row
                      `Question type` = question$QuestionType,
                      `Chapter` = DEFAULT_CHAPTER_TEXT,
                      `Title` = DEFAULT_TITLE_TEXT) 
      
      # add variable name, i.e. the question name given in qualtrics
      # add a suffix x1, x2, ... to the Variable Name
      counter = counter + 1
      new_row[["Variable name"]] = paste0(question$DataExportTag,"x",counter)
      
      # figure out if this question has associated Display Logic
      new_row[["hasDisplayLogic"]] = ("DisplayLogic" %in% names(question)) 
      
      
      # add non response values
      new_row = append(new_row, NON_RESPONSE_COLUMNS_VALUES)
      
      # add question text
      new_row[["Question text"]] = question$QuestionText
      new_row[["Item text"]] = question$Choices[[subquestion]]$Display
      # add translated question text if it exists
      if (question_is_translated) {
        new_row[[paste0("Question text (", TRANSLATION_LANGUAGE_CODE, ")")]] = question[["Language"]][[TRANSLATION_LANGUAGE_CODE]]$QuestionText
        new_row[[paste0("Item text (", TRANSLATION_LANGUAGE_CODE, ")")]] = question[["Language"]][[TRANSLATION_LANGUAGE_CODE]]$Choices[[subquestion]]$Display
      }
      
      # Ensure the new row has all columns, filling missing ones with NA
      new_row <- lapply(colnames(df), function(col) if (col %in% names(new_row)) new_row[[col]] else NA)
      names(new_row) <- colnames(df)
      
      new_row[["0"]] = NOT_MARKED_TEXT_ORIGINAL
      new_row[[paste0("0 (",TRANSLATION_LANGUAGE_CODE,")")]] = NOT_MARKED_TEXT_TRANSLATED
      
      new_row[["1"]] = MARKED_TEXT_ORIGINAL
      new_row[[paste0("1 (",TRANSLATION_LANGUAGE_CODE,")")]] = MARKED_TEXT_TRANSLATED
      
      # turn the row into a dataframe so we can add it to the output dataframe df
      new_row <- as.data.frame(new_row, stringsAsFactors = FALSE, check.names = FALSE)
      
      # make sure all entries are treated as text for the excel
      new_row[] <- lapply(new_row, as.character)
      
      # Add the new row to the dataframe
      df <- bind_rows(df, new_row)
      
      # If this choice allows text entries (e.g. "other: ...") then we add another line with _txt at the end
      # almost everything else will be empty
      if ("TextEntry" %in% names(question$Choices[[subquestion]]) && question$Choices[[subquestion]]$TextEntry == "true") {
        # fill the row
        new_row = list( `Dataset` = DATAFRAME_NAME, # same for every row
                        `Item source` = DATA_SOURCE_NAME,  # same for every row
                        `Question type` = question$QuestionType,
                        `Chapter` = DEFAULT_CHAPTER_TEXT,
                        `Title` = DEFAULT_TITLE_TEXT) 
        
        new_row[["Variable name"]] = paste0(question$DataExportTag,"x",counter,"_txt")
        
        # figure out if this question has associated Display Logic
        new_row[["hasDisplayLogic"]] = ("DisplayLogic" %in% names(question)) 
        
        # add non response values
        new_row = append(new_row, NON_RESPONSE_COLUMNS_VALUES)
        
        # add question text
        new_row[["Question text"]] = question$QuestionText
        new_row[["Item text"]] = question$Choices[[subquestion]]$Display
        # add translated question text if it exists
        if (question_is_translated) {
          new_row[[paste0("Question text (", TRANSLATION_LANGUAGE_CODE, ")")]] = question[["Language"]][[TRANSLATION_LANGUAGE_CODE]]$QuestionText
          new_row[[paste0("Item text (", TRANSLATION_LANGUAGE_CODE, ")")]] = question[["Language"]][[TRANSLATION_LANGUAGE_CODE]]$Choices[[subquestion]]$Display
        }
        
        # turn the row into a dataframe so we can add it to the output dataframe df
        new_row <- as.data.frame(new_row, stringsAsFactors = FALSE, check.names = FALSE)
        
        # make sure all entries are treated as text for the excel
        new_row[] <- lapply(new_row, as.character)
        
        # Add the new row to the dataframe
        df <- bind_rows(df, new_row)
      }
    }
    
  }
  
  
  # Handle questions that Rankings
  
  
  else if (question$QuestionType == "RO") {
    for (ranking_number in 1:length(question$Choices)) {
      # fill the row
      new_row = list( `Dataset` = DATAFRAME_NAME, # same for every row
                      `Item source` = DATA_SOURCE_NAME,  # same for every row
                      `Question type` = question$QuestionType,
                      `Chapter` = DEFAULT_CHAPTER_TEXT,
                      `Title` = DEFAULT_TITLE_TEXT) 
      
      # add variable name, i.e. the question name given in qualtrics
      # add a suffix x1, x2, ... to the Variable Name
      new_row[["Variable name"]] = paste0(question$DataExportTag,"_ranking_",ranking_number)
      
      # figure out if this question has associated Display Logic
      new_row[["hasDisplayLogic"]] = ("DisplayLogic" %in% names(question)) 
      
      
      # add non response values
      new_row = append(new_row, NON_RESPONSE_COLUMNS_VALUES)
      
      # add question text
      new_row[["Question text"]] = question$QuestionText
      # add translated question text if it exists
      if (question_is_translated) {
        new_row[[paste0("Question text (", TRANSLATION_LANGUAGE_CODE, ")")]] = question[["Language"]][[TRANSLATION_LANGUAGE_CODE]]$QuestionText
      }
      
      # Ensure the new row has all columns, filling missing ones with NA
      new_row <- lapply(colnames(df), function(col) if (col %in% names(new_row)) new_row[[col]] else NA)
      names(new_row) <- colnames(df)
      
      for (i in names(question$Choices)) {
        i = as.character(i)
        
        # get the choice text (the statement text)
        choice_text = question[["Choices"]][[i]]$Display
        # get the translated choice text if it exists
        if (question_is_translated) {
          choice_text_translated = question[["Language"]][[TRANSLATION_LANGUAGE_CODE]][["Choices"]][[i]]$Display
        }
        
        # if the question has recoded values, the texts need to be put into the column according to those values
        if ("RecodeValues" %in% names(question)) {
          # use the recoded values and the corresponding translation column
          new_row[[as.character(question$RecodeValues[[i]])]] = choice_text
          
          if (question_is_translated) {
            new_row[[paste0(question$RecodeValues[[i]], " (", TRANSLATION_LANGUAGE_CODE,")")]] = choice_text_translated
          }
        } 
        # otherwise they just go into their respective columns
        else {
          # use the default values and the corresponding translation column
          new_row[[as.character(i)]] = choice_text
          new_row[[paste0(i, " (", TRANSLATION_LANGUAGE_CODE,")")]] = choice_text_translated
        }
      }
      
      # turn the row into a dataframe so we can add it to the output dataframe df
      new_row <- as.data.frame(new_row, stringsAsFactors = FALSE, check.names = FALSE)
      
      # make sure all entries are treated as text for the excel
      new_row[] <- lapply(new_row, as.character)
      
      # Add the new row to the dataframe
      df <- bind_rows(df, new_row)
    }
    
  }
}




# ------------------------------------------------------------------------------
# SAVE DATAFRAME TO EXCEL
# ------------------------------------------------------------------------------

remove_html <- function(text) {
  str_replace_all(text, "<.*?>", "")
  str_replace_all(text, "&nbsp;", "")
}

df <- data.frame(lapply(df, function(col) {
  if (is.character(col)) {
    col[-1] <- remove_html(col[-1])
  }
}), stringsAsFactors = FALSE, check.names = FALSE)

write.xlsx(df, CODEBOOK_XLSX_FILENAME)
