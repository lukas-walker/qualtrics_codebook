
# This file accesses a Qualtrics Survey through the api and stores the metadata in an excel file
# The excel file can subsequently be used to create a codebook. 

# In the first part, you should update the variables that are specific to your project
# It already loads all the question info so you can check at the end if it worked (variable questions)

# DO NOT change the parts after that without knowing what you are doing.

# Content
# 1. Project specific variables
# 2. SORT QUESTIONS ACCORDING TO SURVEY
# 3. FILTER QUESTIONS
# 4. FIND ALL POSSIBLE CHOICE CODES IN ALL QUESTIONS
# 5. CREATE METADATA DATAFRAME WITH ALL ITS COLUMNS
# 6. FILL THE DATAFRAME FOR EACH QUESTION

# IMPORTANT
# Special question types like Constant Sum, Rank Order, etc. may cause issues or 
# won't show up correctly. Either make exceptions in chapter 6 for these question types
# and/or edit the excel subsequently


#-------------------------------------------------------------------------------


require(pacman)

# Read environment file
# You have to create this in your local folder.
# It should contain your folder path, API key, survey id, etc.
# Also see Readme
readRenviron("./.Renviron")


# LOAD PACKAGES

p_load(httr,tidyverse,here,openxlsx,rlist)


# CUSTOM VARIABLES AND PATHS

# This should contain the network location of your share (Create a file called .Renviron and write API_KEY="YOUR_API_KEY" ...)
SHARE <- Sys.getenv("SHARE")

# This is the path to your codebook folder (usually where this file is) on your share
WORKING_DIRECTORY <- "/qualtrics_codebook"

# This is the output name for the excel file
CODEBOOK_XLSX_FILENAME <- "codebook_data.xlsx"

# This is the name that the data set has in the end (e.g. panelX_wave1)
# (this should be computer readable)
DATAFRAME_NAME = "dataframe_name" 

# This is the human readable name for the data set (i.e. the title)
DATA_SOURCE_NAME = "Data Source Name"

# This script expects all relevant questions to have a prefix (e.g. wave1_question1)
# This is used to only select actual questions and remove e.g. intro texts, timers etc. 
QUESTION_PREFIX = "wave1_"

DEFAULT_CHAPTER_TEXT = "CHAPTER"
DEFAULT_TITLE_TEXT = "TITLE"

# Set this to true if your survey uses prefixes
USING_QUESTION_PREFIX = FALSE

# Qualtrics API url: https://[Datacenter ID].qualtrics.com/API/v3/surveys/[Survey ID]/
# All info can be found on Qualtrics (Account Settings-> Qualtrics IDs)

# The base needs to match your survey link (But with the "/API/v3/surveys/" in the end)
BASE_URL = "https://descil.eu.qualtrics.com/API/v3/survey-definitions/"

# you can find this token in the survey settings
QUALTRICS_API_TOKEN = Sys.getenv("API_KEY")

# you can find this ID in the survey link when you publish it
QUALTRICS_SURVEY_ID = Sys.getenv("SURVEY_ID")

# This code needs to match the language settings in the qualtrics survey
TRANSLATION_LANGUAGE_CODE = "EN"

URL_WITH_TRANSLATIONS = paste0(BASE_URL, QUALTRICS_SURVEY_ID) #, "/translations/", TRANSLATION_LANGUAGE_CODE)

# Non response columns
NON_RESPONSE_COLUMNS = c("-22", #"not in panel"
                         "-33", #"unit nonresponse"
                         "-44", #"missing by m.o.p."
                         "-55", #"missing by technical error"
                         "-66", #"missing by design"
                         "-77", #"not reached"
                         "-88", #"missing by filter"
                         "-97", #"nonvalid answer in survey (e.g. ambigious)"
                         "-99") #"item nonresponse"

NON_RESPONSE_COLUMNS_VALUES = list(`-22` = "not in panel",
                                 `-33` = "unit nonresponse",
                                 `-44` = "missing by m.o.p.",
                                 `-55` = "missing by technical error",
                                 `-66` = "missing by design",
                                 `-77` = "not reached",
                                 `-88` = "missing by filter",
                                 `-97` = "nonvalid answer in survey (e.g. ambigious)",
                                 `-99` = "item nonresponse")

# for binary questions, these will be added to the 0 and 1 columns respectively
MARKED_TEXT_ORIGINAL = "Markiert"
NOT_MARKED_TEXT_ORIGINAL = "Nicht Markiert"
MARKED_TEXT_TRANSLATED = "Selected"
NOT_MARKED_TEXT_TRANSLATED = "Not Selected"

# SET WORKING DIRECTORY
setwd(paste0(SHARE,WORKING_DIRECTORY))

# ACCESS QUALTRICS API TO RECEIVE METADATA
headers = c("X-API-TOKEN" = QUALTRICS_API_TOKEN)

# Make API get request
res <- GET(url = URL_WITH_TRANSLATIONS, add_headers(headers))

# Get whole content from survey 
cont <- content(res)

# All question data 
questions = cont$result$Questions

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
      print(element$QuestionID)
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


default_columns = c("Variable name", # the name given to the qualtrics question
                    "Variable label", # additional name for e.g. embedded data fields
                    "Dataset", # contains the computer readable name of the dataset (each row is the same)
                    "Item source", # contains the human readable name of the dataset (each row is the same)
                    "Question type", # contains the question type
                    "Chapter", # contains a manually chosen chapter name (Insert manually in excel!)
                    "Title", # contains a manually chosen title for the question(Insert manually in excel!)
                    "Comment" # additional comments that can be added in the excel
                    )


# These columns with get an additional column for each language with the corresponding language code (e.g. Column (EN))
translated_columns = c("Intro text", # introduction text to the question
                    "Question text", # The actual text of the question
                    "Item text") # Text for multiple choice questions with multiple possible answers (will be split into binary questions for each item)

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


all_columns = c(default_columns, translated_columns_expanded, codes, codes_translation, NON_RESPONSE_COLUMNS)

df <- data.frame(matrix(ncol = length(all_columns), nrow = 0))

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
        new_row[[paste0(question$RecodeValues[[i]], " (", TRANSLATION_LANGUAGE_CODE,")")]] = choice_text_translated
      } 
      # otherwise they just go into their respective columns
      else {
        # use the default values and the corresponding translation column
        new_row[[as.character(i)]] = choice_text
        new_row[[paste0(i, " (", TRANSLATION_LANGUAGE_CODE,")")]] = choice_text_translated
      }
    }
    
    
    # turn the row into a dataframe so we can add it to the output dataframe df
    new_row <- as.data.frame(new_row, stringsAsFactors = FALSE, , check.names = FALSE)
    
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
      
      # add non response values
      new_row = append(new_row, NON_RESPONSE_COLUMNS_VALUES)
      
      new_row[["Question text"]] = question$QuestionText
      # add translated question text if it exists
      if (question_is_translated) {
        new_row[[paste0("Question text (", TRANSLATION_LANGUAGE_CODE, ")")]] = question[["Language"]][[TRANSLATION_LANGUAGE_CODE]]$QuestionText
      }
      
      # turn the row into a dataframe so we can add it to the output dataframe df
      new_row <- as.data.frame(new_row, stringsAsFactors = FALSE, , check.names = FALSE)
      
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
      
      # add question text
      new_row[["Question text"]] = question$Choices[[subquestion]]$Display
      # add translated question text if it exists
      if (question_is_translated) {
        new_row[[paste0("Question text (", TRANSLATION_LANGUAGE_CODE, ")")]] = question[["Language"]][[TRANSLATION_LANGUAGE_CODE]]$Choices[[subquestion]]$Display
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
          new_row[[paste0(question$RecodeValues[[i]], " (", TRANSLATION_LANGUAGE_CODE,")")]] = choice_text_translated
        } 
        # otherwise they just go into their respective columns
        else {
          # use the default values and the corresponding translation column
          new_row[[as.character(i)]] = choice_text
          new_row[[paste0(i, " (", TRANSLATION_LANGUAGE_CODE,")")]] = choice_text_translated
        }
      }
      
      
      # turn the row into a dataframe so we can add it to the output dataframe df
      new_row <- as.data.frame(new_row, stringsAsFactors = FALSE, , check.names = FALSE)
      
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
        
        # add non response values
        new_row = append(new_row, NON_RESPONSE_COLUMNS_VALUES)
        
        # add question text
        new_row[["Question text"]] = question$Choices[[subquestion]]$Display
        # add translated question text if it exists
        if (question_is_translated) {
          new_row[[paste0("Question text (", TRANSLATION_LANGUAGE_CODE, ")")]] = question[["Language"]][[TRANSLATION_LANGUAGE_CODE]]$Choices[[subquestion]]$Display
        }
        
        # turn the row into a dataframe so we can add it to the output dataframe df
        new_row <- as.data.frame(new_row, stringsAsFactors = FALSE, , check.names = FALSE)
        
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
      
      
      # add non response values
      new_row = append(new_row, NON_RESPONSE_COLUMNS_VALUES)
      
      # add question text
      new_row[["Question text"]] = question$Choices[[subquestion]]$Display
      # add translated question text if it exists
      if (question_is_translated) {
        new_row[[paste0("Question text (", TRANSLATION_LANGUAGE_CODE, ")")]] = question[["Language"]][[TRANSLATION_LANGUAGE_CODE]]$Choices[[subquestion]]$Display
      }
      
      # Ensure the new row has all columns, filling missing ones with NA
      new_row <- lapply(colnames(df), function(col) if (col %in% names(new_row)) new_row[[col]] else NA)
      names(new_row) <- colnames(df)
      
      new_row[["0"]] = NOT_MARKED_TEXT_ORIGINAL
      new_row[[paste0("0 (",TRANSLATION_LANGUAGE_CODE,")")]] = NOT_MARKED_TEXT_TRANSLATED
      
      new_row[["1"]] = MARKED_TEXT_ORIGINAL
      new_row[[paste0("1 (",TRANSLATION_LANGUAGE_CODE,")")]] = MARKED_TEXT_TRANSLATED
      
      # turn the row into a dataframe so we can add it to the output dataframe df
      new_row <- as.data.frame(new_row, stringsAsFactors = FALSE, , check.names = FALSE)
      
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
        
        # add non response values
        new_row = append(new_row, NON_RESPONSE_COLUMNS_VALUES)
        
        # add question text
        new_row[["Question text"]] = question$Choices[[subquestion]]$Display
        # add translated question text if it exists
        if (question_is_translated) {
          new_row[[paste0("Question text (", TRANSLATION_LANGUAGE_CODE, ")")]] = question[["Language"]][[TRANSLATION_LANGUAGE_CODE]]$Choices[[subquestion]]$Display
        }
        
        # turn the row into a dataframe so we can add it to the output dataframe df
        new_row <- as.data.frame(new_row, stringsAsFactors = FALSE, , check.names = FALSE)
        
        # make sure all entries are treated as text for the excel
        new_row[] <- lapply(new_row, as.character)
        
        # Add the new row to the dataframe
        df <- bind_rows(df, new_row)
      }
    }
    
  }
  
  
  # Handle questions that are MultipleChoice with multiple selection
  # (The Matrix question will be split into subquestions with their own row for each statement)
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
          new_row[[paste0(question$RecodeValues[[i]], " (", TRANSLATION_LANGUAGE_CODE,")")]] = choice_text_translated
        } 
        # otherwise they just go into their respective columns
        else {
          # use the default values and the corresponding translation column
          new_row[[as.character(i)]] = choice_text
          new_row[[paste0(i, " (", TRANSLATION_LANGUAGE_CODE,")")]] = choice_text_translated
        }
      }
      
      # turn the row into a dataframe so we can add it to the output dataframe df
      new_row <- as.data.frame(new_row, stringsAsFactors = FALSE, , check.names = FALSE)
      
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

write.xlsx(df, CODEBOOK_XLSX_FILENAME)
