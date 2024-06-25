
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



# LOAD PACKAGES

require(pacman)
p_load(httr,tidyverse,here,openxlsx,rlist,xml2)

# sets wd to this script's location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Read environment file
# You have to create this in your local folder.
# It should contain your folder path, API key, survey id, etc.
# Also see Readme
readRenviron("./.Renviron")



# CUSTOM VARIABLES AND PATHS

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
TRANSLATION_LANGUAGE_CODE = Sys.getenv("LANGUAGE_CODE")

URL_WITH_TRANSLATIONS = paste0(BASE_URL, QUALTRICS_SURVEY_ID) #, "/translations/", TRANSLATION_LANGUAGE_CODE)

# Default Columns
default_columns = c("Variable name", # the name given to the qualtrics question
                    "Variable label", # additional name for e.g. embedded data fields
                    "Dataset", # contains the computer readable name of the dataset (each row is the same)
                    "Item source", # contains the human readable name of the dataset (each row is the same)
                    "Question type", # contains the question type
                    "Comment", # additional comments that can be added in the excel
                    "Sample Filter",
                    "Questionnaire flow filter",
                    "Treatment filter"
)

default_columns_not_in_table = c("Chapter", # contains a manually chosen chapter name (Insert manually in excel!)
                                 "Subchapter", # contains a manually chosen subchapter name (Insert manually in excel!)
                                 "Title") # contains a manually chosen title for the question(Insert manually in excel!)



# These columns with get an additional column for each language with the corresponding language code (e.g. Column (EN))
translated_columns = c("Intro text", # introduction text to the question
                       "Question text", # The actual text of the question
                       "Item text") # Text for multiple choice questions with multiple possible answers (will be split into binary questions for each item)


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

DEFAULT_CHAPTER_TEXT = "CHAPTER"
DEFAULT_TITLE_TEXT = "TITLE"

# ACCESS QUALTRICS API TO RECEIVE METADATA
headers = c("X-API-TOKEN" = QUALTRICS_API_TOKEN)

# Make API get request
res <- GET(url = URL_WITH_TRANSLATIONS, add_headers(headers))

# Get whole content from survey 
cont <- content(res)

# All question data 
questions = cont$result$Questions

source("scripts/process_questions_metatada.R")
