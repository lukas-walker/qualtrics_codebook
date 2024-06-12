# Qualtrics Codebook

## Manual

1. Adapt variables in first part of get_metadata_from_qualtrics_to_excel.R
2. Use get_metadata_from_qualtrics_to_excel.R to create an excel with all codebook data
3. Adapt excel file to suit your needs
4. Adapt frontpage.tex
5. Adapt variables in first part of assemble_codebook_from_excel.Rmd
6. knit pdf with assemble_codebook_from_excel.Rmd

## ToDo before start

### Create an .Renviron file in the local folder

It should contain the following :

API_KEY="YOUR API KEY"
SURVEY_ID="YOUR SURVEY ID"
SHARE_WINDOWS="YOUR WORKING SHARE"
SHARE_MAC="YOUR WORKING SHARE"