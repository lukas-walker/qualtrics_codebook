# Qualtrics Codebook

## Manual

1. Adapt variables in first part of get_metadata_from_qualtrics_to_excel.R
2. Use get_metadata_from_qualtrics_to_excel.R to create an excel with all codebook data
3. Adapt excel file to suit your needs
4. Adapt frontpage.tex
5. Adapt variables in first part of assemble_codebook_from_excel.Rmd
6. knit pdf with assemble_codebook_from_excel.Rmd

## More detailed:

### Git

https://git-scm.com/book/de/v2/Erste-Schritte-Git-installieren
 
### Clone git repository

- Start cmd (Windows key, type “cmd”, start)
- Go to working directory
- ```cd Folder``` goes to folder “Folder”
- ```cd ..``` jumps to parent folder

### git clone https://github.com/lukas-walker/qualtrics_codebook.git FolderName
- This command copies the full repository into the new folder ```FolderName```

### Fill in .Renviron file
- ```API_KEY=""```
- ```SURVEY_ID=""```
- ```SHARE=""```
- ```WORKING_DIRECTORY=""```
- ```LANGUAGE_CODE=""```

### get_metadata_from_qualtrics_to_excel.R
- Go through first part of the file and adapt everything that needs to be adapted. The comments will guide you. 
- Run the file

### Update the Excel that was created
- Edit Chapters and titles
- Reorganize variables according to your needs
- Delete empty columns that might appear

### Adapt frontpage.tex

### assemble_codebook_from_excel.Rmd
- Install all fonts if you didn't already (see .fonts folder)
- Knit this R markdown to PDF


## Generate Titles with ChatGPT

### import script 

Import the excel_openai_api_script.bas file into your excel file. Here's how to do that: https://support.tetcos.com/support/solutions/articles/14000143233-how-to-import-vba-script-bas-file-in-ms-excel-

Or click Alt + F11 to open the Visual Basic editor, choose Insert > Module and copy the script code. 

### Add your OpenAI API Key

Go to https://platform.openai.com/ and create an account. You will have to provide payment information to do this (the API withdraws money per token used from your account. THIS CAN GET COSTLY. USE WITH CARE.)

Go to the API Keys menu and create a new API key. Copy this string into the VBA script in Excel. Save the script.

### Use the new AIAssistant function in Excel

The script gives you a new function. It takes one cell as input. This cell should contain the question for which you want to generate your title. 

Example: ``` =AIAssistant(A9) ```

If you only want to apply this function to some rows that have some condition, you can do the following:

``` =IF(E9="x",AIAssistant(A9),"") ```

This only uses the API on this row if E9 has "x" as its value. Otherwise it just prints "". You could also put the cell above instead of "" so it just copypastes whatever is above it. 


### ATTENTION

- if you change and save the VBA script, all functions will access the API again --> cost!
- If you apply the function to multiple rows, it can take a while. The excel will not react. It is best to only to this in batches (instead of applying it to thousands of rows at the same time).
