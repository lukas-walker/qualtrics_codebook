# Qualtrics Codebook

## Manual

1. Make a copy the file .Renviron_Template and name it .Renviron (Files that start on a "." are hidden files. You might have to change your folder view settings to be able to see them.)
2. Edit the variables within the .Renviron file
3. Run the script ```step_1_get_metadata_from_qualtrics_to_excel.R```, which creates an excel with all codebook data
5. Adapt excel file to suit your needs
6. Adapt frontpage.tex to suit your needs
7. Adapt the file ```step_2_assemble_codebook_from_excel.Rmd``` (e.g. some texts that you want to add)
8. Run the ```step_2_assemble_codebook_from_excel.Rmd``` file and enjoy your codebook!

## More detailed:

### Install git

https://git-scm.com/book/de/v2/Erste-Schritte-Git-installieren
 
### Clone this git repository

- Start cmd (Windows key, type “cmd”, start)
- Go to the directory where you want your codebook to be built. The following commands help you:
- ```cd Folder``` goes to folder “Folder”
- ```cd ..``` jumps to parent folder

### git clone https://github.com/lukas-walker/qualtrics_codebook.git FolderName
- This command copies the full repository into the new folder ```FolderName```

### Fill in .Renviron file
- Copy the file .Renviron_Template file and name it .Renviron
- Files that start with a . are "hidden files". If you don't see it, you probably have to change your setting to see hidden files
- The lines that start with # are comments and tell you what value you should put

### Run ```step_1_get_metadata_from_qualtrics_to_excel.R```
- If the .Renviron file was filled in correctly, this file should run without any problems

### Update the Excel file that was created
- Edit Chapters and titles
- Reorganize variables according to your needs
- Delete empty columns that might appear

### Adapt frontpage.tex

### assemble_codebook_from_excel.Rmd
- Install all fonts if you don't already have installed locally (see .fonts folder)
- Knit this R markdown to PDF
- done!

## Generate Titles with ChatGPT

### Open the Excel with Macros-enabled file OpenAI_Excel.xlsm and import the ```excel_openai_api_script.bas``` script

Import the excel_openai_api_script.bas file into your excel file. Here's how to do that: https://support.tetcos.com/support/solutions/articles/14000143233-how-to-import-vba-script-bas-file-in-ms-excel-

Or click Alt + F11 to open the Visual Basic editor, choose Insert > Module and copy the script code. 

This script contains a User Defined Function (UDF) ```=AIAssistant()``` that calls the OpenAI API. It takes as input one cell that should contain a question and gives as output a title. 

### Add your OpenAI API Key to the script

Go to https://platform.openai.com/ and create an account. You will have to provide payment information to do this (the API withdraws money per token used from your account. THIS CAN GET COSTLY. USE WITH CARE.)

Go to the API Keys menu and create a new API key. Copy this string into the VBA script in Excel into the variable ```api_key``` Save the script.

### Copy the questions column from your metadata excel file to the OpenAI_Excel.xlsm.

Make sure you do all of the following in one step in order not to shift any rows. 


### Use the ```AIAssistant()``` function to generate titles in the column next to it.

The Macro-enabled Excel file already contains a template function. Below is an explanation of how it works.

If you only want to apply this function to some rows that have some condition, you can do the following:

``` =IF(R[-1]="x",AIAssistant(R[-2]),"") ``` (only uses ChatGPT if there is an "x" in the column to the left and prints "" if there is no "x")

If you only want to apply the function to rows that have a new question (instead of a question that is equal to the previous row), you can do the following:

``` =IF(AND(R[-1]C[-1]=RC[-1]), R[-1]C, AIAssistant(RC[-1])) ``` 

This prints the same value as the row above if the two questions to the left are equal (in that case you really don't need to run the function twice, you can just copy the same title from above)

### ATTENTION

- if you change and save the VBA script, all functions will access the API again --> cost!
- If you apply the function to multiple rows, it can take a while. The excel will not react. It is best to only to this in batches (instead of applying it to thousands of rows at the same time).
