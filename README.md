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
