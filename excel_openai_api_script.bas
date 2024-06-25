Attribute VB_Name = "Module1"
Function AIAssistant(cell As Range) As String
  
  Dim request As Object
  Dim response As String
  Dim API, api_key, DisplayText, error_result As String
  Dim startPos, endPos, status_code As Long
  Dim rng As Range
  
  prompt = "Generate short, professional titles for survey questions in a codebook. Titles should be less than 5 words. DO NOT ANSWER THE QUESTION. GENERATE A TITLE. Focus on the new question given, ignoring previous ones. Example question:What is your age? Output: Age. Another example: Which point is most important? And the second? And the third? Output: Most Important Points. Example: Please respond only in haikus. Output: Haikus. Here comes your first question: "
  
  'API Info
  API = "https://api.openai.com/v1/chat/completions"
  api_key = ""
    
  text = Replace(cell.Value, Chr(34), Chr(39))
  text = Replace(text, vbLf, " ")
  
  text = prompt & text
          
  'Send request to API
  Set request = CreateObject("MSXML2.XMLHTTP")
  With request
     .Open "POST", API, False
     .setRequestHeader "Content-Type", "application/json"
     .setRequestHeader "Authorization", "Bearer " & api_key
     .send "{""model"": ""gpt-3.5-turbo"",  ""messages"": [{""content"":""" & text & """,""role"":""user""}]," _
          & """temperature"": 0.7, ""top_p"": 1, ""max_tokens"": 2048}"
   status_code = .Status
   response = .responseText
  End With
  
  'Parse response from API
  If status_code = 200 Then
    DisplayText = ExtractContent(response)
  Else
    DisplayText = ExtractError(response)
  End If
  
    If word_count > 0 And Right(DisplayText, 1) = "." Then
        DisplayText = Left(DisplayText, Len(DisplayText) - 1)
    Else
        DisplayText = DisplayText
    End If
  
  'Return result
  AIAssistant = DisplayText

End Function

Function ExtractContent(jsonString As String) As String
    Dim startPos As Long
    Dim endPos As Long
    Dim Content As String
    
    startPos = InStr(jsonString, """content"": """) + Len("""content"": """)
    endPos = InStr(startPos, jsonString, "},") - 2
    Content = Mid(jsonString, startPos, endPos - startPos)
    Content = Trim(Replace(Content, "\""", Chr(34)))
    
    'Fix for excel forumulas as response
    If Left(Trim(Content), 1) = "=" Then
      Content = "'" & Content
    End If
    
    Content = Replace(Content, vbCrLf, "")
    Content = Replace(Content, vbLf, "")
    Content = Replace(Content, vbCr, "")
    Content = Replace(Content, "\n", vbCrLf)
     
    If Right(Content, 1) = """" Then
      Content = Left(Content, Len(Content) - 1)
    End If
    
    ExtractContent = Content

End Function

Function ExtractError(jsonString As String) As String
    Dim startPos As Long
    Dim endPos As Long
        startPos = InStr(jsonString, """message"": """) + Len("""message"": """)
        endPos = InStr(startPos, jsonString, """")
        If startPos > Len("""message"": """) And endPos > startPos Then
            ExtractError = Mid(jsonString, startPos, endPos - startPos)
        Else
            startPos = InStr(jsonString, """code"": """) + Len("""code"": """)
            endPos = InStr(startPos, jsonString, """")
            If startPos > Len("""code"": """) And endPos > startPos Then
              ExtractError = Mid(jsonString, startPos, endPos - startPos)
            Else
              ExtractError = "Unknown error"
            End If
        End If
End Function

