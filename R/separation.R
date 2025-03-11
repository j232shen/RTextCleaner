library(httr)
library(jsonlite)
library(tidyr)
library(dplyr)
library(readr)
library(purrr)
library(stringr)

process_text <- function(text) {
  text <- gsub("\\\\n", "\n", text)
  text<- clean_raw(text)
  #print(text)
  if (grepl("\"code\": \"\"", text)) { 
    return(process_pure_text(text))
  } else if (grepl("\"text\": \"\"", text)) { 
    return(process_code(text))
  } else {  
    return(process_mixed_code(text))
  }
}

# Step 2: Process pure text (no code)
process_pure_text <- function(text) {

  text_clean <- gsub("\\s+", " ", text) 
  text_clean <- gsub("\\n", " ", text_clean)  
  
  json_text <- gsub("[^[:print:]]", "", text_clean)

  if (!validate(json_text)) {
    warning("Invalid JSON format. Returning original text.")
    return(list(text = text_clean, code = ""))
  }

  parsed <- fromJSON(json_text)
  parsed_text <- parsed$text
  
  return(list(text = parsed_text, code = ""))
}

# Step 3: Process mixed text and code
process_mixed_code <- function(text) {
  # Step 1: Replace 'n+' with newlines (correct pattern)
  text <- gsub("\\n+", "\n", text)
  
  # Step 2: Remove excess whitespace
  text <- gsub("\\s+", " ", text)
  
  #print(text)
  text_part <- sub('.*"text": "(.*?)".*', "\\1", text)
  code_part <- sub('.*"code": "(.*?)".*', "\\1", text)
  text_part <- gsub("n", "\n", text_part)
  code_part <- gsub("n", "\n", code_part)
  
  
  return(list(text = text_part, code = code_part))
  
}

# Step 4: Process pure code (only code)
process_code <- function(text) {
  # Assuming the input text is a JSON block, we clean it up
  text_clean <- gsub("```json\\n|\\n```", "", text)  # Remove JSON block markers
  text_clean <- gsub("\\n", " ", text_clean)  # Replace newlines with spaces
  
  return(list(text = "", code = text_clean))
}





clean_raw <- function(json_string){
  # Clean up the JSON string
  json_string <- gsub("```json\\n|\\n```", "", json_string)
  json_string <- gsub("\\n", " ", json_string)
  json_string <- gsub("\\s+", " ", json_string)
  json_string <- gsub('\\\\\\"', '"', json_string)
  json_string <- gsub("\\\\", "", json_string)
  json_string <- gsub("\\n+", "\n", json_string)
  return (json_string)
}

# Function to handle Gemini API call
gemini <- function(prompt, 
                   temperature=1,
                   max_output_tokens=1024,
                   api_key=Sys.getenv("GEMINI_API_KEY"),
                   model = "gemini-2.0-flash") {
  if(nchar(api_key)<1) {
    api_key <- readline("Paste your API key here: ")
    Sys.setenv(GEMINI_API_KEY = api_key)
  }
  
  model_query <- paste0(model, ":generateContent")
  
  response <- POST(
    url = paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query),
    query = list(key = api_key),
    content_type_json(),
    encode = "json",
    body = list(
      contents = list(
        parts = list(
          list(text = prompt)
        )),
      generationConfig = list(
        temperature = temperature,
        maxOutputTokens = max_output_tokens
      )
    )
  )
  
  if (response$status_code > 200) {
    warning(paste("Error - ", content(response)$error$message))
    return(NA)
  }
  
  candidates <- content(response)$candidates
  json_string <- candidates[[1]]$content$parts[[1]]$text[1]
  response_data <- process_text(json_string)
  
  
  return(response_data)
}
separate_code_prompt <- function(text) {
  paste0(
    "Please analyze the following content and separate it into two parts: \n",
    "(1) Only natural language text (no code), \n",
    "(2) Mixed text with code, \n",
    "Return the result in the following JSON format: \n",
    "{\n",
    "  \"text\": \"[Extracted natural language text]\",\n",
    "  \"code\": \"[Extracted code snippets]\"\n",
    "}\n",
    "\n",
    "Here is the input content:\n\n", text, "\n\n",
    
    "Please note: Ignore the examples in this prompt and only return the relevant results based on the input content provided above.\n",
    
    # Example 1: Mixed text and code
    "Example 1: Mixed text and code\n",
    "Input: What is the benefit in using this approach:\n",
    "```\n",
    "func wrapError(err error, msg string) error {\n",
    "    if err != nil {\n",
    "        return fmt.Errorf(\"%s: %w\", msg, err)\n",
    "    }\n",
    "    return nil\n",
    "}\n",
    "```\n",
    "Output: \n",
    "{\n",
    "  \"text\": \"What is the benefit in using this approach:\",\n",
    "  \"code\": \"func wrapError(err error, msg string) error {\\n    if err != nil {\\n        return fmt.Errorf(\\\"%s: %w\\\", msg, err)\\n    }\\n    return nil\\n}\",\n",
    "}\n\n",
    
    # Example 2: Only text
    "Example 2: Only text\n",
    "Input: Does this mean jenkins running?\n",
    "Output: \n",
    "{\n",
    "  \"text\": \"Does this mean jenkins running?\",\n",
    "  \"code\": \"\",\n",
    "}\n\n",
    
    # Example 3: Code-only text
    "Example 3: Only code\n",
    "Input: ```c\n#include <stdio.h>\nint main() {\n    printf(\\\"Hello World!\\\");\n    return 0;\n}\n``` \n",
    "Output: \n",
    "{\n",
    "  \"text\": \"\",\n",
    "  \"code\": \"#include <stdio.h>\\nint main() {\\n    printf(\\\"Hello World!\\\");\\n    return 0;\\n}\",\n",
    "}\n\n",
    
    # Example 4: Text with newlines but no code
    "Example 4: Text with newlines but no code\n",
    "Input: \njenkins@991bc3a88ae7:/$\n\nDoes this mean jenkins running:\n",
    "Output: \n",
    "{\n",
    "  \"text\": \"jenkins@991bc3a88ae7:/$\\n\\nDoes this mean jenkins running:\",\n",
    "  \"code\": \"\",\n",
    "}\n\n",
    
    # Example 5: Only text
    "Example 5: Only text\n",
    "Input: How to run a java class inside of a container with testcontainers?\n",
    "Output: \n",
    "{\n",
    "  \"text\": \"How to run a java class inside of a container with testcontainers?\",\n",
    "  \"code\": \"\",\n",
    "}\n\n",
    
    # Example 6: Mixed text and code (new example added)
    "Example 6: Mixed text and code (generalized)\n",
    "Input: Explain this code\n",
    "```\n",
    "import module1\n",
    "import module2\n",
    "\n",
    "from package import submodule\n",
    "\n",
    "class Example:\n",
    "    def __init__(self, param=None):\n",
    "        if param:\n",
    "            self.value = param\n",
    "            print(f\"Value set to {param}\")\n",
    "        else:\n",
    "            self.default_behavior()\n",
    "```\n",
    "Output: \n",
    "{\n",
    "  \"text\": \"Explain this code\",\n",
    "  \"code\": \"import module1\\nimport module2\\n\\nfrom package import submodule\\n\\nclass Example:\\n    def __init__(self, param=None):\\n        if param:\\n            self.value = param\\n            print(f\\\"Value set to {param}\\\")\\n        else:\\n            self.default_behavior()\",\n",
    "}\n",
    
    # Example 8: #include with code or mixed content
    "Example 8: #include with code or mixed content\n",
    "Input: \n#include <stdio.h>\n\nint main() {\n    printf(\"Hello World!\");\n    return 0;\n}\n", 
    "Output: \n",
    "{\n",
    "  \"text\": \"\",\n",
    "  \"code\": \"#include <stdio.h>\\n\\nint main() {\\n    printf(\\\"Hello World!\\\");\\n    return 0;\\n}\",\n",
    "}\n\n",
    
    # Example 9: #include with mixed text
    "Example 9: #include with mixed text\n",
    "Input: \nHere is a simple C code:\n#include <stdio.h>\n\nint main() {\n    printf(\"Hello World!\");\n    return 0;\n}\n", 
    "Output: \n",
    "{\n",
    "  \"text\": \"Here is a simple C code:\",\n",
    "  \"code\": \"#include <stdio.h>\\n\\nint main() {\\n    printf(\\\"Hello World!\\\");\\n    return 0;\\n}\",\n",
    "}\n\n",
    
    "Example 9: Python error with explanation (Mixed text and code)\n",
    "Input: \n",
    "I encountered this error when running my script:\n",
    "```\n",
    "File \"<ipython-input-30-ddfc2a3977c3>\", line 2\n",
    "    img = np.invert(np.array([img]))\n",
    "    ^\n",
    "IndentationError: unexpected indent\n",
    "```\n",
    "What does this mean, and how can I fix it?\n",
    "Output: \n",
    "{\n",
    "  \"text\": \"I encountered this error when running my script: What does this mean, and how can I fix it?\",\n",
    "  \"code\": \"File \\\"<ipython-input-30-ddfc2a3977c3>\\\", line 2\\n    img = np.invert(np.array([img]))\\n    ^\\nIndentationError: unexpected indent\",\n",
    "}\n\n",
    
    # Python Error Example - Code only
    "Example 10: Pure Python error message (Only code)\n",
    "Input: \n",
    "```\n",
    "File \"<ipython-input-42-abcd1234>\", line 5\n",
    "    x = y + z\n",
    "        ^\n",
    "ValueError: invalid operation\n",
    "```\n",
    "Output: \n",
    "{\n",
    "  \"text\": \"\",\n",
    "  \"code\": \"File \\\"<ipython-input-42-abcd1234>\\\", line 5\\n    x = y + z\\n        ^\\nValueError: invalid operation\",\n",
    "}\n\n",
    # Example 11: Traceback error message
    "Example 11: Traceback error message\n",
    "Input: \nTraceback (most recent call last):\n  File \"example.py\", line 10, in <module>\n    do_something()\nNameError: name 'do_something' is not defined\n", 
    "Output: \n",
    "{\n",
    "  \"text\": \"\",\n",
    "  \"code\": \"Traceback (most recent call last):\\n  File \\\"example.py\\\", line 10, in <module>\\n    do_something()\\nNameError: name 'do_something' is not defined\",\n",
    "}\n\n",
    
    # Example 12: Flask app (mixed text and code)
    "Example 12: Flask app (mixed text and code)\n",
    "Input: \nHere is my Flask app:\n```\nfrom flask import Flask, render_template, jsonify\nimport os\nimport cv2\nimport numpy as np\nimport tensorflow as tf\napp = Flask(__name__)\n\n@app.route('/')\ndef home():\n    return 'Hello, World!'\nif __name__ == '__main__':\n    app.run(debug=True)\n```", 
    "Output: \n",
    "{\n",
    "  \"text\": \"Here is my Flask app:\",\n",
    "  \"code\": \"from flask import Flask, render_template, jsonify\\nimport os\\nimport cv2\\nimport numpy as np\\nimport tensorflow as tf\\napp = Flask(__name__)\\n\\n@app.route('/')\\ndef home():\\n    return 'Hello, World!'\\nif __name__ == '__main__':\\n    app.run(debug=True)\",\n",
    "}\n\n",
    
    # Example 13: Python comment (with special encoding and notebook format)
    "Example 13: Python comment (with special encoding)\n",
    "Input: \n# -*- coding: utf-8 -*-\n\"\"\"Untitled12.ipynb\"\"\"\n\nimport numpy as np\nprint('Hello World!')", 
    "Output: \n",
    "{\n",
    "  \"text\": \"# -*- coding: utf-8 -*-\\n\"\"\"Untitled12.ipynb\"\"\",\n",
    "  \"code\": \"import numpy as np\\nprint('Hello World!')\",\n",
    "}\n\n",
    
    # Example 14: SQL query (generalized)
    "Example 14: SQL query (generalized)\n",
    "Input: \nSELECT ename FROM employees LEFT JOIN player p ON players.eid = employees.eid;\n", 
    "Output: \n",
    "{\n",
    "  \"text\": \"\",\n",
    "  \"code\": \"SELECT ename FROM employees LEFT JOIN player p ON players.eid = employees.eid;\",\n",
    "}\n\n",
    
    # Example 15: JavaScript (text mixed with code)
    "Example 15: JavaScript (mixed text and code)\n",
    "Input: \nYou are Junior, an AI system aiding developers. You are working with a part of a large program called the \"Working Set.\"\nBefore starting, check if you need more files to solve the task.\nDo not edit files without knowing their contents!\nAsk for them in normal conversational format.\nHere is the code snippet:\n```\nlet x = 5;\nfunction add(a, b) {\n    return a + b;\n}\n```", 
    "Output: \n",
    "{\n",
    "  \"text\": \"You are Junior, an AI system aiding developers. You are working with a part of a large program called the 'Working Set.'\\nBefore starting, check if you need more files to solve the task.\\nDo not edit files without knowing their contents!\\nAsk for them in normal conversational format.\",\n",
    "  \"code\": \"let x = 5;\\nfunction add(a, b) {\\n    return a + b;\\n}\",\n",
    "}\n\n",
    
    # Example 16: CSS (code-only, React example)
    "Example 16: CSS (code-only, React example)\n",
    "Input: \nIs this a correct understanding of React's useLayoutEffect:\n```\nMental model\ncomponent code runs --> React updates DOM --> component settles --> useEffect runs\n```", 
    "Output: \n",
    "{\n",
    "  \"text\": \"Is this a correct understanding of React's useLayoutEffect:\",\n",
    "  \"code\": \"Mental model\\ncomponent code runs --> React updates DOM --> component settles --> useEffect runs\",\n",
    "}\n\n"
    )
}

data <- read_csv("NL_code_difference_raw.csv") %>%
  mutate(extractedCode = trimws(extractedCode)) %>%
  filter(extractedCode != 'No code found in input')%>%
  slice(103:123) %>%
  filter(Detected_Language == 'en') %>%
  mutate(result = map(Prompt, ~{
    prompt <- separate_code_prompt(.x)
    response <- gemini(prompt)
    
    #print(response)
    
    if (is.list(response) && all(c("text", "code") %in% names(response))) {
      return(list(text = response$text, code = response$code))
    } else {
      return(list(text = NA, code = NA))
    }
  })) %>%
  unnest_wider(result)

write_csv(data, "processed_output.csv")










