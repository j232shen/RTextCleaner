# RTextCleaner

## Overview
RTextCleaner is an R package designed to process and clean text data efficiently. It includes functions for separating natural language text from code, removing noise, normalizing text, simplifying complex text, and visualizing token frequency before and after cleaning. This package is especially useful for data cleaning in NLP tasks, preparing raw text for analysis, and enhancing text quality in datasets.

## Features
- **Separation of Code and Text** (@Jingran)  
  - Automatically extracts code snippets (e.g., Python, JavaScript, HTML) from natural language text.
  - Useful for cleaning technical documentation, forum posts, or datasets with mixed content.

- **Remove Noise** (@Skylar Shao)  
  - Eliminates unwanted characters, excessive punctuation, odd spacing, and other non-textual noise.
  - Ideal for processing scraped web data, social media posts, and user-generated content.

- **Text Normalization** (@Skylar Shao)  
  - Corrects capitalization, spelling errors, and informal language (e.g., "u" to "you").
  - Helps standardize text for better analysis in customer feedback and online reviews.
 
- **Sentiment Analysis** (@Skylar Shao) 
  - Function: Classifies text as Positive, Neutral, or Negative based on sentiment analysis using the Gemini API.
  - Use Case: Analyzing customer feedback, product reviews, or social media sentiment.

- **Text Simplification** (@Jane Shen)  
  - Converts complex text into simpler language, making it easier to read and understand.
  - Useful for summarizing academic papers, medical documents, and technical content.

- **Token Frequency Visualization** (@Jane Shen)  
  - Generates comparative bar charts to display token frequency before and after text cleaning.
  - Helps users evaluate the impact of text processing on datasets.

## Installation
To install the latest version of `RTextCleaner`, run:
```r
# Install from GitHub
remotes::install_github("yourusername/RTextCleaner")
```

## Usage
### Separating Code from Text
```r
library(RTextCleaner)
text_sample <- "Here is some text. ```python print('Hello, World!') ```"
result <- separate_code_prompt(text_sample)
print(result)
```

### Removing Noise
```r
noisy_text <- c("OMG!!!!! This is soooo cooool!!!")
cleaned_text <- gemini_remove_noise(noisy_text)
print(cleaned_text)
```

### Normalizing Text
```r
text_samples <- c("omg dis is da best day evr!!!", "yayyy, tysm this is sooo gr8!!!")
normalized_text <- gemini_text_normalization(text_samples)
print(normalized_text)
```

### Sentiment Analysis
```r
text_samples <- c("I love pizza, sushi, and burgers!", "I hate horror movies. They're terrible.")
sentiment_results <- gemini_sentiment_analysis(text_samples)
print(sentiment_results)
```

### Text Simplification
```r
complex_text <- "Quantum computing leverages the principles of quantum mechanics to perform computations exponentially faster than classical computers."
simplified_text <- gemini_text_simplification(complex_text)
print(simplified_text)
```

### Visualizing Token Frequency
```r
visualize_normalization(top_n = 20)
```

## Contributing
We welcome contributions from the community! Please follow these steps:
1. Fork the repository.
2. Create a new branch for your feature or bug fix.
3. Commit your changes with clear commit messages.
4. Submit a pull request.

## Code of Conduct
This project follows the [Contributor Covenant Code of Conduct](CODE_OF_CONDUCT.md). Please be respectful in all interactions.

## License
This package is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

## Contact
For questions or issues, please reach out via GitHub Issues or contact the maintainers:
- [@Jingran](https://github.com/zhaojr23)
- [@SkylarShao](https://github.com/SkylarShao97)
- [@JaneShen](https://github.com/j232shen)

