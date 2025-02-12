ref_data=list(
  keywords_list = c("SELECT", "FROM", "WHERE", "INNER JOIN", "OUTER JOIN", 
                     "GROUP BY", "ORDER BY", "HAVING", "AND", "OR", 
                     "BULK", "UPDATE", "DELETE", "CREATE", "ALTER", 
                     "DROP", "TRUNCATE","WITH")  ,
  new_keywords = c( "ADD CONSTRAINT", "ANY", "ASC", "BACKUP DATABASE", 
                     "BETWEEN", "CHECK", "CONSTRAINT", 
                     "CREATE DATABASE", "CREATE INDEX", "CREATE OR REPLACE VIEW", 
                     "CREATE TABLE", "CREATE PROCEDURE", "CREATE UNIQUE INDEX", 
                     "CREATE VIEW", "DATABASE", "DEFAULT", 
                      "DROP CONSTRAINT", "DROP DATABASE", 
                     "DROP DEFAULT", "DROP INDEX", "DROP VIEW", "EXEC", 
                     "FOREIGN KEY", "FULL OUTER JOIN", "GO", "GROUP BY", 
                     "HAVING", "INSERT INTO", "INSERT INTO SELECT", 
                     "LEFT JOIN", "LIMIT", "PRIMARY KEY", 
                     "PROCEDURE", "RIGHT JOIN", "ROWNUM", "SELECT DISTINCT", 
                     "SELECT INTO", "SELECT TOP", "SET", "TOP", 
                     "TRUNCATE TABLE", "UNION", "UNIQUE", 
                     "VALUES", "VIEW","ON","REBUILD","JOIN","INTO") ,
  join_keywords = c(
    "JOIN", "LEFT JOIN", "RIGHT JOIN", "INNER JOIN", 
    "OUTER JOIN", "FULL JOIN", "LEFT OUTER JOIN", 
    "RIGHT OUTER JOIN", "CROSS JOIN", "NATURAL JOIN"
  ),
  sql_functions = c("ABS", "ASCII", "AVG", "CAST", "CASE", "CEIL", "CEILING", "CHAR", "CHARINDEX", 
                     "CONCAT", "CONVERT", "COUNT", "CURRENT_TIMESTAMP", "CURRENT_USER", "CURDATE", 
                     "DATEDIFF", "DATENAME", "DATEPART", "DATEADD", "DAY", "DATALENGTH", "EXTRACT", 
                     "FLOOR", "GETDATE", "GETUTCDATE", "IF", "ISDATE", "ISNULL", "ISNUMERIC", "LAG", 
                     "LEAD", "LEFT", "LEN", "LOWER", "LTRIM", "MAX", "MIN", "MOD", "MONTH", "NCHAR", 
                     "NOW", "NULLIF","OBJECT_ID", "PATINDEX", "POWER", "RAND", "REPLACE", "RIGHT", "ROUND", 
                     "RTRIM", "SESSION_USER", "SESSIONPROPERTY", "SIGN", "SPACE", "STR", "STUFF", 
                     "SUBSTRING", "SUM", "SYSTEM_USER", "TRIM", "TRY_CAST", "TRY_CONVERT","TOP", "UPPER", 
                     "USER_NAME", "YEAR","VARCHAR","DATEFROMPARTS","EOMONTH","in","IN","COALESCE","DATEADD","OVER","IIF","RANK","ROW_NUMBER")
  
)

###   ALL THE FUNCTIONS ARE WRITTEN BELOW:

library(stringr)

drop_table_if_exists <- function(sql_code) {
  
  stopifnot(is.character(sql_code))
  
  # Define the pattern for 'DROP TABLE' not followed by ' IF'
  pattern <- "(?<!\\bDROP TABLE\\s)DROP TABLE(?!\\sIF\\b)"
  replacement <- "DROP TABLE IF EXISTS"
  
  # Replace occurrences based on the pattern
  modified_sql <- str_replace_all(sql_code, pattern, replacement)
  
  # Return the modified SQL code
  return(modified_sql)
}

capitalize_known_titles <- function(sql_code) {
  
  stopifnot(is.character(sql_code))
  
  # Define the replacement rules as a named vector
  title_replacements <- c(
    
    "dl-maa2023-46" = "DL-MAA2023-46",
    "idi_sandpit" = "IDI_Sandpit",
    "idi_clean" = "IDI_Clean",
    "\\bidi\\b" = "IDI",
    "\\bidi_" = "IDI_"
  )
  
  # Function to apply the replacements to the text
  replace_titles <- function(text) {
    for (pattern in names(title_replacements)) {
      text <- str_replace_all(text, pattern, title_replacements[pattern])
    }
    return(text)
  }
  
  # Apply the replacement function to the input SQL code
  modified_sql <- replace_titles(sql_code)
  
  # Return the modified SQL code
  return(modified_sql)
}


remove_whitespace <- function(sql_content) {
  comment_list <- sql_content[-1]
  sql_content <- sql_content[[1]]
  stopifnot(is.character(sql_content))
  
  #sql_no_comments <- paste(sql_content, collapse = "\n")
  sql_standardized <- sql_content |>
    str_trim() |>
    str_replace_all("\\s+", " ") |>            
    str_replace_all("\\s*=\\s*", "=") |>       
    str_replace_all("\\s*,\\s*", ",") |>       
    str_replace_all("\\s*\\(\\s*", "(") |>     
    str_replace_all("\\s*\\)\\s*", ")")  |>
    str_replace_all("\\s*>=\\s*", ">=") |>
    str_replace_all("\\s*<=\\s*", "<=")|>
    str_replace_all("(?<!<)\\s*>\\s*", ">") |>  # Only replace if '>' is NOT preceded by '<'
    str_replace_all("\\s*<\\s*(?!>)", "<")
  
  
  sql_standardized <- c(list(sql_standardized), comment_list)
  
  return(sql_standardized)
}

restore_whitespace <- function(sql_content) {
  
  stopifnot(is.character(sql_content))
  
  #sql_no_comments <- paste(sql_content, collapse = "\n")
  sql_standardized <- sql_content |>
    
    str_replace_all("=", " = ") |>
    str_replace_all(">=", " >= ") |>
    str_replace_all("<=", " <= ")|>
    str_replace_all("(?<=[^<])>", " > ") |>
    str_replace_all("<(?=[^>])", " < ")|>
    str_replace_all(">  =", ">=")|>
    str_replace_all("<  =", "<=")|>
    str_replace_all(",", ", ")
  
  #singel space after commas
  return(sql_standardized)
  
}

comment_disassemble <- function(sql_code) {
  
  stopifnot(is.character(sql_code))
  
  # Define the regex patterns to match comments including surrounding whitespace
  single_line_comment_pattern <- "\\s*--.*(?:\\n|$)"  # Capture all leading/trailing spaces, newline is optional
  multi_line_comment_pattern <- "(?s)\\s*/\\*.*?\\*/\\s*"  # Capture all leading/trailing spaces
  
  # Combine the patterns into one
  comment_pattern <- paste(single_line_comment_pattern, multi_line_comment_pattern, sep = "|")
  
  # Initialize an empty list to store the comments
  comment_list <- list()
  
  # Set a variable to track the current index for placeholders
  current_index <- 1
  
  # Function to process and replace each comment
  replace_comments <- function(comment) {
    # Create the placeholder string for the current index
    placeholder <- sprintf("<><>c%d<><>", current_index)
    
    # Store the comment in the list with a key like c1, c2, etc.
    comment_list[[paste0("c", current_index)]] <<- comment
    
    # Increment the index for the next placeholder
    current_index <<- current_index + 1
    
    # Return the placeholder to replace the comment
    return(placeholder)
  }
  
  # Identify and process comments before replacement
  # Find all matches of comments in the SQL code
  matches <- str_extract_all(sql_code, comment_pattern)[[1]]
  
  # Replace all comments with placeholders
  modified_sql <- sql_code
  for (match in matches) {
    placeholder <- replace_comments(match) # Generate placeholder and store comment
    modified_sql <- str_replace(modified_sql, fixed(match), placeholder)
  }

  result <- c(list(modified_sql), comment_list)
  
  # Return the result
  return(result)
}

library(stringr)

find_balanced_brackets <- function(text, sql_functions = c("SUM", "AVG", "MAX", "MIN", "COUNT")) {
  # Create a regex pattern for SQL function names followed by optional whitespace and an opening bracket
  sql_function_pattern <- paste0("\\b(", paste(sql_functions, collapse = "|"), ")\\s*\\(")
  
  # Initialize a list to store matches
  matches <- list()
  
  # Locate all occurrences of the SQL function pattern
  all_matches <- str_locate_all(text, sql_function_pattern)[[1]]
  if (nrow(all_matches) == 0) return(matches) # No matching patterns found
  
  # Process each match to extract balanced brackets only
  for (row in seq_len(nrow(all_matches))) {
    start_pos <- all_matches[row, 2] # Position of the opening bracket
    
    # Initialize a stack for bracket balancing
    stack <- 0
    for (i in seq(start_pos, nchar(text))) {
      char <- substr(text, i, i)
      if (char == "(") stack <- stack + 1
      if (char == ")") stack <- stack - 1
      if (stack == 0) {
        # Extract only the balanced bracket content
        full_match <- substr(text, start_pos, i)
        matches <- append(matches, list(full_match))
        break
      }
    }
  }
  return(matches)
}

function_disassemble <- function(sql_list) {
  stopifnot(is.list(sql_list) && length(sql_list) >= 1 && is.character(sql_list[[1]]))
  
  # Extract SQL code from the first element of the list
  sql_code <- sql_list[[1]]
  
  # Define the list of functions to be replaced
  sql_functions <- ref_data$sql_functions
  
  # Initialize a list to store the functions with placeholders
  function_list <- list()
  
  # Set a variable to track the current index for placeholders
  current_index <- 1
  
  # Function to process and replace each function with a placeholder
  replace_function <- function(func_match) {
    # Create the placeholder string for the current index
    placeholder <- sprintf("<><>f%d<><>", current_index)
    
    # Store the function in the list with a key like f1, f2, etc.
    function_list[[paste0("f", current_index)]] <<- func_match
    
    # Increment the index for the next placeholder
    current_index <<- current_index + 1
    
    # Return the placeholder to replace the function
    return(placeholder)
  }
  
  # Function to find and replace standard SQL functions without nested functions
  replace_no_nested_functions <- function(code) {
    pattern <- paste0("\\b(", paste(sql_functions, collapse = "|"), ")\\s*\\(([^()]*?)\\)")
    matches <- str_extract_all(code, pattern)[[1]]
    for (match in matches) {
      # Extract only the bracketed content
      bracketed_content <- str_extract(match, "\\([^()]*?\\)")
      if (!is.na(bracketed_content)) {
        placeholder <- replace_function(bracketed_content) # Generate placeholder and store function
        code <- str_replace(code, fixed(bracketed_content), placeholder)
      }
    }
    return(code)
  }
  
  # Step 1: Replace standard SQL functions with placeholders
  modified_sql <- replace_no_nested_functions(sql_code)
  
  # Step 2: Find and replace balanced SQL functions with placeholders
  balanced_matches <- find_balanced_brackets(modified_sql, sql_functions)
  for (match in balanced_matches) {
    placeholder <- replace_function(match) # Generate placeholder and store function
    modified_sql <- str_replace(modified_sql, fixed(match), placeholder)
  }
  
  # Add the list of function placeholders after the comment placeholders
  # If the input list has comments, append them after the function placeholders
  comment_list <- sql_list[-1] # Extract comments (everything except the first element)
  final_result <- c(list(modified_sql), function_list, comment_list)
  
  # Return the result
  return(final_result)
}


special_patterns_disassemble <- function(sql_text) {
  # Initialize a list to store matches and placeholders
  sql_raw<-sql_text
  sql_text<-sql_text[[1]]
  match_list <- list()
  current_index <- 1

  # Helper function to replace matches with placeholders
  
  # Helper function to replace matches with placeholders, with optional newline
    replace_with_placeholder <- function(match, add_newline = TRUE) {
      placeholder <- sprintf("{}{}y%d{}{}", current_index)
      if (add_newline) {
        match_list[[paste0("y", current_index)]] <<- paste0("\n",match)  # Add newline to the match
      } else {
        match_list[[paste0("y", current_index)]] <<- match  # No newline for this match
      }
      current_index <<- current_index + 1
      return(placeholder)
    }

  # 1. Handle DATA_COMPRESSION: Capture everything between the previous and next newline (case-insensitive)
  pattern_data_compression <- "(?i)(?<=\\n)(.*?DATA_COMPRESSION.*?)(?=\\n)"
  sql_text <- str_replace_all(sql_text, pattern_data_compression, function(match) {
    replace_with_placeholder(match, add_newline = TRUE)
  })

  # 2. Handle CREATE INDEX patterns: Capture up to the next closing bracket (case-insensitive)
  pattern_create_index <- "(?i)\\bCREATE (?:CLUSTERED|NONCLUSTERED)? ?INDEX.*?\\)"
  sql_text <- str_replace_all(sql_text, pattern_create_index, function(match) {
    replace_with_placeholder(match, add_newline = TRUE)
  })

  # 3. Handle CASE ... WHEN ... END: Capture entire case block (case-insensitive)
  pattern_case_when <- "(?i)CASE\\s+WHEN[\\s\\S]*?END"
  #(?i)CASE\\s+WHEN\\s+.*?\\s+END"

  sql_text <- str_replace_all(sql_text, pattern_case_when, function(match) {
    replace_with_placeholder(match, add_newline = FALSE)
  })
  
  pattern_single_quotes <- "'(?:''|[^'])*'"
  sql_text <- str_replace_all(sql_text,pattern_single_quotes, function(match){
    replace_with_placeholder(match,add_newline=FALSE)
  })

  if(length(sql_raw)>1){
  for (i in 2:length(sql_raw)) {
    key <- names(sql_raw)[i]
    match_list[[key]] <- sql_raw[[i]]
  }
  }

  # Return the modified SQL text and match list
  result<- c(list(sql_text), match_list)
  return (result)
}


# draft_replace_special_patterns <- function(sql_text) {
#   # Initialize a list to store matches and placeholders
#   sql_raw <- sql_text
#   sql_text <- sql_text[[1]]
#   match_list <- list()
#   current_index <- 1
# 
#   # Helper function to replace matches with placeholders, with optional newline
#   replace_with_placeholder <- function(match, add_newline = TRUE) {
#     placeholder <- sprintf("{}{}y%d{}{}", current_index)
#     if (add_newline) {
#       match_list[[paste0("y", current_index)]] <<- paste0("\n",match)  # Add newline to the match
#     } else {
#       match_list[[paste0("y", current_index)]] <<- match  # No newline for this match
#     }
#     current_index <<- current_index + 1
#     return(placeholder)
#   }
# 
#   # 1. Handle DATA_COMPRESSION: Capture everything between the previous and next newline (case-insensitive)
#   pattern_data_compression <- "(?i)(?<=\\n)(.*?DATA_COMPRESSION.*?)(?=\\n)"
#   sql_text <- str_replace_all(sql_text, pattern_data_compression, function(match) {
#     replace_with_placeholder(match, add_newline = TRUE)
#   })
# 
#   # 2. Handle CREATE INDEX patterns: Capture up to the next closing bracket (case-insensitive)
#   pattern_create_index <- "(?i)\\bCREATE (?:CLUSTERED|NONCLUSTERED|) INDEX.*?\\)"
#   sql_text <- str_replace_all(sql_text, pattern_create_index, function(match) {
#     replace_with_placeholder(match, add_newline = TRUE)
#   })
# 
#   # 3. Handle CASE ... WHEN ... END: Capture entire case block (case-insensitive)
#   pattern_case_when <- "(?i)CASE\\s+WHEN[\\s\\S]*?END"
#   sql_text <- str_replace_all(sql_text, pattern_case_when, function(match) {
#     replace_with_placeholder(match, add_newline = FALSE)  # No newline for CASE WHEN
#   })
# 
#   # 4. Handle ALTER TABLE patterns: Capture entire ALTER TABLE statements (case-insensitive)
#   pattern_alter_table <- "(?i)ALTER TABLE\\s+[\\s\\S]*?;"
#   sql_text <- str_replace_all(sql_text, pattern_alter_table, function(match) {
#     replace_with_placeholder(match, add_newline = TRUE)
#   })
# 
#   # Include additional elements from the original input list
#   for (i in 2:length(sql_raw)) {
#     key <- names(sql_raw)[i]
#     match_list[[key]] <- paste0(sql_raw[[i]], "\n")  # Add newline to extra matches
#   }
# 
#   # Return the modified SQL text and match list
#   result <- c(list(sql_text), match_list)
#   return(result)
# }



blankline_disassemble <- function(sql_code) {
  
  stopifnot(is.character(sql_code))
  
  # Define the regex pattern to match blank lines (\n followed by zero or more spaces and another \n)
  blank_line_pattern <- "\n[ \t]*(?=\n)"
  
  # Function to replace each blank line with the specified pattern
  # replace_blank_lines <- function(text) {
  #   return(str_replace_all(text, blank_line_pattern, "{}{}b{}{}"))
  # }
  
  # Apply the replacement function to the input SQL code
  #modified_sql <- replace_blank_lines(sql_code)
  modified_sql<-str_replace_all(sql_code, blank_line_pattern,"<><>b<><>")
  
  # Return the modified SQL code
  return(modified_sql)
}


remove_double_newlines <- function(sql_code) {
  
  stopifnot(is.character(sql_code))
  
  # Define the regex pattern to match blank lines (\n followed by zero or more spaces and another \n)
  blank_line_pattern <- "\n[ \t]*(?=\n)"
  
  # Function to replace each blank line with the specified pattern
  # replace_blank_lines <- function(text) {
  #   return(str_replace_all(text, blank_line_pattern, ""))
  # }
  
  # Apply the replacement function to the input SQL code
  #modified_sql <- replace_blank_lines(sql_code)
  modified_sql<- str_replace_all(sql_code, blank_line_pattern, "")
  
  # Return the modified SQL code
  return(modified_sql)
}


restore_blank_lines <- function(sql_code) {
  
  stopifnot(is.character(sql_code))
  
  # Define the regex pattern to match blank lines (\n followed by zero or more spaces and another \n)
  blank_line_pattern <- "<><>b<><>"
  
  # Function to replace each blank line with the specified pattern
  # replace_blank_lines <- function(text) {
  #   return(str_replace_all(text, blank_line_pattern, "\n"))
  # }
  
  # Apply the replacement function to the input SQL code
  #modified_sql <- replace_blank_lines(sql_code)
  modified_sql <- str_replace_all(sql_code, blank_line_pattern, "\n")
  
  # Return the modified SQL code
  return(modified_sql)
}


library(dplyr)
library(writexl)  
library(stringr)

# Define the function that processes SQL code and writes the result to an Excel file
calculate_indentation <- function(sql_code) {
  stopifnot(is.character(sql_code))
  
  # Split the SQL code into lines
  lines <- strsplit(sql_code, "\n")[[1]]
  
  # Trim whitespace from each line
  lines <- str_trim(lines)
  
  # Create a data frame to store information for each line
  df <- data.frame(
    line = lines,
    text = lines,
    stringsAsFactors = FALSE
  )
  
 
  # Use mutate to add computed columns
  df <- df %>%
    mutate(
      brackets_open = str_count(line, "\\("),
      brackets_close = str_count(line, "\\)"),
      # is_comma_open = substr(line, 1, 1) == "," & (is.na(lag(line, 1)) | substr(lag(line, 1), 1, 1) != ","),
      # is_comma_close = substr(line, 1, 1) == "," & (is.na(lead(line, 1)) | substr(lead(line, 1), 1, 1) != ","),
      is_comma_open = substr(line, 1, 1) == ","  & !str_detect(lag(line, 1,""), ","),
      is_comma_close = substr(lag(line,1,""), 1, 1) == ","& !str_detect(line, ","),
      
      num_open = brackets_open + is_comma_open,
      num_close = brackets_close + is_comma_close,
      cumulative_open = cumsum(num_open),
      cumulative_close = cumsum(num_close),
      num_open_indentations = pmax(0,cumulative_open - cumulative_close-brackets_open),
      correct_indent = strrep(" ", num_open_indentations * 4),
      re_indent = paste0(correct_indent, text)
    )
  
  # Write the dataframe to an Excel file
  #write_xlsx(df, output_file)
  indented_sql <- paste(df$re_indent, collapse = "\n")
  # Return the dataframe
  return(indented_sql)
}


insert_newlines_before_keywords_and_brackets <- function(sql_code, keywords_list) {
  # Ensure sql_code is a character string
  stopifnot(is.list(sql_code)) # Ensure input is a list
  stopifnot(length(sql_code) >= 1) # Ensure the list is not empty
  
  # Check the first element is a string
  stopifnot(is.character(sql_code[[1]]), length(sql_code[[1]]) == 1) # Single string
  
  # Check other elements of the list
  if (length(sql_code) > 1) {
    other_elements <- sql_code[-1] # Exclude the first element
    #stopifnot(all(grepl("^[cf]", names(other_elements)))) # Names must start with 'c'
    stopifnot(all(sapply(other_elements, is.character))) # All elements must be strings
  }
  
  
  comment_list <- sql_code[-1]
  sql_code <- sql_code[[1]]
  
  stopifnot(is.character(sql_code))
  keywords_list <- ref_data$keywords_list
  # New keywords to add if not present
  new_keywords <- ref_data$new_keywords 
  
  # Add keywords not already present
  keywords_list <- union(keywords_list, new_keywords)
  # keywords_list <- tolower(keywords_list)
  
  # Create a pattern to match any of the keywords in the keywords_list
  # We need to make sure the keywords are word boundaries (e.g., no partial matches)
  keywords_pattern <- paste0("\\b(", paste(keywords_list, collapse = "|"), ")\\b")
  
  # Define the pattern for opening and closing brackets
  #brackets_pattern <- "[()]"
  
  # Combine the two patterns (keywords and brackets)
  #combined_pattern <- paste0("(", keywords_pattern, "|", brackets_pattern, ")")
  combined_pattern<-keywords_pattern
  # First, insert newline before the keywords
  modified_sql <- gsub(keywords_pattern, "\n\\1", sql_code, perl = TRUE)
  
  # Second, insert newline before the opening bracket '(' and closing bracket ')'
  remove_four_spaces<- function(code) {
    code_modified <- code
    
    # Create a regex pattern that matches any of the keywords preceded by four spaces
    
    pattern <-paste(keywords_list, collapse = "|" )
    # Replace the first occurrence of any keyword with the keyword alone (removing the four spaces)
    code_modified <- gsub(
      paste0("\\(\n(\\b(", pattern, ")\\b)"),
      "(\\1",
      modified_sql
    )
    return(code_modified)
  }
  
  modified_sql<-remove_four_spaces(modified_sql)
  modified_sql <- gsub("\\(", "(\n", modified_sql)
  modified_sql <- gsub("\\)", "\n)", modified_sql)
  modified_sql <- gsub("\\,", "\n,", modified_sql)
  #modified_sql <- gsub("\\)", "\n)", modified_sql)
  
  
  
  modified_sql <- c(list(modified_sql), comment_list)
  
  
  # Return the modified SQL code
  return(modified_sql)
}


reassemble_functions <- function(disassembled) {
  sql_with_placeholders <- disassembled[[1]]
  functions <- disassembled[grep("^f", names(disassembled))]
  
  stopifnot(is.character(sql_with_placeholders))
  stopifnot(is.list(functions))
  
  repeat {
    # Search for all function placeholders in the current SQL code (e.g., {}{}f1{}{}, {}{}f2{}{})
    function_placeholders <- str_extract_all(sql_with_placeholders, "<><>f\\d+<><>")[[1]]
    
    # If no placeholders are found, exit the loop
    if (length(function_placeholders) == 0) {
      break
    }
    
    # Replace each function placeholder with its corresponding function
    for (placeholder in function_placeholders) {
      # Extract the index from the placeholder (e.g., {}{}f1{}{} -> 1)
      index <- as.integer(sub("<><>f(\\d+)<><>", "\\1", placeholder))
      
      # Ensure the function exists in the list
      #stopifnot(!is.null(functions[[paste0("f", index)]]))
      
      # Get the replacement function
      function_to_insert <- functions[[paste0("f", index)]]
      
      # Replace the placeholder in the SQL code
      sql_with_placeholders <- gsub(placeholder, function_to_insert, sql_with_placeholders, fixed = TRUE)
    }
  }
  
  return(sql_with_placeholders)
}


reassemble_comments <- function(disassembled) {
  # Ensure inputs are valid
  sql_with_placeholders <- disassembled[[1]]
  comments <- disassembled[grep("^c", names(disassembled))]
  
  stopifnot(is.character(sql_with_placeholders))
  stopifnot(is.list(comments))
  
  # Search for all comment placeholders in the current SQL code (e.g., <<c1>>, <<c2>>)
  comment_placeholders <- str_extract_all(sql_with_placeholders, "<><>c\\d+<><>")[[1]]
  
  # Replace each comment placeholder with its corresponding comment from the list
  for (placeholder in comment_placeholders) {
    # Extract the index from the placeholder (e.g., <<c1>> -> 1)
    index <- as.integer(sub("<><>c(\\d+)<><>", "\\1", placeholder))
    
    # Check if the comment exists in the list
    stopifnot(!is.null(comments[[paste0("c", index)]]))
    
    # Construct the replacement comment from the list
    comment_to_insert <- comments[[paste0("c", index)]]
    
    # Replace the placeholder in the SQL code with the corresponding comment
    sql_with_placeholders <- gsub(placeholder, comment_to_insert, sql_with_placeholders, fixed = TRUE)
  }
  
  return(sql_with_placeholders)
}


# reassemble_special_patterns <- function(disassembled) {
#   # Ensure inputs are valid
#   sql_with_placeholders <- disassembled[[1]]  # Extract the SQL text
#   placeholders <- disassembled[-1]           # Extract all keys except the SQL text
#   
#   stopifnot(is.character(sql_with_placeholders))
#   stopifnot(is.list(placeholders))
#   
#   # Extract all placeholders in the SQL text
#   all_placeholders <- str_extract_all(sql_with_placeholders, "{}{}\\w\\d+{}{}")[[1]]
#   
#   # Initialize list for remaining placeholders
#   remaining_placeholders <- list()
#   
#   # Loop through all placeholders in the SQL text
#   for (placeholder in all_placeholders) {
#     # Determine the type of placeholder (e.g., y, c, f)
#     type <- sub("{}{}(\\w)\\d+{}{}", "\\1", placeholder)
#     index <- sub("{}{}\\w(\\d+){}{}", "\\1", placeholder)
#     key <- paste0(type, index)
#     
#     # Replace placeholders of type 'y' if they exist in the list
#     if (type == "y" && !is.null(placeholders[[key]])) {
#       replacement <- placeholders[[key]]
#       sql_with_placeholders <- gsub(placeholder, replacement, sql_with_placeholders, fixed = TRUE)
#     } 
#   }
#   
#   # Return the modified SQL text as the first element and the remaining placeholders as subsequent elements
#   result <- c(list(sql_with_placeholders), remaining_placeholders)
#   return(result)
# }

reassemble_special_patterns <- function(disassembled) {
  # Ensure inputs are valid
  sql_with_placeholders <- disassembled[[1]]  # Extract the SQL text
  placeholders <- disassembled[-1]           # Extract all keys except the SQL text
  
  stopifnot(is.character(sql_with_placeholders))
  stopifnot(is.list(placeholders))
  
  # Extract all placeholders in the SQL text
  all_placeholders <- str_extract_all(sql_with_placeholders, "\\{\\}\\{\\}\\w\\d+\\{\\}\\{\\}")[[1]]
  
  # Initialize list for remaining placeholders
  remaining_placeholders <- placeholders
  
  # Loop through all placeholders in the SQL text
  for (placeholder in all_placeholders) {
    # Determine the type of placeholder (e.g., y, c, f)
    type <- sub("\\{\\}\\{\\}(\\w)\\d+\\{\\}\\{\\}", "\\1", placeholder)
    index <- sub("\\{\\}\\{\\}\\w(\\d+)\\{\\}\\{\\}", "\\1", placeholder)
    key <- paste0(type, index)
    
    # Replace placeholders of type 'y' if they exist in the list
    if (type == "y" && !is.null(placeholders[[key]])) {
      replacement <- placeholders[[key]]
      sql_with_placeholders <- gsub(placeholder, replacement, sql_with_placeholders, fixed = TRUE)
      remaining_placeholders[[key]] <- NULL  # Remove the replaced 'y' placeholder
    }
  }
  
  # Return the modified SQL text as the first element and the remaining placeholders as subsequent elements
  result <- c(list(sql_with_placeholders), remaining_placeholders)
  return(result)
}


# else {
#   # Add other placeholders (e.g., c1, f1) to the remaining list
#   remaining_placeholders[[key]] <- placeholders[[key]]
# }

capitalize_sql_keywords <- function(sql_code) {
  # Define a vector of SQL keywords
  stopifnot(is.list(sql_code))
  sql_code=sql_code[[1]]
  
  
  sql_keywords <- c("PARTITION","TABLE","SELECT", "FROM", "WHERE", "JOIN", "ON", "AND", "OR", "INSERT", 
                    "UPDATE", "DELETE", "CREATE", "ALTER", "DROP", "IN", "NOT", "LIKE", 
                    "IS", "NULL", "DISTINCT", "GROUP", "BY", "ORDER", "HAVING", "LIMIT", 
                    "OFFSET", "UNION", "INTERSECT", "EXCEPT", "ALL", "AS", "SET", 
                    "VALUES", "WITH", "CASE", "WHEN", "THEN", "ELSE", "END", "ASC", 
                    "DESC", "BETWEEN", "INTO", "OUTER", "INNER", "LEFT", "RIGHT", "FULL", 
                    "ISNULL", "CAST", "CONVERT", "EXISTS", "AND", "OR", "LIKE", "ILIKE","in", "ABS", "ASCII", "AVG", "CAST", "CASE", "CEIL", "CEILING", "CHAR", "CHARINDEX", 
                                                                                                                 "CONCAT", "CONVERT", "COUNT", "CURRENT_TIMESTAMP", "CURRENT_USER", "CURDATE", 
                                                                                                                 "DATEDIFF", "DATENAME", "DATEPART", "DATEADD", "DAY", "DATALENGTH", "EXTRACT", 
                                                                                                                 "FLOOR", "GETDATE", "GETUTCDATE", "IF", "ISDATE", "ISNULL", "ISNUMERIC", "LAG", 
                                                                                                                 "LEAD", "LEFT", "LEN", "LOWER", "LTRIM", "MAX", "MIN", "MOD", "MONTH", "NCHAR", 
                                                                                                                 "NOW", "NULLIF", "PATINDEX", "POWER", "RAND", "REPLACE", "RIGHT", "ROUND", 
                                                                                                                 "RTRIM", "SESSION_USER", "SESSIONPROPERTY", "SIGN", "SPACE", "STR", "STUFF", 
                                                                                                                 "SUBSTRING", "SUM", "SYSTEM_USER", "TRIM", "TRY_CAST", "TRY_CONVERT", "UPPER", 
                                                                                                                 "USER_NAME", "YEAR","VARCHAR","DATEFROMPARTS","EOMONTH","in","IN","COALESCE","JOIN","DATEADD")
  
  # sql_keywords<- union(sql_keywords, ref_data$sql_functions)
  sql_funcs <- ref_data$sql_functions
  sql_keywords<- union(sql_keywords, sql_funcs)
  # Iterate over each keyword and replace it in the sql_code with its uppercase version
  for (keyword in sql_keywords) {
    # Create a case-insensitive pattern to match the keyword (using '\\b' to match whole word)
    pattern <- paste0("\\b", keyword, "\\b")
    
    # Replace the keyword with its capitalized version
    sql_code <- str_replace_all(sql_code, regex(pattern, ignore_case = TRUE), toupper(keyword))
  }
  
  return(sql_code)
}



process_sql_files <- function(filenames, project_folders,outputpath) {
  # Check that the lengths of filenames and project_folders match
  # if (length(filenames) != length(project_folders)) {
  #   stop("The number of filenames does not match the number of project folders.")
  # }
  
  # Iterate over each file and its corresponding project folder
  for (i in seq_along(filenames)) {
    # Construct the full file path by combining the project and subfolder paths
    input_file <- project_folders
    #file.path(project_folders[i], filenames[i])

    # Validate the input file
    stopifnot(is.character(input_file), length(input_file) == 1)
    if (!grepl("\\.sql$|\\.txt$", input_file)) {
      stop(sprintf("Invalid file type: %s. Only .sql and .txt files are allowed.", input_file))
    }
    if (!file.exists(input_file)) {
      stop(sprintf("File does not exist: %s", input_file))
    }


    # Read the file content
    sql_content <- readLines(input_file, warn = FALSE)
    if (!is.character(sql_content)) {
      stop("The content of the file is not a string. Ensure the file contains valid text.")
    }
    sql_string <- paste(sql_content, collapse = "\n")
    sql_input <- sql_string
    
    stopifnot(is.character(sql_input))

    # Run the provided script for processing
    sql_string<-blankline_disassemble(sql_string)
    sql_string <- comment_disassemble(sql_string)
    sql_string[[1]] <- capitalize_sql_keywords(sql_string)
    sql_string <- function_disassemble(sql_string)
    sql_string<- special_patterns_disassemble(sql_string)
    sql_string <- remove_whitespace(sql_string)
    sql_string <- insert_newlines_before_keywords_and_brackets(sql_string)
    
    sql_string[[1]] <- calculate_indentation(sql_string[[1]])
    # sql_string[[1]] <- capitalize_sql_keywords(sql_string)
    
    sql_string[[1]]<-restore_whitespace(sql_string[[1]])
    sql_string<-reassemble_special_patterns(sql_string)
    sql_string[[1]]<-reassemble_functions(sql_string)
    
    sql_string<-reassemble_comments(sql_string)
    sql_string<-remove_double_newlines(sql_string)
    sql_string<-restore_blank_lines(sql_string)
    sql_string<-capitalize_known_titles(sql_string)
    sql_string<-drop_table_if_exists(sql_string)
    # standardised_result <- standardize_sql(sql_string)
    # standardised_input <- standardize_sql(sql_input)
    # standardised_input<-trimws(standardised_input)
    # standardised_result<-trimws(standardised_result)
    
    
    
    #stopifnot(standardised_input == standardised_result)
    
    
    is_ascii <-function(x) all(charToRaw(x)<=127) && !any(grepl("@",x))
    
    if ( is_ascii(sql_string) & is_ascii(sql_input)  ){
      standardised_result <- standardize_sql(sql_string)
      standardised_input <- standardize_sql(sql_input)
      standardised_input<-trimws(standardised_input)
      standardised_result<-trimws(standardised_result)
      #stopifnot(standardised_input==standardised_result)

      if(standardised_input!=standardised_result)
      {
        #cat(sprintf("Validation failed for file: %s\n",filenames[i])   )


        }
    }
    
    
    
    output_file <-outputpath 

    
    writeLines(sql_string, output_file)

    
    # paths <- "I:\\MAA2023-46\\styling_toolV2\\testresult"
    # paths2 <- "I:\\MAA2023-46\\styling_toolV2\\testoutput"
    # # 
    # writeLines(standardised_result , paths)
    # # 
    # writeLines(standardised_input , paths2)
  }
}

library(rstudioapi)

style_files_interface <- function() {
  # Function to style a file
  style_file <- function(input_file, styled_folder, unstyled_folder) {
    if (file.info(input_file)$size == 0) {
      stop(sprintf("File is empty: %s", input_file))
    }
    if (!grepl("\\.sql$|\\.txt$", input_file)) {
      stop(sprintf("Invalid file type: %s. Only .sql and .txt files are allowed.", input_file))
    }
    
    unstyled_file_path <- file.path(unstyled_folder, basename(input_file))
    file.copy(input_file, unstyled_file_path, overwrite = TRUE)
    
    styled_file_path <- file.path(styled_folder, paste0("styled_", basename(input_file)))
    process_sql_files(input_file, unstyled_file_path, styled_file_path)
    
    styled_file_path
  }
  
  # Function to style files in a folder
  style_folder <- function(input_folder, styled_folder, unstyled_folder) {
    files <- list.files(input_folder, full.names = TRUE)
    lapply(files, function(file) style_file(file, styled_folder, unstyled_folder))
  }
  
  # Ensure the 'styled' and 'unstyled' folders exist
  ensure_subfolders <- function(base_folder) {
    styled_folder <- file.path(base_folder, "styled")
    unstyled_folder <- file.path(base_folder, "unstyled")
    
    if (!dir.exists(styled_folder)) dir.create(styled_folder)
    if (!dir.exists(unstyled_folder)) dir.create(unstyled_folder)
    
    list(styled_folder = styled_folder, unstyled_folder = unstyled_folder)
  }
  
  # Function to handle file or folder selection
  get_input_path <- function() {
    selection_type <- askYesNo("Do you want to style a single file? (No means a folder will be selected)")
    
    input_path <- if (selection_type) {
      selectFile(
        caption = "Select File to Be Styled",
        label = "Select",
        path = getActiveProject(),
        filter = "All Files (*)",
        existing = TRUE
      )
    } else {
      selectDirectory(
        caption = "Select Folder Containing Files to Be Styled",
        label = "Select",
        path = getActiveProject()
      )
    }
    
    if (is.null(input_path)) {
      cat(if (selection_type) "No file selected. Exiting.\n" else "No folder selected. Exiting.\n")
      return(NULL)
    }
    
    input_path
  }
  
  # Main logic
  input_path <- get_input_path()
  if (is.null(input_path)) return()
  
  output_directory <- selectDirectory(
    caption = "Select Directory to Save Styled and Unstyled Files",
    label = "Select",
    path = getActiveProject()
  )
  
  if (is.null(output_directory)) {
    stop("No directory selected. Exiting.")
  }
  
  subfolders <- ensure_subfolders(output_directory)
  
  if (file.info(input_path)$isdir) {
    style_folder(input_path, subfolders$styled_folder, subfolders$unstyled_folder)
  } else {
    style_file(input_path, subfolders$styled_folder, subfolders$unstyled_folder)
  }
  
  cat("File(s) styled successfully!\nUnstyled files saved at:", subfolders$unstyled_folder, 
      "\nStyled files saved at:", subfolders$styled_folder, "\n")
}




delete_files_interface <- function() {
  
  # Function to delete a file
  delete_file <- function(input_file, delete_folder) {
    stop_if_invalid_file(input_file)
    
    # Define the path of the deleted file
    deleted_file_path <- file.path(delete_folder, basename(input_file))
    
    # Delete the file (replace with actual delete operation)
    delete_after_match(input_file, deleted_file_path)
    
    deleted_file_path
  }
  
  # Helper function to check if the file is valid
  stop_if_invalid_file <- function(input_file) {
    if (!file.exists(input_file)) {
      stop(sprintf("File does not exist: %s", input_file))
    }
    if (file.info(input_file)$size == 0) {
      stop(sprintf("File is empty: %s", input_file))
    }
    if (!grepl("\\.sql$|\\.txt$", input_file)) {
      stop(sprintf("Invalid file type: %s. Only .sql and .txt files are allowed.", input_file))
    }
  }
  
  # Function to delete files in a folder
  delete_folder <- function(input_folder, delete_folder) {
    files <- list.files(input_folder, full.names = TRUE)
    deleted_files <- lapply(files, function(file) delete_file(file, delete_folder))
    deleted_files
  }
  
  # Ensure the 'delete' folder exists
  ensure_subfolders <- function(base_folder) {
    delete_folder <- file.path(base_folder, "deleted")
    
    if (!dir.exists(delete_folder)) dir.create(delete_folder)
    
    delete_folder
  }
  
  # Main logic
  selection_type <- askYesNo("Do you want to delete a single file? (No means a folder will be selected)")
  
  if (!selection_type) {  # Folder selected
    input_folder <- selectDirectory(
      caption = "Select Folder Containing Files to Be Deleted",
      label = "Select",
      path = getActiveProject()
    )
    
    if (is.null(input_folder)) {
      cat("No folder selected. Exiting.\n")
      return()
    }
    
    output_directory <- selectDirectory(
      caption = "Select Directory to Save Deleted Files",
      label = "Select",
      path = getActiveProject()
    )
    
    if (is.null(output_directory)) {
      stop("No directory selected. Exiting.")
    }
    
    delete_folder_path <- ensure_subfolders(output_directory)
    delete_folder(input_folder, delete_folder_path)
    cat("Files deleted successfully!\nDeleted files saved at:", delete_folder_path, "\n")
    return()
  }
  
  # File selected
  input_file <- selectFile(
    caption = "Select File to Be Deleted",
    label = "Select",
    path = getActiveProject(),
    filter = "All Files (*)",
    existing = TRUE
  )
  
  if (is.null(input_file)) {
    cat("No file selected. Exiting.\n")
    return()
  }
  
  output_directory <- selectDirectory(
    caption = "Select Directory to Save Deleted Files",
    label = "Select",
    path = getActiveProject()
  )
  
  if (is.null(output_directory)) {
    stop("No directory selected. Exiting.")
  }
  
  delete_folder_path <- ensure_subfolders(output_directory)
  result_path <- delete_file(input_file, delete_folder_path)
  cat("File deleted successfully!\nDeleted file saved at:", result_path, "\n")
}



standardize_sql <- function(sql_content) {
  
  stopifnot(is.character(sql_content))
  
  #sql_no_comments <- paste(sql_content, collapse = "\n")
  sql_standardized <- sql_content |>
    tolower() |>
    str_replace_all("\\s+", " ") |>            
    str_replace_all("\\s*=\\s*", "=") |>       
    str_replace_all("\\s*,\\s*", ",") |> 
    str_replace_all("\\s*;\\s*", ";") |>
    str_replace_all("\\s*\\]\\s*", "\\]") |>
    str_replace_all("\\s*\\[\\s*", "\\[") |>
    str_replace_all("\\s*\\(\\s*", "(") |>     
    str_replace_all("\\s*\\)\\s*", ")")  |>
    str_replace_all("\\s*>=\\s*", ">=") |>
    str_replace_all("\\s*<=\\s*", "<=")|>
    str_replace_all("\\s*>\\s*", ">") |>
    str_replace_all("\\s*<\\s*", "<")
  
  
  return(sql_standardized)
}


