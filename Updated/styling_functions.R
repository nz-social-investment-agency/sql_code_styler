################################################################################
# SQL code styling tool
# reference data
#
# Author: Daniel Contractor
# Updated: Simon Anastasiadis
################################################################################
# We use consistent input names:
# - sql_code is a single character string containing all lines of the file.
# - sql_content is a list containing sql_code and all the extracted/protected
#   components.
# 
# Uses code folding: Alt+O to collapse all.
################################################################################

## install required packages ---------------------------------------------- ----

# req_packages = c("stringr", "dplyr", "rstudioapi")
# for(pkg in req_packages){
#   if(pkg %in% installed.packages()){ next }
#   install.packages(pkg)
# }

## disassemble functions -------------------------------------------------- ----

comment_disassemble = function(sql_code) {
  stopifnot(is.character(sql_code))
  
  # Define the regex patterns to match comments including surrounding whitespace
  single_line_comment_pattern = "\\s*--.*(?:\\n|$)"  # Capture all leading/trailing spaces, newline is optional
  multi_line_comment_pattern = "(?s)\\s*/\\*.*?\\*/\\s*"  # Capture all leading/trailing spaces
  
  # Combine the patterns into one
  comment_pattern = paste(single_line_comment_pattern, multi_line_comment_pattern, sep = "|")
  
  # Initialize an empty list to store the comments
  sql_content = list()
  
  # Identify and process comments before replacement
  # Find all matches of comments in the SQL code
  matches = stringr::str_extract_all(sql_code, comment_pattern)[[1]]
  
  for(index in seq_along(matches)){
    # Create the placeholder string for the current index
    placeholder = sprintf("<><>c%d<><>", index)
    
    sql_content[[placeholder]] = matches[index]
    sql_code = suppressWarnings(
      stringr::str_replace(sql_code, stringr::fixed(matches[index]), placeholder)
    )
  }
  
  # Return the result
  sql_content$code = sql_code
  return(sql_content)
}

function_disassemble = function(sql_content) {
  stopifnot(is.list(sql_content))
  stopifnot("code" %in% names(sql_content))
  sql_code = sql_content$code
  stopifnot(is.character(sql_code))
  
  # Define the list of functions to be replaced
  sql_functions = ref_data$sql_functions
  
  # Step 1: Replace standard SQL functions with placeholders
  pattern = paste0("\\b(", paste(sql_functions, collapse = "|"), ")\\s*\\(([^()]*?)\\)")
  matches = stringr::str_extract_all(sql_code, pattern)[[1]]
  
  for(index in seq_along(matches)){
    # Extract only the bracketed content
    bracketed_content = stringr::str_extract(matches[index], "\\([^()]*?\\)")
    # skip if NA
    if (is.na(bracketed_content)) { next }
    # replace with placeholder
    placeholder = sprintf("<><>f%d<><>", index)
    
    # Store the function in the list with a key like f1, f2, etc.
    sql_content[[placeholder]] = bracketed_content
    sql_code = suppressWarnings(
      stringr::str_replace(sql_code, stringr::fixed(bracketed_content), placeholder)
    )
  }
  
  # Step 2: Find and replace balanced SQL functions with placeholders
  balanced_matches = find_balanced_brackets(sql_code, sql_functions)
  
  for(index2 in seq_along(balanced_matches)){
    # Create the placeholder string for the current index
    placeholder = sprintf("<><>f%d<><>", index + index2)
    
    sql_content[[placeholder]] = balanced_matches[index2]
    sql_code = suppressWarnings(
      stringr::str_replace(sql_code, stringr::fixed(balanced_matches[index2]), placeholder)
    )
  }
  
  # Return the result
  sql_content$code = sql_code
  return(sql_content)
}

blankline_disassemble = function(sql_code) {
  stopifnot(is.character(sql_code))
  
  # Define the regex pattern to match blank lines (\n followed by zero or more spaces and another \n)
  blank_line_pattern = "\n[ \t]*(?=\n)"
  
  # Apply the replacement function to the input SQL code
  sql_code = stringr::str_replace_all(sql_code, blank_line_pattern,"<><>b<><>")
  
  # Return the modified SQL code
  return(sql_code)
}

special_patterns_disassemble = function(sql_content){
  stopifnot(is.list(sql_content))
  stopifnot("code" %in% names(sql_content))
  sql_code = sql_content$code
  stopifnot(is.character(sql_code))
  
  # pattern list:
  pattern = c(
    # 1. Handle DATA_COMPRESSION: Capture everything between the previous and next newline (case-insensitive)
    "(?i)(?<=\\n)(.*?DATA_COMPRESSION.*?)(?=\\n)",
    # 2. Handle CREATE INDEX patterns: Capture up to the next closing bracket (case-insensitive)
    "(?i)\\bCREATE (?:CLUSTERED|NONCLUSTERED)? ?INDEX.*?\\)",
    # 3. Handle CASE ... WHEN ... END: Capture entire case block (case-insensitive)
    "(?i)CASE\\s+WHEN[\\s\\S]*?END",
    # 4. Handle anything between single quotes --> text strings
    "'(?:''|[^'])*'",
    # 5. Handle BETWEEN pattern
    " BETWEEN .* AND "
  )
  
  # add_newline
  add_newline = c(
    TRUE,
    TRUE,
    FALSE,
    FALSE,
    FALSE
  )
  
  # process patterns
  index = 0
  
  for(ii in seq_along(pattern)){
    matches = stringr::str_extract_all(sql_code, pattern[ii])[[1]]
    
    for(match in matches){
      index = index + 1
      placeholder = sprintf("<><>y%d<><>", index)
      sql_content[[placeholder]] = paste0(match, ifelse(add_newline[ii], "\n", ""))
      sql_code = suppressWarnings(
        stringr::str_replace(sql_code, stringr::fixed(match), placeholder)
      )
    }
  }
  
  # Return the modified SQL text and match list
  sql_content$code = sql_code
  return(sql_content)
}

## reassemble functions --------------------------------------------------- ----

reassemble_comments = function(sql_content) {
  stopifnot(is.list(sql_content))
  stopifnot("code" %in% names(sql_content))
  sql_code = sql_content$code
  stopifnot(is.character(sql_code))
  
  comments = sql_content[grep("^<><>c", names(sql_content))]
  
  # Search for all comment placeholders in the current SQL code (e.g., <<c1>>, <<c2>>)
  comment_placeholders = stringr::str_extract_all(sql_code, "<><>c\\d+<><>")[[1]]
  
  # Replace each comment placeholder with its corresponding comment from the list
  for (placeholder in comment_placeholders) {
    # Check if the comment exists in the list
    stopifnot(!is.null(comments[[placeholder]]))
    # Replace the placeholder in the SQL code with the corresponding comment
    sql_code = gsub(placeholder, comments[[placeholder]], sql_code, fixed = TRUE)
  }
  
  return(sql_code)
}

reassemble_functions = function(sql_content) {
  stopifnot(is.list(sql_content))
  stopifnot("code" %in% names(sql_content))
  sql_code = sql_content$code
  stopifnot(is.character(sql_code))
  
  functions = sql_content[grep("^<><>f", names(sql_content))]
  
  repeat {
    # Search for all function placeholders in the current SQL code (e.g., <><>f1<><>, <><>f2<><>)
    function_placeholders = stringr::str_extract_all(sql_code, "<><>f\\d+<><>")[[1]]
    
    # If no placeholders are found, exit the loop
    if (length(function_placeholders) == 0) { break }
    
    # Replace each function placeholder with its corresponding function
    for (placeholder in function_placeholders) {
      # Replace the placeholder in the SQL code
      sql_code = gsub(placeholder, functions[[placeholder]], sql_code, fixed = TRUE)
    }
  }
  
  return(sql_code)
}

restore_blank_lines = function(sql_code) {
  stopifnot(is.character(sql_code))
  
  # Define the regex pattern to match blank lines (\n followed by zero or more spaces and another \n)
  blank_line_pattern = "<><>b<><>"
  
  # Apply the replacement function to the input SQL code
  sql_code = stringr::str_replace_all(sql_code, blank_line_pattern, "\n")
  
  # Return the modified SQL code
  return(sql_code)
}

reassemble_special_patterns = function(sql_content) {
  stopifnot(is.list(sql_content))
  stopifnot("code" %in% names(sql_content))
  sql_code = sql_content$code
  stopifnot(is.character(sql_code))
  
  specials = sql_content[grep("^<><>y", names(sql_content))]
  
  repeat {
    # Search for all function placeholders in the current SQL code (e.g., <><>f1<><>, <><>f2<><>)
    special_placeholders = stringr::str_extract_all(sql_code, "<><>y\\d+<><>")[[1]]
    
    # If no placeholders are found, exit the loop
    if (length(special_placeholders) == 0) { break }
    
    # Replace each function placeholder with its corresponding function
    for (placeholder in special_placeholders) {
      # Replace the placeholder in the SQL code
      sql_code = gsub(placeholder, specials[[placeholder]], sql_code, fixed = TRUE)
    }
  }
  
  # Return the modified SQL text as the first element and the remaining placeholders as subsequent elements
  sql_content$code = sql_code
  return(sql_content)
}

## other functions -------------------------------------------------------- ----

drop_table_if_exists = function(sql_code) {
  stopifnot(is.character(sql_code))
  
  # Define the pattern for 'DROP TABLE' not followed by ' IF'
  pattern = "(?<!\\bDROP TABLE\\s)DROP TABLE(?!\\sIF\\b)"
  replacement = "DROP TABLE IF EXISTS"
  
  # Replace occurrences based on the pattern
  modified_sql = stringr::str_replace_all(sql_code, pattern, replacement)
  
  # Return the modified SQL code
  return(modified_sql)
}

capitalize_known_titles = function(sql_code) {
  stopifnot(is.character(sql_code))
  
  # Define the replacement rules as a named vector
  title_replacements = c(
    "dl-maa20" = "DL-MAA20",
    "idi_sandpit" = "IDI_Sandpit",
    "idi_clean" = "IDI_Clean",
    "\\bidi\\b" = "IDI",
    "\\bidi_" = "IDI_"
  )
  
  # Apply the replacement function to the input SQL code
  for(ii in seq_along(title_replacements)){
    this_tr = title_replacements[ii]
    sql_code = stringr::str_replace_all(sql_code, names(this_tr), this_tr)
  }
  
  # Return the modified SQL code
  return(sql_code)
}

remove_whitespace = function(sql_content) {
  stopifnot(is.list(sql_content))
  stopifnot("code" %in% names(sql_content))
  sql_code = sql_content$code
  stopifnot(is.character(sql_code))
  
  sql_code = stringr::str_trim(sql_code)
  sql_code = stringr::str_replace_all(sql_code, "\\s+", " ")
  sql_code = stringr::str_replace_all(sql_code, "\\s*=\\s*", "=")
  sql_code = stringr::str_replace_all(sql_code, "\\s*,\\s*", ",")
  sql_code = stringr::str_replace_all(sql_code, "\\s*\\(\\s*", "(")
  sql_code = stringr::str_replace_all(sql_code, "\\s*\\)\\s*", ")")
  sql_code = stringr::str_replace_all(sql_code, "\\s*>=\\s*", ">=")
  sql_code = stringr::str_replace_all(sql_code, "\\s*<=\\s*", "<=")
  sql_code = stringr::str_replace_all(sql_code, "(?<!<)\\s*>\\s*", ">") # Only replace if '>' is NOT preceded by '<'
  sql_code = stringr::str_replace_all(sql_code, "\\s*<\\s*(?!>)", "<")
  
  sql_content$code = sql_code
  return(sql_content)
}

restore_whitespace = function(sql_code) {
  stopifnot(is.character(sql_code))
  
  sql_code = stringr::str_replace_all(sql_code, "=", " = ")
  sql_code = stringr::str_replace_all(sql_code, ">=", " >= ")
  sql_code = stringr::str_replace_all(sql_code, "<=", " <= ")
  sql_code = stringr::str_replace_all(sql_code, "(?<=[^<])>", " > ")
  sql_code = stringr::str_replace_all(sql_code, "<(?=[^>])", " < ")
  sql_code = stringr::str_replace_all(sql_code, ">  =", ">=")
  sql_code = stringr::str_replace_all(sql_code, "<  =", "<=")
  sql_code = stringr::str_replace_all(sql_code, ",", ", ")
  
  return(sql_code)
}

find_balanced_brackets = function(text, sql_functions = c("SUM", "AVG", "MAX", "MIN", "COUNT")) {
  # Create a regex pattern for SQL function names followed by optional whitespace and an opening bracket
  sql_function_pattern = paste0("\\b(", paste(sql_functions, collapse = "|"), ")\\s*\\(")
  
  # Locate all occurrences of the SQL function pattern
  all_matches = stringr::str_locate_all(text, sql_function_pattern)[[1]]
  
  # No matching patterns found - exit early
  if(nrow(all_matches) == 0){ return(list()) }
  
  # Initialize a list to store matches
  matches = list()
  
  # Process each match to extract balanced brackets only
  for (row in seq_len(nrow(all_matches))) {
    start_pos = all_matches[row, 2] # Position of the opening bracket
    
    # Initialize a stack for bracket balancing
    stack = 0
    for (i in seq(start_pos, nchar(text))) {
      char = substr(text, i, i)
      if (char == "(") stack = stack + 1
      if (char == ")") stack = stack - 1
      if (stack == 0) {
        # Extract only the balanced bracket content
        full_match = substr(text, start_pos, i)
        matches = append(matches, list(full_match))
        break
      }
    }
  }
  return(matches)
}

remove_double_newlines = function(sql_code) {
  stopifnot(is.character(sql_code))
  
  # Define the regex pattern to match blank lines (\n followed by zero or more spaces and another \n)
  blank_line_pattern = "\n[ \t]*(?=\n)"
  
  # Apply the replacement function to the input SQL code
  sql_code = stringr::str_replace_all(sql_code, blank_line_pattern, "")
  
  # Return the modified SQL code
  return(sql_code)
}

standardize_sql = function(sql_code) {
  stopifnot(is.character(sql_code))
  
  sql_code = tolower(sql_code)
  sql_code = stringr::str_replace_all(sql_code, "\\s+", " ")
  sql_code = stringr::str_replace_all(sql_code, "\\s*=\\s*", "=")
  sql_code = stringr::str_replace_all(sql_code, "\\s*,\\s*", ",")
  sql_code = stringr::str_replace_all(sql_code, "\\s*;\\s*", ";")
  sql_code = stringr::str_replace_all(sql_code, "\\s*\\]\\s*", "\\]")
  sql_code = stringr::str_replace_all(sql_code, "\\s*\\[\\s*", "\\[")
  sql_code = stringr::str_replace_all(sql_code, "\\s*\\(\\s*", "(")
  sql_code = stringr::str_replace_all(sql_code, "\\s*\\)\\s*", ")")
  sql_code = stringr::str_replace_all(sql_code, "\\s*>=\\s*", ">=")
  sql_code = stringr::str_replace_all(sql_code, "\\s*<=\\s*", "<=")
  sql_code = stringr::str_replace_all(sql_code, "\\s*>\\s*", ">")
  sql_code = stringr::str_replace_all(sql_code, "\\s*<\\s*", "<")
  
  return(sql_code)
}

capitalize_sql_keywords = function(sql_content) {
  stopifnot(is.list(sql_content))
  stopifnot("code" %in% names(sql_content))
  sql_code = sql_content$code
  stopifnot(is.character(sql_code))
  
  sql_keywords = unique(unlist(ref_data))
  
  # Iterate over each keyword and replace it in the sql_code with its uppercase version
  for (keyword in sql_keywords) {
    # Create a case-insensitive pattern to match the keyword (using '\\b' to match whole word)
    pattern = paste0("\\b", keyword, "\\b")
    # Replace the keyword with its capitalized version
    sql_code = stringr::str_replace_all(sql_code, stringr::regex(pattern, ignore_case = TRUE), toupper(keyword))
  }
  
  return(sql_code)
}

calculate_indentation = function(sql_code) {
  stopifnot(is.character(sql_code))
  
  # Split the SQL code into lines
  lines = strsplit(sql_code, "\n")[[1]]
  
  # Trim whitespace from each line
  lines = stringr::str_trim(lines)
  
  # Create a data frame to store information for each line
  df = data.frame(
    line = lines,
    stringsAsFactors = FALSE
  )
  
  # Use mutate to add computed columns
  df = dplyr::mutate(
    df,
    brackets_open = stringr::str_count(line, "\\("),
    brackets_close = stringr::str_count(line, "\\)"),
    is_comma_open = substr(line, 1, 1) == ","  & !stringr::str_detect(dplyr::lag(line, 1,""), ","),
    is_comma_close = substr(dplyr::lag(line,1,""), 1, 1) == ","& !stringr::str_detect(line, ",")
  )
  df = dplyr::mutate(
    df,
    num_open = brackets_open + is_comma_open,
    num_close = brackets_close + is_comma_close,
    cumulative_open = cumsum(num_open),
    cumulative_close = cumsum(num_close),
    num_open_indentations = pmax(0,cumulative_open - cumulative_close - brackets_open),
    correct_indent = strrep(" ", num_open_indentations * 4),
    re_indent = paste0(correct_indent, line)
  )

  # print out data frame for debugging
  # write.csv(df, output_file.csv, row.names = FALSE)
  
  # Return the indented sql
  indented_sql = paste(df$re_indent, collapse = "\n")
  return(indented_sql)
}

insert_newlines_before_keywords_and_brackets = function(sql_content) {
  stopifnot(is.list(sql_content))
  stopifnot("code" %in% names(sql_content))
  sql_code = sql_content$code
  stopifnot(is.character(sql_code))
  
  # Add keywords not already present
  keywords_list = unique(c(ref_data$keywords_list, ref_data$new_keywords))
  
  # Create a pattern to match any of the keywords in the keywords_list
  # We need to make sure the keywords are word boundaries (e.g., no partial matches)
  keywords_pattern = paste0("\\b(", paste(keywords_list, collapse = "|"), ")\\b")
  
  # First, insert newline before the keywords
  sql_code = gsub(keywords_pattern, "\n\\1", sql_code, perl = TRUE)
  
  # Second, insert newline before the opening bracket '(' and closing bracket ')'
  # sql_code = remove_four_spaces(sql_code)
  sql_code = gsub("\\(", "(\n", sql_code)
  sql_code = gsub("\\)", "\n)", sql_code)
  sql_code = gsub("\\,", "\n,", sql_code)
  
  # remove_four_spaces= function(sql_code) {
  #   # Create a regex pattern that matches any of the keywords preceded by four spaces
  #   
  #   pattern = paste(keywords_list, collapse = "|" )
  #   # Replace the first occurrence of any keyword with the keyword alone (removing the four spaces)
  #   sql_code = gsub(paste0("\\(\n(\\b(", pattern, ")\\b)"), "(\\1", sql_code)
  #   return(sql_code)
  # }
  
  # Return the modified SQL code
  sql_content$code = sql_code
  return(sql_content)
}

## core execution --------------------------------------------------------- ----

style_files_interface = function() {
  
  ## get input path ----
  
  selection_type = rstudioapi::showQuestion(
    "Mode selection",
    "Style a single file of a whole folder?",
    ok = "File",
    cancel = "Folder"
    )
  selection_type = ifelse(selection_type, "File", "Folder")
  
  # get file
  if(selection_type == "File"){
    input_path = rstudioapi::selectFile(caption = "Select File to Be Styled")
  }
  
  # get folder
  if(selection_type == "Folder"){
    input_path = rstudioapi::selectDirectory(caption = "Select Folder Containing Files to Be Styled")
  }
  
  if(is.null(input_path)){
    cat("No input selected. Exiting\n")
    return(NULL)
  }
    
  ## output directory ----
  output_directory = rstudioapi::selectDirectory(
    caption = "Select Directory to Save Styled and Unstyled Files",
    path = dirname(input_path)
    )
  if(is.null(output_directory)){
    stop("No directory selected. Exiting.")
  }
  
  styled_folder = file.path(output_directory, "styled")
  unstyled_folder = file.path(output_directory, "unstyled")
  
  if (!dir.exists(styled_folder)){ dir.create(styled_folder) }
  if (!dir.exists(unstyled_folder)){ dir.create(unstyled_folder) }

  ## process file(s) ----
  
  # get contents of folder if in folder mode
  if(selection_type == "Folder"){
    input_path = list.files(input_path, full.names = TRUE)
  }
  
  # process all files
  for(ff in input_path){
    # skip empty files
    if(file.info(ff)$size == 0){
      cat("Skipping ", ff, " as file is empty\n")
      next
    }
    
    # copy unstyled file
    unstyled_file_path = file.path(unstyled_folder, basename(ff))
    file.copy(ff, unstyled_file_path, overwrite = TRUE)
    
    # copy and style file
    styled_file_path = file.path(styled_folder, paste0("styled_", basename(ff)))
    process_sql_files(ff, styled_file_path)
  }
  
  ## conclude ----
  cat("File styling complete\n")
  cat("Unstyled files saved at:", unstyled_folder, "\n")
  cat("Styled files saved at:", styled_folder, "\n")
}

process_sql_files = function(input_file, output_file){
  stopifnot(is.character(input_file), length(input_file) == 1)
  stopifnot(is.character(output_file), length(output_file) == 1)
  
  # exists
  if(!grepl("\\.sql$|\\.txt$", input_file)){
    msg = sprintf("Invalid file type: '%s'. Only .sql and .txt files are allowed.", basename(input_file))
    warning(msg)
    return(NULL)
  }
  stopifnot(file.exists(input_file))
  
  # Read the file content
  sql_content = readLines(input_file, warn = FALSE)
  stopifnot(is.character(sql_content))
  sql_content = paste(sql_content, collapse = "\n")
  
  # Run the provided script for processing
  sql_content = blankline_disassemble(sql_content)
  sql_content = comment_disassemble(sql_content)
  sql_content$code = capitalize_sql_keywords(sql_content)
  sql_content = function_disassemble(sql_content)
  sql_content = special_patterns_disassemble(sql_content)
  sql_content = remove_whitespace(sql_content)
  sql_content = insert_newlines_before_keywords_and_brackets(sql_content)
  
  sql_content$code = calculate_indentation(sql_content$code)
  
  sql_content$code = restore_whitespace(sql_content$code)
  sql_content = reassemble_special_patterns(sql_content)
  sql_content$code = reassemble_functions(sql_content)
  
  sql_content = reassemble_comments(sql_content)
  sql_content = remove_double_newlines(sql_content)
  sql_content = restore_blank_lines(sql_content)
  sql_content = capitalize_known_titles(sql_content)
  sql_content = drop_table_if_exists(sql_content)
  
  writeLines(sql_content, output_file)
}
