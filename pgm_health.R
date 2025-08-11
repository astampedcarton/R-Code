library(stringr)

# Read SAS file
sas_file <- "G:/My Drive/Pharma_Life/SAS/ADaM_Review.sas"
sas_code <- readLines(sas_file, warn = FALSE)

# Join into single string for multi-line regex
sas_text <- paste(sas_code, collapse = "\n")

# --- Pattern 1: Block comments /* ... */ ---
blkpat <- str_extract_all(
  sas_text,
  regex("/\\*.*?\\*/", dotall = TRUE, ignore_case = TRUE)
)[[1]]

# --- Pattern 2: Single-line or multi-line * ... ; comments ---
# Rule: * must be first non-space char on the line
linepat <- "(?m)^\\s*\\*[^;]*?;"
# This will only grab from * to ; on the same line.
# To allow multi-line * comments until first semicolon:
linepat_mult <- "(?ms)^\\s*\\*.*?;"

linecoms <- str_extract_all(
  sas_text,
  regex(linepat_mult, dotall = TRUE, ignore_case = TRUE))[[1]]

# --- Clean up: remove false positives like table var1*var2 / out=p;
# We'll keep * comments only if they start the line
linecoms <- linecoms[!grepl("^\\s*table\\s", linecoms, ignore.case = TRUE)]

# Combine
all_comments <- c(blkpat, linecoms)

# Count and display
comcnt <- length(all_comments)

cat("Found", comcnt, "comments\n")
print(all_comments)

# Remove block comments /* ... */
nblkcoms <- str_remove_all(
  sas_code,
  regex("/\\*.*?\\*/", dotall = TRUE, ignore_case = TRUE)
)

# Remove star-style comments (* ... ;)
# Rule: starts with * after optional spaces, until the first ;
nlinecoms <- str_remove_all(
  nblkcoms,
  regex("(?ms)^\\s*\\*.*?;", dotall = TRUE, ignore_case = TRUE)
)

#Count the nr of lines
pgmnline <- length(nlinecoms)
print(pgmnline)

#Count the nr of ;
semicnt <- sum(str_count(nlinecoms,";"))

print(semicnt)

# Count the nr of lines with 2 or more statements per line
cnt <- length(which(sas_code > 1))
print(cnt)

