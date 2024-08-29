library(tidyverse)

analyze_file <- function(file_path) {
  content <- readLines(file_path, warn = FALSE)
  list(
    libraries = str_extract(content, "^library\\([^)]+\\)") %>% na.omit(),
    sources = str_extract(content, "^source\\([^)]+\\)") %>% na.omit(),
    functions = str_extract(content, "^[\\w.]+\\s*<-\\s*function\\(") %>% 
      na.omit() %>% 
      str_remove("\\s*<-\\s*function\\("),
    content = content
  )
}

r_files <- list.files(pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
analyses <- r_files %>%
  set_names() %>%
  map(safely(analyze_file)) %>%
  transpose()

if (any(!map_lgl(analyses$error, is.null))) {
  cat("Ошибки анализа файлов:\n")
  walk2(names(analyses$error), analyses$error, ~{
    if (!is.null(.y)) cat(.x, ":", .y$message, "\n")
  })
}

analyses <- analyses$result %>% compact()

main_file <- analyses %>%
  map_int(~length(.$sources)) %>%
  which.max() %>%
  names()

file_conn <- file("project_analysis.txt", "w")

writeLines("Структура проекта:", file_conn)
dir(recursive = TRUE) %>% writeLines(file_conn)
writeLines("\n", file_conn)

writeLines(paste("Главный файл:", main_file), file_conn)
writeLines("Содержимое главного файла:", file_conn)
analyses[[main_file]]$content %>% writeLines(file_conn)
writeLines("\n", file_conn)

writeLines("Зависимости проекта:", file_conn)
analyses %>%
  map(~.$libraries) %>%
  unlist() %>%
  unique() %>%
  writeLines(file_conn)
writeLines("\n", file_conn)

writeLines("Функции в проекте:", file_conn)
analyses %>%
  imap(~{
    if (length(.x$functions) > 0) {
      c(paste("Файл:", .y),
        paste("  ", .x$functions),
        "")
    }
  }) %>%
  compact() %>%
  flatten_chr() %>%
  writeLines(file_conn)

writeLines("\nГраф зависимостей файлов:", file_conn)
analyses %>%
  imap(~{
    if (length(.x$sources) > 0) {
      sources <- str_extract(.x$sources, "'[^']+'|\"[^\"]+\"") %>%
        str_remove_all("^['\"]+|['\"]+$") %>%
        basename()
      paste0(basename(.y), " -> ", paste(sources, collapse = ", "))
    }
  }) %>%
  compact() %>%
  unlist() %>%
  writeLines(file_conn)

close(file_conn)

cat("Анализ проекта сохранен в файл 'project_analysis.txt'\n")
