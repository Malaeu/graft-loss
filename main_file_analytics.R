library(tidyverse)

# Функция для рекурсивного вывода структуры директории
print_dir_structure <- function(path, prefix = "") {
  files <- list.files(path, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  files <- files[!grepl("^\\.", basename(files))]  # Исключаем файлы, начинающиеся с точки
  
  for (file in files) {
    if (file.info(file)$isdir) {
      cat(prefix, basename(file), "/\n", sep = "")
      print_dir_structure(file, paste0(prefix, "  "))
    } else {
      cat(prefix, basename(file), "\n", sep = "")
    }
  }
}

# Функция для анализа зависимостей и содержимого файла
analyze_file <- function(file_path) {
  content <- readLines(file_path)
  
  # Находим все library() вызовы
  libraries <- content[grep("^\\s*library\\(", content)]
  libraries <- gsub("^\\s*library\\(|\\)\\s*$", "", libraries)
  
  # Находим все source() вызовы
  sources <- content[grep("^\\s*source\\(", content)]
  sources <- gsub("^\\s*source\\(|\\)\\s*$", "", sources)
  sources <- gsub("[\"\']", "", sources)
  
  # Находим все функции
  funcs <- content[grep("^\\s*[\\w.]+\\s*<-\\s*function\\(", content)]
  funcs <- gsub("\\s*<-\\s*function.*", "", funcs)
  funcs <- trimws(funcs)
  
  list(
    libraries = libraries,
    sources = sources,
    functions = funcs,
    content = content
  )
}

# Анализируем все R файлы в проекте
r_files <- list.files(pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
analyses <- lapply(r_files, analyze_file)
names(analyses) <- r_files

# Находим файл с наибольшим количеством source() вызовов
main_file <- names(analyses)[which.max(sapply(analyses, function(x) length(x$sources)))]

# Открываем файл для записи
con <- file("project_analysis.txt", "w")

# Записываем структуру проекта
writeLines("Структура проекта:", con)
capture.output(print_dir_structure("."), file = con, append = TRUE)
writeLines("\n", con)

# Записываем информацию о главном файле
writeLines(paste("Предполагаемый главный файл:", main_file), con)
writeLines("Содержимое главного файла:", con)
writeLines(analyses[[main_file]]$content, con)
writeLines("\n", con)

# Записываем информацию о зависимостях
writeLines("Зависимости проекта:", con)
all_libs <- unique(unlist(lapply(analyses, function(x) x$libraries)))
writeLines(paste("Библиотеки:", paste(all_libs, collapse = ", ")), con)
writeLines("\n", con)

# Записываем информацию о функциях
writeLines("Функции в проекте:", con)
for (file in names(analyses)) {
  funcs <- analyses[[file]]$functions
  if (length(funcs) > 0) {
    writeLines(paste("Файл:", file), con)
    writeLines(paste("  ", funcs), con)
  }
}

# Создаем граф зависимостей
writeLines("\nГраф зависимостей файлов:", con)
for (file in names(analyses)) {
  sources <- analyses[[file]]$sources
  if (length(sources) > 0) {
    writeLines(paste0(basename(file), " -> ", paste(basename(sources), collapse = ", ")), con)
  }
}

# Закрываем файл
close(con)

cat("Анализ проекта сохранен в файл 'project_analysis.txt'\n")

# Функция для подготовки датасета
prepare_dataset <- function(dataset_path) {
  # Загружаем датасет
  dataset <- read_csv(dataset_path)
  
  # Здесь можно добавить код для предобработки датасета
  # Например:
  # dataset <- dataset %>%
  #   filter(!is.na(important_column)) %>%
  #   mutate(new_column = some_calculation)
  
  return(dataset)
}

# Функция для выполнения анализа
run_analysis <- function(dataset) {
  # Здесь нужно добавить код для выполнения анализа
  # Например:
  # result <- dataset %>%
  #   group_by(some_column) %>%
  #   summarize(mean_value = mean(another_column))
  
  # return(result)
}

# Основная функция
main <- function(dataset_path) {
  dataset <- prepare_dataset(dataset_path)
  result <- run_analysis(dataset)
  
  # Сохраняем результат
  write_csv(result, "analysis_result.csv")
  
  cat("Анализ завершен. Результаты сохранены в 'analysis_result.csv'\n")
}

# Пример использования:
# main("path/to/your/dataset.csv")
