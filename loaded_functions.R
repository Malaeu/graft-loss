# Сохраним текущее окружение
old_env <- ls()

# Создадим пустой список для хранения информации о функциях
func_info <- list()

# Загрузим файлы и соберем информацию
for (file in list.files("./R", full.names = TRUE)) {
  # Сохраним текущее окружение перед загрузкой файла
  pre_source_env <- ls()
  
  # Загрузим файл
  source(file)
  
  # Найдем новые объекты после загрузки файла
  new_objs <- setdiff(ls(), pre_source_env)
  
  # Отфильтруем только функции
  new_funcs <- new_objs[sapply(new_objs, function(x) is.function(get(x)))]
  
  # Добавим информацию о каждой новой функции в наш список
  for (func in new_funcs) {
    func_info[[func]] <- list(
      name = func,
      file = basename(file),
      code = deparse(get(func))
    )
  }
}

# Найдем все новые функции после загрузки всех файлов
all_new_funcs <- setdiff(ls(), old_env)
all_new_funcs <- all_new_funcs[sapply(all_new_funcs, function(x) is.function(get(x)))]

# Откроем файл для записи
con <- file("loaded_functions.txt", "w")

# Запишем информацию о каждой функции
for (i in seq_along(all_new_funcs)) {
  func <- all_new_funcs[i]
  if (func %in% names(func_info)) {
    info <- func_info[[func]]
    writeLines(paste0(i, ". Функция: ", info$name), con)
    writeLines(paste0("   Файл: ", info$file), con)
    writeLines("   Код:", con)
    writeLines(paste0("   ", info$code), con)
    writeLines("", con)  # Пустая строка для разделения функций
  }
}

# Закроем файл
close(con)

cat("Загружено функций:", length(all_new_funcs), "\n")
cat("Информация сохранена в файл 'loaded_functions.txt'\n")
