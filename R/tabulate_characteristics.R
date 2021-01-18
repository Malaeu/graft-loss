##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param phts_all
tabulate_characteristics <- function(phts_all, labels) {

  tb1_vars <- 
    c("age_listing",
      "sex",
      "race",
      "hisp",
      "prim_dx",
      "hxsurg",
      "hxmed",
      "txecmo",
      "txnomcsd")
  
  # make this label more clear for table 1
  labels$variables$label[labels$variables$variable == 'sex'] <- 
    "F0 Male recipient"
  
  tbl_data_raw <- 
    phts_all %>%
    mutate(
      txpl_year = cut(
        x = txpl_year,
        breaks = c(0, 2013, 2016, 2019),
        labels = c("Before 2014", "2014 through 2016", "After 2017")
      )
    ) %>%
    select(txpl_year, !!!tb1_vars)

  overall <- tbl_data_raw %>%
    select(-txpl_year) %>%
    map_dfr(tb1_fun, .id = 'variable') %>%
    mutate(group = 'Overall')

  by_year <- tbl_data_raw %>%
    split(.$txpl_year) %>%
    map_dfr(
      ~ select(.x, -txpl_year) %>%
        map_dfr(tb1_fun, .id = 'variable'),
      .id = 'group'
    )

  tbl_data <- bind_rows(overall, by_year)

  tbl_data_wide <- tbl_data %>%
    pivot_wider(names_from = group, values_from = smry_value)

  tbl_inline <- tbl_data %>%
    mutate(
      group = str_replace_all(group, ' ', '_'),
      group = str_replace(group, '(^\\d)', 'From_\\1')
    ) %>%
    as_inline(tbl_variables = c('variable', 'group', 'level'),
              tbl_values = c('smry_value'))

  tbl_data_clean <- tbl_data_wide %>%
    add_count(variable) %>%
    mutate(
      variable_label = recode(variable, !!!deframe(labels$variables)),
      level = if_else(
        n <= 2,
        variable_label,
        level
      ),
      variable_label = if_else(
        variable_label == level,
        NA_character_,
        variable_label
      )
    ) %>%
    split(.$n == 2)

  tbl_data_clean$`TRUE` %<>%
    group_by(variable) %>%
    slice(2) %>%
    ungroup()

  tbl_data_clean %<>%
    bind_rows() %>%
    mutate(variable = factor(variable, levels = tb1_vars)) %>% 
    arrange(variable) %>% 
    select(-variable, -n)

  tbl_grouped_data <- tbl_data_clean %>%
    as_grouped_data(groups = 'variable_label') %>%
    remove_empty('rows')

  padding_index <- which(
    is.na(tbl_grouped_data$variable_label) &
      tbl_grouped_data$level %in% tbl_data$level
  )

  tbl_flex <- tbl_grouped_data %>%
    as_flextable(hide_grouplabel = TRUE)  %>%
    add_header_row(
      values = c('Variable', 'Overall', 'Transplant year'),
      colwidths = c(1, 1, 3)
    ) %>%
    theme_box() %>%
    align(align = 'center', part = 'all') %>%
    align(j = 1, align = 'left', part = 'body') %>%
    padding(i = padding_index,
            j = 1,
            padding.left = 15) %>%
    width(j = 1, width = 2.5) %>%
    width(j = c(2:5), width = 1.1) %>%
    set_header_labels(level = 'Variable') %>%
    merge_v(j = c(1,2), part = 'header') %>%
    font(fontname = 'Times New Roman', part = 'all') %>%
    fontsize(size = 12, part = 'all') %>%
    footnote(
      i = 1,
      j = 1,
      value = as_paragraph(
        'Table values are mean (standard deviation)',
        ' and percent for continuous and categorical',
        ' variables, respectively.'
      )
    )

  list(
    table = tbl_flex,
    inline = tbl_inline
  )


}
