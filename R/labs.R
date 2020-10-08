relevel_labs <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all(
      pattern = ".*indeterminate.*",
      replacement = "I"
    ) %>%
    stringr::str_replace_all(
      pattern = "negative",
      replacement = "N"
    ) %>%
    stringr::str_replace_all(
      pattern = ".*presumptive.*",
      replacement = "P"
    ) %>%
    stringr::str_replace_all(
      pattern = ".*positive.*",
      replacement = "C"
    ) %>%
    forcats::as_factor() %T>%
    {message("Levels collapsed to 'U' in relevel_labs:")} %T>%
    {
      levels(.) %>%
        {.[!. %in% c("C", "P", "N", "I")]} %>%
        glue::glue_collapse(sep = "\t") %>%
        message()
    } %>%
    forcats::fct_expand("S") %>%
    forcats::fct_collapse(
      C = "C",
      P = "P",
      N = "N",
      I = "I",
      S = "S",
      other_level = "U"
    ) %>%
    {if (!"U" %in% levels(.)) forcats::fct_expand(., "U") else .} %>%
    forcats::fct_relevel("C", "P", "N", "I", "S", "U")
}
