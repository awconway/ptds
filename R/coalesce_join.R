##' @title Coalesce Join
##' @description https://alistaire.rbind.io/blog/coalescing-joins/
##' @return
##' @export

#' @importFrom dplyr union coalesce bind_cols full_join
#' @importFrom purrr map_dfc
coalesce_join <- function(x, y,
                          by = NULL, suffix = c(".x", ".y"),
                          join = full_join, ...) {
  joined <- join(x, y, by = by, suffix = suffix, ...)
  # names of desired output
  cols <- union(names(x), names(y))

  to_coalesce <- names(joined)[!names(joined) %in% cols]
  suffix_used <- suffix[ifelse(endsWith(to_coalesce, suffix[1]), 1, 2)]
  # remove suffixes and deduplicate
  to_coalesce <- unique(substr(
    to_coalesce,
    1,
    nchar(to_coalesce) - nchar(suffix_used)
  ))

  coalesced <- map_dfc(to_coalesce, ~ coalesce(
    joined[[paste0(.x, suffix[1])]],
    joined[[paste0(.x, suffix[2])]]
  ))
  names(coalesced) <- to_coalesce

  bind_cols(joined, coalesced)[cols]
}
