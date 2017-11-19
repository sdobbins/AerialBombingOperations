# @author Scott Dobbins
# @version 0.9.9.6
# @date 2017-11-19 01:00


### Statistics --------------------------------------------------------------

tabulate_factor <- function(fact) {
  return (tabulate(fact, nbins = nlevels(fact)))
}

mode_factor <- function(fact) {
  return (levels(fact)[which.max(tabulate_factor(fact))])
}

mode_and_others_factor <- function(fact) {
  all_levels <- levels(fact)
  tabs <- tabulate_factor(fact)
  present_levels <- all_levels[tabs > 0L]
  mode_level <- all_levels[which.max(tabs)]
  non_mode_levels <- present_levels %d% mode_level
  return (list("mode" = mode_level, "others" = non_mode_levels))
}


### Information -------------------------------------------------------------

level_proportions <- function(column, na.rm = FALSE) {
  assert_that(is.factor(column), 
              msg = "You supplied a non-factor vector/column to a factor-only method")
  tab_counts <- tabulate_factor(column)
  if (na.rm && (sum(tab_counts == 0L) > 0L)) {
    tab_counts <- tab_counts %[!=]% 0
  }
  tab_total <- sum(tab_counts)
  return (tab_counts / tab_total)
}

missing_levels <- function(fact) {
  return (levels(fact)[tabulate_factor(fact) == 0L])
}

level_index <- function(fact, lev) {
  if (is.na(lev)) {
    return (which(is.na(levels(fact))))
  } else {
    return (which(levels(fact) == lev))
  }
}

# duplicate of level_index() with better name and default argument for finding NAs
NA_index <- function(fact, NA_level = NA_character_) {
  if (is.na(NA_level)) {
    return (which(is.na(levels(fact))))
  } else {
    return (which(levels(fact) == NA_level))
  }
}


### Helpers -----------------------------------------------------------------

if (is_package_installed("data.table")) {
  # best versions of the factor functions set levels (and names) by reference using data.table::setattr
  re_name <- function(vec, new_names) {
    data.table::setattr(vec, "names", new_names)
  }
  
  re_level <- function(vec, new_levels, NA_level = NA_character_) {
    data.table::setattr(vec, "levels", fix_NA_levels(new_levels, NA_level = NA_level))
  }
} else {
  re_name <- function(vec, new_names) {
    names(vec) <- new_names
  }
  
  re_level <- function(vec, new_levels) {
    levels(vec) <- new_levels
  }
}

fix_NA_levels <- function(levs, NA_level = NA_character_) {
  if (!is.na(NA_level) && anyNA(levs)) {
    warning("NA levels set to the given (character) NA_level")
    levs[is.na(levs)] <- NA_level
  }
  return (levs)
}


### Ordering Function -------------------------------------------------------

ordered_empty_at_end <- function(column, empty_string) {
  ordered_levels <- sort(levels(column))
  if ("" %c% ordered_levels) {
    ordered_levels <- c(ordered_levels %[!=]% "", empty_string)
    return (ordered(replace_level(column, from = "", to = empty_string), levels = ordered_levels))
  } else {
    if (empty_string %c% ordered_levels) {
      ordered_levels <- c(ordered_levels %[!=]% empty_string, empty_string)
    }
    return (ordered(column, levels = ordered_levels))
  }
}

refactor_and_order <- function(column, empty_string, drop_to = "") {
  if (drop_to %c% missing_levels(column) || drop_to %!c% levels(column)) {
    return (ordered_empty_at_end(column = droplevels(column), empty_string = empty_string))
  } else {
    return (ordered_empty_at_end(column = drop_missing_levels(column, to = drop_to), empty_string = empty_string))
  }
}


### Level Functions ---------------------------------------------------------

### Format

format_levels <- function(fact, func, ...) {
  re_level(fact, func(levels(fact), ...))
}

format_similar_levels <- function(fact, pairings, exact = FALSE, ...) {
  new_levels <- levels(fact)
  pairings_names <- get_names(pairings)
  if (exact) {
    for (i in seq_along(pairings)) {
      new_levels[new_levels %exactlylike% pairings_names[[i]]] <- (pairings[[i]])(new_levels %whichexactlylike% pairings_names[[i]], ...)
    }
  } else {
    for (i in seq_along(pairings)) {
      new_levels[new_levels %like% pairings_names[[i]]] <- (pairings[[i]])(new_levels %whichlike% pairings_names[[i]], ...)
    }
  }
  re_level(fact, new_levels)
}


### Replace

replace_level <- function(fact, from, to) {
  assert_that(length(from) == 1L && length(to) == 1L, 
              msg = "either (or both) 'from' or 'to' are of length > 1L (you may have intended to use replace_levels, not replace_level")
  new_levels <- levels(fact)
  new_levels[new_levels == from] <- to
  re_level(fact, new_levels)
}

replace_levels <- function(fact, from, to) {
  assert_that(length(from) == length(to) || length(to) == 1L, 
              msg = "Lengths of 'from' and 'to' don't match")
  new_levels <- levels(fact)
  if (length(to) == 1L) {
    for (i in seq_along(from)) {
      new_levels[new_levels == from[[i]]] <- to
    }
  } else {
    for (i in seq_along(from)) {
      new_levels[new_levels == from[[i]]] <- to[[i]]
    }
  }
  re_level(fact, new_levels)
}


### Rename

rename_levels <- function(fact, changes) {
  new_levels <- levels(fact)
  re_name(new_levels, new_levels)
  changes <- changes %whichin% new_levels
  if (!is_empty(changes)) {
    change_empty_level <- "" %c% changes
    if (change_empty_level) {
      empty_change <- get_names(changes %[==]% "")
      changes <- changes %[!=]% ""
    }
    changes_names <- get_names(changes)
    for (i in seq_along(changes)) {
      new_levels[[changes[[i]]]] <- changes_names[[i]]
    }
    if (change_empty_level) {
      new_levels[new_levels == ""] <- empty_change
    }
    re_level(fact, unname(new_levels))
  } else {
    invisible(fact)
  }
}

rename_similar_levels <- function(fact, changes, exact = FALSE) {
  new_levels <- levels(fact)
  changes_names <- get_names(changes)
  for (i in seq_along(changes)) {
    new_levels <- gsub(pattern = changes[[i]], replacement = changes_names[[i]], new_levels, fixed = exact)
  }
  re_level(fact, new_levels)
}


### Add

add_levels <- function(fact, add) {
  new_levels <- levels(fact)
  add <- add %which!in% new_levels
  if (!is_empty(add)) {
    re_level(fact, c(new_levels, add))
  } else {
    invisible(fact)
  }
}


### Drop

drop_levels <- function(fact, drop, to = "") {
  new_levels <- levels(fact)
  drop <- drop %whichin% new_levels
  if (!is_empty(drop)) {
    re_name(new_levels, new_levels)
    drop_empty_level <- "" %c% drop
    if (drop_empty_level) {
      drop <- drop %[!=]% ""
    }
    for (i in seq_along(drop)) {
      new_levels[[drop[[i]]]] <- to
    }
    if (drop_empty_level) {
      new_levels[new_levels == ""] <- to
    }
    re_level(fact, unname(new_levels))
  } else {
    invisible(fact)
  }
}

drop_similar_levels <- function(fact, drop, to = "", exact = FALSE) {
  new_levels <- levels(fact)
  for (i in seq_along(drop)) {
    new_levels[grepl(pattern = drop[[i]], fixed = exact, new_levels)] <- to
  }
  re_level(fact, new_levels)
}

drop_missing_levels <- function(fact, to = "") {
  drop_levels(fact, drop = missing_levels(fact), to)
}

drop_levels_formula <- function(fact, expr, to = "") {
  drop_levels <- eval(parse(text = paste0("levels(fact) %[]% ", deparse(substitute(expr)))))
  drop_levels(fact, drop = drop_levels, to)
}

keep_levels <- function(fact, keep, to = "") {
  new_levels <- levels(fact)
  new_levels[new_levels %!in% keep] <- to
  re_level(fact, new_levels)
}

keep_similar_levels <- function(fact, keep, to = "", exact = FALSE) {
  levels_to_drop <- levels(fact)
  for (i in seq_along(keep)) {
    levels_to_drop[grepl(pattern = keep[[i]], fixed = exact, levels_to_drop)] <- to
  }
  levels_to_drop <- levels_to_drop[levels_to_drop != to]
  drop_levels(fact, drop = levels_to_drop, to = to)
}


### Simplify

reduce_levels <- function(fact, rules, other = "other", exact = FALSE) {
  replacements <- get_names(rules)
  patterns <- unname(rules)
  old_levels <- levels(fact)
  new_levels <- rep(other, length(old_levels))
  for (i in seq_along(rules)) {
    slicer <- grepl(pattern = patterns[[i]], fixed = exact, old_levels)
    new_levels[slicer] <- replacements[[i]]
    old_levels[slicer] <- ""
  }
  re_level(fact, new_levels)
}

otherize_levels_rank <- function(fact, cutoff, other = "other", otherize_empty_levels = TRUE, include_ties = TRUE) {
  fact_levels <- levels(fact)
  if (otherize_empty_levels) {
    otherize_empty_levels <- "" %c% fact_levels
  }
  lookup_table <- data.table(levels = fact_levels, count = tabulate_factor(fact))
  print(lookup_table)
  if (other %c% fact_levels) {
    cutoff <- cutoff + 1L
  }
  if (otherize_empty_levels) {
    lookup_table <- lookup_table[levels != ""]
  }
  setkey(lookup_table, count)
  if (include_ties) {
    count_at_cutoff <- lookup_table[.N-cutoff, count]
    dropped_levels <- lookup_table[count < count_at_cutoff, levels]
  } else {
    dropped_levels <- lookup_table[1:(.N-cutoff), levels]
  }
  if (otherize_empty_levels) {
    dropped_levels <- append(dropped_levels, "")
  }
  drop_levels(fact, drop = dropped_levels, to = other)
}

otherize_levels_prop <- function(fact, cutoff, other = "other", otherize_empty_levels = TRUE) {
  fact_levels <- levels(fact)
  if (otherize_empty_levels) {
    otherize_empty_levels <- "" %c% fact_levels
  }
  lookup_table <- data.table(levels = fact_levels, prop = level_proportions(fact))
  if (otherize_empty_levels) {
    lookup_table <- lookup_table[levels != ""]
  }
  dropped_levels <- lookup_table[prop < cutoff, levels]
  if (otherize_empty_levels) {
    dropped_levels <- append(dropped_levels, "")
  }
  drop_levels(fact, drop = dropped_levels, to = other)
}

otherize_levels_count <- function(fact, cutoff, other = "other", otherize_empty_levels = TRUE) {
  fact_levels <- levels(fact)
  if (otherize_empty_levels) {
    otherize_empty_levels <- "" %c% fact_levels
  }
  lookup_table <- data.table(levels = fact_levels, count = tabulate_factor(fact))
  if (otherize_empty_levels) {
    lookup_table <- lookup_table[levels != ""]
  }
  dropped_levels <- lookup_table[count < cutoff, levels]
  if (otherize_empty_levels) {
    dropped_levels <- append(dropped_levels, "")
  }
  drop_levels(fact, drop = dropped_levels, to = other)
}

otherize_groups_count <- function(dt, cutoff, group_cols = colnames(dt), cols_to_otherize = colnames(dt), other = "other") {
  count_table <- dt[, .N, by = group_cols]
  for_removal <- count_table[N < cutoff, group_cols, with = FALSE]
  otherize_groups_base(dt, for_removal, group_cols, cols_to_otherize, other)
}

otherize_groups_prop <- function(dt, cutoff, group_cols = colnames(dt), cols_to_otherize = colnames(dt), other = "other") {
  dt_nrows <- nrow(dt)
  prop_table <- dt[, .(prop = .N / dt_nrows), by = group_cols]
  for_removal <- prop_table[prop < cutoff, group_cols, with = FALSE]
  otherize_groups_base(dt, for_removal, group_cols, cols_to_otherize, other)
}

otherize_groups_rank <- function(dt, cutoff, group_cols = colnames(dt), cols_to_otherize = colnames(dt), other = "other", include_ties = TRUE) {
  count_table <- dt[, .N, by = group_cols]
  setkey(count_table, N)
  if (include_ties) {
    N_at_cutoff <- count_table[.N-cutoff, N]
    for_removal <- count_table[N < N_at_cutoff, group_cols, with = FALSE]
  } else {
    for_removal <- count_table[1:(.N-cutoff), group_cols, with = FALSE]
  }
  otherize_groups_base(dt, for_removal, group_cols, cols_to_otherize, other)
}

otherize_groups_base <- function(dt, for_removal, group_cols, cols_to_otherize, other) {
  otherize_ncols <- length(cols_to_otherize)
  other <- recycle_arguments(other, otherize_ncols)
  removal_nrows <- nrow(for_removal)
  for (c in seq_len(removal_nrows)) {
    rows <- dt[for_removal[c], on = group_cols, which = TRUE]
    for (d in seq_len(otherize_ncols)) {
      set(dt, i = rows, j = cols_to_otherize[[d]], value = other[[d]])
    }
  }
}


### Validate

if (is_package_loaded("data.table")) {
  
  fill_matching_values_ <- function(data, code_col, value_col, ...) {
    code_col <- deparse(substitute(code_col))
    value_col <- deparse(substitute(value_col))
    fill_matching_values(data, code_col, value_col, ...)
  }
  
  # currently requires both codes and values to be factors before calling the function
  # currently this function only exists through data.table (if data.table isn't laoded, this function isn't supported)
  # need to also ensure that different types of missing/incomplete values are handled (what if "" and NA are both present?)
  fill_matching_values <- function(data, code_col, value_col, backfill = FALSE, drop.codes = FALSE, drop.values = FALSE, drop.missing.levels = FALSE, NA_code = NA_of_same_type(data[[code_col]]), NA_value = NA_of_same_type(data[[value_col]]), assume.exclusive = FALSE) {
    NA_index_value <- NA_index(data[[value_col]], NA_value)
    if (is_empty(NA_index_value)) {
      code_to_values <- eval(parse(text = paste0("data[, .('codes' = ", code_col, ", 'values' = ", value_col, ")]")))
    } else {
      code_to_values <- eval(parse(text = paste0("data[as.integer(", value_col, ") != ", as.character(NA_index_value), "L, .('codes' = ", code_col, ", 'values' = ", value_col, ")]")))
    }
    NA_index_code <- NA_index(code_to_values[["codes"]], NA_code)
    if (is_empty(NA_index_code)) {
      code_to_values <- unique(code_to_values)
    } else {
      code_to_values <- unique(code_to_values[as.integer(codes) != NA_index_code, ])
    }
    code_to_values <- code_to_values[, .(values, 'GRPN' = .N), keyby = codes]
    indeterminate_codes <- code_to_values[GRPN > 1L, as.character(unique(codes))]
    if (!is_empty(indeterminate_codes)) {
      if (assume.exclusive) {
        if (is_empty(NA_index_value)) {
          for (a_code in indeterminate_codes) {
            present_levels <- mode_and_others_factor(eval(parse(text = paste0("data[", code_col, " == a_code, ", value_col, "]"))))
            replace_levels(code_to_values[["values"]], from = present_levels[["others"]], to = present_levels[["mode"]])
          }
        } else {
          for (a_code in indeterminate_codes) {
            present_levels <- mode_and_others_factor(eval(parse(text = paste0("data[", code_col, " == a_code & as.integer(", value_col, ") != ", as.character(NA_index_value), "L, ", value_col, "]"))))
            replace_levels(code_to_values[["values"]], from = present_levels[["others"]], to = present_levels[["mode"]])
          }
        }
      } else {
        if (is_empty(NA_index_value)) {
          for (a_code in indeterminate_codes) {
            present_levels <- mode_and_others_factor(eval(parse(text = paste0("data[", code_col, " == a_code, ", value_col, "]"))))
            set(code_to_values, i = which(code_to_values[["codes"]] == a_code), j = "values", present_levels[["mode"]])
          }
        } else {
          for (a_code in indeterminate_codes) {
            present_levels <- mode_and_others_factor(eval(parse(text = paste0("data[", code_col, " == a_code & as.integer(", value_col, ") != ", as.character(NA_index_value), "L, ", value_col, "]"))))
            set(code_to_values, i = which(code_to_values[["codes"]] == a_code), j = "values", present_levels[["mode"]])
          }
        }
      }
      code_to_values <- unique(code_to_values[, .(codes, values)])
    } else {
      code_to_values <- code_to_values[, .(codes, values)]
    }
    verified_codes <- as.character(code_to_values[["codes"]])
    
    NA_index_code <- NA_index(data[[code_col]], NA_code)
    if (is_empty(NA_index_code)) {
      all_codes <- levels(data[[code_col]])
    } else {
      if (is.na(NA_code)) {
        all_codes <- as.character(unique(data[[code_col]] %[]% (as.integer(.) != NA_index_code)))
      } else {
        all_codes <- levels(data[[code_col]]) %[!=]% ""
      }
    }
    unverified_codes <- all_codes %d% verified_codes
    
    if (is_empty(unverified_codes)) {
      code_to_values_for_join <- code_to_values
    } else {
      if (drop.codes) {
        drop_levels(data[[code_col]], drop = unverified_codes, to = NA_code)
        code_to_values_for_join <- rbindlist(list(code_to_values, data.table(codes = NA_code, values = NA_value)))
      } else {
        code_to_values_for_join <- rbindlist(list(code_to_values, data.table(codes = unverified_codes, values = NA_value)))
      }
    }
    
    joined_data <- eval(parse(text = paste0("code_to_values_for_join[data, .(values), on = c('codes' = '", code_col, "')]")))
    NA_index_code <- NA_index(data[[code_col]], NA_code)
    if (is_empty(NA_index_code)) {
      set(data, j = value_col, value = joined_data[["values"]])
    } else {
      slicer <- as.integer(data[[code_col]]) != NA_index_code
      set(data, i = which(slicer), j = value_col, value = joined_data[["values"]][slicer])
    }
    
    if (drop.missing.levels) {
      drop_missing_levels(data[[value_col]], to = NA_value)
    }
    
    NA_index_code <- NA_index(data[[code_col]], NA_code)
    if (is_empty(NA_index_code)) {
      value_to_codes <- eval(parse(text = paste0("data[, .('values' = ", value_col, ", 'codes' = ", code_col, ")]")))
    } else {
      value_to_codes <- eval(parse(text = paste0("data[as.integer(", code_col, ") != ", as.character(NA_index_code), "L, .('values' = ", value_col, ", 'codes' = ", code_col, ")]")))
    }
    NA_index_value <- NA_index(value_to_codes[["values"]], NA_value)
    if (is_empty(NA_index_value)) {
      value_to_codes <- unique(value_to_codes)
    } else {
      value_to_codes <- unique(value_to_codes[as.integer(values) != NA_index_value, ])
    }
    value_to_codes <- value_to_codes[, .(codes, 'GRPN' = .N), keyby = values]
    indeterminate_values <- value_to_codes[GRPN > 1L, as.character(unique(values))]
    if (!is_empty(indeterminate_values)) {
      if (assume.exclusive) {
        if (is_empty(NA_index_code)) {
          for (a_value in indeterminate_values) {
            present_levels <- mode_and_others_factor(eval(parse(text = paste0("data[", value_col, " == a_value, ", code_col, "]"))))
            replace_levels(value_to_codes[["codes"]], from = present_levels[["others"]], to = present_levels[["mode"]])
          }
        } else {
          for (a_value in indeterminate_values) {
            present_levels <- mode_and_others_factor(eval(parse(text = paste0("data[", value_col, " == a_value & as.integer(", code_col, ") != ", as.character(NA_index_code), "L, ", code_col, "]"))))
            replace_levels(value_to_codes[["codes"]], from = present_levels[["others"]], to = present_levels[["mode"]])
          }
        }
      } else {
        if (is_empty(NA_index_code)) {
          for (a_value in indeterminate_values) {
            present_levels <- mode_and_others_factor(eval(parse(text = paste0("data[", value_col, " == a_value, ", code_col, "]"))))
            set(value_to_codes, i = which(value_to_codes[["values"]] == a_value), j = "codes", present_levels[["mode"]])
          }
        } else {
          for (a_value in indeterminate_values) {
            present_levels <- mode_and_others_factor(eval(parse(text = paste0("data[", value_col, " == a_value & as.integer(", code_col, ") != ", as.character(NA_index_code), "L, ", code_col, "]"))))
            set(value_to_codes, i = which(value_to_codes[["values"]] == a_value), j = "codes", present_levels[["mode"]])
          }
        }
      }
      value_to_codes <- unique(value_to_codes[, .(values, codes)])
    } else {
      value_to_codes <- value_to_codes[, .(values, codes)]
    }
    verified_values <- as.character(value_to_codes[["values"]])
    
    NA_index_value <- NA_index(data[[value_col]], NA_value)
    if (is_empty(NA_index_value)) {
      all_values <- levels(data[[value_col]])
    } else {
      if (is.na(NA_value)) {
        all_values <- as.character(unique(data[[value_col]] %[]% (as.integer(.) != NA_index_value)))
      } else {
        all_values <- levels(data[[value_col]]) %[!=]% ""
      }
    }
    unverified_values <- all_values %d% verified_values
    
    if (is_empty(unverified_values)) {
      value_to_codes_for_join <- value_to_codes
    } else {
      if (drop.values) {
        drop_levels(data[[value_col]], drop = unverified_values, to = NA_value)
        value_to_codes_for_join <- rbindlist(list(value_to_codes, data.table(values = NA_value, codes = NA_code)))
      } else {
        value_to_codes_for_join <- rbindlist(list(value_to_codes, data.table(values = unverified_values, codes = NA_code)))
      }
    }
    
    if (backfill) {
      joined_data <- eval(parse(text = paste0("value_to_codes_for_join[data, .(codes), on = c('values' = '", value_col, "')]")))
      NA_index_value <- NA_index(data[[value_col]], NA_value)
      if (is_empty(NA_index_value)) {
        set(data, j = code_col, value = joined_data[["codes"]])
      } else {
        slicer <- as.integer(data[[value_col]]) != NA_index_value
        set(data, i = which(slicer), j = code_col, value = joined_data[["codes"]][slicer])
      }
      
      if (drop.missing.levels) {
        drop_missing_levels(data[[code_col]], to = NA_code)
      }
    }
    invisible(data)
  }
  
  # correct_mistakes <- function() {#*** haven't implemented this yet as I'm not sure I'll need it
  #   # do fill_matching_values()-like stuff but correct based on other columns (useful for me here as I'll be correcting City and Country based on latitude and longitude)
  #   # get code_to_values (here observation 1 and observation 2 or obseration and corollary)
  #   # n-dimensional (easy case 2-dimensional like I have) corresponding values that can be used (distance deviation from median-wise) to determine whether two corollaries of an observation are different and should be different or not *and/or* whether two observations are labeled differently but should be labeled the same
  # }
}


### Repeat by column

by_col <- function(data, col_func, cols = colnames(keep(data, is.factor)), ...) {
  for (col in cols) {
    col_func(data[[col]], ...)
  }
  invisible(data)
}

fix_NAs_by_col <- function(data, NA_level = "<NA>", cols = colnames(keep(data, is.factor))) {
  for (col in cols) {
    if (anyNA(data[[col]])) {
      data[[col]] %>% add_levels(NA_level)
      set(data, i = which(is.na(data[[col]])), j = col, NA_level)
    }
  }
  invisible(data)
}

format_levels_by_col <- function(data, func, cols = colnames(keep(data, is.factor)), ...) {
  for (col in cols) {
    format_levels(data[[col]], func, ...)
  }
  invisible(data)
}

format_similar_levels_by_col <- function(data, pairings, exact = FALSE, cols = colnames(keep(data, is.factor))) {
  for (col in cols) {
    format_similar_levels(data[[col]], pairings)
  }
  invisible(data)
}

replace_level_by_col <- function(data, from, to, cols = colnames(keep(data, is.factor))) {
  for (col in cols) {
    replace_level(data[[col]], from, to)
  }
  invisible(data)
}

replace_levels_by_col <- function(data, from, to, cols = colnames(keep(data, is.factor))) {
  for (col in cols) {
    replace_levels(data[[col]], from, to)
  }
  invisible(data)
}

rename_levels_by_col <- function(data, changes, cols = colnames(keep(data, is.factor))) {
  for (col in cols) {
    rename_levels(data[[col]], changes)
  }
  invisible(data)
}

rename_similar_levels_by_col <- function(data, changes, exact = FALSE, cols = colnames(keep(data, is.factor))) {
  for (col in cols) {
    rename_similar_levels(data[[col]], changes, exact)
  }
  invisible(data)
}

add_levels_by_col <- function(data, add, cols = colnames(keep(data, is.factor))) {
  for (col in cols) {
    add_levels(data[[col]], add)
  }
  invisible(data)
}

drop_levels_by_col <- function(data, drop, to = "", cols = colnames(keep(data, is.factor))) {
  for (col in cols) {
    drop_levels(data[[col]], drop, to)
  }
  invisible(data)
}

drop_similar_levels_by_col <- function(data, drop, to = "", exact = FALSE, cols = colnames(keep(data, is.factor))) {
  for (col in cols) {
    drop_similar_levels(data[[col]], drop, to, exact)
  }
  invisible(data)
}

drop_missing_levels_by_col <- function(data, to = "", cols = colnames(keep(data, is.factor))) {
  for (col in cols) {
    drop_missing_levels(data[[col]], to)
  }
  invisible(data)
}

drop_levels_formula_by_col <- function(data, expr, to = "", cols = colnames(keep(data, is.factor))) {
  for (col in cols) {
    drop_levels_formula(data[[col]], expr, to)
  }
  invisible(data)
}

keep_levels_by_col <- function(data, keep, to = "", cols = colnames(keep(data, is.factor))) {
  for (col in cols) {
    keep_levels(data[[col]], keep, to)
  }
  invisible(data)
}

keep_similar_levels_by_col <- function(data, keep, to = "", exact = FALSE, cols = colnames(keep(data, is.factor))) {
  for (col in cols) {
    keep_similar_levels(data[[col]], keep, to, exact)
  }
  invisible(data)
}

reduce_levels_by_col <- function(data, rules, other = "other", exact = FALSE, cols = colnames(keep(data, is.factor))) {
  for (col in cols) {
    reduce_levels(data[[col]], rules, other, exact)
  }
  invisible(data)
}

otherize_levels_rank_by_col <- function(data, rank, other = "other", exact = FALSE, cols = colnames(keep(data, is.factor))) {
  for (col in cols) {
    otherize_levels_rank(data[[col]], rank, other, exact)
  }
  invisible(data)
}

otherize_levels_prop_by_col <- function(data, cutoff, other = "other", exact = FALSE, cols = colnames(keep(data, is.factor))) {
  for (col in cols) {
    otherize_levels_prop(data[[col]], cutoff, other, exact)
  }
  invisible(data)
}

fill_matching_values_by_col <- function(data, code_cols, value_cols, backfill = FALSE, drop.codes = FALSE, drop.values = FALSE, drop.missing.levels = FALSE, NA_code = NA_of_same_type_by_col(select(data, code_cols)), NA_value = NA_of_same_type_by_col(select(data, value_cols)), assume.exclusive = FALSE) {
  assert_that(length(value_cols) == length(code_cols), 
              msg = "There is a different number of value and code columns")
  num_cols <- length(value_cols)
  drop.codes          <- recycle_arguments(drop.codes,          num_cols)
  drop.values         <- recycle_arguments(drop.values,         num_cols)
  drop.missing.levels <- recycle_arguments(drop.missing.levels, num_cols)
  backfill            <- recycle_arguments(backfill,            num_cols)
  NA_value            <- recycle_arguments(NA_value,            num_cols)
  NA_code             <- recycle_arguments(NA_code,             num_cols)
  assume.exclusive    <- recycle_arguments(assume.exclusive,    num_cols)
  for (i in seq_along(value_cols)) {
    fill_matching_values(data, code_col = code_cols[[i]], value_col = value_cols[[i]], backfill = backfill[[i]], drop.codes = drop.codes[[i]], drop.values = drop.values[[i]], drop.missing.levels = drop.missing.levels[[i]], NA_value = NA_value[[i]], NA_code = NA_code[[i]], assume.exclusive = assume.exclusive[[i]])
  }
  invisible(data)
}
