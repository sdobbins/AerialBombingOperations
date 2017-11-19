# @author Scott Dobbins
# @version 0.9.9.6
# @date 2017-11-19 01:00


### Constants ---------------------------------------------------------------

regex_metacharacters <- c("$", "(", ")", "*", "+", ".", "?", "[", "\\", "]", "^", "{", "|", "}")
regex_metacharacters_escaped <- paste0("\\", regex_metacharacters)
regex_metacharacters_set <- "[]|{}$()*+.?\\[^]"
regex_metacharacters_set_captured <- paste0("(", regex_metacharacters_set, ")")


### Regex -------------------------------------------------------------------

rem <- function(strings, pattern, exact = FALSE) {
  return (sub(pattern = pattern, replacement = "", x = strings, fixed = exact))
}

grem <- function(strings, pattern, exact = FALSE) {
  return (gsub(pattern = pattern, replacement = "", x = strings, fixed = exact))
}

grems <- function(strings, patterns, exact = FALSE) {
  exact <- recycle_arguments(exact, length(patterns))
  for (i in seq_along(patterns)) {
    strings <- gsub(pattern = patterns[[i]], replacement = "", x = strings, fixed = exact[[i]])
  }
  return (strings)
}

gsubs <- function(strings, changes, exact = FALSE) {
  changes_names <- get_names(changes)
  exact <- recycle_arguments(exact, length(changes))
  for (i in seq_along(changes)) {
    strings <- gsub(pattern = changes[[i]], replacement = changes_names[[i]], x = strings, fixed = exact[[i]])
  }
  return (strings)
}


### Regex Helpers -----------------------------------------------------------

### Simple Repeats

possible <- function(strings) {
  return (paste0(capturing_group(strings), "?"))
}

multiple <- function(strings) {
  return (paste0(capturing_group(strings), "+"))
}

some <- function(strings) {
  return (paste0(capturing_group(strings), "*"))
}

### Complex Repeats

as_many_of <- function(strings, spacing = " ") {
  return (paste0(capturing_group(paste0(strings, spacing, "?")), "*"))
}

n_or_fewer <- function(strings, n) {
  return (paste0(capturing_group(strings), "{,", n, "}"))
}

n_or_more <- function(strings, n) {
  return (paste0(capturing_group(strings), "{", n, ",}"))
}

m_to_n <- function(strings, m, n) {
  return (paste0(capturing_group(strings), "{", m, ",", n, "}"))
}

### Selectors/Modifiers

any_of <- function(strings) {
  return (capturing_group(paste0(strings, collapse = "|")))
}

literal <- function(strings) {
  return (gsub(pattern = regex_metacharacters_set_captured, "\\\\\\1", strings))
}

word <- function(strings) {
  return (paste0("\\b", strings, "\\b"))
}

capturing_group <- function(strings) {
  return (paste0("(", strings, ")"))
}

selection_group <- function(strings, not = FALSE) {
  if (length(not) == 1L) {
    if (not) {
      invert <- "^"
    } else {
      invert <- ""
    }
    return (paste0("[", invert, strings, "]"))
  } else {
    assert_that(length(strings) == length(not), 
                msg = "Length of negation condition doesn't match length of captured strings")
    return (paste0("[", if_else(not, "^", ""), strings, "]"))
  }
}

beginning_with <- function(strings) {
  return (paste0("^", strings))
}

ending_with <- function(strings) {
  return (paste0(strings, "$"))
}

### Grabbers

with_preceding <- function(strings, marks = " ", mandatory = FALSE) {
  needs_grouping <- nchar(marks) > 1L
  marks[needs_grouping] <- capturing_group(marks[needs_grouping])
  if (mandatory) {
    return (paste0(marks, capturing_group(strings)))
  } else {
    return (paste0(marks, "?", capturing_group(strings)))
  }
}

with_following <- function(strings, marks = " ", mandatory = FALSE) {
  needs_grouping <- nchar(marks) > 1L
  marks[needs_grouping] <- capturing_group(marks[needs_grouping])
  if (mandatory) {
    return (paste0(capturing_group(strings), marks))
  } else {
    return (paste0(capturing_group(strings), marks, "?"))
  }
}

and_preceding <- function(strings, precedings = " ", succeedings = " ?", exact = FALSE, greedy = FALSE, stoppers = "") {
  if (exact) {
    strings <- literal(strings)
  }
  greed <- if_else(greedy, "", "?")
  return (paste0("([^", precedings, stoppers, "]*", precedings, ")*", greed, strings, succeedings))
}

and_after <- function(strings, precedings = "", exact = FALSE, greedy = TRUE, stoppers = "") {
  if (exact) {
    strings <- literal(strings)
  }
  if (length(stoppers) == 1L) {
    stoppers <- rep(stoppers, length(strings))
  }
  if (length(greedy) == 1L) {
    greedy <- rep(greedy, length(strings))
  }
  chars <- if_else(stoppers == "", "[\n\r\t -~]*", paste0("[^", stoppers, "]*"))
  greed <- if_else(greedy, "", paste0("(?!", chars, strings, ")"))
  return (paste0(precedings, strings, greed, chars))
}

and_between <- function(starts, ends, preceding_starts = " ", precedings = " ", succeedings = " ", exact = FALSE, greedy = FALSE, stoppers = "") {
  if (exact) {
    starts <- literal(starts)
    ends <- literal(ends)
  }
  greed <- if_else(greedy, "", "?")
  preceding_starts_chars <- grem(preceding_starts, "[]\\[]")
  stop_sets <- paste0("[", preceding_starts_chars, precedings, "]")
  not_stop_sets <- paste0("[^", preceding_starts_chars, precedings, "]")
  look_aheads <- if_else(greedy, "", paste0("(?!", starts, ")"))
  return (paste0(preceding_starts, starts, "(", not_stop_sets, "*", stop_sets, look_aheads, ")*", greed, ends, succeedings))
}

#and_between_containing <- function()#*** not implemented yet


### Formatters -------------------------------------------------------------

### Simple removers

fix_spaces <- function(strings) {
  return (gsubs(changes = c(" " = "  +", 
                            "^ ", 
                            " $"), 
                strings))
}

remove_parentheticals <- function(strings) {
  return (grem(pattern = " ?\\([^)]*\\)", strings))
}

remove_square_brackets <- function(strings) {
  return (grem(pattern = " ?\\[[^]]*\\]", strings))
}

fix_parentheses <- function(strings) {
  return (gsub(pattern = "(\\w)\\(", replacement = "\\1 \\(", strings))
}

remove_quotes <- function(strings) {
  return (grem(pattern = "\"", strings))
}

remove_nonASCII_chars <- function(strings) {
  return (grem(pattern = "[^ -~]+", strings))
}

remove_extra_whitespace <- function(strings) {
  return (gsub(pattern = "\\s{2,}", replacement = " ", strings))
}

remove_bad_formatting <- trimws %.% remove_extra_whitespace %.% remove_quotes %.% remove_nonASCII_chars

remove_duplicate <- function(strings, duplicate, exact = FALSE) {
  return (gsub(pattern = multiple(duplicate), replacement = duplicate, x = strings, fixed = exact))
}

remove_duplicates <- function(strings, duplicates, exact = FALSE) {
  exact <- recycle_arguments(exact, length(duplicates))
  for (i in seq_along(duplicates)) {
    strings <- gsub(pattern = multiple(duplicates[[i]]), replacement = duplicates[[i]], x = strings, fixed = exact[[i]])
  }
  return (strings)
}

remove_hanging_punctuation <- function(strings, chars, exact = FALSE, isolated = TRUE) {
  if (isolated) {
    changes <- c("\\2" = paste0("(^| )", chars, "([A-Za-z]*)"), 
                 "\\1" = paste0("([A-Za-z]*)", chars, "( |$)"), 
                 " "   = paste0(" ?", chars, "( |$)"))
  } else {
    changes <- c("\\2" = paste0("(^| )", chars, "([A-Za-z]*)"), 
                 "\\1" = paste0("([A-Za-z]*)", chars, "( |$)"))
  }
  return (gsubs(strings = strings, changes = changes, exact = exact))
}

remove_single_letters <- function(strings, ...) {
  return (remove_words_with_fewer_than_n_letters(strings = strings, n = 2L, ...))
}

remove_words_with_fewer_than_n_letters <- function(strings, n, with_punctuation = "", only_lower_case = FALSE, only_upper_case = FALSE) {
  assert_that(!(only_lower_case && only_upper_case), 
              msg = "Only lower case and only upper case are mutually exclusive.")
  assert_that(n >= 2L, 
              msg = "There are no words with less than 1 letter.")
  if (with_punctuation == "") {
    punctuation <- ""
  } else {
    punctuation <- possible(capturing_group(with_punctuation))
  }
  if (only_lower_case) {
    letters_used <- "[a-z]"
  } else if (only_upper_case) {
    letters_used <- "[A-Z]"
  } else {
    letters_used <- "[A-Za-z]"
  }
  basic_pattern <- paste0(punctuation, word(m_to_n(letters_used, 1L, n - 1L)), punctuation)
  return (gsub(pattern = paste0(capturing_group(paste0(basic_pattern, " ")), "|", capturing_group(paste0(possible(" "), basic_pattern))), replacement = "", x = strings))
}


### Specific formatters

format_commas <- function(strings) {
  return (gsub(pattern = " *, *", replacement = ", ", x = strings))
}

provide_buffers_around <- function(strings, chars, buffers = " ", exact = FALSE) {
  if (length(chars) > 1L) {
    exact <- recycle_arguments(exact, length(chars))
    buffers <- recycle_arguments(buffer, length(buffers))
    for (i in seq_along(chars)) {
      strings <- gsub(pattern = capturing_group(chars[[i]]), replacement = paste0(buffers[[i]], "\\1", buffers[[i]]), x = strings, fixed = exact[[i]])
    }
    return (strings)
  } else {
    return (gsub(pattern = capturing_group(chars), replacement = paste0(buffers, "\\1", buffers), x = strings, fixed = exact))
  }
}

remove_buffers_around <- function(strings, chars, buffers = " ", exact = FALSE) {
  if (length(chars) > 1L) {
    exact <- recycle_arguments(exact, length(chars))
    buffers <- recycle_arguments(buffer, length(buffers))
    for (i in seq_along(chars)) {
      strings <- gsub(pattern = paste0(buffers[[i]], capturing_group(chars[[i]]), buffers[[i]]), replacement = "\\1", x = strings, fixed = exact[[i]])
    }
    return (strings)
  } else {
    return (gsub(pattern = paste0(buffers, capturing_group(chars), buffers), replacement = "\\1", x = strings, fixed = exact))
  }
}



### Complex Removers --------------------------------------------------------

remove_before <- function(strings, points, exact = FALSE, greedy = FALSE, inclusive = FALSE) {
  if (greedy) {
    positions <- map_int(gregexpr(points, strings, fixed = exact), last)
  } else {
    positions <- as.integer(regexpr(points, strings, fixed = exact))
  }
  slicer <- positions != -1L
  if (is.factor(strings)) {
    strings <- as.character(strings)
  }
  if (inclusive) {
    strings[slicer] <- sub(points, "", substr(strings[slicer], start = positions, stop = nchar(strings)), fixed = exact)
  } else {
    strings[slicer] <- substr(strings[slicer], start = positions[slicer], stop = nchar(srings))
  }
  return (strings)
}

remove_after <- function(strings, points, exact = FALSE, greedy = FALSE, inclusive = FALSE) {
  if (greedy) {
    matches <- regexpr(points, strings, fixed = exact)
    match_lengths <- attr(matches, "match.length")
    positions <- as.integer(matches)
  } else {
    matches <- gregexpr(points, strings, fixed = exact)
    match_lengths <- map_int(matches, ~last(. %@% "match.length"))
    positions <- map_int(matches, last)
  }
  slicer <- positions != -1L
  if (is.factor(strings)) {
    strings <- as.character(strings)
  }
  if (inclusive) {
    strings[slicer] <- substr(strings[slicer], start = 1L, stop = positions[slicer] - 1L)
  } else {
    strings[slicer] <- substr(strings[slicer], start = 1L, stop = positions[slicer] + match_lengths[slicer])
  }
  return (strings)
}
