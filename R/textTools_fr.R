#' Strip Bracket Tags
#' @description Removes bracket tags like [rire], [pause], (laughter), etc. from transcripts.
#' This should be applied early in the processing pipeline to avoid counting metadata as language.
#' @param text a character vector of texts.
#' @return a character vector with bracket tags removed
#' @keywords internal
strip_bracket_tags <- function(text) {
  text <- as.character(text)
  # Remove square brackets: [tag], [tag with spaces], [tag:value]
  text <- gsub("\\[[^\\]]*\\]", "", text, perl = TRUE)
  # Remove round brackets: (tag), (tag with spaces), (tag:value)
  text <- gsub("\\([^)]*\\)", "", text, perl = TRUE)
  # Remove curly brackets: {tag}, {tag with spaces}, {tag:value}
  text <- gsub("\\{[^}]*\\}", "", text, perl = TRUE)
  # Clean up any double spaces that might result
  text <- gsub("\\s+", " ", text)
  text <- trimws(text)
  return(text)
}

#' French Contraction Expander
#' @description Expands French Contractions
#' @param text a character vector of texts.
#' @return a character vector
#' @keywords internal
ctxpand_fr <- function(text) {
  text <- as.character(text)

  # 1) Normalize apostrophes (curly -> straight)
  text <- gsub("\u2019|\u2018|\u02BC", "'", text)

  # 2) Common spoken / transcript reductions (useful, low-risk)
  # y'a / y a / ya  -> il y a
  text <- gsub("(?i)\\by\\s*'\\s*a\\b", "il y a", text, perl = TRUE)
  text <- gsub("(?i)\\bya\\b", "il y a", text, perl = TRUE)

  # chuis / j'suis / jsuis -> je suis
  text <- gsub("(?i)\\bchuis\\b", "je suis", text, perl = TRUE)
  text <- gsub("(?i)\\bj\\s*'\\s*suis\\b", "je suis", text, perl = TRUE)
  text <- gsub("(?i)\\bjsuis\\b", "je suis", text, perl = TRUE)

  # j'sais / jsais -> je sais
  text <- gsub("(?i)\\bj\\s*'\\s*sais\\b", "je sais", text, perl = TRUE)
  text <- gsub("(?i)\\bjsais\\b", "je sais", text, perl = TRUE)

  # t'es -> tu es (but NOT "tes" which is possessive adjective)
  text <- gsub("(?i)\\bt\\s*'\\s*es\\b", "tu es", text, perl = TRUE)
  
  # t'as / tas -> tu as
  text <- gsub("(?i)\\bt\\s*'\\s*as\\b", "tu as", text, perl = TRUE)
  
  # c'est -> ce est (for consistent tokenization)
  text <- gsub("(?i)\\bc\\s*'\\s*est\\b", "ce est", text, perl = TRUE)
  
  # qu'est-ce que -> que est ce que
  text <- gsub("(?i)\\bqu\\s*'\\s*est[- ]ce[- ]que\\b", "que est ce que", text, perl = TRUE)
  
  # m'a / m'as -> me a / me as
  text <- gsub("(?i)\\bm\\s*'\\s*a\\b", "me a", text, perl = TRUE)
  text <- gsub("(?i)\\bm\\s*'\\s*as\\b", "me as", text, perl = TRUE)
  
  # l'a -> le a / la a (simplified - could be more sophisticated)
  text <- gsub("(?i)\\bl\\s*'\\s*a\\b", "le a", text, perl = TRUE)
  
  # d' -> de (common contraction)
  text <- gsub("(?i)\\bd\\s*'\\b", "de ", text, perl = TRUE)
  
  # n' -> ne (negation contraction)
  text <- gsub("(?i)\\bn\\s*'\\b", "ne ", text, perl = TRUE)

  # p'tit / p'tite -> petit / petite
  text <- gsub("(?i)\\bp\\s*'\\s*tit\\b", "petit", text, perl = TRUE)
  text <- gsub("(?i)\\bp\\s*'\\s*tite\\b", "petite", text, perl = TRUE)

  # Optional: split aujourd'hui into two tokens (harmless for counting)
  text <- gsub("(?i)\\baujourd'hui\\b", "aujourd hui", text, perl = TRUE)

  # 3) Cleanup spacing
  text <- gsub("\\s+", " ", text)
  text <- trimws(text)

  unname(text)
}


#' Clean Text (French)
#' @description Basic text cleaning for French
#' @param text character text to be cleaned
#' @param language string. Default "french".
#' @param stop.words logical. Default TRUE
#' @return a character vector
#' @keywords internal
cleantext_fr<-function (text, language = "french", stop.words = TRUE) {
  text <- tolower(text)
  text <- gsub("[[:punct:]]", " ", text)
  text <- gsub("[[:cntrl:]]", " ", text)
  if (length(stop.words) > 1) {
    text <- tm::removeWords(text, stop.words)
  }
  else if (stop.words) {
    text <- tm::removeWords(text, tm::stopwords(language))
  }
  text <- tm::removeNumbers(text)
  text <- tm::stripWhitespace(text)
  text <- paste0(" ",text," ")
  return(as.character(text))
}

#' Normalize Phrases (French)
#' @description Normalizes phrase lists the same way text is normalized before feature extraction.
#' This ensures that phrase matching works correctly by applying the same transformations
#' (cleanpunct, ctxpand_fr, cleantext_fr) to both the search keys and the text.
#' @param x a character vector of phrases to normalize
#' @return a character vector of normalized phrases
#' @keywords internal
normalize_phrase_fr <- function(x) {
  cleantext_fr(ctxpand_fr(cleanpunct(x)), stop.words = FALSE)
}

