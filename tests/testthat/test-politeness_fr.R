# Tests for French politeness function
# -*- coding: utf-8 -*-

context("politeness_fr function")

# =============================================================================
# TEST DATA - Using ASCII-safe representations for special characters
# =============================================================================
test_texts_fr <- c(
  "Bonjour, pourriez-vous m'aider s'il vous plait ?",
  "Salut, tu peux m'aider ?",
  "Je suis vraiment desole pour le retard.",
  "Merci beaucoup pour votre aide !",
  "C'est nul, je deteste ca.",
  "Excusez-moi, serait-il possible de decaler la reunion ?",
  "Donne-moi ca maintenant.",
  "Nous pensons que c'est une bonne idee.",
  "Au revoir et a bientot !",
  "D'accord, pas de probleme."
)

num_messages_fr <- length(test_texts_fr)

# =============================================================================
# BASIC FUNCTIONALITY TESTS
# =============================================================================

test_that("politeness_fr runs with parser none", {
  df_polite <- politeness_fr(text = test_texts_fr, parser = "none", 
                              metric = "count", drop_blank = FALSE, num_mc_cores = 1)
  
  expect_true(is.data.frame(df_polite))
  expect_equal(nrow(df_polite), num_messages_fr)
  expect_true(ncol(df_polite) > 0)
})

test_that("politeness_fr binary metric works", {
  df_polite <- politeness_fr(text = test_texts_fr, parser = "none", 
                              metric = "binary", drop_blank = FALSE, num_mc_cores = 1)
  
  expect_equal(nrow(df_polite), num_messages_fr)
  # Binary values should only be 0 or 1
  unique_vals <- unique(unlist(df_polite))
  expect_true(all(unique_vals %in% c(0, 1)))
})

test_that("politeness_fr average metric works", {
  df_polite <- politeness_fr(text = test_texts_fr, parser = "none", 
                              metric = "average", drop_blank = FALSE, num_mc_cores = 1)
  
  expect_equal(nrow(df_polite), num_messages_fr)
  unique_values <- unique(unlist(df_polite))
  expect_true(all(unique_values >= 0, na.rm = TRUE))
})

test_that("politeness_fr drop_blank works", {
  df_polite_with_blanks <- politeness_fr(text = test_texts_fr, parser = "none", 
                                          metric = "count", drop_blank = FALSE, num_mc_cores = 1)
  df_polite_no_blanks <- politeness_fr(text = test_texts_fr, parser = "none", 
                                        metric = "count", drop_blank = TRUE, num_mc_cores = 1)
  
  # With drop_blank = TRUE, should have fewer or equal columns
  expect_true(ncol(df_polite_no_blanks) <= ncol(df_polite_with_blanks))
})

# =============================================================================
# EDGE CASES
# =============================================================================

test_that("politeness_fr handles empty strings", {
  df_polite <- politeness_fr(text = c("", "Bonjour"), parser = "none", 
                              metric = "binary", drop_blank = FALSE, num_mc_cores = 1)
  
  expect_equal(nrow(df_polite), 2)
  # Empty string should have all zeros
  expect_true(all(df_polite[1, ] == 0))
})

test_that("politeness_fr handles NA values", {
  df_polite <- politeness_fr(text = c(NA_character_, "Bonjour"), parser = "none", 
                              metric = "binary", drop_blank = FALSE, num_mc_cores = 1)
  
  expect_equal(nrow(df_polite), 2)
  # NA should be treated like empty and have all zeros
  expect_true(all(df_polite[1, ] == 0))
})

test_that("politeness_fr handles single text", {
  df_polite <- politeness_fr(text = "Bonjour, merci beaucoup !", parser = "none", 
                              metric = "count", drop_blank = FALSE, num_mc_cores = 1)
  
  expect_true(is.data.frame(df_polite))
  expect_equal(nrow(df_polite), 1)
})

test_that("politeness_fr handles factor input", {
  df_polite <- politeness_fr(text = factor(test_texts_fr), parser = "none", 
                              metric = "binary", drop_blank = FALSE, num_mc_cores = 1)
  
  expect_equal(nrow(df_polite), num_messages_fr)
})

# =============================================================================
# FRENCH-SPECIFIC FEATURE TESTS
# =============================================================================

test_that("politeness_fr detects French greetings", {
  greetings <- c("Bonjour !", "Salut !", "Bonsoir !", "Coucou !")
  df_polite <- politeness_fr(text = greetings, parser = "none", 
                              metric = "count", drop_blank = FALSE, num_mc_cores = 1)
  
  expect_true("Hello" %in% colnames(df_polite))
  expect_true(all(df_polite$Hello > 0))
})

test_that("politeness_fr detects French please", {
  # Using variations without accents for testing
  please_texts <- c("s'il vous plait merci", "s'il te plait aide moi", "SVP venez", "STP aide")
  df_polite <- politeness_fr(text = please_texts, parser = "none", 
                              metric = "count", drop_blank = FALSE, num_mc_cores = 1)
  
  expect_true("Please" %in% colnames(df_polite))
  expect_true(sum(df_polite$Please) > 0)
})

test_that("politeness_fr detects gratitude", {
  gratitude_texts <- c("Merci beaucoup", "Je vous remercie", "Merci infiniment")
  df_polite <- politeness_fr(text = gratitude_texts, parser = "none", 
                              metric = "count", drop_blank = FALSE, num_mc_cores = 1)
  
  expect_true("Gratitude" %in% colnames(df_polite))
  expect_true(all(df_polite$Gratitude > 0))
})

test_that("politeness_fr detects apologies", {
  # Using ASCII-safe versions
  apology_texts <- c("Je suis desole", "Excusez-moi", "Pardon", "Je suis navre")
  df_polite <- politeness_fr(text = apology_texts, parser = "none", 
                              metric = "count", drop_blank = FALSE, num_mc_cores = 1)
  
  expect_true("Apology" %in% colnames(df_polite))
  # At least some should detect apology (pardon, excusez should work)
  expect_true(sum(df_polite$Apology) > 0)
})

test_that("politeness_fr detects tutoiement vs vouvoiement", {
  tu_text <- c("Tu peux m'aider ?", "Comment vas-tu ?")
  vous_text <- c("Pouvez-vous m'aider ?", "Comment allez-vous ?")
  
  df_tu <- politeness_fr(text = tu_text, parser = "none", 
                          metric = "count", drop_blank = FALSE, num_mc_cores = 1)
  df_vous <- politeness_fr(text = vous_text, parser = "none", 
                            metric = "count", drop_blank = FALSE, num_mc_cores = 1)
  
  expect_true("T_Form" %in% colnames(df_tu))
  expect_true("V_Form" %in% colnames(df_vous))
  expect_true(all(df_tu$T_Form > 0))
  expect_true(all(df_vous$V_Form > 0))
})

test_that("politeness_fr detects conditional requests (Could.You)", {
  conditional_texts <- c("Pourriez-vous m'aider ?", "Voudriez-vous venir ?", 
                         "Serait-il possible de partir ?")
  df_polite <- politeness_fr(text = conditional_texts, parser = "none", 
                              metric = "count", drop_blank = FALSE, num_mc_cores = 1)
  
  expect_true("Could.You" %in% colnames(df_polite))
  expect_true(sum(df_polite$Could.You) > 0)
})

test_that("politeness_fr detects hedges", {
  # Using clear hedge words
  hedge_texts <- c("Probablement demain", "Je pense que oui", "Il semble correct")
  df_polite <- politeness_fr(text = hedge_texts, parser = "none", 
                              metric = "count", drop_blank = FALSE, num_mc_cores = 1)
  
  expect_true("Hedges" %in% colnames(df_polite))
  expect_true(sum(df_polite$Hedges) > 0)
})

test_that("politeness_fr detects goodbyes", {
  # Using ASCII-safe goodbye words
  goodbye_texts <- c("Au revoir monsieur", "Salut a plus", "Ciao tout le monde")
  df_polite <- politeness_fr(text = goodbye_texts, parser = "none", 
                              metric = "count", drop_blank = FALSE, num_mc_cores = 1)
  
  expect_true("Goodbye" %in% colnames(df_polite))
  expect_true(sum(df_polite$Goodbye) > 0)
})

# =============================================================================
# FEATURE COMPLETENESS TEST
# =============================================================================

test_that("politeness_fr returns expected features", {
  df_polite <- politeness_fr(text = test_texts_fr, parser = "none", 
                              metric = "count", drop_blank = FALSE, num_mc_cores = 1)
  
  # Core features that should always be present
  expected_features <- c(
    "Hedges", "Impersonal.Pronoun", "Swearing", "Negation", 
    "Filler.Pause", "Informal.Title", "Formal.Title",
    "Could.You", "Can.You", "Please", "Hello", "Goodbye",
    "First.Person.Single", "First.Person.Plural",
    "T_Form", "V_Form", "Third.Person",
    "Positive.Emotion", "Negative.Emotion",
    "Gratitude", "Apology", "Truth.Intensifier",
    "Affirmation", "Conjunction.Start"
  )
  
  for (feature in expected_features) {
    expect_true(feature %in% colnames(df_polite), 
                info = paste("Missing feature:", feature))
  }
})
