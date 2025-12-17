
context("Rude vs Polite Verification")

test_that("politeness function detects differences between rude and polite texts", {

  texts <- c(
    "Could you please help me with this project? I would really appreciate it.",
    "Do this now. I don't care about your problems.",
    "Thank you so much for your assistance, it was very helpful.",
    "Your work is terrible and you are stupid."
  )

  # Expected: 1 and 3 are polite, 2 and 4 are rude.

  df_polite <- politeness(texts, parser="none", metric="count", drop_blank=FALSE)

  # Check dimensions
  expect_equal(nrow(df_polite), 4)

  # Check specific features
  # "Please" should be present in text 1
  expect_true(df_polite$Please[1] > 0)

  # "Positive.Emotion" should be higher in polite texts (1, 3) than rude ones (2, 4) generally
  # Note: "appreciate" might be gratitude, "helpful" positive.
  # "terrible", "stupid" are negative.

  expect_true(df_polite$Positive.Emotion[3] > 0)
  expect_true(df_polite$Negative.Emotion[4] > 0)

  # Gratitude
  expect_true(df_polite$Gratitude[3] > 0)

  # Bare Command (requires parser usually, but let's check what we can with parser="none")
  # Parser="none" approximates some things.

})
