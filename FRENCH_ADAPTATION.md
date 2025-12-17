# French Politeness Function Adaptation

This document describes the French adaptation of the politeness package.

## New Functions

### `politeness_fr()`
Main function for detecting politeness features in French texts. This is a French-language adaptation of the original `politeness()` function.

**Usage:**
```r
# Basic usage without SpaCy
text_fr <- c("Bonjour, pourriez-vous m'aider s'il vous plaît ?",
             "Salut, tu peux m'aider ?")
politeness_fr(text_fr, parser="none", drop_blank=FALSE)

# With SpaCy (requires French model)
# First initialize SpaCy with French model:
# spacyr::spacy_initialize(model = "fr_core_news_sm")
politeness_fr(text_fr, parser="spacy", drop_blank=FALSE)
```

## File Structure

### R Files
1. **R/politeness_fr.R** - Main French politeness function (exported)
2. **R/getTokenSets_fr.R** - Token extraction for French texts
3. **R/spacyParser_fr.R** - SpaCy parser adapted for French
4. **R/textTools_fr.R** - French text processing utilities (contractions, cleaning)
5. **R/dictTools_fr.R** - French dictionary definitions (source of truth)

### Internal Data
French dictionaries are stored in `R/sysdata.rda` along with the English dictionaries:
- `hedge_list_fr` - French hedging expressions
- `positive_list_fr` - French positive emotion words
- `negative_list_fr` - French negative emotion words
- `polite_dicts_fr` - French politeness dictionaries (for quanteda)

### Tests
- **tests/testthat/test-politeness_fr.R** - Comprehensive test suite for French functions

## French-Specific Adaptations

### Dictionaries
- **Hedge words**: French hedging expressions (e.g., "peut-être", "probablement", "je pense")
- **Positive/Negative emotions**: French emotion words
- **Politeness dictionaries**: French negation, filled pauses, titles, pronouns, swearing

### Text Processing
- **Contractions**: French contractions (e.g., "y'a" → "il y a", "chuis" → "je suis")
- **Stopwords**: Uses French stopwords from `tm::stopwords("french")`

### Unique French Features
- **T_Form**: Tutoiement (informal "tu" forms) - counts tu, te, toi, ton, ta, tes
- **V_Form**: Vouvoiement (formal "vous" forms) - counts vous, votre, vos
- This replaces the English "Second.Person" feature to capture the key French T-V distinction

### Politeness Features Adapted
- **Greetings**: "bonjour", "salut", "bonsoir", "coucou"
- **Please**: "s'il vous plaît", "s'il te plaît", "svp", "stp"
- **Conditional requests**: "pourriez-vous", "voudriez-vous", "serait-il possible de"
- **Pronouns**: French personal pronouns (je, tu, vous, nous, etc.)
- **Question words**: qui, quoi, où, quand, pourquoi, comment, quel, etc.
- **Apologies**: "désolé", "excusez-moi", "pardon", "navré"
- **Gratitude**: "merci", "remercier", "gratitude"

## Setup Requirements

### For Basic Usage (parser="none")
No additional setup required. Works out of the box.

### For Advanced Features (parser="spacy")
1. Install SpaCy in Python:
   ```bash
   pip install spacy
   python -m spacy download fr_core_news_sm
   ```

2. In R, initialize SpaCy with French model:
   ```r
   library(spacyr)
   spacyr::spacy_initialize(model = "fr_core_news_sm")
   ```

## Rebuilding Internal Data

If you need to modify the French dictionaries, edit `R/dictTools_fr.R` and then run:

```r
source("rebuild_sysdata.R")
```

This will update `R/sysdata.rda` with the new dictionary definitions.

## Testing

Run the French-specific tests:
```r
devtools::test(filter = "politeness_fr")
```

## Notes

- The function follows the same structure and feature set as the English version
- SpaCy-dependent features use French dependency patterns
- The dictionaries can be expanded based on your specific research needs
- French negation patterns (ne...pas) are handled in the parser
- UTF-8 encoding is handled automatically, but test files use ASCII-safe text to avoid encoding issues on different systems
