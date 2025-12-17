# Script to rebuild sysdata.rda with French dictionaries
# Run from the package root directory: source("rebuild_sysdata.R")

# Load existing internal data first
load("R/sysdata.rda")

# Check what variables exist
cat("Existing internal data objects:\n")
print(ls())

# =============================================================================
# FRENCH HEDGE WORDS (mots d'atténuation)
# =============================================================================
hedge_list_fr <- c(
  # Adverbs of uncertainty
  "peut-être", "probablement", "sans doute", "apparemment", "vraisemblablement",
  "peut être", "semble", "semblerait", "sembler", "paraît", "paraîtrait",
  # Cognitive verbs (conjugated)
  "penser", "pensais", "pensait", "pensons", "pensez", "pensent",
  "croire", "croyais", "croyait", "croyons", "croyez", "croient",
  "supposer", "supposais", "supposait", "supposons", "supposez", "supposent",
  "imaginer", "imaginais", "imaginait", "imaginons", "imaginez", "imaginent",
  # Approximators
  "presque", "quasiment", "à peu près", "environ", "approximativement",
  "un peu", "un tant soit peu", "légèrement", "relativement", "assez",
  "plutôt", "quelque peu", "plus ou moins", "en quelque sorte",
  # Epistemic phrases
  "je crois", "je pense", "je suppose", "je trouve", "je trouve que",
  "il me semble", "il semble", "on dirait", "on aurait dit",
  "à mon avis", "selon moi", "pour moi", "de mon point de vue"
)

# =============================================================================
# FRENCH POSITIVE EMOTION WORDS (mots d'émotion positive)
# =============================================================================
positive_list_fr <- c(
  # Quality adjectives
  "bon", "bonne", "bons", "bonnes", "bien", "mieux", "meilleur", "meilleure",
  "excellent", "excellente", "excellents", "excellentes", "parfait", "parfaite",
  "merveilleux", "merveilleuse", "merveilleuses",
  "formidable", "fantastique", "génial", "géniale", "super", "superbe",
  "magnifique", "splendide", "extraordinaire", "remarquable",
  # Pleasant states
  "agréable", "plaisant", "plaisir", "joie", "heureux", "heureuse",
  "content", "contente", "satisfait", "satisfaite", "ravi", "ravie",
  "enthousiaste", "enthousiasme", "réjoui", "réjouie",
  # Positive verbs (conjugated)
  "aimer", "aime", "aimes", "aimons", "aimez", "aiment", "aimé", "aimée",
  "adorer", "adore", "adores", "adorons", "adorez", "adorent", "adoré", "adorée",
  "apprécier", "apprécie", "apprécies", "apprécions", "appréciez", "apprécient",
  # Gratitude
  "merci", "remercier", "remercie", "remercies", "remercions", "remerciez", "remercient",
  "gratitude", "reconnaissant", "reconnaissante", "reconnaissance",
  # Pride
  "fier", "fière", "fiers", "fières", "fierté",
  # Congratulations
  "bravo", "félicitations", "féliciter", "félicite", "félicites",
  # Success
  "succès", "réussir", "réussit", "réussis", "réussissons", "réussissez", "réussissent",
  "victoire", "gagner", "gagne", "gagnes", "gagnons", "gagnez", "gagnent",
  # Hope
  "espoir", "espérer", "espère", "espères", "espérons", "espérez", "espèrent",
  "optimiste", "optimisme", "positif", "positive"
)

# =============================================================================
# FRENCH NEGATIVE EMOTION WORDS (mots d'émotion négative)
# =============================================================================
negative_list_fr <- c(
  # Negative quality
  "mauvais", "mauvaise", "mauvaises", "mal", "pire", "pires",
  "terrible", "horrible", "affreux", "affreuse", "atroce", "atroces",
  "désagréable", "déplaisant", "déplaisante", "désagréablement",
  # Sad states
  "triste", "tristes", "tristesse", "malheureux", "malheureuse",
  "déçu", "déçue", "déçus", "déçues", "déception", "décevoir",
  "mécontent", "mécontente", "mécontents", "mécontentes", "mécontentement",
  # Anger
  "fâché", "fâchée", "fâchés", "fâchées", "fâcher", "fâche", "fâches",
  "en colère", "colère", "colérique", "irrité", "irritée", "irriter",
  # Hate/disgust
  "détester", "déteste", "détestes", "détestons", "détestez", "détestent",
  "haïr", "hais", "hait", "haïssons", "haïssez", "haïssent",
  "dégoût", "dégoûter", "dégoûte", "dégoûtes", "dégoûtant", "dégoûtante",
  # Fear/anxiety
  "peur", "craindre", "craint", "craignons", "craignez", "craignent",
  "inquiet", "inquiète", "inquiets", "inquiètes", "inquiétude", "inquiéter",
  "anxieux", "anxieuse", "anxiété", "anxieusement",
  # Despair
  "désespéré", "désespérée", "désespérer", "désespoir",
  "déprimé", "déprimée", "déprimer", "dépression",
  # Fatigue/boredom
  "fatigué", "fatiguée", "fatiguer", "fatigue",
  "ennuyé", "ennuyée", "ennuyer", "ennui", "ennuyeux", "ennuyeuse",
  # Problems
  "problème", "problèmes", "difficulté", "difficultés",
  # Failure
  "échec", "échouer", "échoue", "échoues", "échouons", "échouez", "échouent",
  "perdre", "perd", "perds", "perdons", "perdez", "perdent", "perdu", "perdue",
  "faillir", "failli", "faillis", "faillons", "faillez", "faillent"
)

# =============================================================================
# FRENCH POLITENESS DICTIONARIES (for quanteda)
# =============================================================================
polite_dicts_fr <- list(
  Negation = c(
    "ne", "n'", "non", "pas", "jamais", "rien", "personne", "aucun", "aucune",
    "aucuns", "aucunes", "nul", "nulle", "nuls", "nulles", "ni", "sans"
  ),
  FilledPause = c(
    "euh", "euh*", "hem", "hem*", "hmm", "hmm*", "ah", "oh", "bah", "ben",
    "eh", "hein", "hum", "mh", "mmh"
  ),
  InformalTitle = c(
    "mec", "mec*", "pote", "pote*", "copain", "copine", "poteau", "poteau*",
    "camarade", "ami", "amie", "gars", "gars*", "fille", "fille*",
    "mon pote", "ma pote", "mon gars", "mon vieux", "ma vieille"
  ),
  FormalTitle = c(
    "monsieur", "m.", "madame", "mme", "mademoiselle", "mlle",
    "monsieur*", "madame*", "mademoiselle*", "sieur", "dame",
    "docteur", "docteure", "docteurs", "docteures", "dr",
    "professeur", "professeure", "professeurs", "professeures", "prof"
  ),
  Pronouns = c(
    "on", "nous", "vous", "ils", "elles", "eux", "soi", "soi-même"
  ),
  Swearing = c(
    "merde", "putain", "con", "connard", "connasse", "salope", "salopard",
    "enculé", "enculée", "bordel", "foutre", "fout", "fous", "foutons",
    "foutez", "foutent", "crétin", "crétine", "idiot", "idiote", "imbécile",
    "abruti", "abrutie", "débile", "débiles"
  )
)

# =============================================================================
# SAVE ALL INTERNAL DATA
# =============================================================================
cat("\nSaving internal data with French dictionaries...\n")

# Use usethis to save - only include objects that exist
usethis::use_data(
  hedge_list,
  positive_list,
  negative_list,
  polite_dicts,
  hedge_list_fr,
  positive_list_fr,
  negative_list_fr,
  polite_dicts_fr,
  internal = TRUE,
  overwrite = TRUE
)

cat("\nDone! French dictionaries added to R/sysdata.rda\n")
cat("New internal data objects:\n")
load("R/sysdata.rda")
print(ls())
