#' French Dictionaries for Politeness Features
#' 
#' @description French-language dictionaries for politeness feature extraction.
#' These dictionaries are loaded as internal package data via sysdata.rda.
#' 
#' @name french_dictionaries
#' @keywords internal
NULL

# =============================================================================
# FRENCH HEDGE WORDS (mots d'atténuation)
# =============================================================================
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
  # multiword vocatives (low false-positive)
  "mon pote", "ma pote",
  "mon gars",
  "mon vieux", "ma vieille",
  "mon ami", "mon amie",
  "ma belle", "mon beau",
  "mon grand", "ma grande",
  "les gars", "les amis"
),

# single-token vocatives ONLY counted at start-of-turn (to avoid "un mec", "des mecs")
InformalTitleStartWords = c(
  "mec", "pote",
  "frero", "fréro",
  "frerot", "frérot",
  "frangin",
  "bro",
  "boss"
),

# interjection + vocative (captures mid-turn addressing without counting "un mec")
InformalTitleInterjectionPhrases = c(
  "eh mec", "hé mec", "hey mec", "yo mec",
  "eh pote", "hé pote", "hey pote", "yo pote",
  "eh copain", "hé copain", "hey copain", "yo copain",
  "eh bro", "hé bro", "hey bro", "yo bro",
  "eh frero", "hé frero", "hey frero", "yo frero", "eh frérot", "hé frérot", "hey frérot", "yo frérot",
  "eh frere", "hé frere", "hey frere", "yo frere", "eh frére", "hé frére", "hey frére", "yo frére",
  "wesh mec", "wesh frero", "wesh frérot", "wesh frere", "wesh frére", "wesh bro",
  "ouais bro", "ouais frero", "ouais frérot", "ouais frere", "ouais frére"
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
  # core profanity
  "merde","putain","ptain","p tain","bordel","chiant","chiante","chiants","chiantes",
  "fait chier","faire chier","fait iech","saoule","saoulant","saoulante",
  "putain de","bordel de merde","merdier","merdique","saloperie",

  # "con" family (very common)
  "con","cons","conne","connes",
  "connard","connards","connasse","connasses",
  "connerie","conneries",

  # stupidity/insults (common in workplace + patients)
  "idiot","idiots","idiote","idiotes",
  "imbecile","imbeciles","imbécile","imbéciles",
  "debil","debile","debiles","débile","débiles",
  "cretin","cretins","cretine","cretines","crétin","crétins","crétine","crétines",
  "abruti","abrutis","abrutie","abruties",
  "boulet","boulets","gogole","gogoles","bouffon","bouffons","bouffonne","bouffonnes",
  "clown","clowns","guignol","guignols",
  "naze","nul","nulle","nuls","nulles","minable","minables",

  # vulgar/sexual insults (strong)
  "encule","encules","enculee","enculees","enculé","enculés","enculée","enculées",
  "pute","putes",
  "salope","salopes","salaud","salauds","salopard","salopards","ordure","ordures",
  "batard","batards","batarde","batardes","bâtard","bâtards","bâtarde","bâtardes",

  # very common hostile imperatives / phrases
  "ta gueule","ferme ta gueule","tg","ftg",
  "va te faire foutre","va te faire voir","va te faire mettre",
  "casse toi","degage","dégage","barre toi","barre toi de la",
  "nique","nique ta mere","nique ta mère","ntm",
  "fils de pute","fdp",
  "sale con","sale conne","gros con","grosse conne","sac a merde","sac à merde",
  "enfoire",

  # “couilles” family (very spoken)
  "couille","couilles","casse couilles","casse couille","casse-couilles","casse-couille"
  )
)

# =============================================================================
# REBUILD SYSDATA SCRIPT
# =============================================================================
# To add French dictionaries to sysdata.rda, run this in the package directory:
# 
# # Load existing internal data
# load("R/sysdata.rda")
# 
# # Source the French dictionaries
# source("R/dictTools_fr.R")
# 
# # Save all internal data (existing + French)
# usethis::use_data(
#   hedge_list,
#   positive_list,
#   negative_list,
#   polite_dicts,
#   receptive_model,
#   hedge_list_fr,
#   positive_list_fr,
#   negative_list_fr,
#   polite_dicts_fr,
#   internal = TRUE,
#   overwrite = TRUE
# )
