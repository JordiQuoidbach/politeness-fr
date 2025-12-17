#' Politeness Features (French)
#'
#' @description Detects linguistic markers of politeness in French natural language.
#'     This function is adapted for French texts, taking an N-length vector of text documents and returning an N-row data.frame of feature counts.
#' @param text character A vector of texts, each of which will be tallied for politeness features.
#' @param parser character Name of dependency parser to use (see details). Without a dependency parser, some features will be approximated, while others cannot be calculated at all.
#' @param metric character What metric to return? Raw feature count totals, Binary presence/absence of features, or feature counts per 100 words. Default is "count".
#' @param drop_blank logical Should features that were not found in any text be removed from the data.frame? Default is FALSE
#' @param num_mc_cores integer Number of cores for parallelization. Default is 1, but we encourage users to try parallel::detectCores() if possible.
#' @details Some politeness features depend on part-of-speech tagged sentences (e.g. "bare commands" are a particular verb class).
#'     To include these features in the analysis, a POS tagger must be initialized beforehand - we currently support SpaCy which must
#'     be installed separately in Python (see example for implementation). For French, use the French language model: spacyr::spacy_initialize(model = "fr_core_news_sm")
#' @return a data.frame of politeness features, with one row for every item in `text`. Possible politeness features are listed in \code{\link{feature_table}}
#' @references
#' Brown, P., & Levinson, S. C. (1987). Politeness: Some universals in language usage (Vol. 4). Cambridge university press.
#'
#' Danescu-Niculescu-Mizil, C., Sudhof, M., Jurafsky, D., Leskovec, J., & Potts, C. (2013). A computational approach to politeness with application to social factors. arXiv preprint arXiv:1306.6078.
#'
#' @examples
#'
#' # Example with French text
#' text_fr <- c("Bonjour, pourriez-vous m'aider s'il vous plaît ?",
#'              "Salut, tu peux m'aider ?")
#'
#' politeness_fr(text_fr, parser="none", drop_blank=FALSE)
#'
#' \dontrun{
#' # Detect multiple cores automatically for parallel processing
#' politeness_fr(text_fr, num_mc_cores=parallel::detectCores())
#'
#' # Connect to SpaCy installation for part-of-speech features (French model)
#' install.packages("spacyr")
#' spacyr::spacy_initialize(model = "fr_core_news_sm")
#' politeness_fr(text_fr, parser="spacy", drop_blank=FALSE)
#' }
#'
#' @export

politeness_fr<-function(text, parser=c("none","spacy"),
                        metric=c("count","binary","average"),
                        drop_blank=FALSE,
                        num_mc_cores=1){

  text=as.character(unlist(text))
  text[is.na(text)]<-" "
  text=strip_bracket_tags(text)
  text<-paste(text," ")

  ########################################################
  # Generates broad token lists for feature creation below
  if(length(text)<2000){
    sets<-getTokenSets_fr(text=text,parser=parser[1],num_mc_cores=num_mc_cores)
  } else{
    # Batched loop to minimize memory load on SpaCy for big files
    textList<-split(text, ceiling(seq_along(text)/1000))
    setList<-lapply(1:length(textList),function(x) NA)
    sets<-list()
    tpb<-utils::txtProgressBar(0,length(textList))
    for (x in 1:length(textList)){
      setList[[x]]<-getTokenSets_fr(text=textList[[x]],parser=parser[1],num_mc_cores=num_mc_cores)
      utils::setTxtProgressBar(tpb,x)
    }
    sets[["dicts"]]<-do.call(rbind,lapply(setList,function(x) x[["dicts"]]))
    .names<-names(setList[[1]])[names(setList[[1]])!="dicts"]
    for(n in .names){
      sets[[n]]<-do.call(c,lapply(setList,function(x) x[[n]]))
    }
  }
  ########################################################
  # French dictionaries are loaded from package internal data (sysdata.rda)
  # See dictTools_fr.R for dictionary definitions
  ########################################################
  
  features<-list()
  features[["Hedges"]]<-textcounter(hedge_list_fr,sets[["c.words"]],words=TRUE, num_mc_cores=num_mc_cores)
  features[["Impersonal.Pronoun"]]<-sets[["dicts"]][,"Pronouns"]
  features[["Swearing"]]<-sets[["dicts"]][,"Swearing"]
  features[["Negation"]]<-sets[["dicts"]][,"Negation"]
  features[["Filler.Pause"]]<-sets[["dicts"]][,"FilledPause"]
  features[["Informal.Title"]]<-sets[["dicts"]][,"InformalTitle"]
  features[["Formal.Title"]]<-sets[["dicts"]][,"FormalTitle"]

  # Conditional requests - French equivalents
  could_you_keys <- normalize_phrase_fr(c("pourriez-vous", "pourriez vous", "pourrions-nous", "pourrions nous",
                                          "pourrais-je", "pourrais je", "pourrait-on", "pourrait on",
                                          "auriez-vous", "auriez vous", "aurions-nous", "aurions nous",
                                          "voudriez-vous", "voudriez vous",
                                          "si ça ne vous dérange pas", "si ça vous dérange pas",
                                          "si ça ne te dérange pas", "si ça te dérange pas",
                                          "si ça ne nous dérange pas", "si ça nous dérange pas",
                                          "si vous voulez bien", "si tu veux bien", "si on veut bien", "si nous voulons bien",
                                          "ce serait possible de", "serait-il possible de", "serait il possible de",
                                          "j'aimerais", "je voudrais",
                                          "je me demandais si vous", "je me demandais si tu", "je me demandais si on", "je me demandais si nous",
                                          "est-ce que ça vous embête de", "est ce que ça vous embête de",
                                          "est-ce que ça t'embête de", "est ce que ça t'embête de",
                                          "est-ce que ça te embête de", "est ce que ça te embête de",
                                          "est-ce que ça nous embête de", "est ce que ça nous embête de"))
  features[["Could.You"]]<-textcounter(could_you_keys, sets[["clean"]], num_mc_cores=num_mc_cores)
  can_you_keys <- normalize_phrase_fr(c("pouvez-vous", "pouvez vous", "peux-tu", "peux tu",
                                        "peut-on", "peut on", "peuvent-ils", "peuvent ils",
                                        "voulez-vous", "voulez vous", "veux-tu", "veux tu",
                                        "quand vous pouvez", "quand tu peux", "quand on peut", "quand nous pouvons"))
  features[["Can.You"]]<-textcounter(can_you_keys, sets[["clean"]], num_mc_cores=num_mc_cores)

  by_the_way_keys <- normalize_phrase_fr(c("au fait", "d'ailleurs", "de plus", "en passant"))
  features[["By.The.Way"]]<-textcounter(by_the_way_keys, sets[["clean"]], num_mc_cores=num_mc_cores)
  let_me_know_keys <- normalize_phrase_fr(c("faites-moi savoir", "faites moi savoir", "dites-moi", "dites moi",
                                           "tenez-moi au courant", "tenez moi au courant"))
  features[["Let.Me.Know"]]<-textcounter(let_me_know_keys, sets[["clean"]], num_mc_cores=num_mc_cores)
  goodbye_keys <- normalize_phrase_fr(c("au revoir", "à bientôt", "à plus", "à plus tard",
                                        "salut", "ciao", "adieu"))
  features[["Goodbye"]]<-textcounter(goodbye_keys, sets[["clean"]], num_mc_cores=num_mc_cores)
  for_me_keys <- normalize_phrase_fr(c("pour moi", "pour nous", "à moi", "à nous"))
  features[["For.Me"]]<-textcounter(for_me_keys, sets[["clean"]], num_mc_cores=num_mc_cores)
  for_you_keys <- normalize_phrase_fr("pour vous")
  features[["For.You"]]<-textcounter(for_you_keys, sets[["clean"]], num_mc_cores=num_mc_cores)
  reasoning_keys <- normalize_phrase_fr(c("raison", "pourquoi je", "pourquoi nous", "pourquoi on",
                                          "expliquer", "explique", "causé", "parce que",
                                          "car", "étant donné que", "vu que"))
  features[["Reasoning"]]<-textcounter(reasoning_keys, sets[["clean"]], num_mc_cores=num_mc_cores)

  contrast_conjunction_keys <- normalize_phrase_fr(c("mais","cependant","pourtant",
                                                      "bien que","même si",
                                                      "malgré","néanmoins",
                                                      "toutefois","alors que"))
  features[["Contrast.Conjunction"]]<-textcounter(contrast_conjunction_keys, sets[["clean"]],
                                                  num_mc_cores=num_mc_cores)

  reassurance_keys <- normalize_phrase_fr(c("c'est bon", "c'est bien", "pas de problème",
                                             "pas de souci", "aucun problème", "c'est pas grave",
                                             "c'est rien", "tout va bien", "c'est ok",
                                             "pas grave", "ça va",
                                             "ne vous inquiétez pas", "vous inquiétez pas",
                                             "ne t'inquiète pas", "t'inquiète pas", "ne te inquiète pas", "te inquiète pas",
                                             "ça va aller",
                                             "c'est normal",
                                             "je comprends que", "je comprends votre inquiétude", "je comprends ton inquiétude", "je comprends ta inquiétude",
                                             "je vous rassure", "je te rassure",
                                             "vous en faites pas", "ne vous en faites pas",
                                             "t'en fais pas", "ne t'en fais pas", "te en fais pas", "ne te en fais pas",
                                             "on va s'en occuper", "on va gérer", "on va voir ça",
                                             "nous allons s'en occuper", "nous allons gérer", "nous allons voir ça",
                                             "nous allons nous en occuper"))
  features[["Reassurance"]]<-textcounter(reassurance_keys, sets[["clean"]],
                                         num_mc_cores=num_mc_cores)
  
  # Check-for-understanding / Shared decision-making patterns
check_understanding_keys <- normalize_phrase_fr(c(
  # checks / alignment
  "d'accord",
  "ok", "okay",
  "ça marche", "ça roule",
  "ça vous va", "si ça vous va", "ça te va", "si ça te va",
  "ça nous va", "si ça nous va",
  "ça vous convient", "ça te convient", "ça nous convient",
  "ça vous irait", "ça t irait", "ça nous irait",
  "c'est clair",

  # understanding checks
  "vous voyez", "tu vois",
  "vous comprenez", "tu comprends",
  "vous suivez", "tu suis",
  "vous avez compris", "t as compris",
  "c'est bon pour vous", "c'est bon pour toi",

  # preference / SDM prompts
  "qu'est ce que vous en pensez", "qu'est ce que tu en penses",
  "vous en pensez quoi", "tu en penses quoi",
  "vous préférez", "tu préfères",
  "vous êtes d'accord", "t es d'accord",
  "on part la dessus", "on fait comme ça"
))

features[["Check.Understanding"]] <- textcounter(
  check_understanding_keys, sets[["clean"]], num_mc_cores = num_mc_cores
)

  
  ask_agency_keys <- normalize_phrase_fr(c("me faire une faveur", "me permettre", "me laisser",
                                           "puis-je", "puis je", "est-ce que je peux",
                                           "est-ce que je pourrais", "devrais-je", "devrais je",
                                           "pourrais-je", "pourrais je"))
  features[["Ask.Agency"]]<-textcounter(ask_agency_keys, sets[["clean"]],
                                        num_mc_cores=num_mc_cores)
  give_agency_keys <- normalize_phrase_fr(c("vous laisser", "te laisser", "vous permettre",
                                           "te permettre", "vous pouvez", "tu peux",
                                           "vous pourriez", "tu pourrais"))
  features[["Give.Agency"]]<-textcounter(give_agency_keys, sets[["clean"]],
                                         num_mc_cores=num_mc_cores)

  hello_clean_keys <- normalize_phrase_fr(c("bon matin", "bonne matinée", "bon après-midi",
                                            "bonne après-midi", "bonne soirée", "bonne nuit"))
  features[["Hello"]]<-(textcounter(c("salut","bonjour","bonsoir","coucou","hey","allô"),sets[["c.words"]],words=TRUE,
                                    num_mc_cores=num_mc_cores)+
                          textcounter(hello_clean_keys, sets[["clean"]],
                                      num_mc_cores=num_mc_cores))

  please_keys <- normalize_phrase_fr(c("s'il vous plaît", "s'il vous plait", "s'il te plaît",
                                       "s'il te plait", "svp", "stp"))
  features[["Please"]]<-textcounter(please_keys, sets[["clean"]], num_mc_cores=num_mc_cores)

  # First.Person.Plural includes "on" since in modern French it often replaces "nous"
  features[["First.Person.Plural"]]<-textcounter(c("nous", "notre", "nos", "nous-mêmes", "on"),sets[["c.words"]],words=TRUE,
                                                 num_mc_cores=num_mc_cores)

  features[["First.Person.Single"]]<-textcounter(c("je","me","moi","mon","ma","mes","mien","mienne"),sets[["c.words"]],words=TRUE,
                                                 num_mc_cores=num_mc_cores)
  
  # Imparfait de politesse (imperfect tense for politeness)
  # French uses imperfect tense to soften requests: "Je voulais vous demander" vs "Je veux vous demander"
  imparfait_politesse_keys <- normalize_phrase_fr(c(
    "je voulais", "je souhaitais", "je désirais", "je pensais",
    "je cherchais", "je demandais", "je proposais", "je suggérais",
    "nous voulions", "nous souhaitions", "nous désirions", "nous pensions",
    "nous cherchions", "nous demandions", "nous proposions", "nous suggérions",
    "on voulait", "on souhaitait", "on désirait", "on pensait"
  ))
  features[["Imparfait.Politesse"]] <- textcounter(imparfait_politesse_keys, sets[["clean"]], 
                                                     num_mc_cores=num_mc_cores)
  
  # Conditionnel de politesse (conditional tense for politeness)
  # Systematic detection of conditional tense used for polite requests
  conditionnel_politesse_keys <- normalize_phrase_fr(c(
    # First person singular
    "j'aimerais", "je voudrais", "je souhaiterais", "je préférerais",
    "je pourrais", "je devrais", "je ferais", "je dirais",
    # First person plural
    "nous aimerions", "nous voudrions", "nous souhaiterions", "nous préférerions",
    "nous pourrions", "nous devrions", "nous ferions", "nous dirions",
    # Third person (impersonal)
    "il faudrait", "ce serait", "ça serait", "il serait",
    "on aimerait", "on voudrait", "on souhaiterait", "on préférerait",
    "on pourrait", "on devrait", "on ferait"
  ))
  features[["Conditional.Politeness"]] <- textcounter(conditionnel_politesse_keys, sets[["clean"]], 
                                                        num_mc_cores=num_mc_cores)

  # Tutoiement vs Vouvoiement (core French politeness distinction)
  # T_Form: informal "tu" forms (tutoiement)
  features[["T_Form"]]<-textcounter(c("tu","te","toi","ton","ta","tes","tien","tienne"),
                                    sets[["c.words"]],words=TRUE,
                                    num_mc_cores=num_mc_cores)
  # V_Form: formal "vous" forms (vouvoiement)
  features[["V_Form"]]<-textcounter(c("vous","votre","vos","vôtre","vôtres"),
                                    sets[["c.words"]],words=TRUE,
                                    num_mc_cores=num_mc_cores)

  features[["Third.Person"]]<-textcounter(c("il","lui","son","sa","ses","sien","sienne",
                                            "elle","son","sa","ses","sien","sienne",
                                            "ils","elles","eux","leur","leurs","eux-mêmes",
                                            "elles-mêmes"),sets[["c.words"]],words=TRUE,
                                          num_mc_cores=num_mc_cores)
  # Question detection: use spacy question_counts if available (includes heuristics),
  # otherwise use punctuation + heuristics on normalized text
  if(parser[1]=="spacy" && "question_counts" %in% names(sets)){
    features[["Questions"]]<-sets[["question_counts"]]
  } else {
    # Non-spacy case: count "?" and add heuristics
    question_punct <- textcounter("?",text, num_mc_cores=num_mc_cores)
    # Heuristics: "est ce que" / "est-ce que" and inversion patterns
    est_ce_que_keys <- normalize_phrase_fr(c("est ce que", "est-ce que"))
    est_ce_que_count <- textcounter(est_ce_que_keys, sets[["clean"]], num_mc_cores=num_mc_cores)
    # Inversion patterns (verb + pronoun)
    inversion_keys <- normalize_phrase_fr(c("pouvez vous", "avez vous", "voulez vous", "pourriez vous",
                                            "peux tu", "as tu", "veux tu", "pourrais tu",
                                            "peut on", "a on", "veut on", "pourrait on",
                                            "peuvent ils", "ont ils", "veulent ils", "pourraient ils"))
    inversion_count <- textcounter(inversion_keys, sets[["clean"]], num_mc_cores=num_mc_cores)
    features[["Questions"]]<-question_punct + est_ce_que_count + inversion_count
  }

  #if(parser[1]=="none"){
  if(parser[1]!="spacy"){
    if(length(parser)==2){
      cat("Warning: Please install and initialize SpaCy with French model (fr_core_news_sm), using the spacyr R package. This is an INCOMPLETE version of the package.")
    }
    features[["Positive.Emotion"]]<-textcounter(positive_list_fr,sets[["c.words"]],words=TRUE, num_mc_cores=num_mc_cores)
    features[["Negative.Emotion"]]<-textcounter(negative_list_fr,sets[["c.words"]],words=TRUE, num_mc_cores=num_mc_cores)
    features[["Gratitude"]]<-(unlist(lapply(sets[["c.words"]], function(x) sum(startsWith(unlist(x), prefix="merci"))))+
                                unlist(lapply(sets[["c.words"]], function(x) sum(startsWith(unlist(x), prefix="remerci"))))+
                                unlist(lapply(sets[["c.words"]], function(x) sum(startsWith(unlist(x), prefix="gratitude")))))
    features[["Apology"]]<-textcounter(c("désolé","désolée","désolés","désolées",
                                         "excuse","excuses","excuser","excusez",
                                         "pardon","navré","navrée","navrés","navrées",
                                         "désolant"),sets[["c.words"]],words=TRUE,
                                       num_mc_cores=num_mc_cores)
    truth_intensifier_clean_keys <- normalize_phrase_fr(c("en fait", "en réalité", "à vrai dire"))
    features[["Truth.Intensifier"]]<-(textcounter(c("vraiment", "réellement", "honnêtement", "sûrement",
                                                     "effectivement", "en fait", "en réalité"),sets[["c.words"]],words=TRUE,
                                                  num_mc_cores=num_mc_cores)+
                                        textcounter(truth_intensifier_clean_keys, sets[["clean"]],num_mc_cores=num_mc_cores))
  } else {
    # With SpaCy parser - more sophisticated features
    features[["Positive.Emotion"]]<-(textcounter(positive_list_fr,sets[["unneg.words"]],words=TRUE, num_mc_cores=num_mc_cores)
                                     +textcounter(negative_list_fr,sets[["neg.words"]],words=TRUE, num_mc_cores=num_mc_cores))
    features[["Negative.Emotion"]]<-(textcounter(positive_list_fr,sets[["neg.words"]],words=TRUE, num_mc_cores=num_mc_cores)
                                     +textcounter(negative_list_fr,sets[["unneg.words"]],words=TRUE, num_mc_cores=num_mc_cores))

    # Agreement/Disagreement - French patterns would need to be adapted based on French syntax
    # This is a simplified version - full implementation would require French dependency patterns
    agreement_keys <- normalize_phrase_fr(c("d'accord", "d accord", "je suis d'accord",
                                           "nous sommes d'accord", "c'est vrai", "c est vrai",
                                           "c'est correct", "c est correct", "c'est juste",
                                           "c est juste", "bonne idée", "excellente idée",
                                           "bon point", "excellent point"))
    features[["Agreement"]]<-textcounter(agreement_keys, sets[["clean"]],
                                         num_mc_cores=num_mc_cores)

    disagreement_keys <- normalize_phrase_fr(c("pas d'accord", "pas d accord", "je ne suis pas d'accord",
                                                "nous ne sommes pas d'accord", "c'est faux", "c est faux",
                                                "c'est incorrect", "c est incorrect", "c'est faux",
                                                "mauvaise idée", "mauvais point"))
    features[["Disagreement"]]<-textcounter(disagreement_keys, sets[["clean"]],
                                            num_mc_cores=num_mc_cores)

    acknowledgement_keys <- normalize_phrase_fr(c("je comprends", "je vois", "je saisis",
                                                  "je reconnais", "j'entends", "je saisis",
                                                  "nous comprenons", "nous voyons", "nous saisissons",
                                                  "je prends note", "nous prenons note"))
    features[["Acknowledgement"]]<-textcounter(acknowledgement_keys, sets[["clean"]],
                                               num_mc_cores=num_mc_cores)

    subjectivity_keys <- normalize_phrase_fr(c("je pense", "je crois", "je suppose",
                                               "je pensais", "je croyais", "je sentais",
                                               "mon avis", "ma croyance", "mon point de vue",
                                               "ma connaissance", "mon opinion",
                                               "nous pensons", "nous croyons", "nous supposons",
                                               "notre avis", "notre croyance", "notre point de vue",
                                               "notre opinion"))
    features[["Subjectivity"]]<-textcounter(subjectivity_keys, sets[["clean"]],
                                            num_mc_cores=num_mc_cores)

    # Bare commands - French imperative mood
    # Checks for sentences starting with a verb (bare commands like "Fais ça")
    # Format of pos.nums: (token_id-token-lemma-VERB)
    features[["Bare.Command"]]<-unlist(lapply(sets[["pos.nums"]],function(x) {
      pos_nums_list <- unlist(x)
      # Find first token (token_id == 1) that is a VERB
      first_token_verbs <- pos_nums_list[grepl("^\\(1-", pos_nums_list) & 
                                           grepl("-VERB\\)$", pos_nums_list)]
      if(length(first_token_verbs) == 0) return(0)
      # Exclude certain verb lemmas (être, avoir, espérer, excuser, remercier)
      excluded_lemmas <- c("être", "avoir", "espérer", "excuser", "remercier")
      # Check each verb: extract lemma and verify it's not excluded
      valid_commands <- sapply(first_token_verbs, function(verb_str) {
        # Format: (1-token-lemma-VERB)
        # Use regex to capture groups instead of splitting by dash (safer for hyphens in tokens/lemmas)
        # Pattern: (1-TOKEN-LEMMA-VERB) where TOKEN and LEMMA can contain hyphens
        match_result <- regmatches(verb_str, regexec("^\\(([0-9]+)-(.+)-(.+)-(VERB)\\)$", verb_str))[[1]]
        if(length(match_result) < 5) return(FALSE)
        # match_result[1] = full match, match_result[2] = token_id, match_result[3] = token, 
        # match_result[4] = lemma, match_result[5] = pos
        lemma <- tolower(match_result[4])
        # Check if lemma is not in exclusion list
        !(lemma %in% excluded_lemmas)
      })
      sum(valid_commands)
    }))

    ######### QUESTION TYPES #########
    q.words<-c("qui","quoi","où","quand","pourquoi","comment","quel","quelle","quels","quelles","lequel","laquelle") # French question words
    features[["WH.Questions"]]<-(unlist(lapply(sets[["ques.pre.root"]],
                                               function(x) sum(textcounter(q.words,x))))
    )
    features[["Repair.Questions"]]<-textcounter(c("punct(qui, ?)","punct(quoi, ?)","punct(pardon, ?)","punct(désolé, ?)",
                                                  "punct(hein, ?)","punct(comment, ?)"),
                                                sets[["p.nonum"]], words=TRUE,
                                                num_mc_cores=num_mc_cores)

    tag_questions_keys <- normalize_phrase_fr(c("n'est-ce pas?", "n est ce pas?",
                                                "tu vois?", "vous voyez?",
                                                "d'accord?", "d accord?"))
    features[["Tag.Questions"]]<-unlist(lapply(text,
                                               function(x) sum(textcounter(tag_questions_keys, x,
                                                                           num_mc_cores=num_mc_cores))))

    features[["YesNo.Questions"]]<-(features[["Questions"]]
                                    -features[["WH.Questions"]]
                                    -features[["Repair.Questions"]]
                                    -features[["Tag.Questions"]]
    )
    features[["Questions"]]<-NULL
    #########################################

    features[["Gratitude"]]<-(unlist(lapply(sets[["c.words"]], function(x) sum(startsWith(unlist(x), prefix="merci"))))+
                                unlist(lapply(sets[["c.words"]], function(x) sum(startsWith(unlist(x), prefix="remerci"))))+
                                unlist(lapply(sets[["c.words"]], function(x) sum(startsWith(unlist(x), prefix="gratitude"))))+
                                unlist(lapply(sets[["p.unnegs"]], function(x) sum(grepl("(apprécier, nous)",x,fixed=TRUE))))+
                                unlist(lapply(sets[["p.unnegs"]], function(x) sum(grepl("(apprécier, je)",x,fixed=TRUE)))))

    features[["Apology"]]<-(textcounter(c("oups","zut","mince"),sets[["unneg.words"]],words=TRUE,
                                        num_mc_cores=num_mc_cores)
                            +textcounter(c("acomp(suis, désolé)","acomp(est, désolé)",
                                           "root(root, désolé)",
                                           "nsubj(excuser, nous)","nsubj(excuser, je)",
                                           "nsubj(regretter, je)", "nsubj(regretter, nous)",
                                           "dobj(excuser, moi)",
                                           "poss(pardon, votre)",
                                           "dobj(pardonner, moi)"),sets[["p.unnegs"]], words=TRUE,
                                         num_mc_cores=num_mc_cores)
                            # Additional French apology patterns
                            +textcounter(normalize_phrase_fr(c("je suis désolé", "je suis désolée", "nous sommes désolés",
                                                              "je m'excuse", "nous nous excusons")),sets[["clean"]],
                                        num_mc_cores=num_mc_cores))

    features[["Truth.Intensifier"]]<-(textcounter(c("vraiment", "réellement", "honnêtement", "sûrement",
                                                    "effectivement"),sets[["c.words"]],words=TRUE,
                                                  num_mc_cores=num_mc_cores)
                                      +textcounter(c("det(point, le)","det(réalité, la)","det(vérité, la)","pobj(fait, en)","case(fait, en)"),sets[["p.nonum"]], words=TRUE,
                                                   num_mc_cores=num_mc_cores))
    features[["Adverb.Limiter"]]<-unlist(lapply(sets[["p.nonum"]] ,function(x) sum(grepl("advmod",unlist(x))&
                                                                                     (grepl("juste)",unlist(x),fixed=TRUE)
                                                                                      |grepl("seulement)",unlist(x),fixed=TRUE)
                                                                                      |grepl("uniquement)",unlist(x),fixed=TRUE)
                                                                                      |grepl("simplement)",unlist(x),fixed=TRUE)))
    ))
  }
  features[["Affirmation"]]<-textcounter(c("oui","ok","okay","parfait","bien","génial","super","formidable",
                                           "d'accord","entendu","absolument","exactement","tout à fait",
                                           "voilà","certes","effectivement","précisément",
                                           "bien sûr","évidemment","naturellement","volontiers"),
                                         sets[["c.words"]],words=TRUE,start=TRUE,
                                         num_mc_cores=num_mc_cores)

  # listed at the end for backward compatibility
  features[["Conjunction.Start"]]<-textcounter(c("donc","alors","et","mais","ou","cependant"),
                                               sets[["c.words"]],words=TRUE,start=TRUE,
                                               num_mc_cores=num_mc_cores)

  if(metric[1]=="binary"){
    features<-parallel::mclapply(features, function(x) 1*(x>0), mc.cores=num_mc_cores)
  } else if (metric[1]=="average"){
    word_counts <- stringr::str_count(text, "[[:alpha:]]+")
    features<-parallel::mclapply(features, function(x) x*100/word_counts, mc.cores=num_mc_cores)
  }
  feature.data<-as.data.frame(features)
  feature.data[feature.data<0]<-0
  if(drop_blank){
    feature.data<-feature.data[,colMeans(feature.data,na.rm=TRUE)!=0, drop=FALSE]
  }
  return(feature.data)
}

