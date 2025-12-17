# Used to avoid incorrect notes of "no visible binding"
utils::globalVariables(c("l_parse_nums","parses",
                         "l_parses","p.nonums",
                         "l_pos_nums","pos.nums",
                         "l_w_nums","w.nums",
                         "doc_sent_id","pre_root","qwords","root_token_id","WH","WH_pre_root",
                         "doc_id","sentence_id",
                         "token_id","token", ".",
                         "raw_head_token_id",
                         "head_token","head_token_id",
                         "dep_rel", "tag","pos","lemma","question",
                         "token_next","token_next2","has_est_ce_que","has_inversion",
                         "is_inversion_verb","is_pronoun_next",
                         "anyNeg","parseNeg","negs",
                         "negP1","negP2","negP3","negP4",
                         "negM1","negM2",
                         "parseNeg1","parseNeg2","parseNeg3",
                         "selfies","selfscope",
                         "selfscope1","selfscope2"))

################################################################
# Workflow for SpaCy (French)
################################################################
# spacyr::spacy_initialize(model = "fr_core_news_sm")
################################################################

#' Spacy Parser (French)
#' @description Return POS tags from French natural language.
#' @param txt a character vector of texts.
#' @return list of compiled POS-tagged items.
#' @keywords internal
#' @import data.table
spacyParser_fr<- function(txt){
  # Note: French model must be initialized first: spacyr::spacy_initialize(model = "fr_core_news_sm")
  parsedtxt <- spacyr::spacy_parse(txt, dependency=TRUE,lemma=TRUE,pos=TRUE,tag=TRUE,entity=FALSE)
  parsedtxt$token<-tolower(parsedtxt$token)
  .ds<-paste0(parsedtxt$sentence_id,parsedtxt$doc_id)
  parsedtxt$question<-1*(.ds%in%(.ds[parsedtxt$token=="?"]))
  dt_parsedtxt <- data.table::data.table(parsedtxt)
  unique_doc_ids <- dt_parsedtxt[ , unique(doc_id)]
  dt_parsedtxt[ , doc_sent_id := paste(doc_id,sentence_id,sep="-")]
  dt_parsedtxt[ , doc_id := factor(doc_id, levels = unique_doc_ids)]
  dt_head_token <- dt_parsedtxt[ , .(doc_id, sentence_id, token_id,token)]
  setnames(dt_head_token, c("token_id","token"), c("head_token_id","head_token"))
  v_s_keys <-  c("doc_id", "sentence_id", "head_token_id" )
  setkeyv(dt_head_token, v_s_keys)
  setkeyv(dt_parsedtxt, v_s_keys)
  dt_parsedtxt <- dt_head_token[dt_parsedtxt] # left merge on dt_parsedtxt
  dt_parsedtxt[dep_rel=="ROOT" , head_token := "ROOT" ]
  dt_parsedtxt[dep_rel=="ROOT" , head_token_id := 0 ]
  dt_parsedtxt[dep_rel=="ROOT" , dep_rel := "root" ]
  dt_parsedtxt[,raw_head_token_id:=head_token_id]

  roottoken=dt_parsedtxt[dep_rel=="root",.(root_token_id = token_id, doc_sent_id)]
  dt_parsedtxt=roottoken[dt_parsedtxt,on="doc_sent_id"]
  dt_parsedtxt[,pre_root:=1*(token_id<root_token_id)]

  # Question heuristics (punctuation-free detection for spoken transcripts)
  # Check for "est ce que" / "est-ce que" pattern
  dt_parsedtxt[,token_next:=shift(token,-1,type="lead"),by=list(doc_id,sentence_id)]
  dt_parsedtxt[,token_next2:=shift(token,-2,type="lead"),by=list(doc_id,sentence_id)]
  dt_parsedtxt[,has_est_ce_que:=as.integer((token=="est" & token_next=="ce" & token_next2=="que"))]
  dt_parsedtxt[,has_est_ce_que:=max(has_est_ce_que,na.rm=TRUE),by=list(doc_id,sentence_id)]
  
  # Check for inversion patterns (verb + pronoun) at start of sentence
  # Common patterns: pouvez vous, avez vous, voulez vous, pourriez vous, etc.
  inversion_verbs <- c("pouvez","peux","peut","peuvent","avez","as","a","ont",
                       "voulez","veux","veut","veulent","pourriez","pourrais","pourrait",
                       "pourrions","pourraient","êtes","es","est","sont")
  pronouns <- c("vous","tu","nous","on","il","elle","ils","elles","je","me")
  dt_parsedtxt[,is_inversion_verb:=as.integer(token%in%inversion_verbs)]
  dt_parsedtxt[,is_pronoun_next:=as.integer(token_next%in%pronouns)]
  dt_parsedtxt[,has_inversion:=as.integer((is_inversion_verb==1 & token_id<=3 & is_pronoun_next==1))]
  dt_parsedtxt[,has_inversion:=max(has_inversion,na.rm=TRUE),by=list(doc_id,sentence_id)]
  
  # Update question flag: either has "?" OR matches heuristics
  dt_parsedtxt[,question:=pmax(question, has_est_ce_que, has_inversion, na.rm=TRUE)]
  
  # Clean up temporary columns
  dt_parsedtxt[,c("token_next","token_next2","is_inversion_verb","is_pronoun_next"):=NULL]

  # French question words
  dt_parsedtxt[,WH:=1*(token%in%c("qui","quoi","où","quand","pourquoi","comment","quel","quelle","quels","quelles","lequel","laquelle"))]
  dt_parsedtxt[,WH_pre_root:=1*(WH & pre_root)]

  Qset=dt_parsedtxt[WH_pre_root==1 & question==1,]
  Qset=Qset[!duplicated(doc_sent_id),c("doc_id","token")]

  Qset[,qwords:=paste(token,collapse=" "),by="doc_id"]
  Qset<-Qset[!duplicated(doc_id),c("doc_id","qwords")]

  blanks<-unique(dt_parsedtxt, by= "doc_id")

  Qwords<-Qset[blanks,c("doc_id","qwords"),on="doc_id"]
  Qwords[is.na(qwords),qwords:=""]

  ###### Construct Tag Sets
  # Include lemma for Bare.Command feature (needs lemma to filter excluded verbs)
  dt_parsedtxt[ , pos.nums := paste0("(",token_id,"-",token,"-",lemma,"-",pos,")")]
  dt_parsedtxt[ , parses := paste0(dep_rel, "(",head_token,"-",head_token_id,", ",token,"-",token_id,")")]
  dt_parsedtxt[ , p.nonums := paste0(dep_rel, "(",head_token,", ",token,")")]
  dt_parsedtxt[ , w.nums := paste0(token,"-",token_id)]

  all.parses <- dt_parsedtxt[ , .(l_parse_nums = list(parses)), keyby = "doc_id"][ , l_parse_nums]
  all.pos.nums <- dt_parsedtxt[ , .(l_pos_nums = list(pos.nums)), keyby = "doc_id"][ , l_pos_nums]
  p.nonums <- dt_parsedtxt[ , .(l_parses = list(p.nonums)), keyby = "doc_id"][ , l_parses]
  w.nums <- dt_parsedtxt[ , .(l_w_nums = list(w.nums)), keyby = "doc_id"][ , l_w_nums]

  # Only from questions
  ques.pre.root=as.list(Qwords$qwords)

  #################### Negations! (French)
  setkeyv(dt_parsedtxt, c("doc_id", "sentence_id", "token_id"))

  # French negation words
  if(exists("polite_dicts_fr")){
    negations=c(polite_dicts_fr$Negation,"ne","n'","pas","jamais","rien","personne","aucun","aucune")
  } else {
    negations=c("ne", "n'", "non", "pas", "jamais", "rien", "personne", "aucun", "aucune",
                "aucuns", "aucunes", "nul", "nulle", "nuls", "nulles", "ni", "sans")
  }
  # Expand the negation window
  dt_parsedtxt[,negs:=token%in%negations]
  dt_parsedtxt[,negP1:=shift(negs,1),by=list(doc_id,sentence_id)]
  dt_parsedtxt[,negP2:=shift(negs,2),by=list(doc_id,sentence_id)]
  dt_parsedtxt[,negP3:=shift(negs,3),by=list(doc_id,sentence_id)]
  dt_parsedtxt[,negP4:=shift(negs,4),by=list(doc_id,sentence_id)]
  dt_parsedtxt[,negM1:=shift(negs,-1),by=list(doc_id,sentence_id)]
  dt_parsedtxt[,negM2:=shift(negs,-2),by=list(doc_id,sentence_id)]
  dt_parsedtxt[,anyNeg:=sum(negP1,negP2,negP3,negP4,negM1,negM2,na.rm=T)>0,by=list(doc_id, sentence_id, token_id)]

  blanks<-rbindlist(list(unique(dt_parsedtxt, by= "doc_id"),
                         unique(dt_parsedtxt, by= "doc_id")))
  blanks[,anyNeg:=duplicated(doc_id)]
  blanks[,token:=" "]
  dt_parsedtxt=rbindlist(list(dt_parsedtxt,blanks))

  neg.words=dt_parsedtxt[(anyNeg), .(l_w_nums = list(token)), keyby = "doc_id"][ , l_w_nums]
  unneg.words=dt_parsedtxt[(!anyNeg), .(l_w_nums = list(token)), keyby = "doc_id"][ , l_w_nums]

  dt_parsedtxt=dt_parsedtxt[token!=" "]

  # French first person pronouns
  dt_parsedtxt[,selfies:=token%in%c("je","Je","nous","Nous","moi","Moi","me","Me","notre","Notre","nos","Nos")]
  dt_selfies=dt_parsedtxt[selfies==1,c("doc_id","sentence_id","head_token_id")]
  dt_selfies[,selfscope1:=1]
  dt_parsedtxt <- dt_selfies[dt_parsedtxt, on=c("doc_id","sentence_id","head_token_id"),allow.cartesian=TRUE]
  dt_parsedtxt[is.na(selfscope1),selfscope1:=0]

  dt_selfies=dt_parsedtxt[selfscope1==1,c("doc_id","sentence_id","token_id","selfscope1")]
  setnames(dt_selfies, c("token_id","selfscope1"), c("head_token_id","selfscope2"))
  dt_parsedtxt <- dt_selfies[dt_parsedtxt, on=c("doc_id","sentence_id","head_token_id")]
  dt_parsedtxt[is.na(selfscope2),selfscope2:=0]
  dt_parsedtxt[,selfscope:=sum(selfscope1,selfscope2,na.rm=T)>0,by=list(doc_id, sentence_id, token_id)]

  # adverbial, adpositional and conjunctive clauses shouldn't propogate dependency negations.
  dt_parsedtxt[dep_rel%in%c("advcl","prep"),head_token_id:=0]
  dt_parsedtxt[dep_rel%in%c("conj") & abs(head_token_id-token_id)>3,head_token_id:=0]

  dt_negged<-dt_parsedtxt[dep_rel=="neg",c("doc_id","sentence_id","head_token_id")]
  dt_negged[,parseNeg1:=TRUE]
  dt_parsedtxt <- dt_negged[dt_parsedtxt, on=c("doc_id","sentence_id","head_token_id"),allow.cartesian=TRUE] # left merge on dt_parsedtxt
  setnames(dt_negged, c("head_token_id","parseNeg1"), c("token_id","parseNeg2"))
  dt_parsedtxt <- dt_negged[dt_parsedtxt, on=c("doc_id","sentence_id","token_id"),allow.cartesian=TRUE] # left merge on dt_parsedtxt

  dt_parsedtxt[,parseNeg:=sum(parseNeg1,parseNeg2,na.rm=T)>0,by=list(doc_id, sentence_id, token_id)]

  dt_superhead=dt_parsedtxt[parseNeg==TRUE, c("doc_id","sentence_id","token_id")]
  setnames(dt_superhead, "token_id","head_token_id")
  dt_superhead[,parseNeg3:=TRUE]
  dt_parsedtxt <- dt_superhead[dt_parsedtxt, on=c("doc_id","sentence_id","head_token_id"),allow.cartesian=TRUE] # left merge on dt_parsedtxt

  dt_parsedtxt[,parseNeg:=sum(parseNeg,parseNeg3,na.rm=T)>0,by=list(doc_id, sentence_id, token_id)]

  # Fill in blanks
  blanks<-rbindlist(list(unique(dt_parsedtxt, by= "doc_id"),
                         unique(dt_parsedtxt, by= "doc_id"),
                         unique(dt_parsedtxt, by= "doc_id")))
  blanks[,parseNeg:=duplicated(doc_id)]
  blanks[,p.nonums:=" "]
  blanks[,question:=0]
  blanks[,selfscope:=1]
  dt_parsedtxt=rbindlist(list(dt_parsedtxt,blanks))

  p.negs <- dt_parsedtxt[(parseNeg)&(question==0), .(l_parses = list(p.nonums)), keyby = "doc_id"][ , l_parses]
  p.unnegs <- dt_parsedtxt[(!parseNeg)&(question==0), .(l_parses = list(p.nonums)), keyby = "doc_id"][ , l_parses]

  self.unnegs <- dt_parsedtxt[(!parseNeg)&(question==0)&(selfscope==1), .(l_parses = list(p.nonums)), keyby = "doc_id"][ , l_parses]
  
  # Count questions per document (including heuristic-based)
  # Count unique sentences with questions per document
  question_counts <- dt_parsedtxt[question > 0, .(question_sentences = uniqueN(sentence_id)), by = "doc_id"]
  all_docs <- unique(dt_parsedtxt[, .(doc_id)])
  question_counts <- question_counts[all_docs, on = "doc_id"]
  question_counts[is.na(question_sentences), question_sentences := 0]
  question_counts <- question_counts[order(doc_id), question_sentences]
  
  return(list(parses=all.parses,
              ques.pre.root=ques.pre.root,
              pos.nums=all.pos.nums,
              p.nonums=p.nonums,
              w.nums=w.nums,
              neg.words=neg.words,
              unneg.words=unneg.words,
              p.negs=p.negs,
              p.unnegs=p.unnegs,
              self.unnegs=self.unnegs,
              question_counts=question_counts))
}

