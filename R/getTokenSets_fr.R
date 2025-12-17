# Used to avoid incorrect notes of "no visible binding"
utils::globalVariables(c("sets","text"))

#' Extracting Tokens from Natural Language (French)
#' @description Return tokens (words or POS tags) from French natural language.
#' @param text a character vector of texts.
#' @param parser character Name of dependency parser to use.
#' @param num_mc_cores integer Number of cores for parallelization. Default is 1.
#' @return list of compiled POS-tagged items.
#' @keywords internal
#'
getTokenSets_fr<-function(text,parser=c("none","spacy"),num_mc_cores=1){
  text=strip_bracket_tags(text)
  text=cleanpunct(text)
  text=ctxpand_fr(text)
  sets<-list()
  
  # French dictionaries are loaded from package internal data (sysdata.rda)
  # See dictTools_fr.R for dictionary definitions
  polite_dicts_fr_quanteda <- quanteda::dictionary(polite_dicts_fr)
  sets[["dicts"]]<-dictWrap(text, dict=polite_dicts_fr_quanteda, num_mc_cores=num_mc_cores)

  sets[["clean"]]<-parallel::mclapply(text, cleantext_fr, stop.words=FALSE,mc.cores=num_mc_cores)
  sets[["c.words"]]<-parallel::mclapply(sets[["clean"]], function(x) strsplit(x, split=" ")[[1]],mc.cores=num_mc_cores)
  sets[["c.words"]]<-parallel::mclapply(sets[["c.words"]],function(x) x[x!=""],mc.cores=num_mc_cores)

  if(parser[1]=="spacy"){
    s.p<-spacyParser_fr(text)
    sets[["parses"]]<-parallel::mclapply(s.p$parses,tolower,mc.cores=num_mc_cores)
    sets[["p.nonum"]]<-parallel::mclapply(s.p$p.nonums,tolower,mc.cores=num_mc_cores)
    sets[["pos.nums"]]<-parallel::mclapply(s.p$pos.nums,tolower,mc.cores=num_mc_cores)
    sets[["w.nums"]]<-parallel::mclapply(s.p$w.nums,tolower,mc.cores=num_mc_cores)
    sets[["ques.pre.root"]]<-parallel::mclapply(s.p$ques.pre.root,tolower,mc.cores=num_mc_cores)
    sets[["unneg.words"]]<-parallel::mclapply(s.p$unneg.words,tolower,mc.cores=num_mc_cores)
    sets[["neg.words"]]<-parallel::mclapply(s.p$neg.words,tolower,mc.cores=num_mc_cores)
    sets[["p.negs"]]<-parallel::mclapply(s.p$p.negs,tolower,mc.cores=num_mc_cores)
    sets[["p.unnegs"]]<-parallel::mclapply(s.p$p.unnegs,tolower,mc.cores=num_mc_cores)
    sets[["self.unnegs"]]<-parallel::mclapply(s.p$self.unnegs,tolower,mc.cores=num_mc_cores)
    sets[["question_counts"]]<-s.p$question_counts
  }
  return(sets)
}

