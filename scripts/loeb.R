library("XML")
library("stringr")
library("data.table")

auth.allowed=c("D", "N", "R", "T", "U")
src.allowed=c("ov_am", "ov_ars", "ov_rem", "ov_her", "ov_met", "verg_aen", "stat_theb", "stat_ach")

squash.cit <- Vectorize(function(s) {
  # try to flatten variation in the way texts are cited
  
  s <- str_trim(s)
  s <- tolower(s)
  s <- str_replace(s, perl("^[^a-z]"), "")
  s <- str_replace_all(s, perl("[^a-z]+"), "_")
  s <- str_replace(s, perl("_$"), "")
  
  s <- switch(s,
    ach="stat_ach",
    aen="verg_aen",
    verg="verg_aen",
    virg="verg_aen",
    sta_theb="stat_theb",
    s
  )
  
  return(s)
})


squash.loc <- Vectorize(function(s) {
  # try flatten variation in the way loci are written
  
  s <- chartr(".", ",", s)
  s <- str_replace_all(s, "\\s", "")
  return(s)
})


squash.ortho <- function(s) {
  # flatten variation in latin orthography for the purpose of 
  # compiling word frequencies
  
  s <- tolower(s)
  s <- gsub("[^a-z]+", " ", s, perl=T)
  s <- gsub("\\s+", " ", s, perl=T)
  s <- gsub("^\\s", "", s, perl=T)
  s <- gsub("\\s$", "", s, perl=T)
  s <- chartr("jv", "iu", s)
  return(s)
}


loc.range <- function(loc) {
  # try to standardize the way a range of consecutive loci is expressed
  
  loc <- str_replace(loc, "(sqq|ff).*", "-+2")
  loc <- str_replace(loc, "(sq|f).*", "-+1")
  
  range.test <- unlist(str_split(loc, "-"))
  if (length(range.test) > 2) {
    return(NA)
  }
  if (length(range.test) < 2) {
    return(rep(loc,2))
  }
  
  l <- range.test[1]
  lpref <- ""
  lseg <- unlist(str_split(l, ","))
  if (length(lseg) > 1) {
    lpref <- paste(lseg[-length(lseg)], collapse=",")
    l <- lseg[length(lseg)]
  }
  
  r <- range.test[2]
  rpref <- ""
  if (r == "+2") {
    r <- as.character(as.numeric(l)+2)
    rpref <- lpref
  } else if (r == "+1") {
    r <- as.character(as.numeric(l)+1)
    rpref <- lpref
  } else {
    rseg <- unlist(str_split(r, ","))
    if (length(rseg) > 1) {
      rpref <- paste(rseg[-length(rseg)], collapse=",")
      r <- rseg[length(rseg)]
    }
  }
  
  if (str_length(rpref) < str_length(lpref)) {
    rpref <- paste(substr(lpref, 1, str_length(lpref)-str_length(rpref)), rpref, sep="")
  }
  if (rpref == lpref) {
    if (str_length(r) < str_length(l)) {
      r <- paste(substr(l, 1, str_length(l)-str_length(r)), r, sep="")
    }
  }
  
  l <- str_replace(paste(lpref, l, sep=","), perl("^,"), "")
  r <- str_replace(paste(rpref, r, sep=","), perl("^,"), "")
  
  return(c(l,r))
}


load.tess.text <- function(file) {
  # read a source text in Tesserae's simple XML format
  
  cat("Reading", file, "\n")
  xml <- xmlParse(file=file)
  
  nodes <- getNodeSet(xml, "//TextUnit")
  
  data.table(
    verse = sapply(nodes, xmlValue),
    loc = squash.loc(sapply(nodes, xmlGetAttr, name="loc"))
  )
}


load.ref.corpus <- function(index.file, base.dir=file.path("texts")) {
  # load a corpus of texts, based on an index file

  index <- read.csv(index.file)
  
  corpus <- lapply(file.path(base.dir, paste(index$file, "xml", sep=".")), load.tess.text)
  names(corpus) <- sapply(index$cit, squash.cit)
  
  return(corpus)
}

process.cit <- function(cit, auth) {
  # process a single citation
  
  # citations are set off from what follows by ">";
  if (is.na(str_extract(cit, ">"))) {
    # if this doesn't occur, bail out
    
    return(NULL)
  }
  # otherwise, everything to the left is the citation,
  # and everything to the right is the comment
  seg <- unlist(str_split(cit, ">"))
  cit <- str_trim(seg[1])
  comment <- seg[2]
  
  # placeholder for results
  default <- list(auth=auth, cit=cit, src=NA, src_start=NA, src_end=NA)

  #
  # first, the citation itself
  # 
  
  # check for use of roman numerals in book number
  roman.test <- str_extract(cit, "[IVXLC]+,\\s*")
  if (! is.na(roman.test)) {
    old <- roman.test
    roman.test <- str_replace(roman.test, ",.*", "")
    new <- paste(as.integer(as.roman(roman.test)), ",", sep="")
    cit <- str_replace(cit, old, new)
  }
  
  # divide citation into work and line(s)
  cit.field <- unlist(strsplit(str_replace(cit, "\\b([0-9])", ":::\\1"), ":::"))
  if (length(cit.field) == 2) {
    
    work <- str_trim(cit.field[1])
    work <- squash.cit(work)
    default$src <- work
    
    loc <- squash.loc(cit.field[2])
    loc <- loc.range(loc)
    
    default$src_start <- match(loc[1], corpus[[work]]$loc)
    default$src_end <- match(loc[2], corpus[[work]]$loc)
  }
  
  #
  # then the comment
  #
  
  if (! is.na(comment)) {
    comment_ <- chartr(".,", "  ", comment)
    comment_ <- str_replace_all(comment_, perl("\\bR-*S\\b"), "R")
    comment.tokens <- unlist(str_split(comment_, "\\s+"))
    flag <- comment.tokens %in% auth.allowed
    
    default$auth <- comment.tokens[flag]
    default$src_note <- str_trim(paste(comment.tokens[! flag], collapse=" "))
  }
  
  return(default)
}


process.citgroup <- function(citgroup) {
  # process a citation group
  auth <- NA
  
  rbindlist( 
    lapply(citgroup, function(cit) {
      default <- process.cit(cit, auth)
      auth <<- default$auth
      
      rbindlist(
        lapply(auth, function(a) {
          row <- default
          row$auth <- a
          return(row)
        })
      )
    })
  )
}


process.annotation <- Vectorize(function(note, by=NA, id=NA) {
  # process a single annotation
  
  note <- str_trim(note)
  
  # line should have the format TARGET TOKENS <CIT1> <CIT2> ...
  # - citations are preceeded by the important words in the target
  # - each citation begins with a left angle-bracket
  citgroup <- unlist(str_split(note, "<"))
  
  # citgroup[1] is the target tokens themselves, not a citation
  target.tokens <- unlist(str_split(citgroup[1], "\\s+"))
  
  # get the range of lines from target token 1
  lno <- str_extract(target.tokens[1], perl("\\d[\\d,a-fsq\\-]*"))
  targ <- loc.range(squash.loc(lno))
  
  # paste remaining tokens back into a single string
  target.tokens <- str_trim(paste(target.tokens[-1], collapse=" "))
  
  # check for malformed lines with no citations
  if (length(citgroup) < 2) { return(NULL) }
  
  # now drop the target tokens and consider everything else
  cits <- rbindlist(lapply(citgroup[-1], process.citgroup))
  
  if (nrow(cits) < 1) { return (NULL) }
  
  # add target details which are common to all citations in this group
  cits[,trg_note := target.tokens]
  cits[,trg_start := as.integer(targ[1])]
  cits[,trg_end := as.integer(targ[2])]
  cits[,added_by := by]
  cits[,note_id := id]
  
  return(cits)
})


load.tess.results <- function(file) {
  # load a set of Tesserae allusions
  
  res <- read.table(file, encoding="UTF-8", sep="\t", header=T, stringsAsFactors=F)
  
  src <- regmatches(res$SOURCE_LOC, regexpr("[^0-9]+", res$SOURCE_LOC))
  src <- gsub("[ .]+", "_", src)
  src <- gsub("_$", "", perl=T, src)
  
  src_start <- chartr(".", ",", sub("[^0-9]+", "", res$SOURCE_LOC))
  src_start <- sapply(1:length(src), function(i) {
    match(src_start[i], corpus[[src[i]]]$loc, nomatch=NA)
  })
  
  data.frame(
    trg_start = as.numeric(sub(".*\\.", "", res$TARGET_LOC)),
    src = src,
    src_start = as.numeric(src_start),
    score = res$SCORE
  )
}


eladjust <- Vectorize(function(src, src_start) {
  # adjust elegiacs for numbering by couplet rather than line
  
  # get the locus as vector (book, poem, ..., line)
  loc <- as.numeric(unlist(strsplit(corpus[[src]][src_start, loc], ",")))
  
  # assume line number is rightmost element:
  # if it's even, this is a pentameter line
  if (loc[length(loc)] %% 2 == 0) {
    # check to see whether preceeding line is in our corpus
    locadjust <- c(rep(0, length(loc)-1), -1)
    test.loc <- paste(loc + locadjust, collapse=",")
    src_start <- match(test.loc, corpus[[src]]$loc, nomatch=src_start)
  }
  
  return(src_start)
})


wc <- function(doc, freq=T, remove.hapax=F, remove.top=0) {
  # calculate word counts (or word frequencies)
  
  doc <- tolower(paste(doc, collapse=" "))
  bow <- unlist(strsplit(doc, "\\W+"))
  bow <- table(bow)
  
  if(remove.hapax) {
    bow <- bow[bow > 1]
  }
  if(freq) {
    bow <- 1000 * bow/sum(bow)
  }
  
  bow <- sort(bow, decreasing=T)
  if(remove.top>0) {
    bow <- bow[-(1:remove.top)]
  }
  
  return(bow)
}


feat.vec <- function(s, features){
  # extract frequencies for a set of feature words
  
  row <- wc(s)[features]
  row[is.na(row)] <- 0
  names(row) <- features
  as.list(row)
}

