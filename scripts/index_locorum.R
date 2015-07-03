# libraries

# load base script
source(file.path("R", "loeb.R"))

# some functions for formatting annotations

loc.range <- Vectorize(function(loc.start, loc.stop) {
  if (is.na(loc.stop)) { loc.stop <- loc.start }
  if (loc.start != loc.stop) {
    seg.start = unlist(strsplit(loc.start, ","))
    seg.stop = unlist(strsplit(loc.stop, ","))
    
    if (length(seg.start) == length(seg.stop)) {      
       loc.stop <- paste(seg.stop[match(TRUE, seg.start!=seg.stop):length(seg.stop)], collapse=",")
    }
    
    return(paste(loc.start, loc.stop, sep="-"))
  } else {
    return(loc.start)
  }
})

# load corpus

index.file <- file.path("data", "index.txt")
index <- fread(index.file)
index$src <- cit.squash(index$cit)
setkey(index, src)
corpus <- load.ref.corpus(index.file)

# load annotations

notes <- ingest.dir(file.path("data", "2015-05-20"))
notes <- unique(notes)
cits <- rbindlist(process.annotation(note=notes$Annotation, by=notes$Contributor, id=1:nrow(notes)))
cits <- cits[! is.na(cits$trg_start),]
cits <- cits[! is.na(cits$src_start),]
cits <- unique(cits)

# use tesserae names for source texts
source.name <- Vectorize(function(src) {
  switch(src,
    verg_aen = "Virg. Aen.",
    ov_met = "Ov. met.",
    stat_theb = "Stat. Theb.",
    stat_ach = "Stat. Ach.",
    ov_ars = "Ov. ars",
    ov_her = "Ov. her.",
    ov_am = "Ov. am",
    ov_rem = "Ov. rem."
  )
})
get.loc <- Vectorize(function(src, unitid) {
  as.character(corpus[[src]][unitid, loc])
})

# export grouped by place in the source, target
setkey(cits, trg_start, trg_end, src, src_start, src_end)
cits.export <- cits[,.(
  TARGET_START = get.loc("stat_ach", trg_start),
  TARGET_STOP = get.loc("stat_ach", trg_end),
  TARGET_TEXT = trg_note,
  SOURCE = source.name(src),
  SOURCE_START = get.loc(src, src_start),
  SOURCE_STOP = get.loc(src, src_end),
  AUTH = paste(grep("T", auth, invert=T, value=T), collapse=';'),
  ADDED_BY = added_by,
  NOTE = src_note
  ),
  by = .(trg_start, trg_end, src, src_start, src_end)
]

cits.export[,TARGET_TEXT := sub("^\\s+", "", TARGET_TEXT, perl=T)]
cits.export[,TARGET_TEXT := sub("\\s+$", "", TARGET_TEXT, perl=T)]
cits.export[,TARGET_TEXT := chartr("jv", "iu", TARGET_TEXT)]

cits.export <- cits.export[order(trg_start, trg_end, TARGET_TEXT, SOURCE, src_start, src_end)]

cits.export <- cits.export[,.(
  TARGET = loc.range(TARGET_START, TARGET_STOP),
  TARGET_TEXT,
  SOURCE,
  SOURCE_LOC = loc.range(SOURCE_START, SOURCE_STOP),
  AUTH,
  NOTE
)]


write.table(cits.export, "~/Desktop/cits.export.tsv", sep="\t", row.names=F, fileEncoding = "utf8")
