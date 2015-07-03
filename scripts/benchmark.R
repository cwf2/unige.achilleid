# libraries

# load base script
source(file.path("R", "loeb.R"))

# load corpus

index.file <- file.path("data", "index.txt")
index <- fread(index.file)
index$src <- cit.squash(index$cit)
setkey(index, src)
corpus <- load.ref.corpus(index.file)

# load annotations

notes <- ingest.dir(file.path("data", "2015-05-16"))
notes <- unique(notes)
cits <- rbindlist(process.annotation(note=notes$Annotation, by=notes$Contributor, id=1:nrow(notes)))
cits <- cits[! is.na(cits$trg_start),]
cits <- cits[! is.na(cits$src_start),]
cits <- unique(cits)

# use tesserae names for source texts
source.name <- Vectorize(function(src) {
  switch(src,
    verg_aen = "vergil.aeneid",
    ov_met = "ovid.metamorphoses",
    stat_theb = "statius.thebiad",
    stat_ach = "statius.achilleid",
    ov_ars = "ovid.ars_amatoria",
    ov_her = "ovid.heroides",
    ov_am = "ovid.amores",
    ov_rem = "ovid.remedia_amoris"
  )
})
get.loc <- Vectorize(function(src, unitid) {
  as.character(corpus[[src]][unitid, loc])
})

# export grouped by place in the source, target
setkey(cits, trg_start, trg_end, src_start, src_end)
cits.export <- cits[,.(
  TARGET = "statius.achilleid",
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
  by = .(trg_start, trg_end, src_start, src_end)
][,.(
  TARGET, 
  TARGET_START = chartr(",", "_", TARGET_START),
  TARGET_STOP = chartr(",", "_", TARGET_STOP),
  TARGET_TEXT,
  SOURCE,
  SOURCE_START = chartr(",", "_", SOURCE_START),
  SOURCE_STOP = chartr(",", "_", SOURCE_STOP),
  SOURCE_TEXT = "",
  TYPE = "",
  AUTH,
  ADDED_BY,
  NOTE
  )
]

write.table(cits.export, file="~/Desktop/achilleid.csv", quote=T, sep="\t", row.names=F)
