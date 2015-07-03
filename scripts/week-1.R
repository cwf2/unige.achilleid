# Week 1
#
#  - differences between commentaries
#  - location of intertexts in source

# load base script

source(file.path("R", "loeb.R"))

# functions

book.getter <- Vectorize(function(src, id) {
  as.numeric(sub(",.*", "", corpus[[src]]$loc[id]))
}, USE.NAMES=F)

# load the texts
index.file <- file.path("data", "index.txt")
corpus <- load.ref.corpus(index.file)

# load the students' notes
notes <- read.table(file.path("data", "all-loeb.txt"), encoding="utf8", header=T, stringsAsFactors=F)
cits <- rbindlist(process.annotation(note=notes$Annotation, by=notes$Contributor, id=1:nrow(notes)))
cits <- cits[! is.na(cits$trg_start),]
cits <- cits[! is.na(cits$src_start),]

# take only auth annotations
auth.allowed.comm <- c("D", "R", "N", "U")
auth.names <- c("Dilke", "Ripoll-Soubiran", "Nuzzo", "Uccellini")
cits <- cits[auth %in% auth.allowed.comm,][, auth:=factor(auth, levels=auth.allowed.comm)]

# put commentaries in chronological order
for (l in rev(auth.allowed.comm)) {
    x <- relevel(cits$auth, l)
}
rm(l)

# key citations by source
setkey(cits, src)

# omit intratexts
cits <- cits[! "stat_ach"]

# combine ovid's elegies
cits[,src2:=src]
cits[c("ov_ars", "ov_rem", "ov_am", "ov_her"), src2 := "ov_eleg"]

# remove duplicates
setkey(cits, trg_start, src, src_start, auth)
cits <- unique(cits)

# only consider first 103 verses
cits <- cits[trg_start <= 104]

# tallies by source text, commentator
itext.comm <- cits[, table(src2, auth)]

# now credit for intertext goes only to earliest commentary
itext.uniq <- cits[! duplicated(cits[,paste(trg_start, src, src_start)]), table(src2, auth)]

#
# graphs
#

# commentary preferences

pdf(file.path("plot", "by_auth_beside.pdf"), height=4, width=8)
op <- par(mar=c(4,4,4,8)+0.1, xpd=T)
barplot(itext.comm,
  beside=T, 
  main=paste("commentaires", "vers 1 à 103", sep="\n"),
  ylab="intertextes",
  names.arg=auth.names,
  legend.text=T,
  args.legend = list(x="right", inset=-.25)
)
par(op)
dev.off()

# new for each commentary
pdf(file.path("plot", "by_auth_unique.pdf"), height=4, width=8)
op <- par(mar=c(4,4,4,8)+0.1, xpd=T)
barplot(itext.uniq, beside=T,
  main=paste("commentaires: intertextes uniques", "vers 1 à 103", sep="\n"),
  ylab="intertextes",
  names.arg=auth.names,
  legend.text=T, 
  args.legend = list(x="right", inset=-.25)
)
par(op)
dev.off()

# location in Vergil

book.verg <- cits[src=="verg_aen", 
  .(key = paste(trg_start, src_start), book = book.getter(src, src_start))
  ][! duplicated(key), book]

pdf(file.path("plot", "by_loc_verg.pdf"), height=4, width=8)
barplot(table(book.verg),
  main=paste("Intertextes avec l'Énéide, des commentaires", "vers 1 à 103", sep="\n"),
  ylab="intertextes",
  xlab="livre de l'Énéide"
)
dev.off()

# location in Thebaid
book.theb <- cits[src=="stat_theb", 
  .(key = paste(trg_start, src_start), book = book.getter(src, src_start))
  ][! duplicated(key), book]

pdf(file.path("plot", "by_loc_theb.pdf"), height=4, width=8)
barplot(table(book.theb),
  main=paste("Intertextes avec la Thébaïde, des commentaires", "vers 1 à 103", sep="\n"),
  ylab="intertextes",
  xlab="livre de la Thébaïde"
)
dev.off()

