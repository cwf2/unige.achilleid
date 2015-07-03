# Week 2
#
#  - Tesserae versus the commentaries

# load base script

source(file.path("R", "loeb.R"))

# libraries

library(VennDiagram)

#
# main
#

# 1. Start with the hand-entered data

# load the texts
index.file <- file.path("data", "index.txt")
corpus <- load.ref.corpus(index.file)

# load the students' notes
notes <- read.table(file.path("data", "all-loeb.txt"), encoding="utf8", header=T, stringsAsFactors=F)
cits <- rbindlist(process.annotation(note=notes$Annotation, by=notes$Contributor, id=1:nrow(notes)))
cits <- cits[! is.na(cits$trg_start),]
cits <- cits[! is.na(cits$src_start),]
cits <- unique(cits)

# select Tesserae results
t.cits <- cits[cits$auth == "T",]

# take only auth annotations
auth.allowed.comm <- c("D", "N", "R", "U")
auth.names <- c("Dilke", "Nuzzo", "Ripoll-Soubiran", "Uccellini")
cits <- cits[cits$auth %in% auth.allowed.comm,]
cits$auth <- as.factor(cits$auth)

# put commentaries in chronological order
lapply(rev(auth.allowed.comm), function(l) {
  relevel(cits$auth, l)
})

# omit intratexts
cits <- cits[cits$src != "stat_ach",]

# adjust line numbers for elegiacs
cits[grep("ov_[^m]", cits$src), src_start := eladjust(src, src_start)]
cits[grep("ov_[^m]", cits$src), src_end := eladjust(src, src_start)]

# put intertexts in order by target loc
cits <- cits[order(cits$trg_start),]
t.cits <- t.cits[order(t.cits$trg_start),]

# patch a couple of data entry errors
# t.cits[27, "src_start"] <- t.cits[27, "src_end"]
# t.cits[42, "src_start"] <- t.cits[42, "src_end"]
# t.cits[119, "src_start"] <- t.cits[119, "src_end"]


# 2. Cross-ref all tesserae results

# load all tesserae results
all.tess <- rbindlist(
  lapply(c("ov_eleg", "ov_met", "verg_aen", "stat_theb"), function(name) {
    file <- file.path("data", "tess", paste(name, "all", "txt", sep="."))
    load.tess.results(file)
  })
)

# don't double-count intertexts in recall tests
cit.key <- cits[, paste(trg_start, src, src_start)]
cits <- cits[! duplicated(cit.key),]

all.tess.key <- all.tess[, paste(trg_start, src, src_start)]
all.tess <- all.tess[! duplicated(all.tess.key),]

t.cit.key <- t.cits[,paste(trg_start, src, src_start)]
t.cits <- t.cits[t.cit.key %in% all.tess.key & ! duplicated(t.cit.key),]

# limit test to first 197 verses
cits <- cits[trg_start <= 197]
cit.key <- cits[, paste(trg_start, src, src_start)]

t.cits <- t.cits[trg_start <= 197]
t.cit.key <- t.cits[,paste(trg_start, src, src_start)]

all.tess <- all.tess[trg_start <= 197]
all.tess.key <- all.tess[, paste(trg_start, src, src_start)]

# check recall, precision at different score levels
#  - "true": Tesserae results also in commentaries
#  - "false": Tesserae results not in commentaries
tess.prec <- do.call(rbind,
  lapply(seq(from=9, to=10, by=0.1), function(cutoff) {
    kept.tess.key <- all.tess[score >= cutoff, paste(trg_start, src, src_start)]
    data.frame(
      cutoff = cutoff,
      true = length(intersect(kept.tess.key, cit.key)),
      false = length(kept.tess.key) - length(intersect(kept.tess.key, cit.key))
    )
  })
)  


#
# graphiques
#

# output directory
dir.plot <- file.path("plot", "week-2")

# Tesserae true and false positive results for cutoff 9 & 10
pdf(file=file.path(dir.plot, "tess_precision.pdf"), width=8, height=4)
op <- par(mfrow=c(1,2), xpd=NA)
with(tess.prec[tess.prec$cutoff==9,], pie(c(true, false), 
  label=paste(c("validés", "non-validés"), c(true, false), sep="\n"),
  col=c(rgb(.75,.9,.75), rgb(1,.9,.9)),
  border="black"
))
mtext("cutoff 9", 1)
with(tess.prec[tess.prec$cutoff==10,], pie(c(true, false), 
  label=paste(c("validés", "non-validés"), c(true, false), sep="\n"),
  col=c(rgb(.75,.9,.75), rgb(1,.9,.9)),  
  border="black"
))
mtext("cutoff 10", 1)
par(op)
dev.off()


pdf(file=file.path(dir.plot, "tess_p-r.pdf"), width=8, height=4)
with(tess.prec, 
  plot(true/nrow(cits) ~ cutoff, 
    main="Ach. 1,1-1,197", 
    ylab="", 
    col=1, 
    type="l", 
    lwd=2
  )
)
with(tess.prec, 
  lines(true/(true+false) ~ cutoff, 
    ylab="", 
    col=2, 
    lwd=2
  )
)
legend("topright", legend=c("rappel", "précision"), lty=1, lwd=2, col=c(1,2), cex=.8)
#text(x=c(5.5, 5.5), y=c(0.3, 0.1), 
#  labels=c("rappel: hits / tous les résultats des commentaires", 
#    "precision: hits / tous les résultats de tesserae"), col=c(1,2))
dev.off()

# venn diagrams

# for venn diagrams graphs fix results at score 9
all.tess.key <- all.tess[score >= 9, paste(trg_start, src, src_start)]
t.cits <- t.cits[t.cit.key %in% all.tess.key,]
t.cit.key <- t.cits[, paste(trg_start, src, src_start)]

venn.valid <- draw.pairwise.venn(
  area1 = length(t.cit.key),
  area2 = length(cit.key),
  cross.area = length(intersect(t.cit.key, cit.key)),
  category = c(paste("Tesserae", "résultats validés", sep="\n"), "commentaires"),
  cat.pos = c(0, -30),
  cat.dist = c(.2, .1), 
  fill = c(rgb(.7,.9,.7), rgb(.9,.7,.7)),  
  fontfamily = "sans", 
  cat.fontfamily = "sans",
  inverted = T,
  ind = F,
  margin = c(0.5,0.5,0.5,0.5)
)
pdf(file=file.path(dir.plot, "venn_valid.pdf"), width=6, height=6)
grid.draw(venn.valid)
dev.off()

venn.raw <- draw.pairwise.venn(
  area1 = length(all.tess.key),
  area2 = length(cit.key),
  cross.area = length(intersect(all.tess.key, cit.key)),
  category=c(paste("Tesserae", "résultats bruts", sep="\n"), "commentaires"),
  cat.pos = 0,
  cat.dist = 0.05, 
  fill = c(rgb(.7,.9,.7), rgb(.9,.7,.7)),
  fontfamily = "sans", 
  cat.fontfamily = "sans",
  ext.text = F,
  ind=F
)

pdf(file=file.path(dir.plot, "venn_raw.pdf"), width=6, height=6)
grid.draw(venn.raw)
dev.off()


# location of intertexts

pdf(file=file.path(dir.plot, "hist_all_tess.pdf"), width=8, height=4)
hist(all.tess$trg_start,
  breaks=20,
  main="Tesserae: tous résultats",
  ylab="Nombre d'intertextes",
  xlab="Position dans l'Achiléide"
)
dev.off()

pdf(file=file.path(dir.plot, "hist_tess_valid.pdf"), width=8, height=4)
hist(t.cits$trg_start,
  breaks=20,
  main="Tesserae: résultats validés",
  ylab="Nombre d'intertextes",
  xlab="Position dans l'Achiléide"
)
dev.off()

pdf(file=file.path(dir.plot, "hist_comm.pdf"), width=8, height=4)
hist(cits[! duplicated(cit.key), trg_start],
  breaks=20,
  main="Commentaires",
  ylab="Nombre d'intertextes",
  xlab="Position dans l'Achiléide"
)
dev.off()
