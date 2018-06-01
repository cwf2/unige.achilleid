# Week 5
#
#  - Ripoll's divisions into scenes
#  - survey of class's impressions of each scene
#  - genre signal derived from PC1, by scene

# libraries

# load base script
source(file.path("scripts", "loeb.R"))

#
# functions
#

make.samples <- function(verses, labels=NULL, n=20) {
  # cut a text from the corpus into n-line samples
  
  sampleid <- floor(seq(along.with = verses)/n)
  samples <- split(verses, sampleid)
  samples <- squash.ortho(unlist(lapply(samples, paste, collapse=" ")))
  
  if (! is.null(labels)) {
    labels <- sapply(split(labels, sampleid), function(lab) {
      paste(lab[1], lab[length(lab)], sep=":")
    })
  }
  
  names(samples) <- labels
  return(samples)
}

make.theme.index <- function(file) {
  # read in scene divisions and classifications from a file
  
  themes <- read.table(file, header=T, stringsAsFactors=F)
  themes$note <- factor(themes$note, levels=themes$note)
  lno <- sapply(strsplit(corpus[["stat_ach"]]$loc, ","), as.integer)
  lno <- lno[2, lno[1,]==1]
  index <- themes[findInterval(lno, themes$start), "note"]
  return(index)
}


#
# main
#

# load the corpus
index.file <- file.path("data", "index-large-corpus.txt")
index <- fread(index.file)
index$src <- squash.cit(index$cit)
setkey(index, src)
corpus <- load.ref.corpus(index.file)

# create samples of 50 lines, tagged by source, class
samples <- lapply(corpus, function(x){
  make.samples(x$verse, labels=x$loc, n=50)
})
samples <- data.table(
  src = rep(names(samples), times=sapply(samples, length)),
  id = names(unlist(samples)),
  text = unlist(samples)
)
samples$class <- as.factor(index[samples$src, class])
samples$src <- as.factor(samples$src)
setkey(samples, src, class)

# calculate all word frequencies for the corpus
wf.corpus <- wc(samples$text, freq=F, remove.hapax=T)

# build a feature set:
#  - drop the top 50 words, take the next 2000
features <- names(wf.corpus)[51:2050]

# calculate a feature vector for each sample using these 2000 word frequencies
vec.samples <- rbindlist(lapply(samples$text, feat.vec, features=features))
setnames(vec.samples, features)

# PCA
pca.model <- prcomp(vec.samples)
pca.samples <- data.table(pca.model$x[,1:2], samples[,.(src, class)])

# compare no-met pca with kmeans unsupervised classification
kmeans.class <- kmeans(pca.samples[! "ov_met", .(PC1, PC2)], centers=2)$cluster
wrong <- kmeans.class != unclass(pca.samples[! "ov_met", class])
if (length(which(wrong)) > pca.samples[! "ov_met", .N]/2) {
  wrong = ! wrong
  kmeans.class <- 3 - kmeans.class
}

# now look at intertexts collected by students
notes <- read.table(file.path("data", "all-loeb.txt"), encoding="utf8", header=T, stringsAsFactors=F)
cits <- rbindlist(process.annotation(note=notes$Annotation, by=notes$Contributor, id=1:nrow(notes)))
cits <- cits[! is.na(cits$trg_start),]
cits <- cits[! is.na(cits$src_start),]
cits <- unique(cits)

# take only auth annotations
auth.allowed.comm <- c("D", "N", "R", "U")
auth.names <- c("Dilke", "Nuzzo", "Ripoll-Soubiran", "Uccellini")
cits <- cits[cits$auth %in% auth.allowed.comm,]

# organize by genre of source
ach.itexts <- cits[,.(src=unique(src)), by=.(trg_start, trg_end)]
ach.itexts$class <- index[ach.itexts$src, class]
setkey(ach.itexts, class)

# test intertextual achilleid lines by source class
ach_eleg <- unique(corpus[["stat_ach"]][ach.itexts["eleg", trg_start], verse])
ach_epic <- unique(corpus[["stat_ach"]][ach.itexts["epic", trg_start], verse])

ach_eleg.samples <- make.samples(verses = ach_eleg, n = 20)
ach_eleg.samples <- ach_eleg.samples[-length(ach_eleg.samples)]
ach_epic.samples <- make.samples(verses = ach_epic, n = 20)
ach_epic.samples <- ach_epic.samples[-length(ach_eleg.samples)]

vec.ach_eleg <- rbindlist(lapply(ach_eleg.samples, feat.vec, features=features))
vec.ach_epic <- rbindlist(lapply(ach_epic.samples, feat.vec, features=features))
pca.ach <- rbind(
  data.table(
    predict(pca.model, newdata = vec.ach_eleg)[,c("PC1", "PC2")],
    src="stat_ach",
    class="eleg"
  ),
  data.table(
    predict(pca.model, newdata = vec.ach_epic)[,c("PC1", "PC2")],
    src="stat_ach",
    class="epic"
  )
)
setkey(pca.ach, class)

# divisions of Ripoll
index.ripoll <- make.theme.index(file.path("data", "themes-ripoll.txt"))
ach.itexts[,theme := index.ripoll[trg_start]]

# new samples by ripoll section
ach.theme.samples <- lapply(
  split(corpus[["stat_ach"]][1:length(index.ripoll), verse], index.ripoll),
  paste, collapse=" "
)
ach.theme.samples <- squash.ortho(ach.theme.samples)

# feature vectors for ripoll sections
vec.ach.theme <- rbindlist(lapply(ach.theme.samples, feat.vec, features=features))

# pca for ripoll sections
pca.ach.theme <- data.table(
  predict(pca.model, newdata = vec.ach.theme)[,c("PC1", "PC2")],
  theme = levels(index.ripoll)
)

# tone according to class poll
tone <- data.table(read.table(file.path("data", "tone-class.txt"), header=T, stringsAsFactors=F))
nstudents <- 9
tone[,c("epic", "eleg") := list(epic/nstudents, eleg/nstudents)]

# tone according to pc1
ach.center <- mean(pca.samples["stat_ach", PC1])
tone$pc1 <- pca.ach.theme[,PC1 - ach.center]

###########################
#         graphs          #
###########################

# location for output
dir.plot <- file.path("plot", "week-5")

# achilleid alone
pdf(file.path(dir.plot, "achilleid.pc12.pdf"), width=8, height=6)
pca.samples["stat_ach", plot(PC1, PC2)]
dev.off()

# achilleid: ripoll's divisions
pdf(file.path(dir.plot, "achilleid.ripoll.pc12.pdf"), width=8, height=6)
plot(pca.ach.theme[,.(PC1, PC2)])
identify(pca.ach.theme[,.(PC1, PC2)], labels = pca.ach.theme$theme)
dev.off()

# by genre, with and without ov.met
pdf(file.path(dir.plot, "genre-no-met.pdf"), width=8, height=6)
pca.samples[,plot(PC1, PC2, type="n")]
pca.samples[! "ov_met", points(PC1, PC2, col=unclass(class))]
dev.off()

pdf(file.path(dir.plot, "genre-with-met.pdf"), width=8, height=6)
pca.samples[,plot(PC1, PC2, type="n")]
pca.samples[! "ov_met", points(PC1, PC2, col=unclass(class))]
pca.samples["ov_met", points(PC1, PC2, col=3)]
dev.off()

# unsupervised classification by genre
pdf(file.path(dir.plot, "kmeans-no-met.pdf"), width=8, height=6)
pca.samples[,plot(PC1, PC2, type="n")]
pca.samples[! "ov_met", points(PC1, PC2, col=kmeans.class)]
pca.samples[! "ov_met",][wrong, points(PC1, PC2, col=class, pch=5, cex=2, lwd=2)]
dev.off()

# achilleid with intertextual lines
pdf(file.path(dir.plot, "ach.it.genre.pdf"), width=8, height=6)
pca.samples[,plot(PC1, PC2, type="n")]
pca.samples[! "ov_met", points(PC1, PC2, col=gray(.5 + unclass(class)/5), pch=2)]
pca.samples["stat_ach", points(PC1, PC2, col="gold4", pch=2)]
pca.ach["epic", points(PC1, PC2, col="green", lwd=2, cex=1.5)]
pca.ach["eleg", points(PC1, PC2, col="red", lwd=2, cex=1.5)]
dev.off()

# intertexts by ripoll's scenes
rip <- data.table(
  table(ach.itexts[,.(theme, class)])[,c("epic", "eleg")],
  nlines = table(index.ripoll)
)
pdf(file.path(dir.plot, "itext-normalized-ripoll.pdf"), width=12, height=6)
backup.par <- par(mar=c(10,3,2,2), cex.axis=.8)
barplot(t(rip[1:18,.(epic/nlines, eleg/nlines)]),
  col=c("grey85", "grey15"),
  beside=T, 
  names.arg=levels(index.ripoll)[1:18],
  las=2,
  legend.text=c("épopée", "élégie"),
  args.legend=list(title="intertextes/vers")
)
par(backup.par)
dev.off()

# tone according to class
pdf(file.path(dir.plot, "tone-by-scene-class.pdf"), width=8, height=6)
layout(matrix(c(1,1,2,2), 2, 2, byrow=T), heights=c(1,2.5))
backup.par <- par(mar=c(.2,3,1.2,2), cex.axis=.8)
plot(tone[1:18,epic-eleg],
  type="n",
  axes=F,
  ann=F
)
abline(h=0, lty=2, col="grey")
mtext("élégiaque", side=2, at=-1, adj=0, cex=.6)
mtext("épique", side=2, at=1, adj=1, cex=.6)
points(tone[1:18,epic-eleg], type="b")
par(backup.par)

backup.par <- par(mar=c(10,3,2,2), cex.axis=.8)
barplot(t(rip[1:18,.(epic/nlines, eleg/nlines)]),
        col=c("grey85", "grey15"),
        beside=T, 
        names.arg=levels(index.ripoll)[1:18],
        las=2,
        legend.text=c("épopée", "élégie"),
        args.legend=list(title="intertextes/vers")
)
par(backup.par)
dev.off()

# tone according to pc1
# full poem, no intertexts
pdf(file.path(dir.plot, "tone-by-scene-pc1-full.pdf"), width=12, height=6)
backup.par <- par(mar=c(12,3,2,2), cex.axis=.8)
plot(tone[,pc1],
     type="n",
     axes=F,
     ann=F
)
abline(h=0, lty=2, col="grey")
mtext("élégiaque", side=2, at=min(tone[,pc1]), adj=0, cex=1)
mtext("épique", side=2, at=max(tone[,pc1]), adj=1, cex=1)
mtext(levels(index.ripoll), side=1, at=1:nlevels(index.ripoll), las=2)
points(tone[,pc1], type="b")
par(backup.par)
dev.off()

# with intertexts to section 18
pdf(file.path(dir.plot, "tone-by-scene-pc1.pdf"), width=8, height=6)
layout(matrix(c(1,1,2,2), 2, 2, byrow=T), heights=c(1,2.5))
backup.par <- par(mar=c(.2,3,1.2,2), cex.axis=.8)
plot(tone[1:18,pc1],
     type="n",
     axes=F,
     ann=F
)
abline(h=0, lty=2, col="grey")
mtext("élégiaque", side=2, at=min(tone[1:18,pc1]), adj=0, cex=.6)
mtext("épique", side=2, at=max(tone[1:18,pc1]), adj=1, cex=.6)
points(tone[1:18,pc1], type="b")
par(backup.par)

backup.par <- par(mar=c(10,3,2,2), cex.axis=.8)
barplot(t(rip[1:18,.(epic/nlines, eleg/nlines)]),
        col=c("grey85", "grey15"),
        beside=T, 
        names.arg=levels(index.ripoll)[1:18],
        las=2,
        legend.text=c("épopée", "élégie"),
        args.legend=list(title="intertextes/vers")
)
par(backup.par)
dev.off()


