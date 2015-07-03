# Week 3
#
#  - PCA on samples of all source texts

# libraries

# load base script
source(file.path("R", "loeb.R"))

# functions
distinctive <- function(t, c) {
  feat <- intersect(names(t), names(c))
  sort(setNames(sapply(feat, function(f) {t[f]/c[f]}), feat))
}


#
# main
#

# 1. work with the texts

# load the corpus
index.file <- file.path("data", "index.txt")
index <- fread(index.file)
index$src <- squash.cit(index$cit)
setkey(index, src)
corpus <- load.ref.corpus(index.file)

# tally frequencies for every word in the corpus,
# create a featureset
wf.corpus <- wc(unlist(lapply(corpus, function(work) {work$verse})), remove.hapax=T)
features <- names(wf.corpus)

# turn each work in the corpus into a feature vector
vec.corpus <- rbindlist(
  lapply(index$src, function(src) {
    feat.vec(corpus[[src]]$verse, features=features)
  })
)
row.names(vec.corpus) <- index$src

# most distinctive words for each class
#  - after A. Kenney
is.eleg <- index$class == "eleg"
vec.diff.class <- colMeans(vec.corpus[is.eleg]) - colMeans(vec.corpus[! is.eleg])

# principal components analysis to concentrate variance in a few dimensions
pca.model <- prcomp(vec.corpus)
pca.corpus <- pca.model$x

# 2. work with Tesserae

# load all tesserae results
all.tess <- do.call(rbind,
    lapply(c("ov_eleg", "ov_met", "verg_aen", "stat_theb"), function(name) {
    file <- file.path("data", "tess", paste(name, "all", "txt", sep="."))
    load.tess.results(file)
  })
)

# extract just intertextual lines as new document set
itext <- do.call(rbind,
  lapply(index$src, function(src) {
    data.frame(
      src = src,
      text = paste(corpus[[src]][all.tess[all.tess$src==src, "src_start"], verse], collapse=" "),
      stringsAsFactors = F
    )  
  })
)

# calculate feature vectors for intertextual lines only
vec.itext <- rbindlist(
  lapply(itext$text, feat.vec, features=features)
)
row.names(vec.itext) <- itext$src

# project new, itext feature vectors into the PCA space defined by
# the original documents
pca.itext <- predict(pca.model, newdata = vec.itext)


#
# graphs
#

# destination for output
dir.plot <- file.path("plot", "week-3")

# graph first two principal components
pdf(file=file.path(dir.plot, "pca.pdf"), width=8, height=6, pointsize=11)
plot(pca$x, xlim=c(-20,20), ylim=c(-15, 15), type="n")
text(pca$x, index$src)
dev.off()

# show the relationship of the intertextual lines to the original documents
pdf(file=file.path("plot", "week03", "pca_predict.pdf"), width=8, height=6, pointsize=11)
plot(pca.corpus, xlim=c(-20,20), ylim=c(-15, 15), type="n")
text(pca.corpus, index$src)
text(pca.itext, itext$src, col=2)
dev.off()

