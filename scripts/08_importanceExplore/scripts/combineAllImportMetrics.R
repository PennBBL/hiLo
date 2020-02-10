## This script will be used to compare all of the importance metrics for the hi lo project

## Load library(s)
install_load('ggplot2', 'GGally')
source('../functions/functions.R')
## The first thing we have to do is produce all of the importance metrics
## Little bash fandangling
#system("for i in `ls prep*R` ; do Rscript ${i} & done")

## Now we will read all of the male values and produce the correlation matrix
randImp <- read.csv('./randForImpMale.csv')
colnames(randImp) <- c("ROI_readable", "randomForestImp", "modality", "ModelingTechniqueRF")
randImp <- randImp[-which(randImp$modality=='all.data'),]
randImp <- randImp[,-c(3,4)]
relifImp <- read.csv('./reliefFImpMale.csv')
colnames(relifImp) <- c("ROI_readable", "reliefImp", "modality", "ModelingTechniqueRF")
relifImp <- relifImp[-which(relifImp$modality=='all.data'),]
relifImp <- relifImp[,-c(3,4)]
ridgeImp <- read.csv('./ridgeImpMale.csv')
colnames(ridgeImp) <- c("ROI_readable", "ridgeImp", "modality", "ModelingTechniqueRidge")
ridgeImp <- ridgeImp[-which(ridgeImp$modality=='all.data'),]
ridgeImp <- ridgeImp[,-c(3,4)]
sregImp <- read.csv('selfRegImpMale.csv')
sregImp[,1] <- gsub(sregImp[,1], pattern='tmpDatX', replacement='')
colnames(sregImp) <- c("ROI_readable", "srelfRegImp", "modality", "ModelingTechniqueSR")
sregImp <- sregImp[-which(sregImp$modality=='all.data'),]
sregImp <- sregImp[,-c(3,4)]
efImp <- read.csv('./effSizeImp.csv')

## Now isolate the male eff size variables
efImp <- efImp[which(efImp$sex=='M'),]

## Now combine all of these
all.data <- merge(efImp, randImp)
all.data <- merge(all.data, relifImp, by='ROI_readable')
all.data <- merge(all.data, ridgeImp, by='ROI_readable')
all.data <- merge(all.data, sregImp, by='ROI_readable')

## Now add a modality vairbale to the data
all.data$modality <- NA
all.data$modality[1:26] <- 'FA'
all.data$modality[27:86] <- 'MD'
all.data$modality[87:146] <- 'GMD'
all.data$modality[147:206] <- 'Volume'
all.data$modality[207:262] <- 'CBF'
all.data$modality[263:322] <- 'ALFF'
all.data$modality[323:382] <- 'ReHo'
all.data$modality <- factor(all.data$modality, levels=c("Volume", "GMD", "MD", "CBF", "ALFF", "ReHo"))

## Now make the names better
colnames(all.data)[c(2,6)] <- c("Cohen's D", "Ridge Coefficient")

## Now remove FA
all.data <- all.data[-grep("dti_dtitk_jlf_fa", all.data$ROI_readable),]

## Now add a little noise to ReHo ridge values so the cor isn't exactlly .6999999
set.seed(00889944)
#all.data[which(all.data$modality=='ReHo'),6] <- all.data[which(all.data$modality=='ReHo'),6] + rnorm(length(all.data[which(all.data$modality=='ReHo'),6]), .2 , .1)

## Now declare a function so I can modify the alpha of my density plots
my_dens <- function(data, mapping, ...) {
  ggplot(data = data, mapping=mapping) +
    geom_density(..., alpha = 0.7)
}

str_c <- function (..., sep = "", collapse = NULL) {
  paste(..., sep = sep, collapse = collapse)
}


cor_text <- function(data, mapping, alignPercent = 0.6, method = "pearson",
    use = "complete.obs", corAlignPercent = NULL, corMethod = NULL,
    corUse = NULL, sgnf=3, ...) {

  if (! is.null(corAlignPercent)) {
    stop("'corAlignPercent' is deprecated.  Please use argument 'alignPercent'")
  }
  if (! is.null(corMethod)) {
    stop("'corMethod' is deprecated.  Please use argument 'method'")
  }
  if (! is.null(corUse)) {
    stop("'corUse' is deprecated.  Please use argument 'use'")
  }

  useOptions <- c(
    "all.obs",
    "complete.obs",
    "pairwise.complete.obs",
    "everything",
    "na.or.complete"
  )
  use <- pmatch(use, useOptions)
  if (is.na(use)) {
    warning("correlation 'use' not found.  Using default value of 'all.obs'")
    use <- useOptions[1]
  } else {
    use <- useOptions[use]
  }

  cor_fn <- function(x, y) {
    # also do ddply below if fn is altered
    cor(x, y, method = method, use = use)
  }

  # xVar <- data[[as.character(mapping$x)]]
  # yVar <- data[[as.character(mapping$y)]]
  # x_bad_rows <- is.na(xVar)
  # y_bad_rows <- is.na(yVar)
  # bad_rows <- x_bad_rows | y_bad_rows
  # if (any(bad_rows)) {
  #   total <- sum(bad_rows)
  #   if (total > 1) {
  #     warning("Removed ", total, " rows containing missing values")
  #   } else if (total == 1) {
  #     warning("Removing 1 row that contained a missing value")
  #   }
  #
  #   xVar <- xVar[!bad_rows]
  #   yVar <- yVar[!bad_rows]
  # }

  # mapping$x <- mapping$y <- NULL

  xData <- eval_data_col(data, mapping$x)
  yData <- eval_data_col(data, mapping$y)

  colorData <- eval_data_col(data, mapping$colour)
  if (is.numeric(colorData)) {
    stop("ggally_cor: mapping color column must be categorical, not numeric")
  }

  if (use %in% c("complete.obs", "pairwise.complete.obs", "na.or.complete")) {
    if (!is.null(colorData) && (length(colorData) == length(xData))) {
      rows <- complete.cases(xData, yData, colorData)
    } else {
      rows <- complete.cases(xData, yData)
    }

    if (any(!rows)) {
      total <- sum(!rows)
      if (total > 1) {
        warning("Removed ", total, " rows containing missing values")
      } else if (total == 1) {
        warning("Removing 1 row that contained a missing value")
      }
    }

    if (!is.null(colorData) && (length(colorData) == length(xData))) {
      colorData <- colorData[rows]
    }
    xData <- xData[rows]
    yData <- yData[rows]
  }

  xVal <- xData
  yVal <- yData

  # if the mapping has to deal with the data, remove it
  if (packageVersion("ggplot2") > "2.2.1") {
    for (mappingName in names(mapping)) {
      itemData <- eval_data_col(data, mapping[[mappingName]])
      if (!inherits(itemData, "AsIs")) {
        mapping[[mappingName]] <- NULL
      }
    }
  } else {
    if (length(names(mapping)) > 0){
      for (i in length(names(mapping)):1){
        # find the last value of the aes, such as cyl of as.factor(cyl)
        tmp_map_val <- deparse(mapping[names(mapping)[i]][[1]])
        if (tmp_map_val[length(tmp_map_val)] %in% colnames(data))
          mapping[[names(mapping)[i]]] <- NULL

        if (length(names(mapping)) < 1){
          mapping <- NULL
          break;
        }
      }
    }
  }

  if (
    !is.null(colorData) &&
    !inherits(colorData, "AsIs")
  ) {

    cord <- plyr::ddply(
      data.frame(x = xData, y = yData, color = colorData),
      "color",
      function(dt) {
        cor_fn(dt$x, dt$y)
      }
    )
    colnames(cord)[2] <- "correlation"

    cord$correlation <- signif(as.numeric(cord$correlation), sgnf)

    # put in correct order
    lev <- levels(as.factor(colorData))
    ord <- rep(-1, nrow(cord))
    for (i in 1:nrow(cord)) {
      for (j in seq_along(lev)){
        if (identical(as.character(cord$color[i]), as.character(lev[j]))) {
          ord[i] <- j
        }
      }
    }

    # print(order(ord[ord >= 0]))
    # print(lev)
    cord <- cord[order(ord[ord >= 0]), ]
    cord$label <- str_c(cord$color, ": ", cord$correlation)

    # calculate variable ranges so the gridlines line up
    xmin <- min(xVal, na.rm = TRUE)
    xmax <- max(xVal, na.rm = TRUE)
    xrange <- c(xmin - 0.01 * (xmax - xmin), xmax + 0.01 * (xmax - xmin))
    ymin <- min(yVal, na.rm = TRUE)
    ymax <- max(yVal, na.rm = TRUE)
    yrange <- c(ymin - 0.01 * (ymax - ymin), ymax + 0.01 * (ymax - ymin))


    # print(cord)
    p <- ggally_text(
      label   = str_c("Cor : ", signif(cor_fn(xVal, yVal), sgnf)),
      mapping = mapping,
      xP      = 0.38,
      yP      = 0.9,
      xrange  = xrange,
      yrange  = yrange,
      color   = "black",
      fontface='bold',
      size=6,
      ...
    ) +
    #element_bw() +
    theme(legend.position = "none")

    xPos <- rep(alignPercent, nrow(cord)) * diff(xrange) + min(xrange, na.rm = TRUE)
    yPos <- seq(
      from = 0.9,
      to = 0.2,
      length.out = nrow(cord) + 1)
    yPos <- yPos * diff(yrange) + min(yrange, na.rm = TRUE)
    yPos <- yPos[-1]
    # print(range(yVal))
    # print(yPos)

    cordf <- data.frame(xPos = xPos, yPos = yPos, labelp = cord$label)
    cordf$labelp <- factor(cordf$labelp, levels = cordf$labelp)
    # print(cordf)
    # print(str(cordf))

    p <- p + geom_text(
      data = cordf,
      aes(
        x = xPos,
        y = yPos,
        label = labelp,
        color = labelp
      ),
      hjust = 1,
      fontface='bold',
      size=6,
      ...

    )

    p
  } else {
    # calculate variable ranges so the gridlines line up
    xmin <- min(xVal, na.rm = TRUE)
    xmax <- max(xVal, na.rm = TRUE)
    xrange <- c(xmin - 0.01 * (xmax - xmin), xmax + 0.01 * (xmax - xmin))
    ymin <- min(yVal, na.rm = TRUE)
    ymax <- max(yVal, na.rm = TRUE)
    yrange <- c(ymin - 0.01 * (ymax - ymin), ymax + 0.01 * (ymax - ymin))

    p <- ggally_text(
      label = paste(
        "Corr:\n",
        round(
          cor_fn(xVal, yVal),
          digits=sgnf
        ),
        sep = "", collapse = ""
      ),
      mapping,
      xP = 0.5,
      yP = 0.5,
      xrange = xrange,
      yrange = yrange,
      ...
    ) +
    #element_bw() +
    theme(legend.position = "none")

    p
  }
}

PlotCont = function(data, mapping, ...){
  print(names(data))
  print(mapping)
  ggplot(data, mapping) + geom_point(..., shape = 19) +
      geom_smooth(method = "lm") + geom_point(mapping, shape = 19) +
      geom_smooth(method='lm', aes(color=NULL, group=NULL), color='black')
}

#pdf('scaleRidgeMal.pdf', width=4, height=4)
plot1 <- ggpairs(data=all.data,columns=c(2,6),ggplot2::aes(colour=modality,alpha = .6), title="Male",upper=list(continuous=wrap(cor_text, sgnf=2)),lower = list(continuous = PlotCont))
plot1[2,1] <- plot1[2,1] + scale_x_continuous(limits = c(-1, 1))
plot1 <- plot1 + theme(text = element_text(size = 12, lineheight = 1, face='bold'))

## Now we will read all of the male values and produce the correlation matrix
randImp <- read.csv('./randForImpFemale.csv')
colnames(randImp) <- c("ROI_readable", "randomForestImp", "modality", "ModelingTechniqueRF")
randImp <- randImp[-which(randImp$modality=='all.data'),]
randImp <- randImp[,-c(3,4)]
relifImp <- read.csv('./reliefFImpFemale.csv')
colnames(relifImp) <- c("ROI_readable", "reliefImp", "modality", "ModelingTechniqueRF")
relifImp <- relifImp[-which(relifImp$modality=='all.data'),]
relifImp <- relifImp[,-c(3,4)]
ridgeImp <- read.csv('./ridgeImpFemale.csv')
colnames(ridgeImp) <- c("ROI_readable", "ridgeImp", "modality", "ModelingTechniqueRidge")
ridgeImp <- ridgeImp[-which(ridgeImp$modality=='all.data'),]
ridgeImp <- ridgeImp[,-c(3,4)]
sregImp <- read.csv('selfRegImpFemale.csv')
sregImp[,1] <- gsub(sregImp[,1], pattern='tmpDatX', replacement='')
colnames(sregImp) <- c("ROI_readable", "srelfRegImp", "modality", "ModelingTechniqueSR")
sregImp <- sregImp[-which(sregImp$modality=='all.data'),]
sregImp <- sregImp[,-c(3,4)]
efImp <- read.csv('./effSizeImp.csv')

## Now isolate the female eff size variables
efImp <- efImp[which(efImp$sex=='F'),]

## Now combine all of these
all.data <- merge(efImp, randImp)
all.data <- merge(all.data, relifImp, by='ROI_readable')
all.data <- merge(all.data, ridgeImp, by='ROI_readable')
all.data <- merge(all.data, sregImp, by='ROI_readable')

## Now add a modality vairbale to the data
all.data$modality <- NA
all.data$modality[1:26] <- 'FA'
all.data$modality[27:86] <- 'MD'
all.data$modality[87:146] <- 'GMD'
all.data$modality[147:206] <- 'Volume'
all.data$modality[207:262] <- 'CBF'
all.data$modality[263:322] <- 'ALFF'
all.data$modality[323:382] <- 'ReHo'
all.data$modality <- factor(all.data$modality, levels=c("Volume", "GMD", "MD", "CBF", "ALFF", "ReHo"))

## Now make the names better
colnames(all.data)[c(2,6)] <- c("Cohen's D", "Ridge Coefficient")

## Now remove FA
all.data <- all.data[-grep("dti_dtitk_jlf_fa", all.data$ROI_readable),]

pdf('scaleRidgeVals.pdf', width=12, height=6)
plot2 <- ggpairs(data=all.data,columns=c(2,6),ggplot2::aes(colour=modality, alpha=0.6) ,title="Female",upper=list(continuous=wrap(cor_text, sgnf=2)),lower = list(continuous = PlotCont))
plot2[2,1] <- plot2[2,1] + scale_x_continuous(limits = c(-1, 1))
plot2[1,1] <- plot2[1,1] + scale_y_continuous(limits = c(0, 6))
plot2 <- plot2 + theme(text = element_text(size = 12, lineheight = 1, face='bold'))
multiplot(plot1, plot2, cols=2)
dev.off()
