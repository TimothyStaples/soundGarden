# Sound garden
rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#setwd()

library(sound)
library(vcd)
library(performance)
library(brms)
library(vegan)
library(DHARMa)

# source functions from 'functions' sub-folder
sapply(paste0("./functions/", list.files("./functions")), source)

# DATA INPUT ####

sg <- read.csv("./data/210122_SoundGardenData.csv",
               stringsAsFactors = TRUE)

# convert factors to numbers
numVars <- c("tongueFlickCount", "freezeBehaviourCount", "headJerkCount",
  "headMovementLeftDirection", "headMovementRightDirection",
  "headMovementTowardSpeaker", "headMovementAwayFromSpeaker",
  "headMovementDistanceTotal", "Fixation.behaviour.", "hissesNumber",
  "parascoping", "lowerJawDrop")

sg[,numVars] = sapply(numVars, function(x){
  as.numeric(as.character(sg[,x]))
})

# remove familiarization block
sg <- sg[sg$trialBlock != "0",]

# merge in snake reference table data
snake <- read.csv("./data/snakeRefTable.csv")
snakeCols <- read.csv("./data/colours.csv")

snake$genus <- substr(snake$species, 1, regexpr(" ", snake$species)-1)

sg <- merge(sg, snake,
            by.x="snakeName", by.y="name",
            all.x=TRUE, all.y=FALSE, sort=FALSE)

sg <- merge(sg, snakeCols[,c(1,3,4)],
            by.x="species", by.y="species",
            all.x=TRUE, all.y=FALSE, sort=FALSE)

genCols <- sg[!duplicated(sg$genus), c("genus", "colour", "colourL")]

sg$moveBin <- ifelse(sg$headMovementDistanceTotal > 20, 1, 0)

# convert exploration behaviour to binary
sg$cautionaryExplorationBehaviour[sg$cautionaryExplorationBehaviour == ""] = NA
sg$cautionaryExplorationBehaviour[sg$cautionaryExplorationBehaviour == "na"] = NA
sg$cautionaryExplorationBehaviour <- droplevels(sg$cautionaryExplorationBehaviour)
sg$cautionaryExplorationBehaviour <- as.numeric(sg$cautionaryExplorationBehaviour) - 1

sg$defcautBehav <- sg$freezeBehaviourCount +
  sg$hissesNumber +
  sg$Fixation.behaviour. +
  sg$headJerkCount +
  sg$lowerJawDrop +
  sg$parascoping +
  sg$cautionaryExplorationBehaviour

sg$defcautBin <- ifelse(sg$defcautBehav > 0, 1, 0)
sg$tongueBin <- ifelse(sg$tongueFlickCount > 0, 1, 0)

sg$logAge <- log(sg$age)

sg$soundFact <- as.factor(sg$soundToPlay)

# DATA EXPLORATION ####

boxplot(sg$tongueFlickCount ~ sg$genus, las=2, xlab="", mar=c(6,2,1,1))

boxplot(sg$headMovementDistanceTotal ~  sg$soundToPlay * sg$snakeName, las=2, xlab="", mar=c(6,2,1,1),
        col = rep(as.factor(snake$species)[match(levels(sg$snakeName), snake$name)], each=4))

pairs(sg[,c("tongueFlickCount",
            "freezeBehaviourCount", "headJerkCount",
            "headMovementLeftDirection", "headMovementRightDirection",
            "headMovementTowardSpeaker", "headMovementAwayFromSpeaker",
            "headMovementDistanceTotal", "Fixation.behaviour.", "hissesNumber")],
      mar=c(0,0,0,0), oma=c(3,3,3,3))

cor(sg[,c("tongueFlickCount",
      "freezeBehaviourCount", "headJerkCount",
      "headMovementLeftDirection", "headMovementRightDirection",
      "headMovementTowardSpeaker", "headMovementAwayFromSpeaker",
      "headMovementDistanceTotal", "Fixation.behaviour.", "hissesNumber",
      "parascoping")],
    use="complete.obs")

hist(sg$tongueFlickCount)
hist(sg$freezeBehaviourCount)
hist(sg$headJerkCount)
hist(sg$headMovementTowardSpeaker)

sg$colour <- as.character(sg$colour)
sg$colourL <- as.character(sg$colourL)

#         comparison of responses ####        

plotX <- seq(0.15,0.95,len=4)
xWidth = 0.0175

ploty <- rev(seq(0.075,0.4,len=7)[2:6])
yWidth = abs(diff(ploty[1:2])) /2 
sg$soundBin <- ifelse(sg$soundFact == "0", FALSE, TRUE)

pdf("./plots/behaviour breakdown.pdf", height=3.5, width=6, useDingbats=FALSE)

split.screen(rbind(c(0,1,0.4,1),
                   cbind(plotX[1]+xWidth, plotX[2]-xWidth, ploty - yWidth, ploty + yWidth),
                   cbind(plotX[2]+xWidth, plotX[3]-xWidth, ploty - yWidth, ploty + yWidth),
                   cbind(plotX[3]+xWidth, plotX[4]-xWidth, ploty - yWidth, ploty + yWidth)))

screen(1)
par(mar=c(0,0,0,0), ps=8)
plot(x=NULL, y=NULL, xlim=c(0,1), ylim=c(0,1), axes=FALSE, xlab="", ylab="")

plot.pos <- data.frame(genus = sort(unique(sg$genus)),
                       pos = seq(0.1,0.9, len=length(unique(sg$genus))))

cat <- c("Ambush Elapid", "Active/arboreal\nPython", "Arboreal Elapid", "Active Elapid")
catPos <- c(plot.pos$pos[1:3], mean(plot.pos$pos[4:5]))

text(y=0.85, x=catPos, adj=0.5, labels=cat)
segments(y0=0.5, y1=0.75, x0=catPos[1:3], x1=catPos[1:3])
axis(side=3, line=-2.75, at=plot.pos$pos[4:5], tcl=2, labels=NA)
segments(y0=0.7425, y1=0.79, x0=catPos[4], x1=catPos[4])

gNames <- lapply(split(sg, f=sg$genus), function(test){

# species names etc
sp <- as.character(sort(unique(snake$species[snake$genus == test$genus[1]])))
spCount <- table(as.character(snake$species[snake$genus == test$genus[1]]))

sp[1] <- gsub(" ", "\n", sp[1])
sp[-1] <- paste0(substr(sp[-1], 1, 1),". ",
                 substr(sp[-1], regexpr(" ", sp[-1])+1, nchar(sp[-1])))

return(list(sp, spCount))
})
gCount <- sapply(gNames, function(x){x[[2]]})
gNames <- sapply(gNames, function(x){x[[1]]})

gCol <- as.character(sapply(split(sg, f=sg$genus), function(test){
col <- snakeCols$colour[grepl(test$genus[1], snakeCols$species)][1]
#colL <- snakeCols$colourL[grepl(test$genus[1], snakeCols$species)][1]
#pos <- plot.pos$pos[plot.pos$genus == test$genus[1]] + c(-0.05,0,0.05)
return(col)
}))
gColL <- as.character(sapply(split(sg, f=sg$genus), function(test){
  #col <- snakeCols$colour[grepl(test$genus[1], snakeCols$species)][1]
  colL <- snakeCols$colourL[grepl(test$genus[1], snakeCols$species)][1]
  return(colL)
}))

# photo spot
rect(ybottom=0.25, ytop=0.7,
     xleft=plot.pos$pos-0.1, xright=plot.pos$pos+0.1,
     col="white", border=gCol, lwd=2.5)

par(lheight=0.75)
text(x=plot.pos$pos -0.1, y=0.18, labels=sapply(gNames, function(x){x[[1]]}),
     col=gCol, font=3, adj=0)
text(x=plot.pos$pos+0.075, y=0.18, labels=paste0("(",sapply(gCount, function(x){x[[1]]}),")"),
     col=gCol)

text(x=plot.pos$pos-0.1, y=0.09, labels=sapply(gNames, function(x){ifelse(length(x)<2, "", x[[2]])}),
     col=gCol, font=3, adj=0)
tempCount <- sapply(gCount, function(x){ifelse(length(x)<2, NA, x[[2]])})
text(x=plot.pos$pos+0.075, y=0.09, labels=ifelse(is.na(tempCount),
                                                "",
                                                paste0("(",tempCount,")")),
     col=gCol)
par(lheight=1)

# text(x=-0.09, y=spPos, adj=1, col=col, font=3, labels=sp)
# text(x=-0.065, y=spPos, adj=0.5, col=col, font=1, labels=paste0("(",spCount, ")"))
# par(lheight=1)

# Legend
# text(x=0.25, y=-0.02, adj=0.5,
#      labels=expression(bold("T")*": > 0 Tongue flicks; "*bold("D")*": > 0 Defensive/cautious behaviors; "*bold("M")*": > 20cm head movement; "*bold("Color")*": behavior observed"))

close.screen(1)

# Proportion plots
sapply(1:3, function(bN){

var <- c("tongueBin", "defcautBin", "moveBin")[bN]
  
tongueTable <- table(sg[,var], sg$soundBin, sg$genus)
  
sapply(1:length(gCol), function(n){
  
  screen(1 + 5 * (bN-1)+n)
  par(mar=c(0,0,0,0), ps=8, tcl=-0.25)
  
  plot(x=NULL, y=NULL, xlim=c(0,1), ylim=c(0,1),
       axes=FALSE, xlab="", ylab="", xaxs="i", yaxs="i")
  
  if(n==5){
    axis(side=1, at = c(0,0.5,1), las=1, mgp=c(3,0,0))
    mtext(side=1, line=0.65, text="Proportion of trials", las=0)
    
  }
  if(n==1){
    mtext(side=3, 
          text=c("Tongue flick",
                 "Defensive/cautious behavior",
                 "Moving > 20cm")[bN], las=0, font=2)
  }
  
  if(bN==1){
    axis(side=2, at=0.5, 
         labels=dimnames(tongueTable)[[3]][n],
         col.axis=gCol[n], las=1, font.axis=3, mgp=c(3,0.5,0))
  } else {axis(side=2, at=0.5, labels=NA)}
  
  subTab <- tongueTable[,,n]
  subTab <- t(t(subTab) / colSums(subTab))
  
  # controls
  rect(ybottom = 0, ytop = 1, 
       xleft = c(0, subTab[2,1]),
       xright = c(subTab[2,1], 1),
       col=c(gCol[n], "white"))
  
  close.screen(1 + 5 * (bN-1)+n)
  
})
})

close.screen(all.screens=TRUE)
dev.off()

#         breakdown of defensive/cautious behaviours by genus ####

defcaut <- sg[,c("freezeBehaviourCount", "hissesNumber", "Fixation.behaviour.", 
                 "headJerkCount", "lowerJawDrop", "parascoping", "cautionaryExplorationBehaviour")]
defcaut <- ifelse(defcaut > 0 , 1, 0)

dcMat <- do.call("rbind", lapply(sort(unique(sg$genus)), function(x){
  subdc <- as.data.frame(defcaut[sg$genus == x,])
  submat <- sapply(subdc, function(y){tapply(y, sg$soundFact[sg$genus==x], sum, na.rm=TRUE)})
  return(data.frame(genus=x,
                    soundFact=0:3,
                    submat))
  }))

dcMatProp <- as.data.frame(prop.table(as.matrix(dcMat[,-(1:2)]), 1))
dcMatProp[is.na(dcMatProp)] = 0 

rownames(dcMatProp) = paste0(dcMat$genus, ":", dcMat$soundFact)

dcMat$dcCount <- rowSums(dcMat[,-(1:2)])
dcMat$iszero <- dcMat$dcCount == 0

dcMatProp <- dcMatProp[!dcMat$iszero,]

dcOrd <- metaMDS(dcMatProp)
dcOrdPoints <- dcOrd$points

dcMatPlot <- dcMat[!dcMat$iszero,]

# PERMANOVA of def/caut behaviour change
dcPerm <- adonis2(dcMatProp ~ genus * soundFact, data=dcMatPlot)
dcPerm

# do dissimilarities reduce between C, S1, S2 and S3

dcDiff <- vegdist(dcMatProp)

# remove two genera that don't 
dcMatSub <- dcMatProp[!grepl("Acanthophis|Hoplocephalus", rownames(dcMatProp)),]
dcCont <- vegdist(dcMatSub[grepl("0", rownames(dcMatSub)),])
dcS1 <- vegdist(dcMatSub[grepl("1", rownames(dcMatSub)),])
dcS2 <- vegdist(dcMatSub[grepl("2", rownames(dcMatSub)),])
dcS3 <- vegdist(dcMatSub[grepl("3", rownames(dcMatSub)),])

dcAs <- vegdist(dcMatSub[grepl("Pseudo", rownames(dcMatSub)),])


sapply(list(dcCont, dcS1, dcS2, dcS3), mean)

# DEFENSIVE/CAUTIOUS BEHAVIOUR MODEL ####
#         model ####

sg_dc <- droplevels(sg[!sg$genus %in% c("Acanthophis", "Hoplocephalus"),])

with(sg[sg$soundFact==0,], table(genus, defcautBin))

defcautM0 <- brm(defcautBin ~ soundFact * genus + logAge + (1|snakeName) + (1|trialBlock) + (1|roomSideSoundPlayed),
                 family="bernoulli", data=sg_dc,
                 cores=4, iter=10000,
                 control = list(adapt_delta = 0.99,
                                max_treedepth=15),
                 prior = c(set_prior("cauchy(0,2)", class = "sd", group = "snakeName", coef = "Intercept"),
                           set_prior("cauchy(0,2)", class = "sd", group = "trialBlock", coef = "Intercept"),
                           set_prior("cauchy(0,2)", class = "sd", group = "roomSideSoundPlayed", coef = "Intercept"),
                           set_prior("normal(0,5)", class = "Intercept"),
                           set_prior("normal(0,5)", class = "b")))
saveRDS(defcautM0, "./outputs/defcautNosex.rds")
defcautM0 <- readRDS("./outputs/defcautNosex.rds")
summary(defcautM0)

defcautM1 <- brm(defcautBin ~ soundFact * genus + logAge + Sex + (1|snakeName) + (1|trialBlock) + (1|roomSideSoundPlayed),
                   family="bernoulli", data=sg_dc,
                 cores=4, iter=10000,
                 control = list(adapt_delta = 0.99,
                                max_treedepth=15),
                 prior = c(set_prior("cauchy(0,2)", class = "sd", group = "snakeName", coef = "Intercept"),
                           set_prior("cauchy(0,2)", class = "sd", group = "trialBlock", coef = "Intercept"),
                           set_prior("cauchy(0,2)", class = "sd", group = "roomSideSoundPlayed", coef = "Intercept"),
                           set_prior("normal(0,5)", class = "Intercept"),
                           set_prior("normal(0,5)", class = "b")))
saveRDS(defcautM1, "./outputs/defcautsex.rds")

defcautM0 <- readRDS("./outputs/defcautNosex.rds")
summary(defcautM0)
ranef(defcautM0)
r2(defcautM0)
#plot(defcautM0)
brms::pp_check(defcautM0)
#conditional_effects(defcautM0)
loo(defcautM0, defcautM1)

write.csv(summary(defcautM0)$fixed, "./outputs/defcautFixedSumm.csv")
write.csv(do.call("rbind", summary(defcautM0)$random), "./outputs/defcautRandomSumm.csv")

# sample from the Posterior Predictive Distribution
defPreds <- posterior_predict(defcautM0, nsamples = 250, summary = FALSE)
defPreds <- t(defPreds)

defRes <- createDHARMa(simulatedResponse = defPreds, 
                    fittedPredictedResponse = apply(defPreds, 1, median), 
                    observedResponse = sg_dc$defcautBin[!is.na(sg_dc$defcautBin)], 
                    integerResponse = F)
plot(defRes)
testDispersion(defRes)
testUniformity(defRes)
loo(defcautM0)


#  plot ####

cont.pred <- data.frame(genus=sort(unique(sg_dc$genus)))
cont.pred$soundFact=0
cont.pred$logAge= mean(sg_dc$logAge)

defcautContPred <- fitted(defcautM0, newdata = cont.pred, scale="linear",
                              re_formula = NA, summary = FALSE)

treat.pred <- expand.grid(logAge= mean(sg_dc$logAge),
                         genus=sort(unique(sg_dc$genus)),
                         soundFact=factor(1:3))

defcautTreatPred <- fitted(defcautM0, newdata = treat.pred, scale="linear",
                                         re_formula = NA, summary = FALSE)

defcautTreatPred <- plogis(defcautTreatPred) - plogis(cbind(defcautContPred, 
                                                            defcautContPred, 
                                                            defcautContPred))

contPred <- cbind(cont.pred,
                  Estimate = apply(defcautContPred, 2, mean),
                  Q2.5 = apply(defcautContPred, 2, quantile, prob=0.025),
                  Q97.5 = apply(defcautContPred, 2, quantile, prob=0.975))

defcautPred <- cbind(treat.pred,
                  Estimate = apply(defcautTreatPred, 2, mean),
                  Q2.5 = apply(defcautTreatPred, 2, quantile, prob=0.025),
                  Q97.5 = apply(defcautTreatPred, 2, quantile, prob=0.975))

defcautPred$pos <- (2:4)[defcautPred$soundFact] + seq(-0.2,0.2,len=length(unique(defcautPred$genus)))[defcautPred$genus]

contPred$pos <- 0.5 + seq(-0.2,0.2,len=3)[contPred$genus]
  
pdf("./plots/defcaut.pdf", height=7, width=4.5, useDingbats = FALSE)

split.screen(rbind(c(0.12,0.37,0.6,0.965),
                   c(0.49,0.99,0.6,0.965),
                   c(0.195,0.895,0.1,0.55)))

screen(1)
par(mar=c(0,0,0,0), las=1, ps=8, mgp=c(3,0.5,0), tcl=-0.25)
plot(x=NULL, y=NULL, xlim=c(0.2,0.8), ylim=c(0,1), xlab="", ylab="", axes=FALSE)
axis(side=1, at=0.5, labels = "Control", line=0, mgp=c(3,0.5,0))
axis(side=2)
mtext(side=2, line=1.65, text="Probability of exhibiting defensive/cautious behavior", las=0) 

par(xpd=NA)
lines(y=rep(relative.axis.point(-0.0425, "y"), 2), x=c(0.39,0.61), col="red")
par(xpd=FALSE)

segments(x0=contPred$pos, x1=contPred$pos,
         y0=plogis(contPred$Q2.5), y1=plogis(contPred$Q97.5), lwd=1,
         col=sg$colour[match(contPred$genus, sg$genus)])

points(y=plogis(contPred$Estimate), x=contPred$pos, pch=21,
       bg=sg$colour[match(contPred$genus, sg$genus)], lwd=0.5, cex=1.6)

genSub <- substr(contPred$genus, 1, 1)
genSub[genSub=="A"] = paste0(genSub[genSub=="A"], "s")
text(y=plogis(contPred$Estimate), x=contPred$pos, labels=genSub, col="white",
     cex=0.8)
text(x=relative.axis.point(0.025, "x"),
     y=relative.axis.point(0.965, "y"), labels="(A)", font=2, adj=0)
box()
close.screen(1)

screen(2)
par(mar=c(0,0,0,0), las=1, ps=7, mgp=c(3,0.5,0), tcl=-0.25)
plot(x=NULL, y=NULL, xlim=c(1.65,4.35), ylim=c(-0.4,0.7),
     xaxt="n", xlab="", ylab="", yaxs="i")

abline(h=0, lty="31")
axis(side=1, at=2:4, labels=c("0-150Hz", "150-300Hz", "300-450Hz"), 
     line=0, mgp=c(3,0.5,0))
test1 <- Sine(75, 0.035, 10000)
test2 <- Sine(225, 0.035, 10000)
test3 <- Sine(375, 0.035, 10000)
par(xpd=NA)
lines(y=relative.axis.point(-0.0425, "y") + test1$sound / 100, x=seq(1.75,2.25, len=length(test1$sound)), col="red")
lines(y=relative.axis.point(-0.0425, "y") +test2$sound / 100, x=seq(2.75,3.25, len=length(test2$sound)), col="red")
lines(y=relative.axis.point(-0.0425, "y") +test3$sound / 100, x=seq(3.75,4.25, len=length(test3$sound)), col="red")
par(xpd=FALSE)
 
mtext(side=2, line=1.5, text=expression(Delta*" Probability from control"), las=0) 

defcautSig <- defcautPred$Q97.5 < 0 | defcautPred$Q2.5 > 0

segments(x0=defcautPred$pos, x1=defcautPred$pos,
         y0=defcautPred$Q2.5, y1=defcautPred$Q97.5, lwd=1,
         col=ifelse(defcautSig,
                    sg$colour[match(defcautPred$genus, sg$genus)],
                    sg$colourL[match(defcautPred$genus, sg$genus)]))

points(y=defcautPred$Estimate,
       x=defcautPred$pos,
       pch=ifelse(defcautSig, 21, 16),
       col=ifelse(defcautSig, "black", sg$colourL[match(defcautPred$genus, sg$genus)]),
       bg=ifelse(defcautSig,
                 sg$colour[match(defcautPred$genus, sg$genus)],
                 NA), cex=1.6)

genSub <- substr(defcautPred$genus, 1, 1)
genSub[genSub=="A"] = "As"
text(y=defcautPred$Estimate, x=defcautPred$pos, labels=genSub, col="white",
     cex=0.8)

text(x=relative.axis.point(0.02, "x"),
     y=relative.axis.point(0.965, "y"), labels="(B)", font=2, adj=0)
close.screen(2)

screen(3)
par(mar=c(0,0,0,0), las=1, ps=8, mgp=c(3,0,0), tcl=-0.25)
plot(x=NULL,y=NULL, xlim=c(-1.5,1.3), ylim=c(-1,1), asp=1, yaxt="n", xlab="", ylab="")
axis(side=2, mgp=c(3,0.5,0))
mtext(side=1,line=0.75, text="nMDS 1")
mtext(side=2,line=1.5, text="nMDS 2", las=0)

arrows(x0=0, y0=0, x1=dcOrd$species[,1], y1=dcOrd$species[,2], length=0.05, lwd=2)
text(dcOrd$species, labels=c("Freeze", "Hiss", "Fixation", "Head jerk", "Jaw drop", "Periscoping", "Cautious\nexploration"), 
     font=1, pos=c(1, 4, 2, 2, 2, 1, 1), offset=0.3)

sapply(sort(unique(dcMatPlot$genus)), function(gen){
  subPoints <- dcOrd$points[dcMatPlot$genus == gen,]
  lines(subPoints, col=as.character(genCols$colourL[genCols$genus == gen]), lwd=2)
})

points(dcOrdPoints, pch=c(21,22,23,24)[as.factor(dcMatPlot$soundFact)], 
       bg=as.character(genCols$colour[match(dcMatPlot$genus, genCols$genus)]),
       cex=ifelse(log(dcMatPlot$dcCount)<1, 1, 2))
text(dcOrdPoints, 
     labels=ifelse(dcMatPlot$genus %in% c("Acanthophis", "Hoplocephalus"), "",
                   c("C", "S1", "S2", "S3")[as.factor(dcMatPlot$soundFact)]), 
     col=as.character(genCols$colourL[match(dcMatPlot$genus, genCols$genus)]), 
     font=2, cex=0.85)
text(x=relative.axis.point(0.01, "x"), y=relative.axis.point(0.03, "y"),
     adj=0, labels=paste0("Stress = ", round(dcOrd$stress, 3)))

# add some genus labels
labPoints <- rbind(dcOrdPoints[dcMatPlot$genus == "Acanthophis",][1,],
                   dcOrdPoints[dcMatPlot$soundFact == 0,])

labs <- as.character(sort(unique(dcMatPlot$genus)))

sapply(1:length(labs), function(n){
  
shorts <- substring(labs[n], 1, 1)
if(shorts=="A"){shorts = ifelse(labs[n]=="Aspidites", "As", "Ac")}  
  
labT <- labs[n]
if(labs[n] %in% defcautPred$genus){
text(y=labPoints[n,2], x=labPoints[n,1] + c(0.2,0,-0.2,0,0)[n], 
     labels=bquote(italic(.(labT))~"("*.(shorts)*")"),
     col=as.character(genCols$colour[order(genCols$genus)])[n], 
     pos=c(3,4,3,3,4)[n], font=3, cex=0.85)
} else {
  text(y=labPoints[n,2], x=labPoints[n,1] + c(0.2,0,-0.2,0,0)[n], 
       labels=bquote(italic(.(labT))),
       col=as.character(genCols$colour[order(genCols$genus)])[n], 
       pos=c(3,4,3,3,4)[n], font=3, cex=0.85)
  }
})

legendYs <- rev(relative.axis.point(seq(0.8,0.97, len=4), "y"))
legendXs <- relative.axis.point(c(0.585,0.665), "x")

points(x=rep(relative.axis.point(0.698, "x"), 4), 
       y=legendYs,
       pch=21:24, bg="white", cex=1.35)

# text(x=relative.axis.point(0.76, "x"), 
#      y=legendYs,
#      labels=paste0(0:3), adj=0, cex=0.8)
text(x=relative.axis.point(0.73, "x"), 
     y=legendYs,
     labels=c("Control (C)", "0-150Hz (S1)", "150-300Hz (S2)", "300-450Hz (S3)"), adj=0)

test1 <- Sine(75, 0.025, 10000)
test2 <- Sine(225, 0.025, 10000)
test3 <- Sine(375, 0.025, 10000)
segments(x0=legendXs[1], x1=legendXs[2], y0=legendYs[1], y1=legendYs[1], col="red")
lines(y=legendYs[2] + test1$sound / 50, x=seq(legendXs[1],legendXs[2], len=length(test1$sound)), col="red")
lines(y=legendYs[3] +test2$sound / 50, x=seq(legendXs[1],legendXs[2], len=length(test2$sound)), col="red")
lines(y=legendYs[4] +test3$sound / 50, x=seq(legendXs[1],legendXs[2], len=length(test3$sound)), col="red")

rect(xleft=relative.axis.point(0.57, "x"),
     xright=par("usr")[2],
     ybottom=relative.axis.point(0.77, "y"),
     ytop=par("usr")[4])

text(x=relative.axis.point(0.02, "x"),
     y=relative.axis.point(0.965, "y"), labels="(C)", font=2, adj=0)
close.screen(3)

dev.off()

# MOVEMENT PROBABILITY MODELS ####

table(sg$moveBin, sg$soundFact, sg$genus)
with(droplevels(sg[sg$genus == "Aspidites",]), table(moveBin, soundFact, snakeName))

moveBinM1 <- brm(moveBin ~ soundFact * genus + logAge + (1|snakeName) + (1|trialBlock) + (1|roomSideSoundPlayed),
                 family="bernoulli", data=sg,
                 cores=4, iter=10000,
                 control = list(adapt_delta = 0.999,
                                max_treedepth=15),
                 prior = c(set_prior("cauchy(0,2)", class = "sd", group = "snakeName", coef = "Intercept"),
                           set_prior("cauchy(0,2)", class = "sd", group = "trialBlock", coef = "Intercept"),
                           set_prior("cauchy(0,2)", class = "sd", group = "roomSideSoundPlayed", coef = "Intercept"),
                           set_prior("normal(0,5)", class = "Intercept"),
                           set_prior("normal(0,5)", class = "b")))
saveRDS(moveBinM1, "./outputs/movebinary.rds")
moveBinM1 <- readRDS("./outputs/movebinary.rds")
# summary(moveBinM1)
# ranef(moveBinM1)
# r2(moveBinM1)
# plot(moveBinM1)
# brms::pp_check(moveBinM1)
# conditional_effects(moveBinM1)

write.csv(summary(moveBinM1)$fixed, "./outputs/movebinFixedSumm.csv")
write.csv(do.call("rbind", summary(moveBinM1)$random), "./outputs/movebinRandomSumm.csv")

# sample from the Posterior Predictive Distribution
movePreds <- posterior_predict(moveBinM1, nsamples = 250, summary = FALSE)
movePreds <- t(movePreds)

movePreds <- createDHARMa(simulatedResponse = movePreds, 
                       fittedPredictedResponse = apply(movePreds, 1, median), 
                       observedResponse = sg$moveBin[!is.na(sg$moveBin)], 
                       integerResponse = F)
plot(movePreds)
testDispersion(movePreds)
testUniformity(movePreds)
loo(moveBinM1)

# MOVEMENT DIRECTION MODEL ####

# set head direction to four level factor
levels(sg$headDirectionWhenLiftedHutch) = list(na = c("", "na"),
                                               away = "away from sound",
                                               left = "left",
                                               right = "right",
                                               toward = "toward the sound")
sg$headDirectionWhenLiftedHutch[sg$headDirectionWhenLiftedHutch=="na"] = NA
sg$headDirectionWhenLiftedHutch <- droplevels(sg$headDirectionWhenLiftedHutch)

# find the direction the snake moved most in
sg$headMovementDirection <- apply(sg[,c("headMovementRightDirection",
                                        "headMovementLeftDirection",
                                        "headMovementTowardSpeaker",
                                        "headMovementAwayFromSpeaker")],
                                  1, function(x){
                                    xMax <- x[x==max(x)]
                                    if(length(xMax)>1){return("none")}
                                    return(which.max(x))
                                  })

sg$headMovementDirection <- as.factor(sg$headMovementDirection)

levels(sg$headMovementDirection) = list(none = "none", 
                                        away = "4",
                                        right = "1",
                                        left = "2",
                                        toward = "3")

# correct for left and right being different based on the speaker playing the
# sound
sg$headMoveCorrected <- sg$headMovementDirection
sg$headMoveCorrected[sg$headMovementDirection == "left" &
                       sg$roomSideSoundPlayed == 1] = "right"
sg$headMoveCorrected[sg$headMovementDirection == "right" &
                       sg$roomSideSoundPlayed == 1] = "left"

# set up binary movement direction variables
sg$moveAway <- ifelse(sg$headMoveCorrected == "away", 1, 0)
sg$moveTowards <- ifelse(sg$headMoveCorrected == "toward", 1, 0)
sg$moveOrth <- ifelse(sg$headMoveCorrected %in% c("left", "right"), 1, 0)

sgMoveFull <- sg[complete.cases(sg[,c("moveBin", "headDirectionWhenLiftedHutch")]),]

# moveBin1dir <- brm(moveAway ~ soundFact * genus + logAge + headDirectionWhenLiftedHutch + (1|snakeName) + (1|trialBlock) + (1|roomSideSoundPlayed),
#                    family="bernoulli", data=sgMoveFull,
#                    cores=4, iter=10000,
#                    control = list(adapt_delta = 0.99,
#                                   max_treedepth = 15),
#                    prior = c(set_prior("cauchy(0,2)", class = "sd", group = "snakeName", coef = "Intercept"),
#                              set_prior("cauchy(0,2)", class = "sd", group = "trialBlock", coef = "Intercept"),
#                              set_prior("cauchy(0,2)", class = "sd", group = "roomSideSoundPlayed", coef = "Intercept"),
#                              set_prior("normal(0,5)", class = "Intercept"),
#                              set_prior("normal(0,5)", class = "b")),
#                    save_pars = save_pars(all=TRUE))
# moveBin1dir <- add_criterion(moveBin1dir, "loo", moment_match = TRUE)

moveBin2dir <- brm(moveAway ~ soundFact * genus + logAge + (1|snakeName) + (1|trialBlock) + (1|roomSideSoundPlayed),
                   family="bernoulli", data=sgMoveFull,
                   cores=4, iter=10000,
                   control = list(adapt_delta = 0.999,
                                  max_treedepth = 15),
                   prior = c(set_prior("cauchy(0,2)", class = "sd", group = "snakeName", coef = "Intercept"),
                             set_prior("cauchy(0,2)", class = "sd", group = "trialBlock", coef = "Intercept"),
                             set_prior("cauchy(0,2)", class = "sd", group = "roomSideSoundPlayed", coef = "Intercept"),
                             set_prior("normal(0,5)", class = "Intercept"),
                             set_prior("normal(0,5)", class = "b")),
                   save_pars = save_pars(all=TRUE))
moveBin2dir <- add_criterion(moveBin2dir, "loo", moment_match = TRUE)
saveRDS(moveBin2dir, "./outputs/movedirection.rds")

summary(moveBin2dir)
plot(moveBin2dir)
brms::pp_check(moveBin2dir)
conditional_effects(moveBin2dir)

write.csv(summary(moveBin2dir)$fixed, "./outputs/moveDirFixedSumm.csv")
write.csv(do.call("rbind", summary(moveBin2dir)$random), "./outputs/moveDirRandomSumm.csv")

# sample from the Posterior Predictive Distribution
movePreds <- posterior_predict(moveBin2dir, nsamples = 250, summary = FALSE)
movePreds <- t(movePreds)

movePreds <- createDHARMa(simulatedResponse = movePreds, 
                          fittedPredictedResponse = apply(movePreds, 1, median), 
                          observedResponse = sgMoveFull$moveAway[!is.na(sgMoveFull$moveAway)], 
                          integerResponse = F)
plot(movePreds)
testDispersion(movePreds)
testUniformity(movePreds)
loo(moveBin2dir)
# plot predictions ####

# binary model
bin.cont.pred <- data.frame(genus=sort(unique(sg$genus)))
bin.cont.pred$soundFact=0
bin.cont.pred$logAge= mean(sg$logAge)

moveBinContPred <- fitted(moveBinM1, newdata = bin.cont.pred, scale="linear",
                          re_formula = NA, summary = FALSE)

bin.treat.pred <- expand.grid(logAge= mean(sg$logAge),
                          genus=sort(unique(sg$genus)),
                          soundFact=factor(1:3))

moveBinTreatPred <- fitted(moveBinM1, newdata = bin.treat.pred, scale="linear",
                           re_formula = NA, summary = FALSE)

moveBinTreatPred <- plogis(moveBinTreatPred) - plogis(cbind(moveBinContPred, 
                                                            moveBinContPred, 
                                                            moveBinContPred))

binContPred <- cbind(bin.cont.pred,
                  Estimate = apply(moveBinContPred, 2, mean),
                  Q2.5 = apply(moveBinContPred, 2, quantile, prob=0.025),
                  Q97.5 = apply(moveBinContPred, 2, quantile, prob=0.975))
binTreatPred <- cbind(bin.treat.pred,
                     Estimate = apply(moveBinTreatPred, 2, mean),
                     Q2.5 = apply(moveBinTreatPred, 2, quantile, prob=0.025),
                     Q97.5 = apply(moveBinTreatPred, 2, quantile, prob=0.975))

binTreatPred$pos <- (2:4)[binTreatPred$soundFact] + seq(-0.3,0.3,len=length(unique(binTreatPred$genus)))[binTreatPred$genus]

binContPred$pos <- 0.5 + seq(-0.2,0.2,len=5)

# direction model

dir.cont.pred <- data.frame(genus=sort(unique(sg$genus)))
dir.cont.pred$soundFact=0
dir.cont.pred$logAge= mean(sgMoveFull$logAge)

movedirContPred <- fitted(moveBin2dir, newdata = dir.cont.pred, scale="linear",
                          re_formula = NA, summary = FALSE)

dir.treat.pred <- expand.grid(logAge= mean(sg$logAge),
                              genus=sort(unique(sg$genus)),
                              soundFact=factor(1:3))

movedirTreatPred <- fitted(moveBin2dir, newdata = dir.treat.pred, scale="linear",
                           re_formula = NA, summary = FALSE)

movedirTreatPred <- plogis(movedirTreatPred) - plogis(cbind(movedirContPred, 
                                                            movedirContPred, 
                                                            movedirContPred))

dirContPred <- cbind(dir.cont.pred,
                     Estimate = apply(movedirContPred, 2, mean),
                     Q2.5 = apply(movedirContPred, 2, quantile, prob=0.025),
                     Q97.5 = apply(movedirContPred, 2, quantile, prob=0.975))
dirTreatPred <- cbind(dir.treat.pred,
                      Estimate = apply(movedirTreatPred, 2, mean),
                      Q2.5 = apply(movedirTreatPred, 2, quantile, prob=0.025),
                      Q97.5 = apply(movedirTreatPred, 2, quantile, prob=0.975))

dirTreatPred$pos <- (2:4)[dirTreatPred$soundFact] + seq(-0.3,0.3,len=length(unique(dirTreatPred$genus)))[dirTreatPred$genus]

dirContPred$pos <- 0.5 + seq(-0.2,0.2,len=5)

# plot

pdf("./plots/moveagg1.pdf", height=5.5, width=7, useDingbats = FALSE)

0.99-0.55
0.5-0.07

split.screen(rbind(c(0.45,0.6,0.54,0.99),
                   c(0.675,0.99,0.54,0.99),
                   
                   c(0.45,0.6,0.07,0.52),
                   c(0.675,0.99,0.07,0.52),
                   
                   c(0.1,0.375,0.07,0.99)))

screen(1)
par(mar=c(0,0,0,0), las=1, ps=8, mgp=c(3,0.5,0), tcl=-0.25)
plot(x=NULL, y=NULL, xlim=c(0.2,0.8), ylim=c(0,1), xlab="", ylab="", axes=FALSE)
axis(side=1, at=0.5, labels = NA)
axis(side=2)
mtext(side=2, line=1.65, text="Probability of moving > 20cm", las=0, cex=1.1) 

segments(x0=binContPred$pos, x1=binContPred$pos,
         y0=plogis(binContPred$Q2.5), y1=plogis(binContPred$Q97.5), lwd=1,
         col=sg$colour[match(binContPred$genus, sg$genus)])

points(y=plogis(binContPred$Estimate), x=binContPred$pos, pch=21,
       bg=sg$colour[match(binContPred$genus, sg$genus)], lwd=0.5, cex=1.6)

genSub <- substr(binContPred$genus, 1, 1)
genSub[1:2] = paste0(genSub[1:2], c("c", "s"))
text(y=plogis(binContPred$Estimate), x=binContPred$pos, labels=genSub, col="white",
     cex=0.8)
text(x=relative.axis.point(0.025, "x"),
     y=relative.axis.point(0.965, "y"), labels="(B)", font=2, adj=0)
box()
close.screen(1)

screen(2)
par(mar=c(0,0,0,0), las=1, ps=8, mgp=c(3,0.5,0), tcl=-0.25)
plot(x=NULL, y=NULL, xlim=c(1.65,4.35), ylim=c(-0.6,1),
     xaxt="n", xlab="", ylab="", yaxs="i")

abline(h=0, lty="31")
axis(side=1, at=2:4, labels=NA)
mtext(side=2, line=1.5, text=expression(Delta*" Probability from control"), las=0, cex=1.1) 

movePredSig <- binTreatPred$Q97.5 < 0 | binTreatPred$Q2.5 > 0

segments(x0=binTreatPred$pos, x1=binTreatPred$pos,
         y0=binTreatPred$Q2.5, y1=binTreatPred$Q97.5, lwd=1,
         col=ifelse(movePredSig,
                    sg$colour[match(binTreatPred$genus, sg$genus)],
                    sg$colourL[match(binTreatPred$genus, sg$genus)]))

points(y=binTreatPred$Estimate,
       x=binTreatPred$pos,
       pch=ifelse(movePredSig, 21, 16),
       col=ifelse(movePredSig,
                  "black",
                  sg$colourL[match(binTreatPred$genus, sg$genus)]),
       bg=ifelse(movePredSig,
                 sg$colour[match(binTreatPred$genus, sg$genus)],
                 NA), 
       lwd=0.5, cex=1.6)

genSub <- substr(binTreatPred$genus, 1, 1)
genSub[binTreatPred$genus=="Aspidites"] = "As"
genSub[binTreatPred$genus=="Acanthophis"] = "Ac"
text(y=binTreatPred$Estimate, x=binTreatPred$pos, labels=genSub, col="white",
     cex=0.8)

text(x=relative.axis.point(0.02, "x"),
     y=relative.axis.point(0.965, "y"), labels="(C)", font=2, adj=0)
close.screen(2)

screen(3)
par(mar=c(0,0,0,0), las=1, ps=8, mgp=c(3,0.5,0), tcl=-0.25)
plot(x=NULL, y=NULL, xlim=c(0.2,0.8), ylim=c(0,1), xlab="", ylab="", axes=FALSE)
axis(side=1, at=0.5, labels = "Control", line=0, mgp=c(3,0.5,0))
axis(side=2)
mtext(side=2, line=1.65, text="Probability of moving away from speaker", las=0, cex=1.1) 

par(xpd=NA)
lines(y=rep(relative.axis.point(-0.0425, "y"), 2), x=c(0.375,0.625), col="red")
par(xpd=FALSE)

segments(x0=dirContPred$pos, x1=dirContPred$pos,
         y0=plogis(dirContPred$Q2.5), y1=plogis(dirContPred$Q97.5), lwd=1,
         col=sg$colour[match(dirContPred$genus, sg$genus)])

points(y=plogis(dirContPred$Estimate), x=dirContPred$pos, pch=21,
       bg=sg$colour[match(dirContPred$genus, sg$genus)], lwd=0.5, cex=1.6)

genSub <- substr(dirContPred$genus, 1, 1)
genSub[1:2] = paste0(genSub[1:2], c("c", "s"))
text(y=plogis(dirContPred$Estimate), x=dirContPred$pos, labels=genSub, col="white",
     cex=0.8)
text(x=relative.axis.point(0.025, "x"),
     y=relative.axis.point(0.965, "y"), labels="(D)", font=2, adj=0)
box()
close.screen(3)

screen(4)
par(mar=c(0,0,0,0), las=1, ps=8, mgp=c(3,0.5,0), tcl=-0.25)
plot(x=NULL, y=NULL, xlim=c(1.65,4.35), ylim=c(-0.625,0.7),
     xaxt="n", xlab="", ylab="", yaxs="i")

abline(h=0, lty="31")
axis(side=1, at=2:4, labels=c("0-150Hz", "150-300Hz", "300-450Hz"), 
     line=0, mgp=c(3,0.5,0))
test1 <- Sine(75, 0.035, 10000)
test2 <- Sine(225, 0.035, 10000)
test3 <- Sine(375, 0.035, 10000)
par(xpd=NA)
lines(y=relative.axis.point(-0.0425, "y") + test1$sound / 100, x=seq(1.75,2.25, len=length(test1$sound)), col="red")
lines(y=relative.axis.point(-0.0425, "y") +test2$sound / 100, x=seq(2.75,3.25, len=length(test2$sound)), col="red")
lines(y=relative.axis.point(-0.0425, "y") +test3$sound / 100, x=seq(3.75,4.25, len=length(test3$sound)), col="red")
par(xpd=FALSE)

mtext(side=2, line=1.5, text=expression(Delta*" Probability from control"), las=0, cex=1.1) 

moveAwaySig <- dirTreatPred$Q2.5 > 0 | dirTreatPred$Q97.5 < 0

segments(x0=dirTreatPred$pos, x1=dirTreatPred$pos,
         y0=dirTreatPred$Q2.5, y1=dirTreatPred$Q97.5, lwd=1,
         col=ifelse(moveAwaySig,
                    sg$colour[match(dirTreatPred$genus, sg$genus)],
                    sg$colourL[match(dirTreatPred$genus, sg$genus)]))

points(y=dirTreatPred$Estimate,
       x=dirTreatPred$pos,
       pch=ifelse(moveAwaySig, 21, 16),
       col=ifelse(moveAwaySig,
                  "black",
                  sg$colourL[match(dirTreatPred$genus, sg$genus)]),
       bg=ifelse(moveAwaySig,
                 sg$colour[match(dirTreatPred$genus, sg$genus)],
                 NA), 
       lwd=0.5, cex=1.6)

genSub <- substr(dirTreatPred$genus, 1, 1)
genSub[dirTreatPred$genus=="Aspidites"] = "As"
genSub[dirTreatPred$genus=="Acanthophis"] = "Ac"
text(y=dirTreatPred$Estimate, x=dirTreatPred$pos, labels=genSub, col="white",
     cex=0.8)

text(x=relative.axis.point(0.02, "x"),
     y=relative.axis.point(0.965, "y"), labels="(E)", font=2, adj=0)
close.screen(4)

# add in densities/histograms ####

sgMoveSub <- sg[!is.na(sg$headMovementDistanceTotal), ]
sgMoveSub$headMovementDistanceTotal[sgMoveSub$headMovementDistanceTotal < 10] = 9

mBreaks <- c(1, 10, seq(20,90,10), seq(100,300,50))
moveDens <- tapply(sgMoveSub$headMovementDistanceTotal,
                   list(sgMoveSub$genus, sgMoveSub$soundFact),
                   function(x){
                     table(cut(x, breaks=mBreaks))},
                   simplify = FALSE)

screen(5)
par(mar=c(0,0,0,0), las=1, ps=8, mgp=c(3,0.5,0), tcl=-0.25)
plot(x=NULL, y=NULL, xlim=c(2,140), ylim=c(-0.3,4.5),
     axes=FALSE, xlab="", ylab="")

axis(side=1, at=seq(0,100,10), labels=NA)
axis(side=1, at=seq(0,100,20), mgp=c(3,0,0))
axis(side=1, at=seq(110,150,10), labels=NA)
axis(side=1, at=seq(120,160,20), labels=c(200,300,400), mgp=c(3,0,0))

mtext(side=1, line=0.6, text="Total head movement (cm)")

treatTicks <- rev(seq(-0.35,0.35,len=4))

axis(side=2, at =  rep(0:4, each=4) + rep(treatTicks, 5),
     labels=rep(c("C","S1","S2","S3"), 5), mgp=c(3,0.5,0))

rect(xleft=par("usr")[1], xright=20,
     ybottom=par('usr')[3], ytop=par("usr")[4],
     col="grey85", border="grey70")

abline(v=100, lty="31")

test1 <- Sine(75, 0.025, 10000)
test2 <- Sine(225, 0.025, 10000)
test3 <- Sine(375, 0.025, 10000)

sapply(1:5, function(n){
  print(n)
  axis(side=2, at = range(5-n + treatTicks + c(0.1,-0.1)),
       line=2.5, labels=NA, tcl=0.25)
  
  tempGen <- sort(unique(sg$genus))[n]
  GenS <- substr(tempGen, 1, 1)
  if(GenS=="A"){GenS = ifelse(tempGen=="Aspidites", "As", "Ac")}
  
  mtext(side=2, at = 5-n,
        las=0, text=bquote(italic(.(tempGen)) ~ "("*.(GenS)*")"),
        line=2.5, col=gCol[n], font=3)
  
  linepos <- 5-n + treatTicks
  legendXs <- relative.axis.point(c(-0.24, -0.14), "x")
  par(xpd=NA)
  segments(y0=linepos[1], y1=linepos[1],
           x0=legendXs[1], x1=legendXs[2], col="red")
  lines(y= linepos[2] + test1$sound / 50, 
        x=seq(legendXs[1],legendXs[2], len=length(test1$sound)), col="red")
  lines(y=linepos[3] +test2$sound / 50, 
        x=seq(legendXs[1],legendXs[2], len=length(test2$sound)), col="red")
  lines(y=linepos[4] +test3$sound / 50, 
        x=seq(legendXs[1],legendXs[2], len=length(test3$sound)), col="red")
  par(xpd=FALSE)
  
  sapply(1:4, function(n1){
    
    tempTab <- unlist(moveDens[n,n1])
    tempTab <- tempTab / sum(tempTab)
    densScale = 0.05
    binWidth = 5
    
    tempCenters <- rowMeans(cbind(as.numeric( sub("\\((.+),.*", "\\1", names(tempTab))),
                                  as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", names(tempTab)))))
    tempCenters[1]=5
    tempCenters[tempCenters > 100] = seq(105, 105 + 10*(sum(tempCenters>100)-1),10)
    
    yBottom = 5-n + treatTicks[n1]
    tempCol <- col2rgb(gCol[n])/255
    
    # how many to make?
    tempRects <- tempTab %/% 0.2 + ifelse(tempTab %% 0.2 > 0, 1, 0)
    
    segments(x0=par("usr")[1],
             x1=max(tempCenters[tempTab >0]),
             y0=5-n + treatTicks[n1],
             y1=5-n + treatTicks[n1],
             col=rgb(0.5,0.5,0.5,0.5))
    
    sapply(1:length(tempRects), function(n2){
      
      if(tempTab[n2]==0){return(NULL)}
      
      if(tempRects[n2] > 1){
        bots <- yBottom + seq(0, densScale * (tempRects[n2]), densScale)
        tops <- bots[2:length(bots)]
        bots <- bots[1:(length(bots)-1)]
        tops[length(tops)] = tops[length(tops)-1] + densScale * ((tempTab[n2] %% 0.2)/0.2)
      } else {
        bots=yBottom
        tops=yBottom + densScale * ((tempTab[n2] %% 0.2)/0.2)
      }
      
      rect(xleft=tempCenters[n2] - binWidth,
           xright=tempCenters[n2] + binWidth,
           ybottom=bots, ytop=tops, col=c(gCol[n], gColL[n]), lwd=0.5)
    })
    
  })
})

box()
text(x=relative.axis.point(0.02, "x"),
     y=relative.axis.point(0.985, "y"), labels="(A)", font=2, adj=0)

densScale = 0.05
binWidth = 5
rect(xleft = relative.axis.point(0.765, "x") - binWidth,
     xright = relative.axis.point(0.765, "x") + binWidth,
     ybottom = relative.axis.point(0.975, "y"),
     ytop = relative.axis.point(0.975, "y") + densScale, 
     col = "white", lwd = 0.5)

text(x=relative.axis.point(0.765, "x") + binWidth,
     y=relative.axis.point(0.975, "y") + 0.5 * densScale,
     pos=4, labels= "= 20%", offset=0.25)

close.screen(5)
dev.off()
# ####
# SUPPLEMENTARY ACCELERATION MODEL ####
# Import & process data ####

accel_path <- "./data/accel_readings"
accel_files <- list.files(accel_path)
zero_files <- accel_files[grepl("no tone", accel_files)]

zeroData <- do.call("rbind", 
                      lapply(1:length(zero_files), function(n){
                        
                        print(n)
                        data <- read.csv(paste0(accel_path, "/", zero_files[n]), sep=";")
                        name <- zero_files[n]
                        
                        if(nrow(data) < 60){return(NULL)}
                        
                        # subset middle 2 seconds of data
                        data_mid <- max(data$index) * 0.5
                        data_range <- round(data_mid - 30)
                        data_range <- data_range:(data_range+60)
                        data <- data[data_range,]
                        data$indexSub <- 1:nrow(data)
                        
                        # correct measurements to hertz
                        data$time <- data$index / 30
                        
                        # convert from "gravities" to "m s-2"
                        data$x <- data$x * 9.8
                        data$y <- data$y * 9.8
                        data$z <- data$z * 9.8
                        
                        freq <- substr(name, 1, regexpr(" ", name)-1)
                        if(grepl("no tone", name)){freq = "no tone"}
                        
                        if(grepl("near", name)){dist = 0}
                        if(grepl("mid", name)){dist = 85}  
                        if(grepl("far", name)){dist = 170}
                        
                        if(grepl("foam", name)){foam = TRUE} else {foam=FALSE}
                        
                        rep <- substr(name, regexpr("_1|_2|_3", name)+1, nchar(name)-4)
                        if(grepl("no tone", name) & !grepl("_1|_2|_3", name)){rep=1}
                        
                        return(cbind(data.frame(freq = freq,
                                          dist = dist,
                                          foam = foam,
                                          rep = rep),
                                     data))
                            }))

hist(zeroData$z)

phoneZero <- zeroData$z[zeroData$freq == "no tone"]
phoneZero <- mean(phoneZero)

accel_data <- do.call("rbind", 
                      lapply(1:length(accel_files), function(n){
  
  print(n)
  data <- read.csv(paste0(accel_path, "/", accel_files[n]), sep=";")
  name <- accel_files[n]
  
  if(nrow(data) < 60){return(NULL)}
  
  # subset middle 2 seconds of data
  data_mid <- max(data$index) * 0.5
  data_range <- round(data_mid - 30)
  data_range <- data_range:(data_range+60)
  data <- data[data_range,]
  data$indexSub <- 1:nrow(data)
  
  # correct measurements to hertz
  data$time <- data$index / 30
  data$x <- data$x * 9.8
  data$y <- data$y * 9.8
  
  data$z <- data$z * 9.8
  data$zDev <- data$z - phoneZero
  data$zDevA <- abs(data$zDev)
  
  name <- gsub("pink|Pink", " ", name)
  
  freq <- substr(name, 1, regexpr(" ", name)-1)
  if(grepl("no tone", name)){freq = "no tone"}
  
  if(grepl("near", name)){dist = 0}
  if(grepl("mid", name)){dist = 85}  
  if(grepl("far", name)){dist = 170}
  
  foam = "none"
  if(grepl("handheld", name)){foam = "hand"}
  if(grepl("foam", name)){foam = "foam"}
  
  rep <- substr(name, regexpr("_1|_2|_3", name)+1, nchar(name)-4)
  if(grepl("no tone", name) & !grepl("_1|_2|_3", name)){rep=1}

  return(cbind(data.frame(freq = freq,
                    dist = dist,
                    foam = foam,
                    rep = rep,
                    group = paste(freq, dist, foam, rep, sep = ".")),
               data))
  
  return(data.frame(freq = freq,
                          dist = dist,
                          foam = foam,
                          rep = rep,
                          mean.z = mean(data$z),
                          var.z = var(data$z),
                          sd.z = sd(data$z)))
  
}))

hist(accel_data$zDevA)

accel_data$freq <- factor(accel_data$freq,
                          levels=c("no tone", "0-150", 
                                   "150-300", "300-450"))

#sound_data <- droplevels(accel_data[accel_data$freq != "no tone",])

accel_data$freqFoam <- factor(paste0(accel_data$freq,".",accel_data$foam))
accel_data$freqFoam <- relevel(accel_data$freqFoam, ref = "no tone.none")

# full model ####

library(lme4)
library(DHARMa)
library(brms)

test1 <- brm(sqrt(zDevA) ~ freqFoam * dist + (1|group), data=accel_data)
testCoef <- summary(test1)$fixed
write.csv(testCoef, "./outputs/accel_coefs.csv")
plot(test1)

r2(test1)
#plot(defcautM0)
brms::pp_check(test1)

defPreds <- posterior_predict(test1, nsamples = 250, summary = FALSE)
defPreds <- t(defPreds)

defRes <- createDHARMa(simulatedResponse = defPreds, 
                       fittedPredictedResponse = apply(defPreds, 1, median), 
                       observedResponse = sqrt(accel_data$zDevA), 
                       integerResponse = F)
plot(defRes)
testDispersion(defRes)
testUniformity(defRes)
loo(test1)

testUniformity(simulateResiduals(test1))

# no distance model ####

test2 <- brm(sqrt(zDevA) ~ freqFoam + (1|group), data=accel_data)
summary(test2)

testData <- data.frame(freqFoam = unique(accel_data$freqFoam))
testPreds <- predict(test2, newdata=testData, se.fit=TRUE, re_formula = NA)

# model coef plot ####

pdf("./plots/accel_plot.pdf", height=3, width=4.5, useDingbats = FALSE)
par(mar=c(2.5,3.5,0.5,0.5), ps=10, tcl=-0.25, mgp=c(3,0.5,0), las=1)
plot(x=NULL, y=NULL, ylim=c(0,0.13), xlim=c(1,4.5), type="n",
     xlab="", ylab="", xaxt="n")
mtext(side=2, line=2.25, las=0, text=expression("Absolute delta acceleration (ms"^2*")"))

xpos <- c(1.8, 2, 2.2, 2.8, 3, 3.2, 3.8, 4, 4.2, 1.25)

# control
jitt.amount <- 0.075

points(y=accel_data$zDevA[accel_data$freqFoam=="no tone.none"],
       x=jitter(rep(1.25, sum(accel_data$freqFoam=="no tone.none")), amount=jitt.amount), pch=16, col="grey90", cex=0.5)


points(y=accel_data$zDevA[accel_data$freqFoam=="0-150.none"],
       x=jitter(rep(xpos[1], sum(accel_data$freqFoam=="0-150.none")), amount=jitt.amount), pch=16, col="grey90", cex=0.5)
points(y=accel_data$zDevA[accel_data$freqFoam=="0-150.foam"],
       x=jitter(rep(xpos[2], sum(accel_data$freqFoam=="0-150.foam")), amount=jitt.amount), pch=16, col="grey90", cex=0.5)
points(y=accel_data$zDevA[accel_data$freqFoam=="0-150.hand"],
       x=jitter(rep(xpos[3], sum(accel_data$freqFoam=="0-150.hand")), amount=jitt.amount), pch=16, col="grey90", cex=0.5)


points(y=accel_data$zDevA[accel_data$freqFoam=="150-300.none"],
       x=jitter(rep(xpos[4], sum(accel_data$freqFoam=="150-300.none")), amount=jitt.amount), pch=16, col="grey90", cex=0.5)
points(y=accel_data$zDevA[accel_data$freqFoam=="150-300.foam"],
       x=jitter(rep(xpos[5], sum(accel_data$freqFoam=="150-300.foam")), amount=jitt.amount), pch=16, col="grey90", cex=0.5)
points(y=accel_data$zDevA[accel_data$freqFoam=="150-300.hand"],
       x=jitter(rep(xpos[6], sum(accel_data$freqFoam=="150-300.hand")), amount=jitt.amount), pch=16, col="grey90", cex=0.5)

points(y=accel_data$zDevA[accel_data$freqFoam=="300-450.none"],
       x=jitter(rep(xpos[7], sum(accel_data$freqFoam=="300-450.none")), amount=jitt.amount), pch=16, col="grey90", cex=0.5)
points(y=accel_data$zDevA[accel_data$freqFoam=="300-450.foam"],
       x=jitter(rep(xpos[8], sum(accel_data$freqFoam=="300-450.foam")), amount=jitt.amount), pch=16, col="grey90", cex=0.5)
points(y=accel_data$zDevA[accel_data$freqFoam=="300-450.hand"],
       x=jitter(rep(xpos[9], sum(accel_data$freqFoam=="300-450.hand")), amount=jitt.amount), pch=16, col="grey90", cex=0.5)

segments(x0=xpos, x1=xpos,
         y0=testPreds[,3]^2, y1=testPreds[,4]^2)
points(testPreds[,1]^2 ~ xpos, pch=16, cex=1.5)

axis(side=1, at=1.25, labels="Control", mgp=c(3,0.1,0))
axis(side=1, at=xpos[c(1,4,7)], labels=rep("-",3), mgp=c(3,0.1,0))
axis(side=1, at=xpos[c(2,5,8)], labels=rep("F",3), mgp=c(3,0.1,0))
axis(side=1, at=xpos[c(3,6,9)], labels=rep("H",3), mgp=c(3,0.1,0))
mtext(side=1, at=c(mean(xpos[1:3]), mean(xpos[4:6]), mean(xpos[7:9])), text=c("0-150Hz", "150-300Hz", "300-450Hz"),
     line=1)
dev.off()

# positive control comparison plot ####

# add in jump data
jumpDev <- read.csv("./data/positive control_jumping.txt", sep=";")
jumpDev$z <- jumpDev$z * 9.8
jumpDev$zDevA <- abs(jumpDev$z - phoneZero)
jumpDev <- jumpDev[100:150,]

pdf("./plots/accel_150_jump.pdf", height=3, width=2.5, useDingbats = FALSE)
par(mar=c(2.5,3.5,0.5,0.5), ps=10, tcl=-0.25, mgp=c(3,0.5,0), las=1)
plot(x=NULL, y=NULL, ylim=c(0,2), xlim=c(0.65,1.65), type="n",
     xlab="", ylab="", xaxt="n")
mtext(side=2, line=2.25, las=0, text=expression("Absolute delta acceleration (ms"^2*")"))

xpos<-c(0.8,1,1.2, 1.5)

# control
jitt.amount <- 0.075

points(y=accel_data$zDevA[accel_data$freqFoam=="0-150.none"],
       x=jitter(rep(xpos[1], sum(accel_data$freqFoam=="0-150.none")), amount=jitt.amount), pch=16, col="grey90", cex=0.5)
points(y=accel_data$zDevA[accel_data$freqFoam=="0-150.foam"],
       x=jitter(rep(xpos[2], sum(accel_data$freqFoam=="0-150.foam")), amount=jitt.amount), pch=16, col="grey90", cex=0.5)
points(y=accel_data$zDevA[accel_data$freqFoam=="0-150.hand"],
       x=jitter(rep(xpos[3], sum(accel_data$freqFoam=="0-150.hand")), amount=jitt.amount), pch=16, col="grey90", cex=0.5)

points(y=jumpDev$zDevA,
       x=jitter(rep(xpos[4], length(jumpDev$zDevA)), amount=jitt.amount), pch=16, col="grey90", cex=0.5)

segments(x0=xpos[1:3], x1=xpos[1:3],
         y0=testPreds[c(1:3),3]^2, y1=testPreds[c(1:3),4]^2)
points(testPreds[c(1:3),1]^2 ~ xpos[c(1:3)], pch=16, cex=1.5)

jumpLm <- lm(sqrt(zDevA) ~ 1, data=jumpDev)
points(y=summary(jumpLm)$coefficients[1]^2, x=xpos[4], pch=16, cex=1.5)
segments(x0=xpos[4], x1=xpos[4],
         y0=(summary(jumpLm)$coefficients[1] - 1.96 * summary(jumpLm)$coefficients[2])^2, 
         y1=(summary(jumpLm)$coefficients[1] + 1.96 * summary(jumpLm)$coefficients[2])^2)

axis(side=1, at=1.5, labels="Jump", mgp=c(3,0.1,0))
axis(side=1, at=xpos[1], labels="-", mgp=c(3,0.1,0))
axis(side=1, at=xpos[2], labels="F", mgp=c(3,0.1,0))
axis(side=1, at=xpos[3], labels="H", mgp=c(3,0.1,0))
mtext(side=1, at=c(mean(xpos[1:3])), text=c("0-150Hz"),
      line=1)
dev.off()
