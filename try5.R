setwd("~/Desktop/Rpackage")
install.packages("BreedingSchemeLanguage_1.0.tar.gz",repos=NULL,type="source")
library(BreedingSchemeLanguage)
?defineSpecies
?calcGenotypicValue
setwd("~/Desktop/BSLtest")


require(ggplot2)
plotData2 <- function (ymax = NULL, add = F, addDataFileName = "plotData") 
{
    breedingData <- lists[[1]]$breedingData
    popIDselU <- sort(unique(breedingData$popIDsel))
    muSim <- matrix(NA, nSim, length(popIDselU))
    for (sim in 1:nSim) {
        breedingData <- lists[[sim]]$breedingData
        mu <- rep(NA, length(popIDselU))
        for (i in 1:length(popIDselU)) {
            GID.now <- breedingData$GID[breedingData$popIDsel == 
                popIDselU[i]]
            g.now <- NULL
            for (j in GID.now) {
                g.now <- c(g.now, breedingData$gValue[breedingData$GID == 
                  j])
            }
            mu[i] <- mean(g.now)
        }
        muSim[sim, ] <- mu
    }
    muSim <- muSim - muSim[, 1]
    g <- NULL
    group <- NULL
    col <- NULL
    size <- NULL
    for (sim in 1:nSim) {
        g <- c(g, muSim[sim, ])
        group <- c(group, rep(sim, length(popIDselU)))
        size <- c(size, rep(1, length(popIDselU)))
    }
    g <- c(g, apply(muSim, 2, mean))
    group <- c(group, rep(nSim + 1, length(popIDselU)))
    size <- c(size, rep(2, length(popIDselU)))
    data <- data.frame(g = g, popID = rep(0:(length(popIDselU) - 
        1), nSim + 1), size = size, group = group, scheme = rep(1, 
        length(g)))
    if (add) {
        load(file = paste(addDataFileName, ".RData", sep = ""))
        data.previous$scheme <- data.previous$scheme + 1
        data.previous$group <- data.previous$group + max(data$group)
        data <- rbind(data, data.previous)
    }
    data.previous <- data
    save(data.previous, file = paste(addDataFileName, ".RData", 
        sep = ""))
    p <- ggplot(data = data, aes(x = popID, y = g))
    p <- p + geom_line(aes(size = factor(size), 
        linetype = factor(scheme), group = factor(group)))
    if (is.null(ymax)) {
        p <- p + ylim(min(data$g), max(data$g))
    }
    else {
        p <- p + ylim(min(data$g), ymax)
    }
    p <- p + scale_size_manual(name = "", values = c(0.5, 3), labels = c("Each", 
        "Mean"))
    p <- p + labs(title = "", x = "Generation", y = "Genetic improvement")
    p <- p + guides(alpha = guide_legend("Lines"), size = guide_legend("Lines"))
    p <- p + guides(linetype = guide_legend("Scheme"))
    print(p)
}





defineSpecies(load="previousData1")
initializePopulation()
phenotype()
select()
cross()
phenotype()
select()
cross()
phenotype()
select()
cross()
phenotype()
select()
cross()
phenotype()
select()
cross()
plotData2()

defineSpecies(load="previousData1")
initializePopulation()
phenotype()
genotype()
predict()
select()
cross()
phenotype()
genotype()
predict()
select()
cross()
phenotype()
genotype()
predict()
select()
cross()
phenotype()
genotype()
predict()
select()
cross()
phenotype()
genotype()
predict()
select()
cross()
plotData2(add=T)

defineSpecies(load="previousData1")
initializePopulation()
phenotype()
genotype()
predict()
select()
cross()
genotype()
predict()
select()
cross()
genotype()
predict()
select()
cross()
genotype()
predict()
select()
cross()
genotype()
predict()
select()
cross()
plotData2(add=T)









defineSpecies(load = "previousData1")
initializePopulation()
phenotype()
genotype()
predict()
select()
cross()
genotype()
predict()
select()
cross()
genotype()
predict()
select()
cross()
plotData2()

defineSpecies(, saveDataFileName = "previousData2", nSim=5, nCore=2, , propDomi=0.3, nEpiLoci=2)
initializePopulation()
phenotype()
genotype()
predict()
select()
cross()
genotype()
predict()
select()
cross()
genotype()
predict()
select()
cross()
plotData2(add=T)









defineSpecies(load = "previousData1")
initializePopulation()
phenotype()
genotype()
predict()
select()
cross()
genotype()
predict()
select()
cross()
phenotype()
genotype()
predict(trainingPopID = 4)
select()
cross()
genotype()
predict(trainingPopID = c(4, 5))
select()
cross()
plotData2()






######## test #############
