# script with all the content from the EDA
# Importing libraries
library(dplyr)
library(ggplot2)
library(stringr)
library(gridExtra)
library(outliers)
library(PerformanceAnalytics)
library(foreach)
library(MASS)
library(e1071) 
library(VGAM)
library(caret)
library(klaR)
library(arm)
library(caTools)
library(stepPlr)
library(LiblineaR)

# turn off warnings
options(warn=-1)


# importing the data
df <- read.csv('./data/data.csv')

# function to plot
plots <- function(dataset, col, fw=FALSE, hist='default',
                  density='default' , bins='default',
                  xtick_angles='default', sep=FALSE, savefig='default', filename='./plot.png') {
    var <- dataset %>% dplyr::select(col)
    if (bins == 'default') {bins <- rep(10,2)}
    if (xtick_angles == 'default') {xtick_angles <- rep(90,2)}
    if (hist == 'default') {hist <- c(FALSE,FALSE)}
    if (density == 'default') {density <- c(TRUE,TRUE)}
    if (savefig == 'default') {savefig <- c(FALSE,14,14)}

    p1 <- dataset %>% ggplot(aes(x=var[,1])) +
        geom_boxplot() +
        ggtitle(str_interp("${col}")) +
        theme(axis.title.x=element_blank(),axis.text.y=element_blank())
    p2 <- dataset %>% ggplot(aes(x=var[,1], fill=hdi_cat)) +
        geom_boxplot() +
        ggtitle(str_interp("${col} grouped by HDI")) +
        theme(axis.title.x=element_blank(),axis.text.y=element_blank())
    p3 <- dataset %>% ggplot(aes(x=var[,1])) +
        ggtitle(str_interp("${col}")) +
        theme(axis.title.x=element_blank(),
                axis.text.x = element_text(angle = xtick_angles[1]))
    p4 <- dataset %>% ggplot(aes(x=var[,1])) +
        ggtitle(str_interp("${col} by HDI group")) +
        theme(axis.title.x=element_blank(),
                axis.text.x = element_text(angle = xtick_angles[2]))
    if (hist[1] == TRUE) {
        p3 <- p3 + geom_histogram(aes(y=..density..),bins=bins[1])}
    if (hist[2] == TRUE) {
        p4 <- p4 + geom_histogram(show.legend = FALSE,bins=bins[2],
                                  aes(fill=hdi_cat,y=..density..))}
    if (density[1] == TRUE) {
        p3 <- p3 + geom_density()}
    if (density[2] == TRUE) {
        p4 <- p4 + geom_density(aes(group=hdi_cat,colour=hdi_cat,fill=hdi_cat))}
    if (fw == TRUE) {p4 <- p4 + facet_wrap(~hdi_cat, nrow = 1)}
    if (sep == TRUE) {
        grid.arrange(p1,p2, nrow=2)
        grid.arrange(p3,p4, nrow=2)}
    else {grid.arrange(p1,p2,p3,p4, nrow=4)}
    if (savefig[1] == TRUE) {ggsave(file=filename, width=savefig[2], height=savefig[3],
                                 arrangeGrob(p1,p2,p3,p4, nrow=4))}
}

# Helper function to colour num. variables by cat. variables
colors <- function(cat_var, colors_vector) {
    kleuren <- as.numeric(as.factor(cat_var))
    foreach (i=1:length(kleuren), kleur=kleuren) %do% {
        kleuren[i] = colors_vector[kleur]
    }
    return(kleuren)
}

# setting colnames for variables to use in the analysis
cols = names(df)
cols = cols[6:(length(cols)-2)]

# Selecting colours per HDI
color_1 <- "blueviolet"
color_2 <- "red"
color_3 <- "black"
color_4 <- "green"
palette <- c(color_1,color_2,color_3,color_4)
hdi_colours <- colors(df$hdi_cat,palette)

# foreign investment inflows
col = 'foreign_inv_inflows'
plots(dataset=df, col=col, hist=c(TRUE,TRUE), density=c(FALSE,FALSE), xtick_angles=c(50,50) ,bins=c(30,30), fw=TRUE, sep=FALSE, savefig=c(TRUE,12,12), filename=str_interp('./img/${col}.png'))

# education years
col = 'education_years'
plots(dataset=df, col=col, hist=c(TRUE,FALSE), density=c(TRUE,TRUE), xtick_angles=c(50,50) ,bins=c(20,20), fw=TRUE, sep=FALSE, savefig=c(TRUE,12,12), filename=str_interp('./img/${col}.png'))

# access to electricity
col = 'access_to_electricity'
plots(dataset=df, col=col, hist=c(TRUE,TRUE), density=c(FALSE,FALSE), xtick_angles=c(50,50) ,bins=c(30,30), fw=TRUE, sep=FALSE, savefig=c(TRUE,12,12), filename=str_interp('./img/${col}.png'))

# construction of correlation matrix
methods = c('kendall','spearman','pearson')
corr_mat = matrix(rep(0,(length(cols)^2)*4), nrow=length(cols)^2)
corr_mat = corr_mat %>% data.frame() %>% setNames(c('var1','var2','coef','corr_type'))
cnt = 0
for (i in 1:length(cols)) {
    for (j in 1:length(cols)) {
        cnt = cnt + 1
        comb1 <- df %>% dplyr::select(cols[i])
        comb2 <- df %>% dplyr::select(cols[j])
        maximum_cor = 0
        method_used = ''
        for (method in methods) {
            correl <- cor(comb1[,1],comb2[,1], method=method)
            if (abs(correl) > abs(maximum_cor)) {
                maximum_cor <- correl
                method_used = method
            }
        }
        corr_mat$coef[cnt] = maximum_cor
        corr_mat$var1[cnt] = cols[i]
        corr_mat$var2[cnt] = cols[j]
        corr_mat$corr_type[cnt] = method_used
    }
}

# plot of the correlation matrix
corr_mat %>% ggplot(aes(var1, var2, fill=coef)) +
                 geom_tile() +
                 geom_text(aes(label=round(coef,2))) +
                 scale_fill_gradient(low="red", high="blue", limits=c(-1,1))+
                 theme( axis.text.x = element_text(angle = 70, vjust = 1, size = 12, hjust = 1),
                        axis.title.x = element_blank(),
                        axis.title.y = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.border = element_blank(),
                        panel.background = element_blank(),
                        axis.ticks = element_blank()) +
                ggsave(file='./img/correl_heatmap.png')


# modelling part
df = read.csv('./data/data.csv')

# vars
vars = names(df)
# training vars
train = vars[6:(length(vars)-2)]
train = df %>% dplyr::select(train)

# target var
target = vars[length(vars)]
target = df %>% dplyr::select(target)
target$hdi_cat <- as.factor(target$hdi_cat)

# leaving a dataset with everything included except IDs
data <- cbind(train,target)

control <- trainControl(method = "repeatedcv",
                        repeats = 3,
                        number = 10 )

# models
# LDA
ldafit <- train(hdi_cat ~ ., 
                method = "lda", 
                data = data,
                preProcess = c("center", "scale"),
                metric = "Accuracy",
                trControl = control)

ldapred <- predict(ldafit, data)
ldacm <- confusionMatrix(ldapred, data$hdi_cat)

# LR
lrfit <- train(hdi_cat ~ ., 
               method = "glmnet",
               family = "multinomial",
               data = data,
               metric = "Accuracy",
               preProcess = c("center","scale"),
               tuneGrid = expand.grid(alpha = seq(0, 2, 0.1), 
                                      lambda = seq(0, .1, 0.01)),
               trControl = control)

lrpred <- predict(lrfit, data)
lrcm <- confusionMatrix(lrpred, data$hdi_cat)

# QDA
qdafit <- train(hdi_cat ~ ., 
                method = "qda", 
                data = data,
                preProcess = c("center", "scale"),
                metric = "Accuracy",
                trControl = control)

qdapred <- predict(qdafit, data)
qdacm <- confusionMatrix(qdapred, data$hdi_cat)

# KNN
knnfit <- train(hdi_cat ~ ., 
                method = "knn", 
                data = data,
                preProcess = c("center", "scale"),
                metric = "Accuracy",
                trControl = control)

knnpred <- predict(knnfit, data)
knncm <- confusionMatrix(knnpred, data$hdi_cat)

# plot for variable importance
ldaimp <- varImp(ldafit, scale = F)
lrimp <- varImp(lrfit, scale = F)
knnimp <- varImp(knnfit, scale = F)
qdaimp <- varImp(qdafit, scale = F)
par(mfrow=c(2,2))
p1 <- ggplot(ldaimp, scales = list(y = list(cex = .95))) + ggtitle("LDA Var. Imp.")
p2 <- ggplot(lrimp, scales = list(y = list(cex = .95))) + ggtitle("LR Var. Imp.")
p3 <- ggplot(knnimp, scales = list(y = list(cex = .95))) + ggtitle("KNN Var. Imp.")
p4 <- ggplot(qdaimp, scales = list(y = list(cex = .95))) + ggtitle("QDA Var. Imp.")
ggsave(file='./img/variable_importance_models.png', width=12, height=14,
                                 arrangeGrob(p1,p2,p3,p4, nrow=2))

# confusion matrices
ldacm
lrcm
qdacm
knncm