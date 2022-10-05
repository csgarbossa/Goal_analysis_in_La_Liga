dat <- shoot_dat
rm(shoot_dat)
rm(dataset_shoot)
gc()

library(dplyr)

colnames(dat)
# qualitative and quantitative esplanatory variables ----------------------

expl <- dat %>% dplyr::select(- yn, - y)
cl <- sapply(expl, class)
cl
var.quant <- which(cl == "integer" | cl == "numeric")
var.quali <- which(cl == "factor")

names(var.quant)
names(var.quali)

# model matrix ------------------------------------------------------------

set.seed(1234)
K <- 5
myfolds <- sample(1:K, NROW(dat), replace = T)
xmatr <- model.matrix(~ ., data = (dat %>% dplyr::select(- yn, - y)))[, - 1]

# correlation -------------------------------------------------------------

library(DataExplorer)
plot_correlation(dat, type = "continuous")

# classification threshold ------------------------------------------------

prop.table(table(dat$yn))
s <- 0.15

# formula -----------------------------------------------------------------

expl.sum <- paste0(names(dat %>% dplyr::select(- y, - yn)), collapse = "+") 
f.bin <- formula(paste0("y ~ ", expl.sum))
f.num <- formula(paste0("yn ~ ", expl.sum))

# metrics -----------------------------------------------------------------

lift.roc<- function(predicted, g, type = "bin", plot.it = TRUE)
{
  library(sm)
  if(!is.numeric(g)) stop("g not numeric")
  ind <- rev(order(predicted))
  n <- length(g)
  x1 <-  (1:n)/n
  x2 <- cumsum(g[ind])/(mean(g)*(1:n))
  if(type=="crude" & plot.it) 
    plot(x1, x2, type="l", col=2,
         xlab="Proportion of sample", ylab="Lift")
  if(type=="sm") {
    a<- sm.regression(x1, x2, h=0.1, display="none")
    if(plot.it)
      plot(a$eval, a$etrainte, type="l",xlim=c(0,1), col=2,
           xlab="Proportion of sample", ylab="Lift")
  }
  if(type=="bin") {
    b <-  binning(x1,x2, breaks=(-0.001:10)/9.999)
    x <- c(0,seq(0.05,0.95, by=0.1),1)
    if(plot.it) plot(x, c(x2[1],b$means,1), type="b", xlim=c(0,1),
                     ylim=c(1,max(x2)), cex=0.75, col=2,
                     xlab="Proportion of sample",
                     ylab="Lift")
    x1<- x
    x2<- c(x2[1],b$means,1)
  }
  #if(plot.it) {cat("premere <cr>"); readline()}
  u1<- cumsum(1-g[ind])/sum(1-g)
  u2<- cumsum(g[ind])/sum(g)
  if(type=="crude" & plot.it)
    plot(u1, u2, type="l", xlim=c(0,1), ylim=c(0,1), col=2,
         xlab="1-specificity", ylab="sensitivity")
  if(type=="sm") {
    # browser()
    eps<- 0.00001
    a<- sm.regression(u1,log((u2+eps)/(1-u2+2*eps)), h=0.1, display="none")
    q<- exp(a$etrainte)/(1+exp(a$etrainte))
    if(plot.it) plot(a$eval, q, type="l", xlim=c(0,1), ylim=c(0,1),
                     xlab="1-specificity", ylab="sensitivity", col=2)
  }
  if(type=="bin") {
    b <- binning(u1,u2, breaks=(-0.001:10)/9.999)
    x <- c(0,seq(0.05,0.95, by=0.1),1)
    y<- c(0,b$means,1)
    if(plot.it)
      plot(x, y, type="b", xlim=c(0,1),
           ylim=c(0,1),cex=0.75, xlab="1 - specificity",
           ylab="sensitivity", col=2)
    u1<- x
    u2<- y
  }                      
  if(plot.it) {
    abline(0,1, lty=2, col=3)
  }
  invisible(list(x1,x2,u1,u2))
}

# confusion matrix and other values based on that

tab.summ <- function(predicted, observed){
  n <-  table(predicted, observed)
  err.tot <- 1-sum(diag(n))/sum(n)
  fn <- n[1,2]/(n[1,2]+n[2,2])
  fp <- n[2,1]/(n[1,1]+n[2,1])
  print(n)
  cat("Total error: ", format(err.tot),"\n")
  cat("False positive and false negative rate: ",format(c(fp, fn)),"\n")
  invisible(n)
}

eval.class <- function(t) {
  return(0.8*(t[2,2]/(t[2,2] + t[1,2])) + (0.2)*(t[1,1]/ (t[1,1]+t[2,1])))
}


# linear regression -------------------------------------------------------

err.lin <- numeric(K) 

for(i in 1:K) {
  cat("\n", "FOld", i, "/", K, "\n")
  m.lin <- lm(f.num, data = dat[myfolds != i, ])
  summary(m.lin)
  
  p.lin <- predict(m.lin, newdata = dat[myfolds == i, ])
  p.lin.class <- ifelse(p.lin > s, TRUE, FALSE)
  
  lift.roc(p.lin, dat[myfolds == i, ]$yn)
  t <- tab.summ(p.lin.class, dat[myfolds == i, ]$y)
  err.lin[i] <- eval.class(t)
}

evaluation <- data.frame(Linear = c(mean(err.lin), sd(err.lin)))
evaluation
rownames(evaluation) <- c("Balanced accuracy", "sd")

# stepwise linear regression ----------------------------------------------

err.step <- numeric(K)

for(i in 1:K) {
  cat("\n", "Fold", i, "\n")
  m0 <- lm(yn ~ 1, data = dat[myfolds != i, ])
  m.step <- step(m0, scope = formula(m.lin), direction = "both")
  
  p.step <- predict(m.step, newdata = dat[myfolds == i, ])
  p.step.class <- ifelse(p.step > s, TRUE, FALSE)
  
  lift.roc(p.step, dat[myfolds == i, ]$yn)
  t <- tab.summ(p.step.class, dat[myfolds == i, ]$y)
  err.step[i] <- eval.class(t)
  print(formula(m.step))
}

evaluation$Stepwise_linear <- c(mean(err.step), sd(err.step))
t(evaluation)


# logistic regression -----------------------------------------------------

err.log <- numeric(K)

for(i in 1:K) {
  cat("\n", "FOld", i, "\n")
  m.log <- glm(f.bin, data = dat[myfolds != i, ], family = "binomial")

  p.log <- predict(m.log, type = "response", newdata = dat[myfolds == i, ])
  p.log.class <- ifelse(p.log > s, TRUE, FALSE)
  
  lift.roc(p.log, dat[myfolds == i, ]$yn)
  t <- tab.summ(p.log.class, dat[myfolds == i, ]$y)
  err.log[i] <- eval.class(t)
}

evaluation$Logistic <- c(mean(err.log), sd(err.log))
t(evaluation)


# lasso -------------------------------------------------------------------

library(glmnet)
set.seed(39)

err.lass <- numeric(K)

for(i in 1:K) {
  m.lass <- cv.glmnet(xmatr[myfolds != i, ], dat[myfolds != i, ]$yn, alpha = 1)
  plot(m.lass)
  plot(m.lass$glmnet.fit, xvar = "lambda")
  abline(v = log(m.lass$lambda.1se), col = 1, lwd = 2, lty = 2)
  abline(v = log(m.lass$lambda.min), col = 1, lwd = 2, lty = 2)
  
  p.lasso <- predict(m.lass, newx = xmatr[myfolds == i, ], s = "lambda.min")
  p.lasso.class <- ifelse(p.lasso > s, TRUE, FALSE)

  t <- tab.summ(p.lasso.class, dat[myfolds == i, ]$y)
  lift.roc(p.lasso, dat[myfolds == i, ]$yn)
  err.lass[i] <- eval.class(t)
}

evaluation$Lasso <- c(mean(err.lass), sd(err.lass))
t(evaluation)


# discriminant analysis ---------------------------------------------------

library(MASS)

err.lda <- numeric(K)

for(i in 1:K) {
  cat("\n", "FOld", i, "\n")
  m.lda <- lda(f.bin, data = dat[myfolds != i, ])
  
  p.lda <- predict(m.lda, dat[myfolds == i, ])$posterior[, 2]
  p.lda.class <- ifelse(p.lda > s, TRUE, FALSE)
  
  lift.roc(p.lda, dat[myfolds == i, ]$yn)
  t <- tab.summ(p.lda.class, dat[myfolds == i, ]$y)
  err.lda[i] <- eval.class(t)
}

evaluation$LDA <- c(mean(err.lda), sd(err.lda))
t(evaluation)


# gam ---------------------------------------------------------------------

library(gam)

quant <- paste0(sprintf("s(%s)", setdiff(names(dat[myfolds != i, ]  %>% dplyr::select(- y, -yn)),
                                         names(var.quali))), collapse = "+")
quali <- paste0(names(var.quali), collapse = "+")
f <- paste0("y ~ ", quali, "+", quant) 
f

err.gam <- numeric(K)

for(i in 1:K) {
  cat("\n", "FOld", i, "\n")
  m.gam <- gam(formula(f), data = dat[myfolds != i, ], family = binomial)
  
  p.gam <- predict(m.gam, newdata = dat[myfolds == i, ], type = "response")
  p.gam.class <- ifelse(p.gam > s, TRUE, FALSE)
  
  lift.roc(p.gam, dat[myfolds == i, ]$yn)
  t <- tab.summ(p.gam.class, dat[myfolds == i, ]$y)
  err.gam[i] <- eval.class(t)
}

evaluation$GAM <- c(mean(err.gam), sd(err.gam))
t(evaluation)


# mars --------------------------------------------------------------------

library(earth)

err.mars <- numeric(K)

for(i in 1:K) {
  cat("\n", "FOld", i, "\n")
  m.mars <- earth(f.bin, data = dat[myfolds != i, ], glm = list(family = binomial)) # degree = 2 to consider the interactions
  plotmo(m.mars)
  
  p.mars <- predict(m.mars, newdata = dat[myfolds == i, ], type = "response")
  p.mars.class <- ifelse(p.mars > s, TRUE, FALSE)

  lift.roc(p.mars, dat[myfolds == i, ]$yn)
  t <- tab.summ(p.mars.class, dat[myfolds == i, ]$y)
  err.mars[i] <- eval.class(t)
}

evaluation$Mars <- c(mean(err.mars), sd(err.mars))
t(evaluation)


# albero ------------------------------------------------------------------

library(rpart)
set.seed(94)

err.tree <- numeric(K)

for(i in 1:K) {
  cat("\n", 'Fold ',i,'/',K,'\n')
  tree2 <- rpart(formula(m.lin), data = dat[myfolds != i, ],
                 control = rpart.control(xval = 5, minbucket = 2, cp = 0.0001))
  cp_ottim <- tree2$cptable[which.min(tree2$cptable[,"xerror"]),"CP"]
  rsq.rpart(tree2)
  j <- tree2$cptable[which.min(tree2$cptable[,"xerror"]), "nsplit"]
  j # number of splits
  abline(v = j, lty = 2, col = 2)
  
  fit.tree <- prune(tree2, cp = cp_ottim)
  fit.tree
  print(fit.tree$variable.importance)
  
  p.tree2 <- predict(fit.tree, newdata = dat[myfolds == i, ])
  p.tree2.class <- ifelse(p.tree2 > s, TRUE, FALSE)
  
  lift.roc(p.tree2, dat[myfolds == i, ]$yn)
  t <- tab.summ(p.tree2.class, dat[myfolds == i, ]$y)
  err.tree[i] <- eval.class(t)
}

evaluation$Tree <- c(mean(err.tree), sd(err.tree))
t(evaluation)

# random forest -----------------------------------------------------------

library(randomForest)
library(ranger)

for(i in 1:K){
  ranfo.try <- randomForest(y ~ .,
                            data = dat[myfolds != i, ],
                            xtest = dat[myfolds == i, ] %>% dplyr::select(- y),
                            ytest = dat[myfolds == i, ]$y,
                            ntree = 100)
  plot(ranfo.try)
}

set.seed(394)

cols <- 1:(ncol(dat[myfolds != i, ]) - 2) # without y and yn
err.ran <- matrix(NA, NROW(cols), ncol = K)
err <- numeric(NROW(cols))

for(i in 1:K){
  cat("\n", "Fold", i, "\n")
  for(j in 1:length(cols)) {
    m.ran <- ranger(yn ~ ., data = dat[myfolds != i, ] %>% dplyr::select(- y), 
                    importance = "impurity",
                    num.trees = 100, mtry = cols[i],
                    max.depth = nrow(dat[myfolds != i, ]))

    p.ran <- predict(m.ran, data = dat[myfolds == i, ] %>% dplyr::select(- y))  
    p.ran.class <- ifelse(p.ran$predictions > s, TRUE, FALSE)
    
    lift.roc(p.ran$predictions, dat[myfolds == i, ]$yn)
    t <- tab.summ(p.ran.class, dat[myfolds == i, ]$y)
    err.ran[j, i] <- c(eval.class(t))
  }
}

err <- apply(err.ran, 1, mean)
j <- which.min(err)
j

err.fore <- numeric(K)

for(i in 1:K){
  cat("\n", "Fold", i, "\n")
  m.ran <- ranger(yn ~ ., data = dat[myfolds != i, ] %>% dplyr::select(- y), 
                  importance = "impurity",
                  num.trees = 100, mtry = j,
                  max.depth = nrow(dat[myfolds != i, ]))
  
  p.ran <- predict(m.ran, data = dat[myfolds == i, ] %>% dplyr::select(- y))  
  p.ran.class <- ifelse(p.ran$predictions > s, TRUE, FALSE)
  
  lift.roc(p.ran$predictions, dat[myfolds == i, ]$yn)
  t <- tab.summ(p.ran.class, dat[myfolds == i, ]$y)
  err.fore[i] <- c(eval.class(t))
}


evaluation$Random_forest <- c(mean(err.fore), sd(err.fore))
t(evaluation)


# projection pursuit -----------------------------------------------------------

set.seed(39)

nterms <- 1:15
y.position <- which(colnames(dat[myfolds != i, ]) == "y")
err <- matrix(NA, NROW(nterms), 2)
er <- matrix(NA, nrow = NROW(nterms), K)


for(i in seq_along(nterms)){
  cat(i, "/", NROW(nterms), "...\n")
  for(j in 1:K) {
    m <- ppr(yn ~ ., data = dat[myfolds != j, - y.position], nterms = nterms[i])
    
    p <- predict(m, newdata = dat[myfolds == j, - y.position])
    p.class <- ifelse(p > s, TRUE, FALSE)
    
    lift.roc(p, dat[myfolds == j, ]$yn)
    t <- tab.summ(p.class, dat[myfolds == j, ]$y)
    er[i, j] <- eval.class(t)
  }
  e <- mean(er[i, ])
  err[i,] <- c(nterms[i], e)
}

rotat <- which.min(err[, 2])
plot(err, type = "b")
abline(v = rotat, lty = 2, col = 2)

evaluation$PPR <- c(err [rotat, 2], sd(er[rotat, ]))
t(evaluation)


# results -----------------------------------------------------------------

e <- data.frame(t(evaluation))
knitr::kable(arrange(e, - Balanced.accuracy), digits = 2)


# best model --------------------------------------------------------------


m0 <- lm(yn ~ 1, data = dat)
m.step <- step(m0, scope = formula(m.lin), direction = "both")
coef(m.step)
  
library(ggplot2)
dati_step = data.frame(coef(m.step)[-1], confint(m.step)[-1,]) # tolgo intercetta
colnames(dati_step) = c("stima", "lower", "upper")
dati_step$segno = ifelse(dati_step$stima > 0, "1", "-1")
dim(dati_step)
dati_step$variabile = c("Distance", "Second period", "Degree", "Home")
dati_step = arrange(dati_step, stima)
dati_step$variabile = factor(dati_step$variabile, levels = unique(dati_step$variabile))
ggplot(dati_step) +
  geom_bar(aes(x = variabile, y = stima), stat = "identity", alpha = 0.7, 
           color = "#8B0D11", fill = "#FCB507") +
  geom_hline(yintercept = 0) +
  labs(x = "", y = "") +
  coord_flip() +
  ylim(-0.03, 0.15) +
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text = element_text(size = 20),
    legend.position = "none",
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change axis line
    axis.line = element_line(colour = "black"),
    # Change plot background
    panel.background = element_rect(fill = "white"))


