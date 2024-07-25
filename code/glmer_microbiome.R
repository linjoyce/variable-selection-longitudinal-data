setwd("/Users/joycelin/Documents/UW/Course/STAT571/Project/Code")
source("proc.R")
library(lme4)
library(glmmLasso)
library(optimx)

# combined_by_class, combined_by_order, combined_by_family
data = combined_by_genus
data[, 3:ncol(data)] = apply(data[, 3:ncol(data)], 2, scale)
data$patientID = factor(data$patientID)

# naive method -- p.adjust
alpha = 0.05

form = as.formula(paste("label ~", paste(colnames(data)[3:ncol(data)], collapse = " + "),
                        "+ (1|patientID)", sep = " "))
mod.glmm = glmer(form, family = binomial, data = data,
                 control = glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')))

mod.glmm.smry = summary(mod.glmm)
pvalues.adj = p.adjust(mod.glmm.smry$coefficients[,4], method = "holm")
bestModel_holm = which(pvalues.adj[-1] < alpha)
as.vector(bestModel_holm)
paste(names(bestModel_holm), collapse = ", ")

pvalues.adj = p.adjust(mod.glmm.smry$coefficients[,4], method = "fdr")
bestModel_fdr = which(pvalues.adj[-1] < alpha)
as.vector(bestModel_fdr)
paste(names(bestModel_fdr), collapse = ", ")

step.glmm = lmerTest::step(mod.glmm)


# glmmLasso
lambda = seq(500, 10, by = -5)
BIC_vec = unlist(lapply(lambda, function(l) {
  print(paste("Iteration ", which(lambda == l), sep = ""))
  form <- as.formula(paste("label ~", paste(colnames(data)[3:ncol(data)], collapse = " + ")))
  mod.glmm <- glmmLasso(form, rnd = list(patientID = ~1), family = binomial(link = "logit"),
                        lambda = l, data = data)
  mod.glmm$bic
}))
lambda[which.min(BIC_vec)]

# family: 75; order: 15; class: 
options(expressions = 50000)
form = as.formula(paste("label ~", paste(colnames(data)[3:ncol(data)], collapse = " + ")))
mod.glmm = glmmLasso(form, rnd = list(patientID = ~1), family = binomial(link = "logit"),
                     lambda = lambda[which.min(BIC_vec)], data = data)
bestModel_glmmLasso = which(summary(mod.glmm)$coefficient[-1,1] != 0)
as.vector(bestModel_glmmLasso)
paste(names(bestModel_glmmLasso), collapse = ", ")


# AIC forward selection
getAIC=function(explanatory,dataset)
{
  if(length(explanatory) == 0){
    model = glmer(label ~ 1 + (1|patientID), family = binomial, data = dataset,
                  control = glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')))
    
    output = AIC(model)
  }
  else{
    form = as.formula(paste("label ~ ",
                            paste(colnames(dataset)[as.numeric(explanatory)], collapse = " + "),
                            "+ (1|patientID)", sep = " "))
    model = glmer(form,
                  family = binomial, data = dataset,
                  control = glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')))
    output = AIC(model)
  }
  return(output)
}

# GLMM forward search based on AIC
glmmforwardSearch = function(lastPredictor,dataset)
{
  bestModel = NULL #index of best model
  bestModelAIC = getAIC(bestModel,dataset) # AIC of best model
  cat('\nforwardSearch: The empty GLMM has AIC=', bestModelAIC, '\n');
  
  VariablesNotInModel = 3:lastPredictor
  
  stepNumber = 0;
  while(length(VariablesNotInModel) >= 1)
  {
    stepNumber = stepNumber + 1
    
    regAIC = vector("numeric",length(VariablesNotInModel))
    n=length(VariablesNotInModel)
    
    for (i in 1:n){
      regAIC[i] = getAIC(union(bestModel, VariablesNotInModel[i]), dataset)
    }
    
    minAIC=min(regAIC)
    min_addVariable=VariablesNotInModel[which.min(regAIC)]
    
    if (minAIC < bestModelAIC){
      bestModelAIC = minAIC
      bestModel = union(bestModel, min_addVariable)
      VariablesNotInModel = setdiff(VariablesNotInModel,min_addVariable)
      cat('\nforwardSearch: current step with number of features:',length(bestModel),'\n');
      cat('\nforwardSearch: added feature:',min_addVariable,'\n');
    }
    else{
      break
    }
    
  }
  
  return(bestModel)
}

bestModel_glmmAIC = glmmforwardSearch(ncol(data), data)
bestModel_glmmAIC
paste(colnames(data)[bestModel_glmmAIC], collapse = ", ")


# bGLMM
setwd("/Users/joycelin/Documents/UW/Course/STAT571/Project/Code")
source("proc.R")

data = combined_by_genus
data[, 3:ncol(data)] = apply(data[, 3:ncol(data)], 2, scale)
data$patientID = factor(data$patientID)

library(GMMBoost)
options(expressions = 50000)
form = as.formula(paste("label ~", paste(colnames(data)[3:ncol(data)], collapse = " + ")))
mod.bglmm = bGLMM(form, rnd = list(patientID = ~1), family = binomial(link = "logit"),
                 data = data, control = bGLMMControl(steps = 100))
bestModel_bGLMM = which(summary(mod.bglmm)$coefficient[-1,1] != 0)
as.vector(bestModel_bGLMM)
paste(names(bestModel_bGLMM), collapse = ", ")

