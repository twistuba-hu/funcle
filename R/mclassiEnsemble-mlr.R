#' mclassiEnsemble
#' @importFrom mlr makeLearner
#' @importFrom mlr predictLearner
#' @importFrom mlr trainLearner
#' @importFrom mlr makeClassifTask
#' @importFrom mlr tuneParams


mclassiStackLearner <- function(
    task,
    cl,
    id,
    knn,
    kernel = NULL,
    par.vals,
    subset = NULL) {
  if (is.null(task)) {
    stop("Error: task is NULL")
  }
  if (length(id) == 1) {
    stop("Error: id should be greater than or equal to 2")
  }
  learner <- list()
  for (j in 1:length(id)) {
    # Set default predict.type if not provided
    predict_type <- if (is.null(par.vals[[j]]$predict.type)) "prob" else par.vals[[j]]$predict.type


    if ("measure" %in% names(par.vals[[j]])) {
      checkmate::assertChoice(par.vals[[j]]$measure, choices = mmeasuresChoices())
      par.vals[[j]]$metric <- NULL # "Euclidean"
    } else {
      checkmate::assertCharacter(par.vals[[j]]$metric)
      checkmate::assertChoice(par.vals[[j]]$metric, choices = mmetricChoices())
      par.vals[[j]]$measure <- NULL
    }
    if (cl == "classif.mclassiKnn") {
      if (is.null(subset)) {
        new_par.vals <- list(metric = par.vals[[j]]$metric, mdist = par.vals[[j]]$mdist, knn = knn[j], measure = par.vals[[j]]$measure)
      } else {
        new_par.vals <- list(metric = par.vals[[j]]$metric, mdist = par.vals[[j]]$mdist[((1:task$task.desc$size) %in% subset), ((1:task$task.desc$size) %in% subset)], knn = knn[j], measure = par.vals[[j]]$measure)
      }
    } else {
      new_par.vals <- list(metric = par.vals[[j]]$metric, measure = par.vals[[j]]$measure, kernel = kernel, h = par.vals[[j]]$h)
    }

    learner[[j]] <- makeLearner(cl = cl, id = id[j], predict.type = predict_type, par.vals = new_par.vals)
  }
  class(learner) <- c("StackLearner")
  learner
}

mclassiStackTrain <- function(learner,
                              task,
                              subset,
                              outer = FALSE) {
  model <- list()
  if (outer == TRUE) { # is this needed, i think both variants are the same.
    for (j in 1:length(learner)) {
      model[[j]] <- train(learner = learner[[j]], task = task, subset = subset)
    }
  } else {
    subtaskTrain <- subsetTask(task, subset = c(1:task$task.desc$size)[(c(1:task$task.desc$size) %in% subset)])
    for (j in 1:length(learner)) {
      model[[j]] <- train(learner = learner[[j]], task = subtaskTrain)
    }
  }

  class(model) <- c("StackTrain")
  return(model)
}

mclassiStackTest <- function(learner, model, task) {
  if (class(model) != "StackTrain") {
    stop("Error: model object should be of class StackTrain")
  }
  pred <- list()
  for (j in 1:length(model)) {
    pred[[j]] <- predictLearner(.learner = learner[[j]], .model = model[[j]], .newdata = task$env$data[-ncol(task$env$data)])
  }
  class(pred) <- c("StackTest")
  predRes <- list()
  predRes[[1]] <- pred
  predRes[[2]] <- task$env$data[ncol(task$env$data)]
  names(predRes) <- c("probabilities", "condition")
  class(predRes) <- "StackTest"
  return(predRes)
}

mclassiStack <- function(model, pred, super.learner = "randomForest", par.super.learner) {
  if (class(model) != "StackTrain") {
    stop("Error: model object should be of class StackTrain")
  }
  if (class(pred) != "StackTest") {
    stop("Error: model object should be of class StackTest")
  }
  checkmate::assertChoice(super.learner, choices = c("randomForest", "boosting", "nnet"))

  leveloneData <- cbind(pred[[2]], sapply(pred[[1]], function(x) as.numeric(as.character(x))))

  colnames(leveloneData)[1] <- "response"
  colnames(leveloneData)[2:ncol(leveloneData)] <- paste("exp", 2:ncol(leveloneData) - 1, sep = "")
  # data_size <- nrow(leveloneData) # TODO: needed ?
  # if (data_size == 0) {
  #   return(NULL)
  # }
  if (super.learner == "randomForest") {
    learner.aux <- makeLearner(
      cl = "classif.randomForest",
      predict.type = "prob"
    )

    ps.aux <- makeParamSet(
      makeDiscreteParam("ntree", values = par.super.learner[1]),
      makeDiscreteParam("mtry", values = par.super.learner[2])
    )
    control.aux <- makeTuneControlGrid()
  } else if (super.learner == "boosting") {
    learner.aux <- makeLearner(
      cl = "classif.gbm",
      distribution = "bernoulli",
      predict.type = "prob",
    )

    ps.aux <- makeParamSet(
      makeDiscreteParam("n.trees", values = par.super.learner[1]),
      makeDiscreteParam("interaction.depth", values = par.super.learner[2]),
      makeDiscreteParam("shrinkage", values = par.super.learner[3])
    )
    control.aux <- makeTuneControlRandom(maxit = 1)
  } else {
    learner.aux <- makeLearner(
      cl = "classif.nnet",
      predict.type = "prob",
      skip = TRUE
    )
    ps.aux <- makeParamSet(
      makeDiscreteParam("size", values = par.super.learner[1]),
      makeDiscreteParam("maxit", values = par.super.learner[2]),
      makeDiscreteParam("decay", values = par.super.learner[3])
    )
    control.aux <- makeTuneControlRandom(maxit = 1)
  }
  task.aux <- makeClassifTask(data = leveloneData, target = "response")
  cv.aux <- makeResampleDesc("CV", iters = 10)
  par.aux <- tuneParams(learner.aux,
    task = task.aux, resampling = cv.aux, par.set = ps.aux,
    control = control.aux, measure = acc
  )
  return(par.aux)
}
