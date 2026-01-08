#' mclassiOnested
#'
#' task: define a task with makeClassifTask
#' data: data set with: id, dimension, trajectories as rows and condition
#' target: condition
#' knn: value for nearest neighbor
#' par.vals: hyperparameters
#' cv: nested loop (model performance) is done given k-fold cross-validation (cv=k)
#' M: number of iterations in the inner loop (subsampling)
#' weight: subsampling is used to tuned parameters. "weight" is a vector with weights for training and testing data sets
#' @export
mclassiOnested <- function(cl, # TODO: add kernels and super learner selection
                           task,
                           knn = NULL,
                           kernel = NULL,
                           id,
                           par.vals,
                           hyperparams.vals = NULL,
                           super.learner = "boosting", # input check for the implemented super learners
                           cv = 5,
                           M = 100,
                           weight) {
    start <- Sys.time()
    checkmate::assertChoice(super.learner, choices = c("randomForest", "boosting", "nnet"))


    osubset <- sample(cut(1:task$task.desc$size, breaks = cv, labels = 1:cv))
    list.osubset <- plyr::alply(cbind(1:cv), 1, function(i) (1:task$task.desc$size)[osubset == i])
    list.isubset <- plyr::alply(cbind(1:cv), 1, function(j) (1:task$task.desc$size)[!((1:task$task.desc$size) %in% list.osubset[[j]])])



    if (cl == "classif.mclassiKernel") {
        data.grid <- expand.grid(
            hyperparams.vals
        )
        num_knn <- 0
    } else {
        knn.grid <- expand.grid(
            knn
        )
        hyperparams.grid <- expand.grid(
            hyperparams.vals
        )

        data.grid <- merge(knn.grid, hyperparams.grid, by = NULL)
        num_knn <- ncol(knn.grid)
    }


    num_grid <- ncol(data.grid)
    innerLoop <- replicate(M, apply(
        data.grid,
        1,
        function(z) {
            sapply(
                list.isubset,
                function(k) {
                    mclassiInested(
                        cl = cl,
                        task = task,
                        id = id,
                        knn = z[1:num_knn],
                        kernel = kernel,
                        par.vals = par.vals,
                        subset = k,
                        weight = weight,
                        super.learner = super.learner,
                        par.super.learner = z[(num_knn + 1):num_grid]
                    )
                }
            )
        }
    ))

    innerLoop <- apply(innerLoop, 1:2, mean)
    innerLoop <- cbind(innerLoop, 1:cv)

    innerLoopExtract <- plyr::alply(innerLoop, 1, function(x) innerResultsExtract(x, data.grid))


    outerLoopExtract <- sapply(
        innerLoopExtract,
        function(x) {
            mclassiPerf(
                cl = cl,
                task = task,
                id = id,
                knn = as.numeric(x$variables[1:num_knn]),
                kernel = kernel,
                par.vals = par.vals,
                subset = list.isubset[[as.numeric(x$cv_sample)]],
                super.learner = super.learner,
                par.super.learner = as.numeric(x$variables[(num_knn + 1):num_grid])
            )
        }
    )

    outerLoopExtract <- cbind(set = 1:cv, accuracy = outerLoopExtract, t(sapply(innerLoopExtract, function(x, data.grid) x$variables)))
    end <- Sys.time()
    print(end - start)
    return(outerLoopExtract)
}


# #' mclassiInested
# #'
# #' task: define a task with makeClassifTask
# #' data: data set with: id, dimension, trajectories as rows and condition
# #' target: condition
# #' knn: value for nearest neighbor
# #' par.vals: hyperparameters
# #' subset: ith inner sample
# #' weight: subsampling is used to tuned parameters. "weight" is a vector with weights for training and testing data sets
# #' @export
# #'

# cl <- "classif.mclassiKnn"
# fdata <- lapply(fdata, as.matrix)
# Mdist_1 <- mManhattan(fdata, parallel = TRUE, cl = 8)
# Mdist_2 <- mEuclidean(fdata, parallel = TRUE, cl = 8)
# # Mdist_3=mglobMax(fdata,parallel=TRUE,cl=8)
# task <- makeClassifTask(data = dataApp4, target = "condition")
# id <- c("L1", "L2")
# knn <- c(4, 4)
# par.vals <- list(
#     list(metric = "L1", mdist = Mdist_1, predict.type = "prob"),
#     list(metric = "L2", mdist = Mdist_2, predict.type = "prob")
# )
# subset <- list.isubset[[1]]

# test <- mclassiInested(
#     cl = "classif.mclassiKnn",
#     task = makeClassifTask(data = dataApp4, target = "condition"),
#     id = c("L1", "L2", "globMax"),
#     par.vals = list(
#         list(metric = "L1", mdist = Mdist_1, predict.type = "prob"),
#         list(metric = "L2", mdist = Mdist_2, predict.type = "prob"),
#         list(metric = "globMax", mdist = Mdist_3, predict.type = "prob")
#     ),
#     knn = c(4, 4, 4),
#     subset = list.isubset[[1]],
#     weight = NULL,
#     super.learner = "boosting",
#     par.super.learner = c(300, 1, 0.01)
# )

mclassiInested <- function(cl,
                           task,
                           id,
                           par.vals,
                           knn = 1L,
                           kernel = NULL,
                           subset,
                           weight = NULL,
                           super.learner,
                           par.super.learner) {
    if (is.null(task)) {
        stop("Error: task is NULL")
    }

    if (length(id) == 1) {
        if ("measure" %in% names(par.vals)) {
            checkmate::assertChoice(par.vals$measure, choices = mmeasuresChoices())
            par.vals$metric <- NULL # "Euclidean"
        } else {
            checkmate::assertCharacter(par.vals$metric)
            checkmate::assertChoice(par.vals$metric, choices = mmetricChoices())
            par.vals$measure <- NULL
        }
        if (cl == "classif.mclassiKnn") {
            learner <- makeLearner(
                cl = cl,
                id = id,
                par.vals = list(
                    metric = par.vals$metric,
                    mdist = par.vals$mdist[((1:task$task.desc$size) %in% subset), ((1:task$task.desc$size) %in% subset)],
                    knn = knn,
                    measure = par.vals$measure
                ),
                predict.type = par.vals$predict.type
            )
        } else {
            learner <- makeLearner(
                cl = cl,
                id = id,
                par.vals = list(
                    metric = par.vals$metric,
                    measure = par.vals$measure,
                    kernel = kernel
                ),
                predict.type = par.vals$predict.type
            )
        }
    } else {
        learner <- mclassiStackLearner(task = task, cl = cl, id = id, knn = knn, kernel = kernel, par.vals = par.vals, subset = subset)
    }
    subtaskTrain <- subsetTask(task, subset = subset)
    isubset <- sample(c(TRUE, FALSE), size = length(subset), replace = TRUE, prob = weight)
    isubset <- c(1:length(isubset))[isubset]
    if (length(id) == 1) {
        model <- train(learner = learner, task = subtaskTrain, subset = isubset)
    } else {
        model <- mclassiStackTrain(learner, task = task, subset = isubset)
    }

    subtaskTest <- subsetTask(subtaskTrain, subset = c(1:length(subset))[!(c(1:length(subset)) %in% isubset)])
    if (length(id) == 1) {
        pred <- predict(model, task = subtaskTest)
        rreturn <- sum(diag(table(pred$data[, -1]))) / sum(table(pred$data[, -1]))
    } else {
        pred <- mclassiStackTest(learner, model, subtaskTest)

        rreturn <- mclassiStack(model, pred, par.super.learner = par.super.learner, super.learner = super.learner)

        rreturn <- rreturn$y
    }
    return(rreturn)
}

#' mclassiPerf
#'
#' cl: classifier
#' task: define a task with makeClassifTask
#' data: data set with: id, dimension, trajectories as rows and condition
#' target: condition
#' knn: value for nearest neighbor
#' kernel: type of kernel to be used
#' par.vals: hyperparameters
#' subset: ith inner sample
#' #'@export
mclassiPerf <- function(cl,
                        task,
                        id,
                        knn = NULL,
                        kernel = NULL,
                        par.vals,
                        subset,
                        super.learner = NULL,
                        par.super.learner = NULL) {
    if (length(id) == 1) {
        if (cl == "classif.mclassiKnn") {
            learner <- makeLearner(
                cl = cl,
                id = id,
                par.vals = list(
                    metric = par.vals$metric,
                    mdist = par.vals$mdist,
                    knn = knn,
                    measure = par.vals$measure
                ),
                predict.type = par.vals$predict.type
            )
        } else {
            learner <- makeLearner(
                cl = cl,
                id = id,
                par.vals = list(
                    metric = par.vals$metric,
                    measure = par.vals$measure
                ),
                predict.type = par.vals$predict.type
            )
        }

        modelPerm <- train(learner = learner, task = task, subset = subset)
        pred <- predict(modelPerm, task = task, subset = (1:task$task.desc$size)[!((1:task$task.desc$size) %in% subset)])
        rreturn <- sum(diag(table(pred$data[, -1]))) / sum(table(pred$data[, -1]))
    } else {
        learnerPerm <- mclassiStackLearner(task = task, cl = cl, id = id, knn = knn, kernel = kernel, par.vals = par.vals)
        modelPerm <- mclassiStackTrain(learnerPerm, task = task, subset = subset, outer = TRUE)

        subtaskTest <- subsetTask(task, subset = (1:task$task.desc$size)[!((1:task$task.desc$size) %in% subset)])

        pred <- mclassiStackTest(learnerPerm, modelPerm, subtaskTest)

        rreturn <- mclassiStack(modelPerm, pred, par.super.learner = par.super.learner, super.learner = super.learner)
        rreturn <- rreturn$y
    }


    return(rreturn)

    # learnerPerm <- makeLearner(
    #     cl = cl,
    #     id = "testsy",
    #     par.vals = list(
    #         metric = par.vals$metric,
    #         mdist = par.vals$mdist,
    #         knn = knn
    #     )
    # )
    # modelPerm <- train(learner = learnerPerm, task = task, subset = subset)
    # pred <- predict(modelPerm, task = task, subset = (1:task$task.desc$size)[!((1:task$task.desc$size) %in% subset)])
    # return(sum(diag(table(pred$data[, -1]))) / sum(table(pred$data[, -1])))
}

#' innerResultsExtract
#'
#' @export
innerResultsExtract <- function(x, df) {
    max <- which.max(x[-length(x)])
    list(
        cv_sample = x[length(x)],
        variables = df[max, ],
        accuracy = x[max]
    )
}
