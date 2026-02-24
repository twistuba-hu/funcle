#' Example dataset for demonstrating package functionality
#'
#' The dataset was originally published in:
#' 
#' Kuric, Eduard & Demcak, Peter & Krajcovic, Matus & Nemcek, Peter (2024) Is mouse dynamics information credible for user behavior research? An empirical investigation
#' Computer Standards & Interfaces, 90, 103849.
#' 
#' The original data are available at:
#' https://github.com/micemicsresearch/mouse-dynamics-data-credibility
#' 
#' Licensed under CC0 1.0.
#' 
#' The data contains the mouse trajectories of participants completing a mini-game with a computer mouse.
#' We pre-processed the the mouse trajectories with the \pkg{mousetrap} package (Kieslich et al., 2024).
#' We time-normalized the mouse trajectories to make sure that we have trajectories with equal sampling lengths.
#' 
#' Hence, the pre-processing step is NOT required to use this dataset.
#'
#' @format A data frame with 480 rows and 305 variables:
#' \describe{
#'   \item{mt_id}{Identifier of the trajectory as integer}
#'   \item{dim}{Dimensionality of the trajectory}
#'   \item{timestamps1-timestamps101}{Sampled timepoints}
#'   \item{xpos1-xpos101}{x-position of the cursor at a given timestamp}
#'   \item{ypos1-ypos101}{y-position of the cursor at a given timestamp}
#'   \item{sex}{Gender of user as factor}
#' }
#' @source
#' Original data: Kuric et al. (2024), CC0 1.0
#' @references 
#' Kuric, E. & Demcak, P. & Krajcovic, M. & Nemcek, P. (2024). Is mouse dynamics information credible for user behavior research? An empirical investigation
#' Computer Standards & Interfaces, 90, 103849.
#' 
#' Wulff, D. U. & Kieslich, P. J. & Henninger, F. & Haslbeck, J. M. B. & Schulte-Mecklenbeck, M. (2024). Movement tracking of psychological processes: A tutorial using mousetrap
#' R package version 3.2.3
"kuric_minigame"