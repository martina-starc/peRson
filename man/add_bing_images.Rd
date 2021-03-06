% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{add_bing_images}
\alias{add_bing_images}
\title{Add Bing image search images to questions with missing images}
\usage{
add_bing_images(questions)
}
\arguments{
\item{questions}{A data frame with questions (see [demo_questions] for
format).}
}
\value{
A data frame in which missing values in image variables have been
  replaces with URLs generated by [get_first_bing_image()].
}
\description{
Add Bing image search images to questions with missing images
}
