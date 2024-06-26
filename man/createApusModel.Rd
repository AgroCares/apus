% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model.R
\name{createApusModel}
\alias{createApusModel}
\title{A torch model for apus}
\usage{
createApusModel(
  dataset.train,
  dataset.valid,
  width = 12,
  layers = 1,
  epochs = 100,
  device
)
}
\arguments{
\item{dataset.train}{(ApusDataset) Dataset created with createApusDataset to train the model}

\item{dataset.valid}{(ApusDataset) Dataset created with createApusDataset to validate the model}

\item{width}{(integer)}

\item{layers}{(integer)}

\item{epochs}{(integer)}

\item{device}{(character)}
}
\description{
Creates a torch model to be used for apus
}
