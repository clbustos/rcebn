% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mi.descriptives.r
\name{mi.descriptives}
\alias{mi.descriptives}
\title{Calcular las medias, desviaciones estándar e intervalos de confianza, considerando la existencia de datos perdidos}
\usage{
mi.descriptives(
  mi,
  subset = 1:nrow(mice::complete(mi)),
  FUN = I,
  variables = 1:ncol(FUN(mice::complete(mi)))
)
}
\description{
Calcular las medias, desviaciones estándar e intervalos de confianza, considerando la existencia de datos perdidos
}
