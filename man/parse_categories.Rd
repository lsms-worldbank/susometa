% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parse_qnr_categories.R
\name{parse_categories}
\alias{parse_categories}
\title{Parse all categories files into a single data frame}
\usage{
parse_categories(
  dir,
  file_pattern = "\\\\.xlsx",
  recurse = TRUE,
  sheet = "Categories"
)
}
\arguments{
\item{dir}{Character. Directory where category files are located.}

\item{file_pattern}{Character. Default value is SuSo's default file name.}

\item{recurse}{Boolean. Default value assumes that \code{dir} is parent diectory
that contains child directories that contain the categories.}

\item{sheet}{Character. Name of the Excel sheet where categories data are
stored. By default, this is Survey Solutions' storage: \code{"Categories"}.}
}
\value{
Data frame with the following columns:
\itemize{
\item \code{category_id}. GUID used in questionnaire JSON reference a reusable
category.
\item \code{value}. Numerical value input in Designer (e.g., \code{1}, \code{2}).
\item \code{text}. Character label input in Designer (e.g., \code{"Yes"}, \code{"No"})
\item \code{parentid}. GUID used to identify parent question.
}

Note: each group with the same \code{category_id} value represents the contents
of a single reusable category file.
}
\description{
Parse all categories files into a single data frame
}
