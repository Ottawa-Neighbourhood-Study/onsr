% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/census.R
\name{census_get_data}
\alias{census_get_data}
\title{Get census data for one or more DGUIDs}
\usage{
census_get_data(dguids = NA, topic = 0, notes = 0, stat = 0, lang = "E")
}
\arguments{
\item{topic}{Which subset of census data should be returned? The following values are accepted;
\itemize{
\item 0 = All topics (default)
\item 1 = Aboriginal peoples
\item 2 = Education
\item 3 = Ethnic origin
\item 4 = Families, households and marital status
\item 5 = Housing
\item 6 = Immigration and citizenship
\item 7 = Income
\item 8 = Journey to work
\item 9 = Labour
\item 10 = Language
\item 11 = Language of work
\item 12 = Mobility
\item 13 = Population
\item 14 = Visible minority
}}

\item{notes}{Should footnotes be returned? 0 = no (default), 1 = yes.}

\item{stat}{What statistic should be returned? 0 = counts (default), 1 = rates.}
}
\value{

}
\description{
Should write more stuff here
}
