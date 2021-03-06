% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Bankkonto-sol.R
\name{TransactionLog}
\alias{TransactionLog}
\title{Transaction log Class}
\description{
Class for a transaction log which tracks all of the attempted transactions of a bank account
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{transactions}}{'data.frame'-object, listing all the transactions made, including opening the corresponding bank account}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{TransactionLog$new()}}
\item \href{#method-new_entry}{\code{TransactionLog$new_entry()}}
\item \href{#method-clone}{\code{TransactionLog$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TransactionLog$new(date, starting_balance)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{date}}{date and time of the transaction}

\item{\code{starting_balance}}{amount of money with which the account is opened}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new_entry"></a>}}
\if{latex}{\out{\hypertarget{method-new_entry}{}}}
\subsection{Method \code{new_entry()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TransactionLog$new_entry(date, old_balance, amount, type, success, new_balance)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{date}}{date and time of the transaction}

\item{\code{old_balance}}{account balance before transaction}

\item{\code{amount}}{amount of money which was attempted to be transferred}

\item{\code{type}}{type of transaction. Must be either "deposit" or "withdraw"}

\item{\code{success}}{was the transaction successful? Either TRUE or FALSE}

\item{\code{new_balance}}{account balance after transaction}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TransactionLog$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
