% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Bankkonto-sol.R
\name{Account}
\alias{Account}
\title{General Account Class}
\description{
Class for a general bank account which enables the deposit and withdraw of money
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{account_holder}}{name of the person owning the bank account}

\item{\code{balance}}{current available credit on bank account}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{Account$new()}}
\item \href{#method-print}{\code{Account$print()}}
\item \href{#method-deposit}{\code{Account$deposit()}}
\item \href{#method-withdraw}{\code{Account$withdraw()}}
\item \href{#method-clone}{\code{Account$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Account$new(account_holder, balance)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{account_holder}}{name of the person owning the bank account}

\item{\code{balance}}{starting credit of the bank account}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-print"></a>}}
\if{latex}{\out{\hypertarget{method-print}{}}}
\subsection{Method \code{print()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Account$print()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-deposit"></a>}}
\if{latex}{\out{\hypertarget{method-deposit}{}}}
\subsection{Method \code{deposit()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Account$deposit(amount)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{amount}}{amount of money which should be transferred}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-withdraw"></a>}}
\if{latex}{\out{\hypertarget{method-withdraw}{}}}
\subsection{Method \code{withdraw()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Account$withdraw(amount)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{amount}}{amount of money which should be transferred}
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
\if{html}{\out{<div class="r">}}\preformatted{Account$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
