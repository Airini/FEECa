% -*- latex -*-
%----------------------------------------------------------------------------
% TODO:
%  Title       :  F.FEEC: Functional Finite Element Exterior Calculus
%  Author(s)   :  
%  Copyright   :  
%  Created     :  2015-06-15
%
%  Purpose     :  
%
%----------------------------------------------------------------------------

%%%let techreport = True
%let submit = False
%if submit
\documentclass[times,authoryear]{sigplanconf}
%else
\documentclass[preprint,times]{sigplanconf}
%endif

%%% Standard definitions from the lhs2TeX installation
%include polycode.fmt
%%% Put your own formatting directives in a separate file
%include tech-report.format

\usepackage{url}
\usepackage[utf8]{inputenc}
\usepackage[toc,page]{appendix}

%if techreport
\usepackage{TRtitlepage}
%endif

%%% Some useful macros
%if submit
\newcommand{\todo}[2][?]{}
%else
\newcommand{\todo}[2][?]{\marginpar{\raggedright \tiny TODO: #2}}
%endif
\newcommand{\TODO}[1]{\todo{#1}}
\newcommand{\refSec}[1]{Sec. \ref{#1}}
\newcommand{\refSecs}[1]{Secs. \ref{#1}}
\newcommand{\refSecI}[1]{Section \ref{#1}}
\newcommand{\refSecsI}[1]{Sections \ref{#1}}
\newcommand{\refTab}[1]{Tab. \ref{#1}}

%include macros.tex

\toappear{}
\begin{document}

%-------------------------------------------------------------------------------

%if submit
\CopyrightYear{2015}
%elif not techreport
\titlebanner{Preprint}
\preprintfooter{Preprint}
%endif

%%%TODO: TRtitlepage to be defined
%if techreport
\TRtitlepage
  {F.FEEC: Functional Finite Element Exterior Calculus}             % argument 1 <= the title
  {Tess Ting \\[1em] Test Ing} % argument 2 <= authors
  {}                                     % argument 3 <= report number
%else
\title{F.FEEC: Functional Finite Element Exterior Calculus}

\authorinfo{Tess Ting}
           {Chalmers Univeristy of Technology, Sweden}
           {\texttt{tess.ting@@chalmers.se}}
\authorinfo{Test Ing}
           {Chalmers University of Technology, Sweden}
           {\texttt{test.ing@@chalmers.se}}

\maketitle
%endif

%-------------------------------------------------------------------------------

\begin{abstract}
  TBD
\end{abstract}

%------------------------------------------------------------------------------
\section{Introduction}
\label{sec:intro}

Explanation of the purpose, referring to the supporting literature the implementation is based on
and the suitability of functional languages for mathematical representation.
%
%% plus: target audience


%------------------------------------------------------------------------------
\section{Basic Mathematical Components}
\label{sec:internal}

Introduction of the basic constructs and structures relevant to the implementation, both from the
mathematics and programming points of view.
%
These correspond to (most of the) internal components of the package, covering
\begin{itemize}
\item |Vector| and |Point| representations
\item |MultiIndex| operations
\item type classes structuring the package (in \module{Spaces})
\end{itemize}

We further justify the choice of the latter, referring to the following components to be specified.

The aim towards the reader's understanding will be two-fold:
\begin{itemize}
\item explain general aspects of the implementation that tie in with simpler mathematical concepts
      (e.g.: vector spaces and our type class representation of them)
\item describe the most simple datatypes and operations on them, as examples to serve as a gentle
      introduction to the non- functional programmer mathematician.
\end{itemize}


%------------------------------------------------------------------------------
\section{Geometrical Constructs}
\label{sec:elements}

Geometrical elements implemented (so far, simplices; if extended, will include cubes too) are
described.
%
Implementation aspects will be covered and computation of subs-structures (subsimplices)
explained.

In addition, barycentric coordinates (necessary for \refSec{sec:polyns}) and the purpose of having
a reference simplex will be covered.


%------------------------------------------------------------------------------
\section{Polynomials}
\label{sec:polyns}

The generic implementation for polynomials |Polynomial a| is introduced first, giving those defined
over the canonical (monomial) basis as the first basic option.
%
Bernstein polynomials are then built on those and the numerical significance of their use is briefly
explained.

Supported operations on polynomials and their type class instances are specified.

This section should also introduce coordinate functions.


%------------------------------------------------------------------------------
\section{Mathematical Algorithms}
\label{sec:utility}

% XXX: shall we keep this?

A brief overview of Gram-Scmidt and quadrature algorithms (as in our implementation).


%------------------------------------------------------------------------------
\section{Differential Forms}
\label{sec:forms}

The generic underlying type |Form a| is introduced, along with its |Algebra| (a class introduced in
\refSec{sec:internal}) operations.

The simpler alternating forms are specified first as a |Form a| datatype followed by differential
forms' definition.
%
We show how the operations on alternating and differential forms are expressible in terms of the
generalised |Form a| operations.

The built-in Whitney form implementation is also given (towards building the negative spaces in
\refSec{sec:FEspaces}.


%------------------------------------------------------------------------------
\section{Finite Element Spaces}
\label{sec:FEspaces}

Both $\mathcal{P}_r^-\Lambda^n$ and $\mathcal{P}_r\Lambda^n$ finite element spaces implementations
are given.
%
Specific, built-in "names" for spaces over concrete kinds of elements (for ease of use) are
documented too.


%------------------------------------------------------------------------------
\section{The Top-Level Language}
\label{sec:lang-mask}

We complete the implementation's overview by (re-)stating the top-level language constructs
available to the user (to define objects of the types explained).
%
Some examples of possible use-cases are provided.


%------------------------------------------------------------------------------
\section{Conclusions and related work}
\label{sec:conc}

% Final overview

\paragraph{Acknowledgements.} ...


%------------------------------------------------------------------------------
\begin{appendices}
\section{Functional Programming Concepts}
%% Anything we wish/need to explain for clarity keeping the report self-contained

\section{Mathematics Concepts}
%% Any basic mathematical concepts

\section{Notation}
%% Maybe have it be the first appendix?

%% XXX: perhaps: overview of purpose? \section{Finite Elements}

\end{appendices}

%------------------------------------------------------------------------------
\bibliographystyle{abbrvnat}
%%% Keep references in a separate bib-file
\bibliography{tech-report}

\end{document}
