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
\usepackage[utf8x]{inputenc}
% math environments and symbols
\usepackage{amsmath, amsfonts}
\usepackage[toc,page]{appendix}
\usepackage{macros}
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

\newcommand{\code}[1]{{\textbf{#1}}}
\newcommand{\module}[1]{{\textbf{#1}}}

% Symbol for euclidean space of dimension n.
\newcommand{\R}[1]{\mathbb{R}^{#1}}  % ensuremath
% Command for package name
\newcommand{\FEEC}{\textbf{FEEC}}
% Command to typeset vectors
\renewcommand{\vec}[1]{\boldsymbol{{#1}}}
% Bernstein polynomial.
\newcommand{\B}[2]{\boldsymbol{B}_{#1}^{#2}}

% Missing: where to find them?? TODO: Simon
\newcommand{\smp}[1]{\mathit{#1}}
\newcommand{\pl}[2]{\mathcal{P}_r\Lambda^k}
\newcommand{\pml}[2]{\mathcal{P}_r^-\Lambda^k}
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
  {Simon Pfreundschuh \\[1em] Irene Lobo Valbuena} % argument 2 <= authors
  {}                                     % argument 3 <= report number
%else
\title{F.FEEC: Functional Finite Element Exterior Calculus}

\authorinfo{Simon Pfreundschuh}
           {Chalmers Univeristy of Technology, Sweden}
           {\texttt{simonpf@@student.chalmers.se}}
\authorinfo{Irene Lobo Valbuena}
           {Chalmers University of Technology, Sweden}
           {\texttt{lobo@@chalmers.se}}

\maketitle
%endif

%-------------------------------------------------------------------------------

\begin{abstract}
  TBD
\end{abstract}

%------------------------------------------------------------------------------
\section{Introduction}
\label{sec:intro}

%%%include intro.lhs

Explanation of the purpose, referring to the supporting literature the implementation is based on
and the suitability of functional languages for mathematical representation.
%
%% plus: target audience


%------------------------------------------------------------------------------
\section{Basic Mathematical Components}
\label{sec:internal}

% Irene

%include Internal.lhs



%------------------------------------------------------------------------------
\section{Geometrical Constructs}
\label{sec:geo}

% Simon

%include Geometry.lhs

%------------------------------------------------------------------------------
\section{Polynomials}
\label{sec:polyns}

%Simon
%include Polynomials.lhs


NB: start with: |MultiIndex| operations

The generic implementation for polynomials |Polynomial a| is introduced first, giving those defined
over the canonical (monomial) basis as the first basic option.
%
Bernstein polynomials are then built on those and the numerical significance of their use is briefly
explained.

Supported operations on polynomials and their type class instances are specified.

This section should also introduce coordinate functions.


%------------------------------------------------------------------------------
\section{Differential Forms}
\label{sec:forms}

% Irene

%include forms.lhs

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

% Simon

%include FEspaces.lhs

Both $\mathcal{P}_r^-\Lambda^n$ and $\mathcal{P}_r\Lambda^n$ finite element spaces implementations
are given.
%
Specific, built-in "names" for spaces over concrete kinds of elements (for ease of use) are
documented too.


%------------------------------------------------------------------------------
\section{The Top-Level Language}
\label{sec:lang-mask}

% Irene

%%%include lang-mask.lhs

We complete the implementation's overview by (re-)stating the top-level language constructs
available to the user (to define objects of the types explained).
%
Some examples of possible use-cases are provided.


%------------------------------------------------------------------------------
\section{Conclusions and related work}
\label{sec:conc}

%%%include conc.lhs

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
