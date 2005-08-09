\documentclass[a4paper,10pt]{article}
\usepackage{pscyr}
%\usepackage{cm-super}
\usepackage[cp1251]{inputenc}
\usepackage[english,russian]{babel}
\usepackage{color}
%\pdfoutput=0
\ifx\pdftexversion\undefined %if using TeX
  \usepackage{graphicx}
\else %if using PDFTeX
  \usepackage[pdftex]{graphicx}
\fi
\usepackage{ifpdf}
\ifpdf %if using PDFTeX in PDF mode
  \DeclareGraphicsExtensions{.pdf,.png,.mps}
  \usepackage{pgf}% uses xcolor
%\ifx\pdftexversion\undefined 
\else %if using TeX or PDFTeX in TeX mode
  \DeclareGraphicsExtensions{.eps,.bmp}
  \DeclareGraphicsRule{.emf}{bmp}{}{}% declare EMF filename extension
  \DeclareGraphicsRule{.png}{bmp}{}{}% declare PNG filename extension
  \usepackage{pgf}% uses xcolor
  \usepackage{pstricks}%variant: \usepackage{pst-all}
\fi
\usepackage{epic,bez123}
\usepackage{floatflt}% package for floatingfigure environment
\usepackage{wrapfig}% package for wrapfigure environment
\renewcommand*{\rmdefault}{ftm} 
\renewcommand*{\sfdefault}{ftx} 
\renewcommand*{\ttdefault}{fcr} 

%\def\acdefault{fac} %% Academy
%\def\aqdefault{faq} %% AntiquaPSCyr
%\def\codefault{fco} %% College
%\def\hadefault{fha} %% HandbookPSCyr
%\def\jndefault{fjn} %% JournalPSCyr
%\def\lzdefault{flz} %% Lazurski
%\def\tmdefault{ftm} %% TimesNewRomanPSMT

%\def\ardefault{far} %% ArialMT
%\def\madefault{fma} %% MagazinePSCyr
%\def\txdefault{ftx} %% TextbookPSCyr

%\def\erdefault{fer} %% ERKurierPSCyr
%\def\crdefault{fcr} %% CourierNewPSMT
