\documentclass[a4paper,10pt]{article}
\usepackage{color}
%\pdfoutput=0 % uncomment this if running PDFTeX in TeX mode
\usepackage{ifpdf}
\ifx\pdftexversion\undefined %if using TeX
  \usepackage{graphicx}
\else %if using PDFTeX
  \usepackage[pdftex]{graphicx}
\fi
\ifpdf %if using PDFTeX in PDF mode
  \DeclareGraphicsExtensions{.pdf,.png,.mps}
  \usepackage{pgf}
\else %if using TeX or PDFTeX in TeX mode
  \usepackage{graphicx}
  \DeclareGraphicsExtensions{.eps,.bmp}
  \DeclareGraphicsRule{.emf}{bmp}{}{}% declare EMF filename extension
  \DeclareGraphicsRule{.png}{bmp}{}{}% declare PNG filename extension
  \usepackage{pgf}
  \usepackage{pstricks}%variant: \usepackage{pst-all}
\fi
\usepackage{epic,bez123}
\usepackage{floatflt}% package for floatingfigure environment
\usepackage{wrapfig}% package for wrapfigure environment
