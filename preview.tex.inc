\documentclass[a4paper,10pt]{article}
\usepackage[cp1251]{inputenc}
\usepackage[english,russian]{babel}
\usepackage{color}
\ifx\pdftexversion\undefined %if using TeX
  \usepackage{graphicx}
  \usepackage{pstricks}%variant: \usepackage{pst-all}
  \DeclareGraphicsExtensions{.eps,.bmp}
  \DeclareGraphicsRule{.emf}{bmp}{}{}% declare EMF filename extension
  \DeclareGraphicsRule{.png}{bmp}{}{}% declare PNG filename extension
\else %if using PDFTeX
  \usepackage[pdftex]{graphicx}
  \DeclareGraphicsExtensions{.pdf,.png,.mps}
\fi
\usepackage{epic,bez123}
\usepackage{floatflt}% package for floatingfigure environment
\usepackage{wrapfig}% package for wrapfigure environment
