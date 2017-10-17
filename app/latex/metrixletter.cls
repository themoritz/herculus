\ProvidesClass{metrixletter}

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{letter}}
\ProcessOptions
\LoadClass[a4paper]{letter}

\RequirePackage{calc}
\RequirePackage{ifthen}

\DeclareFixedFont{\viiisf}{OT1}{cmss}{m}{n}{8}

\newcommand{\@subject}{}
\newcommand{\subject}[1]{\renewcommand{\@subject}{{\bf #1}}}

\newcommand{\@shortaddress}{}
\newcommand{\shortaddress}[1]{\renewcommand{\@shortaddress}{#1}}

\newcommand{\@noaddress}{}
\newcommand{\noaddress}{\renewcommand{\@noaddress}{dummy}}

\RequirePackage{graphicx}
\RequirePackage{eso-pic}
\AddToShipoutPictureBG{%
  \AtPageUpperLeft{\raisebox{-\height}{\includegraphics{background-logo-big}}}%
}


\newlength{\rightfield}

\renewcommand{\ps@firstpage}{%
   \renewcommand{\@oddfoot}{}
   \renewcommand{\@evenfoot}{}
}

% TODO: Folding hint at 9cm from top (!)

\renewcommand{\opening}[1]{
   \thispagestyle{firstpage}

   % Short address in letter window
   \newsavebox{\preturn}
   \sbox{\preturn}{\viiisf\underline{\@shortaddress}}

   % Generate from address box and measure its width
   \newsavebox{\fromaddy}
   \sbox{\fromaddy}{%
      \begin{tabular}[t]{@{}r@{}}
         \fromaddress%
      \end{tabular}%
   }

   \settowidth{\rightfield}{\usebox{\fromaddy}}

  %  % Heading
  %  \fromname\\[-1.2ex]
  %  \raisebox{0.4ex}{\rule{\textwidth}{0.7pt}}
   \vspace*{1cm}
   \parbox[t]{0.6\textwidth}{%
      \vspace*{1.0cm}
      \ifthenelse{\equal{\@noaddress}{}}{%
        \parbox[t]{0.6\textwidth}{\usebox{\preturn}}\\
        \parbox[b][3.2cm][c]{0.6\textwidth}{%
          \toname\\ \toaddress}
      }{%
      \parbox[t]{0.6\textwidth}{\vspace*{1.5cm}}
      }
   }%
   \makebox[0.4\textwidth][r]{%
      \usebox{\fromaddy}%
   }

   \vspace{2\parskip}

   \ifthenelse{\equal{\@subject}{}}
      {}{\@subject\par}

   % Datum buendig mit Adresse
  %  \makebox[\textwidth][r]{%
  %    \makebox[\rightfield][l]{\@date}
  %  }

   % Datum rechtsbuendig
   \makebox[\textwidth][r]{\@date}

   \vspace{2\parskip} #1 \vspace{2\parskip}
}

\renewcommand{\closing}[1]{
   \vspace{2\parskip}
   \mbox{%
      \hspace{1cm}%
      \parbox[t]{0.8\textwidth}{#1}
   }
}

\renewcommand{\@texttop}{}
