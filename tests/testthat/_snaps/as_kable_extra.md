# latex-column-alignment

    Code
      tstack
    Output
      
      \begin{tabular}{l|c|c}
      \hline
      \textbf{Characteristic} & \makecell[c]{\textbf{Drug A}\\N = 98} & \makecell[c]{\textbf{Drug B}\\N = 102}\\
      \hline
      Age & 46 (37, 59) & 48 (39, 56)\\
      \hline
      \hspace{1em}Unknown & 7 & 4\\
      \hline
      Grade &  & \\
      \hline
      \hspace{1em}I & 35 (36\%) & 33 (32\%)\\
      \hline
      \hspace{1em}II & 32 (33\%) & 36 (35\%)\\
      \hline
      \hspace{1em}III & 31 (32\%) & 33 (32\%)\\
      \hline
      \multicolumn{3}{l}{\rule{0pt}{1em}\textsuperscript{1} Median (IQR)}\\
      \end{tabular}

