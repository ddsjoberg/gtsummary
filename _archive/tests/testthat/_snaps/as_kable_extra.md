# tbl_summary

    Code
      tbl
    Output
      
      \begin{tabular}{l|c}
      \hline
      \textbf{Characteristic} & \textbf{N = 200}\\
      \hline
      Chemotherapy Treatment & \\
      \hline
      \hspace{1em}Drug A & 98 (49\%)\\
      \hline
      \hspace{1em}Drug B & 102 (51\%)\\
      \hline
      Age & 47 (38, 57)\\
      \hline
      \hspace{1em}Unknown & 11\\
      \hline
      Marker Level (ng/mL) & 0.64 (0.22, 1.39)\\
      \hline
      \hspace{1em}Unknown & 10\\
      \hline
      T Stage & \\
      \hline
      \hspace{1em}T1 & 53 (27\%)\\
      \hline
      \hspace{1em}T2 & 54 (27\%)\\
      \hline
      \hspace{1em}T3 & 43 (22\%)\\
      \hline
      \hspace{1em}T4 & 50 (25\%)\\
      \hline
      Grade & \\
      \hline
      \hspace{1em}I & 68 (34\%)\\
      \hline
      \hspace{1em}II & 68 (34\%)\\
      \hline
      \hspace{1em}III & 64 (32\%)\\
      \hline
      Tumor Response & 61 (32\%)\\
      \hline
      \hspace{1em}Unknown & 7\\
      \hline
      Patient Died & 112 (56\%)\\
      \hline
      Months to Death/Censor & 22.4 (16.0, 24.0)\\
      \hline
      \multicolumn{2}{l}{\rule{0pt}{1em}\textsuperscript{1} n (\%); Median (IQR)}\\
      \end{tabular}

---

    Code
      tbl
    Output
      
      \begin{tabular}{l|c}
      \hline
      \textbf{Characteristic} & \textbf{N = 200}\\
      \hline
      Chemotherapy Treatment & \\
      \hline
      \hspace{1em}Drug A & 98 (49\%)\\
      \hline
      \hspace{1em}Drug B & 102 (51\%)\\
      \hline
      Age & 47 (38, 57)\\
      \hline
      \hspace{1em}Unknown & 11\\
      \hline
      \multicolumn{2}{l}{\rule{0pt}{1em}\textsuperscript{1} n (\%); Median (IQR)}\\
      \multicolumn{2}{l}{\rule{0pt}{1em}\textsuperscript{2} test footnote}\\
      \end{tabular}

# tbl_cross

    Code
      tbl
    Output
      
      \begin{tabular}{l|c|c|c|c}
      \hline
      \multicolumn{1}{c|}{ } & \multicolumn{3}{c|}{Grade} & \multicolumn{1}{c}{ } \\
      \cline{2-4}
       & I & II & III & Total\\
      \hline
      Chemotherapy Treatment &  &  &  & \\
      \hline
      \hspace{1em}Drug A & 35 & 32 & 31 & 98\\
      \hline
      \hspace{1em}Drug B & 33 & 36 & 33 & 102\\
      \hline
      Total & 68 & 68 & 64 & 200\\
      \hline
      \end{tabular}

# tbl_regression

    Code
      tbl
    Output
      
      \begin{tabular}{l|c|c|c}
      \hline
      \textbf{Characteristic} & \textbf{Beta} & \textbf{95\% CI} & \textbf{p-value}\\
      \hline
      Age & 0.00 & -0.01, 0.01 & >0.9\\
      \hline
      \multicolumn{4}{l}{\rule{0pt}{1em}\textsuperscript{1} CI = Confidence Interval}\\
      \end{tabular}

# tbl_uvregression

    Code
      tbl
    Output
      
      \begin{tabular}{l|c|c|c|c}
      \hline
      \textbf{Characteristic} & \textbf{N} & \textbf{Beta} & \textbf{95\% CI} & \textbf{p-value}\\
      \hline
      Chemotherapy Treatment & 189 &  &  & \\
      \hline
      \hspace{1em}Drug A &  & — & — & \\
      \hline
      \hspace{1em}Drug B &  & 0.44 & -3.7, 4.6 & 0.8\\
      \hline
      Marker Level (ng/mL) & 179 & -0.05 & -2.5, 2.4 & >0.9\\
      \hline
      T Stage & 189 &  &  & \\
      \hline
      \hspace{1em}T1 &  & — & — & \\
      \hline
      \hspace{1em}T2 &  & 1.3 & -4.2, 6.9 & 0.6\\
      \hline
      \hspace{1em}T3 &  & 2.6 & -3.3, 8.6 & 0.4\\
      \hline
      \hspace{1em}T4 &  & -2.0 & -7.8, 3.8 & 0.5\\
      \hline
      Grade & 189 &  &  & \\
      \hline
      \hspace{1em}I &  & — & — & \\
      \hline
      \hspace{1em}II &  & 1.4 & -3.6, 6.4 & 0.6\\
      \hline
      \hspace{1em}III &  & 2.0 & -3.1, 7.0 & 0.4\\
      \hline
      Tumor Response & 183 & 3.8 & -0.66, 8.3 & 0.094\\
      \hline
      Patient Died & 189 & 2.2 & -2.0, 6.3 & 0.3\\
      \hline
      Months to Death/Censor & 189 & -0.14 & -0.54, 0.26 & 0.5\\
      \hline
      \multicolumn{5}{l}{\rule{0pt}{1em}\textsuperscript{1} CI = Confidence Interval}\\
      \end{tabular}

# tbl_survfit

    Code
      tbl
    Output
      
      \begin{tabular}{l|c|c}
      \hline
      \textbf{Characteristic} & 12 Months & 24 Months\\
      \hline
      Chemotherapy Treatment &  & \\
      \hline
      \hspace{1em}Drug A & 91\% (85\%, 97\%) & 47\% (38\%, 58\%)\\
      \hline
      \hspace{1em}Drug B & 86\% (80\%, 93\%) & 41\% (33\%, 52\%)\\
      \hline
      \end{tabular}

# tbl_merge/tbl_stack

    Code
      tbl
    Output
      
      \begin{tabular}{l|c|c|c|c|c|c}
      \hline
      \multicolumn{1}{c|}{ } & \multicolumn{3}{c|}{\textbf{Tumor Response}} & \multicolumn{3}{c}{\textbf{Time to Death}} \\
      \cline{2-4} \cline{5-7}
      \textbf{Characteristic} & \textbf{OR} & \textbf{95\% CI} & \textbf{p-value} & \textbf{HR} & \textbf{95\% CI} & \textbf{p-value}\\
      \hline
      Chemotherapy Treatment &  &  &  &  &  & \\
      \hline
      \hspace{1em}Drug A & — & — &  & — & — & \\
      \hline
      \hspace{1em}Drug B & 1.13 & 0.60, 2.13 & 0.7 & 1.30 & 0.88, 1.92 & 0.2\\
      \hline
      Grade &  &  &  &  &  & \\
      \hline
      \hspace{1em}I & — & — &  & — & — & \\
      \hline
      \hspace{1em}II & 0.85 & 0.39, 1.85 & 0.7 & 1.21 & 0.73, 1.99 & 0.5\\
      \hline
      \hspace{1em}III & 1.01 & 0.47, 2.15 & >0.9 & 1.79 & 1.12, 2.86 & 0.014\\
      \hline
      Age & 1.02 & 1.00, 1.04 & 0.10 & 1.01 & 0.99, 1.02 & 0.3\\
      \hline
      \multicolumn{7}{l}{\rule{0pt}{1em}\textsuperscript{1} OR = Odds Ratio, CI = Confidence Interval, HR = Hazard Ratio}\\
      \end{tabular}

---

    Code
      tbl
    Output
      
      \begin{tabular}{l|l|c|c|c}
      \hline
      \textbf{Group} & \textbf{Characteristic} & \textbf{OR} & \textbf{95\% CI} & \textbf{p-value}\\
      \hline
      **Tumor Response** & Chemotherapy Treatment &  &  & \\
      \hline
      \hspace{1em} & Drug A & — & — \vphantom{1} & \\
      \hline
      \hspace{1em} & Drug B & 1.13 & 0.60, 2.13 & 0.7\\
      \hline
       & Grade &  &  \vphantom{1} & \\
      \hline
      \hspace{1em} & I & — & — \vphantom{1} & \\
      \hline
      \hspace{1em} & II & 0.85 & 0.39, 1.85 & 0.7\\
      \hline
      \hspace{1em} & III & 1.01 & 0.47, 2.15 & >0.9\\
      \hline
       & Age & 1.02 & 1.00, 1.04 & 0.10\\
      \hline
      **Time to Death** & Chemotherapy Treatment &  &  & \\
      \hline
      \hspace{1em} & Drug A & — & — & \\
      \hline
      \hspace{1em} & Drug B & 1.30 & 0.88, 1.92 & 0.2\\
      \hline
       & Grade &  &  & \\
      \hline
      \hspace{1em} & I & — & — & \\
      \hline
      \hspace{1em} & II & 1.21 & 0.73, 1.99 & 0.5\\
      \hline
      \hspace{1em} & III & 1.79 & 1.12, 2.86 & 0.014\\
      \hline
       & Age & 1.01 & 0.99, 1.02 & 0.3\\
      \hline
      \multicolumn{5}{l}{\rule{0pt}{1em}\textsuperscript{1} OR = Odds Ratio, CI = Confidence Interval}\\
      \end{tabular}

---

    Code
      tbl
    Output
      
      \begin{tabular}{l|l|c|c|c}
      \hline
      \textbf{Group} & \textbf{Characteristic} & \textbf{OR} & \textbf{95\% CI} & \textbf{p-value}\\
      \hline
      **Tumor Response** & Chemotherapy Treatment &  &  & \\
      \hline
      \hspace{1em} & Drug A & — & — \vphantom{1} & \\
      \hline
      \hspace{1em} & Drug B & 1.13 & 0.60, 2.13 & 0.7\\
      \hline
       & Grade &  &  \vphantom{1} & \\
      \hline
      \hspace{1em} & I & — & — \vphantom{1} & \\
      \hline
      \hspace{1em} & II & 0.85 & 0.39, 1.85 & 0.7\\
      \hline
      \hspace{1em} & III & 1.01 & 0.47, 2.15 & >0.9\\
      \hline
       & Age & 1.02 & 1.00, 1.04 & 0.10\\
      \hline
      **Time to Death** & Chemotherapy Treatment &  &  & \\
      \hline
      \hspace{1em} & Drug A & — & — & \\
      \hline
      \hspace{1em} & Drug B & 1.30 & 0.88, 1.92 & 0.2\\
      \hline
       & Grade &  &  & \\
      \hline
      \hspace{1em} & I & — & — & \\
      \hline
      \hspace{1em} & II & 1.21 & 0.73, 1.99 & 0.5\\
      \hline
      \hspace{1em} & III & 1.79 & 1.12, 2.86 & 0.014\\
      \hline
       & Age & 1.01 & 0.99, 1.02 & 0.3\\
      \hline
      \multicolumn{5}{l}{\rule{0pt}{1em}\textsuperscript{1} OR = Odds Ratio, CI = Confidence Interval}\\
      \end{tabular}

---

    Code
      tbl
    Output
      
      \begin{tabular}{>{}l|l|c|c|c}
      \hline
      Group & Characteristic & OR & 95\% CI & p-value\\
      \hline
      \textbf{**Tumor Response**} & Chemotherapy Treatment &  &  & \\
      \hline
      \hspace{1em} & Drug A & — & — \vphantom{1} & \\
      \hline
      \hspace{1em} & Drug B & 1.13 & 0.60, 2.13 & 0.7\\
      \hline
      \textbf{} & Grade &  &  \vphantom{1} & \\
      \hline
      \hspace{1em} & I & — & — \vphantom{1} & \\
      \hline
      \hspace{1em} & II & 0.85 & 0.39, 1.85 & 0.7\\
      \hline
      \hspace{1em} & III & 1.01 & 0.47, 2.15 & >0.9\\
      \hline
      \textbf{} & Age & 1.02 & 1.00, 1.04 & 0.10\\
      \hline
      \textbf{**Time to Death**} & Chemotherapy Treatment &  &  & \\
      \hline
      \hspace{1em} & Drug A & — & — & \\
      \hline
      \hspace{1em} & Drug B & 1.30 & 0.88, 1.92 & 0.2\\
      \hline
      \textbf{} & Grade &  &  & \\
      \hline
      \hspace{1em} & I & — & — & \\
      \hline
      \hspace{1em} & II & 1.21 & 0.73, 1.99 & 0.5\\
      \hline
      \hspace{1em} & III & 1.79 & 1.12, 2.86 & 0.014\\
      \hline
      \textbf{} & Age & 1.01 & 0.99, 1.02 & 0.3\\
      \hline
      \multicolumn{5}{l}{\rule{0pt}{1em}\textsuperscript{1} OR = Odds Ratio, CI = Confidence Interval}\\
      \end{tabular}

---

    Code
      tbl
    Output
      
      \begin{tabular}{l|l|c|c|c}
      \hline
      Group & Characteristic & OR & 95\% CI & p-value\\
      \hline
      **Tumor Response** & Chemotherapy Treatment &  &  & \\
      \hline
      \hspace{1em} & Drug A & — & — \vphantom{1} & \\
      \hline
      \hspace{1em} & Drug B & 1.13 & 0.60, 2.13 & 0.7\\
      \hline
       & Grade &  &  \vphantom{1} & \\
      \hline
      \hspace{1em} & I & — & — \vphantom{1} & \\
      \hline
      \hspace{1em} & II & 0.85 & 0.39, 1.85 & 0.7\\
      \hline
      \hspace{1em} & III & 1.01 & 0.47, 2.15 & >0.9\\
      \hline
       & Age & 1.02 & 1.00, 1.04 & 0.10\\
      \hline
      **Time to Death** & Chemotherapy Treatment &  &  & \\
      \hline
      \hspace{1em} & Drug A & — & — & \\
      \hline
      \hspace{1em} & Drug B & 1.30 & 0.88, 1.92 & 0.2\\
      \hline
       & Grade &  &  & \\
      \hline
      \hspace{1em} & I & — & — & \\
      \hline
      \hspace{1em} & II & 1.21 & 0.73, 1.99 & 0.5\\
      \hline
      \hspace{1em} & III & 1.79 & 1.12, 2.86 & 0.014\\
      \hline
       & Age & 1.01 & 0.99, 1.02 & 0.3\\
      \hline
      \multicolumn{5}{l}{\rule{0pt}{1em}\textsuperscript{1} OR = Odds Ratio, CI = Confidence Interval}\\
      \end{tabular}

---

    Code
      tbl
    Output
      <table style="NAborder-bottom: 0;">
       <thead>
        <tr>
         <th style="text-align:left;"> Group </th>
         <th style="text-align:left;"> Characteristic </th>
         <th style="text-align:center;"> OR </th>
         <th style="text-align:center;"> 95% CI </th>
         <th style="text-align:center;"> p-value </th>
        </tr>
       </thead>
      <tbody>
        <tr>
         <td style="text-align:left;"> **Tumor Response** </td>
         <td style="text-align:left;"> Chemotherapy Treatment </td>
         <td style="text-align:center;">  </td>
         <td style="text-align:center;">  </td>
         <td style="text-align:center;">  </td>
        </tr>
        <tr>
         <td style="text-align:left;padding-left: 2em;" indentlevel="1">  </td>
         <td style="text-align:left;"> Drug A </td>
         <td style="text-align:center;"> — </td>
         <td style="text-align:center;"> — </td>
         <td style="text-align:center;">  </td>
        </tr>
        <tr>
         <td style="text-align:left;padding-left: 2em;" indentlevel="1">  </td>
         <td style="text-align:left;"> Drug B </td>
         <td style="text-align:center;"> 1.13 </td>
         <td style="text-align:center;"> 0.60, 2.13 </td>
         <td style="text-align:center;"> 0.7 </td>
        </tr>
        <tr>
         <td style="text-align:left;">  </td>
         <td style="text-align:left;"> Grade </td>
         <td style="text-align:center;">  </td>
         <td style="text-align:center;">  </td>
         <td style="text-align:center;">  </td>
        </tr>
        <tr>
         <td style="text-align:left;padding-left: 2em;" indentlevel="1">  </td>
         <td style="text-align:left;"> I </td>
         <td style="text-align:center;"> — </td>
         <td style="text-align:center;"> — </td>
         <td style="text-align:center;">  </td>
        </tr>
        <tr>
         <td style="text-align:left;padding-left: 2em;" indentlevel="1">  </td>
         <td style="text-align:left;"> II </td>
         <td style="text-align:center;"> 0.85 </td>
         <td style="text-align:center;"> 0.39, 1.85 </td>
         <td style="text-align:center;"> 0.7 </td>
        </tr>
        <tr>
         <td style="text-align:left;padding-left: 2em;" indentlevel="1">  </td>
         <td style="text-align:left;"> III </td>
         <td style="text-align:center;"> 1.01 </td>
         <td style="text-align:center;"> 0.47, 2.15 </td>
         <td style="text-align:center;"> &gt;0.9 </td>
        </tr>
        <tr>
         <td style="text-align:left;">  </td>
         <td style="text-align:left;"> Age </td>
         <td style="text-align:center;"> 1.02 </td>
         <td style="text-align:center;"> 1.00, 1.04 </td>
         <td style="text-align:center;"> 0.10 </td>
        </tr>
        <tr>
         <td style="text-align:left;"> **Time to Death** </td>
         <td style="text-align:left;"> Chemotherapy Treatment </td>
         <td style="text-align:center;">  </td>
         <td style="text-align:center;">  </td>
         <td style="text-align:center;">  </td>
        </tr>
        <tr>
         <td style="text-align:left;padding-left: 2em;" indentlevel="1">  </td>
         <td style="text-align:left;"> Drug A </td>
         <td style="text-align:center;"> — </td>
         <td style="text-align:center;"> — </td>
         <td style="text-align:center;">  </td>
        </tr>
        <tr>
         <td style="text-align:left;padding-left: 2em;" indentlevel="1">  </td>
         <td style="text-align:left;"> Drug B </td>
         <td style="text-align:center;"> 1.30 </td>
         <td style="text-align:center;"> 0.88, 1.92 </td>
         <td style="text-align:center;"> 0.2 </td>
        </tr>
        <tr>
         <td style="text-align:left;">  </td>
         <td style="text-align:left;"> Grade </td>
         <td style="text-align:center;">  </td>
         <td style="text-align:center;">  </td>
         <td style="text-align:center;">  </td>
        </tr>
        <tr>
         <td style="text-align:left;padding-left: 2em;" indentlevel="1">  </td>
         <td style="text-align:left;"> I </td>
         <td style="text-align:center;"> — </td>
         <td style="text-align:center;"> — </td>
         <td style="text-align:center;">  </td>
        </tr>
        <tr>
         <td style="text-align:left;padding-left: 2em;" indentlevel="1">  </td>
         <td style="text-align:left;"> II </td>
         <td style="text-align:center;"> 1.21 </td>
         <td style="text-align:center;"> 0.73, 1.99 </td>
         <td style="text-align:center;"> 0.5 </td>
        </tr>
        <tr>
         <td style="text-align:left;padding-left: 2em;" indentlevel="1">  </td>
         <td style="text-align:left;"> III </td>
         <td style="text-align:center;"> 1.79 </td>
         <td style="text-align:center;"> 1.12, 2.86 </td>
         <td style="text-align:center;"> 0.014 </td>
        </tr>
        <tr>
         <td style="text-align:left;">  </td>
         <td style="text-align:left;"> Age </td>
         <td style="text-align:center;"> 1.01 </td>
         <td style="text-align:center;"> 0.99, 1.02 </td>
         <td style="text-align:center;"> 0.3 </td>
        </tr>
      </tbody>
      <tfoot><tr><td style="padding: 0; " colspan="100%">
      <sup>1</sup> OR = Odds Ratio, CI = Confidence Interval</td></tr></tfoot>
      </table>

---

    Code
      tbl
    Output
      <table style="NAborder-bottom: 0;">
       <thead>
        <tr>
         <th style="text-align:left;"> Group </th>
         <th style="text-align:left;"> Characteristic </th>
         <th style="text-align:center;"> OR </th>
         <th style="text-align:center;"> 95% CI </th>
         <th style="text-align:center;"> p-value </th>
        </tr>
       </thead>
      <tbody>
        <tr>
         <td style="text-align:left;font-weight: bold;"> **Tumor Response** </td>
         <td style="text-align:left;"> Chemotherapy Treatment </td>
         <td style="text-align:center;">  </td>
         <td style="text-align:center;">  </td>
         <td style="text-align:center;">  </td>
        </tr>
        <tr>
         <td style="text-align:left;padding-left: 2em;" indentlevel="1">  </td>
         <td style="text-align:left;"> Drug A </td>
         <td style="text-align:center;"> — </td>
         <td style="text-align:center;"> — </td>
         <td style="text-align:center;">  </td>
        </tr>
        <tr>
         <td style="text-align:left;padding-left: 2em;" indentlevel="1">  </td>
         <td style="text-align:left;"> Drug B </td>
         <td style="text-align:center;"> 1.13 </td>
         <td style="text-align:center;"> 0.60, 2.13 </td>
         <td style="text-align:center;"> 0.7 </td>
        </tr>
        <tr>
         <td style="text-align:left;font-weight: bold;">  </td>
         <td style="text-align:left;"> Grade </td>
         <td style="text-align:center;">  </td>
         <td style="text-align:center;">  </td>
         <td style="text-align:center;">  </td>
        </tr>
        <tr>
         <td style="text-align:left;padding-left: 2em;" indentlevel="1">  </td>
         <td style="text-align:left;"> I </td>
         <td style="text-align:center;"> — </td>
         <td style="text-align:center;"> — </td>
         <td style="text-align:center;">  </td>
        </tr>
        <tr>
         <td style="text-align:left;padding-left: 2em;" indentlevel="1">  </td>
         <td style="text-align:left;"> II </td>
         <td style="text-align:center;"> 0.85 </td>
         <td style="text-align:center;"> 0.39, 1.85 </td>
         <td style="text-align:center;"> 0.7 </td>
        </tr>
        <tr>
         <td style="text-align:left;padding-left: 2em;" indentlevel="1">  </td>
         <td style="text-align:left;"> III </td>
         <td style="text-align:center;"> 1.01 </td>
         <td style="text-align:center;"> 0.47, 2.15 </td>
         <td style="text-align:center;"> &gt;0.9 </td>
        </tr>
        <tr>
         <td style="text-align:left;font-weight: bold;">  </td>
         <td style="text-align:left;"> Age </td>
         <td style="text-align:center;"> 1.02 </td>
         <td style="text-align:center;"> 1.00, 1.04 </td>
         <td style="text-align:center;"> 0.10 </td>
        </tr>
        <tr>
         <td style="text-align:left;font-weight: bold;"> **Time to Death** </td>
         <td style="text-align:left;"> Chemotherapy Treatment </td>
         <td style="text-align:center;">  </td>
         <td style="text-align:center;">  </td>
         <td style="text-align:center;">  </td>
        </tr>
        <tr>
         <td style="text-align:left;padding-left: 2em;" indentlevel="1">  </td>
         <td style="text-align:left;"> Drug A </td>
         <td style="text-align:center;"> — </td>
         <td style="text-align:center;"> — </td>
         <td style="text-align:center;">  </td>
        </tr>
        <tr>
         <td style="text-align:left;padding-left: 2em;" indentlevel="1">  </td>
         <td style="text-align:left;"> Drug B </td>
         <td style="text-align:center;"> 1.30 </td>
         <td style="text-align:center;"> 0.88, 1.92 </td>
         <td style="text-align:center;"> 0.2 </td>
        </tr>
        <tr>
         <td style="text-align:left;font-weight: bold;">  </td>
         <td style="text-align:left;"> Grade </td>
         <td style="text-align:center;">  </td>
         <td style="text-align:center;">  </td>
         <td style="text-align:center;">  </td>
        </tr>
        <tr>
         <td style="text-align:left;padding-left: 2em;" indentlevel="1">  </td>
         <td style="text-align:left;"> I </td>
         <td style="text-align:center;"> — </td>
         <td style="text-align:center;"> — </td>
         <td style="text-align:center;">  </td>
        </tr>
        <tr>
         <td style="text-align:left;padding-left: 2em;" indentlevel="1">  </td>
         <td style="text-align:left;"> II </td>
         <td style="text-align:center;"> 1.21 </td>
         <td style="text-align:center;"> 0.73, 1.99 </td>
         <td style="text-align:center;"> 0.5 </td>
        </tr>
        <tr>
         <td style="text-align:left;padding-left: 2em;" indentlevel="1">  </td>
         <td style="text-align:left;"> III </td>
         <td style="text-align:center;"> 1.79 </td>
         <td style="text-align:center;"> 1.12, 2.86 </td>
         <td style="text-align:center;"> 0.014 </td>
        </tr>
        <tr>
         <td style="text-align:left;font-weight: bold;">  </td>
         <td style="text-align:left;"> Age </td>
         <td style="text-align:center;"> 1.01 </td>
         <td style="text-align:center;"> 0.99, 1.02 </td>
         <td style="text-align:center;"> 0.3 </td>
        </tr>
      </tbody>
      <tfoot><tr><td style="padding: 0; " colspan="100%">
      <sup>1</sup> OR = Odds Ratio, CI = Confidence Interval</td></tr></tfoot>
      </table>

# indent2

    Code
      tbl
    Output
      
      \begin{tabular}{l|c}
      \hline
      \textbf{Characteristic} & \textbf{N = 200}\\
      \hline
      Age & 47 (38, 57)\\
      \hline
      \hspace{2em}\hspace{1em}Unknown & 11\\
      \hline
      \multicolumn{2}{l}{\rule{0pt}{1em}\textsuperscript{1} Median (IQR)}\\
      \end{tabular}

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

