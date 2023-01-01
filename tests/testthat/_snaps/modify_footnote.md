# modify_footnote works

    Code
      tbl_summary %>% modify_footnote(update = starts_with("stat_") ~
        "median (IQR) for continuous variables; n (%) categorical variables")
    Output
      <div id="ngabqinzwk" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
        <style>html {
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
      }
      
      #ngabqinzwk .gt_table {
        display: table;
        border-collapse: collapse;
        margin-left: auto;
        margin-right: auto;
        color: #333333;
        font-size: 16px;
        font-weight: normal;
        font-style: normal;
        background-color: #FFFFFF;
        width: auto;
        border-top-style: solid;
        border-top-width: 2px;
        border-top-color: #A8A8A8;
        border-right-style: none;
        border-right-width: 2px;
        border-right-color: #D3D3D3;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #A8A8A8;
        border-left-style: none;
        border-left-width: 2px;
        border-left-color: #D3D3D3;
      }
      
      #ngabqinzwk .gt_heading {
        background-color: #FFFFFF;
        text-align: center;
        border-bottom-color: #FFFFFF;
        border-left-style: none;
        border-left-width: 1px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 1px;
        border-right-color: #D3D3D3;
      }
      
      #ngabqinzwk .gt_caption {
        padding-top: 4px;
        padding-bottom: 4px;
      }
      
      #ngabqinzwk .gt_title {
        color: #333333;
        font-size: 125%;
        font-weight: initial;
        padding-top: 4px;
        padding-bottom: 4px;
        padding-left: 5px;
        padding-right: 5px;
        border-bottom-color: #FFFFFF;
        border-bottom-width: 0;
      }
      
      #ngabqinzwk .gt_subtitle {
        color: #333333;
        font-size: 85%;
        font-weight: initial;
        padding-top: 0;
        padding-bottom: 6px;
        padding-left: 5px;
        padding-right: 5px;
        border-top-color: #FFFFFF;
        border-top-width: 0;
      }
      
      #ngabqinzwk .gt_bottom_border {
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
      }
      
      #ngabqinzwk .gt_col_headings {
        border-top-style: solid;
        border-top-width: 2px;
        border-top-color: #D3D3D3;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        border-left-style: none;
        border-left-width: 1px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 1px;
        border-right-color: #D3D3D3;
      }
      
      #ngabqinzwk .gt_col_heading {
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: normal;
        text-transform: inherit;
        border-left-style: none;
        border-left-width: 1px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 1px;
        border-right-color: #D3D3D3;
        vertical-align: bottom;
        padding-top: 5px;
        padding-bottom: 6px;
        padding-left: 5px;
        padding-right: 5px;
        overflow-x: hidden;
      }
      
      #ngabqinzwk .gt_column_spanner_outer {
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: normal;
        text-transform: inherit;
        padding-top: 0;
        padding-bottom: 0;
        padding-left: 4px;
        padding-right: 4px;
      }
      
      #ngabqinzwk .gt_column_spanner_outer:first-child {
        padding-left: 0;
      }
      
      #ngabqinzwk .gt_column_spanner_outer:last-child {
        padding-right: 0;
      }
      
      #ngabqinzwk .gt_column_spanner {
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        vertical-align: bottom;
        padding-top: 5px;
        padding-bottom: 5px;
        overflow-x: hidden;
        display: inline-block;
        width: 100%;
      }
      
      #ngabqinzwk .gt_group_heading {
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: initial;
        text-transform: inherit;
        border-top-style: solid;
        border-top-width: 2px;
        border-top-color: #D3D3D3;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        border-left-style: none;
        border-left-width: 1px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 1px;
        border-right-color: #D3D3D3;
        vertical-align: middle;
        text-align: left;
      }
      
      #ngabqinzwk .gt_empty_group_heading {
        padding: 0.5px;
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: initial;
        border-top-style: solid;
        border-top-width: 2px;
        border-top-color: #D3D3D3;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        vertical-align: middle;
      }
      
      #ngabqinzwk .gt_from_md > :first-child {
        margin-top: 0;
      }
      
      #ngabqinzwk .gt_from_md > :last-child {
        margin-bottom: 0;
      }
      
      #ngabqinzwk .gt_row {
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
        margin: 10px;
        border-top-style: solid;
        border-top-width: 1px;
        border-top-color: #D3D3D3;
        border-left-style: none;
        border-left-width: 1px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 1px;
        border-right-color: #D3D3D3;
        vertical-align: middle;
        overflow-x: hidden;
      }
      
      #ngabqinzwk .gt_stub {
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: initial;
        text-transform: inherit;
        border-right-style: solid;
        border-right-width: 2px;
        border-right-color: #D3D3D3;
        padding-left: 5px;
        padding-right: 5px;
      }
      
      #ngabqinzwk .gt_stub_row_group {
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: initial;
        text-transform: inherit;
        border-right-style: solid;
        border-right-width: 2px;
        border-right-color: #D3D3D3;
        padding-left: 5px;
        padding-right: 5px;
        vertical-align: top;
      }
      
      #ngabqinzwk .gt_row_group_first td {
        border-top-width: 2px;
      }
      
      #ngabqinzwk .gt_summary_row {
        color: #333333;
        background-color: #FFFFFF;
        text-transform: inherit;
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
      }
      
      #ngabqinzwk .gt_first_summary_row {
        border-top-style: solid;
        border-top-color: #D3D3D3;
      }
      
      #ngabqinzwk .gt_first_summary_row.thick {
        border-top-width: 2px;
      }
      
      #ngabqinzwk .gt_last_summary_row {
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
      }
      
      #ngabqinzwk .gt_grand_summary_row {
        color: #333333;
        background-color: #FFFFFF;
        text-transform: inherit;
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
      }
      
      #ngabqinzwk .gt_first_grand_summary_row {
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
        border-top-style: double;
        border-top-width: 6px;
        border-top-color: #D3D3D3;
      }
      
      #ngabqinzwk .gt_striped {
        background-color: rgba(128, 128, 128, 0.05);
      }
      
      #ngabqinzwk .gt_table_body {
        border-top-style: solid;
        border-top-width: 2px;
        border-top-color: #D3D3D3;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
      }
      
      #ngabqinzwk .gt_footnotes {
        color: #333333;
        background-color: #FFFFFF;
        border-bottom-style: none;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        border-left-style: none;
        border-left-width: 2px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 2px;
        border-right-color: #D3D3D3;
      }
      
      #ngabqinzwk .gt_footnote {
        margin: 0px;
        font-size: 90%;
        padding-left: 4px;
        padding-right: 4px;
        padding-left: 5px;
        padding-right: 5px;
      }
      
      #ngabqinzwk .gt_sourcenotes {
        color: #333333;
        background-color: #FFFFFF;
        border-bottom-style: none;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        border-left-style: none;
        border-left-width: 2px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 2px;
        border-right-color: #D3D3D3;
      }
      
      #ngabqinzwk .gt_sourcenote {
        font-size: 90%;
        padding-top: 4px;
        padding-bottom: 4px;
        padding-left: 5px;
        padding-right: 5px;
      }
      
      #ngabqinzwk .gt_left {
        text-align: left;
      }
      
      #ngabqinzwk .gt_center {
        text-align: center;
      }
      
      #ngabqinzwk .gt_right {
        text-align: right;
        font-variant-numeric: tabular-nums;
      }
      
      #ngabqinzwk .gt_font_normal {
        font-weight: normal;
      }
      
      #ngabqinzwk .gt_font_bold {
        font-weight: bold;
      }
      
      #ngabqinzwk .gt_font_italic {
        font-style: italic;
      }
      
      #ngabqinzwk .gt_super {
        font-size: 65%;
      }
      
      #ngabqinzwk .gt_footnote_marks {
        font-style: italic;
        font-weight: normal;
        font-size: 75%;
        vertical-align: 0.4em;
      }
      
      #ngabqinzwk .gt_asterisk {
        font-size: 100%;
        vertical-align: 0;
      }
      
      #ngabqinzwk .gt_indent_1 {
        text-indent: 5px;
      }
      
      #ngabqinzwk .gt_indent_2 {
        text-indent: 10px;
      }
      
      #ngabqinzwk .gt_indent_3 {
        text-indent: 15px;
      }
      
      #ngabqinzwk .gt_indent_4 {
        text-indent: 20px;
      }
      
      #ngabqinzwk .gt_indent_5 {
        text-indent: 25px;
      }
      </style>
        <table class="gt_table">
        
        <thead class="gt_col_headings">
          <tr>
            <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
            <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Drug A&lt;/strong&gt;, N = 98&lt;sup class=&quot;gt_footnote_marks&quot;&gt;1&lt;/sup&gt;"><strong>Drug A</strong>, N = 98<sup class="gt_footnote_marks">1</sup></th>
            <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Drug B&lt;/strong&gt;, N = 102&lt;sup class=&quot;gt_footnote_marks&quot;&gt;1&lt;/sup&gt;"><strong>Drug B</strong>, N = 102<sup class="gt_footnote_marks">1</sup></th>
          </tr>
        </thead>
        <tbody class="gt_table_body">
          <tr><td headers="label" class="gt_row gt_left">Age</td>
      <td headers="stat_1" class="gt_row gt_center">46 (37, 59)</td>
      <td headers="stat_2" class="gt_row gt_center">48 (39, 56)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
      <td headers="stat_1" class="gt_row gt_center">7</td>
      <td headers="stat_2" class="gt_row gt_center">4</td></tr>
          <tr><td headers="label" class="gt_row gt_left">Grade</td>
      <td headers="stat_1" class="gt_row gt_center"></td>
      <td headers="stat_2" class="gt_row gt_center"></td></tr>
          <tr><td headers="label" class="gt_row gt_left">    I</td>
      <td headers="stat_1" class="gt_row gt_center">35 (36%)</td>
      <td headers="stat_2" class="gt_row gt_center">33 (32%)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">    II</td>
      <td headers="stat_1" class="gt_row gt_center">32 (33%)</td>
      <td headers="stat_2" class="gt_row gt_center">36 (35%)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">    III</td>
      <td headers="stat_1" class="gt_row gt_center">31 (32%)</td>
      <td headers="stat_2" class="gt_row gt_center">33 (32%)</td></tr>
        </tbody>
        
        <tfoot class="gt_footnotes">
          <tr>
            <td class="gt_footnote" colspan="3"><sup class="gt_footnote_marks">1</sup> median (IQR) for continuous variables; n (%) categorical variables</td>
          </tr>
        </tfoot>
      </table>
      </div>

---

    Code
      tbl_summary %>% modify_footnote(label = "Variable Footnote", starts_with(
        "stat_") ~
        "median (IQR) for continuous variables; n (%) categorical variables")
    Output
      <div id="adfwukblbk" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
        <style>html {
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
      }
      
      #adfwukblbk .gt_table {
        display: table;
        border-collapse: collapse;
        margin-left: auto;
        margin-right: auto;
        color: #333333;
        font-size: 16px;
        font-weight: normal;
        font-style: normal;
        background-color: #FFFFFF;
        width: auto;
        border-top-style: solid;
        border-top-width: 2px;
        border-top-color: #A8A8A8;
        border-right-style: none;
        border-right-width: 2px;
        border-right-color: #D3D3D3;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #A8A8A8;
        border-left-style: none;
        border-left-width: 2px;
        border-left-color: #D3D3D3;
      }
      
      #adfwukblbk .gt_heading {
        background-color: #FFFFFF;
        text-align: center;
        border-bottom-color: #FFFFFF;
        border-left-style: none;
        border-left-width: 1px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 1px;
        border-right-color: #D3D3D3;
      }
      
      #adfwukblbk .gt_caption {
        padding-top: 4px;
        padding-bottom: 4px;
      }
      
      #adfwukblbk .gt_title {
        color: #333333;
        font-size: 125%;
        font-weight: initial;
        padding-top: 4px;
        padding-bottom: 4px;
        padding-left: 5px;
        padding-right: 5px;
        border-bottom-color: #FFFFFF;
        border-bottom-width: 0;
      }
      
      #adfwukblbk .gt_subtitle {
        color: #333333;
        font-size: 85%;
        font-weight: initial;
        padding-top: 0;
        padding-bottom: 6px;
        padding-left: 5px;
        padding-right: 5px;
        border-top-color: #FFFFFF;
        border-top-width: 0;
      }
      
      #adfwukblbk .gt_bottom_border {
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
      }
      
      #adfwukblbk .gt_col_headings {
        border-top-style: solid;
        border-top-width: 2px;
        border-top-color: #D3D3D3;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        border-left-style: none;
        border-left-width: 1px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 1px;
        border-right-color: #D3D3D3;
      }
      
      #adfwukblbk .gt_col_heading {
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: normal;
        text-transform: inherit;
        border-left-style: none;
        border-left-width: 1px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 1px;
        border-right-color: #D3D3D3;
        vertical-align: bottom;
        padding-top: 5px;
        padding-bottom: 6px;
        padding-left: 5px;
        padding-right: 5px;
        overflow-x: hidden;
      }
      
      #adfwukblbk .gt_column_spanner_outer {
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: normal;
        text-transform: inherit;
        padding-top: 0;
        padding-bottom: 0;
        padding-left: 4px;
        padding-right: 4px;
      }
      
      #adfwukblbk .gt_column_spanner_outer:first-child {
        padding-left: 0;
      }
      
      #adfwukblbk .gt_column_spanner_outer:last-child {
        padding-right: 0;
      }
      
      #adfwukblbk .gt_column_spanner {
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        vertical-align: bottom;
        padding-top: 5px;
        padding-bottom: 5px;
        overflow-x: hidden;
        display: inline-block;
        width: 100%;
      }
      
      #adfwukblbk .gt_group_heading {
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: initial;
        text-transform: inherit;
        border-top-style: solid;
        border-top-width: 2px;
        border-top-color: #D3D3D3;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        border-left-style: none;
        border-left-width: 1px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 1px;
        border-right-color: #D3D3D3;
        vertical-align: middle;
        text-align: left;
      }
      
      #adfwukblbk .gt_empty_group_heading {
        padding: 0.5px;
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: initial;
        border-top-style: solid;
        border-top-width: 2px;
        border-top-color: #D3D3D3;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        vertical-align: middle;
      }
      
      #adfwukblbk .gt_from_md > :first-child {
        margin-top: 0;
      }
      
      #adfwukblbk .gt_from_md > :last-child {
        margin-bottom: 0;
      }
      
      #adfwukblbk .gt_row {
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
        margin: 10px;
        border-top-style: solid;
        border-top-width: 1px;
        border-top-color: #D3D3D3;
        border-left-style: none;
        border-left-width: 1px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 1px;
        border-right-color: #D3D3D3;
        vertical-align: middle;
        overflow-x: hidden;
      }
      
      #adfwukblbk .gt_stub {
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: initial;
        text-transform: inherit;
        border-right-style: solid;
        border-right-width: 2px;
        border-right-color: #D3D3D3;
        padding-left: 5px;
        padding-right: 5px;
      }
      
      #adfwukblbk .gt_stub_row_group {
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: initial;
        text-transform: inherit;
        border-right-style: solid;
        border-right-width: 2px;
        border-right-color: #D3D3D3;
        padding-left: 5px;
        padding-right: 5px;
        vertical-align: top;
      }
      
      #adfwukblbk .gt_row_group_first td {
        border-top-width: 2px;
      }
      
      #adfwukblbk .gt_summary_row {
        color: #333333;
        background-color: #FFFFFF;
        text-transform: inherit;
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
      }
      
      #adfwukblbk .gt_first_summary_row {
        border-top-style: solid;
        border-top-color: #D3D3D3;
      }
      
      #adfwukblbk .gt_first_summary_row.thick {
        border-top-width: 2px;
      }
      
      #adfwukblbk .gt_last_summary_row {
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
      }
      
      #adfwukblbk .gt_grand_summary_row {
        color: #333333;
        background-color: #FFFFFF;
        text-transform: inherit;
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
      }
      
      #adfwukblbk .gt_first_grand_summary_row {
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
        border-top-style: double;
        border-top-width: 6px;
        border-top-color: #D3D3D3;
      }
      
      #adfwukblbk .gt_striped {
        background-color: rgba(128, 128, 128, 0.05);
      }
      
      #adfwukblbk .gt_table_body {
        border-top-style: solid;
        border-top-width: 2px;
        border-top-color: #D3D3D3;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
      }
      
      #adfwukblbk .gt_footnotes {
        color: #333333;
        background-color: #FFFFFF;
        border-bottom-style: none;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        border-left-style: none;
        border-left-width: 2px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 2px;
        border-right-color: #D3D3D3;
      }
      
      #adfwukblbk .gt_footnote {
        margin: 0px;
        font-size: 90%;
        padding-left: 4px;
        padding-right: 4px;
        padding-left: 5px;
        padding-right: 5px;
      }
      
      #adfwukblbk .gt_sourcenotes {
        color: #333333;
        background-color: #FFFFFF;
        border-bottom-style: none;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        border-left-style: none;
        border-left-width: 2px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 2px;
        border-right-color: #D3D3D3;
      }
      
      #adfwukblbk .gt_sourcenote {
        font-size: 90%;
        padding-top: 4px;
        padding-bottom: 4px;
        padding-left: 5px;
        padding-right: 5px;
      }
      
      #adfwukblbk .gt_left {
        text-align: left;
      }
      
      #adfwukblbk .gt_center {
        text-align: center;
      }
      
      #adfwukblbk .gt_right {
        text-align: right;
        font-variant-numeric: tabular-nums;
      }
      
      #adfwukblbk .gt_font_normal {
        font-weight: normal;
      }
      
      #adfwukblbk .gt_font_bold {
        font-weight: bold;
      }
      
      #adfwukblbk .gt_font_italic {
        font-style: italic;
      }
      
      #adfwukblbk .gt_super {
        font-size: 65%;
      }
      
      #adfwukblbk .gt_footnote_marks {
        font-style: italic;
        font-weight: normal;
        font-size: 75%;
        vertical-align: 0.4em;
      }
      
      #adfwukblbk .gt_asterisk {
        font-size: 100%;
        vertical-align: 0;
      }
      
      #adfwukblbk .gt_indent_1 {
        text-indent: 5px;
      }
      
      #adfwukblbk .gt_indent_2 {
        text-indent: 10px;
      }
      
      #adfwukblbk .gt_indent_3 {
        text-indent: 15px;
      }
      
      #adfwukblbk .gt_indent_4 {
        text-indent: 20px;
      }
      
      #adfwukblbk .gt_indent_5 {
        text-indent: 25px;
      }
      </style>
        <table class="gt_table">
        
        <thead class="gt_col_headings">
          <tr>
            <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;&lt;sup class=&quot;gt_footnote_marks&quot;&gt;1&lt;/sup&gt;"><strong>Characteristic</strong><sup class="gt_footnote_marks">1</sup></th>
            <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Drug A&lt;/strong&gt;, N = 98&lt;sup class=&quot;gt_footnote_marks&quot;&gt;2&lt;/sup&gt;"><strong>Drug A</strong>, N = 98<sup class="gt_footnote_marks">2</sup></th>
            <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Drug B&lt;/strong&gt;, N = 102&lt;sup class=&quot;gt_footnote_marks&quot;&gt;2&lt;/sup&gt;"><strong>Drug B</strong>, N = 102<sup class="gt_footnote_marks">2</sup></th>
          </tr>
        </thead>
        <tbody class="gt_table_body">
          <tr><td headers="label" class="gt_row gt_left">Age</td>
      <td headers="stat_1" class="gt_row gt_center">46 (37, 59)</td>
      <td headers="stat_2" class="gt_row gt_center">48 (39, 56)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
      <td headers="stat_1" class="gt_row gt_center">7</td>
      <td headers="stat_2" class="gt_row gt_center">4</td></tr>
          <tr><td headers="label" class="gt_row gt_left">Grade</td>
      <td headers="stat_1" class="gt_row gt_center"></td>
      <td headers="stat_2" class="gt_row gt_center"></td></tr>
          <tr><td headers="label" class="gt_row gt_left">    I</td>
      <td headers="stat_1" class="gt_row gt_center">35 (36%)</td>
      <td headers="stat_2" class="gt_row gt_center">33 (32%)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">    II</td>
      <td headers="stat_1" class="gt_row gt_center">32 (33%)</td>
      <td headers="stat_2" class="gt_row gt_center">36 (35%)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">    III</td>
      <td headers="stat_1" class="gt_row gt_center">31 (32%)</td>
      <td headers="stat_2" class="gt_row gt_center">33 (32%)</td></tr>
        </tbody>
        
        <tfoot class="gt_footnotes">
          <tr>
            <td class="gt_footnote" colspan="3"><sup class="gt_footnote_marks">1</sup> Variable Footnote</td>
          </tr>
          <tr>
            <td class="gt_footnote" colspan="3"><sup class="gt_footnote_marks">2</sup> median (IQR) for continuous variables; n (%) categorical variables</td>
          </tr>
        </tfoot>
      </table>
      </div>

---

    Code
      tbl_summary %>% modify_footnote(update = everything() ~ NA)
    Output
      <div id="aindgaweuz" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
        <style>html {
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
      }
      
      #aindgaweuz .gt_table {
        display: table;
        border-collapse: collapse;
        margin-left: auto;
        margin-right: auto;
        color: #333333;
        font-size: 16px;
        font-weight: normal;
        font-style: normal;
        background-color: #FFFFFF;
        width: auto;
        border-top-style: solid;
        border-top-width: 2px;
        border-top-color: #A8A8A8;
        border-right-style: none;
        border-right-width: 2px;
        border-right-color: #D3D3D3;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #A8A8A8;
        border-left-style: none;
        border-left-width: 2px;
        border-left-color: #D3D3D3;
      }
      
      #aindgaweuz .gt_heading {
        background-color: #FFFFFF;
        text-align: center;
        border-bottom-color: #FFFFFF;
        border-left-style: none;
        border-left-width: 1px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 1px;
        border-right-color: #D3D3D3;
      }
      
      #aindgaweuz .gt_caption {
        padding-top: 4px;
        padding-bottom: 4px;
      }
      
      #aindgaweuz .gt_title {
        color: #333333;
        font-size: 125%;
        font-weight: initial;
        padding-top: 4px;
        padding-bottom: 4px;
        padding-left: 5px;
        padding-right: 5px;
        border-bottom-color: #FFFFFF;
        border-bottom-width: 0;
      }
      
      #aindgaweuz .gt_subtitle {
        color: #333333;
        font-size: 85%;
        font-weight: initial;
        padding-top: 0;
        padding-bottom: 6px;
        padding-left: 5px;
        padding-right: 5px;
        border-top-color: #FFFFFF;
        border-top-width: 0;
      }
      
      #aindgaweuz .gt_bottom_border {
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
      }
      
      #aindgaweuz .gt_col_headings {
        border-top-style: solid;
        border-top-width: 2px;
        border-top-color: #D3D3D3;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        border-left-style: none;
        border-left-width: 1px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 1px;
        border-right-color: #D3D3D3;
      }
      
      #aindgaweuz .gt_col_heading {
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: normal;
        text-transform: inherit;
        border-left-style: none;
        border-left-width: 1px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 1px;
        border-right-color: #D3D3D3;
        vertical-align: bottom;
        padding-top: 5px;
        padding-bottom: 6px;
        padding-left: 5px;
        padding-right: 5px;
        overflow-x: hidden;
      }
      
      #aindgaweuz .gt_column_spanner_outer {
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: normal;
        text-transform: inherit;
        padding-top: 0;
        padding-bottom: 0;
        padding-left: 4px;
        padding-right: 4px;
      }
      
      #aindgaweuz .gt_column_spanner_outer:first-child {
        padding-left: 0;
      }
      
      #aindgaweuz .gt_column_spanner_outer:last-child {
        padding-right: 0;
      }
      
      #aindgaweuz .gt_column_spanner {
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        vertical-align: bottom;
        padding-top: 5px;
        padding-bottom: 5px;
        overflow-x: hidden;
        display: inline-block;
        width: 100%;
      }
      
      #aindgaweuz .gt_group_heading {
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: initial;
        text-transform: inherit;
        border-top-style: solid;
        border-top-width: 2px;
        border-top-color: #D3D3D3;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        border-left-style: none;
        border-left-width: 1px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 1px;
        border-right-color: #D3D3D3;
        vertical-align: middle;
        text-align: left;
      }
      
      #aindgaweuz .gt_empty_group_heading {
        padding: 0.5px;
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: initial;
        border-top-style: solid;
        border-top-width: 2px;
        border-top-color: #D3D3D3;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        vertical-align: middle;
      }
      
      #aindgaweuz .gt_from_md > :first-child {
        margin-top: 0;
      }
      
      #aindgaweuz .gt_from_md > :last-child {
        margin-bottom: 0;
      }
      
      #aindgaweuz .gt_row {
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
        margin: 10px;
        border-top-style: solid;
        border-top-width: 1px;
        border-top-color: #D3D3D3;
        border-left-style: none;
        border-left-width: 1px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 1px;
        border-right-color: #D3D3D3;
        vertical-align: middle;
        overflow-x: hidden;
      }
      
      #aindgaweuz .gt_stub {
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: initial;
        text-transform: inherit;
        border-right-style: solid;
        border-right-width: 2px;
        border-right-color: #D3D3D3;
        padding-left: 5px;
        padding-right: 5px;
      }
      
      #aindgaweuz .gt_stub_row_group {
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: initial;
        text-transform: inherit;
        border-right-style: solid;
        border-right-width: 2px;
        border-right-color: #D3D3D3;
        padding-left: 5px;
        padding-right: 5px;
        vertical-align: top;
      }
      
      #aindgaweuz .gt_row_group_first td {
        border-top-width: 2px;
      }
      
      #aindgaweuz .gt_summary_row {
        color: #333333;
        background-color: #FFFFFF;
        text-transform: inherit;
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
      }
      
      #aindgaweuz .gt_first_summary_row {
        border-top-style: solid;
        border-top-color: #D3D3D3;
      }
      
      #aindgaweuz .gt_first_summary_row.thick {
        border-top-width: 2px;
      }
      
      #aindgaweuz .gt_last_summary_row {
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
      }
      
      #aindgaweuz .gt_grand_summary_row {
        color: #333333;
        background-color: #FFFFFF;
        text-transform: inherit;
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
      }
      
      #aindgaweuz .gt_first_grand_summary_row {
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
        border-top-style: double;
        border-top-width: 6px;
        border-top-color: #D3D3D3;
      }
      
      #aindgaweuz .gt_striped {
        background-color: rgba(128, 128, 128, 0.05);
      }
      
      #aindgaweuz .gt_table_body {
        border-top-style: solid;
        border-top-width: 2px;
        border-top-color: #D3D3D3;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
      }
      
      #aindgaweuz .gt_footnotes {
        color: #333333;
        background-color: #FFFFFF;
        border-bottom-style: none;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        border-left-style: none;
        border-left-width: 2px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 2px;
        border-right-color: #D3D3D3;
      }
      
      #aindgaweuz .gt_footnote {
        margin: 0px;
        font-size: 90%;
        padding-left: 4px;
        padding-right: 4px;
        padding-left: 5px;
        padding-right: 5px;
      }
      
      #aindgaweuz .gt_sourcenotes {
        color: #333333;
        background-color: #FFFFFF;
        border-bottom-style: none;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        border-left-style: none;
        border-left-width: 2px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 2px;
        border-right-color: #D3D3D3;
      }
      
      #aindgaweuz .gt_sourcenote {
        font-size: 90%;
        padding-top: 4px;
        padding-bottom: 4px;
        padding-left: 5px;
        padding-right: 5px;
      }
      
      #aindgaweuz .gt_left {
        text-align: left;
      }
      
      #aindgaweuz .gt_center {
        text-align: center;
      }
      
      #aindgaweuz .gt_right {
        text-align: right;
        font-variant-numeric: tabular-nums;
      }
      
      #aindgaweuz .gt_font_normal {
        font-weight: normal;
      }
      
      #aindgaweuz .gt_font_bold {
        font-weight: bold;
      }
      
      #aindgaweuz .gt_font_italic {
        font-style: italic;
      }
      
      #aindgaweuz .gt_super {
        font-size: 65%;
      }
      
      #aindgaweuz .gt_footnote_marks {
        font-style: italic;
        font-weight: normal;
        font-size: 75%;
        vertical-align: 0.4em;
      }
      
      #aindgaweuz .gt_asterisk {
        font-size: 100%;
        vertical-align: 0;
      }
      
      #aindgaweuz .gt_indent_1 {
        text-indent: 5px;
      }
      
      #aindgaweuz .gt_indent_2 {
        text-indent: 10px;
      }
      
      #aindgaweuz .gt_indent_3 {
        text-indent: 15px;
      }
      
      #aindgaweuz .gt_indent_4 {
        text-indent: 20px;
      }
      
      #aindgaweuz .gt_indent_5 {
        text-indent: 25px;
      }
      </style>
        <table class="gt_table">
        
        <thead class="gt_col_headings">
          <tr>
            <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
            <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Drug A&lt;/strong&gt;, N = 98"><strong>Drug A</strong>, N = 98</th>
            <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Drug B&lt;/strong&gt;, N = 102"><strong>Drug B</strong>, N = 102</th>
          </tr>
        </thead>
        <tbody class="gt_table_body">
          <tr><td headers="label" class="gt_row gt_left">Age</td>
      <td headers="stat_1" class="gt_row gt_center">46 (37, 59)</td>
      <td headers="stat_2" class="gt_row gt_center">48 (39, 56)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
      <td headers="stat_1" class="gt_row gt_center">7</td>
      <td headers="stat_2" class="gt_row gt_center">4</td></tr>
          <tr><td headers="label" class="gt_row gt_left">Grade</td>
      <td headers="stat_1" class="gt_row gt_center"></td>
      <td headers="stat_2" class="gt_row gt_center"></td></tr>
          <tr><td headers="label" class="gt_row gt_left">    I</td>
      <td headers="stat_1" class="gt_row gt_center">35 (36%)</td>
      <td headers="stat_2" class="gt_row gt_center">33 (32%)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">    II</td>
      <td headers="stat_1" class="gt_row gt_center">32 (33%)</td>
      <td headers="stat_2" class="gt_row gt_center">36 (35%)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">    III</td>
      <td headers="stat_1" class="gt_row gt_center">31 (32%)</td>
      <td headers="stat_2" class="gt_row gt_center">33 (32%)</td></tr>
        </tbody>
        
        
      </table>
      </div>

---

    Code
      tbl_summary %>% modify_footnote(update = NULL)
    Message
      i No columns were selected. Use `quiet = TRUE` to supress these messages.
    Output
      
    Message
      i As a usage guide, the code below re-creates the current column headers.
      modify_header(
        label = "**Characteristic**",
        stat_1 = "**Drug A**, N = 98",
        stat_2 = "**Drug B**, N = 102"
      )
    Output
      
      
      Column Name   Column Header       
      ------------  --------------------
      label         **Characteristic**  
      stat_1        **Drug A**, N = 98  
      stat_2        **Drug B**, N = 102 
      <div id="madgbowhvu" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
        <style>html {
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
      }
      
      #madgbowhvu .gt_table {
        display: table;
        border-collapse: collapse;
        margin-left: auto;
        margin-right: auto;
        color: #333333;
        font-size: 16px;
        font-weight: normal;
        font-style: normal;
        background-color: #FFFFFF;
        width: auto;
        border-top-style: solid;
        border-top-width: 2px;
        border-top-color: #A8A8A8;
        border-right-style: none;
        border-right-width: 2px;
        border-right-color: #D3D3D3;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #A8A8A8;
        border-left-style: none;
        border-left-width: 2px;
        border-left-color: #D3D3D3;
      }
      
      #madgbowhvu .gt_heading {
        background-color: #FFFFFF;
        text-align: center;
        border-bottom-color: #FFFFFF;
        border-left-style: none;
        border-left-width: 1px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 1px;
        border-right-color: #D3D3D3;
      }
      
      #madgbowhvu .gt_caption {
        padding-top: 4px;
        padding-bottom: 4px;
      }
      
      #madgbowhvu .gt_title {
        color: #333333;
        font-size: 125%;
        font-weight: initial;
        padding-top: 4px;
        padding-bottom: 4px;
        padding-left: 5px;
        padding-right: 5px;
        border-bottom-color: #FFFFFF;
        border-bottom-width: 0;
      }
      
      #madgbowhvu .gt_subtitle {
        color: #333333;
        font-size: 85%;
        font-weight: initial;
        padding-top: 0;
        padding-bottom: 6px;
        padding-left: 5px;
        padding-right: 5px;
        border-top-color: #FFFFFF;
        border-top-width: 0;
      }
      
      #madgbowhvu .gt_bottom_border {
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
      }
      
      #madgbowhvu .gt_col_headings {
        border-top-style: solid;
        border-top-width: 2px;
        border-top-color: #D3D3D3;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        border-left-style: none;
        border-left-width: 1px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 1px;
        border-right-color: #D3D3D3;
      }
      
      #madgbowhvu .gt_col_heading {
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: normal;
        text-transform: inherit;
        border-left-style: none;
        border-left-width: 1px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 1px;
        border-right-color: #D3D3D3;
        vertical-align: bottom;
        padding-top: 5px;
        padding-bottom: 6px;
        padding-left: 5px;
        padding-right: 5px;
        overflow-x: hidden;
      }
      
      #madgbowhvu .gt_column_spanner_outer {
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: normal;
        text-transform: inherit;
        padding-top: 0;
        padding-bottom: 0;
        padding-left: 4px;
        padding-right: 4px;
      }
      
      #madgbowhvu .gt_column_spanner_outer:first-child {
        padding-left: 0;
      }
      
      #madgbowhvu .gt_column_spanner_outer:last-child {
        padding-right: 0;
      }
      
      #madgbowhvu .gt_column_spanner {
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        vertical-align: bottom;
        padding-top: 5px;
        padding-bottom: 5px;
        overflow-x: hidden;
        display: inline-block;
        width: 100%;
      }
      
      #madgbowhvu .gt_group_heading {
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: initial;
        text-transform: inherit;
        border-top-style: solid;
        border-top-width: 2px;
        border-top-color: #D3D3D3;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        border-left-style: none;
        border-left-width: 1px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 1px;
        border-right-color: #D3D3D3;
        vertical-align: middle;
        text-align: left;
      }
      
      #madgbowhvu .gt_empty_group_heading {
        padding: 0.5px;
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: initial;
        border-top-style: solid;
        border-top-width: 2px;
        border-top-color: #D3D3D3;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        vertical-align: middle;
      }
      
      #madgbowhvu .gt_from_md > :first-child {
        margin-top: 0;
      }
      
      #madgbowhvu .gt_from_md > :last-child {
        margin-bottom: 0;
      }
      
      #madgbowhvu .gt_row {
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
        margin: 10px;
        border-top-style: solid;
        border-top-width: 1px;
        border-top-color: #D3D3D3;
        border-left-style: none;
        border-left-width: 1px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 1px;
        border-right-color: #D3D3D3;
        vertical-align: middle;
        overflow-x: hidden;
      }
      
      #madgbowhvu .gt_stub {
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: initial;
        text-transform: inherit;
        border-right-style: solid;
        border-right-width: 2px;
        border-right-color: #D3D3D3;
        padding-left: 5px;
        padding-right: 5px;
      }
      
      #madgbowhvu .gt_stub_row_group {
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: initial;
        text-transform: inherit;
        border-right-style: solid;
        border-right-width: 2px;
        border-right-color: #D3D3D3;
        padding-left: 5px;
        padding-right: 5px;
        vertical-align: top;
      }
      
      #madgbowhvu .gt_row_group_first td {
        border-top-width: 2px;
      }
      
      #madgbowhvu .gt_summary_row {
        color: #333333;
        background-color: #FFFFFF;
        text-transform: inherit;
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
      }
      
      #madgbowhvu .gt_first_summary_row {
        border-top-style: solid;
        border-top-color: #D3D3D3;
      }
      
      #madgbowhvu .gt_first_summary_row.thick {
        border-top-width: 2px;
      }
      
      #madgbowhvu .gt_last_summary_row {
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
      }
      
      #madgbowhvu .gt_grand_summary_row {
        color: #333333;
        background-color: #FFFFFF;
        text-transform: inherit;
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
      }
      
      #madgbowhvu .gt_first_grand_summary_row {
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
        border-top-style: double;
        border-top-width: 6px;
        border-top-color: #D3D3D3;
      }
      
      #madgbowhvu .gt_striped {
        background-color: rgba(128, 128, 128, 0.05);
      }
      
      #madgbowhvu .gt_table_body {
        border-top-style: solid;
        border-top-width: 2px;
        border-top-color: #D3D3D3;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
      }
      
      #madgbowhvu .gt_footnotes {
        color: #333333;
        background-color: #FFFFFF;
        border-bottom-style: none;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        border-left-style: none;
        border-left-width: 2px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 2px;
        border-right-color: #D3D3D3;
      }
      
      #madgbowhvu .gt_footnote {
        margin: 0px;
        font-size: 90%;
        padding-left: 4px;
        padding-right: 4px;
        padding-left: 5px;
        padding-right: 5px;
      }
      
      #madgbowhvu .gt_sourcenotes {
        color: #333333;
        background-color: #FFFFFF;
        border-bottom-style: none;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        border-left-style: none;
        border-left-width: 2px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 2px;
        border-right-color: #D3D3D3;
      }
      
      #madgbowhvu .gt_sourcenote {
        font-size: 90%;
        padding-top: 4px;
        padding-bottom: 4px;
        padding-left: 5px;
        padding-right: 5px;
      }
      
      #madgbowhvu .gt_left {
        text-align: left;
      }
      
      #madgbowhvu .gt_center {
        text-align: center;
      }
      
      #madgbowhvu .gt_right {
        text-align: right;
        font-variant-numeric: tabular-nums;
      }
      
      #madgbowhvu .gt_font_normal {
        font-weight: normal;
      }
      
      #madgbowhvu .gt_font_bold {
        font-weight: bold;
      }
      
      #madgbowhvu .gt_font_italic {
        font-style: italic;
      }
      
      #madgbowhvu .gt_super {
        font-size: 65%;
      }
      
      #madgbowhvu .gt_footnote_marks {
        font-style: italic;
        font-weight: normal;
        font-size: 75%;
        vertical-align: 0.4em;
      }
      
      #madgbowhvu .gt_asterisk {
        font-size: 100%;
        vertical-align: 0;
      }
      
      #madgbowhvu .gt_indent_1 {
        text-indent: 5px;
      }
      
      #madgbowhvu .gt_indent_2 {
        text-indent: 10px;
      }
      
      #madgbowhvu .gt_indent_3 {
        text-indent: 15px;
      }
      
      #madgbowhvu .gt_indent_4 {
        text-indent: 20px;
      }
      
      #madgbowhvu .gt_indent_5 {
        text-indent: 25px;
      }
      </style>
        <table class="gt_table">
        
        <thead class="gt_col_headings">
          <tr>
            <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
            <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Drug A&lt;/strong&gt;, N = 98&lt;sup class=&quot;gt_footnote_marks&quot;&gt;1&lt;/sup&gt;"><strong>Drug A</strong>, N = 98<sup class="gt_footnote_marks">1</sup></th>
            <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Drug B&lt;/strong&gt;, N = 102&lt;sup class=&quot;gt_footnote_marks&quot;&gt;1&lt;/sup&gt;"><strong>Drug B</strong>, N = 102<sup class="gt_footnote_marks">1</sup></th>
          </tr>
        </thead>
        <tbody class="gt_table_body">
          <tr><td headers="label" class="gt_row gt_left">Age</td>
      <td headers="stat_1" class="gt_row gt_center">46 (37, 59)</td>
      <td headers="stat_2" class="gt_row gt_center">48 (39, 56)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
      <td headers="stat_1" class="gt_row gt_center">7</td>
      <td headers="stat_2" class="gt_row gt_center">4</td></tr>
          <tr><td headers="label" class="gt_row gt_left">Grade</td>
      <td headers="stat_1" class="gt_row gt_center"></td>
      <td headers="stat_2" class="gt_row gt_center"></td></tr>
          <tr><td headers="label" class="gt_row gt_left">    I</td>
      <td headers="stat_1" class="gt_row gt_center">35 (36%)</td>
      <td headers="stat_2" class="gt_row gt_center">33 (32%)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">    II</td>
      <td headers="stat_1" class="gt_row gt_center">32 (33%)</td>
      <td headers="stat_2" class="gt_row gt_center">36 (35%)</td></tr>
          <tr><td headers="label" class="gt_row gt_left">    III</td>
      <td headers="stat_1" class="gt_row gt_center">31 (32%)</td>
      <td headers="stat_2" class="gt_row gt_center">33 (32%)</td></tr>
        </tbody>
        
        <tfoot class="gt_footnotes">
          <tr>
            <td class="gt_footnote" colspan="3"><sup class="gt_footnote_marks">1</sup> Median (IQR); n (%)</td>
          </tr>
        </tfoot>
      </table>
      </div>

---

    Code
      glm(response ~ age + grade, trial, family = binomial) %>% tbl_regression(
        exponentiate = TRUE) %>% modify_footnote(ci ~ "CI = Credible Interval",
      abbreviation = TRUE)
    Output
      <div id="qygqbbbrwj" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
        <style>html {
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
      }
      
      #qygqbbbrwj .gt_table {
        display: table;
        border-collapse: collapse;
        margin-left: auto;
        margin-right: auto;
        color: #333333;
        font-size: 16px;
        font-weight: normal;
        font-style: normal;
        background-color: #FFFFFF;
        width: auto;
        border-top-style: solid;
        border-top-width: 2px;
        border-top-color: #A8A8A8;
        border-right-style: none;
        border-right-width: 2px;
        border-right-color: #D3D3D3;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #A8A8A8;
        border-left-style: none;
        border-left-width: 2px;
        border-left-color: #D3D3D3;
      }
      
      #qygqbbbrwj .gt_heading {
        background-color: #FFFFFF;
        text-align: center;
        border-bottom-color: #FFFFFF;
        border-left-style: none;
        border-left-width: 1px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 1px;
        border-right-color: #D3D3D3;
      }
      
      #qygqbbbrwj .gt_caption {
        padding-top: 4px;
        padding-bottom: 4px;
      }
      
      #qygqbbbrwj .gt_title {
        color: #333333;
        font-size: 125%;
        font-weight: initial;
        padding-top: 4px;
        padding-bottom: 4px;
        padding-left: 5px;
        padding-right: 5px;
        border-bottom-color: #FFFFFF;
        border-bottom-width: 0;
      }
      
      #qygqbbbrwj .gt_subtitle {
        color: #333333;
        font-size: 85%;
        font-weight: initial;
        padding-top: 0;
        padding-bottom: 6px;
        padding-left: 5px;
        padding-right: 5px;
        border-top-color: #FFFFFF;
        border-top-width: 0;
      }
      
      #qygqbbbrwj .gt_bottom_border {
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
      }
      
      #qygqbbbrwj .gt_col_headings {
        border-top-style: solid;
        border-top-width: 2px;
        border-top-color: #D3D3D3;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        border-left-style: none;
        border-left-width: 1px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 1px;
        border-right-color: #D3D3D3;
      }
      
      #qygqbbbrwj .gt_col_heading {
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: normal;
        text-transform: inherit;
        border-left-style: none;
        border-left-width: 1px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 1px;
        border-right-color: #D3D3D3;
        vertical-align: bottom;
        padding-top: 5px;
        padding-bottom: 6px;
        padding-left: 5px;
        padding-right: 5px;
        overflow-x: hidden;
      }
      
      #qygqbbbrwj .gt_column_spanner_outer {
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: normal;
        text-transform: inherit;
        padding-top: 0;
        padding-bottom: 0;
        padding-left: 4px;
        padding-right: 4px;
      }
      
      #qygqbbbrwj .gt_column_spanner_outer:first-child {
        padding-left: 0;
      }
      
      #qygqbbbrwj .gt_column_spanner_outer:last-child {
        padding-right: 0;
      }
      
      #qygqbbbrwj .gt_column_spanner {
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        vertical-align: bottom;
        padding-top: 5px;
        padding-bottom: 5px;
        overflow-x: hidden;
        display: inline-block;
        width: 100%;
      }
      
      #qygqbbbrwj .gt_group_heading {
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: initial;
        text-transform: inherit;
        border-top-style: solid;
        border-top-width: 2px;
        border-top-color: #D3D3D3;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        border-left-style: none;
        border-left-width: 1px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 1px;
        border-right-color: #D3D3D3;
        vertical-align: middle;
        text-align: left;
      }
      
      #qygqbbbrwj .gt_empty_group_heading {
        padding: 0.5px;
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: initial;
        border-top-style: solid;
        border-top-width: 2px;
        border-top-color: #D3D3D3;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        vertical-align: middle;
      }
      
      #qygqbbbrwj .gt_from_md > :first-child {
        margin-top: 0;
      }
      
      #qygqbbbrwj .gt_from_md > :last-child {
        margin-bottom: 0;
      }
      
      #qygqbbbrwj .gt_row {
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
        margin: 10px;
        border-top-style: solid;
        border-top-width: 1px;
        border-top-color: #D3D3D3;
        border-left-style: none;
        border-left-width: 1px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 1px;
        border-right-color: #D3D3D3;
        vertical-align: middle;
        overflow-x: hidden;
      }
      
      #qygqbbbrwj .gt_stub {
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: initial;
        text-transform: inherit;
        border-right-style: solid;
        border-right-width: 2px;
        border-right-color: #D3D3D3;
        padding-left: 5px;
        padding-right: 5px;
      }
      
      #qygqbbbrwj .gt_stub_row_group {
        color: #333333;
        background-color: #FFFFFF;
        font-size: 100%;
        font-weight: initial;
        text-transform: inherit;
        border-right-style: solid;
        border-right-width: 2px;
        border-right-color: #D3D3D3;
        padding-left: 5px;
        padding-right: 5px;
        vertical-align: top;
      }
      
      #qygqbbbrwj .gt_row_group_first td {
        border-top-width: 2px;
      }
      
      #qygqbbbrwj .gt_summary_row {
        color: #333333;
        background-color: #FFFFFF;
        text-transform: inherit;
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
      }
      
      #qygqbbbrwj .gt_first_summary_row {
        border-top-style: solid;
        border-top-color: #D3D3D3;
      }
      
      #qygqbbbrwj .gt_first_summary_row.thick {
        border-top-width: 2px;
      }
      
      #qygqbbbrwj .gt_last_summary_row {
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
      }
      
      #qygqbbbrwj .gt_grand_summary_row {
        color: #333333;
        background-color: #FFFFFF;
        text-transform: inherit;
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
      }
      
      #qygqbbbrwj .gt_first_grand_summary_row {
        padding-top: 8px;
        padding-bottom: 8px;
        padding-left: 5px;
        padding-right: 5px;
        border-top-style: double;
        border-top-width: 6px;
        border-top-color: #D3D3D3;
      }
      
      #qygqbbbrwj .gt_striped {
        background-color: rgba(128, 128, 128, 0.05);
      }
      
      #qygqbbbrwj .gt_table_body {
        border-top-style: solid;
        border-top-width: 2px;
        border-top-color: #D3D3D3;
        border-bottom-style: solid;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
      }
      
      #qygqbbbrwj .gt_footnotes {
        color: #333333;
        background-color: #FFFFFF;
        border-bottom-style: none;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        border-left-style: none;
        border-left-width: 2px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 2px;
        border-right-color: #D3D3D3;
      }
      
      #qygqbbbrwj .gt_footnote {
        margin: 0px;
        font-size: 90%;
        padding-left: 4px;
        padding-right: 4px;
        padding-left: 5px;
        padding-right: 5px;
      }
      
      #qygqbbbrwj .gt_sourcenotes {
        color: #333333;
        background-color: #FFFFFF;
        border-bottom-style: none;
        border-bottom-width: 2px;
        border-bottom-color: #D3D3D3;
        border-left-style: none;
        border-left-width: 2px;
        border-left-color: #D3D3D3;
        border-right-style: none;
        border-right-width: 2px;
        border-right-color: #D3D3D3;
      }
      
      #qygqbbbrwj .gt_sourcenote {
        font-size: 90%;
        padding-top: 4px;
        padding-bottom: 4px;
        padding-left: 5px;
        padding-right: 5px;
      }
      
      #qygqbbbrwj .gt_left {
        text-align: left;
      }
      
      #qygqbbbrwj .gt_center {
        text-align: center;
      }
      
      #qygqbbbrwj .gt_right {
        text-align: right;
        font-variant-numeric: tabular-nums;
      }
      
      #qygqbbbrwj .gt_font_normal {
        font-weight: normal;
      }
      
      #qygqbbbrwj .gt_font_bold {
        font-weight: bold;
      }
      
      #qygqbbbrwj .gt_font_italic {
        font-style: italic;
      }
      
      #qygqbbbrwj .gt_super {
        font-size: 65%;
      }
      
      #qygqbbbrwj .gt_footnote_marks {
        font-style: italic;
        font-weight: normal;
        font-size: 75%;
        vertical-align: 0.4em;
      }
      
      #qygqbbbrwj .gt_asterisk {
        font-size: 100%;
        vertical-align: 0;
      }
      
      #qygqbbbrwj .gt_indent_1 {
        text-indent: 5px;
      }
      
      #qygqbbbrwj .gt_indent_2 {
        text-indent: 10px;
      }
      
      #qygqbbbrwj .gt_indent_3 {
        text-indent: 15px;
      }
      
      #qygqbbbrwj .gt_indent_4 {
        text-indent: 20px;
      }
      
      #qygqbbbrwj .gt_indent_5 {
        text-indent: 25px;
      }
      </style>
        <table class="gt_table">
        
        <thead class="gt_col_headings">
          <tr>
            <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
            <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;OR&lt;/strong&gt;&lt;sup class=&quot;gt_footnote_marks&quot;&gt;1&lt;/sup&gt;"><strong>OR</strong><sup class="gt_footnote_marks">1</sup></th>
            <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;95% CI&lt;/strong&gt;&lt;sup class=&quot;gt_footnote_marks&quot;&gt;1&lt;/sup&gt;"><strong>95% CI</strong><sup class="gt_footnote_marks">1</sup></th>
            <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;"><strong>p-value</strong></th>
          </tr>
        </thead>
        <tbody class="gt_table_body">
          <tr><td headers="label" class="gt_row gt_left">Age</td>
      <td headers="estimate" class="gt_row gt_center">1.02</td>
      <td headers="ci" class="gt_row gt_center">1.00, 1.04</td>
      <td headers="p.value" class="gt_row gt_center">0.10</td></tr>
          <tr><td headers="label" class="gt_row gt_left">Grade</td>
      <td headers="estimate" class="gt_row gt_center"></td>
      <td headers="ci" class="gt_row gt_center"></td>
      <td headers="p.value" class="gt_row gt_center"></td></tr>
          <tr><td headers="label" class="gt_row gt_left">    I</td>
      <td headers="estimate" class="gt_row gt_center">—</td>
      <td headers="ci" class="gt_row gt_center">—</td>
      <td headers="p.value" class="gt_row gt_center"></td></tr>
          <tr><td headers="label" class="gt_row gt_left">    II</td>
      <td headers="estimate" class="gt_row gt_center">0.85</td>
      <td headers="ci" class="gt_row gt_center">0.39, 1.85</td>
      <td headers="p.value" class="gt_row gt_center">0.7</td></tr>
          <tr><td headers="label" class="gt_row gt_left">    III</td>
      <td headers="estimate" class="gt_row gt_center">1.01</td>
      <td headers="ci" class="gt_row gt_center">0.47, 2.16</td>
      <td headers="p.value" class="gt_row gt_center">>0.9</td></tr>
        </tbody>
        
        <tfoot class="gt_footnotes">
          <tr>
            <td class="gt_footnote" colspan="4"><sup class="gt_footnote_marks">1</sup> OR = Odds Ratio, CI = Credible Interval</td>
          </tr>
        </tfoot>
      </table>
      </div>

