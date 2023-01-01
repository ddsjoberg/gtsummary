# input checks

    Code
      lm(age ~ marker + grade, trial) %>% tbl_regression() %>% modify_column_hide(
        column = ci) %>% modify_column_unhide(column = std.error)
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
            <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Beta&lt;/strong&gt;"><strong>Beta</strong></th>
            <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;SE&lt;/strong&gt;&lt;sup class=&quot;gt_footnote_marks&quot;&gt;1&lt;/sup&gt;"><strong>SE</strong><sup class="gt_footnote_marks">1</sup></th>
            <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;"><strong>p-value</strong></th>
          </tr>
        </thead>
        <tbody class="gt_table_body">
          <tr><td headers="label" class="gt_row gt_left">Marker Level (ng/mL)</td>
      <td headers="estimate" class="gt_row gt_center">-0.04</td>
      <td headers="std.error" class="gt_row gt_center">1.28</td>
      <td headers="p.value" class="gt_row gt_center">>0.9</td></tr>
          <tr><td headers="label" class="gt_row gt_left">Grade</td>
      <td headers="estimate" class="gt_row gt_center"></td>
      <td headers="std.error" class="gt_row gt_center"></td>
      <td headers="p.value" class="gt_row gt_center"></td></tr>
          <tr><td headers="label" class="gt_row gt_left">    I</td>
      <td headers="estimate" class="gt_row gt_center">—</td>
      <td headers="std.error" class="gt_row gt_center">—</td>
      <td headers="p.value" class="gt_row gt_center"></td></tr>
          <tr><td headers="label" class="gt_row gt_left">    II</td>
      <td headers="estimate" class="gt_row gt_center">0.64</td>
      <td headers="std.error" class="gt_row gt_center">2.70</td>
      <td headers="p.value" class="gt_row gt_center">0.8</td></tr>
          <tr><td headers="label" class="gt_row gt_left">    III</td>
      <td headers="estimate" class="gt_row gt_center">2.4</td>
      <td headers="std.error" class="gt_row gt_center">2.64</td>
      <td headers="p.value" class="gt_row gt_center">0.4</td></tr>
        </tbody>
        
        <tfoot class="gt_footnotes">
          <tr>
            <td class="gt_footnote" colspan="4"><sup class="gt_footnote_marks">1</sup> SE = Standard Error</td>
          </tr>
        </tfoot>
      </table>
      </div>

