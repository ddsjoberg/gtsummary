# tbl_likert(data)

    Code
      as.data.frame(add_n(tbl_likert(df_likert)))
    Output
        **Characteristic** **N** **Strongly Disagree** **Disagree** **Agree**
      1   recommend_friend    20               7 (35%)      3 (15%)   7 (35%)
      2    regret_purchase    20               7 (35%)     1 (5.0%)   8 (40%)
        **Strongly Agree**
      1            3 (15%)
      2            4 (20%)

# tbl_likert(statistic)

    Code
      as.data.frame(tbl_likert(df_likert, statistic = ~"{n} / {N} ({p}%)"))
    Output
        **Characteristic** **Strongly Disagree**  **Disagree**    **Agree**
      1   recommend_friend          7 / 20 (35%)  3 / 20 (15%) 7 / 20 (35%)
      2    regret_purchase          7 / 20 (35%) 1 / 20 (5.0%) 8 / 20 (40%)
        **Strongly Agree**
      1       3 / 20 (15%)
      2       4 / 20 (20%)

# tbl_likert(digits)

    Code
      as.data.frame(tbl_likert(df_likert, digits = ~ list(p = label_style_sigfig(
        digits = 3, scale = 100))))
    Output
        **Characteristic** **Strongly Disagree** **Disagree** **Agree**
      1   recommend_friend             7 (35.0%)    3 (15.0%) 7 (35.0%)
      2    regret_purchase             7 (35.0%)    1 (5.00%) 8 (40.0%)
        **Strongly Agree**
      1          3 (15.0%)
      2          4 (20.0%)

# tbl_likert(sort)

    Code
      as.data.frame(tbl_likert(df_likert, sort = "descending"))
    Output
        **Characteristic** **Strongly Agree** **Agree** **Disagree**
      1   recommend_friend            3 (15%)   7 (35%)      3 (15%)
      2    regret_purchase            4 (20%)   8 (40%)     1 (5.0%)
        **Strongly Disagree**
      1               7 (35%)
      2               7 (35%)

