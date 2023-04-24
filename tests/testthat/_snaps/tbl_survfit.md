# no errors/warnings with stratified variable

    Code
      tbl_survfit(s1, times = c(12, 24)) %>% as.data.frame()
    Output
            **Characteristic**    **Time 12**    **Time 24**
      1 Chemotherapy Treatment           <NA>           <NA>
      2                 Drug A 91% (85%, 97%) 47% (38%, 58%)
      3                 Drug B 86% (80%, 93%) 41% (33%, 52%)

---

    Code
      tbl_survfit(s1.1, times = c(12, 24)) %>% as.data.frame()
    Output
        **Characteristic**    **Time 12**    **Time 24**
      1          trial$trt           <NA>           <NA>
      2             Drug A 91% (85%, 97%) 47% (38%, 58%)
      3             Drug B 86% (80%, 93%) 41% (33%, 52%)

---

    Code
      tbl_survfit(s1, times = c(12, 24), reverse = TRUE) %>% as.data.frame()
    Output
            **Characteristic**      **Time 12**    **Time 24**
      1 Chemotherapy Treatment             <NA>           <NA>
      2                 Drug A 9.2% (3.3%, 15%) 53% (42%, 62%)
      3                 Drug B  14% (6.8%, 20%) 59% (48%, 67%)

---

    Code
      tbl_survfit(s1, probs = c(0.2, 0.4), estimate_fun = partial(style_sigfig,
        digits = 4)) %>% as.data.frame()
    Output
            **Characteristic**   **20% Percentile**   **40% Percentile**
      1 Chemotherapy Treatment                 <NA>                 <NA>
      2                 Drug A 16.43 (14.06, 18.29) 20.90 (18.37, 23.60)
      3                 Drug B 13.00 (11.18, 15.59) 18.00 (15.77, 21.49)

# no errors/warnings with no stratified variable

    Code
      tbl_survfit(s2, times = c(12, 24)) %>% as.data.frame()
    Output
        **Characteristic**    **Time 12**    **Time 24**
      1            Overall 89% (84%, 93%) 44% (38%, 51%)

---

    Code
      tbl_survfit(s2.1, times = c(12, 24)) %>% as.data.frame()
    Output
        **Characteristic**    **Time 12**    **Time 24**
      1            Overall 89% (84%, 93%) 44% (38%, 51%)

---

    Code
      tbl_survfit(s2, probs = c(0.2, 0.4), estimate_fun = partial(style_sigfig,
        digits = 4)) %>% as.data.frame()
    Output
        **Characteristic**   **20% Percentile**   **40% Percentile**
      1            Overall 14.60 (12.68, 16.23) 19.44 (17.77, 21.49)

---

    Code
      tbl %>% as.data.frame()
    Output
            **Characteristic**       **Time 10**
      1                  Grade              <NA>
      2                      I 100% (100%, 100%)
      3                     II    91% (85%, 98%)
      4                    III    92% (86%, 99%)
      5 Chemotherapy Treatment              <NA>
      6                 Drug A   96% (92%, 100%)
      7                 Drug B    93% (88%, 98%)

# no errors/warnings with competing events

    Code
      tbl_survfit(cr_1, times = c(12, 24)) %>% as.data.frame()
    Message
      tbl_survfit: Multi-state model detected. Showing probabilities into state 'death from cancer'
    Output
        **Characteristic**       **Time 12**    **Time 24**
      1            Overall 3.0% (1.4%, 6.6%) 27% (21%, 33%)

---

    Code
      summod2 %>% as.data.frame()
    Output
        **Characteristic**      **Time 12**    **Time 24**
      1        Tumor Grade             <NA>           <NA>
      2                  I 1.5% (0.2%, 10%) 28% (19%, 41%)
      3                 II 4.4% (1.5%, 13%) 21% (13%, 33%)
      4                III 3.1% (0.8%, 12%) 31% (22%, 45%)

