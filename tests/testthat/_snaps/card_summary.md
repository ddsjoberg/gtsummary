# card_summary() works

    Code
      as.data.frame(card_summary(ard_stack(data = ADSL, by = ARM, ard_categorical(
        variables = "AGEGR1"), ard_continuous(variables = "AGE"), .attributes = TRUE,
      .missing = TRUE)))
    Output
        **Characteristic** **Placebo**  \nN = 86 **Xanomeline High Dose**  \nN = 84
      1 Pooled Age Group 1                  <NA>                               <NA>
      2              65-80            42 (48.8%)                         55 (65.5%)
      3                <65            14 (16.3%)                         11 (13.1%)
      4                >80            30 (34.9%)                         18 (21.4%)
      5                Age     76.0 (69.0, 82.0)                  76.0 (70.5, 80.0)
        **Xanomeline Low Dose**  \nN = 84
      1                              <NA>
      2                        47 (56.0%)
      3                          8 (9.5%)
      4                        29 (34.5%)
      5                 77.5 (71.0, 82.0)

---

    Code
      as.data.frame(card_summary(ard_stack(data = ADSL, by = NULL, ard_categorical(
        variables = "AGEGR1"), ard_continuous(variables = "AGE"), .attributes = TRUE,
      .missing = TRUE)))
    Output
        **Characteristic**       **N = 254**
      1 Pooled Age Group 1              <NA>
      2              65-80       144 (56.7%)
      3                <65        33 (13.0%)
      4                >80        77 (30.3%)
      5                Age 77.0 (70.0, 81.0)

