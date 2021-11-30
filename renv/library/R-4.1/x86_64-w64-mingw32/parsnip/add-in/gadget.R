parsnip_spec_add_in <- function() {
  # ------------------------------------------------------------------------------
  # check installs

  libs <- c("shiny", "miniUI", "rstudioapi")
  is_inst <- rlang::is_installed(libs)
  if (any(!is_inst)) {
    missing_pkg <- libs[!is_inst]
    missing_pkg <- paste0(missing_pkg, collapse = ", ")
    rlang::abort(
      glue::glue(
        "The add-in requires some CRAN package installs: ",
        glue::glue_collapse(glue::glue("'{missing_pkg}'"), sep = ", ")
      )
    )
  }

  library(shiny)
  library(miniUI)
  library(rstudioapi)

  data(model_db, package = "parsnip")

  # ------------------------------------------------------------------------------

  make_spec <- function(x, tune_args) {
    if (tune_args) {
      nms <- x$parameters[[1]]$parameter
      args <- purrr::map(nms, ~ rlang::call2("tune"))
      names(args) <- nms
    } else {
      args <- NULL
    }

    if (x$package != "parsnip") {
      pkg <- x$package
    } else {
      pkg <- NULL
    }

    if (length(args) > 0) {
      cl_1 <- rlang::call2(.ns = pkg, .fn = x$model, !!!args)
    } else {
      cl_1 <- rlang::call2(.ns = pkg, .fn = x$model)
    }

    obj_nm <- paste0(x$model,"_", x$engine, "_spec")
    chr_1 <- rlang::expr_text(cl_1, width = 500)
    chr_1 <- paste0(chr_1, collapse = " ")
    chr_1 <- paste(obj_nm, "<-\n ", chr_1)
    chr_2 <- paste0("set_engine('", x$engine, "')")

    res <- paste0(chr_1, " %>%\n  ", chr_2)

    if (!x$single_mode) {
      chr_3 <- paste0("set_mode('", x$mode, "')")
      res <- paste0(res, " %>%\n  ", chr_3)
    }

    res
  }

  ui <-
    miniPage(
      gadgetTitleBar("Write out model specifications"),
      miniContentPanel(
        fillRow(
          fillCol(
            radioButtons(
              "model_mode",
              label = h3("Type of Model"),
              choices = c("Classification", "Regression")
            ),
            checkboxInput(
              "tune_args",
              label = "Tag parameters for tuning (if any)?",
              value = TRUE
            ),
            textInput(
              "pattern",
              label = "Match on (regex)"
            )
          ),
          fillRow(
            miniContentPanel(uiOutput("model_choices"))
          )
        )
      ),
      miniButtonBlock(
        actionButton("write", "Write specification code", class = "btn-success")
      )
    )


  server <-
    function(input, output) {
      get_models <- reactive({
        req(input$model_mode)

        models <- model_db[model_db$mode == tolower(input$model_mode),]
        if (nchar(input$pattern) > 0) {
          incld <- grepl(input$pattern, models$model) | grepl(input$pattern, models$engine)
          models <- models[incld,]

        }
        models
      }) # get_models

      output$model_choices <- renderUI({

        model_list <- get_models()
        if (nrow(model_list) > 0) {

        choices <- paste0(model_list$model, " (", model_list$engine, ")")
        choices <- unique(choices)
        } else {
          choices <- NULL
        }

        checkboxGroupInput(
          inputId = "model_name",
          label = "",
          choices = choices
        )
      }) # model_choices

      create_code <- reactive({

        req(input$model_name)
        req(input$model_mode)

        model_mode <- tolower(input$model_mode)
        selected <- model_db[model_db$label %in% input$model_name,]
        selected <- selected[selected$mode %in% model_mode,]

        res <- purrr::map_chr(1:nrow(selected),
                              ~ make_spec(selected[.x,], tune_args = input$tune_args))

        paste0(res, sep = "\n\n")

      }) # create_code

      observeEvent(input$write, {
        res <- create_code()
        for (txt in res) {
          rstudioapi::insertText(txt)
        }
      })

      observeEvent(input$done, {
        stopApp()
      })
    }

  viewer <- paneViewer(300)
  runGadget(ui, server, viewer = viewer)
}

parsnip_spec_add_in()

