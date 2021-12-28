# 1. install pkg that will run and save the documentation images
remotes::install_github("ddsjoberg/gt.doc.images")

# 2. Install the most recent version of gtsummary

# 3. Restart R to have a fresh R session.
#    No packages should be loaded, not even gtsummary
#    Any object that is in the global environment may be written over!

# 4. Run the function below to save the images created in the help files.
#    Only objects whose named in `_ex` or `_ex[:digit:]+` are saved.
#    Files will be saved to "~gtsummary/man/figures/<filename>.png",
#    where the file name is the object name, i.e.'tbl_ae_count_ex1.png'.
#    No example object name may overlap throughout the entire package.
gt.doc.images::save_help_file_images(pkg = "gtsummary")

# 5. You can save the images for a single file as well
gt.doc.images::save_help_file_images(
  pkg = "gtsummary",
  rd_files = "add_ci"
)
