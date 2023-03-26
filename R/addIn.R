addIn_colabPy2Rmd = function(){
  rstudioapi::getSourceEditorContext() -> sourceActiveDoc
  sourceActiveDoc$path -> pyfile
  pyfile |>
    returnFileTypeInUppercase() -> fileType
  # sourceEditor is a py file
  if(fileType=="PY"){
    pyfile |> convertPyToRmd()
  } else
  # there is colabPy from Sys.getenv
  if(Sys.getenv("colabPy")!=""){
    pyfile = Sys.getenv("colabPy")
    assertthat::assert_that(
      file.exists(pyfile),
      msg=glue::glue("{pyfile} does not exist under the project root."))

    pyfile |> returnFileTypeInUppercase() -> fileType
    assertthat::assert_that(
      fileType=="PY",
      msg=glue::glue("{pyfile} is no a .py file.")
    )
    pyfile |> convertPyToRmd()
  } else {
    stop("Neither .py file is active in the source editor or\n a proper colabPy is set in Sys.setenv.")
  }
}
