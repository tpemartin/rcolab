convertColabPy2Rmd <- function(py) {
  py |>
    stringr::str_which("^\"\"\"") -> whichThreeQuotations

  whichThreeQuotations |>
    matrix(nrow=length(whichThreeQuotations)/2, byrow = T) |>
    as.data.frame() |>
    setNames(c("start",'end')) -> textAreaTable

  data.frame(
    start=textAreaTable$end+1,
    end=c(textAreaTable$start[-1]-1, length(py)+1)
  ) -> codeAreaTable

  for(i in 1:nrow(codeAreaTable)){
    py[[codeAreaTable$start[[i]]]] = "```{r}"
    py[[codeAreaTable$end[[i]]]]="```"
  }
  title <- py[[textAreaTable$start[[1]]]]
  title |> stringr::str_extract("[^\"]+") -> title
  py <- py[-whichThreeQuotations]
  c(glue::glue("---\ntitle: \"{title}\"\n---"),
    py) -> Rmd
  return(Rmd)
}
returnFileTypeInUppercase = function(filepath){
  filepath |> stringr::str_extract("[^\\.]+$") |>
    toupper() -> fileType
  fileType
}
convertPyToRmd = function(pyfile){
  py= xfun::read_utf8(pyfile)
  Rmd=convertColabPy2Rmd(py)
  RmdFile = stringr::str_replace(pyfile,".py",".Rmd")
  # RmdFile
  xfun::write_utf8(Rmd,con=RmdFile)
  file.edit(RmdFile)}
