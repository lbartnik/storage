helper_empty_filesystem <- function (path)
{
  if (missing(path)) path <- file.path(tempdir(), 'filesystem')
  unlink(path, recursive = TRUE, force = TRUE)

  filesystem(path, create = TRUE)
}

helper_sample_filesystem <- function (path)
{
  fs <- helper_empty_filesystem(path)
  lapply(1:10, function(i) os_write(fs, i, list(tag = letters[i])))
  fs
}

helper_rm_rf <- function (x)
{
  stopifnot(is_filesystem(x))
  unlink(x, recursive = TRUE, force = TRUE)
}



helper_empty_memory <- function ()
{
  memory()
}

helper_sample_memory <- function ()
{
  mm <- helper_empty_memory()
  lapply(1:10, function(i) os_write(mm, i, list(tag = letters[i])))
  mm
}

