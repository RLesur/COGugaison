reenc_nonascii_char <- function(
  str, # un vecteur de chaînes de caractères
  type = c("html", "ascii") # type du document : html pour 00Index.html, ascii pour DESCRIPTION
) {

  if (match.arg(type) == "html") {
    op <- "&lt;" # signe <
    cl <- "&gt;" # signe >
  } else {
    op <- "<"
    cl <- ">"
  }

  # on recherche si des caractères unicode sont présents dans le document
  patterns <- unique(unlist(stringi::stri_extract_all_regex(
    str,
    paste0("(", op, "[\\p{ASCII_HEX_DIGIT}]{2}", cl, "){2,4}"),
    omit_no_match = TRUE
  )))

  # si aucun caractère unicode n'est présent, on s'arrête
  if (length(patterns) == 0)
    return(str)

  # on sépare les différents octets composant les caractères unicode détectés
  chars_bytes <- stringi::stri_split_regex(patterns, cl, omit_empty = TRUE)

  # on reformate en les octets en hexadécimal
  chars_hex <- lapply(chars_bytes,
                      function(x) stringi::stri_replace_all_regex(x, op, "0x")
  )

  # on récupère les caractères unicode
  chars_utf8 <- vapply(chars_hex,
                       function(x) stringi::stri_conv(as.raw(x), from = "UTF-8", to = "UTF-8"),
                       FUN.VALUE = character(1)
  )

  # on remplace les chaînes d'octets détectées par leur caractère
  stringi::stri_replace_all_fixed(str, patterns, chars_utf8, vectorize_all = FALSE)
}

cat("\n===Début du clean===\n")
index <- system.file("html/00Index.html", package = "COGugaison")
if (file.exists(index)) {
  cat("--Re encodage du fichier 00Index.html\n")
  content <- readLines(index)
  content_utf8 <- enc2utf8(content)
  cat("--Correction du titre de l'index--\n")
  content_utf8 <- reenc_nonascii_char(content_utf8, type = "html")
  out <- file(index, open = "w+", encoding = "native.enc")
  writeLines(content_utf8, out, useBytes = TRUE)
  close(out)
}

description <- system.file("DESCRIPTION", package = "COGugaison")
if (file.exists(description)) {
  cat("--Correction du fichier DESCRIPTION--\n")
  content <- readLines(description)
  content_utf8 <- reenc_nonascii_char(content, type = "ascii")
  out <- file(description, open = "w+", encoding = "native.enc")
  writeLines(content_utf8, out, useBytes = TRUE)
  close(out)
}
cat("===Fin du clean===\n")
