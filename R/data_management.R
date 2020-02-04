DATASETS <- list(
  "arcene.csv" = "https://www.openml.org/data/get_csv/1586211/phpt8tg99",
  "bioresponse.csv" = "https://www.openml.org/data/get_csv/1681097/phpSSK7iA",
  "christine.csv" = "https://www.openml.org/data/get_csv/19335515/file764d5d063390.arff",
  "dexter.arff" = "https://www.openml.org/data/download/1681111/phpEUwA95",
  "gina.csv" = "https://www.openml.org/data/get_csv/53921/gina_agnostic.arff",
  "gisette.arff" = "https://www.openml.org/data/download/18631146/gisette.arff",
  "human-activity-recognition.csv" = "https://www.openml.org/data/get_csv/1589271/php88ZB4Q",
  "hill-valley.csv" = "https://www.openml.org/data/get_csv/1590101/php3isjYz",
  "IMDB.arff" = "https://www.openml.org/data/download/11346/IMDB-F.drama.arff",
  "internet-advertisements.csv" =  "https://www.openml.org/data/get_csv/18140371/phpPIHVvG",
  "isolet.csv" = "https://www.openml.org/data/get_csv/52405/phpB0xrNj",
  "jasmine.csv" = "https://www.openml.org/data/get_csv/19335516/file79b563a1a18.arff",
  "madelon.csv" = "https://www.openml.org/data/get_csv/1590986/phpfLuQE4",
  "mfeat-factors.csv" = "https://www.openml.org/data/get_csv/12/dataset_12_mfeat-factors.arff",
  "mfeat-pixel.csv" = "https://www.openml.org/data/get_csv/20/dataset_20_mfeat-pixel.arff",
  "riccardo.csv" = "https://www.openml.org/data/get_csv/19335534/file7b535210a7df.arff",
  "scene.csv" = "https://www.openml.org/data/get_csv/1390080/phpuZu33P",
  "sonar.csv" = "https://www.openml.org/data/get_csv/40/dataset_40_sonar.arff"
)

#' @export
download_datasets <- function(dir = "ext_data") {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  walk2(DATASETS, names(DATASETS), function(url, dest) download.file(url, file.path(dir, dest)))
}

#' @export
dataset_list <- function(base_dir="ext_data", only=NULL, except=NULL) {
  fn <- names(DATASETS)
  datasets <- list()

  # https://www.openml.org/data/get_csv/1586211/phpt8tg99
  # Arcene: Cancer classification from mass-spectrometric data, slightly imbalanced
  # NIPS2003 FS challenge
  datasets$Arcene          <- function() read.csv(file.path(base_dir, fn[1])) %>% class_last()

  datasets$Bioresponse     <- function() read.csv(file.path(base_dir, fn[2])) %>% class_last()

  # https://www.openml.org/data/get_csv/19335515/file764d5d063390.arff
  # Christine: balanced
  datasets$Christine       <- function() read.csv(file.path(base_dir, fn[3])) %>% class_first()

  # https://www.openml.org/data/download/1681111/phpEUwA95
  # Dexter: text classification from bag-of-words, balanced
  # NIPS2003 FS challenge
  datasets$Dexter          <- function() mldr:::read_arff_internal(file.path(base_dir, fn[4]), stringsAsFactors = T)$dataset %>% class_last("1")

  # https://www.openml.org/data/get_csv/53921/gina_agnostic.arff
  # Gina (odd-vs-even number classification with anonymized features)
  datasets$Gina            <- function() read.csv(file.path(base_dir, fn[5])) %>% class_last()

  # https://www.openml.org/data/download/18631146/gisette.arff
  # Gisette: handwritten 4-vs-9 classification
  # NIPS2003 FS challenge
  datasets$Gisette         <- function() mldr:::read_arff_internal(file.path(base_dir, fn[6]), stringsAsFactors = T)$dataset %>% class_last()

  # https://www.openml.org/data/get_csv/1589271/php88ZB4Q
  # Classes: walking vs staying
  datasets$HAR             <- function() read.csv(file.path(base_dir, fn[7])) %>% class_last(c(1, 2, 3))

  datasets$HillValley      <- function() read.csv(file.path(base_dir, fn[8])) %>% class_last()

  # https://www.openml.org/data/get_csv/11346/IMDB-F.drama.arff
  # IMDB.drama: text classification, imbalanced
  datasets$IMDB            <- function() mldr:::read_arff_internal(file.path(base_dir, fn[9]), stringsAsFactors = T)$dataset %>% class_first()

  # https://www.openml.org/data/get_csv/18140371/phpPIHVvG
  # Internet-Advertisements: Very imbalanced, many binary features
  datasets$Internet        <- function() read.csv(file.path(base_dir, fn[10])) %>% class_last("ad")

  # https://www.openml.org/data/get_csv/52405/phpB0xrNj
  # Isolet (letter names pronounced)
  # Vowels vs consonants
  datasets$IsoletVowels    <- function() read.csv(file.path(base_dir, fn[11]), quote = "\"'") %>% class_last(c("1", "5", "9", "15", "21"))

  datasets$Jasmine         <- function() read.csv(file.path(base_dir, fn[12])) %>% class_first()

  # https://www.openml.org/data/get_csv/1590986/phpfLuQE4
  # Madelon: synthetic, balanced
  # NIPS2003 FS challenge
  datasets$Madelon         <- function() read.csv(file.path(base_dir, fn[13])) %>% class_last()

  # Number recognition: odd vs even
  datasets$MFeatFactorsOdd <- function() read.csv(file.path(base_dir, fn[14])) %>% class_last(c(1, 3, 5, 7, 9))

  # Number recognition: odd vs even
  datasets$MFeatPixelOdd   <- function() read.csv(file.path(base_dir, fn[15])) %>% class_last(c(1, 3, 5, 7, 9))

  datasets$Riccardo        <- function() read.csv(file.path(base_dir, fn[16])) %>% class_first()

  # Scene is multilabel: one test for each label
  datasets$SceneBeach      <- function() read.csv(file.path(base_dir, fn[17]))[, c(1:294, 295)] %>% class_last()
  datasets$SceneSunset     <- function() read.csv(file.path(base_dir, fn[17]))[, c(1:294, 296)] %>% class_last()
  datasets$SceneFall       <- function() read.csv(file.path(base_dir, fn[17]))[, c(1:294, 297)] %>% class_last()
  datasets$SceneField      <- function() read.csv(file.path(base_dir, fn[17]))[, c(1:294, 298)] %>% class_last()
  datasets$SceneMountain   <- function() read.csv(file.path(base_dir, fn[17]))[, c(1:294, 299)] %>% class_last()
  datasets$SceneUrban      <- function() read.csv(file.path(base_dir, fn[17]))[, c(1:294, 300)] %>% class_last()

  datasets$Sonar           <- function() read.csv(file.path(base_dir, fn[18])) %>% class_last("Rock")

  return(if (!is.null(only)) {
    datasets[only]
  } else if (!is.null(except)) {
    datasets[except] <- NULL
    datasets
  } else {
    datasets
  })
}
