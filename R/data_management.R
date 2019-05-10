download_datasets <- function(dir = "data") {
  # TODO: Auto-download datasets needed by dataset_list()
}

dataset_list <- function() {
  datasets <- list()

  # # Abalone: 19 vs remainder
  # abalone <- read_data("data/abalone.data")[, -1]
  # datasets$Abalone19 <- abalone %>% class_last(19)
  # # Abalone: 9 vs 18
  # datasets$Abalone9vs18 <- function() abalone %>% vs_last(18, 9)

  # https://www.openml.org/data/get_csv/1586211/phpt8tg99
  # Arcene: Cancer classification from mass-spectrometric data, slightly imbalanced
  # NIPS2003 FS challenge
  datasets$Arcene <- function() read.csv("data/arcene.csv") %>% class_last()

  # datasets$AutoUniv1 <- read.csv("data/autouniv.csv") %>% class_last("class1")
  # datasets$AutoUniv2 <- read.csv("data/autouniv.csv") %>% class_last("class2")

  datasets$Bioresponse <- function() read.csv("data/bioresponse.csv") %>% class_last()

  # https://www.openml.org/data/get_csv/19335515/file764d5d063390.arff
  # Christine: balanced
  datasets$Christine <- function() read.csv("data/christine.csv") %>% class_first()

  # https://www.openml.org/data/download/1681111/phpEUwA95
  # Dexter: text classification from bag-of-words, balanced
  # NIPS2003 FS challenge
  datasets$Dexter <- function() mldr:::read_arff_internal("data/dexter.arff", stringsAsFactors = T)$dataset %>% class_last("1")

  # ecoli <- read.table("data/ecoli.data")[, -1]
  # # Ecoli0vs1 (im vs cp)
  # datasets$Ecoli0vs1 <- function() ecoli %>% vs_last("im", "cp")
  # # Ecoli1 (im)
  # datasets$Ecoli1 <- function() ecoli %>% class_last("im")
  # # Ecoli2 (pp)
  # datasets$Ecoli2 <- function() ecoli %>% class_last("pp")
  # # Ecoli3 (imU)
  # datasets$Ecoli3 <- function() ecoli %>% class_last("imU")
  # # Ecoli4 (om)
  # datasets$Ecoli4 <- function() ecoli %>% class_last("om")
  # # Ecoli0137vs26
  # datasets$Ecoli0137vs26 <- function() ecoli %>% vs_last(c("pp", "imL"), c("cp", "im", "imU", "imS"))

  # https://www.openml.org/data/get_csv/53921/gina_agnostic.arff
  # Gina (odd-vs-even number classification with anonymized features)
  datasets$Gina <- function() read.csv("data/gina_agnostic.csv") %>% class_last()

  # https://www.openml.org/data/download/18631146/gisette.arff
  # Gisette: handwritten 4-vs-9 classification
  # NIPS2003 FS challenge
  datasets$Gisette <- function() mldr:::read_arff_internal("data/gisette.arff", stringsAsFactors = T)$dataset %>% class_last()

  # glass <- read_data("data/glass.data", row.names = 1)
  # # Glass: building_windows_float_processed vs remainder
  # datasets$Glass0 <- function() glass %>% class_last(1)
  # # Glass: building_windows_non_float_processed vs remainder
  # datasets$Glass1 <- function() glass %>% class_last(2)
  # # Glass: vehicle_windows_float_processed vs remainder
  # datasets$Glass2 <- function() glass %>% class_last(3)
  # # Glass: containers vs remainder
  # datasets$Glass4 <- function() glass %>% class_last(5)
  # # Glass: tableware vs remainder
  # # datasets$Glass5 <- glass %>% class_last(6)
  # # non-window glass; remainder
  # datasets$Glass0123vs456 <- function() glass %>% vs_last(5:7, 1:4)
  # # ve-win-oat-proc vs build-win-oat-proc,build-win-non oat-proc,headlamps
  # datasets$Glass016vs2 <- function() glass %>% vs_last(3, c(1,2,7))
  # # tableware vs build-win-oat-proc,build-win-non oat-proc,headlamps
  # # datasets$Glass016vs5 <- glass %>% vs_last(6, c(1,2,7))


  # # Haberman: Die vs Survive
  # datasets$Haberman <- function() read_data("data/haberman.data") %>% class_last(2)

  # https://www.openml.org/data/get_csv/1589271/php88ZB4Q
  # Classes: walking vs staying
  datasets$HAR <- function() read.csv("data/human-activity-recognition.csv") %>% class_last(c(1, 2, 3))

  # datasets$Heart <- function() read.csv("data/heart-statlog.csv") %>% class_last("present")

  datasets$HillValley <- function() read.csv("data/hill-valley.csv") %>% class_last()

  # https://www.openml.org/data/get_csv/11346/IMDB-F.drama.arff
  # IMDB.drama: text classification, imbalanced
  datasets$IMDB <- function() read.csv("data/IMDB-F.drama.csv") %>% class_first()


  # https://www.openml.org/data/get_csv/18140371/phpPIHVvG
  # Internet-Advertisements: Very imbalanced, many binary features
  datasets$Internet <- function() read.csv("data/internet-advertisements.csv") %>% class_last("ad")

  # # Iris0: setosa vs remainder
  # datasets$Iris0 <- function() iris %>% class_last("setosa")

  # https://www.openml.org/data/get_csv/52405/phpB0xrNj
  # Isolet (letter names pronounced)
  # Vowels vs consonants
  datasets$IsoletVowels <- function() read.csv("data/isolet.csv") %>% class_last(c("1", "5", "9", "15", "21"))

  datasets$Jasmine <- function() read.csv("data/jasmine.csv") %>% class_first()

  # https://www.openml.org/data/get_csv/1590986/phpfLuQE4
  # Madelon: synthetic, balanced
  # NIPS2003 FS challenge
  datasets$Madelon <- function() read.csv("data/madelon.csv") %>% class_last()

  # datasets$MFeatFactors1 <- function() read.csv("data/mfeat-factors.csv") %>% class_last(1)
  # datasets$MFeatFactors2 <- function() read.csv("data/mfeat-factors.csv") %>% class_last(2)
  # datasets$MFeatFactors3 <- function() read.csv("data/mfeat-factors.csv") %>% class_last(3)
  # datasets$MFeatFactors4 <- function() read.csv("data/mfeat-factors.csv") %>% class_last(4)
  # datasets$MFeatFactors5 <- function() read.csv("data/mfeat-factors.csv") %>% class_last(5)

  # Number recognition: odd vs even
  datasets$MFeatFactorsOdd <- function() read.csv("data/mfeat-factors.csv") %>% class_last(c(1, 3, 5, 7, 9))

  # datasets$MFeatPixel1 <- function() read.csv("data/mfeat-pixel.csv") %>% class_last(1)
  # datasets$MFeatPixel2 <- function() read.csv("data/mfeat-pixel.csv") %>% class_last(2)
  # datasets$MFeatPixel3 <- function() read.csv("data/mfeat-pixel.csv") %>% class_last(3)
  # datasets$MFeatPixel4 <- function() read.csv("data/mfeat-pixel.csv") %>% class_last(4)
  # datasets$MFeatPixel5 <- function() read.csv("data/mfeat-pixel.csv") %>% class_last(5)
  # Number recognition: odd vs even
  datasets$MFeatPixelOdd <- function() read.csv("data/mfeat-pixel.csv") %>% class_last(c(1, 3, 5, 7, 9))

  # # New-thyroid: hyper vs remainder
  # datasets$New_thyroid1 <- function() read_data("data/new-thyroid.data") %>% class_first(2)
  # # New-thyroid: hypo vs remainder
  # datasets$New_thyroid2 <- function() read_data("data/new-thyroid.data") %>% class_first(3)

  datasets$Riccardo <- function() read.csv("data/riccardo.csv") %>% class_first()

  # # Satellite
  # datasets$SatelliteGrey <- function() read.table("data/sat.trn") %>% class_last(c(3, 4, 7))
  # datasets$SatelliteRed <- function() read.table("data/sat.trn") %>% class_last(1)

  # Scene is multilabel: one test for each label
  datasets$SceneBeach <- function() read.csv("data/scene.csv")[, c(1:294, 295)] %>% class_last()
  datasets$SceneSunset <- function() read.csv("data/scene.csv")[, c(1:294, 296)] %>% class_last()
  datasets$SceneFall <- function() read.csv("data/scene.csv")[, c(1:294, 297)] %>% class_last()
  datasets$SceneField <- function() read.csv("data/scene.csv")[, c(1:294, 298)] %>% class_last()
  datasets$SceneMountain <- function() read.csv("data/scene.csv")[, c(1:294, 299)] %>% class_last()
  datasets$SceneUrban <- function() read.csv("data/scene.csv")[, c(1:294, 300)] %>% class_last()

  datasets$Sonar <- function() read.csv("data/sonar.csv") %>% class_last("Rock")

  # # Vehicle:
  # vehicle <- read.table("data/xfull.dat")
  # datasets$Vehicle0 <- function() vehicle %>% class_last("van")
  # datasets$Vehicle1 <- function() vehicle %>% class_last("saab")
  # datasets$Vehicle2 <- function() vehicle %>% class_last("bus")
  # datasets$Vehicle3 <- function() vehicle %>% class_last("opel")
  #
  # # https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data
  # datasets$Wisconsin <- function() read_data("data/wdbc.data", row.names = 1) %>% class_first("M")
  #
  # # Yeast: some class vs remainder
  # yeast <- read.table("data/yeast.data")[, -1]
  # datasets$Yeast1 <- function() yeast %>% class_last("NUC")
  # datasets$Yeast3 <- function() yeast %>% class_last("ME3")
  # # datasets$Yeast4 <- yeast %>% class_last("ME2")
  # # datasets$Yeast5 <- yeast %>% class_last("ME1")
  # # datasets$Yeast6 <- yeast %>% class_last("EXC")
  # # Yeast: classes vs other classes
  # datasets$Yeast2vs4 <- function() yeast %>% vs_last("ME2", "CYT")
  # datasets$Yeast05679vs4 <- function() yeast %>% vs_last("ME2", c("MIT", "ME3", "EXC", "VAC", "ERL"))
  # # datasets$Yeast1458vs7 <- yeast %>% vs_last("VAC", c("NUC", "ME3", "ME2", "POX"))
  # # datasets$Yeast2vs8 <- yeast %>% vs_last("POX", "CYT")
  # datasets$Yeast1vs7 <- function() yeast %>% vs_last("VAC", "NUC")
  # # datasets$Yeast1289vs7 <- yeast %>% vs_last("VAC", c("NUC", "CYT", "ERL", "POX"))

  # datasets <- datasets %>% keep(~ ncol(.()$x) >= 50)
  # gc()

  # message("Normalizing ", names(datasets)[datasets %>% map("normalize") %>% as.logical()] %>% paste0(collapse = ", "))
  # ir <- datasets %>% map("y") %>% map(as.integer) %>% map(~ . - 1) %>% map(mean)
  # message("Datasets with > 0.95 imbalance ratio: ", names(datasets)[ir <= 0.05] %>% paste0(collapse = ", "))

  datasets

  # list(
  #   # gina = datasets$Gina,
  #   # heart = datasets$Heart,
  #   # sonar = datasets$Sonar,
  #   madelon = datasets$Madelon,
  #   yeast = datasets$Yeast1
  # )

  # list(riccardo = datasets$Riccardo)
  # list(iris = function() iris %>% class_last("setosa"))
}

# dataset_metrics <- function(datasets = dataset_list()) {
#   map2(datasets, names(datasets), function(ds, name) {
#     ds <- ds()
#     data.frame(
#       name = name,
#       minratio = dcme::proportion_examples_minority(ds$y),
#       f1 = dcme::F1(ds$x, ds$y),
#       dimension = dcme::num_features(ds$x)
#     )
#   }) %>% do.call(rbind, .)
# }
