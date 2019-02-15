dataset_list <- function() {
  datasets <- list()

  # Abalone: 19 vs remainder
  abalone <- read_data("data/abalone.data")[, -1]
  datasets$Abalone19 <- abalone %>% class_last(19)
  # Abalone: 9 vs 18
  datasets$Abalone9vs18 <- abalone %>% vs_last(9, 18)

  ecoli <- read.table("data/ecoli.data")[, -1]
  # Ecoli0vs1 (im vs cp)
  datasets$Ecoli0vs1 <- ecoli %>% vs_last("im", "cp")
  # Ecoli1 (im)
  datasets$Ecoli1 <- ecoli %>% class_last("im")
  # Ecoli2 (pp)
  datasets$Ecoli2 <- ecoli %>% class_last("pp")
  # Ecoli3 (imU)
  datasets$Ecoli3 <- ecoli %>% class_last("imU")
  # Ecoli4 (om)
  datasets$Ecoli4 <- ecoli %>% class_last("om")
  # Ecoli0137vs26
  datasets$Ecoli0137vs26 <- ecoli %>% vs_last(c("pp", "imL"), c("cp", "im", "imU", "imS"))

  glass <- read_data("data/glass.data", row.names = 1)
  # Glass: building_windows_float_processed vs remainder
  datasets$Glass0 <- glass %>% class_last(1)
  # Glass: building_windows_non_float_processed vs remainder
  datasets$Glass1 <- glass %>% class_last(2)
  # Glass: vehicle_windows_float_processed vs remainder
  datasets$Glass2 <- glass %>% class_last(3)
  # Glass: containers vs remainder
  datasets$Glass4 <- glass %>% class_last(5)
  # Glass: tableware vs remainder
  datasets$Glass5 <- glass %>% class_last(6)
  # non-window glass; remainder
  datasets$Glass0123vs456 <- glass %>% vs_last(5:7, 1:4)
  # ve-win-oat-proc vs build-win-oat-proc,build-win-non oat-proc,headlamps
  datasets$Glass016vs2 <- glass %>% vs_last(3, c(1,2,7))
  # tableware vs build-win-oat-proc,build-win-non oat-proc,headlamps
  datasets$Glass016vs5 <- glass %>% vs_last(6, c(1,2,7))

  # Haberman: Die vs Survive
  datasets$Haberman <- read_data("data/haberman.data") %>% class_last(2)

  # New-thyroid: hyper vs remainder
  datasets$New_thyroid1 <- read_data("data/new-thyroid.data") %>% class_first(2)
  # New-thyroid: hypo vs remainder
  datasets$New_thyroid2 <- read_data("data/new-thyroid.data") %>% class_first(3)

  # Iris0: setosa vs remainder
  datasets$Iris0 <- iris %>% class_last("setosa")

  # Isolet (letter names pronounced)
  datasets$IsoletVowels <- read_data("data/isolettrain.data") %>% class_last(c(1, 5, 9, 15, 21))

  # Satellite
  datasets$SatelliteGrey <- read.table("data/sat.trn") %>% class_last(c(3, 4, 7))
  datasets$SatelliteRed <- read.table("data/sat.trn") %>% class_last(1)

  # Vehicle:
  vehicle <- read.table("data/xfull.dat")
  datasets$Vehicle0 <- vehicle %>% class_last("van")
  datasets$Vehicle1 <- vehicle %>% class_last("saab")
  datasets$Vehicle2 <- vehicle %>% class_last("bus")
  datasets$Vehicle3 <- vehicle %>% class_last("opel")

  # https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data
  datasets$Wisconsin <- read_data("data/wdbc.data", row.names = 1) %>% class_first("M")

  # Yeast: some class vs remainder
  yeast <- read.table("data/yeast.data")[, -1]
  datasets$Yeast1 <- yeast %>% class_last("NUC")
  datasets$Yeast3 <- yeast %>% class_last("ME3")
  datasets$Yeast4 <- yeast %>% class_last("ME2")
  datasets$Yeast5 <- yeast %>% class_last("ME1")
  datasets$Yeast6 <- yeast %>% class_last("EXC")
  # Yeast: classes vs other classes
  datasets$Yeast2vs4 <- yeast %>% vs_last("CYT", "ME2")
  datasets$Yeast05679vs4 <- yeast %>% vs_last("ME2", c("MIT", "ME3", "EXC", "VAC", "ERL"))
  datasets$Yeast1458vs7 <- yeast %>% vs_last("VAC", c("NUC", "ME3", "ME2", "POX"))
  datasets$Yeast2vs8 <- yeast %>% vs_last("POX", "CYT")
  datasets$Yeast1vs7 <- yeast %>% vs_last("NUC", "VAC")
  datasets$Yeast1289vs7 <- yeast %>% vs_last("VAC", c("NUC", "CYT", "ERL", "POX"))

  message("Normalizing ", names(datasets)[datasets %>% map("normalize") %>% as.logical()] %>% paste0(collapse = ", "))

  datasets
}
