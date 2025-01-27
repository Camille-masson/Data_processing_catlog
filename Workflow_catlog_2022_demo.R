### 0. LIBRARIES AND CONSTANTS ###
#--------------------------------#

# For the pipes
library(tidyverse) # includes ggplot2 dplyr among others
library(lubridate)
library(patchwork) # plot composition, fake facetting
# GIS packages
library(sf)
library(sp)
# Load functions
setwd(dir = "~/These/4 - STrouMPH/STrouMPH_R/")
source("Functions/Functions_ggplot_custom.R")
source("Functions/Functions_utility.R")
# Constants
source("Functions/Constants.R")
ncores = 3 # Number of CPU cores to be used for parallelised analyses. Adapt to the number of cores and amount of RAM available if the script crashes
YEAR = 2023
ALPAGES_TOTAL = list(   "2022" = c("Ane-et-Buyant", "Cayolle", "Combe-Madame", "Grande-Fesse", "Jas-des-Lievres", "Lanchatra", "Pelvas", "Sanguiniere", "Viso"),
                        "2023" = c("Cayolle", "Crouzet", "Grande-Cabane", "Lanchatra", "Rouanette", "Sanguiniere", "Vacherie-de-Roubion", "Viso") )
ALPAGES = ALPAGES_TOTAL[[as.character(YEAR)]]

# Set pathes
data_dir <- "/home/moamo/These/4 - STrouMPH/Donnees_GPS/"
output_dir <- paste0("/home/moamo/These/4 - STrouMPH/STrouMPH_R/Figures_Catlog_",YEAR,"/")
raster_dir <- "/home/moamo/These/QGIS/Donnees_sources/"
UP_file <- "/home/moamo/These/QGIS/Donnees_sources/UP_ZP_Michel/v1_bd_shape_up_inra_2012_2014_2154_all_emprise.shp"
AIF <- paste0(data_dir,YEAR,"_infos_alpages.csv") # Chemin du fichier contenant les colonnes  "alpage", "nom_alpages_determinant", "proportion_jour_allume", "UP_nom1", "chemin_carte_vegetation","chemin_carte_phenologie", "medcrit", "meancrit", "spikesp", "spikecos"


### 1. GPS TRAJECTORIES FILTERING ###
#-----------------------------------#

### 1.1 Simplification des trajectoires pour identification des dates de poses et retrait dans QGIS
if (F) {
    # 1. Faire tourner le code ci-dessous
    # 2. Importer la couche GPKG dans QGIS
    # 3. Calculatrice de champ : créer un champ "Datetime" de type "Date et heure" avec comme expression "to_datetime(date)" (conversion de la string "date" en un format de date compréhensible par QGIS)
    # 4. Changer le style de la couche en "Catégorisé" nouvellement créée pour avoir une couleur par "ID" de collier
    # 5. Dans les propriétés de la couche, onglet "Temporel", cocher la case en haut à gauche pour activer le "Contrôle temporel dynamique", puis dans "Configuration" choisir "Champ unique avec date et heure", dans "Champ" sélectionner le champ "Datetime" nouvellement créé. Valider.
    # 6. Dans la fenêtre principale, cliquer sur l’horloge pour ouvrir le Panneau de contrôle temporel. Régler grossièrement (et par excès) la "Plage d’annimation" avec les dates de début et de fin de saison et un pas de 24h.
    # 7. Explorer les dates de la saison pour trouver les dates de pose et retrait de chaque collier (ne pas hésiter à les cocher/décocher dans la légende pour bien identifier les colliers présents à une date donnée)

    library(terra)
    source("Functions/Functions_filtering.R")
    # ENTREES
    # Un dossier contenant les trajectoires brutes, au format csv issu des colliers catlog, rangées dans des sous-dossiers au nom de leurs alpages
    raw_data_dir = paste0("/home/moamo/These/4 - STrouMPH/Donnees_GPS/Colliers_",YEAR,"_brutes/")
    # Les alpages à traiter
    alpages = c("Rouanette", "Crouzet")
    # SORTIES
    # Un .gpkg contenant les trajectoires rééchantillonnées
    output_file = paste0(data_dir,"Donnees_brutes_",YEAR,"_simplifiees.gpkg")


    lapply(alpages, function(alpage) {
        collar_dir = paste0(raw_data_dir,alpage,"/")
        collar_files = list.files(collar_dir)
        print(collar_files)
        lapply(collar_files, function(collar_f) {
            collar_ID = strsplit(collar_f, split = "_")[[1]][1]
            load_catlog_data(paste0(collar_dir,collar_f)) %>%
                slice(which(row_number() %% 30 == 10))  %>% # sample one point every hour only, to simplify visualisation in QGIS
                mutate(ID = collar_ID) %>%
                mutate(date = lubridate::format_ISO8601(date)) %>%
                vect(geom=c("lon", "lat"), crs = CRS_WSG84) #%>%
                # as.lines()
            }) %>%
            do.call(rbind, .)
        }) %>%
    do.call(rbind, .) %>%
    writeVector(filename = output_file, overwrite=T)
}

### 1.2 BJONERAAS FILTER CALIBRATION
if (F) {
    source("Functions/Functions_filtering.R")
    source("Functions/Functions_map_plot.R")
    # ENTREES
    # Un dossier contenant les trajectoires brutes, au format csv issu des colliers catlog, rangées dans des sous-dossiers au nom de leurs alpages
    raw_data_dir = paste0("/home/moamo/These/4 - STrouMPH/Donnees_GPS/Colliers_",YEAR,"_brutes/")
    # Un data.frame contenant les dates de pose et de retrait des colliers, Doit contenir les colonnes  "alpage", "date_pose" et "date_retrait"
    AIF <- paste0(data_dir,YEAR,"_infos_alpages.csv")
    # L’alpage devant être traité
    alpage = "Cayolle"

    pdf("Filtering_calibration.pdf", width = 9, height = 9)

    files = list.files(paste0(raw_data_dir, alpage),  full.names=T)
    files = files[1:3]
    data = do.call(rbind, lapply(files, function(file) { data = load_catlog_data(file)
                                                         data$ID = file
                                                         return(data) }))
    beg_date = as.POSIXct(get_alpage_info(alpage, AIF, "date_pose"), tz="GMT", format="%d/%m/%Y %H:%M:%S")
    end_date = as.POSIXct(get_alpage_info(alpage, AIF, "date_retrait"), tz="GMT", format="%d/%m/%Y %H:%M:%S")
    data = date_filter(data, beg_date, end_date)

    data_xy = data %>%
                terra::vect(crs="EPSG:4326") %>%
                terra::project(CRS_L93) %>%
                as.data.frame(geom = "XY") %>%
                head()

    # Histogramme des périodes d’échantillonnage
    temps <- diff(data_xy$date)
    temps <- as.numeric(temps, units = "mins")
    hist(temps, nclass = 30)

    # Histogramme des distances parcourues
    dist <- sqrt(diff(data_xy$x)^2+diff(data_xy$y)^2)
    h <- hist(dist, nclass = 30, xlab='Distance (m)', xaxt="n")

    ### Test de différents filtres
    # Parameters sets to be tested
    medcrits = c(750, 500, 750)
    meancrits = c(500, 500, 350)
    spikesps = c(1500, 1500, 1500)
    spikecoss = c(-0.95, -0.95, -0.95)

    for (i in 1:length(medcrits)) {
        trajectories <- position_filter(data, medcrit=medcrits[i], meancrit=meancrits[i], spikesp=spikesps[i], spikecos=spikecoss[i])

        minmax_xy = get_minmax_L93(trajectories[!(trajectories$R1error | trajectories$R2error ),], buffer = 100)

        trajectories$errors = 1
        trajectories$errors[trajectories$R1error] = 2
        trajectories$errors[trajectories$R2error] = 3

        pal <- c("#56B4E9", "red", "black")
        print(ggplot(trajectories, aes(x, y, col = errors)) +
            geom_path(size = 0.2) +
            geom_point(size = 0.3) +
            coord_equal() +
            xlim(minmax_xy$x_min, minmax_xy$x_max) + ylim(minmax_xy$y_min, minmax_xy$y_max) +
            ggtitle(paste0("medcrit = ", medcrits[i], ", meancrit = ", meancrits[i], ", spikesp = ", spikesps[i], ", spikecos = ", spikecoss[i])) +
            scale_colour_gradientn(colors=pal, guide="legend", breaks = c(1, 2, 3), labels = c("OK", "R1error", "R2error")))

        trajectories <- trajectories %>%
                filter(date < beg_date + 3600*24*5)
        print(ggplot(trajectories, aes(x, y, col = errors)) +
            geom_path(size = 0.2) +
            geom_point(size = 0.3) +
            coord_equal() +
            ggtitle(paste0("5 days only, medcrit = ", medcrits[i], ", meancrit = ", meancrits[i], ", spikesp = ", spikesps[i], ", spikecos = ", spikecoss[i])) +
            scale_colour_gradientn(colors=pal, guide="legend", breaks = c(1, 2, 3), labels = c("OK", "R1error", "R2error")))
    }
    dev.off()
}

### 1.3 FILTERING CATLOG DATA
if (F) {
    source("Functions/Functions_filtering.R")
    # ENTREES
    # Un dossier contenant les trajectoires brutes, au format csv issu des colliers catlog, rangées dans des sous-dossiers au nom de leurs alpages. Coordonnées en WSG84. Le nom des fichiers, sous la forme "ID_quelquechose.csv", sera utilisé pour déterminer l’ID du collier qui doit comporter 3 caractères.
    raw_data_dir = paste0("/home/moamo/These/4 - STrouMPH/Donnees_GPS/Colliers_",YEAR,"_brutes/")
    # Un fichier contenant les informations sur chaque individu équipé, les dates de pose et de retrait des colliers, ainsi que la proportion de temps pour laquelle les colliers sont programmés pour être allumés (18h par jour = 0.75). Doit contenir les colonnes "Collier", "Alpage", "Espece", "Race", "date_pose", "date_retrait" et "proportion_jour_allume"
    IIF = paste0(raw_data_dir,YEAR,"_colliers_poses.csv")
    # Les alpages à traiter
    alpages = c("Grande-Cabane")
    # SORTIES
    # Un .RDS contenant les trajectoires filtrées (les nouvelles trajectoires sont ajoutées à la suite des trajectoires traitées précédemment). Coordonnées en Lambert93.
    output_rds_file = paste0(data_dir,"Catlog_",YEAR,"_filtered.rds")
    # Un .csv contenant les performances des colliers (pourcentages de points éliminés à chaque étape, colliers défectueux...)
    indicator_file = paste0("/home/moamo/These/4 - STrouMPH/Donnees_GPS/catlog_",YEAR,"_filtering.csv")

    
    for (alpage in alpages) {
        print(paste("WORKING ON ALPAGE", alpage))
        collar_dir = paste0(raw_data_dir,alpage,"/")
        collar_files = list.files(collar_dir, pattern = ".csv")

        medcrit = get_alpage_info(alpage, AIF, "medcrit")
        meancrit = get_alpage_info(alpage, AIF, "meancrit")
        spikesp = get_alpage_info(alpage, AIF, "spikesp")
        spikecos = as.numeric(gsub(",", ".", get_alpage_info(alpage, AIF, "spikecos")))
        print(paste0("Bjorneraas filter parameters: medcrit=",medcrit,", meancrit=", meancrit, ", spikesp=", spikesp, ", spikecos=", spikecos))
        
        # Filter each trajectory and compute loggers’ performance indicators
        indicators = lapply(collar_files, function(collar)
                            filter_one_collar( load_catlog_data(paste0(collar_dir,collar)),
                                collar, output_rds_file, alpage, beg_date, end_date, IIF,
                                bjoneraas.medcrit=medcrit, bjoneraas.meancrit=meancrit, bjoneraas.spikesp=spikesp, bjoneraas.spikecos=spikecos )
            ) %>%
            do.call(rbind, .)
        indicators_tot = indicators %>%
            filter(worked_until_end == 1) %>% # to compute performance indicators at the alpage level, we remove defective collars
            add_row(name = paste("TOTAL", alpage), worked_until_end = sum(.$worked_until_end), nloc = NA,
                    R1error = NA, R2error = NA,
                    error_perc = sum(.$nloc*.$error_perc)/sum(.$nloc), localisation_rate = mean(.$localisation_rate))
        indicators = rbind(indicators, indicators_tot[nrow(indicators_tot),])

        write.table(indicators, file=indicator_file, append = T, sep=',', row.names=F, col.names=F)
    }
}


### 3. HMM FITTING ###
#--------------------#
if (F) {
    library(snow)
    library(stats)
    # Movement modelling packages
    library(momentuHMM)
    # library(foieGras)
    library(adehabitatLT)
    library(adehabitatHR)
    # Libraries RMarkdown
    library(knitr)
    library(rmarkdown)
    source("Functions/Functions_HMM_fitting.R")

    # ENTREES
    # Un .RDS contenant les trajectoires filtrées
    input_rds_file = paste0(data_dir,"Catlog_",YEAR,"_filtered.rds")
    # Un data.frame contenant la correspondance entre colliers et alpages. Doit contenir les colonnes  "ID", "Alpage" et "Periode d’echantillonnage"
    individual_info_file <- paste0(data_dir,"Colliers_",YEAR,"_brutes/",YEAR,"_colliers_poses.csv")
    # Les alpages à traiter
    alpages = ALPAGES

    # SORTIES
    # Un .RDS contenant les trajectoires catégorisées par comportement (les nouvelles trajectoires sont ajoutées à la suite des trajectoires traitées précédemment)
    output_rds_file = paste0(data_dir,"Catlog_",YEAR,"_viterbi.rds")

    ### LOADING DATA FOR ANALYSES
    data = readRDS(input_rds_file)
    data = data[data$species == "brebis",]
    data = data[data$alpage %in% alpages,]

    ### HMM FIT
    run_parameters = list(
        # Model
        model = "HMM",

        # Resampling
        resampling_ratio = 5,
        resampling_first_index = 0,
        rollavg = FALSE,
        rollavg_convolution = c(0.15, 0.7, 0.15),
        knownRestingStates = FALSE,

        # Observation distributions (step lengths and turning angles)
        dist = list(step = "gamma", angle = "vm"),
        # Design matrices to be used for the probability distribution parameters of each data stream
        DM = list(angle=list(mean = ~1, concentration = ~1)),
        # Covariants formula
        covariants = ~cos(hour*3.141593/12), # ~1 if no covariants used

        # 3-state HMM
        Par0 = list(step = c(10, 25, 50, 10, 15, 40), angle = c(tan(pi/2), tan(0/2), tan(0/2), log(0.5), log(0.5), log(3))),
        fixPar = list(angle = c(tan(pi/2), tan(0/2), tan(0/2), NA, NA, NA))
    )
    run_parameters = scale_step_parameters_to_resampling_ratio(run_parameters)

    startTime = Sys.time()
    results = par_HMM_fit(data, run_parameters, ncores = ncores, individual_info_file, sampling_period = 120, output_dir)
    endTime = Sys.time()

    ### SUMMARIZE MODEL FITTING BY ALPAGE in rmarkdown PDFs
    parameters_df <- parameters_to_data.frame(run_parameters)

    individual_IDs <- sapply(results, function(hmm) hmm$data$ID[1])
    individual_alpages <- get_individual_alpage(individual_IDs, individual_info_file)
    for (alpage in unique(individual_alpages)) {
        res_index  <- individual_alpages == alpage
        data_alpage <- do.call("rbind", lapply(results[res_index], function(result) result$data))
        results_df <- do.call("rbind", lapply(results[res_index], hmm_result_to_data.frame))

        runs_to_pdf(alpage, parameters_df, results_df, data_alpage , paste(round(difftime(endTime, startTime, units='mins'),2), "min"), output_dir, paste0(output_dir,"1 HMM Fit/HMM_fitting_",alpage,".pdf"), show_performance_indicators = FALSE)
    }

    ### SAVE RESULTING TRAJECTORIES
    data_hmm <- do.call("rbind", lapply(results, function(result) result$data))
    viterbi_trajectory_to_rds(data_hmm, output_rds_file, individual_info_file)
}


### 4.1. FLOCK STOCKING RATE (charge) BY DAY AND BY STATE ###
#-------------------------------------------------------------#
if (F) {
    library(adehabitatHR)
    library(data.table)
    library(snow)
    source("Functions/Functions_map_plot.R")
    source("Functions/Functions_flock_density.R")
    # ENTREES
    # Un .RDS contenant les trajectoires catégorisées par comportement
    input_rds_file = paste0(data_dir,"Catlog_",YEAR,"_viterbi.rds")
    # Un data.frame contenant les tailles de troupeaux, le pourcentage de temps d’allumage des colliers sur une journée et le chemin de la carte de phénologie phenOTB (dont la grille sert de base à l’analyse raster).
    # Un data.frame contenant les évolutions des tailles de troupeaux en fonction de la date (une ligne par évolution).
    # Doit contenir les colonnes  "alpage", "date_debut_periode", "taille_totale_troupeau"
    flock_size_file <- paste0(data_dir,YEAR,"_tailles_troupeaux.csv")
    # Les alpages à traiter
    alpages = ALPAGES

    # SORTIES
    # Dossier de sortie
    save_dir = paste0(data_dir,"Chargements_calcules/")
    # Un .RDS par alpage contenant les charges journalières par comportement
    state_daily_rds_prefix = paste0("by_day_and_state_",YEAR,"_")
    # Un .RDS par alpage contenant les charges journalières
    daily_rds_prefix = paste0("by_day_",YEAR,"_")
    # Un .RDS par alpage contenant les charges par comportement
    state_rds_prefix = paste0("by_state_",YEAR,"_")
    # Un .RDS par alpage contenant la charge totale sur toute la saison
    total_rds_prefix = paste0("total_",YEAR,"_")


    h <- 25 # Distance caractéristique pour calculer le chargement, écart-type de la gaussienne 2D sur laquelle chaque point est "dilué"

    for (alpage in alpages) {
        flock_sizes <- get_flock_size_through_time(alpage, flock_size_file)
        prop_time_collar_on <- get_alpage_info(alpage, AIF, "proportion_jour_allume")

        data <- readRDS(input_rds_file)
        data <- data[data$alpage == alpage,]

        pheno_t0 <- get_raster_cropped_L93(get_alpage_info(alpage, AIF, "chemin_carte_phenologie"), get_minmax_L93(data, 100), reproject = T, band = 2, as = "SpatialPixelDataFrame") # The flock load will be computed on the same grid
        flock_load_by_day_and_state_to_rds_kernelbb(data, pheno_t0, save_dir, paste0(state_daily_rds_prefix,alpage,".rds"), flock_sizes, prop_time_collar_on) #Creates the save_rds file
        rm(data)

        charge <- readRDS(paste0(save_dir,state_daily_rds_prefix,alpage,".rds"))

        # By state
        charge_state <- charge %>%
            group_by(x,y,state) %>%
            summarise(Charge = sum(Charge, na.rm = T), .groups = 'drop') %>%
            as.data.frame()
        save_file <- paste0(save_dir,state_rds_prefix,alpage,".rds")
        saveRDS(charge_state, file = save_file)
        rm(charge_state)

        # By day
        # charge_day <- charge %>%
        #     group_by(x,y,day) %>%
        #     summarise(Charge = sum(Charge, na.rm = T), .groups = 'drop') %>%
        #     as.data.frame()
        # save_file <- paste0(save_dir,daily_rds_prefix,alpage,".rds")
        # saveRDS(charge_day, file = save_file)
        # rm(charge_day)
        # Alternative if the data.frame is too large :
        charge_day = lapply(unique(charge$day), function(d) {charge  %>%
                                                                filter(day == d) %>%
                                                                group_by(x,y,day) %>%
                                                                summarise(Charge = sum(Charge, na.rm = T), .groups = 'drop') } )
        charge_day = as.data.frame(rbindlist(charge_day, use.names=TRUE))
        save_file <- paste0(save_dir,daily_rds_prefix,alpage,".rds")
        saveRDS(charge_day, file = save_file)
        rm(charge_day)
        
        # Total
        charge_tot <- charge %>%
            group_by(x,y) %>%
            summarise(Charge = sum(Charge, na.rm = T), .groups = 'drop') %>%
            as.data.frame()
        save_file <- paste0(save_dir,total_rds_prefix,alpage,".rds")
        saveRDS(charge_tot, file = save_file)
        rm(charge_tot)

        rm(charge)
    }
}


### 4.2. IF NEEDED: READAPT FLOCK STOCKING RATE TO NEW FLOCK SIZES ###
#--------------------------------------------------------------------#
if (F) {
    # Permet de recalculer un chargement déjà calculer pour l’adapter à une nouvelle estimation des évolutions de la taille
    # du troupeau, sans pour autant relancer le lourd calcul du home-range.
    source("Functions/Functions_flock_density.R")
    # ENTREES
    # Un data.frame contenant les tailles de troupeaux, le pourcentage de temps d’allumage des colliers sur une journée
    # et le chemin de la carte de phénologie phenOTB (dont la grille sert de base à l’analyse raster).
    # Un data.frame contenant les évolutions des tailles de troupeaux en fonction de la date (une ligne par).
    # Doit contenir les colonnes  "alpage", "date_debut_periode", "taille_totale_troupeau"
    flock_size_file <- paste0(data_dir,YEAR,"_tailles_troupeaux.csv")
    # Les alpages à traiter
    alpages = ALPAGES
    alpages = c("Viso")

    # SORTIES
    # Dossier de sortie
    save_dir = paste0(data_dir,"Chargements_calcules/")
    # Un .RDS par alpage contenant les charges journalières par comportement
    state_daily_rds_prefix = paste0("by_day_and_state_",YEAR,"_")
    # Un .RDS par alpage contenant les charges journalières
    daily_rds_prefix = paste0("by_day_",YEAR,"_")
    # Un .RDS par alpage contenant les charges par comportement
    state_rds_prefix = paste0("by_state_",YEAR,"_")
    # Un .RDS par alpage contenant la charge totale sur toute la saison
    total_rds_prefix = paste0("total_",YEAR,"_")


    h <- 25 # Distance caractéristique pour calculer le chargement, écart-type de la gaussienne 2D sur laquelle chaque point est "dilué"

    for (alpage in alpages) {
        flock_sizes <- get_flock_size_through_time(alpage, flock_size_file)
        prop_time_collar_on <- get_alpage_info(alpage, AIF, "proportion_jour_allume")

        charge <- readRDS(paste0(save_dir,state_daily_rds_prefix,alpage,"_NEW.rds"))
        s2 = sum(charge$Charge)/100
        s2
        charge <- readRDS(paste0(save_dir,state_daily_rds_prefix,alpage,".rds"))
        s1 = sum(charge$Charge)/100
        s1

        sum(flock_sizes[1:284])*0.75

        # Recompute stocking rates by state and day
        charge <- charge %>%
                    group_by(day) %>%
                    group_modify(function(charge_init, d) {
                        recompute_daily_flock_load_by_state(charge_init, flock_sizes[as.numeric(d)], prop_time_collar_on)},
                        .keep = FALSE ) %>%
                    ungroup() %>%
                    as.data.frame()
        save_file <- paste0(save_dir,state_daily_rds_prefix,alpage,"_NEW.rds")
        saveRDS(charge, file = save_file)

        # By state
        charge_state <- charge %>%
            group_by(x,y,state) %>%
            summarise(Charge = sum(Charge, na.rm = T), .groups = 'drop') %>%
            as.data.frame()
        save_file <- paste0(save_dir,state_rds_prefix,alpage,".rds")
        saveRDS(charge_state, file = save_file)
        rm(charge_state)

        # By day
        charge_day <- charge %>%
            group_by(x,y,day) %>%
            summarise(Charge = sum(Charge, na.rm = T), .groups = 'drop') %>%
            as.data.frame()
        save_file <- paste0(save_dir,daily_rds_prefix,alpage,".rds")
        saveRDS(charge_day, file = save_file)
        rm(charge_day)
        # Alternative if the data.frame is too large :
        # charge_day = lapply(unique(charge$day), function(d) {charge  %>%
        #                                                         filter(day == d) %>%
        #                                                         group_by(x,y,day) %>%
        #                                                         summarise(Charge = sum(Charge, na.rm = T), .groups = 'drop') } )
        # charge_day = as.data.frame(rbindlist(charge_day, use.names=TRUE))
        # save_file <- paste0(save_dir,daily_rds_prefix,alpage,".rds")
        # saveRDS(charge_day, file = save_file)
        # rm(charge_day)
        
        # Total
        charge_tot <- charge %>%
            group_by(x,y) %>%
            summarise(Charge = sum(Charge, na.rm = T), .groups = 'drop') %>%
            as.data.frame()
        save_file <- paste0(save_dir,total_rds_prefix,alpage,".rds")
        saveRDS(charge_tot, file = save_file)
        rm(charge_tot)

        rm(charge)
    }
}


### 5. TRAJECTORIES ANIMATION ###
#-------------------------------#
if (F) {
    # IMPORTANT pour une raison inconnue, maptiles ne fonctionne pas avec le calcul paralélisé par snow,
    #           il est donc nécessaire de choisir BACKGROUND_TYPE = "BDALTI" dans Functions_map_plot.R
    # REMARQUE : Pour les alpages les plus étendus, pour éviter que R ne crashe,
    #            il faut, en plus d’une grande partition SWAP, réduire le nombre de cœurs
    #            utilisés pour le calcul (2 fonctionne pour le Pelvas)
    #            Il semblerait que cela évite de consommer trop de RAM à la fois, au prix d’un calcul plus lent
    library(snow)
    source("Functions/Functions_Animation.R")
    BACKGROUND_TYPE = "BDALTI"
    source("Functions/Functions_map_plot.R")
    pdf(file = NULL) # do not show plots
    # ENTREES
    # Un .RDS contenant les trajectoires catégorisées par comportement
    input_rds_file = paste0(data_dir,"Catlog_",YEAR,"_viterbi.rds")
    # Un .RDS par alpage contenant les charges journalières
    daily_rds_prefix = paste0(data_dir,"Chargements_calcules/by_day_",YEAR,"_")
    # Les alpages à traiter
    alpages = ALPAGES
    alpages = c("Grande-Cabane", "Lanchatra", "Rouanette", "Sanguiniere", "Vacherie-de-Roubion", "Viso")

    # SORTIES
    # monthly videos of trajectories and loads in output_dir/alpage_name

    time_per_frame <- 30*60 # in seconds
    remanance_time <- 24*60*60 # in seconds


    for(alpage in alpages) {
        print(paste("WORKING ON ALPAGE", alpage))
        temporary_png_folder = paste0(output_dir,alpage,"/animation/")

        data_tot <- readRDS(input_rds_file)
        data_tot <- data_tot[data_tot$alpage == alpage,]
        data_tot$month = format(data_tot$time, format = "%m")
        data_tot$day = yday(data_tot$time)

        charge <- readRDS(paste0(daily_rds_prefix,alpage,".rds"))

        charge <- charge %>%
                group_by(x, y) %>%
                arrange(day)  %>%
                dplyr::mutate(cumulative_charge = cumsum(Charge)) %>%
                as.data.frame()

        for (month in unique(data_tot$month)) {
            print(paste("WORKING ON MONTH", month))
            unlink(paste0(output_dir,alpage,"/animation/"), recursive=TRUE)
            dir.create(paste0(output_dir,alpage,"/animation/"))

            # Filter data to get only the current month
            data = data_tot[data_tot$month == month,]
            # Map background
            ombrage <- get_ombrage(get_minmax_L93(charge, 0))

            generate_all_frames_parallelized(data, charge, ombrage, time_per_frame, remanance_time, temporary_png_folder)

            pngs_to_mp4(pngs_folder = temporary_png_folder, 
                        output_video_file = paste0(output_dir,alpage,"/Video_",alpage,"_",month,"_",YEAR,".mp4"))

        }
    }

    dev.off()
}


### 6.1. FLOCK PRESENCE ###
#-------------------------#
if (F) {
    library(adehabitatHR)
    source("Functions/Functions_map_plot.R")
    source("Functions/Functions_flock_density.R")
    pdf(file = NULL) # do not show plots
    # ENTREES
    # Un .RDS contenant les trajectoires catégorisées par comportement
    input_rds_file = paste0(data_dir,"Catlog_",YEAR,"_viterbi.rds")
    # Un data.frame contenant les évolutions des tailles de troupeaux en fonction de la date (une ligne par).
    # Doit contenir les colonnes  "alpage", "date_debut_periode", "taille_totale_troupeau"
    flock_size_file <- paste0(data_dir,YEAR,"_tailles_troupeaux.csv")
    # Les alpages à traiter
    alpages = ALPAGES
    
    h <- 25 # Distance caractéristique pour calculer le chargement, écart-type de la gaussienne 2D sur laquelle chaque point est "dilué"

    for (alpage in alpages) {
        data <- readRDS(input_rds_file)
        data = data[data$alpage == alpage,]

        # Plot flock presence during 2-week period
        date_breaks = yday(as.POSIXct(c(paste0("16/06/",YEAR), paste0("01/07/",YEAR), paste0("16/07/",YEAR), paste0("01/08/",YEAR), paste0("16/08/",YEAR), paste0("01/09/",YEAR), paste0("16/09/",YEAR)), format="%d/%m/%Y"))
        date_labs = c("avant 15 jun", "16 - 30 jun", "1 - 15 jul", "16 - 30 jul", "1 - 15 aou", "16 - 30 aou", "1 - 15 sep", "après 16 sep")
        period_breaks = seq(0, length(date_breaks))

        ombrage = get_ombrage(get_minmax_L93(data, 100))
        UP = get_UP_polygon(alpage, AIF, UP_file)

        data %>% # Taking into account the fact that the time is in UTC
            mutate(month_period = factor(date_labs[findInterval(yday(time), date_breaks)+1], levels=date_labs)) %>% 
        ggplot(aes(x=x, y=y, col=month_period) ) +
            add_ombrage_layer(ombrage, UP=NULL) +
            stat_density_2d(breaks = c(5e-08), show.legend=T, geom = "polygon", alpha = 0.4, aes(fill=month_period))+
            geom_density_2d(breaks = c(5e-08), show.legend=F, alpha = 0.6) +
            scale_colour_viridis_d(name = "", drop=F) +
            scale_fill_viridis_d(name = "", drop=F)
        save_ggplot(paste0(output_dir,alpage,"/Presence_by_date.png"))

        # Plot flock load depending on day hour
        pheno_t0 <- get_raster_cropped_L93(get_alpage_info(alpage, AIF, "chemin_carte_phenologie"), get_minmax_L93(data, 100), reproject = T, band = 2, as = "SpatialPixelDataFrame") # The flock load will be computed on the same grid
        day_beg = yday(min(data$time))
        day_end = yday(max(data$time))
        flock_size <- mean(get_flock_size_through_time(alpage, flock_size_file)[day_beg:day_end])
        prop_time_collar_on <- get_alpage_info(alpage, AIF, "proportion_jour_allume")

        data <- data %>% # Taking into account the fact that the time is in UTC
            mutate(day_period = case_when(hour(time) < 8 ~ "00h-10h", hour(time) >= 18 ~ "20h-00h", .default = "10h-20h"))
        data <- SpatialPointsDataFrame(data[, c("x", "y")], data)
        proj4string(data) <- CRS_L93
        UD <- kernelUD(data["day_period"], grid = pheno_t0, h = h) # CAUTION here we don’t use a brownian bridge, but a simple gaussian kernel, which is less computing intensive
        n_points_total = nrow(data) # There is no NA locations : they were previously removed

        charge_by_time = list()
        for (period in unique(data$day_period)) {
            n_points_period = sum(data$day_period == period)
            charge_period = flock_load_from_daily_and_state_UD(UD[[as.character(period)]], n_points_period, n_points_total, flock_size, prop_time_collar_on)
            charge_period$day_period = period
            charge_by_time = append(charge_by_time, list(charge_period))
        }
        charge_by_time <- do.call("rbind", charge_by_time)
        charge_by_time$Charge = charge_by_time$Charge*(day_end - day_beg)

        plot_charge(charge_by_time, ombrage, "", charge_max = set_scale_max_level(charge_by_time$Charge), UP) +
            facet_grid(. ~ day_period) +
            colourbar_bottom()
        save_ggplot(paste0(output_dir,alpage,"/Presence_by_hour.png"))
    }
}


### 6.2. LOAD VS. STATE ###
#-------------------------#
if (F) {
    source("Functions/Functions_map_plot.R")
    source("Functions/Functions_vegetation.R")
    library("tidyterra")
    library("gg.layers")
    pdf(file = NULL) # do not show plots
    # ENTREES
    # Un .RDS par alpage contenant les charges journalières
    daily_rds_prefix = paste0(data_dir,"Chargements_calcules/by_day_",YEAR,"_")
    # Un .RDS par alpage contenant les charges par comportement
    state_rds_prefix = paste0(data_dir,"Chargements_calcules/by_state_",YEAR,"_")
    # Un .RDS par alpage contenant la charge totale sur toute la saison
    total_rds_prefix = paste0(data_dir,"Chargements_calcules/total_",YEAR,"_")
    # Les alpages à traiter
    alpages = ALPAGES
    alpages = "Ane-et-Buyant"

    # SORTIES
    # figures in output_dir/alpage_name

    for (alpage in alpages) {
        charge_tot = readRDS(paste0(total_rds_prefix,alpage,".rds"))
        charge_state = readRDS(paste0(state_rds_prefix,alpage,".rds"))
        xy_minmax <-  get_minmax_L93(charge_tot, 1)
        ombrage <- get_ombrage(xy_minmax)
        UP <- get_UP_polygon(alpage, AIF, UP_file)

        # Plot total load over the whole season
        plot_charge(charge_tot, ombrage, title = "Total", charge_max = 800, UP)
        save_ggplot(paste0(output_dir,alpage,"/HR_Total.png"))

        # Plot load by state over the whole season
        plot_charge(charge_state[charge_state$state == "Repos",], ombrage, "Repos", charge_max = set_scale_max_level(charge_state[charge_state$state == "Repos","Charge"], quantile = 0.995,), UP) +
            colourbar_bottom("short") +
            theme(plot.title = element_text(size=11)) +
        plot_charge(charge_state[charge_state$state == "Paturage",], ombrage, "Pâturage", charge_max = set_scale_max_level(charge_state[charge_state$state == "Paturage","Charge"]), UP) +
            colourbar_bottom("short") +
            theme(axis.text.y = element_blank(), legend.title=element_blank(), plot.title = element_text(size=11)) +
        plot_charge(charge_state[charge_state$state == "Deplacement",], ombrage, "Déplacement", charge_max = set_scale_max_level(charge_state[charge_state$state == "Deplacement","Charge"]), UP) +
            colourbar_bottom("short") +
            theme(axis.text.y = element_blank(), legend.title=element_blank(), plot.title = element_text(size=11))
        save_ggplot(paste0(output_dir,alpage,"/HR_States.png"))
    }
    dev.off()
}


### 7. LOAD VS. VEGETATION ###
#----------------------------#
if (F) {
    source("Functions/Functions_map_plot.R")
    source("Functions/Functions_vegetation.R")
    library("tidyterra")
    pdf(file = NULL) # do not show plots
    # ENTREES
    # Un .RDS par alpage contenant les charges journalières
    daily_rds_prefix = paste0(data_dir,"Chargements_calcules/by_day_",YEAR,"_")
    # Un .RDS par alpage contenant les charges par comportement
    state_rds_prefix = paste0(data_dir,"Chargements_calcules/by_state_",YEAR,"_")
    # Un .RDS par alpage contenant la charge totale sur toute la saison
    total_rds_prefix = paste0(data_dir,"Chargements_calcules/total_",YEAR,"_")
    # Les alpages à traiter
    alpages = ALPAGES

    # SORTIES
    # figures in output_dir/alpage_name

    for (alpage in alpages) {
        if (get_alpage_info(alpage, AIF, "chemin_carte_vegetation") != "") {
            UP <- get_UP_polygon(alpage, AIF, UP_file)
            cropping_polygon = terra::buffer(UP, 300) # buffer around the UP polygon to account for outside UP forraging
            charge_tot = readRDS(paste0(total_rds_prefix,alpage,".rds")) %>%
                crop_data.frame(cropping_polygon)
            charge_state = readRDS(paste0(state_rds_prefix,alpage,".rds")) %>%
                crop_data.frame(cropping_polygon, names_col = "state", values_col = "Charge")
            habitats = get_vegetation_rasterized(get_alpage_info(alpage, AIF, "chemin_carte_vegetation"), get_alpage_info(alpage, AIF, "typologie_vegetation"), charge_tot[c("x", "y")], cropping_polygon)
            habitat_scale = get_habitat_scale(get_alpage_info(alpage, AIF, "typologie_vegetation"))
            xy_minmax <-  get_minmax_L93(charge_tot, 0)
            ombrage <- get_ombrage(xy_minmax)

            # Plot vegetation map
            plot_raster_over_ombrage(habitats[!is.na(habitats$vegetation_type), ], "vegetation_type", habitat_scale, 0.6, ombrage, "", "Milieux pastoral", ) + 
                theme(legend.key.height = NULL,
                      legend.key.width = NULL) #To restaure default dimensions, more elegant for a discrete colourbar
            save_ggplot(paste0(output_dir,alpage,"/Milieux.png"))

            habitats_2 = get_vegetation_rasterized("/home/moamo/These/Cartographie Végétation/Random Forest/Cartographie_v2/Classifications_fusion_ColorIndexed_sc1_landforms_mnh.tif", get_alpage_info(alpage, AIF, "typologie_vegetation"), charge_tot[c("x", "y")], cropping_polygon)
            plot_raster_over_ombrage(habitats_2[!is.na(habitats_2$vegetation_type), ], "vegetation_type", habitat_scale, 0.6, ombrage, "", "Milieux pastoral", ) + 
                theme(legend.key.height = NULL,
                      legend.key.width = NULL) #To restaure default dimensions, more elegant for a discrete colourbar
            save_ggplot(paste0(output_dir,alpage,"/Milieux_RF_v2_sc1.png"))

            # Plot loads by vegetation type
            barplot_load_by_vegetation(charge_tot, habitats, ombrage)
            ggsave(paste0(output_dir,alpage,"/Chargement_par_habitat.png"), width = 8, height = 6)

            barplot_load_by_vegetation_state(charge_state, habitats, ombrage)
            ggsave(paste0(output_dir,alpage,"/Chargement_par_habitat_par_etat.png"), width = 8, height = 6)

            boxplot_load_by_vegetation(charge_tot, habitats, ombrage)
            ggsave(paste0(output_dir,alpage,"/Charge_par_habitat.png"), width = 4, height = 4)

            # Plot load by vegetation unit
            vegetation_units <- get_vegetation_polygons(get_alpage_info(alpage, AIF, "chemin_carte_vegetation"), get_alpage_info(alpage, AIF, "typologie_vegetation"), charge_tot[c("x", "y")], cropping_polygon)

            charge_rast <- rast(charge_tot, crs = CRS_L93)
            vegetation_units$sheepDays <- terra::extract(charge_rast, vegetation_units, fun = sum, na.rm = T, ID = F)
            pixel_surface = get_pixel_surface(charge_tot)
            vegetation_units$sheepDays <- vegetation_units$sheepDays * pixel_surface / 10000 # going from to animals.days/ha to animals.days/pixel
            vegetation_units$charge <- terra::extract(charge_rast, vegetation_units, fun = mean, na.rm = T, ID = F)

            charge_max = set_scale_max_level(vegetation_units$sheepDays)
            plot_charge_by_polygon(vegetation_units, "sheepDays", charge_max, ombrage, "Brebis.jours par unité de végétation", "brebis.jours", UP) +
                colourbar_right()
            save_ggplot(paste0(output_dir,alpage,"/Chargement_par_unite.png"))

            charge_max = set_scale_max_level(vegetation_units$charge, 0.99)
            plot_charge_by_polygon(vegetation_units, "charge", charge_max, ombrage, "Charge moyenne par unité de végétation", "brebis.jours/ha", UP) +
                colourbar_right()
            save_ggplot(paste0(output_dir,alpage,"/Charge_par_unite.png"))
        }
    }

    dev.off()
}


### 8. LOAD VS. PHENOLOGY ###
#---------------------------#
if (F) {
    source("Functions/Functions_phenology.R")
    source("Functions/Functions_vegetation.R")
    source("Functions/Functions_map_plot.R")
    pdf(file = NULL) # do not show plots
    # ENTREES
    # Un .RDS par alpage contenant les charges journalières calculées sur la même grille que la carte de phénologie
    daily_rds_prefix = paste0(data_dir,"Chargements_calcules/by_day_",YEAR,"_")
    # Les alpages à traiter
    alpages = ALPAGES

    # SORTIES
    # figures in output_dir/alpage_name


    for (alpage in alpages) {
        charge = readRDS(paste0(daily_rds_prefix,alpage,".rds"))

        xy_minmax <-  get_minmax_L93(charge, 1) # necessary to take a 1m buffer to avoid pheno being smaller than charge due to roundings
        ombrage <- get_ombrage(xy_minmax)
        UP = get_UP_polygon(alpage, AIF, UP_file)
        pheno <- get_raster_cropped_L93(get_alpage_info(alpage, AIF, "chemin_carte_phenologie"), xy_minmax, reproject = T, band = c(2,3,4), as = "data.frame")
        colnames(pheno) <- c("x", "y", "t0", "t1", "t2")
        pheno <- pheno[order(pheno$x, pheno$y), ] #+ 3*7

        charge_by_season <- load_and_perc_by_vegetation_period(pheno, charge)

        # Plot load percentage by vegetation period (for each pixel, the percentage of its load during the each period, the whole season being 100%)
        charge_by_season %>%
            filter(total >= 0.1) %>%
            pivot_longer(cols=c("perc_growing", "perc_plateau", "perc_senesc"), names_to='Period', values_to='Presence percent') %>%
            mutate(Period = factor(case_when(Period == "perc_growing" ~ "Pousse", Period == "perc_plateau" ~ "Maturité", .default = "Dépérissement"), levels = c("Pousse", "Maturité", "Dépérissement"))) %>%
            # mutate(Period = case_when(Period == "perc_growing" ~ "Growing", Period == "perc_plateau" ~ "Plateau", .default = "Senescence")) %>%
        plot_perc(., "Presence percent", ombrage, "", UP) +
            colourbar_bottom() +
            facet_grid(. ~ Period)
        save_ggplot(paste0(output_dir,alpage,"/Presence_phenology.png"))
        # save_ggplot(paste0(output_dir,alpage,"/Presence_phenology-3weeks.png"))

        # Plot phenological times
        pheno_scale = scale_fill_stepsn(colours = c("#d7191c", "#fdae61", "#ffffc0", "#a6d96a", "#a6d96a"),
                                        oob = scales::squish, # squish anything that exceeds the limits (out-of-bounds/oob values) to the nearest extreme
                                        limits = c(yday(as.POSIXct(paste0("05/01/",YEAR),format="%m/%d/%Y")),
                                                    yday(as.POSIXct(paste0("10/01/",YEAR),format="%m/%d/%Y"))),
                                        breaks = c(121, 136, 152, 167, 182, 197, 213, 228, 244, 259, 274),
                                        labels = c("May 1", "", "Jun 1", "", "Jul 1", "", "Aug 1", "", "Sept 1", "", "Oct 1"))
        pheno %>%
            pivot_longer(cols=c("t0", "t1", "t2"), names_to='Time', values_to='Date') %>%
        plot_raster_over_ombrage(., "Date", pheno_scale, 0.7, ombrage, "", "Date", UP) +
            colourbar_bottom() +
            facet_grid(. ~ Time)
        save_ggplot(paste0(output_dir,alpage,"/Phenology.png"))


        # NE S’EXECUTE PAS POUR LES ALPAGES SANS CARTE DE VEGETATION
        if (get_alpage_info(alpage, AIF, "chemin_carte_vegetation") != "") {
            # Plot load by vegetation period and habitat
            vegetation_units <- get_vegetation_polygons(get_alpage_info(alpage, AIF, "chemin_carte_vegetation"), get_alpage_info(alpage, AIF, "typologie_vegetation"), pheno[c("x","y")], UP)
            vegetation_units[is.na(vegetation_units$vegetation_type),] = "Autres"
            
            habitat_labs <- get_habitat_labs(vegetation_units)
            pheno_veget <- charge_by_season %>%
                            dplyr::select("x", "y", "growing", "plateau", "senesc") %>%
                            rename("1"="growing", "2"="plateau", "3"="senesc") %>%
                            get_chargement_by_period_and_vegetation_units(vegetation_units, c('Growing', 'Plateau', 'Senescence'))

            plot_presence_perc_by_period_and_habitat(pheno_veget, "Saison de végétation", c('Growing', 'Plateau', 'Senescence'), habitat_labs)
            ggsave(paste0(output_dir,alpage,"/Chargement_par_saison_et_habitat.png"), width = 8, height = 6)
            plot_charge_by_period_and_habitat(pheno_veget, "Saison de végétation", c('Growing', 'Plateau', 'Senescence'), habitat_labs)
            ggsave(paste0(output_dir,alpage,"/Charge_par_saison_et_habitat.png"), width = 8, height = 6)


            # Plot load by month and habitat
            breakdays = c(182, 213, 244)
            month_units = c("leq_June", "Jul", "Aug", "geq_Sept")
            month_labs = c("Juin ou avant", "Juillet", "Août", "Septembre ou après")
            pheno_veget <- get_charge_by_period(charge, breakdays) %>%
                        pivot_wider(names_from = period, values_from = charge) %>%
                        get_chargement_by_period_and_vegetation_units(vegetation_units, month_labs)
            
            plot_presence_perc_by_period_and_habitat(pheno_veget, "Mois", month_labs, habitat_labs)
            ggsave(paste0(output_dir,alpage,"/Chargement_par_mois_et_habitat.png"), width = 8, height = 6)
            plot_charge_by_period_and_habitat(pheno_veget, "Mois", month_labs, habitat_labs, title="")
            ggsave(paste0(output_dir,alpage,"/Charge_par_mois_et_habitat.png"), width = 8, height = 6)
        }
    }

    dev.off()
}


### 9. DISTANCE AND DENIVELATION WALKED BY THE SHEEP ##
#------------------------------------------------------#
if (F) {
    library(momentuHMM)
    source("Functions/Functions_HMM_fitting.R")
    pdf(file = NULL) # do not show plots
    # ENTREES
    # Un .RDS contenant les trajectoires (filtrées, éventuellement sous-échantillonnées)
    input_rds_file = paste0(data_dir,"Catlog_",YEAR,"_viterbi.rds")
    # Un raster d’altitude
    altitude_raster = paste0(raster_dir,"BDALTI/BDALTI.tif")
    # Les alpages à traiter
    alpages = ALPAGES

    # SORTIES
    # figures in output_dir/alpage_name


    for (alpage in alpages) {
        data <- readRDS(input_rds_file) %>%
                dplyr::filter(alpage == !!alpage) %>%
                dplyr::select(ID, time, x, y)
        altitude <- get_raster_cropped_L93(altitude_raster, get_minmax_L93(data, 20), reproject = FALSE, as = "spatRast")

        data <- prepData(data, type = "UTM", covNames = NULL) # To get step lengths

        data$altitude = terra::extract(altitude, vect(as.matrix(data[c("x","y")]), type="points", crs=CRS_L93), ID = FALSE)$BDALTI
        data$denivelation = append(0, pmax(diff(data$altitude), 0))
        data$day = yday(data$time)

        sum_without_first <- function(v) { # éliminer le premier élément de chaque groupe permet d’éviter les effet de bords où les distances et dénivelés sont calculés par rapport au jour précédent ou même au collier précédent
            return (sum(v[2:length(v)], na.rm = T))
        }

        data = data %>% group_by(ID, day) %>%
                summarise(date = first(as.Date(time)), distance = sum_without_first(step), denivelation = sum_without_first(denivelation)) %>%
                group_by(date) %>%
                summarise(distance = mean(distance, na.rm = T), denivelation = mean(denivelation, na.rm = T)) %>%
                pivot_longer(cols=c(distance, denivelation), names_to="measure", values_to="value") %>%
                mutate(measure = case_when(measure == "distance" ~ "Distance parcourue", .default = "Dénivelé positif parcouru"))
        ggplot(data, aes(x = date, y = value)) +
            geom_col() +
            xlab(NULL) +
            ylab("m") +
            facet_grid(rows="measure", scales="free")
        ggsave(paste0(output_dir,alpage,"/Distances.png"), height=4)
    }

    dev.off()
}


### 10. PDF SUMMARY FOR SHEPHERDS AND FARMERS ###
#----------------------------------------------#
if (F) {
    library(cowplot)
    # ENTREES
    # Le dossier contenant les figures, rangees par alpage dans des sous dossiers
    daily_rds_prefix = paste0(data_dir,"Chargements_calcules/by_day_",YEAR,"_")
    # Les alpages a traiter
    alpages = ALPAGES
    # Les annees a comparer dans un meme rapport.
    # Si le suivi n’est pas disponible pour les deux annees sur l’alpage, un rapport simple sera donné sur l’annee YEAR2.
    # Pour forcer la generation d’un rapport simple, mettre YEAR2 = YEAR1
    YEAR1 = 2022
    YEAR2 = 2023
    alpages = c("Crouzet", "Grande-Cabane", "Rouanette", "Vacherie-de-Roubion")

    # SORTIES
    # figures in output_dir/alpage_name


    for (alpage in "Vacherie-de-Roubion") {
        vegetation_typology = get_alpage_info(alpage, AIF, "typologie_vegetation")
        vegetation_STrouMPH_figures = vegetation_typology == "typology_STrouMPH"
        vegetation_teledetection_figures = vegetation_typology == "typology_teledetection"
        alpage_name = get_alpage_info(alpage, AIF, "nom_alpage_determinant")
        print(paste("Generation du rapport pour l’alpage", alpage_name))

        # Choix du rapport a generer : une seule annee ou comparaison de deux annees
        # INSERER ICI LES NOUVEAUX MODELES DE RAPPORTS POUR LES NOUVELLES ANNEES
        if (YEAR2 != YEAR1 & alpage %in% ALPAGES_TOTAL[[as.character(YEAR1)]] & alpage %in% ALPAGES_TOTAL[[as.character(YEAR2)]]) {
            output_dir1 <- paste0("/home/moamo/These/4 - STrouMPH/STrouMPH_R/Figures_Catlog_",YEAR1,"/")
            output_dir2 <- paste0("/home/moamo/These/4 - STrouMPH/STrouMPH_R/Figures_Catlog_",YEAR2,"/")
            output_pdf_file = paste0(output_dir,"Principaux_resultats_",alpage,"_",YEAR1,"_",YEAR2,".pdf")
            fig_dir1 = paste0(output_dir1,alpage,"/")
            fig_dir2 = paste0(output_dir2,alpage,"/")
            rmarkdown::render(paste0("Functions/Rendu_eleveurs_bergers_comparaison_",YEAR1,"_",YEAR2,".Rmd"), output_file = output_pdf_file)
        } else {
            output_pdf_file = paste0(output_dir,"Principaux_resultats_",alpage,"_",YEAR2,".pdf")
            fig_dir = paste0(output_dir,alpage,"/")
            rmarkdown::render(paste0("Functions/Rendu_eleveurs_bergers_",YEAR2,".Rmd"), output_file = output_pdf_file)
        }

        # Compression du PDF pour en reduire la taille
        R.utils::compressPDF(output_pdf_file, outPath = output_dir, compression="gs(printer)+qpdf", overwrite = T)
    }
}
