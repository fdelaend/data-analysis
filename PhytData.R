PhytData <- c("md2/phytoplankton.txt",
               "md2/periphyton.txt",
               "md3/phytoplanktonLinuron.txt",
               "Molander/periphyton.txt",
               "hartgers/phytoplankton.txt",
               "geest/periphyton.txt",
               "Nystrom/periphyton.txt",
               "pvdb/phytoplankton.txt",
               "Larson/HighT.txt","Larson/LowT.txt","Larson/MedT.txt",
               "Belanger/Belanger.txt")

PhytData  <- paste("/Users/frederik/Documents/data from people/",
                   PhytData, sep="")

Substances <- c(rep("Linuron",3), "Diuron", 
                "Herbicide mix", "Linuron",
                "Atrazine", "Linuron", rep("Salinity",3),
                "Copper")
Substances <- c(rep(1,3), 2, 3, 1, 4, 1, rep(5,3), 6)

#NOMINAL concentrations cause often fluctuating through time!
Concs <- list(c(0, 15, 50, 150, 500), c(0, 15, 50, 150, 500), 
              c(0, 0.5, 5, 50, 150), c(1.6, 8, 40, 200, 1000), 
              c(0, 0.01, 0.03, 0.1, 0.3, 1), 
              c(0, 0.5, 5, 15, 50),
              c(0, 0.056, 0.1, 0.18, 0.32, 0.56, 1, 1.8, 3.2),
              c(0, 0.5, 5, 15, 50, 150),
              c(30, 60, 90, 120, 150), c(30, 60, 90, 120, 150),
              c(30, 60, 90, 120, 150),
              c(0, 9, 26, 47, 98, 208))
