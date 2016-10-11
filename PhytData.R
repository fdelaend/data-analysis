PhytoData <- c("/Users/frederik/Documents/data from people/md2/PhytoOR.txt",
               "/Users/frederik/Documents/data from people/md3/phytoplanktonLinuron.txt",
               "/Users/frederik/Documents/data from people/chem stress studies/Relevant/Hamilton/Hamilton.txt",
               "/Users/frederik/Documents/data from people/chem stress studies/Relevant/Belanger/Belanger.txt",
               "/Users/frederik/Documents/data from people/chem stress studies/Relevant/Larson/LowT.txt",
               "/Users/frederik/Documents/data from people/chem stress studies/Relevant/Larson/MedT.txt",
               "/Users/frederik/Documents/data from people/chem stress studies/Relevant/Larson/HighT.txt",
               "/Users/frederik/Documents/data from people/chem stress studies/Relevant/Molander/MolanderChla.txt",
               "/Users/frederik/Documents/data from people/hartgers/phytoplankton.txt",
               "/Users/frederik/Documents/data from people/chem stress studies/Relevant/GoldsboroughSim/GoldsboroughSim.txt",
               "/Users/frederik/Documents/data from people/chem stress studies/Relevant/GoldsboroughTer/GoldsboroughTer.txt",
               "/Users/frederik/Documents/data from people/geest/phytoplankton.txt",
               "/Users/frederik/Documents/data from people/chem stress studies/Relevant/Nystrom/NystromChla.txt",
               "/Users/frederik/Documents/data from people/pvdb/me/phytoplankton.txt")
PhytoVs <- c("/Users/frederik/Documents/data from people/md2/PVolumes.txt",
               "/Users/frederik/Documents/data from people/md3/PVolumes.txt",
               NA, NA, NA, NA, NA, NA, "/Users/frederik/Documents/data from people/hartgers/PVolumes.txt",
               NA, NA, "/Users/frederik/Documents/data from people/geest/PVolumes.txt",
               NA, "/Users/frederik/Documents/data from people/pvdb/me/PVolumes.txt")

Substances <- c("linuron","linuron","atrazine","copper","pH","pH","pH","diuron",
                "mix", "simazin", "terazin", "linuron", "atrazine", "linuron")
Substances <- c(1,1,2,3,4,4,4,5,6,7,8,1,2,1)
#concs
Concs <- list(c(0, 15, 50, 150, 500), 
              c(0, 0.5, 5, 50, 150), 
              c(0), 
              c(0, 9, 26, 47, 98, 208), 
              c(30, 60, 90, 120, 150),
              c(30, 60, 90, 120, 150),
              c(30, 60, 90, 120, 150),
             c(1.6, 8, 40, 200, 1000),
             c(0, 0.01, 0.03, 0.1, 0.3, 1), 
             c(0), 
             c(0),
             c(0, 0.5, 5, 15, 50),
             c(0, 0.056, 0.1, 0.18, 0.32, 0.56, 1, 1.8, 3.2),
             c(0, 0.5, 5, 15, 50, 150))
