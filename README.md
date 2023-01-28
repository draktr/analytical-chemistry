# Analytical Chemistry Repository

Computational tools used in the field of Analytical Chemistry

All tools are well commented and documented so they can be reused easily, even by some with limited exposure to analytical chemistry.

## Contents

`buffer_capacity.R` contains code that calculates buffer capacity. Provided are the parameters for acetic acid calculations. Some everyday applications of buffers are use of baking soda (NaHCO3) to control the acidity in the swimming pools.

`manual_spectroscopy.R` contains code for pre-processing, processing, calibration, analysis and visualization of manually collected spectroscopy data. The script originates from an experiment where phone camera was used as spectometer, recording different colours of flames in for solutions of different concentrations. This script helped process that data and perform the analysis. Specialized spectometer is, indeed, more accurate and convenient, but this setup serves as a conceptual introduction to the process of spectroscopy.

`plate_theory.R` contains code presenting conceptual logic behind plate theory, in particular as introduction to distillation and chromatography.

`solving_chemical_equilibrium.R` contains code for solving an chemical equilibrium using `nleqslv` library. While code contains parameters for citric acid, the functions inside are practical guide and can be used to solve any other chemical equilibria.
