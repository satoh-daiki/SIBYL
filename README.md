# SIBYL: Simulation code powered by lattice dose-response functions

[![DOI](https://zenodo.org/badge/528699324.svg)](https://doi.org/10.5281/zenodo.20413118)

![SIBYL_LOGO](https://github.com/satoh-daiki/SIBYL/assets/100466085/fc0d915d-2890-4f73-8cf4-11fe01027217)

[SIBYL]( https://doi.org/10.1371/journal.pone.0245932) is a computer code that couples with an atmospheric dispersion model to calculate the dose distribution on the ground due to gamma rays from a radioactive plume. It can also take into account the shielding effect of gamma rays by buildings for dose assessment in uraban areas. The dose-response functions used in the calculations were evaluated using the general-purpose radiation transport code [PHITS](https://phits.jaea.go.jp/index.html). SIBYL has a graphical user interface and its input and output data can be visualized via [Paraview](https://www.paraview.org/). In addition, it is capable of parallel computing based on OpenMP and MPI technologies.

![ParaView-Input](https://github.com/satoh-daiki/SIBYL/assets/100466085/82815e13-157c-41ba-a3e5-6fd655597a17)
![ParaView-Output](https://github.com/satoh-daiki/SIBYL/assets/100466085/08bc1b88-a800-4b27-96d9-3a995230bead)
![Parallel-computation](https://journals.plos.org/plosone/article/figure/image?download&size=large&id=10.1371/journal.pone.0245932.g019)

## Directory structure
![structure](https://github.com/user-attachments/assets/96fce556-fdbe-4e8e-8b5f-baf5bab7856c)
## Usage
Please refer to the linked [PDF](https://github.com/satoh-daiki/SIBYL/blob/main/SIBYL-manual.pdf) file.

## Citation
D. Satoh, H. Nakayama, T. Furuta, T. Yoshihiro, K. Sakamoto, "Simulation code for estimating external gamma-ray doses from a radioactive plume and contaminated ground using a local-scale atmospheric dispersion model", PLOS ONE, 2021.  
https://doi.org/10.1371/journal.pone.0245932  

H. Nakayama, N. Onodera, D. Satoh, H. Nagai, Y. Hasegawa, Y. Idomura, "Development of local-scale high-resolution atmospheric dispersion and dose assessment system", Journal of Nuclear Science and Technology, 2021.  
https://doi.org/10.1080/00223131.2022.2038302

D. Satoh, H. Nakayama, T. Furuta, T. Yoshihiro, K. Sakamoto, "SIBYL" [Software, Version 2.5.0], Zenodo, 2026.  
https://doi.org/10.5281/zenodo.20413119

## License
Copyright (c) 2018-2026 Japan Atomic Energy Agency.

Licensed under the [MIT](https://github.com/satoh-daiki/SIBYL/blob/main/LICENSE.md) License.