Class11
================

``` r
library(bio3d)
```

``` r
pdb <- read.pdb("1hsg.pdb")
pdb
```

    ## 
    ##  Call:  read.pdb(file = "1hsg.pdb")
    ## 
    ##    Total Models#: 1
    ##      Total Atoms#: 1686,  XYZs#: 5058  Chains#: 2  (values: A B)
    ## 
    ##      Protein Atoms#: 1514  (residues/Calpha atoms#: 198)
    ##      Nucleic acid Atoms#: 0  (residues/phosphate atoms#: 0)
    ## 
    ##      Non-protein/nucleic Atoms#: 172  (residues: 128)
    ##      Non-protein/nucleic resid values: [ HOH (127), MK1 (1) ]
    ## 
    ##    Protein sequence:
    ##       PQITLWQRPLVTIKIGGQLKEALLDTGADDTVLEEMSLPGRWKPKMIGGIGGFIKVRQYD
    ##       QILIEICGHKAIGTVLVGPTPVNIIGRNLLTQIGCTLNFPQITLWQRPLVTIKIGGQLKE
    ##       ALLDTGADDTVLEEMSLPGRWKPKMIGGIGGFIKVRQYDQILIEICGHKAIGTVLVGPTP
    ##       VNIIGRNLLTQIGCTLNF
    ## 
    ## + attr: atom, xyz, seqres, helix, sheet,
    ##         calpha, remark, call

``` r
aa321(pdb$seqres)
```

    ##   [1] "P" "Q" "I" "T" "L" "W" "Q" "R" "P" "L" "V" "T" "I" "K" "I" "G" "G"
    ##  [18] "Q" "L" "K" "E" "A" "L" "L" "D" "T" "G" "A" "D" "D" "T" "V" "L" "E"
    ##  [35] "E" "M" "S" "L" "P" "G" "R" "W" "K" "P" "K" "M" "I" "G" "G" "I" "G"
    ##  [52] "G" "F" "I" "K" "V" "R" "Q" "Y" "D" "Q" "I" "L" "I" "E" "I" "C" "G"
    ##  [69] "H" "K" "A" "I" "G" "T" "V" "L" "V" "G" "P" "T" "P" "V" "N" "I" "I"
    ##  [86] "G" "R" "N" "L" "L" "T" "Q" "I" "G" "C" "T" "L" "N" "F" "P" "Q" "I"
    ## [103] "T" "L" "W" "Q" "R" "P" "L" "V" "T" "I" "K" "I" "G" "G" "Q" "L" "K"
    ## [120] "E" "A" "L" "L" "D" "T" "G" "A" "D" "D" "T" "V" "L" "E" "E" "M" "S"
    ## [137] "L" "P" "G" "R" "W" "K" "P" "K" "M" "I" "G" "G" "I" "G" "G" "F" "I"
    ## [154] "K" "V" "R" "Q" "Y" "D" "Q" "I" "L" "I" "E" "I" "C" "G" "H" "K" "A"
    ## [171] "I" "G" "T" "V" "L" "V" "G" "P" "T" "P" "V" "N" "I" "I" "G" "R" "N"
    ## [188] "L" "L" "T" "Q" "I" "G" "C" "T" "L" "N" "F"

``` r
head(pdb$atom)
```

    ##   type eleno elety  alt resid chain resno insert      x      y     z o
    ## 1 ATOM     1     N <NA>   PRO     A     1   <NA> 29.361 39.686 5.862 1
    ## 2 ATOM     2    CA <NA>   PRO     A     1   <NA> 30.307 38.663 5.319 1
    ## 3 ATOM     3     C <NA>   PRO     A     1   <NA> 29.760 38.071 4.022 1
    ## 4 ATOM     4     O <NA>   PRO     A     1   <NA> 28.600 38.302 3.676 1
    ## 5 ATOM     5    CB <NA>   PRO     A     1   <NA> 30.508 37.541 6.342 1
    ## 6 ATOM     6    CG <NA>   PRO     A     1   <NA> 29.296 37.591 7.162 1
    ##       b segid elesy charge
    ## 1 38.10  <NA>     N   <NA>
    ## 2 40.62  <NA>     C   <NA>
    ## 3 42.64  <NA>     C   <NA>
    ## 4 43.40  <NA>     O   <NA>
    ## 5 37.87  <NA>     C   <NA>
    ## 6 38.40  <NA>     C   <NA>

``` r
inds <- atom.select(pdb, resno = 10)
```

``` r
pdb$atom[inds$atom, ]
```

    ##     type eleno elety  alt resid chain resno insert      x      y      z o
    ## 81  ATOM    81     N <NA>   LEU     A    10   <NA> 25.905 28.285  9.330 1
    ## 82  ATOM    82    CA <NA>   LEU     A    10   <NA> 25.653 28.510 10.750 1
    ## 83  ATOM    83     C <NA>   LEU     A    10   <NA> 26.383 29.770 11.208 1
    ## 84  ATOM    84     O <NA>   LEU     A    10   <NA> 27.567 29.927 10.938 1
    ## 85  ATOM    85    CB <NA>   LEU     A    10   <NA> 26.120 27.284 11.573 1
    ## 86  ATOM    86    CG <NA>   LEU     A    10   <NA> 25.161 26.082 11.544 1
    ## 87  ATOM    87   CD1 <NA>   LEU     A    10   <NA> 25.895 24.743 11.662 1
    ## 88  ATOM    88   CD2 <NA>   LEU     A    10   <NA> 24.206 26.196 12.696 1
    ## 838 ATOM   839     N <NA>   LEU     B    10   <NA> 12.134 31.727 -5.504 1
    ## 839 ATOM   840    CA <NA>   LEU     B    10   <NA> 11.816 30.740 -6.534 1
    ## 840 ATOM   841     C <NA>   LEU     B    10   <NA> 12.459 31.075 -7.877 1
    ## 841 ATOM   842     O <NA>   LEU     B    10   <NA> 12.274 32.150 -8.406 1
    ## 842 ATOM   843    CB <NA>   LEU     B    10   <NA> 10.303 30.637 -6.738 1
    ## 843 ATOM   844    CG <NA>   LEU     B    10   <NA>  9.483 30.307 -5.497 1
    ## 844 ATOM   845   CD1 <NA>   LEU     B    10   <NA>  8.028 30.334 -5.876 1
    ## 845 ATOM   846   CD2 <NA>   LEU     B    10   <NA>  9.845 28.975 -4.951 1
    ##         b segid elesy charge
    ## 81  28.83  <NA>     N   <NA>
    ## 82  31.57  <NA>     C   <NA>
    ## 83  30.48  <NA>     C   <NA>
    ## 84  31.00  <NA>     O   <NA>
    ## 85  31.09  <NA>     C   <NA>
    ## 86  35.91  <NA>     C   <NA>
    ## 87  40.15  <NA>     C   <NA>
    ## 88  40.51  <NA>     C   <NA>
    ## 838 18.74  <NA>     N   <NA>
    ## 839 24.75  <NA>     C   <NA>
    ## 840 28.33  <NA>     C   <NA>
    ## 841 34.15  <NA>     O   <NA>
    ## 842 22.30  <NA>     C   <NA>
    ## 843 26.19  <NA>     C   <NA>
    ## 844 26.68  <NA>     C   <NA>
    ## 845 25.72  <NA>     C   <NA>

``` r
atom.select(pdb, resno = 10, value = TRUE)
```

    ## 
    ##  Call:  trim.pdb(pdb = pdb, sele)
    ## 
    ##    Total Models#: 1
    ##      Total Atoms#: 16,  XYZs#: 48  Chains#: 2  (values: A B)
    ## 
    ##      Protein Atoms#: 16  (residues/Calpha atoms#: 2)
    ##      Nucleic acid Atoms#: 0  (residues/phosphate atoms#: 0)
    ## 
    ##      Non-protein/nucleic Atoms#: 0  (residues: 0)
    ##      Non-protein/nucleic resid values: [ none ]
    ## 
    ##    Protein sequence:
    ##       LL
    ## 
    ## + attr: atom, helix, sheet, seqres, xyz,
    ##         calpha, call

``` r
pdbprot <- atom.select(pdb, 'protein', value = TRUE)
pdblig <- atom.select(pdb, 'ligand', value = TRUE)
write.pdb(pdbprot, file = "1hsg_protein.pdb")
write.pdb(pdblig, file = "1hsg_ligand.pdb")
```
