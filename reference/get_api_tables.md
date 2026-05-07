# Provide Tables of Wasserportal API Documentation

The tables that appear in the API documentation of the wasserportal
(https://wasserportal.berlin.de/download/wasserportal_berlin_getting_data.pdf)
have been added to the wasserportal package. This function returns a
list of data frames with each element representing one of these tables.

## Usage

``` r
get_api_tables(name = NULL)
```

## Arguments

- name:

  of element from the list of data frames to be selected. If this
  argument is left blank (name = NULL), the default, the list of data
  frames is returned.

## Value

list of data frames or data frame specified by the `name` argument

## Examples

``` r
get_api_tables()
#> $global_surface_water
#>   par_name  par_value         par_meaning_de      par_meaning
#> 1  station         ID             Messstelle   Station number
#> 2   sreihe         ew            Einzelwerte    Single values
#> 3   sreihe         tw             Tageswerte     Daily values
#> 4   sreihe         mw            Monatswerte   Monthly values
#> 5    smode          c                    CSV              CSV
#> 6    smode          x          XML (WaterML)              XML
#> 7   sdatum dd:mm:yyyy Datumsangabe (ab wann) Date (from when)
#> 8  anzeige          d               Download         Download
#> 9  anzeige          g     Graphische Ausgabe Graphical output
#> 
#> $surface_soil_water
#>    topic        topic_meaning_de     topic_meaning         unit
#> 1    ows             Wasserstand       Water level           cm
#> 2    odf              Durchfluss        Flow meter         m3/s
#> 3    owt        Wassertemperatur Water temperature deg. Celsius
#> 4    olf          Leitfaehigkeit      Conductivity       myS/cm
#> 5    oph                 pH Wert          pH value            -
#> 6    oog        Sauerstoffgehalt    Oxygen content     mg/liter
#> 7    oos    Sauerstoffsaettigung Oxygen saturation            %
#> 8    opq Oberflaechen-Probenahme              <NA>         <NA>
#> 9    bbf            Bodenfeuchte     Soil moisture         pF %
#> 10   bbt         Bodentemperatur  Soil temperature  grd Celsius
#> 
#> $global_groundwater
#>      par_name                         par_value       par_meaning_de
#> 1     station                                ID           Messstelle
#> 2       thema                               gwq Grundwasserqualitaet
#> 3 exportthema                                gw          Grundwasser
#> 4 exportthema                                pq           Probenahme
#> 5      sreihe                                ew          Einzelwerte
#> 6       smode                                 c           CSV format
#> 7    nstoffid numbers(count:nstoffid;nstoffid2)      IDs fuer Stoffe
#> 8      sdatum                        dd:mm:yyyy   Datumsangabe (von)
#> 9   senddatum                        dd:mm:yyyy   Datumsangabe (bis)
#>           par_meaning
#> 1      Station number
#> 2 Groundwater quality
#> 3         Groundwater
#> 4                <NA>
#> 5       Single values
#> 6          CSV format
#> 7   IDs of substances
#> 8         Date (from)
#> 9        Date (until)
#> 
#> $groundwater
#>   topic        topic_meaning_de       topic_meaning
#> 1   gws        Grundwasserstand   Groundwater level
#> 2   gwq (Grund-)Wasserqualitaet Groundwater quality
#> 3   opq Oberflaechen-Probenahme    surface sampling
#>                                 unit
#> 1                        m (ue. NHN)
#> 2 (see table substances_groundwater)
#> 3                               <NA>
#> 
#> $substances_groundwater
#>    chem_par_id chem_par_name     chem_par_name_long        unit
#> 1            2            TL      Temperatur (Luft) grd Celsius
#> 2            5            PH         pH-Wert (Feld)           -
#> 3            6         LEITF Leitfaehigkeit vor Ort      myS/cm
#> 4            7            OX         Redoxpotential          mV
#> 5           10            TW    Temperatur (Wasser) grd Celsius
#> 6           12            OS          Redoxspannung          mV
#> 7           13            EH         Eh-Wert (Feld)           -
#> 8           37            CL                Chlorid        mg/l
#> 9           38             F                Fluorid        mg/l
#> 10          39          HCO3       Hydrogenkarbonat        mg/l
#> 11          43             S                 Sulfid        mg/l
#> 12          44           SO4                 Sulfat        mg/l
#> 13          45        CYANID         Cyanide (ges.)        mg/l
#> 14          46            BR                 Bromid        mg/l
#> 15          47           NO2                 Nitrit        mg/l
#> 16          48           NO3                 Nitrat        mg/l
#> 17          49         PO4_O         Ortho-Phosphat        mg/l
#> 18          50           JOD                    Jod       myg/l
#> 19          51          SIO2                   SiO2        mg/l
#> 20          52         S_GEL       Sulfid (geloest)        mg/l
#> 21          55         PO4_G        Phosphat (ges.)        mg/l
#> 22          57         NH4_N           Ammonium (N)      mg/l N
#> 23          58          FE_2                Eisen-2        mg/l
#> 24          59          FE_G           Eisen (ges.)        mg/l
#> 25          60             K                 Kalium        mg/l
#> 26          61            CA                Kalzium        mg/l
#> 27          62            MG              Magnesium        mg/l
#> 28          63            NA                Natrium        mg/l
#> 29          64            MN                 Mangan        mg/l
#> 
#> $substances_sampling
#>     chem_par_id                            chem_par_name_long         unit
#> 1           111                                 Silber gesamt        myg/l
#> 2           112                             Aluminium geloest        myg/l
#> 3           113                              Aluminium gesamt        myg/l
#> 4           125                                 Arsen geloest        myg/l
#> 5           126                                  Arsen gesamt        myg/l
#> 6           127                               Desethylatrazin        myg/l
#> 7           128                                       Atrazin        myg/l
#> 8           129                                Azinphos-ethyl        myg/l
#> 9           130                               Azinphos-methyl        myg/l
#> 10          131                                  b-Endosulfan         ng/l
#> 11          132                                         b-HCH        myg/l
#> 12          133                                Barium geloest        myg/l
#> 13          134                                 Barium gesamt        myg/l
#> 14          135                               Beta-Cyfluthrin        myg/l
#> 15          144                              Beryllium gesamt        myg/l
#> 16          145                                      Bentazon        myg/l
#> 17          146                                      Benzidin        myg/l
#> 18          147                              Dichlorbenzidine        myg/l
#> 19          148                                        Benzol        myg/l
#> 20          149                                    Bezafibrat        myg/l
#> 21          151                                    Bifenthrin        myg/l
#> 22          152                                       Bifenox        myg/l
#> 23          153                                      Biphenyl        myg/l
#> 24          154                                   Bisphenol A        myg/l
#> 25          156                                    Bor gesamt        myg/l
#> 26          157                                    Bromoxynil        myg/l
#> 27          158                                      Bromacil        myg/l
#> 28          167                                 Chlordan,cis-        myg/l
#> 29          168                           C10-C13-Chloralkane        myg/l
#> 30          169                         1,2-Dichlorethen,cis-        myg/l
#> 31          170                               Tetrachlorethen        myg/l
#> 32          171                                 Trichlorethen        myg/l
#> 33          172                                       Calcium         mg/l
#> 34          173                                  Carbamazepin        myg/l
#> 35          174                                   Carbendazim        myg/l
#> 36          175                         Tetrachlorkohlenstoff        myg/l
#> 37          176                               Cadmium geloest        myg/l
#> 38          177                                Cadmium gesamt        myg/l
#> 39          178                                 Chlorophyll-a        myg/l
#> 40          179                                      Chlordan        myg/l
#> 41          180                     Chlorethen (Vinylchlorid)        myg/l
#> 42          181                                 Chlorflurenol        myg/l
#> 43          182             Chloropren (2-Chlorbuta-1,3-dien)        myg/l
#> 44          183                                       Chrysen        myg/l
#> 45          184                                 Ciprofloxacin        myg/l
#> 46          185                        1,3-Dichlorpropen,cis-        myg/l
#> 47          186                                 Chloralhydrat        myg/l
#> 48          187                                   Chlorbenzol        myg/l
#> 49          188                              Chloressigsaeure        myg/l
#> 50          189                                2-Chlorethanol        myg/l
#> 51          190                               Chlorfenvinphos        myg/l
#> 52          191                                       Chlorid         mg/l
#> 53          192         Chlornaphtaline (technische Mischung)        myg/l
#> 54          193                       Dichlordiisopropylether        myg/l
#> 55          214                                  Cypermethrin        myg/l
#> 56          215                                         d-HCH        myg/l
#> 57          216                         Dibenzo[a,h]anthracen        myg/l
#> 58          217                   DEHP (Diethylhexylphthalat)        myg/l
#> 59          218                                  Deltamethrin        myg/l
#> 60          228                                 Dichlormethan        myg/l
#> 61          229                          Dichlorprop (2,4-DP)        myg/l
#> 62          230                                    Diclofenac        myg/l
#> 63          231                                      Dieldrin        myg/l
#> 64          232                                  Diflufenican        myg/l
#> 65          233                                  Dimethachlor        myg/l
#> 66          234                                     Dimethoat        myg/l
#> 67          235                                 Dimoxystrobin        myg/l
#> 68          236  DINP+DIDP (Diisononyl- + Diisodecylphthalat)        myg/l
#> 69          237                                    Disulfoton        myg/l
#> 70          238                                        Diuron        myg/l
#> 71          239                                       PCB-118         ng/l
#> 72          240                  Dimethylaminophenazon (DMAA)        myg/l
#> 73          241                        Dimethylpyrazolon (DP)        myg/l
#> 74          242       DOC (Geloester organischer Kohlenstoff)         mg/l
#> 75          243                          Desphenylchloridazon        myg/l
#> 76          244                                         e-HCH        myg/l
#> 77          245                                        E.Coli       /100ml
#> 78          246                                          EDTA        myg/l
#> 79          247                      Intestinale Enterokokken       /100ml
#> 80          248                                        Endrin        myg/l
#> 81          253                                   Ethylbenzol        myg/l
#> 82          254                                   Diethylamin        myg/l
#> 83          255                                     Etrimphos        myg/l
#> 84          256                                     Famoxadon        myg/l
#> 85          258                                  Eisen gesamt         mg/l
#> 86          287                            Quecksilber gesamt        myg/l
#> 87          288                                     Ibuprofen        myg/l
#> 88          296                                        Kalium         mg/l
#> 89          297                                    Ketoprofen        myg/l
#> 90          300                                       Lenacil        myg/l
#> 91          301                                Leitfaehigkeit       myS/cm
#> 92          303                                Lithium gesamt        myg/l
#> 93          304                                       Linuron        myg/l
#> 94          305                     1,3- + 1,4-Dimethylbenzol        myg/l
#> 95          306                 Desphenylchloridazon, Methyl-        myg/l
#> 96          307                                     Malathion        myg/l
#> 97          308           Methylenblau-aktive Substanz (MBAS)         mg/l
#> 98          309                                          MCPA        myg/l
#> 99          311                                 Methamidophos        myg/l
#> 100         312                                  Metaflumizon        myg/l
#> 101         313                                    Metamitron        myg/l
#> 102         314                                   Metazachlor        myg/l
#> 103         316                                    Metconazol        myg/l
#> 104         317                                     Metformin        myg/l
#> 105         318                            Methabenzthiazuron        myg/l
#> 106         319                                  Methoxychlor        myg/l
#> 107         320                                  Dimethylamin        myg/l
#> 108         321                                   Metolachlor        myg/l
#> 109         322                                    Metribuzin        myg/l
#> 110         323                                     Mevinphos         ng/l
#> 111         324                                     Magnesium         mg/l
#> 112         325                                     Miconazol        myg/l
#> 113         326            MKW (Mineraloelkohlenwasserstoffe)        myg/l
#> 114         328                                 Mangan gesamt         mg/l
#> 115         330                              Molybdaen gesamt        myg/l
#> 116         331                                   Monolinuron        myg/l
#> 117         332                                       Natrium         mg/l
#> 118         333                                    Naphthalin        myg/l
#> 119         336                           Ammonium-Stickstoff         mg/l
#> 120         338                                 Nickel gesamt        myg/l
#> 121         339                                   Nitrobenzol        myg/l
#> 122         340                                  Nicosulfuron        myg/l
#> 123         341                             Nitrit-Stickstoff         mg/l
#> 124         342                             Nitrat-Stickstoff         mg/l
#> 125         343                 Gesamt-Stickstoff (N) geloest         mg/l
#> 126         344                  Gesamt-Stickstoff (N) gesamt         mg/l
#> 127         345                   Nitrilotriessigsaeure (NTA)        myg/l
#> 128         346                       ortho-Phosphat-Phosphor         mg/l
#> 129         347                            1,2-Dimethylbenzol        myg/l
#> 130         348                             Sauerstoff-Gehalt         mg/l
#> 131         356                                   Blei gesamt        myg/l
#> 132         363                        Pentachlorphenol (PCP)        myg/l
#> 133         364                                    Penconazol        myg/l
#> 134         365                                 Pendimethalin        myg/l
#> 135         366                              Pentachlorbenzol        myg/l
#> 136         367                                    Permethrin        myg/l
#> 137         368                                    Pethoxamid        myg/l
#> 138         384            PFPeS (Perfluorpentansulfonsaeure)        myg/l
#> 139         385               PFTrDA (Perfluortridecansaeure)        myg/l
#> 140         386         PFTrDS (Perfluortridecansulfonsaeure)        myg/l
#> 141         387                PFUnDA (Perfluorundecansaeure)        myg/l
#> 142         388          PFUnDS (Perfluorundecansulfonsaeure)        myg/l
#> 143         389                                       pH-Wert          ---
#> 144         390                                   Phaeophytin        myg/l
#> 145         391                                        Phenol        myg/l
#> 146         392                                   Phenanthren        myg/l
#> 147         393                                      Phenazon        myg/l
#> 148         394                                       Phenole         mg/l
#> 149         405                    Gesamt-Phosphor (P) gesamt         mg/l
#> 150         410                                Redoxpotential           mV
#> 151         411 Spektraler Absorptionskoeffizient (SAK) 254nm          m-1
#> 152         413                                Antimon gesamt        myg/l
#> 153         415                                  Selen gesamt        myg/l
#> 154         416                              Silicium geloest         mg/l
#> 155         417                                    Sichttiefe           cm
#> 156         434                               Thallium gesamt        myg/l
#> 157         435                                Lufttemperatur deg. Celsius
#> 158         436                 TOC (Organischer Kohlenstoff)         mg/l
#> 159         437                                        Toluol        myg/l
#> 160         448                              Wassertemperatur deg. Celsius
#> 161         449                                  Uran geloest        myg/l
#> 162         450                                   Uran gesamt        myg/l
#> 163         455                                  Zink geloest        myg/l
#> 164         456                                   Zink gesamt        myg/l
#> 
```
