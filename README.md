N2K.CZ
================

## Jedna příroda

<p align="justify">Webová aplikace na hodnocení stavu předmětů ochrany <a href="https://jonasgaigr.shinyapps.io/HOST_N2K/"<strong>HOST.N2K</strong></a> je vyvíjena v rámci projektu <a href="https://www.jednapriroda.cz/">Jedna příroda</a> (LIFE-IP:N2K: Revisited, LIFE17/IPE/CZ/000005). Projekt Jedna příroda přispívá k zachování biodiverzity a podpoře ekosystémových služeb v chráněných územích soustavy Natura 2000. Jedním ze způsobů, kterými bude tohoto cíle dosaženo je zlepšení péče o předměty ochrany (druhy a stanoviště) na území evropsky významných lokalit a ptačích oblastí.</p> 

<p align="center"><a href="#"><img src="https://raw.githubusercontent.com/jonasgaigr/N2K.CZ/main/WWW/LOGO.jpg" alt="Integrace přístupu adaptivního managementu a dostupných poznatků - převzato z Gilson et al. (2019)" width="50%" height="50%" /></a></p>

## Adaptivní managementový cyklus a opatření podložená vědeckými poznatky

<p align="justify"> Adaptivní management představuje iterativní přístup v ochraně přírody umožnující postupné zpřesňování prováděných opatření na základě monitoringu cílových populací a habitatů. V adaptivním managementovém cyklu je v reakci na stav předmětu ochrany naplánován management, který je následně implementován. Předmět ochrany je monitorován a na základě získaných dat o jeho stavu je vyhodnocen dopad managementových opatření. Podle výsledku hodnocení jsou opatření na podporu předmětu ochrany upravena nebo prováděna v nezměněné podobě. </p>
  
<p align="center"><a href="#"><img src="https://raw.githubusercontent.com/jonasgaigr/N2K.CZ/main/WWW/cyklus.jpg" alt="Integrace přístupu adaptivního managementu a dostupných poznatků - převzato z Gilson et al. (2019)" width="50%" height="50%" /></a></p>

<p align="center" class="caption">Integrace přístupu adaptivního managementu a dostupných poznatků -
převzato z Gilson et al. (2019)</p>

## Metodika sběru a vyhodnocení dat

<p align="center"><a href="#"><img src="https://raw.githubusercontent.com/jonasgaigr/N2K.CZ/main/WWW/flow_analysis.png" width="75%" height="75%" style="display: block; margin: auto;" /></a></p>

<p align="center"><a href="#"><img src="https://raw.githubusercontent.com/jonasgaigr/N2K.CZ/main/WWW/flow_main.png" width="90%" height="90%" style="display: block; margin: auto;" /></a></p>

### Data o populacích živočichů a rostlin

<p align="justify"><a href="https://portal.nature.cz/nd/">Nálezová databáze ochrany přírody</a></p>

### Data o prostředí živočichů a rostlin

#### Fyzikálně-chemická data

<p align="justify">Data o stavu habitatu předmětů ochrany s vazbou na vodní prostředí jsou získávána a vyhodnocována v souladu platnou <a href="https://www.mzp.cz/C1257458002F0DC7/cz/metodiky_chranenych_uzemi/$FILE/OOV_Metodika_monitoring_EVL_20201021.pdf">metodikou monitoringu</a> chráněných území vymezených pro ochranu stanovišť a druhů s vazbou na vody, respektive <a href="https://www.mzp.cz/C1257458002F0DC7/cz/metodiky_chranenych_uzemi/$FILE/OOV_Metodika_hodnocen%C3%AD_stavu_EVL_20201020.pdf">metodikou jejich hodnocení</a>. Tato data jsou průběžně aktualizována sledováním vybraných profilů vztažených k jednotlivým evropsky významným lokalitám.</p>

#### Remote-sensing data

### Data o stavu habitatů

## Architektura aplikace

* Uživateské rozhraní bylo vybudováno s využitím [R Shiny](https://github.com/rstudio/shiny)
* Mapy evropsky významných lokalit a ptačích oblastí byly vytvořeny pomocí [leaflet](https://rstudio.github.io/leaflet/) package
* Aplikace vyžaduje [mapové vrstvy](https://gis-aopkcr.opendata.arcgis.com/) Agentury ochrany přírody a krajiny ČR dostupné pod licencí [Creative Commons By 4.0](https://creativecommons.org/licenses/by/4.0/deed.cs)

## Hodnocení předmětů ochrany

Aktuální verze aplikace [HOST.N2K](https://jonasgaigr.shinyapps.io/HOST_N2K/) je k dispozici na platformě [Shinyapps.io](https://www.shinyapps.io/)

### Habitaty

#### Formační skupiny habitatů 

##### Vodní toky a nádrže

##### Mokřady

##### Rašeliniště

##### Skály, sutě a jeskyně

##### Alpínské bezlesí

##### Sekundární trávníky a vřesoviště

##### Křoviny

##### Lesy

#### Hodnocení stavu habitatů

##### Rozloha

##### Typické druhy

##### Kvalita

##### Minimiareál

##### Mozaika

##### Celistvost

##### Konektivita

##### Ohrožené druhy z červeného seznamu

##### Invazní druhy

##### Expanzní druhy

### Rostliny

#### Cévnaté rostliny

#### Mechorosty

### Živočichové
#### Hmyz (*Insecta*)
##### Motýli (*Lepidoptera*)
###### Modrásek bahenní (*Phengaris nausithous*)
| Parametr | Limitní hodnota | Zdroj dat |
| :---: | :---: | :---: |
| výskyt druhu | doložen | NDOP (ArcGIS Survey123) |
| pokryvnost preferovaných habitatů | ≥ 50 % výměry EVL | VMB |
| přítomnost kvetoucích krvavců totenů | ≥ hojně | NDOP (ArcGIS Survey123) |
| sukcese | absence vlivu na lokalitě | NDOP (ArcGIS Survey123) |
| zarůstání expanzními či invazními druhy | absence vlivu na lokalitě | NDOP (ArcGIS Survey123) |
| management | vhodný typ a načasování | NDOP (ArcGIS Survey123) |
| jiný negativní vliv | absence vlivu na lokalitě | NDOP (ArcGIS Survey123) |

Frekvence monitoringu: 1 za 3 roky
###### Modrásek očkovaný (*Phengaris teleius*)
| Parametr | Limitní hodnota | Zdroj dat |
| :---: | :---: | :---: |
| výskyt druhu | doložen | NDOP (ArcGIS Survey123) |
| pokryvnost preferovaných habitatů | ≥ 50 % výměry EVL | VMB |
| přítomnost kvetoucích krvavců totenů | ≥ hojně | NDOP (ArcGIS Survey123) |
| sukcese | absence vlivu na lokalitě | NDOP (ArcGIS Survey123) |
| zarůstání expanzními či invazními druhy | absence vlivu na lokalitě | NDOP (ArcGIS Survey123) |
| management | vhodný typ a načasování | NDOP (ArcGIS Survey123) |
| jiný negativní vliv | absence vlivu na lokalitě | NDOP (ArcGIS Survey123) |

Frekvence monitoringu: 1 za 3 roky
###### Přástevník kostivalový (*Euplagia quadripunctaria*)
| Parametr | Limitní hodnota | Zdroj dat |
| :---: | :---: | :---: |
| výskyt druhu | doložen | NDOP (ArcGIS Survey123) |
| pokryvnost preferovaných habitatů | ≥ 50 % výměry EVL | VMB |
| přítomnost nektaronosných rostlin | ≥ hojně | NDOP (ArcGIS Survey123) |
| sukcese | absence vlivu na lokalitě | NDOP (ArcGIS Survey123) |
| zarůstání expanzními či invazními druhy | absence vlivu na lokalitě | NDOP (ArcGIS Survey123) |
| jiný negativní vliv | absence vlivu na lokalitě | NDOP (ArcGIS Survey123) |


###### Žluťásek barvoměnný (*Colias myrmidone*)
Druh je předmětem ochrany v jedinné EVL, kde vyhynul.

###### Hnědásek chrastavcový (*Euphydryas aurinia*)
| Parametr | Limitní hodnota | Zdroj dat |
| :---: | :---: | :---: |
| výskyt druhu | doložen | NDOP (ArcGIS Survey123) |
| počet larválních hnízd | ≥ 10 | NDOP (ArcGIS Survey123) |
| pokryvnost preferovaných habitatů | ≥ 50 % výměry EVL | VMB |
| přítomnost čertkusů | ≥ hojně | NDOP (ArcGIS Survey123) |
| sukcese | absence vlivu na lokalitě | NDOP (ArcGIS Survey123) |
| zarůstání expanzními či invazními druhy | absence vlivu na lokalitě | NDOP (ArcGIS Survey123) |
| management | vhodný typ a načasování | NDOP (ArcGIS Survey123) |
| jiný negativní vliv | absence vlivu na lokalitě | NDOP (ArcGIS Survey123) |

###### Hnědásek osikový (*Euphydryas maturna*)

###### Ohniváček černočárný (*Lycaena dispar*)
| Parametr | Limitní hodnota | Zdroj dat |
| :---: | :---: | :---: |
| výskyt druhu | doložen | NDOP (ArcGIS Survey123) |
| pokryvnost preferovaných habitatů | ≥ 50 % výměry EVL | VMB |
| přítomnost širokolistých šťovíků | ≥ hojně | NDOP (ArcGIS Survey123) |
| sukcese | absence vlivu na lokalitě | NDOP (ArcGIS Survey123) |
| zarůstání expanzními či invazními druhy | absence vlivu na lokalitě | NDOP (ArcGIS Survey123) |
| management | vhodný typ a načasování | NDOP (ArcGIS Survey123) |
| jiný negativní vliv | absence vlivu na lokalitě | NDOP (ArcGIS Survey123) |

###### Bourovec trnkový (*Eriogaster catax*)

##### Brouci (*Coleoptera*)
###### Chrobák jednorohý (*Bolbelasmus unicornis*)
| Parametr | Limitní hodnota | Zdroj dat |
| :---: | :---: | :---: |
| výskyt druhu | doložen | NDOP (ArcGIS Survey123) |
| pokryvnost preferovaných habitatů | ≥ 25 % výměry EVL | VMB |
| sukcese | absence vlivu na lokalitě | NDOP (ArcGIS Survey123) |
| zarůstání expanzními či invazními druhy | absence vlivu na lokalitě | NDOP (ArcGIS Survey123) |
| jiný negativní vliv | absence vlivu na lokalitě | NDOP (ArcGIS Survey123) |

Frekvence monitoringu: 1 za 3 roky
###### Střevlík panonský (*Carabus hungaricus*)
| Parametr | Limitní hodnota | Zdroj dat |
| :---: | :---: | :---: |
| výskyt druhu | doložen | NDOP (ArcGIS Survey123) |
| pokryvnost preferovaných habitatů | ≥ 25 % výměry EVL | VMB |
| dostatečná přítomnost vysokostébelné vegetace se stařinou | dostatečná přítomnost (min. 10 %) | NDOP (ArcGIS Survey123) |
| přítomnost vhodných remízků a úhorů v blízkém okolí | přítomnost | NDOP (ArcGIS Survey123) |
| sukcese | absence vlivu na lokalitě | NDOP (ArcGIS Survey123) |
| zarůstání expanzními či invazními druhy | absence vlivu na lokalitě | NDOP (ArcGIS Survey123) |
| jiný negativní vliv | absence vlivu na lokalitě | NDOP (ArcGIS Survey123) |

Frekvence monitoringu: 1 za 3 roky
###### Střevlík Menetriesův (*Carabus menetriesi pacholei*)
| Parametr | Limitní hodnota | Zdroj dat |
| :---: | :---: | :---: |
| výskyt druhu | doložen | NDOP (ArcGIS Survey123) |
| pokryvnost preferovaných habitatů | ≥ 25 % výměry EVL | VMB |
| sukcese | absence vlivu na lokalitě | NDOP (ArcGIS Survey123) |
| zarůstání expanzními či invazními druhy | absence vlivu na lokalitě | NDOP (ArcGIS Survey123) |
| jiný negativní vliv | absence vlivu na lokalitě | NDOP (ArcGIS Survey123) |

Frekvence monitoringu: 1 za 6 roky
###### Střevlík hrbolatý (*Carabus variolosus*)
| Parametr | Limitní hodnota | Zdroj dat |
| :---: | :---: | :---: |
| výskyt druhu | doložen | NDOP (ArcGIS Survey123) |
| pokryvnost preferovaných habitatů | ≥ 25 % výměry EVL | VMB |
| přítomnost mrtvého dřeva | dostatečná přítomnost | NDOP (ArcGIS Survey123) |
| přítomnost tekoucí vody | přítomnost | NDOP (ArcGIS Survey123) |
| jiný negativní vliv | absence vlivu na lokalitě | NDOP (ArcGIS Survey123) |

Frekvence monitoringu: 1 za 6 roky
###### tesařík obrovský (*Cerambyx cerdo*)
| Parametr | Limitní hodnota | Zdroj dat |
| :---: | :---: | :---: |
| výskyt druhu | doložen | NDOP (ArcGIS Survey123) |
| pokryvnost preferovaných habitatů | ≥ 25 % výměry EVL | VMB |
| dostatečná nabídka vhodných stromů | dostatečná nabídka | NDOP (ArcGIS Survey123) |
| zabezpečená kontinuita nabídky v hodných stromů | zabezpečená kontinuita | NDOP (ArcGIS Survey123) |
| jiný negativní vliv | absence vlivu na lokalitě | NDOP (ArcGIS Survey123) |

###### Lesák rumělkový (*Cucujus cinnaberinus*)
| Parametr | Limitní hodnota | Zdroj dat |
| :---: | :---: | :---: |
| výskyt druhu | doložen | NDOP (ArcGIS Survey123) |
| pokryvnost preferovaných habitatů | ≥ 25 % výměry EVL | VMB |
| dostatečná nabídka vhodných stromů | dostatečná nabídka | NDOP (ArcGIS Survey123) |
| zabezpečená kontinuita nabídky v hodných stromů | zabezpečená kontinuita | NDOP (ArcGIS Survey123) |
| jiný negativní vliv | absence vlivu na lokalitě | NDOP (ArcGIS Survey123) |

###### Potápník dvoučárý (*Graphoderus bilineatus*)
| Parametr | Limitní hodnota | Zdroj dat |
| :---: | :---: | :---: |
| výskyt druhu | doložen | NDOP (ArcGIS Survey123) |
| pokryvnost preferovaných habitatů | ≥ 25 % výměry EVL | VMB |
| pokryvnost litorální vegetace | ≥ 40 % obvodu vodní plochy | NDOP (ArcGIS Survey123) |
| šířka litorálu | ≥ 2 metry | NDOP (ArcGIS Survey123) |
| zastínění vodní hladiny | < 30 % litorálu | NDOP (ArcGIS Survey123) |
| intenzivní rybniční hospodaření | absence vlivu na lokalitě | NDOP (ArcGIS Survey123) |
| jiný negativní vliv | absence vlivu na lokalitě | NDOP (ArcGIS Survey123) |

###### Kovařík fialový (*Limoniscus violaceus*)
| Parametr | Limitní hodnota | Zdroj dat |
| :---: | :---: | :---: |
| výskyt druhu | doložen | NDOP (ArcGIS Survey123) |
| pokryvnost preferovaných habitatů | ≥ 25 % výměry EVL | VMB |
| dostatečná nabídka vhodných stromů | dostatečná nabídka | NDOP (ArcGIS Survey123) |
| zabezpečená kontinuita nabídky v hodných stromů | zabezpečená kontinuita | NDOP (ArcGIS Survey123) |
| jiný negativní vliv | absence vlivu na lokalitě | NDOP (ArcGIS Survey123) |

###### Roháč obecný (*Lucanus cervus*)
| Parametr | Limitní hodnota | Zdroj dat |
| :---: | :---: | :---: |
| výskyt druhu | doložen | NDOP (ArcGIS Survey123) |
| pokryvnost preferovaných habitatů | ≥ 25 % výměry EVL | VMB |
| dostatečná nabídka vhodných stromů | dostatečná nabídka | NDOP (ArcGIS Survey123) |
| predace larev | absence vlivu | NDOP (ArcGIS Survey123) |
| jiný negativní vliv | absence vlivu na lokalitě | NDOP (ArcGIS Survey123) |

###### Páchník hnědý (*Osmoderma eremita*)
| Parametr | Limitní hodnota | Zdroj dat |
| :---: | :---: | :---: |
| výskyt druhu | doložen | NDOP (ArcGIS Survey123) |
| pokryvnost preferovaných habitatů | ≥ 25 % výměry EVL | VMB |
| dostatečná nabídka vhodných stromů | dostatečná nabídka | NDOP (ArcGIS Survey123) |
| zabezpečená kontinuita nabídky v hodných stromů | zabezpečená kontinuita | NDOP (ArcGIS Survey123) |
| jiný negativní vliv | absence vlivu na lokalitě | NDOP (ArcGIS Survey123) |

###### Rýhovec pralesní (*Rhysodes sulcatus*)
| Parametr | Limitní hodnota | Zdroj dat |
| :---: | :---: | :---: |
| výskyt druhu | doložen | NDOP (ArcGIS Survey123) |
| pokryvnost preferovaných habitatů | ≥ 25 % výměry EVL | VMB |
| dostatečná nabídka vhodných stromů | dostatečná nabídka | NDOP (ArcGIS Survey123) |
| zabezpečená kontinuita nabídky v hodných stromů | zabezpečená kontinuita | NDOP (ArcGIS Survey123) |
| jiný negativní vliv | absence vlivu na lokalitě | NDOP (ArcGIS Survey123) |

###### Tesařík alpský (*Rosalia alpina*)
| Parametr | Limitní hodnota | Zdroj dat |
| :---: | :---: | :---: |
| výskyt druhu | doložen | NDOP (ArcGIS Survey123) |
| pokryvnost preferovaných habitatů | ≥ 25 % výměry EVL | VMB |
| dostatečná nabídka vhodných stromů | dostatečná nabídka | NDOP (ArcGIS Survey123) |
| zabezpečená kontinuita nabídky v hodných stromů | zabezpečená kontinuita | NDOP (ArcGIS Survey123) |
| skládkování dřeva | absence vlivu | NDOP (ArcGIS Survey123) |
| jiný negativní vliv | absence vlivu na lokalitě | NDOP (ArcGIS Survey123) |

##### Vážky (*Odonata*)
###### Šidélko ozdobné (*Coenagrion ornatum*)
| Parametr | Limitní hodnota | Zdroj dat |
| :---: | :---: | :---: |
| výskyt druhu | doložen | NDOP (ArcGIS Survey123) |
| projevy rozmnožování | doloženy | NDOP (ArcGIS Survey123) |
| pokryvnost preferovaných habitatů | ≥ X % výměry EVL | VMB |
| pokryvnost dřevinné vegetace |  ≤ 10% délky obvodu vodní plochy | NDOP (ArcGIS Survey123) |
| jiný negativní vliv | absence vlivu na lokalitě | NDOP (ArcGIS Survey123) |

###### Vážka jasnoskvrnná (*Leucorrhinia pectoralis*)
| Parametr | Limitní hodnota | Zdroj dat |
| :---: | :---: | :---: |
| výskyt druhu | doložen | NDOP (ArcGIS Survey123) |
| projevy rozmnožování | doloženy | NDOP (ArcGIS Survey123) |
| pokryvnost preferovaných habitatů | ≥ X % výměry EVL | VMB |
| pokryvnost dřevinné vegetace | 5 % ≥ pokryvnost ≤ 60 % délky obvodu vodní plochy | NDOP (ArcGIS Survey123) |
| jiný negativní vliv | absence vlivu na lokalitě | NDOP (ArcGIS Survey123) |

###### Klínatka rohatá (*Ophiogomphus cecilia*)
| Parametr | Limitní hodnota | Zdroj dat |
| :---: | :---: | :---: |
| výskyt druhu | doložen | NDOP (ArcGIS Survey123) |
| projevy rozmnožování | doloženy | NDOP (ArcGIS Survey123) |
| pokryvnost preferovaných habitatů | ≥ X % výměry EVL | VMB |
| zazemění původních písečných a štěrkových substrátů | < 10 % plochy lokality | NDOP (ArcGIS Survey123) |
| pokryvnost dřevinné vegetace | ≤ 50 % délky obvodu vodní plochy | NDOP (ArcGIS Survey123) |
| jiný negativní vliv | absence vlivu na lokalitě | NDOP (ArcGIS Survey123) |

###### Páskovec velký (*Cordulegaster heros*)
| Parametr | Limitní hodnota | Zdroj dat |
| :---: | :---: | :---: |
| výskyt druhu | doložen | NDOP (ArcGIS Survey123) |
| projevy rozmnožování | doloženy | NDOP (ArcGIS Survey123) |
| pokryvnost preferovaných habitatů | ≥ X % výměry EVL | VMB |
| přítomnost tekoucí vody | přítomnost | NDOP (ArcGIS Survey123) |
| jiný negativní vliv | absence vlivu na lokalitě | NDOP (ArcGIS Survey123) |

#### Ostatní bezobratlí
##### Rak kamenáč (*Austropotamobius torrentium*)

##### Perlorodka říční (*Margaritifera margaritifera*)

##### Velevrub tupý (*Unio crassus*)

##### Svinutec tenký (*Anisus vorticulus*) a vrkoči (*Vertigo* sp.)

##### Štírek Stellin (*Anthrenochernes stellae*)

#### Ryby a mihule
##### Leuciscus aspius

##### Cobitis taenia

##### Cottus gobio

##### Gobio albipinnatus

##### Gobio kesslerii

##### Gymnocephalus baloni

##### Gymnocephalus schraetser

##### Misgurnus fossilis

##### Pelecus cultratus

##### Rhodeus amarus

##### Sabanejewia aurata

##### Salmo salar

##### Zingel streber

##### Zingel zingel

##### Eudontomyzon mariae

##### Lampetra planeri

#### Obojživelníci (*Amphibia*)

#### Savci (*Mammalia*)
##### Letouni (*Chiroptera*)

##### Sysel obecný (*Spermophilus citellus*)

##### Bobr evropský (*Castor fiber*)

##### Vydra říční (*Lutra lutra*)

##### Vlk obecný(*Canis lupus*), rys ostrovid (*Lynx lynx*) a medvěd hnědý (*Ursus arctos*)

</details>

## Zdroje

<p align="justify">Gillson, Lindsey, et al. "Finding common ground between adaptive management and evidence-based approaches to biodiversity conservation." <i>Trends in ecology & evolution</i> 34.1 (2019): 31-44.</p>

<p align="justify"></p>
