Hodnocení stavu předmětů ochrany v české soustavě Natura 2000 (HOST.N2K)
================

## Jedna příroda

<p align="justify">Soubor funkcí na hodnocení stavu předmětů ochrany v soustavě <a href="https://natura2000.cz/Lokalita/Lokality">Natura 2000</a> je vyvíjen Agenturou ochrany přírody a a krajiny ČR v rámci projektu <a href="https://www.jednapriroda.cz/">Jedna příroda</a> (LIFE-IP:N2K: Revisited, LIFE17/IPE/CZ/000005). Projekt Jedna příroda přispívá k zachování biodiverzity a podpoře ekosystémových služeb v chráněných územích soustavy Natura 2000. Jedním ze způsobů, kterými bude tohoto cíle dosaženo je zlepšení péče o předměty ochrany (druhy a stanoviště) na území evropsky významných lokalit a ptačích oblastí.</p> 

<p align="center"><a href="#"><img src="https://raw.githubusercontent.com/jonasgaigr/N2K.CZ/main/WWW/LOGO.jpg" alt="Integrace přístupu adaptivního managementu a dostupných poznatků - převzato z Gilson et al. (2019)" width="50%" height="50%" /></a></p>

## Adaptivní managementový cyklus a opatření podložená vědeckými poznatky

<p align="justify"> Adaptivní management představuje iterativní přístup v ochraně přírody umožnující postupné zpřesňování prováděných opatření na základě monitoringu cílových populací a habitatů. V adaptivním managementovém cyklu je v reakci na stav předmětu ochrany naplánován management, který je následně implementován. Předmět ochrany je monitorován a na základě získaných dat o jeho stavu je vyhodnocen dopad managementových opatření. Podle výsledku hodnocení jsou opatření na podporu předmětu ochrany upravena nebo prováděna v nezměněné podobě. </p>
  
<p align="center"><a href="#"><img src="https://raw.githubusercontent.com/jonasgaigr/N2K.CZ/main/WWW/cyklus.jpg" alt="Integrace přístupu adaptivního managementu a dostupných poznatků - převzato z Gilson et al. (2019)" width="50%" height="50%" /></a></p>

<p align="center" class="caption">Integrace přístupu adaptivního managementu a dostupných poznatků -
převzato z Gilson et al. (2019)</p>

<p align="center" class="caption">Implementace principů adaptivního managementového cyklu do hodnocení a plánování péče o předměty ochrany v soustavě Natura 2000:</p>

<p align="center"><a href="#"><img src="https://raw.githubusercontent.com/jonasgaigr/N2K.CZ/main/WWW/amc_matice.png" alt="Implementace principů adaptivního managementového cyklu do hodnocení a plánování péče o předměty ochrany v soustavě Natura 2OOO" width="50%" height="50%" /></a></p>


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
###### Šikoušek zelený (*Buxbaumia viridis*)
| Parametr | Limitní hodnota | Zdroj dat |
| :---: | :---: | :---: |
| výskyt druhu | doložen | NDOP |
| početnost populace | ≥ 3 hnědé (zralé) tobolky | NDOP |
| množství mrvtého dřeva | dostačující | NDOP |
| jiný negativní vliv | absence vlivu na lokalitě | NDOP |

Frekvence monitoringu: 1 za 6 let

###### Dvouhrotec zelený (*Dicranum viride*)
| Parametr | Limitní hodnota | Zdroj dat |
| :---: | :---: | :---: |
| výskyt druhu | doložen | NDOP |
| počet mikrolokalit | druh zaznamenán na > 3 stromech | NDOP |
| velikost populace | hromadný součet jedinců (=trsů) > 10 cm^2 | NDOP |
| pokles velikosti populace | < 30% za poslední 2 návštěvy | NDOP |
| věková struktura stromového patra | věkově rozrůzněný porost | NDOP |
| druhové složení stromového patra | > 10 listnatých stromů o průměru > 30 cm | NDOP |
| intenzita těžby v okruhu 100 m od výskytu druhu | bezzásahovost, výběrová těžba | NDOP |
| jiný negativní vliv | absence vlivu na lokalitě | NDOP |

Frekvence monitoringu: 1 za 6 let

###### Srpnatka fermežová (*Hamatocaulis vernicosus*)
| Parametr | Limitní hodnota | Zdroj dat |
| :---: | :---: | :---: |
| výskyt druhu | doložen | NDOP |
| velikost populace | ≥ 500 cm^2 | NDOP |
| plocha výskytu druhu | ≥ 10 m^2 | NDOP |
| změna velikosti populace I | < 10% za poslední 4 návštěvy | NDOP |
| změna velikosti populace II | < 30% za poslední 2 návštěvy | NDOP |
| výměra potenciální lokality | ≥ 500 m^2 | NDOP |
| pokryvnost bylinného patra | ≤ 90 % | NDOP |
| pokryvnost expanzních druhů bylin | ≤ 10 % | NDOP |
| zastínění (průmět korun dřevin > 30 cm) | ≤ 5 % | NDOP |
| management | pozitivní či neutrální | NDOP |
| jiný negativní vliv | absence vlivu na lokalitě | NDOP |

Frekvence monitoringu: 1 za 6 let

###### Mozolka skalní (*Mannia triandra*)
| Parametr | Limitní hodnota | Zdroj dat |
| :---: | :---: | :---: |
| výskyt druhu | doložen | NDOP |
| velikost populace | ≥ 200 stélkových ramen | NDOP |
| plocha výskytu druhu | ≥ 4 m^2 | NDOP |
| změna velikosti populace I | < 10% za poslední 4 návštěvy | NDOP |
| změna velikosti populace II | < 30% za poslední 2 návštěvy | NDOP |
| reprodukce (přítomnost sporogonů) | prokázání reprodukce | NDOP |
| výměra potenciální lokality | ≥ 15 m^2 | NDOP |
| pokryvnost bylinného patra | ≤ 50 % | NDOP |
| pokryvnost expanzních druhů bylin | ≤ 10 % | NDOP |
| zastínění (průmět korun dřevin > 30 cm) | ≤ 5 % | NDOP |
| jiný negativní vliv | absence vlivu na lokalitě | NDOP |

Frekvence monitoringu: 1 za 6 let

###### Šurpek Rogerův (*Orthotrichum rogeri*)
| Parametr | Limitní hodnota | Zdroj dat |
| :---: | :---: | :---: |
| výskyt druhu | doložen | NDOP |
| velikost mikropopulace | ≥ 2 tobolky | NDOP |
| počet sporofytů v mikropopulaci | ≥ 4 m^2 | NDOP |
| vývoj velikosti mikropopulace | < 10% za poslední 4 návštěvy | NDOP |
| plodnost mikropopulace| < 30% za poslední 2 návštěvy | NDOP |
| vhodná mikrostanoviště | prokázání reprodukce | NDOP |
| vývoj lokality | ≥ 15 m^2 | NDOP |
| jiný negativní vliv | absence vlivu na lokalitě | NDOP |

Frekvence monitoringu: 1 za 6 let

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
