# Packages ----
library(tidyverse)
library(shiny)
library(shinycssloaders)
library(DT)
library(zoo)
library(lubridate)
library(condformat)
library(ggplot2)
library(rgdal)
library(raster)
library(ggsn)
library(proj4)
library(sp)
library(sf)
library(leaflet)
options(shiny.sanitize.errors = FALSE)

# SEZNAM TAXONŮ ----
sites_subjects <- read.xlsx("http://webgis.nature.cz/publicdocs/opendata/natura2000/seznam_predmetolokalit_Natura2000.xlsx")
sites_subjects <- sites_subjects %>%
  rename(Název.latinsky = "Název.latinsky.(druh)")

evl <- st_read("Evropsky_v%C3%BDznamn%C3%A9_lokality.shp")
evl <- st_transform(evl, CRS("+init=epsg:4326"))
mammal_evl <- st_read("Biotop_zvl%C3%A1%C5%A1t%C4%9B_chr%C3%A1n%C4%9Bn%C3%BDch_druh%C5%AF_velk%C3%BDch_savc%C5%AF.shp")
mammal_evl <- st_transform(mammal_evl, CRS("+init=epsg:4326"))
czechia <- st_read("HraniceCR.shp")
czechia <- st_transform(czechia, CRS("+init=epsg:4326"))
bioregs <- st_read("BiogeoRegions_CR.shp")
bioregs <- st_transform(bioregs, CRS("+init=epsg:4326"))

taxa <- read.csv("https://raw.githubusercontent.com/jonasgaigr/N2K.CZ/main/taxa.csv", encoding = "UTF-8")

# Texty ----
text_phenau <-
  paste(
    HTML(
      '<h5><b>Ekologie druhu</b></h5>
      <p style="text-align: justify">Modrásek bahenní má podobné ekologické nároky jako 
      modrásek očkovaný, ale je schopen osídlovat širší škálu stanovišť. Preferuje především 
      vlhké, nehnojené, extenzivně kosené krvavcové louky, ale dokáže žít např. i ve vlhkých 
      příkopech podél silnic, na podmáčených ruderálních stanovištích a na poddolovaných územích. 
      Není však schopen přežívat na loukách, na kterých probíhá druhá seč v době od začátku 
      července do začátku září, tj. v období letu dospělců, kladení vajíček a časného vývoje 
      housenek. Dospělci se vyskytují od začátku července do začátku srpna s vrcholem obvykle 
      kolem 20. – 25. července. Sají nektar na krvavci totenu. Vývojový cyklus je obdobný jako
      u modráska očkovaného. Hostitelskou rostlinou housenek je krvavec toten (<i>Sanguisorba 
      officinalis</i>). Samice kladou vajíčka po několika do rozvinutých květních hlávek krvavce. 
      První tři instary housenek se vyvíjejí v semenících. Ve čtvrtém instaru padají housenky na 
      zem, kde jsou vyhledány dělnicemi hostitelských mravenců (druhu Myrmica scabrinodis, méně 
      často Myrmica ruginodis), které je odnášejí do svých mravenišť. Pokud hostitelské mraveniště
      prosperuje, housenky se nechávají od mravenců krmit. Pokud je mravenčí kolonie slabá, živí 
      se housenky larvami a kuklami mravenců. Po přezimování se v hnízdech mravenců i kuklí.</p>')
  )
text_phetel <-
  paste(
    HTML(
      '<h5><b>Ekologie druhu</b></h5>
      <p style="text-align: justify">Druh vlhkých nehnojených, extenzivně kosených, krvavcových luk. 
      V rámci komplexů vlhkých luk však preferují sušší výslunná místa chráněná před větrem. Dospělci 
      se vyskytují od začátku července do druhé poloviny srpna. Sají nektar na krvavci totenu 
      a bobovitých rostlinách. Vývojový cyklus je obdobný jako u modráska bahenního. Hostitelskou 
      rostlinou housenek je krvavec toten (Sanguisorba officinalis). Samice kladou vajíčka 
      jednotlivě na nerozvité květní hlávky krvavce. První tři instary housenek se vyvíjejí v 
      semenících. Tato fáze vývoje trvá 2-3 týdny. Ve čtvrtém instaru padají housenky na zem, 
      kde jsou vyhledány dělnicemi hostitelských mravenců (druhu Myrmica rubra, méně často Myrmica 
      scabrinodis), které je odnášejí do svých mravenišť. Tam se živí larvami a kuklami mravenců, 
      zhruba po dobu 10 měsíců. Po přezimování se v hnízdech mravenců i kuklí. Modrásek očkovaný 
      je ekologicky velmi podobný příbuznému modrásku bahennímu (Phengaris nausithous), se kterým 
      se na řadě lokalit vyskytuje společně. Modrásek očkovaný má však vyhraněnější nároky na 
      stanoviště a je tedy vzácnější.</p>
      <h5><b>Péče o druh</b></h5>'
    )
  )
text_eupqua <-
  paste(
    HTML(
      '<h5><b>Ekologie druhu</b></h5>
      <p style="text-align: justify">Tento druh preferuje skalnaté lesostepi, osluněné křovinaté 
      stráně, řídké teplomilné doubravy, teplé suťové lesy, ale i osluněné lesní průseky. Dospělce 
      ve dne zastihneme nejčastěji na porostech nektaronosných rostlin, především sadce konopáče 
      (Eupatorium cannabinum). Druh má jednu generaci v roce, dospělci se vyskytují od konce 
      června do začátku září, s vrcholem letu v poslední dekádě července a první polovině srpna. 
      Létá ve dne i v noci, v noci je možné jej přilákat na světlo. Samice kladou vajíčka 
      jednotlivě na živné rostliny. Housenky jsou poměrně polyfágní, živí se především hluchavkami,
      šalvějemi, sadcem konopáčem, starčky, vrbovkami, ale i některými listnatými dřevinami 
      (např. lískou, ostružiníky nebo zimolezy). Housenky se líhnou v září, přezimují a kuklí se v 
      květnu následujícího roku při povrchu země v zápředku.</p>
      <h5><b>Péče o druh</b></h5>
      <p style="text-align: justify">Intenzivní hospodaření na lokalitách (intenzivní pastva s 
      plošným sečením nedopasků, plošné sečení luk a vícenásobná seč) je hodnocen negativně z 
      důvodu ničení obývaného habitatu i populace. Pozitivní je naopak extenzivní způsob 
      hospodaření spolu s odstraňováním náletových dřevin, regulace porostu a nelesní vegetace 
      mozaikovitým sečením (jedenkrát ročně) nebo řízenou extenzivní pastvou. Absence managementu 
      vede k přirozeným změnám stanoviště (zarůstání), které se stávají sjeně jako negativní vlivy
      problematické v případě, že se v okolí nenachází další vhodný habitat, který by zajistil 
      přežití dané populace. Zalesňování lokalit má stejný dopad jako zarůstání v případě absence 
      managementu a je proto hodnoceno negativně spolu s plošným používáním biocidů na lokalitách 
      i v jejich nejbližším okolí.</p>'
    )
  )
text_colmyr <-
  paste(
    HTML(
      '<h5><b>Ekologie druhu</b></h5>
      <p style="text-align: justify">Žluťásek barvoměný, původně lesostepní druh, osídlil v 
      našich podmínkách "starou" zemědělskou krajinu s mozaikou výslunných pasených či kosených 
      pastvin a luk, hájků, solitérních dřevin. Na svých biotopech vyžaduje velmi osluněná i 
      stinná místa, nabídku nektaru i živné rostliny, jimiž jsou čilimníky, ve velkých abundancích.
      Během roku vytváří dvě generace, první v květnu až červnu, druhou v červenci a srpnu. 
      Je relativně dobrý letec, ale v disperzi jej pravděpodobně mohou omezit rozsáhlé zapojené 
      lesní porosty. Má dvě generace do roka, dospělce je možno zastihnout v květnu a červnu a v 
      srpnu a září, přezimuje housenka.</p>
      <h5><b>Péče o druh</b></h5>
      <p style="text-align: justify">Na populace druhu se jednoznačně negativně podepisuje plošné 
      sečení obývaných lokalit bez ponechaných neposečených enkláv, dvojí či vícenásobná seč za 
      sezónu nebo seč v nevhodný termín. Nevhodná je i intenzivní pastva kombinovaná se sečením 
      nedopasků, ale také absence managementu, kdy dochází samovolnému zarůstání lokalit dřevinami. 
      Změny ve využívání biotopu (zalesnění, změna na polní kultury), likvidace nebo zarůstání 
      širokých lesních lemů, používání biocidů a hnojiv při obhospodařování travnatých biotopů a 
      jejich okolí druhu rovněž neprospívají. Pozitivně naopak působí management zachovávající 
      stepní nebo lesostepní charakter lokalit, především udržování členitých širokých závětrných 
      lemů, širokých světlých lemů podél cest a trvalých lesních průseků (pod elektrickým vedením 
      apod.), které by zprostupnily krajinu pro migraci motýla.</p>'
    )
  )
text_eupaur <-
  paste(
    HTML(
      '<h5><b>Ekologie druhu</b></h5>
      <p style="text-align: justify">Biotop: Mokré rašelinné a slatinné louky a vlhké podhorské 
      pastviny, případně i vysychavější stanoviště s výskytem živné rostliny. Živná rostlina 
      housenek: V ČR pouze čertkus luční (Succisa pratensis). Vývoj. Jednogenerační (květen–konec 
      června). Vajíčka kladena ve shlucích v několika vrstvách na spodní stranu listů čertkusu. 
      Preferují přitom živné rostliny rostoucí ve výrazných shlucích, jež jsou obklopeny nižší a 
      někdy až nezapojenou vegetací. Pouze zde je živná rostlina dostatečně viditelná pro kladoucí 
      samice, a současně dost osluněná pro vyhřívající se larvy. Housenky jsou gregarické, žijí v 
      pozdním létě a na začátku podzimu v zámotcích na listech živných rostlin; po zkonzumování 
      rostliny se někdy celé hnízdo přesune na jiný trs čertkusu. Podmínkou úspěšného dokončení 
      vývoje na jaře je možnost vyhřívání se na trsech nízkých trav a příjem velkého množství 
      potravy. Nápadně zbarvené kukly leží přímo na listech čertkusu.</p>
      <h5><b>Péče o druh</b></h5>
      <p style="text-align: justify">Motýl má zvláštní nároky na členitost mozaikovitost lokalit.
      K vývoji housenek potřebuje husté porosty živné rostliny situované v nižších travnatých 
      porostech. Vedle nich však vyžaduje bohatou nabídku nektaronosných rostlin a slunná 
      závětrná místa, kde může probíhat páření (pásy keřů, rozhraní luk a lesů). Všechny tyto 
      podmínky nalézá na extenzivních pastvinách, resp. částech pastvin vyhledávaných dobytkem 
      jen nepravidelně. Vhodnou péčí o obsazené lokality je buď extenzivní pastva, nebo 
      mozaikovité sečení během června. Při sečení musí být ponechávány široké okraje i nesečené 
      pásy či enklávy v lukách, ty z roku na rok střídat. Nepřípustná je druhá seč. Kosené louky 
      mohou být na podzim velmi mírně přepaseny (1-2 krávy na hektar), což zajistí narušování 
      drnu. Na menších lokalitách by se sečení mělo přímo vyhýbat rostlinám s hnízdy.</p>'
    )
  )
text_eupmat <-
  paste(
    HTML(
      '<h5><b>Ekologie druhu</b></h5>
      <p style="text-align: justify">Živná rostlina housenek: ve středních Čechách pouze jasan 
      ztepilý (Fraxinus excelsior), po přezimovaní se housenky v Polabí nejprve krmí bylinami 
      (plicníky, violky aj.), případně rašícími pupeny ptačího zobu (Ligustrum vulgare). Poté, 
      co začne rašit jasan, přecházejí na pupeny a mladé listy jasanu. Na jižní Moravě kromě 
      jasanu také na kalina tušalaj (Viburnum lantana). Motýl je příkladem druhu, který je v 
      různých částech areálu vázán na různé živné rostliny. Vývoj je jednogenerační (konec 
      května – začátek července). Samice kladou hromadně do kupiček několik desítek až stovek 
      žlutavých vajíček na osluněné listy mladých jasanů, přičemž na jednom stromku bývá někdy 
      i více snůšek. Je-li snůška umístěna na vzrostlejší strom, není to výše než 4 metry od 
      země. Zajímavé je, že více snůšek lze na některých stromcích najít opakovaně i několik let 
      po sobě až do doby, než daný strom ”přeroste”. Vajíčka jsou zpravidla uložena ve dvou 
      vrstvách. Mladé housenky jsou gregarické, opřádají vlákny nejprve lístek se snůškou, později 
      celý list (vytvoří tzv. primární hnízdo). Po zkonzumování ”snůškového” listu se housenky 
      přesunou na další listy; housenky z různých snůšek se někdy mísí a znovu opřádají vhodnou 
      větev živné rostliny zámotkem (tzv. sekundárním hnízdem). Larvy v hnízdech nezimují, ale 
      již na podzim se rozlézají a hibernují solitérně v přízemní vegetaci, v tuto dobu se musí 
      často slunit.</p>
      <h5><b>Péče o druh</b></h5>
      <p style="text-align: justify">V péči o lokality druhu je jednoznačně negativně hodnocen 
      nízký podíl porostů s hospodářským tvarem lesa středního, nedostatečný podíl a malá rozloha 
      pasek, zarůstání lesních lemů a lemů kolem cest. Druhu neprospívají především změny druhové 
      skladby porostů, výsadba stanovištně nebo geograficky nepůvodních druhů dřevin, plošná 
      likvidace jasanového náletu, mechanizovaná příprava půdy, zejména naorávání pasek a aplikace 
      biocidů. V ideálním habitatu je zachována stálá nabídka po většinu dne osluněných pasek a 
      světlin s mladými jasany a zároveň dostatek nektaronosných křovin a bylin. Světliny od sebe 
      musí být v takové vzdálenosti, aby přeletující samice vždy našly dostatek míst s podmínkami 
      vhodnými pro kladení (maximálně 300 metrů). Vhodné je také propojení již osídlených světliny
      lesními cestami či průseky.</p>'
    )
  )
text_lycdis <-
  paste(
    HTML(
      '<h5><b>Ekologie druhu</b></h5>
      <p style="text-align: justify">Dospělci se vyskytují ve dvou až třech generacích od dubna 
      do září. Létají za teplého počasí a sají nektar, mají poměrně velkou disperzní schopnost. 
      Hostitelskými rostlinami housenek jsou šťovíky, rdesno hadí kořen, aj. Vývoj trvá až jeden 
      rok. Populace žijící na jižní Moravě je spíše eurytopní, často jej lze zastihnout i mimo jeho 
      preferovaná stanoviště, tedy i na ruderálech, v intravilánech obcí, okrajích polí, apod.</p>
      <h5><b>Péče o druh</b></h5>
      <p style="text-align: justify">Na konkrétních lokalitách působí negativně na populaci druhu 
      především intenzivní pastva s plošným sečením nedopasků, plošné sečení luk, vícenásobná nebo 
      nevhodně načasovaná seč a změny vodního režimu (meliorace lokality). Naopak extenzivní způsob 
      hospodaření (mozaikovitá seč či pastva), management vedoucí k zachování vhodného vodního 
      režimu je pro zachování biotopu druhu a přežívání populací vhodný. Absence managementu vede 
      k přirozeným změnám stanoviště (zarůstání), které se stávají zvláště problematické stejně 
      jako další negativní vlivy v případě, že v okolí nenachází další vhodný habitat, který by 
      zajistil přežití dané populace. Zalesňování lokalit má stejný dopad jako zarůstání v případě 
      absence managementu a je proto hodnoceno také negativně spolu s plošným používání biocidů a 
      hnojením konkrétních lokalit a jejich nejbližšího okolí.</p>'
    )
  )
text_Lep_1 <- 
  paste(
    HTML(
      '<h5><b>Metodika hodnocení stavu druhu<h5>
      <p style="text-align: justify">Frekvence monitoringu: </b>Minimálně 1 návštěva během 6 let</p>'
    )
  )
text_Lep_2 <- 
  paste(
    HTML(
      '<h5><b>Metodika hodnocení stavu druhu<h5>
      <p style="text-align: justify">Frekvence monitoringu: </b>Minimálně 1 návštěva během 6 let</p>'
    )
  )
text_SapOpen <-
  paste(
    HTML(
      '<h5><b>Metodika hodnocení stavu druhu<h5>
      <p style="text-align: justify">Frekvence monitoringu: </b>Minimálně 1 návštěva během 6 let</p>
      <p style="text-align: justify"><b>Doložení výskytu druhu:</b> Dospělci, jejich fragmenty a 
      všechna larvální stádia</p>
      <p style="text-align: justify"><b>Pokryvnost preferovaných habitatů: </b>Minimálně stabilní 
      pokryvnost (věkově strukturované světlé a rozvolněné listnaté (zejména dubové) lesní porosty s 
      převahou starších (100 a více let) stromů a parky, s dostatkem mrtvého dřeva a jejich 
      přechodovými formami spojenými s šetrnou obnovou těchto porostů (holiny po mýcení lesného 
      porostu osázené listnatými stromy až mladší listnaté lesy s ponecháním dostatečného množství 
      pařezů, starších výstavků.</p>
      <p style="text-align: justify"><b>Stav preferovaných habitatů:</b> Dostatečná přítomnost 
      mrtvého dřeva a zabezpečená kontinuita nabídky mrtvého dřeva</p> 
      <p style="text-align: justify"><b>Přímá likvidace preferovaného habitatu:</b> 
      Nevyskytuje se</p>
      <p style="text-align: justify"><b>Negativní vlivy:</b> Žádné</p>'
    )
  )
text_SapFor <-
  paste(
    HTML(
      '<h5><b>Metodika hodnocení stavu druhu<h5>
      <p style="text-align: justify"><b>Frekvence monitoringu: </b>Minimálně 1 návštěva během 6 let</p>
      <p style="text-align: justify"><b>Doložení výskytu druhu:</b> Dospělci, jejich fragmenty a 
      všechna larvální stádia</p>
      <p style="text-align: justify"><b>Hustota populace:</b> 
      <p style="text-align: justify"><b>Pokryvnost preferovaných habitatů: </b>Minimálně stabilní 
      pokryvnost (věkově strukturované světlé a rozvolněné listnaté (zejména dubové) lesní porosty s 
      převahou starších (100 a více let) stromů a parky, s dostatkem mrtvého dřeva a jejich 
      přechodovými formami spojenými s šetrnou obnovou těchto porostů (holiny po mýcení lesného 
      porostu osázené listnatými stromy až mladší listnaté lesy s ponecháním dostatečného množství 
      pařezů, starších výstavků.</p>
      <p style="text-align: justify"><b>Stav preferovaných habitatů:</b> Dostatečná přítomnost 
      mrtvého dřeva a zabezpečená kontinuita nabídky mrtvého dřeva</p> 
      <p style="text-align: justify"><b>Přímá likvidace preferovaného habitatu:</b> 
      Nevyskytuje se (např. převod jakékoliv části porostů na jehličnaté monokultury)</p>
      <p style="text-align: justify"><b>Negativní vlivy:</b> Žádné</p>'
    )
  )
text_luccer <-
  paste(
    HTML(
      '<h5><b>Ekologie druhu</b></h5>
      <p style="text-align: justify">Roháč obecný, největší
      evropský brouk, je obyvatelem doubrav a
      smíšených lesů, který proniká i do vhodných městských parků.
      Dává přednost teplým nížinným lesům, ale místy vystupuje i do
      vyšších poloh.Samice kladou vajíčka do trouchnivějících kmenů,
      klád a pařezů, <b>vývoj je v našich podmínkách víceletý (3-5 let)</b>,
      larvy se živí trouchnivějícím dřevem. Dospělí brouci se obvykle
      líhnou již na podzim a přezimují v kukelních komůrkách , v přírodě
      se objevují od května (výjimečně v teplých letech již od konce dubna)
      do srpna, maximum výskytu spadá do června a července. Přes den je
      brouky možné nalézat na kmenech a v korunách stromů, pozdě odpoledne
      a večer (při teplém počasí) létají v korunách stromů. Imaga se živí
      listím dubů, samce láká ronící míza.</p>'
    )
    #br(),
    #div(img(src = "luccer1.jpg", width = "100%", alt = "Jeninec sražený automobilem"),
    #    style = "text-align: center;"),
    #br(),
    #div(img(src = "luccer2.jpg", width = "100%", alt = "Samec"), 
    #    style = "text-align: center;")
  )
text_cercer <-
  paste(
    HTML(
      '<h5><b>Ekologie druhu</b></h5>
      <p style="text-align: justify">Tesařík obrovský se vyvíjí především v dubu, pouze vzácně v 
      jilmu a ořešáku, uváděn je i jasan a vrba, na jihu Evropy též jírovec maďal. Vyhledává 
      zejména osluněné stromy na okrajích lesů, v alejích, prosvětlených porostech na svazích a 
      solitérní stromy na loukách, pastvinách či parcích. Napadá hlavně starší živé stromy, vývoj 
      probíhá pod kůrou v živém lýku a později ve dřevě kmenů i silných větví v korunách. Délka 
      vývoje v ČR je <b>cca 3-5 let</b>. V přírodě se dospělý brouk vyskytuje od května do srpna, 
      maximum výskytu je od poloviny června do poloviny července. Brouci mají večerní a noční 
      aktivitu, přes den se často ukrývají ve výletových otvorech či v korunách stromů.</p>'
    )
  )
text_carmen <-
  paste(
    HTML(
      '<h5><b>Ekologie a biologie</b></h5>
      <p style="text-align: justify">Reliktní druh, stenotopní tyrfobiont. Žije výlučně na původních 
      rašeliništích, údolních i horských (tzv. vrchovištích), popř. rašelinných loukách či rašelinných 
      lesních stanovištích ve vrstvě živého rašeliníku. Imaga se vyskytují od konce dubna do září, s 
      maximem výskytu počátkem června; letní období přežívají v diapauze. Dospělci jsou aktivní 
      především v nočních hodinách. Imago i larva se živí drobnými bezobratlými, zvláště slimáky, červy 
      a hmyzími larvami, popř. pavouky. Vajíčka kladou samice na přelomu května a června, celkový vývoj 
      trvá asi sedm týdnů. Imaga přezimují, v hloubce cca 15 cm pod povrchem.</p>'
    )
  )
text_carvar <-
  paste(
    HTML('<h5><b>Ekologie a biologie</b></h5>
         <p style="text-align: justify">Střevlík hrbolatý žije především v předhůří a horách. Obývá 
         zejména listnaté (dubohabřiny až bučiny) a smíšené, ale i jehličnaté lesy, výjimečně se 
         vyskytuje i na otevřených biotopech které k lesům přiléhají. Druh byl opakovaně zjištěn i v 
         druhotných, mladších smrkových lesích (Oderské vrchy, Jeseníky), jeho vazba na původní 
         lesy není tedy jednoznačná. Jedná se o <b>velmi vlhkomilný druh</b> žijící na březích vodních 
         toků, prameništích, močálech a rašeliništích, vícekrát byl pozorován lovící přímo ve vodě.
         Je aktivní především v noci, méně častá je denní aktivita. Přes den se zpravidla ukrývá ve 
         vlhkém listí, mechu, podmáčené půdě a pod ležícími předměty (kmeny, kameny). Přezimuje 
         jako imago, v přírodě je aktivní od dubna.</p>
         <h5><b>Metodika hodnocení stavu druhu<h5>
         <p style="text-align: justify"><b>Početnost populace:</b> počet jedinců (dospělců) 
         odchycených na lokalitě do stanoveného počtu předem určeného typu zemních pastí.</p>
         <p style="text-align: justify"><b>Pokryvnost preferovaných habitatů:</b> procento výměry 
         lokality s výskytem souvislých celků preferovaných habitatů (listnaté (zejména dubohabřiny a 
         bučiny se zápojem korunového patra 50-80%), případně smíšené lesy (hlavně jedlobučiny, pak 
         smíšené smrčiny se zápojem 20-50%) s výskytem pramenišť, bažin, mokřin, lesních potůčků nebo 
         stojaté vody).</p>
         <p style="text-align: justify"><b>Přítomnost mrtvého dřeva:</b> zastoupení ležícího mrtvého 
         dřeva o tloušťce > 20 cm, jež zasahuje i do vodních ploch, do dvou metrů od obývaného 
         (semi)akvatického habitatu.</p>
         <p style="text-align: justify"><b>Vodní režim:</b> sleduje rozloha (délka v případě toků) 
         (semi)akvatických habitatů na lokalitě.</p>
         <p style="text-align: justify"><b>Přímá likvidace preferovaného habitatu:</b> nevyskytuje se
         velkoplošná těžba lesních porostů nebo změna porostního typu – převod jakékoliv části 
         porostů na velkoplošné jehličnaté monokultury.</p>
         <p style="text-align: justify"><b>Vliv managementu:</b> na lokalitách, kde je aktuálně cíleně 
         pro daný druh prováděn ochranářský i jiný management, je proveden odhad významu 
         konkrétního typu (či více typů) managementu s ohledem na doporučení popsaná v Souboru 
         doporučených opatření péče o danou EVL na početnost a vitalitu populace v současnosti.</p>
         <p style="text-align: justify"><b>Jiný negativní vliv:</b> jakýkoli jiný faktor se závažným 
         nepříznivým vlivem na stav lokality či potenciální lokality v současnosti či blízké 
         budoucnosti (cílená lesnická výsadba, výstavba apod.).</p>')
  )
text_grabil <-
  paste(
    HTML(
      '<h5><b>Ekologie a biologie</b></h5> 
      <p style="text-align: justify">Hlavním stanovištěm tohoto potápníka jsou zejména 
      hluboké nádrže a různá jezera s hustou vodní vegetací. V jižních oblastech svého 
      výskytu osidluje nejčastěji <b>prosluněné čisté až dystrofní vody</b>, v našich podmínkách 
      obýval pravděpodobně především neobhospodařované menší rybníky a nádrže. Dospělí potápníci
      zimují pravděpodobně ve vodě. Životní cyklus tohoto druhu je univoltinní, vajíčka jsou 
      kladena na jaře, larvy se vyvíjejí přes léto.</p>'
    )
  )
text_cuccin <-
  paste(
    HTML(
      '<h5><b>Ekologie a biologie</b></h5>
      <p style="text-align: justify">Larvy se vyvíjejí v hnijícím vlhkém, černohnědě zabarveném lýku 
      pod uvolněnou borkou padlých či zlomených listnatých stromů nebo ulomených silných větvích. 
      Hlavní hostitelské rostliny jsou buk, osika a další topoly, duby i jiné listnáče. 
      Larvy i imaga se živí hnijícím lýkem, larvy jsou příležitostně dravé. Vývoj trvá 
      minimálně dva roky, dospělci brouci se líhnou na konci léta či na podzim, 
      přezimují a na jaře se páří a kladou vajíčka. Nejčastěji jsou nalézáni na 
      podzim a v časném jaře. Dospělce je možno najít v kmenech v časnějším stadiu 
      rozpadu, nezřídka i v čerstvě padlých kmenech. To pravděpodobně souvisí s 
      dynamikou druhu a rozpadu dřeva. Dospělci vyhledávají čerstvější kmeny, zatímco 
      larvy na daných kmenech zůstávají, aby dokončily svůj vývoj.</p>'
    )
  )
text_limvio <-
  paste(
    HTML(
      '<h5><b>Ekologie a biologie</b></h5>
      <p style="text-align: justify">Kovařík fialový je striktně vázán na původní lesní 
      listnaté porosty, často pralesního charakteru, chybí však na lokalitách periodicky 
      zaplavovaných, písčinách, skalních svazích a v lesích s přerušeným kontinuem. Většina 
      lokalit na našem území je v nadmořské výšce od 400-500 m n.m.Larva se vyvíjí v dutinách 
      listnatých stromů, preferuje staré stromy, v nich pak dutiny větších rozměrů ve spodní 
      části kmene, chráněné proti dešti, avšak zároveň v kontaktu se zemní vlhkostí. Larva je 
      nekrofágní, případně i aktivně loví. Její vývoj trvá déle než rok. Imago opouští dutinu 
      velmi zřídka.</p>'
    )
  )
text_rosalp <-
  paste(
    HTML(
      '<h5><b>Ekologie a biologie</b></h5>
      <p style="text-align: justify">V našich podmínkách je tesařík alpský vázán především na 
      buk (Ralsko, Bílé Karpaty), ale je hlášen i z řady dalších druhů dřevin (např. jilm, 
      javor babyka) na jižní Moravě. Preferuje zachovalé, bukové lesy pralesního charakteru, 
      zvláště teplé, jižní svahy, ale i nížinné lužní lesy. Vývoj, minimálně tříletý, probíhá 
      v polosuchém až suchém dřevě větví a kmenů.Imaga kladou vajíčka do zasychajícího nebo 
      čerstvě zaschlého dřeva stojících pahýlů, zlomených stromů nebo větví. Larvy se vyvíjí 
      ve dřevě, poslední larvální instar přezimuje a kuklí se koncem května a v červnu 
      nehluboko pod povrchem dřeva. Imaga žijí od začátku června až do září, s maximem v 
      červenci a jsou aktivní zvláště za teplého, slunného počasí.</p>'
    )
  )
text_Carniv <-
  paste(
    HTML(
      '<h5><b>Metodika hodnocení druhu</b></h5>
      <p style="text-align: justify"><b>Přítomnost druhu:</b> Alespoň jeden věrohodný (C1) záznam 
      přítomnosti druhu na území EVL</p>
      <p style="text-align: justify"><b>Pokryvnost preferovaných habitatů:</b> Alespoň 70%
      plochy EVL pokryto lesy.</p>
      <p style="text-align: justify"><b>Stav preferovaných habitatů:</b> Nevyskytuje se 
      fragmentace lesních porostů.</p>
      <p style="text-align: justify"><b>Pytláctví:</b> Méně než 1 záznam za 6 let</p>
      <p style="text-align: justify"><b>Nežádoucí vlivy:</b> Žádné nové dálnice a silnice 
      1. třídy na území EVL.</p>'
      
    )
  )
text_canlup <-
  paste(
    HTML(
      '<h5><b>Ekologie a biologie</b></h5>
      <p style="text-align: justify">Původní prostředí vlka tvořila široká škála biotopů od 
      arktické tundry, přes lesy všeho druhu, po stepi a lesostepi v jižní Evropě. Sociální jednotka 
      je tvořena párem nebo smečkou, vlci však často žijí i samotářsky. Kořistí jsou větší druhy 
      kopytníků a drobná zvířata, významnou součástí potravy je i rostlinná strava a mršiny. V
      Evropě převažuje u vlka noční aktivita, přičemž ve dne smečka nebo jedinec odpočívá v úkrytu. 
      Velikost teritoria je závislá na dostupnosti potravy, takže obecně platí, že v létě je 
      výrazně menší než v zimě, na jihu dosahuje teritorium menší rozlohy než v severních oblastech. 
      Jedinci i menší skupiny se někdy potulují mimo rámec vlastní smečky. Při těchto potulkách 
      jsou schopni uběhnout 18 – 28 km za den, při pronásledování kořisti byla zaznamenána 
      vzdálenost až 200 km za 24 hodin.</p>
      <p style="text-align: justify">Více informací o návratu vlků do české krajiny a jeho 
      vztahu s lidmi nabízí webové stránky <a href="https://www.navratvlku.cz/" target="_blank">
      Programu péče o vlka</a> AOPK ČR.</p>'
    )
  )
text_lynlyn <-
  paste(
    HTML(
      '<h5><b>Ekologie a biologie</b></h5> 
      <p style="text-align: justify">Za primární prostředí rysa v Evropě a na Sibiři se považují 
      lesy všeho druhu, obývá však i tundru v severních oblastech, středoasijské stepi a polopouště.
      Je to samotářské teritoriální zvíře, okrsky jedinců stejného pohlaví se mohou z malé části 
      překrývat. Teritorium samce bývá větší a obsahuje i více teritorií samic. Rys je aktivní 
      hlavně v noci. Jeho potrava je dosti rozmanitá, nejdůležitější složku tvoří menší kopytníci. 
      V našich podmínkách je zcela dominantní srnec. V oblastech, kde zvěř nebyla na přítomnost
      rysa zvyklá dosahuje zastoupení srnce v potravě vyšší podíl, než v místech s tradičním výskytem 
      rysa.</p>'
    )
  )

# UI ----
ui <- fluidPage(
  theme = "bootstrap.css",
  
  tags$head(tags$link(rel = "shortcut icon", 
                      href = "https://raw.githubusercontent.com/jonasgaigr/N2K.CZ/main/WWW/favicon.ico")),
  
  titlePanel(div(HTML("<h1>Hodnocení stavu evropsky významných druhů")
  ),
  windowTitle = "Hodnocení stavu evropsky významných druhů"),
  
  br(),
  
  column(2,
         htmlOutput("group_selector")),
  column(3,
         htmlOutput("species_selector")),
  column(2,
         htmlOutput("evl_selector")),
  column(2,
         numericInput("curryear", "Rok hodnocení", 2020)),
  column(3
  ),
  
  br(),
  br(),
  br(),
  br(),
  
  hr(),
  
  fluidRow(
    uiOutput("default")
  ),
  
  fluidRow(
    column(7, 
           leafletOutput(outputId = "mapa_evl", height = "600px", width = "100%") %>%
             withSpinner(color = "green")
    ),
    column(5, 
           htmlOutput("metodika"))
  ),
  
  br(),
  br(),
  
  fluidRow(dataTableOutput("mytable") %>%
             withSpinner(color = "green")),
  
  hr(),
  
  #fluidRow(htmlOutput("lokality")),
  
  #hr(),
  
  fluidRow(div(img(
    src = "https://raw.githubusercontent.com/jonasgaigr/N2K.CZ/main/WWW/LOGO.jpg", 
    height = 70), style = "text-align: center;")),
  
  br(),
  
  fluidRow(HTML('<center><b>© 2021 <a href="http://www.nature.cz" target="_blank">AOPK ČR</a></b></center>')),
  br()
)

# SERVER ----
server <- function(input, output, session) {
  
  # Stanovení maximální velikosti nahrávaného souboru
  options(shiny.maxRequestSize=50*1024^2)
  
  find.evl.SITECODE <- function(species) {
    return(subset(sites_subjects, sites_subjects$Název.latinsky == species)$Kód.lokality )
  }
  find.evl.NAZEV <- function(species) {
    return(subset(sites_subjects, sites_subjects$Název.latinsky == species)$Název.lokality)
  }
  find.evl.NUMBER <- function(species) {
    return(nrow(subset(sites_subjects, sites_subjects$Název.latinsky == species)))
  }
  
  species <- read.csv("https://raw.githubusercontent.com/jonasgaigr/N2K.CZ/main/evl_data_export_encoded.csv",
                      sep = ",",
                      stringsAsFactors = FALSE,
                      encoding = "UTF-8")
  
  # HODNOCENÍ EVL ----
  output$mytable <- renderDataTable({
    
    req(input$species)
    
    species$DRUH <- gsub("Maculinea", "Phengaris", species$DRUH)
    species$DRUH <- gsub("Callimorpha quadripunctaria", "Euplagia quadripunctaria", species$DRUH)
    species$DRUH <- gsub("Triturus montandoni", "Lissotriton montandoni", species$DRUH)
    species$DRUH <- gsub("Aspius aspius", "Leuciscus aspius", species$DRUH)
    species$DRUH <- gsub("Cobitis elongatoides", "Cobitis taneia", species$DRUH)
    
    species <- filter(species, DRUH == input$species)
    input_species <- input$species
    
    current_year <- input$curryear

    # Motýle ----
    Lep.1.clear <- function(species) {
      species <- species %>%
        mutate(
          # Převedení druhu na kategorickou veličinu
          DRUH = as.factor(DRUH),
          # Převedení datumu do vhodného formátu
          DATE = as.Date(as.character(DATUM_OD), format = '%d.%m.%Y'),
          # Redukce data na rok
          YEAR = substring(DATE, 1, 4),
          # Izolace kódu EVL
          SITECODE = substr(EVL, 1, 9),
          # Izolace názvu lokality
          NAZEV = substr(as.character(EVL), 12, nchar(as.character(EVL))),
          # Přítomnost druhu
          PRESENT = case_when(is.na(POCET) == TRUE | is.na(POCITANO) == TRUE ~ 0,
                              (is.na(POCET) == FALSE & POCET > 0) | 
                                (is.na(POCITANO) == FALSE & POCET > 0) ~ 1),
          # Převedení dostupných dat o početnosti na jednotné kategorie početnosti
          POCET_CAT = case_when((POCET > 0 & POCET <= 10) | REL_POC == "1-10" | 
                                  REL_POC == "ojediněle" & 
                                  (POCITANO == "imaga" | POCITANO == "jedinci" |
                                     POCITANO == "samice" | POCITANO == "samci") ~ 1,
                                (POCET > 10 & POCET <= 35) | REL_POC == "11-100" | 
                                  REL_POC == "vzácně" & 
                                  (POCITANO == "imaga" | POCITANO == "jedinci" |
                                     POCITANO == "samice" | POCITANO == "samci") ~ 2,
                                (POCET > 35 & POCET < 10) | REL_POC == "11-100" | 
                                  REL_POC == "roztroušeně" & 
                                  (POCITANO == "imaga" | POCITANO == "jedinci" |
                                     POCITANO == "samice" | POCITANO == "samci") ~ 3,
                                (POCET > 100) | REL_POC == "hojně" & 
                                  (POCITANO == "imaga" | POCITANO == "jedinci" |
                                     POCITANO == "samice" | POCITANO == "samci") ~ 4),
          # Vyhodnocení početnosti z hlediska metodiky
          POCETNOST = case_when(DRUH == "Euplagia quadripunctaria" & POCET_CAT > 1 ~ 1,
                                DRUH == "Euplagia quadripunctaria" & POCET_CAT <= 1 ~ 0,
                                DRUH == "Phengaris nausithous" & POCET_CAT > 1 ~ 1,
                                DRUH == "Phengaris nausithous" & POCET_CAT <= 1 ~ 0,
                                DRUH == "Phengaris teleius" & POCET_CAT > 1 ~ 1,
                                DRUH == "Phengaris teleius" & POCET_CAT <= 1 ~ 0,
                                DRUH == "Euphydryas aurinia" & POCET_CAT > 1 ~ 1,
                                DRUH == "Euphydryas aurinia" & POCET_CAT <= 1 ~ 0,
                                DRUH == "Euphydryas maturna" & POCET_CAT > 1 ~ 1,
                                DRUH == "Euphydryas maturna" & POCET_CAT <= 1 ~ 0),
          # Hodnocení výskytu hostitelských rostliv podle poznámky - řešení pro data, která nejsou nasbírána v souladu s metodikou
          # Pro data nasbíraná podle metodiky bude analýza sahat do strukturované poznámky, která by měla obsahovat data ze Survey123
          PLANTS = case_when(DRUH == "Phengaris nausithous" & 
                               grepl(paste(c("krvav", "toten", "sangui"), collapse = "|"), POZNAMKA, ignore.case = TRUE) == TRUE &
                               grepl(paste(c("jedn, vzác, ojed"), collapse = "|"), POZNAMKA, ignore.case = TRUE) == FALSE ~ 1,
                             DRUH == "Phengaris nausithous" & 
                               grepl(paste(c("krvav", "toten", "sangui"), collapse = "|"), POZN_BIO, ignore.case = TRUE) == TRUE &
                               grepl(paste(c("jedn, vzác, ojed"), collapse = "|"), POZN_BIO, ignore.case = TRUE) == FALSE ~ 1,
                             DRUH == "Phengaris nausithous" & 
                               grepl(paste(c("krvav", "toten", "sangui"), collapse = "|"), POZNAMKA, ignore.case = TRUE) == TRUE &
                               grepl(paste(c("jedn, vzác, ojed"), collapse = "|"), POZNAMKA, ignore.case = TRUE) == TRUE ~ 0,
                             DRUH == "Phengaris nausithous" & 
                               grepl(paste(c("krvav", "toten", "sangui"), collapse = "|"), POZN_BIO, ignore.case = TRUE) == TRUE &
                               grepl(paste(c("jedn, vzác, ojed"), collapse = "|"), POZN_BIO, ignore.case = TRUE) == TRUE ~ 0,
                             DRUH == "Phengaris teleius" & 
                               grepl(paste(c("krvav", "toten", "sangui"), collapse = "|"), POZNAMKA, ignore.case = TRUE) == TRUE &
                               grepl(paste(c("jedn, vzác, ojed"), collapse = "|"), POZNAMKA, ignore.case = TRUE) == FALSE ~ 1,
                             DRUH == "Phengaris teleius" & 
                               grepl(paste(c("krvav", "toten", "sangui"), collapse = "|"), POZN_BIO, ignore.case = TRUE) == TRUE &
                               grepl(paste(c("jedn, vzác, ojed"), collapse = "|"), POZN_BIO, ignore.case = TRUE) == FALSE ~ 1,
                             DRUH == "Phengaris teleius" & 
                               grepl(paste(c("krvav", "toten", "sangui"), collapse = "|"), POZNAMKA, ignore.case = TRUE) == TRUE &
                               grepl(paste(c("jedn, vzác, ojed"), collapse = "|"), POZNAMKA, ignore.case = TRUE) == TRUE ~ 0,
                             DRUH == "Phengaris teleius" & 
                               grepl(paste(c("krvav", "toten", "sangui"), collapse = "|"), POZN_BIO, ignore.case = TRUE) == TRUE &
                               grepl(paste(c("jedn, vzác, ojed"), collapse = "|"), POZN_BIO, ignore.case = TRUE) == TRUE ~ 0,
                             DRUH == "Euplagia quadripunctiria" &
                               grepl(paste(c("sadc", "sadec", "hlaváč", "bodlák", "bodlak"), collapse = "|"), POZNAMKA, ignore.case = TRUE) == TRUE &
                               grepl(paste(c("jedn, vzác, ojed"), collapse = "|"), POZNAMKA, ignore.case = TRUE) == FALSE ~ 1,
                             DRUH == "Euplagia quadripunctiria" &
                               grepl(paste(c("sadc", "sadec", "hlaváč", "bodlák", "bodlak"), collapse = "|"), POZNAMKA, ignore.case = TRUE) == TRUE &
                               grepl(paste(c("jedn, vzác, ojed"), collapse = "|"), POZNAMKA, ignore.case = TRUE) == TRUE ~ 0,
                             DRUH == "Euphydryas aurinia" &
                               grepl(paste(c("succisa, čert, certk"), collapse = "|"), POZNAMKA, ignore.case = TRUE) == TRUE &
                               grepl(paste(c("jedn, vzác, ojed"), collapse = "|"), POZNAMKA, ignore.case = TRUE) == FALSE ~ 1,
                             DRUH == "Euphydryas aurinia" &
                               grepl(paste(c("succisa, čert, certk"), collapse = "|"), POZNAMKA, ignore.case = TRUE) == TRUE &
                               grepl(paste(c("jedn, vzác, ojed"), collapse = "|"), POZNAMKA, ignore.case = TRUE) == TRUE ~ 0,
                             DRUH == "Euphydryas maturna" &
                               grepl(paste(c("fraxinus"), collapse = "|"), POZNAMKA, ignore.case = TRUE) == TRUE ~ 1,
                             DRUH == "Euphydryas maturna" &
                               grepl(paste(c("fraxinus"), collapse = "|"), POZNAMKA, ignore.case = TRUE) == TRUE  ~ 0)) 
      species <- species %>%
        # Posledních 6 let pro hodnocení
        filter(YEAR >= (current_year - 6))
      return(species)
    }
    
    # Hodnocení lokality (hodnocení EVL v konečné verzi bude záviset na velikosti EVL - EVL může obsahovat více jak jednu lokalitu)
    Lep.1.site <- function(species) {
      # Data frame obsahující všechny EVL druhu
      species_site <- as.data.frame(cbind(as.vector(find.evl.SITECODE(input_species)), 
                                          as.vector(find.evl.NAZEV(input_species))))
      colnames(species_site) <- c("SITECODE", "NAZEV")
      hab_evl <- Lep.1.clear(species) %>% # Aplikace funcḱce GROUP_clear na hrubá data z NDOP 
        bind_rows(species_site) %>% # Použití kódu a názvu EVL 
        group_by(SITECODE) %>% # Rozdělení dat z NDOP podle kódu lokality - každá EVL je analyzována zvlášť
        # Vytvoření nové matice
        summarise(SITECODE = SITECODE,
                  NAZEV = NAZEV,
                  # Vyhodnocení přítomnosti druhu - pro pozitivní záznam stačí 1 záznam za období
                  PRESENCE = case_when(max(na.omit(PRESENT)) == -Inf ~ "CHYBÍ DATA",
                                       max(na.omit(PRESENT)) >= 1 ~ "ANO",
                                       max(na.omit(PRESENT)) == 0 ~ "NE"),
                  # Optimistická analýza výskytu hostitelských rostlin
                  ROSTLINY = case_when(max(na.omit(PLANTS)) == 1 ~ "DOSTATEČNÝ",
                                       max(na.omit(PLANTS)) == 0 ~ "NEDOSTATEČNÝ",
                                       max(na.omit(PLANTS)) == -Inf ~ "CHYBÍ DATA"),
                  HABITAT = NA, # Hodnocení stavu prostředí zatím nebylo prováděno
                  # Zatím nemonitorované parametry
                  LIKVIDACE = NA,
                  NEGATIV = NA,
                  MANAGEMENT = NA,
                  MONITOR = NA,
                  OVERALL = NA)
      hab_evl <- hab_evl[hab_evl$SITECODE %in% species_site$SITECODE,]
      hab_evl <- hab_evl[!duplicated(hab_evl$SITECODE),]
      return(hab_evl)
    }
    
    # Tabulka s upravenými názvy a vybarvená podle hodnot
    Lep.1.semafor <-  function(species) {
      datatable(species, 
                rownames = FALSE,
                filter = "top",
                colnames = c("KÓD EVL", "NÁZEV EVL", "PŘÍTOMNOST DRUHU", "STAV HABITATU", 
                             "DOSTATEČNÝ VÝSKYT ŽIVNÝCH ROSTLIN", "PŘÍMÁ LIKVIDACE HABITATU", 
                             "VLIV MANAGEMENTU", "NEGATIVNÍ VLIVY",
                             "POSLEDNÍ MONITORING", "CELKOVÉ HODNOCENÍ")) %>%
        formatStyle(columns = "PRESENCE",
                    background = styleEqual(c("ANO", "NE", "CHYBÍ DATA"), 
                                            c("green", "red", "grey"))) %>%
        formatStyle(columns = "ROSTLINY",
                    background = styleEqual(c("DOSTATEČNÝ", "NEDOSTATEČNÝ", "CHYBÍ DATA"), 
                                            c("green", "red", "grey")))
    }
    
    Lep.2.clear <- function(species) {
      species <- species %>%
        mutate(
          DRUH = as.factor(DRUH),
          DATE = as.Date(as.character(DATUM_OD), format = '%d.%m.%Y'),
          YEAR = substring(DATE, 1, 4),
          SITECODE = substr(EVL, 1, 9),
          NAZEV = substr(as.character(EVL), 12, nchar(as.character(EVL))),
          PRESENT = case_when(is.na(POCET) == TRUE | is.na(POCITANO) == TRUE ~ 0,
                              (is.na(POCET) == FALSE & POCET > 0) | 
                                (is.na(POCITANO) == FALSE & POCET > 0) ~ 1)) 
      species <- species %>%
        filter(YEAR >= (current_year - 6))
      return(species)
    }
    
    Lep.2.site <- function(species) {
      species_site <- as.data.frame(cbind(as.vector(find.evl.SITECODE(input_species)), 
                                          as.vector(find.evl.NAZEV(input_species))))
      colnames(species_site) <- c("SITECODE", "NAZEV")
      hab_evl <- Lep.2.clear(species) %>%
        bind_rows(species_site) %>%
        group_by(SITECODE) %>%
        summarise(SITECODE = SITECODE,
                  NAZEV = NAZEV,
                  PRESENCE = case_when(max(na.omit(PRESENT)) == -Inf ~ "CHYBÍ DATA",
                                       max(na.omit(PRESENT)) >= 1 ~ "ANO",
                                       max(na.omit(PRESENT)) == 0 ~ "NE"),
                  HABITAT = NA,
                  LIKVIDACE = NA,
                  NEGATIV = NA,
                  MANAGEMENT = NA,
                  MONITOR = NA,
                  OVERALL = NA)
      hab_evl <- hab_evl[hab_evl$SITECODE %in% species_site$SITECODE,]
      hab_evl <- hab_evl[!duplicated(hab_evl$SITECODE),]
      return(hab_evl)
    }
    
    Lep.2.semafor <-  function(species) {
      datatable(species, 
                rownames = FALSE,
                filter = "top",
                colnames = c("KÓD EVL", "NÁZEV EVL", "PŘÍTOMNOST DRUHU", 
                             "STAV HABITATU", "PŘÍMÁ LIKVIDACE HABITATU", "VLIV MANAGEMENTU", 
                             "NEGATIVNÍ VLIVY", "POSLEDNÍ MONITORING", "CELKOVÉ HODNOCENÍ")) %>%
        formatStyle(columns = "PRESENCE",
                    background = styleEqual(c("ANO", "NE", "CHYBÍ DATA"), 
                                            c("green", "red", "grey")))
    }
    
    Lep.3.clear <- function(species) {
      species <- species %>%
        mutate(
          DRUH = as.factor(DRUH),
          DATE = as.Date(as.character(DATUM_OD), format = '%d.%m.%Y'),
          YEAR = substring(DATE, 1, 4),
          SITECODE = substr(EVL, 1, 9),
          NAZEV = substr(as.character(EVL), 12, nchar(as.character(EVL))),
          PRESENT = case_when(is.na(POCET) == TRUE | is.na(POCITANO) == TRUE ~ 0,
                              (is.na(POCET) == FALSE & POCET > 0) | 
                                (is.na(POCITANO) == FALSE & POCET > 0) ~ 1),
          POCET_CAT = case_when((POCET > 0 & POCET <= 10) | REL_POC == "1-10" | 
                                  REL_POC == "ojediněle" & 
                                  (POCITANO == "imaga" | POCITANO == "jedinci") ~ 1,
                                (POCET > 10 & POCET <= 35) | REL_POC == "11-100" | 
                                  REL_POC == "vzácně" & 
                                  (POCITANO == "imaga" | POCITANO == "jedinci") ~ 2,
                                (POCET > 35 & POCET < 10) | REL_POC == "11-100" | 
                                  REL_POC == "roztroušeně" & 
                                  (POCITANO == "imaga" | POCITANO == "jedinci") ~ 3,
                                (POCET > 100) | REL_POC == "hojně" & 
                                  (POCITANO == "imaga" | POCITANO == "jedinci") ~ 4),
          POCETNOST = case_when(DRUH == "Eriogaster catax" & POCET_CAT > 1 ~ 1,
                                DRUH == "Eriogaster catax" & POCET_CAT <= 1 ~ 0)) 
      species <- species %>%
        filter(YEAR >= (current_year - 6))
      return(species)
    }
    
    Lep.3.site <- function(species) {
      species_site <- as.data.frame(cbind(as.vector(find.evl.SITECODE(input_species)), 
                                          as.vector(find.evl.NAZEV(input_species))))
      colnames(species_site) <- c("SITECODE", "NAZEV")
      hab_evl <- Lep.3.clear(species) %>%
        bind_rows(species_site) %>%
        group_by(SITECODE) %>%
        summarise(SITECODE = SITECODE,
                  NAZEV = NAZEV,
                  PRESENCE = case_when(max(na.omit(PRESENT)) == -Inf ~ "CHYBÍ DATA",
                                       max(na.omit(PRESENT)) >= 1 ~ "ANO",
                                       max(na.omit(PRESENT)) == 0 ~ "NE"),
                  POCETNOST = case_when(max(na.omit(POCETNOST)) == -Inf ~ "CHYBÍ DATA",
                                        max(na.omit(POCETNOST)) == 1 ~ "DOSTATEČNÁ",
                                        max(na.omit(POCETNOST)) == 0 ~ "NEDOSTATEČNÁ"),
                  HABITAT = NA,
                  LIKVIDACE = NA,
                  NEGATIV = NA,
                  MANAGEMENT = NA,
                  MONITOR = NA,
                  OVERALL = NA)
      hab_evl <- hab_evl[hab_evl$SITECODE %in% species_site$SITECODE,]
      hab_evl <- hab_evl[!duplicated(hab_evl$SITECODE),]
      return(hab_evl)
    }
    
    Lep.3.semafor <-  function(species) {
      datatable(species, 
                rownames = FALSE,
                filter = "top",
                colnames = c("KÓD EVL", "NÁZEV EVL", "PŘÍTOMNOST DRUHU", 
                             "POČETNOST POPULACE", "STAV HABITATU",
                             "PŘÍMÁ LIKVIDACE HABITATU", "VLIV MANAGEMENTU", "NEGATIVNÍ VLIVY",
                             "POSLEDNÍ MONITORING", "CELKOVÉ HODNOCENÍ")) %>%
        formatStyle(columns = "PRESENCE",
                    background = styleEqual(c("ANO", "NE", "CHYBÍ DATA"), 
                                            c("green", "red", "grey"))) %>%
        formatStyle(columns = "POCETNOST",
                    background = styleEqual(c("DOSTATEČNÁ", "NEDOSTATEČNÁ", "CHYBÍ DATA"), 
                                            c("green", "red", "grey")))
    }
    
    # Brouci -----
    # Carabidae ----
    # Carabus hungaricus
    CarOpen.clear <- function(species) {
      species <- species %>%
        mutate(
          DRUH = as.factor(DRUH),
          DATE = as.Date(as.character(DATUM_OD), format = '%d.%m.%Y'),
          YEAR = substring(DATE, 1, 4),
          SITECODE = substr(as.character(EVL), 1, 9),
          NAZEV = substr(as.character(EVL), 12, nchar(as.character(EVL))),
          PRESENT = case_when(is.na(POCET) == FALSE & POCET > 0 & is.na(POCITANO) == TRUE ~ 0,
                              is.na(POCET) == FALSE | POCET > 0 | is.na(POCITANO) == FALSE ~ 1),
          POCET_CAT = case_when((POCET > 0 & POCET <= 10) | REL_POC == "1-10" | 
                                  REL_POC == "ojediněle" & 
                                  (POCITANO == "imaga" | POCITANO == "jedinci") ~ 1,
                                (POCET > 10 & POCET <= 35) | REL_POC == "11-100" | 
                                  REL_POC == "vzácně" & 
                                  (POCITANO == "imaga" | POCITANO == "jedinci") ~ 2,
                                (POCET > 35 & POCET < 10) | REL_POC == "11-100" | 
                                  REL_POC == "roztroušeně" & 
                                  (POCITANO == "imaga" | POCITANO == "jedinci") ~ 3,
                                (POCET > 100) | REL_POC == "hojně" & 
                                  (POCITANO == "imaga" | POCITANO == "jedinci") ~ 4),
          POCETNOST = case_when(POCET_CAT > 1 ~ 1,
                                POCET_CAT <= 1 ~ 0))
      species <- species %>%
        filter(YEAR >= (current_year - 6))
      return(species)
    }
    
    CarOpen.site <- function(species) {
      species_site <- as.data.frame(cbind(as.vector(find.evl.SITECODE(input_species)), 
                                          as.vector(find.evl.NAZEV(input_species))))
      colnames(species_site) <- c("SITECODE", "NAZEV")
      hab_evl <- CarOpen.clear(species) %>%
        bind_rows(species_site) %>%
        group_by(SITECODE) %>%
        summarise(SITECODE = SITECODE,
                  NAZEV = NAZEV,
                  PRESENCE = case_when(max(na.omit(PRESENT[YEAR >= (current_year - 2)])) == -Inf ~ "CHYBÍ DATA",
                                       max(na.omit(PRESENT[YEAR >= (current_year - 2)])) >= 1 ~ "ANO",
                                       max(na.omit(PRESENT[YEAR >= (current_year - 2)])) == 0 ~ "NE"),
                  HABITAT = NA,
                  LIKVIDACE = NA,
                  NEGATIV = NA,
                  MONITOR = NA,
                  OVERALL = NA)
      hab_evl <- hab_evl[hab_evl$SITECODE %in% species_site$SITECODE,]
      hab_evl <- hab_evl[!duplicated(hab_evl$SITECODE),]
      return(hab_evl)
    }
    
    CarOpen.semafor <-  function(species) {
      datatable(species, 
                rownames = FALSE,
                filter = "top",
                colnames = c("KÓD EVL", "NÁZEV EVL", "PŘÍTOMNOST DRUHU", "STAV HABITATU", 
                             "PŘÍMÁ LIKVIDACE HABITATU", "NEGATIVNÍ VLIVY", "POSLEDNÍ MONITORING",
                             "CELKOVÉ HODNOCENÍ")) %>%
        formatStyle(columns = "PRESENCE",
                    background = styleEqual(c("ANO", "CHYBÍ DATA"), c("green", "grey")))
    }
    
    
    CarAqua.clear <- function(species) {
      species <- species %>%
        mutate(
          DRUH = as.factor(DRUH),
          DATE = as.Date(as.character(DATUM_OD), format = '%d.%m.%Y'),
          YEAR = substring(DATE, 1, 4),
          SITECODE = substr(as.character(EVL), 1, 9),
          NAZEV = substr(as.character(EVL), 12, nchar(as.character(EVL))),
          PRESENT = case_when(is.na(POCET) == FALSE & POCET > 0 & is.na(POCITANO) == TRUE ~ 0,
                              is.na(POCET) == FALSE | POCET > 0 | is.na(POCITANO) == FALSE ~ 1),
          POCET_CAT = case_when((POCET > 0 & POCET <= 10) | REL_POC == "1-10" | 
                                  REL_POC == "ojediněle" & 
                                  (POCITANO == "imaga" | POCITANO == "jedinci") ~ 1,
                                (POCET > 10 & POCET <= 35) | REL_POC == "11-100" | 
                                  REL_POC == "vzácně" & 
                                  (POCITANO == "imaga" | POCITANO == "jedinci") ~ 2,
                                (POCET > 35 & POCET < 10) | REL_POC == "11-100" | 
                                  REL_POC == "roztroušeně" & 
                                  (POCITANO == "imaga" | POCITANO == "jedinci") ~ 3,
                                (POCET > 100) | REL_POC == "hojně" & 
                                  (POCITANO == "imaga" | POCITANO == "jedinci") ~ 4),
          POCETNOST = case_when(POCET_CAT > 1 ~ 1,
                                POCET_CAT <= 1 ~ 0))
      species <- species %>%
        filter(YEAR >= (current_year - 6))
      return(species)
    }
    
    CarAqua.site <- function(species) {
      species_site <- as.data.frame(cbind(as.vector(find.evl.SITECODE(input_species)), 
                                          as.vector(find.evl.NAZEV(input_species))))
      colnames(species_site) <- c("SITECODE", "NAZEV")
      hab_evl <- CarAqua.clear(species) %>%
        bind_rows(species_site) %>%
        group_by(SITECODE) %>%
        summarise(SITECODE = SITECODE,
                  NAZEV = NAZEV,
                  PRESENCE = case_when(max(na.omit(PRESENT)) == -Inf ~ "CHYBÍ DATA",
                                       max(na.omit(PRESENT)) >= 1 ~ "ANO",
                                       max(na.omit(PRESENT)) == 0 ~ "NE"),
                                    HABITAT = NA,
                  VODA = NA,
                  LIKVIDACE = NA,
                  MANAGEMENT = NA,
                  NEGATIV = NA,
                  MONITOR = NA,
                  OVERALL = NA)
      hab_evl <- hab_evl[hab_evl$SITECODE %in% species_site$SITECODE,]
      hab_evl <- hab_evl[!duplicated(hab_evl$SITECODE),]
      return(hab_evl)
    }
    
    CarAqua.semafor <-  function(species) {
      datatable(species, 
                rownames = FALSE,
                filter = "top",
                colnames = c("KÓD EVL", "NÁZEV EVL", "PŘÍTOMNOST DRUHU", "STAV HABITATU", "VODNÍ REŽIM", 
                             "PŘÍMÁ LIKVIDACE HABITATU", "VLIV MANAGEMENTU", "NEGATIVNÍ VLIVY",
                             "POSLEDNÍ MONITORING", "CELKOVÉ HODNOCENÍ")) %>%
        formatStyle(columns = "PRESENCE",
                    background = styleEqual(c("ANO", "CHYBÍ DATA"), c("green", "grey")))
    }
    
    # Saproxyličtí brouci ----
    SapOpen.clear <- function(species) {
      species <- species %>%
        mutate(
          DRUH = as.factor(DRUH),
          DATE = as.Date(as.character(DATUM_OD), format = '%d.%m.%Y'),
          YEAR = substring(DATE, 1, 4),
          SITECODE = substr(as.character(EVL), 1, 9),
          NAZEV = substr(as.character(EVL), 12, nchar(as.character(EVL))),
          PRESENT = case_when(is.na(POCET) == FALSE & POCET > 0 & is.na(POCITANO) == TRUE ~ 0,
                              is.na(POCET) == FALSE | POCET > 0 | is.na(POCITANO) == FALSE ~ 1))
      species <- species %>%
        filter(YEAR >= (current_year - 6))
      return(species)
    }
    
    SapOpen.site <- function(species) {
      species_site <- as.data.frame(cbind(as.vector(find.evl.SITECODE(input_species)), 
                                          as.vector(find.evl.NAZEV(input_species))))
      colnames(species_site) <- c("SITECODE", "NAZEV")
      hab_evl <- SapOpen.clear(species) %>%
        bind_rows(species_site) %>%
        group_by(SITECODE) %>%
        summarise(SITECODE = SITECODE,
                  NAZEV = NAZEV,
                  PRESENCE = case_when(max(na.omit(PRESENT)) == -Inf ~ "CHYBÍ DATA",
                                       max(na.omit(PRESENT)) >= 1 ~ "ANO",
                                       max(na.omit(PRESENT)) == 0 ~ "NE"),
                  HABITAT = NA,
                  LIKVIDACE = NA,
                  MANAGEMENT = NA,
                  NEGATIV = NA,
                  MONITOR = NA,
                  OVERALL = NA)
      hab_evl <- hab_evl[hab_evl$SITECODE %in% species_site$SITECODE,]
      hab_evl <- hab_evl[!duplicated(hab_evl$SITECODE),]
      return(hab_evl)
    }
    
    SapOpen.semafor <-  function(species) {
      datatable(species, 
                rownames = FALSE,
                filter = "top",
                colnames = c("KÓD EVL", "NÁZEV EVL", "PŘÍTOMNOST DRUHU", "STAV HABITATU", 
                             "PŘÍMÁ LIKVIDACE HABITATU", "VLIV MANAGEMENTU", "NEGATIVNÍ VLIVY",
                             "POSLEDNÍ MONITORING", "CELKOVÉ HODNOCENÍ")) %>%
        formatStyle(columns = "PRESENCE",
                    background = styleEqual(c("ANO", "CHYBÍ DATA"), c("green", "grey")))
    }
    
    SapFor.clear <- function(species) {
      species <- species %>%
        mutate(
          DRUH = as.factor(DRUH),
          DATE = as.Date(as.character(DATUM_OD), format = '%d.%m.%Y'),
          YEAR = substring(DATE, 1, 4),
          SITECODE = substr(EVL, 1, 9),
          NAZEV = substr(as.character(EVL), 12, nchar(as.character(EVL))),
          PRESENT = case_when(is.na(POCET) == FALSE & POCET > 0 & is.na(POCITANO) == TRUE ~ 0,
                              is.na(POCET) == FALSE | POCET > 0 | is.na(POCITANO) == FALSE ~ 1)
        ) 
      species <- species %>%
        filter(YEAR >= (current_year - 6))
      return(species)
    }
    
    SapFor.site <- function(species) {
      species_site <- as.data.frame(cbind(as.vector(find.evl.SITECODE(input_species)), 
                                          as.vector(find.evl.NAZEV(input_species))))
      colnames(species_site) <- c("SITECODE", "NAZEV")
      hab_evl <- SapFor.clear(species) %>%
        bind_rows(species_site) %>%
        group_by(SITECODE) %>%
        summarise(SITECODE = SITECODE,
                  NAZEV = NAZEV,
                  PRESENCE = case_when(max(na.omit(PRESENT)) == -Inf ~ "CHYBÍ DATA",
                                       max(na.omit(PRESENT)) >= 1 ~ "ANO",
                                       max(na.omit(PRESENT)) == 0 ~ "NE"),
                  HABITAT = NA,
                  DREVO = NA,
                  LIKVIDACE = NA,
                  MANAGEMENT = NA,
                  NEGATIV = NA,
                  MONITOR = NA,
                  OVERALL = NA)
      hab_evl <- hab_evl[hab_evl$SITECODE %in% species_site$SITECODE,]
      hab_evl <- hab_evl[!duplicated(hab_evl$SITECODE),]
      return(hab_evl)
    }
    
    SapFor.semafor <-  function(species) {
      datatable(species, 
                rownames = FALSE,
                filter = "top",
                colnames = c("KÓD EVL", "NÁZEV EVL", "PŘÍTOMNOST DRUHU", 
                             "STAV HABITATU", "PŘÍTOMNOST MRVTÉHO DŘEVA", 
                             "PŘÍMÁ LIKVIDACE HABITATU", "VLIV MANAGEMENTU", "NEGATIVNÍ VLIVY", 
                             "POSLEDNÍ MONITORING", "CELKOVÉ HODNOCENÍ")) %>%
        formatStyle(columns = "PRESENCE",
                    background = styleEqual(c("ANO", "CHYBÍ DATA"), c("green", "grey")))
    }
    
    RosAlp.clear <- function(species) {
      species <- species %>%
        mutate(
          DRUH = as.factor(DRUH),
          DATE = as.Date(as.character(DATUM_OD), format = '%d.%m.%Y'),
          YEAR = substring(DATE, 1, 4),
          SITECODE = substr(EVL, 1, 9),
          NAZEV = substr(as.character(EVL), 12, nchar(as.character(EVL))),
          PRESENT = case_when(is.na(POCET) == FALSE & POCET > 0 & is.na(POCITANO) == TRUE ~ 0,
                              is.na(POCET) == FALSE | POCET > 0 | is.na(POCITANO) == FALSE ~ 1),
          POCETNOST = case_when(POCET >= 100 & SITECODE == "CZ0514243" ~ 1,
                                POCET < 100 & SITECODE == "CZ0514243" ~ 0,
                                POCET >= 10 & SITECODE != "CZ0514243" ~ 1,
                                POCET < 10 & SITECODE != "CZ0514243" ~ 0)
        ) 
      species <- species %>%
        filter(YEAR >= (current_year - 6))
      return(species)
    }
    
    RosAlp.site <- function(species) {
      species_site <- as.data.frame(cbind(as.vector(find.evl.SITECODE(input_species)), 
                                          as.vector(find.evl.NAZEV(input_species))))
      colnames(species_site) <- c("SITECODE", "NAZEV")
      hab_evl <- RosAlp.clear(species) %>%
        bind_rows(species_site) %>%
        group_by(SITECODE) %>%
        summarise(SITECODE = SITECODE,
                  NAZEV = NAZEV,
                  PRESENCE = case_when(max(na.omit(PRESENT)) == -Inf ~ "CHYBÍ DATA",
                                       max(na.omit(PRESENT)) >= 1 ~ "ANO",
                                       max(na.omit(PRESENT)) == 0 ~ "NE"),
                  POCET = case_when(max(na.omit(POCETNOST)) == -Inf ~ "CHYBÍ DATA",
                                        max(na.omit(POCETNOST)) == 1 ~ "DOSTATEČNÁ"),
                  HABITAT = NA,
                  DREVO = NA,
                  LIKVIDACE = NA,
                  MANAGEMENT = NA,
                  NEGATIV = NA,
                  MONITOR = NA,
                  OVERALL = NA)
      hab_evl <- hab_evl[hab_evl$SITECODE %in% species_site$SITECODE,]
      hab_evl <- hab_evl[!duplicated(hab_evl$SITECODE),]
      return(hab_evl)
    }
    
    RosAlp.semafor <-  function(species) {
      datatable(species, 
                rownames = FALSE,
                filter = "top",
                colnames = c("KÓD EVL", "NÁZEV EVL", "PŘÍTOMNOST DRUHU", 
                             "POČETNOST POPULACE", "STAV HABITATU", "PŘÍTOMNOST MRVTÉHO DŘEVA", 
                             "PŘÍMÁ LIKVIDACE HABITATU", "VLIV MANAGEMENTU", "NEGATIVNÍ VLIVY", 
                             "POSLEDNÍ MONITORING", "CELKOVÉ HODNOCENÍ")) %>%
        formatStyle(columns = "PRESENCE",
                    background = styleEqual(c("ANO", "CHYBÍ DATA"), c("green", "grey"))) %>%
        formatStyle(columns = "POCET",
                    background = styleEqual(c("DOSTATEČNÁ", "NEDOSTATEČNÁ", "CHYBÍ DATA"), 
                                            c("green", "red", "grey")))
    }
    
    # Graphoderus ----
    
    ColAqua.clear <- function(species) {
      species <- species %>%
        mutate(
          DRUH = as.factor(DRUH),
          DATE = as.Date(as.character(DATUM_OD), format = '%d.%m.%Y'),
          YEAR = substring(DATE, 1, 4),
          SITECODE = substr(as.character(EVL), 1, 9),
          NAZEV = substr(as.character(EVL), 12, nchar(as.character(EVL))),
          PRESENT = case_when(is.na(POCET) == FALSE & POCET > 0 & is.na(POCITANO) == TRUE ~ 0,
                              is.na(POCET) == FALSE | POCET > 0 | is.na(POCITANO) == FALSE ~ 1))
      species <- species %>%
        filter(YEAR >= (current_year - 6))
      return(species)
    }
    
    ColAqua.site <- function(species) {
      species_site <- as.data.frame(cbind(as.vector(find.evl.SITECODE(input_species)), 
                                          as.vector(find.evl.NAZEV(input_species))))
      colnames(species_site) <- c("SITECODE", "NAZEV")
      hab_evl <- ColAqua.clear(species) %>%
        bind_rows(species_site) %>%
        group_by(SITECODE) %>%
        summarise(SITECODE = SITECODE,
                  NAZEV = NAZEV,
                  PRESENCE = case_when(max(na.omit(PRESENT[YEAR >= current_year - 2])) == -Inf ~ "CHYBÍ DATA",
                                       max(na.omit(PRESENT[YEAR >= current_year - 2])) >= 1 ~ "ANO",
                                       max(na.omit(PRESENT[YEAR >= current_year - 2])) == 0 ~ "NE"),
                  HABITAT = NA,
                  PRUHLEDNOST = NA,
                  ZOOPLANKTON = NA,
                  LIKVIDACE = NA,
                  MANAGEMENT = NA,
                  NEGATIV = NA,
                  MONITOR = NA,
                  OVERALL = NA)
      hab_evl <- hab_evl[hab_evl$SITECODE %in% species_site$SITECODE,]
      hab_evl <- hab_evl[!duplicated(hab_evl$SITECODE),]
      return(hab_evl)
    }
    
    ColAqua.semafor <-  function(species) {
      datatable(species, 
                rownames = FALSE,
                filter = "top",
                colnames = c("KÓD EVL", "NÁZEV EVL", "PŘÍTOMNOST DRUHU",
                             "STAV HABITATU", "PŘÍTOMNOST ZOOPLANKTONU", "PRŮHLEDNOST VODY",
                             "PŘÍMÁ LIKVIDACE HABITATU", "VLIV MANAGEMENTU", "NEGATIVNÍ VLIVY",
                             "POSLEDNÍ MONITORING", "CELKOVÉ HODNOCENÍ")) %>%
        formatStyle(columns = "PRESENCE",
                    background = styleEqual(c("ANO", "CHYBÍ DATA"), c("green", "grey")))
    }
    
    # Vážky ----
    
    Odo.clear <- function(species) {
      species <- species %>%
        mutate(
          DRUH = as.factor(DRUH),
          DATE = as.Date(as.character(DATUM_OD), format = '%d.%m.%Y'),
          YEAR = substring(DATE, 1, 4),
          SITECODE = substr(as.character(EVL), 1, 9),
          NAZEV = substr(as.character(EVL), 12, nchar(as.character(EVL))),
          PRESENT = case_when(is.na(POCET) == FALSE & POCET > 0 & is.na(POCITANO) == TRUE ~ 0,
                              is.na(POCET) == FALSE | POCET > 0 | is.na(POCITANO) == FALSE ~ 1),
          POCETNOST = case_when(POCET > 10 ~ 1,
                                POCET <= 10 ~ 0))
      species <- species %>%
        filter(YEAR >= (current_year - 6))
      return(species)
    }
    
    Odo.site <- function(species) {
      species_site <- as.data.frame(cbind(as.vector(find.evl.SITECODE(input_species)), 
                                          as.vector(find.evl.NAZEV(input_species))))
      colnames(species_site) <- c("SITECODE", "NAZEV")
      hab_evl <- Odo.clear(species) %>%
        bind_rows(species_site) %>%
        group_by(SITECODE) %>%
        summarise(SITECODE = SITECODE,
                  NAZEV = NAZEV,
                  PRESENCE = case_when(max(na.omit(PRESENT)) == -Inf ~ "CHYBÍ DATA",
                                       max(na.omit(PRESENT)) >= 1 ~ "ANO",
                                       max(na.omit(PRESENT)) == 0 ~ "NE"),
                  REPRO = NA,
                  HABITAT = NA,
                  LIKVIDACE = NA,
                  NEGATIV = NA,
                  MANAGEMENT = NA,
                  MONITOR = NA,
                  OVERALL = NA)
      hab_evl <- hab_evl[hab_evl$SITECODE %in% species_site$SITECODE,]
      hab_evl <- hab_evl[!duplicated(hab_evl$SITECODE),]
      return(hab_evl)
    }
    
    Odo.semafor <- function(species) {
      datatable(species, 
                rownames = FALSE,
                filter = "top",
                colnames = c("KÓD EVL", "NÁZEV EVL", "PŘÍTOMNOST DRUHU", "ZAZNAMENÁNÍ REPRODUKCE", 
                             "STAV HABITATU", "PŘÍMÁ LIKVIDACE HABITATU", 
                             "VLIV MANAGEMENTU", "NEGATIVNÍ VLIVY",
                             "POSLEDNÍ MONITORING", "CELKOVÉ HODNOCENÍ")) %>%
        formatStyle(columns = "PRESENCE",
                    background = styleEqual(c("ANO", "NE", "CHYBÍ DATA"), 
                                            c("green", "red", "grey")))
    }
    
    # Other Invertebrates ----
    
    Gas.clear <- function(species) {
      species <- species %>%
        mutate(
          DRUH = as.factor(DRUH),
          DATE = as.Date(as.character(DATUM_OD), format = '%d.%m.%Y'),
          YEAR = substring(DATE, 1, 4),
          SITECODE = substr(as.character(EVL), 1, 9),
          NAZEV = substr(as.character(EVL), 12, nchar(as.character(EVL))),
          PRESENT = case_when(is.na(POCET) == FALSE & POCET > 0 & is.na(POCITANO) == TRUE ~ 0,
                              is.na(POCET) == FALSE | POCET > 0 | is.na(POCITANO) == FALSE ~ 1),
          SCHRANKY = case_when(is.na(POCET) == FALSE & POCET > 0 & is.na(POCITANO) == TRUE ~ 0,
                               is.na(POCET) == FALSE | POCET > 0 | is.na(POCITANO) == FALSE &
                                 (POCITANO == "schránky" | POCITANO == "mrtví jedinci") ~ 1),
          POCETNOST = case_when(POCET > 10 ~ 1,
                                POCET <= 10 ~ 0))
      species <- species %>%
        filter(YEAR >= (current_year - 3))
      return(species)
    }
    
    Gas.site <- function(species) {
      species_site <- as.data.frame(cbind(as.vector(find.evl.SITECODE(input_species)), 
                                          as.vector(find.evl.NAZEV(input_species))))
      colnames(species_site) <- c("SITECODE", "NAZEV")
      hab_evl <- Gas.clear(species) %>%
        bind_rows(species_site) %>%
        group_by(SITECODE) %>%
        summarise(SITECODE = SITECODE,
                  NAZEV = NAZEV,
                  PRESENCE = case_when(max(na.omit(PRESENT)) == -Inf ~ "CHYBÍ DATA",
                                       max(na.omit(PRESENT)) >= 1 ~ "ANO",
                                       max(na.omit(PRESENT)) == 0 ~ "NE"),
                  POCETNOST = case_when(max(na.omit(POCETNOST)) == -Inf ~ "CHYBÍ DATA",
                                        max(na.omit(POCETNOST)) == 1 ~ "DOSTATEČNÁ",
                                        max(na.omit(POCETNOST)) == 0 ~ "NEDOSTATEČNÁ"),
                  HABITAT = NA,
                  LIKVIDACE = NA,
                  NEGATIV = NA,
                  MANAGEMENT = NA,
                  MONITOR = NA,
                  OVERALL = NA)
      hab_evl <- hab_evl[hab_evl$SITECODE %in% species_site$SITECODE,]
      hab_evl <- hab_evl[!duplicated(hab_evl$SITECODE),]
      return(hab_evl)
    }
    
    Gas.semafor <- function(species) {
      datatable(species, 
                rownames = FALSE,
                filter = "top",
                colnames = c("KÓD EVL", "NÁZEV EVL", "PŘÍTOMNOST DRUHU", 
                             "POČETNOST POPULACE", "STAV HABITATU",  "PŘÍMÁ LIKVIDACE HABITATU", 
                             "VLIV MANAGEMENTU", "NEGATIVNÍ VLIVY",
                             "POSLEDNÍ MONITORING", "CELKOVÉ HODNOCENÍ")) %>%
        formatStyle(columns = "PRESENCE",
                    background = styleEqual(c("ANO", "NE", "CHYBÍ DATA"), 
                                            c("green", "red", "grey"))) %>%
        formatStyle(columns = "POCETNOST",
                    background = styleEqual(c("DOSTATEČNÁ", "NEDOSTATEČNÁ", "CHYBÍ DATA"), 
                                            c("green", "red", "grey")))
    }
    
    MarMar.clear <- function(species) {
      species <- species %>%
        mutate(
          DRUH = as.factor(DRUH),
          DATE = as.Date(as.character(DATUM_OD), format = '%d.%m.%Y'),
          YEAR = substring(DATE, 1, 4),
          SITECODE = substr(as.character(EVL), 1, 9),
          NAZEV = substr(as.character(EVL), 12, nchar(as.character(EVL))),
          PRESENT = case_when(is.na(POCET) == FALSE & POCET > 0 & is.na(POCITANO) == TRUE ~ 0,
                              is.na(POCET) == FALSE | POCET > 0 | is.na(POCITANO) == FALSE ~ 1),
          POCETNOST = case_when(POCET > 10 ~ 1,
                                POCET <= 10 ~ 0))
      species <- species %>%
        filter(YEAR >= (current_year - 6))
      return(species)
    }
    
    MarMar.site <- function(species) {
      species_site <- as.data.frame(cbind(as.vector(find.evl.SITECODE(input_species)), 
                                          as.vector(find.evl.NAZEV(input_species))))
      colnames(species_site) <- c("SITECODE", "NAZEV")
      hab_evl <- Gas.clear(species) %>%
        bind_rows(species_site) %>%
        group_by(SITECODE) %>%
        summarise(SITECODE = SITECODE,
                  NAZEV = NAZEV,
                  PRESENCE = case_when(max(na.omit(PRESENT)) == -Inf ~ "CHYBÍ DATA",
                                       max(na.omit(PRESENT)) >= 1 ~ "ANO",
                                       max(na.omit(PRESENT)) == 0 ~ "NE"),
                  POCETNOST = case_when(max(na.omit(POCETNOST)) == -Inf ~ "CHYBÍ DATA",
                                        max(na.omit(POCETNOST)) == 1 ~ "DOSTATEČNÁ",
                                        max(na.omit(POCETNOST)) == 0 ~ "NEDOSTATEČNÁ"),
                  HABITAT = NA,
                  LIKVIDACE = NA,
                  NEGATIV = NA,
                  MANAGEMENT = NA,
                  MONITOR = NA,
                  OVERALL = NA)
      hab_evl <- hab_evl[hab_evl$SITECODE %in% species_site$SITECODE,]
      hab_evl <- hab_evl[!duplicated(hab_evl$SITECODE),]
      return(hab_evl)
    }
    
    MarMar.semafor <- function(species) {
      datatable(species, 
                rownames = FALSE,
                filter = "top",
                colnames = c("KÓD EVL", "NÁZEV EVL", "PŘÍTOMNOST DRUHU", 
                             "POČETNOST POPULACE", "VITALITA POPULACE", "ZAZNAMENÁNÍ REPRODUKCE", 
                             "STAV HABITATU", 
                             "DOSTATEČNÝ VÝSKYT ŽIVNÝCH ROSTLIN", "PŘÍMÁ LIKVIDACE HABITATU", 
                             "VLIV MANAGEMENTU", "NEGATIVNÍ VLIVY",
                             "POSLEDNÍ MONITORING", "CELKOVÉ HODNOCENÍ")) %>%
        formatStyle(columns = "PRESENCE",
                    background = styleEqual(c("ANO", "NE", "CHYBÍ DATA"), 
                                            c("green", "red", "grey")))
    }
    
    Dec.clear <- function(species) {
      species <- species %>%
        mutate(
          DRUH = as.factor(DRUH),
          DATE = as.Date(as.character(DATUM_OD), format = '%d.%m.%Y'),
          YEAR = substring(DATE, 1, 4),
          SITECODE = substr(as.character(EVL), 1, 9),
          NAZEV = substr(as.character(EVL), 12, nchar(as.character(EVL))),
          PRESENT = case_when(is.na(POCET) == FALSE & POCET > 0 & is.na(POCITANO) == TRUE ~ 0,
                              is.na(POCET) == FALSE | POCET > 0 | is.na(POCITANO) == FALSE ~ 1),
          POCETNOST = case_when(POCET > 10 ~ 1,
                                POCET <= 10 ~ 0))
      species <- species %>%
        filter(YEAR >= (current_year - 6))
      return(species)
    }
    
    Dec.site <- function(species) {
      species_site <- as.data.frame(cbind(as.vector(find.evl.SITECODE(input_species)), 
                                          as.vector(find.evl.NAZEV(input_species))))
      colnames(species_site) <- c("SITECODE", "NAZEV")
      hab_evl <- Dec.clear(species) %>%
        bind_rows(species_site) %>%
        group_by(SITECODE) %>%
        summarise(SITECODE = SITECODE,
                  NAZEV = NAZEV,
                  PRESENCE = case_when(max(na.omit(PRESENT)) == -Inf ~ "CHYBÍ DATA",
                                       max(na.omit(PRESENT)) >= 1 ~ "ANO",
                                       max(na.omit(PRESENT)) == 0 ~ "NE"),
                  POCETNOST = case_when(max(na.omit(POCETNOST)) == -Inf ~ "CHYBÍ DATA",
                                        max(na.omit(POCETNOST)) == 1 ~ "DOSTATEČNÁ",
                                        max(na.omit(POCETNOST)) == 0 ~ "NEDOSTATEČNÁ"),
                  VITALITA = NA,
                  REPRO = NA,
                  HABITAT = NA,
                  LIKVIDACE = NA,
                  NEGATIV = NA,
                  MANAGEMENT = NA,
                  MONITOR = NA,
                  OVERALL = NA)
      hab_evl <- hab_evl[hab_evl$SITECODE %in% species_site$SITECODE,]
      hab_evl <- hab_evl[!duplicated(hab_evl$SITECODE),]
      return(hab_evl)
    }
    
    Dec.semafor <- function(species) {
      datatable(species, 
                rownames = FALSE,
                filter = "top",
                colnames = c("KÓD EVL", "NÁZEV EVL", "PŘÍTOMNOST DRUHU", "POČETNOST POPULACE", 
                             "VITALITA POPULACE", "ZAZNAMENÁNÍ REPRODUKCE", "STAV HABITATU", 
                             "DOSTATEČNÝ VÝSKYT ŽIVNÝCH ROSTLIN", "PŘÍMÁ LIKVIDACE HABITATU", 
                             "VLIV MANAGEMENTU", "NEGATIVNÍ VLIVY",
                             "POSLEDNÍ MONITORING", "CELKOVÉ HODNOCENÍ")) %>%
        formatStyle(columns = "PRESENCE",
                    background = styleEqual(c("ANO", "NE", "CHYBÍ DATA"), 
                                            c("green", "red", "grey")))
    }
    
    # Fish ----
    
    # Amphibia ----
    
    # Ssawci ----
    Carniv.semafor <- function(species) {
      datatable(species, 
                rownames = FALSE,
                filter = "top",
                colnames = c("KÓD EVL", "NÁZEV EVL", "PŘÍTOMNOST DRUHU", 
                             "ZAZNAMENÁNÍ REPRODUKCE", "POKRYVNOST LESŮ", 
                             "ÚBYTEK FRAGMENTACE", "PYTLÁCTVÍ", 
                             "NOVÉ DÁLNICE A SILNICE 1. TŘÍDY", "CELKOVÉ HODNOCENÍ")) %>%
        formatStyle(columns = "PRESENCE",
                    background = styleEqual(c("ANO", "CHYBÍ DATA"), c("green", "grey"))) %>%
        formatStyle(columns = "REPRODUKCE",
                    background = styleEqual(c("ANO", "NE", "CHYBÍ DATA"), 
                                            c("green", "red", "grey"))) %>%
        formatStyle(columns = "PYTLACTVI",
                    background = styleEqual(c("ANO", "NE", "CHYBÍ DATA"), 
                                            c("red", "green", "grey"))) %>%
        formatStyle(columns = "OVERALL",
                    background = styleEqual(c("POSITIVNÍ", "NEGATIVNÍ", "CHYBÍ DATA"), 
                                            c("green", "red", "grey"))) %>%
        formatStyle(columns = "HABITAT",
                    background = styleEqual(c("DOSTAČUJÍCÍ", "NEDOSTAČUJÍCÍ", "CHYBÍ DATA"), 
                                            c("green", "red", "grey")))
    }
    
    # RESULT ----
    # Motýle 
    if (input$species == "Phengaris nausithous") {
      result <- Lep.1.semafor(Lep.1.site(species))
    } 
    if (input$species == "Phengaris teleius") {
      result <- Lep.1.semafor(Lep.1.site(species))
    } 
    if (input$species == "Euplagia quadripunctaria") {
      result <- Lep.1.semafor(Lep.1.site(species))
    } 
    if (input$species == "Colias myrmidone") {
      result <- Lep.2.semafor(Lep.2.site(species))
    } 
    if (input$species == "Euphydryas aurinia") {
      result <- Lep.1.semafor(Lep.1.site(species))
    }   
    if (input$species == "Euphydryas maturna") {
      result <- Lep.1.semafor(Lep.1.site(species))
    }
    if (input$species == "Lycaena dispar") {
      result <- Lep.2.semafor(Lep.2.site(species))
    }  
    if (input$species == "Eriogaster catax") {
      result <- Lep.3.semafor(Lep.3.site(species))
    } 
    # Brouke
    if (input$species == "Carabus hungaricus") {
      result <- CarOpen.semafor(CarOpen.site(species))
    } 
    if (input$species == "Carabus menetriesi pacholei") {
      result <- CarAqua.semafor(CarAqua.site(species))
    } 
    if (input$species == "Carabus variolosus") {
      result <- CarAqua.semafor(CarAqua.site(species))
    } 
    if (input$species == "Bolbelasmus unicornis") {
      result <- SapOpen.semafor(SapOpen.site(species))
    } 
    if (input$species == "Lucanus cervus") {
      result <- SapOpen.semafor(SapOpen.site(species))
    } 
    if (input$species == "Cerambyx cerdo") {
      result <- SapOpen.semafor(SapOpen.site(species))
    } 
    if (input$species == "Osmoderma barnabita") {
      result <- SapOpen.semafor(SapOpen.site(species))
    }
    if (input$species == "Cucujus cinnaberinus") {
      result <- SapFor.semafor(SapFor.site(species))
    }
    if (input$species == "Rhysodes sulcatus") {
      result <- SapFor.semafor(SapFor.site(species))
    }
    if (input$species == "Limoniscus violaceus") {
      result <- SapFor.semafor(SapFor.site(species))
    }
    if (input$species == "Rosalia alpina") {
      result <- RosAlp.semafor(RosAlp.site(species))
    }
    if (input$species == "Graphoderus bilineatus") {
      result <- ColAqua.semafor(ColAqua.site(species))
    }
    # Vážke
    if (input$species == "Coenagrion ornatum") {
      result <- Odo.semafor(Odo.site(species))
    }
    if (input$species == "Leucorrhinia pectoralis") {
      result <- Odo.semafor(Odo.site(species))
    }
    if (input$species == "Ophiogomphus cecilia") {
      result <- Odo.semafor(Odo.site(species))
    }
    if (input$species == "Cordulegaster heros") {
      result <- Odo.semafor(Odo.site(species))
    }
    # Měkkýše
    if (input$species == "Anisus vorticulus") {
      result <- Gas.semafor(Gas.site(species))
    }   
    if (input$species == "Vertigo angustior") {
      result <- Gas.semafor(Gas.site(species))
    }   
    if (input$species == "Vertigo geyeri") {
      result <- Gas.semafor(Gas.site(species))
    }   
    if (input$species == "Vertigo moulinsiana") {
      result <- Gas.semafor(Gas.site(species))
    }   
    # Rake
    if (input$species == "Austropotamobius torrentium") {
      result <- Dec.semafor(Dec.site(species))
    }  
    # Rybe
    if (input$species == "Cottus gobio") {
      result <- icht_semafor(icht_site(icht_clear(species)))
    }
    if (input$species == "Bombina bombina" |
        input$species == "Bombina variegata" |
        input$species == "Triturus cristatus" |
        input$species == "Triturus carnifex" |
        input$species == "Triturus dobrogicus" |
        input$species == "Lissotriton montandoni") {
      result <- semafor(ampsite(amp_clear(species)))
    }
    if (input$species == "Rhinolophus hipposideros" |
        input$species == "Barbastella barbastellus" |
        input$species == "Myotis bechsteini" |
        input$species == "Myotis blythii" |
        input$species == "Myotis dasycneme" |
        input$species == "Myotis emarginatus" |
        input$species == "Myotis myotis") {
      result <- chir_semafor(chirsite(clear_chir(species)))
    }
    # Ssawci
    if (input$species == "Canis lupus") {
      result <- Carniv.semafor(Carniv.site(species))
    }
    if (input$species == "Ursus arctos") {
      result <- Carniv.semafor(Carniv.site(species))
    }
    if (input$species == "Lynx lynx") {
      result <- Carniv.semafor(Carniv.site(species))
    }
    
    result
    
  })
  
  
  # HODNOCENÍ LOKALIT ----
  output$mytable1 <- renderDataTable({
    
    species <- filter(species, DRUH == input$species)
    
    amp_clear <- function(species) {
      #PREVOD DATA NA POUŽITELNÝ FORMÁT
      species$DATE <- c(species$DATUM_MONIT)
      species$DATE <-
        format(as.Date(species$DATE, format = '%d.%m.%Y %H:%M', origin = "01-01-1990"))
      # PRÁZDNÉ SLOUPCE PRO VYČIŠTĚNÁ DATA
      species$POP_PRESENT <- NA
      species$POP_RELCATEGORY <- NA
      species$REPRO <- NA
      species[species == ""] <- NA
      # "ŽÁDNÝ" A "ŽÁDNÉ" NAHRAZENY "NA" PRO JEDNODUŠŠÍ ČIŠTĚNÍ DAT
      species[species == "žádný"] <- NA
      species[species == "žádný"] <- NA
      species[species == "na"] <- NA
      # SPECIES PRESENCE (P1) AND RELATIVE ABUNDANCE (P3)
      species <-
        within(species, POP_PRESENT[POP_VYSKYTDRUH == "ne"] <- 0)
      # POSTUPNĚ PROJEDE VŠECHNY ADULTNÍ KATEGORIE OD NEJNIŽŠÍCH VÝSKYTŮ PO NEVVYŠŠÍ
      # DO 10 JEDINCŮ
      species <-
        within(species, POP_RELCATEGORY[POP_POCETPOHLNEURCENYADULTREL == "do 10" |
                                          POP_POCETSAMICEREL == "do 10" |
                                          POP_POCETSAMECREL == "do 10" |
                                          POP_CELKPOCETVSEADULTREL == "do 10" |
                                          POP_CELKPOCETVSEEXEMPLARREL == "do 10"] <-
                 1)
      # NIŽŠÍ DESÍTKY
      species <-
        within(species, POP_RELCATEGORY[POP_POCETPOHLNEURCENYADULTREL == "řádově nižší desítky" |
                                          POP_POCETSAMICEREL == "řádově nižší desítky" |
                                          POP_POCETSAMECREL == "řádově nižší desítky" |
                                          POP_CELKPOCETVSEADULTREL == "řádově nižší desítky" |
                                          POP_CELKPOCETVSEEXEMPLARREL == "řádově nižší desítky"] <-
                 2)
      # VYŠŠÍ DESÍTKY
      species <-
        within(species, POP_RELCATEGORY[POP_POCETPOHLNEURCENYADULTREL == "řádově vyšší desítky" |
                                          POP_POCETSAMICEREL == "řádově vyšší desítky" |
                                          POP_POCETSAMECREL == "řádově vyšší desítky" |
                                          POP_CELKPOCETVSEADULTREL == "řádově vyšší desítky" |
                                          POP_CELKPOCETVSEEXEMPLARREL == "řádově vyšší desítky"] <-
                 3)
      # KOLEM 100
      species <-
        within(species, POP_RELCATEGORY[POP_POCETPOHLNEURCENYADULTREL == "cca 100" |
                                          POP_POCETSAMICEREL == "cca 100" |
                                          POP_POCETSAMECREL == "cca 100" |
                                          POP_CELKPOCETVSEADULTREL == "cca 100" |
                                          POP_CELKPOCETVSEEXEMPLARREL == "cca 100"] <-
                 4)
      # STOVKY JEDINCŮ
      species <-
        within(species, POP_RELCATEGORY[POP_POCETPOHLNEURCENYADULTREL == "řádově stovky" |
                                          POP_POCETSAMICEREL == "řádově stovky" |
                                          POP_POCETSAMECREL == "řádově stovky" |
                                          POP_CELKPOCETVSEADULTREL == "řádově stovky" |
                                          POP_CELKPOCETVSEEXEMPLARREL == "řádově stovky"] <-
                 5)
      ## RELATIVNÍ POČETNOSTI SE PŘEPÍŠOU DATY Z ABSOLUTNÍCH ##
      # DO 10 JEDINCŮ
      species <-
        within(species, POP_RELCATEGORY[(POP_POCETPOHLNEURCENYADULT > 0 &
                                           POP_POCETPOHLNEURCENYADULT < 10) |
                                          (POP_POCETSAMICE > 0 &
                                             POP_POCETSAMICE < 10) |
                                          (POP_POCETSAMEC > 0 &
                                             POP_POCETSAMEC < 10) |
                                          (POP_CELKPOCETVSEADULT > 0 &
                                             POP_CELKPOCETVSEADULT < 10) |
                                          (POP_CELKPOCETVSEEXEMPLAR > 0 &
                                             POP_CELKPOCETVSEEXEMPLAR < 10)] <- 1)
      # NIŽSÍ DESÍTKY
      species <-
        within(species, POP_RELCATEGORY[(POP_POCETPOHLNEURCENYADULT >= 10 &
                                           POP_POCETPOHLNEURCENYADULT < 50) |
                                          (POP_POCETSAMICE >= 10 &
                                             POP_POCETSAMICE < 50) |
                                          (POP_POCETSAMEC >= 10 &
                                             POP_POCETSAMEC < 50) |
                                          (POP_CELKPOCETVSEADULT >= 10 &
                                             POP_CELKPOCETVSEADULT < 50) |
                                          (POP_CELKPOCETVSEEXEMPLAR >= 10 &
                                             POP_CELKPOCETVSEEXEMPLAR < 50)] <- 2)
      # VYŠŠÍ DESÍTKY
      species <-
        within(species, POP_RELCATEGORY[(POP_POCETPOHLNEURCENYADULT >= 50 &
                                           POP_POCETPOHLNEURCENYADULT <= 95) |
                                          (POP_POCETSAMICE >= 50 &
                                             POP_POCETSAMICE <= 95) |
                                          (POP_POCETSAMEC >= 50 &
                                             POP_POCETSAMEC <= 95) |
                                          (POP_CELKPOCETVSEADULT >= 50 &
                                             POP_CELKPOCETVSEADULT <= 95) |
                                          (POP_CELKPOCETVSEEXEMPLAR >= 50 &
                                             POP_CELKPOCETVSEEXEMPLAR <= 95)] <- 3)
      # KOLEM 100
      species <-
        within(species, POP_RELCATEGORY[(POP_POCETPOHLNEURCENYADULT > 95 &
                                           POP_POCETPOHLNEURCENYADULT < 150) |
                                          (POP_POCETSAMICE > 95 &
                                             POP_POCETSAMICE < 150) |
                                          (POP_POCETSAMEC > 95 &
                                             POP_POCETSAMEC < 150) |
                                          (POP_CELKPOCETVSEADULT > 95 &
                                             POP_CELKPOCETVSEADULT < 150) |
                                          (POP_CELKPOCETVSEEXEMPLAR > 95 &
                                             POP_CELKPOCETVSEEXEMPLAR < 150)] <- 4)
      # STOVKY JEDINCŮ
      species <-
        within(species, POP_RELCATEGORY[(POP_POCETPOHLNEURCENYADULT >= 150) |
                                          (POP_POCETSAMICE >= 150) |
                                          (POP_POCETSAMEC >= 150) |
                                          (POP_CELKPOCETVSEADULT >= 150) |
                                          (POP_CELKPOCETVSEEXEMPLAR >= 150)] <-
                 5)
      # ŽÁDNÍ JEDINCI
      species <-
        within(species, POP_RELCATEGORY[POP_CELKPOCETVSEEXEMPLAR == 0] <-
                 0)
      # KONTROLA PŘÍTOMNOSTI SNŮŠEK A LARVÁLNÍCH STÁDIÍ
      species <-
        within(species, REPRO[POP_CELKPOCETVSESUBADULT > 0 |
                                is.na(POP_CELKPOCETVSESUBADULTREL) == FALSE |
                                POP_POCETJUVENIL > 0 |
                                is.na(POP_POCETJUVENILREL) == FALSE |
                                POP_POCETLARVA > 0 |
                                is.na(POP_POCETLARVAREL) == FALSE |
                                POP_POCETMETAMORF > 0 |
                                is.na(POP_POCETMETAMORFREL) == FALSE |
                                POP_CELKPOCETVSESNUSKA > 0 |
                                is.na(POP_CELKPOCETVSESNUSKAREL) == FALSE] <-
                 1)
      species$REPRO[is.na(species$REPRO)] <- 0
      # DOČIŠTĚNÍ VÝSKYTU
      species$POP_RELCATEGORY[is.na(species$POP_RELCATEGORY)] <- 0
      species <-
        within(species, POP_PRESENT[POP_RELCATEGORY > 0 | REPRO > 0] <- 1)
      species <-
        within(species, POP_PRESENT[POP_RELCATEGORY == 0 &
                                      REPRO == 0] <- 0)
      species <-
        within(species, POP_PRESENT[POP_VYSKYTDRUH == "ano"] <- 1)
      # DESTRUKCE HABITATU
      species$HAB_LOS <- NA
      species <-
        within(species, HAB_LOS[STA_STAVVODAPERIODTUN < 10 |
                                  STA_STAVVODALITORAL < 10 |
                                  grepl("vysychání mokřadního ekosystému)",
                                        species$VLV_CINNOSTDOPAD)] <-
                 1)
      species$HAB_LOS[is.na(species$HAB_LOS)] <- 0
      # PRŮHLEDNOST VODY
      species$TRANSP <- NA
      species <-
        within(species, TRANSP[STA_PRUHLEDNOSTVODA < 90 |
                                 STA_ZABARVENIZAKAL == "hnědý" |
                                 STA_ZABARVENIZAKAL == "zelený"] <-
                 1)
      species <-
        within(species, TRANSP[grepl("dno", species$STA_PRUHLEDNOSTVODAPOZN)] <-
                 0)
      species <- within(species, TRANSP[STA_PRUHLEDNOSTVODA >= 90 |
                                          STA_ZABARVENIZAKAL == "bez zákalu"] <-
                          0)
      species$TRANSP[is.na(species$TRANSP)] <- 0
      # PŘÍTOMNOST RYBÍ OSÁDKY A POLODIVOKÝCH KACHEN
      species$PRED <- NA
      species <-
        within(species, PRED[grepl("chov ryb intensita", species$VLV_CINNOSTDOPAD) |
                               grepl("predace", species$VLV_CINNOSTDOPAD)] <-
                 1)
      species$PRED[is.na(species$PRED)] <- 0
      # VYSYCHÁNÍ
      species$VYSYCH <- NA
      species <-
        within(species, VYSYCH[grepl("vysychání", species$VLV_CINNOSTDOPAD)] <-
                 1)
      # POKRYVNOST VODNÍ VEGETACE
      species$POKRYV <- NA
      species <-
        within(species, POKRYV[DRUH == "Bombina bombina" &
                                 STA_POKRVEGETACE <= 80 &
                                 STA_POKRVEGETACE > 0] <- 1)
      species <-
        within(species, POKRYV[DRUH == "Bombina bombina" &
                                 STA_POKRVEGETACE == 0] <- 0)
      species <-
        within(species, POKRYV[DRUH == "Lissotriton montandoni" &
                                 STA_POKRVEGETACE <= 80 &
                                 STA_POKRVEGETACE > 0] <- 1)
      species <-
        within(species, POKRYV[grepl("Triturus", species$DRUH) &
                                 STA_POKRVEGETACE <= 80 &
                                 STA_POKRVEGETACE > 0] <- 1)
      species <-
        within(species, POKRYV[grepl("zarust", species$STA_POZNAMKA)] <-
                 0)
    }
    
    current_year <- input$curryear
    ampsite <- function(species) {
      temp_species <- species %>%
        group_by(KOD_LOKAL, DRUH) %>%
        summarise(
          curr_year = max(POP_RELCATEGORY[ROK == current_year]),
          prev_year = max(POP_RELCATEGORY[ROK == (current_year - 1)]),
          prevprev_year = max(POP_RELCATEGORY[ROK == (current_year - 2)]),
          STAV = ifelse(
            curr_year == -Inf | prev_year == -Inf,
            ifelse(
              curr_year == -Inf | prevprev_year == -Inf,
              ifelse(
                prev_year == -Inf | prevprev_year == -Inf,
                NA,
                prev_year >= prevprev_year
              ),
              curr_year >= prevprev_year
            ),
            curr_year >= prev_year
          ),
          curr_hab = max(HAB_LOS[ROK == current_year]),
          prev_hab = max(HAB_LOS[ROK == current_year - 1]),
          prevprev_hab = max(HAB_LOS[ROK == current_year - 2]),
          HAB = ifelse(
            curr_hab == -Inf,
            ifelse(
              prev_hab == -Inf,
              ifelse(prevprev_hab == -Inf, NA,
                     prevprev_hab),
              prev_hab
            ),
            curr_hab
          ),
          curr_pred = max(PRED[ROK == current_year]),
          prev_pred = max(PRED[ROK == current_year - 1]),
          prevprev_pred = max(PRED[ROK == current_year - 2]),
          PREDACE = ifelse(
            curr_pred == -Inf,
            ifelse(
              prev_pred == -Inf,
              ifelse(prevprev_pred == -Inf, NA,
                     prevprev_pred),
              prev_pred
            ),
            curr_pred
          ),
          curr_transp = min(TRANSP[ROK == current_year]),
          prev_transp = min(TRANSP[ROK == current_year - 1]),
          prevprev_transp = min(TRANSP[ROK == current_year - 2]),
          TRANSPAR = ifelse(
            curr_transp == Inf,
            ifelse(
              prev_transp == Inf,
              ifelse(prevprev_transp == Inf, NA,
                     prevprev_transp),
              prev_transp
            ),
            curr_transp
          ),
          curr_repro = max(REPRO[ROK == current_year]),
          prev_repro = max(REPRO[ROK == current_year - 1]),
          prevprev_repro = max(REPRO[ROK == current_year - 2]),
          REPRODUKCE = ifelse(
            curr_repro == -Inf,
            ifelse(
              prev_repro == -Inf,
              ifelse(prevprev_repro == -Inf, 0, prevprev_repro),
              prev_repro
            ),
            curr_repro
          ),
          curr_present = max(na.omit(POP_PRESENT[ROK == current_year])),
          prev_present = max(na.omit(POP_PRESENT[ROK == current_year - 1])),
          prevprev_present = max(na.omit(POP_PRESENT[ROK == current_year - 2])),
          PRESENT = ifelse(
            curr_present == -Inf | 0,
            ifelse(
              prev_present == -Inf | 0,
              ifelse(prevprev_present == -Inf |
                       0, NA,
                     prevprev_present),
              prev_present
            ),
            curr_present
          ),
          curr_vege = min(na.omit(POKRYV[ROK == current_year])),
          prev_vege = min(na.omit(POKRYV[ROK == current_year - 1])),
          prevprev_vege = min(POKRYV[ROK == current_year - 2]),
          VEGE = ifelse(
            curr_vege == Inf,
            ifelse(
              prev_vege == Inf,
              ifelse(prevprev_vege == Inf, NA, prevprev_vege),
              prev_vege
            ),
            curr_vege
          ),
          OVERALL = ifelse((
            PRESENT == 0 | HAB == 1 | VEGE == 0 | REPRODUKCE == 0 |
              PREDACE == 1 |
              STAV == 0 | TRANSPAR == 1
          ),
          "NEPŘÍZNIVÉ",
          ifelse((
            PRESENT == 1 & HAB == 0 & VEGE == 1 & REPRODUKCE == 1 &
              PREDACE == 0 &
              STAV == 1 & TRANSPAR == 0
          ),
          "PŘÍZNIVÉ",
          "CHYBÍ DATA"
          )
          ),
          OVERALL = ifelse((
            PRESENT == 0 | HAB == 1 | VEGE == 0 | REPRODUKCE == 0 |
              PREDACE == 1 |
              STAV == 0 | TRANSPAR == 1
          ),
          0,
          ifelse((
            is.na(PRESENT) == TRUE | is.na(HAB) == TRUE |
              is.na(VEGE) == TRUE |
              is.na(REPRODUKCE) == TRUE |
              is.na(PREDACE) == TRUE |
              is.na(STAV) == TRUE |
              is.na(TRANSPAR) == TRUE |
              PRESENT == -Inf | HAB == -Inf |
              VEGE == Inf |
              REPRODUKCE == Inf | PREDACE == Inf |
              STAV == Inf |
              TRANSPAR == Inf
          ),
          "CHYBÍ DATA",
          1
          )
          ),
          NAZEV_LOKAL = unique(NAZ_LOKAL)[1]
        )
      site <- data.frame(LOKALDRUH = unique(temp_species[c("KOD_LOKAL", "DRUH")]))
      site$PRESENT_P1 <- temp_species$PRESENT
      site$PRESENT_P1[site$PRESENT_P1 == 1] <- "ANO"
      site$PRESENT_P1[site$PRESENT_P1 == 0] <- "NE"
      site$REPRODUKCE_P2 <- temp_species$REPRODUKCE
      site$REPRODUKCE_P2[site$REPRODUKCE_P2 == 1] <- "ANO"
      site$REPRODUKCE_P2[site$REPRODUKCE_P2 == 0] <- "NE"
      site$REPRODUKCE_P2[is.na(site$REPRODUKCE_P2) == TRUE |
                           site$REPRODUKCE_P2 == -Inf] <- "CHYBÍ DATA"
      site$TREND <- temp_species$STAV
      site$TREND[site$TREND == TRUE] <- "POSITIVNÍ"
      site$TREND[site$TREND == FALSE] <- "NEGATIVNÍ"
      site$TREND[site$PRESENT_P1 == "NE"] <- "BEZ VÝSKYTU"
      site$TREND[is.na(site$TREND) == TRUE] <- "CHYBÍ DATA"
      site$HAB_P3 <- temp_species$HAB
      site$HAB_P3[site$HAB_P3 == 1] <- "DESTRUKCE HABITATU"
      site$HAB_P3[site$HAB_P3 == 0] <- "HABITAT ZACHOVÁN"
      site$HAB_P3[site$HAB_P3 == -Inf] <- NA
      site$PREDACE <- temp_species$PREDACE
      site$PREDACE[site$PREDACE == 1] <- "ANO"
      site$PREDACE[site$PREDACE == 0] <- "NE"
      site$TRANSP <- temp_species$TRANSPAR
      site$TRANSP[site$TRANSP == 1] <- "ANO"
      site$TRANSP[site$TRANSP == 0] <- "NE"
      site$POKRYV <- temp_species$VEGE
      site$POKRYV[site$POKRYV == 1] <- "VYHOVUJÍCÍ"
      site$POKRYV[site$POKRYV == 0] <- "NEVYHOVUJÍCÍ"
      site$POKRYV[site$POKRYV == Inf |
                    is.na(site$POKRYV) == TRUE] <- "CHYBÍ DATA"
      site$OVERALL <- temp_species$OVERALL
      site$OVERALL[is.na(site$OVERALL) == TRUE] <- "CHYBÍ DATA"
      site$NAZ_LOKAL <- temp_species$NAZEV_LOKAL
      return(site)
    }
    
    semafor <- function(species) {
      datatable(
        species,
        filter = "top",
        colnames = c(
          "KÓD LOKALITY",
          "DRUH",
          "PŘÍTOMNOST DRUHU",
          "REPRODUKCE",
          "TREND POČETNOSTI",
          "STAV HABITATU",
          "PREDACE",
          "ZÁKAL",
          "POKRYVNOST VEGETACE",
          "CELKOVÉ HODNOCENÍ",
          "NÁZEV LOKALITY"
        ),
        rownames = FALSE
      ) %>%
        formatStyle(columns = "OVERALL",
                    background = styleEqual(c(0, "CHYBÍ DATA", 1),
                                            c("red", "grey", "green"))) %>%
        formatStyle(columns = "PRESENT_P1",
                    background = styleEqual(c("ANO", "NE"), c("green", "red"))) %>%
        formatStyle(columns = "TREND",
                    background = styleEqual(
                      c("POSITIVNÍ", "NEGATIVNÍ", "BEZ VÝSKYTU", "CHYBÍ DATA"),
                      c("green", "red", "red", "grey")
                    )) %>%
        formatStyle(columns = "REPRODUKCE_P2",
                    background = styleEqual(c("ANO", "NE", "CHYBÍ DATA"), c("green", "red", "grey"))) %>%
        formatStyle(columns = "HAB_P3",
                    background = styleEqual(
                      c("HABITAT ZACHOVÁN", "DESTRUKCE HABITATU"),
                      c("green", "red")
                    )) %>%
        formatStyle(columns = "PREDACE",
                    background = styleEqual(c("NE", "ANO"),
                                            c("green", "red"))) %>%
        formatStyle(columns = "TRANSP",
                    background = styleEqual(c("NE", "ANO"),
                                            c("green", "red"))) %>%
        formatStyle(columns = "POKRYV",
                    background = styleEqual(
                      c("VYHOVUJÍCÍ", "NEVYHOVUJÍCÍ", "CHYBÍ DATA"),
                      c("green", "red", "grey")
                    ))
    }
    
    amp_evl <- function(site) {
      temp_site <- site %>%
        mutate(ID = gsub("[^0-9.-]", "", LOKALDRUH.KOD_LOKAL)) %>%
        group_by(ID) %>%
        summarise(VÝSTUP = ifelse(mean(na.omit(OVERALL >= 0.5)), 1, 0),
                  LOKALITA = unique(NAZ_LOKAL)[1])
      return(temp_site)
    }
    
    semafor_evl <- function(species) {
      datatable(species,
                filter = "top",
                rownames = FALSE) %>%
        formatStyle(columns = "VÝSTUP",
                    background = styleEqual(c(1, 0),
                                            c("green", "red")))
    }
    
    
    if (input$species == "Bombina bombina" |
        input$species == "Bombina variegata" |
        input$species == "Triturus cristatus" |
        input$species == "Triturus carnifex" |
        input$species == "Triturus dobrogicus" |
        input$species == "Lissotriton montandoni") {
      result <- semafor_evl(amp_evl(ampsite(amp_clear(species))))
    }
    
    result
    
  })
  
  # MAPA EVL ----
  output$mapa_evl <- renderLeaflet({
    
    req(input$species)
    
    find.evl.SITECODE <- function(species) {
      return(subset(sites_subjects, sites_subjects$Název.latinsky == species)$Kód.lokality )
    }
    find.evl.NAZEV <- function(species) {
      return(subset(sites_subjects, sites_subjects$Název.latinsky == species)$Název.lokality)
    }
    find.evl.NUMBER <- function(species) {
      return(nrow(subset(sites_subjects, sites_subjects$Název.latinsky == species)))
    }
    
    species$DRUH <- gsub("Maculinea", "Phengaris", species$DRUH)
    species$DRUH <- gsub("Callimorpha quadripunctaria", "Euplagia quadripunctaria", species$DRUH)
    species$DRUH <- gsub("Triturus montandoni", "Lissotriton montandoni", species$DRUH)
    species$DRUH <- gsub("Aspius aspius", "Leuciscus aspius", species$DRUH)
    species$DRUH <- gsub("Cobitis elongatoides", "Cobitis taneia", species$DRUH)
    
    species <- filter(species, DRUH == input$species)
    input_species <- input$species
    
    current_year <- input$curryear
    
    # Motýle ----
    Lep.1.clear <- function(species) {
      species <- species %>%
        mutate(
          DRUH = as.factor(DRUH),
          DATE = as.Date(as.character(DATUM_OD), format = '%d.%m.%Y'),
          YEAR = substring(DATE, 1, 4),
          SITECODE = substr(EVL, 1, 9),
          NAZEV = substr(as.character(EVL), 12, nchar(as.character(EVL))),
          PRESENT = case_when(is.na(POCET) == TRUE | is.na(POCITANO) == TRUE ~ 0,
                              (is.na(POCET) == FALSE & POCET > 0) | 
                                (is.na(POCITANO) == FALSE & POCET > 0) ~ 1),
          POCET_CAT = case_when((POCET > 0 & POCET <= 10) | REL_POC == "1-10" | 
                                  REL_POC == "ojediněle" & 
                                  (POCITANO == "imaga" | POCITANO == "jedinci") ~ 1,
                                (POCET > 10 & POCET <= 35) | REL_POC == "11-100" | 
                                  REL_POC == "vzácně" & 
                                  (POCITANO == "imaga" | POCITANO == "jedinci") ~ 2,
                                (POCET > 35 & POCET < 10) | REL_POC == "11-100" | 
                                  REL_POC == "roztroušeně" & 
                                  (POCITANO == "imaga" | POCITANO == "jedinci") ~ 3,
                                (POCET > 100) | REL_POC == "hojně" & 
                                  (POCITANO == "imaga" | POCITANO == "jedinci") ~ 4),
          POCETNOST = case_when(DRUH == "Euplagia quadripunctaria" & POCET_CAT > 1 ~ 1,
                                DRUH == "Euplagia quadripunctaria" & POCET_CAT <= 1 ~ 0,
                                DRUH == "Phengaris nausithous" & POCET_CAT > 1 ~ 1,
                                DRUH == "Phengaris nausithous" & POCET_CAT <= 1 ~ 0,
                                DRUH == "Phengaris teleius" & POCET_CAT > 1 ~ 1,
                                DRUH == "Phengaris teleius" & POCET_CAT <= 1 ~ 0),
          PLANTS = case_when(DRUH == "Phengaris nausithous" & 
                               grepl(paste(c("krvav", "toten", "sangui"), collapse = "|"), POZNAMKA, ignore.case = TRUE) == TRUE &
                               grepl(paste(c("jedn, vzác, ojed"), collapse = "|"), POZNAMKA, ignore.case = TRUE) == FALSE ~ 1,
                             DRUH == "Phengaris nausithous" & 
                               grepl(paste(c("krvav", "toten", "sangui"), collapse = "|"), POZN_BIO, ignore.case = TRUE) == TRUE &
                               grepl(paste(c("jedn, vzác, ojed"), collapse = "|"), POZN_BIO, ignore.case = TRUE) == FALSE ~ 1,
                             DRUH == "Phengaris nausithous" & 
                               grepl(paste(c("krvav", "toten", "sangui"), collapse = "|"), POZNAMKA, ignore.case = TRUE) == TRUE &
                               grepl(paste(c("jedn, vzác, ojed"), collapse = "|"), POZNAMKA, ignore.case = TRUE) == TRUE ~ 0,
                             DRUH == "Phengaris nausithous" & 
                               grepl(paste(c("krvav", "toten", "sangui"), collapse = "|"), POZN_BIO, ignore.case = TRUE) == TRUE &
                               grepl(paste(c("jedn, vzác, ojed"), collapse = "|"), POZN_BIO, ignore.case = TRUE) == TRUE ~ 0,
                             DRUH == "Phengaris teleius" & 
                               grepl(paste(c("krvav", "toten", "sangui"), collapse = "|"), POZNAMKA, ignore.case = TRUE) == TRUE &
                               grepl(paste(c("jedn, vzác, ojed"), collapse = "|"), POZNAMKA, ignore.case = TRUE) == FALSE ~ 1,
                             DRUH == "Phengaris teleius" & 
                               grepl(paste(c("krvav", "toten", "sangui"), collapse = "|"), POZN_BIO, ignore.case = TRUE) == TRUE &
                               grepl(paste(c("jedn, vzác, ojed"), collapse = "|"), POZN_BIO, ignore.case = TRUE) == FALSE ~ 1,
                             DRUH == "Phengaris teleius" & 
                               grepl(paste(c("krvav", "toten", "sangui"), collapse = "|"), POZNAMKA, ignore.case = TRUE) == TRUE &
                               grepl(paste(c("jedn, vzác, ojed"), collapse = "|"), POZNAMKA, ignore.case = TRUE) == TRUE ~ 0,
                             DRUH == "Phengaris teleius" & 
                               grepl(paste(c("krvav", "toten", "sangui"), collapse = "|"), POZN_BIO, ignore.case = TRUE) == TRUE &
                               grepl(paste(c("jedn, vzác, ojed"), collapse = "|"), POZN_BIO, ignore.case = TRUE) == TRUE ~ 0,
                             DRUH == "Euplagia quadripunctiria" &
                               grepl(paste(c("sadc", "sadec", "hlaváč", "bodlák", "bodlak"), collapse = "|"), POZNAMKA, ignore.case = TRUE) == TRUE &
                               grepl(paste(c("jedn, vzác, ojed"), collapse = "|"), POZNAMKA, ignore.case = TRUE) == FALSE ~ 1)) 
      species <- species %>%
        filter(YEAR >= (current_year - 6))
      return(species)
    }
    
    Lep.1.site <- function(species) {
      species_site <- as.data.frame(cbind(as.vector(find.evl.SITECODE(input_species)), 
                                          as.vector(find.evl.NAZEV(input_species))))
      colnames(species_site) <- c("SITECODE", "NAZEV")
      hab_evl <- Lep.1.clear(species) %>%
        bind_rows(species_site) %>%
        group_by(SITECODE) %>%
        summarise(SITECODE = SITECODE,
                  NAZEV = NAZEV,
                  PRESENCE = case_when(max(na.omit(PRESENT)) == -Inf ~ "CHYBÍ DATA",
                                       max(na.omit(PRESENT)) >= 1 ~ "ANO",
                                       max(na.omit(PRESENT)) == 0 ~ "NE"),
                  POCETNOST = case_when(max(na.omit(POCETNOST)) == -Inf ~ "CHYBÍ DATA",
                                        max(na.omit(POCETNOST)) == 1 ~ "DOSTATEČNÁ",
                                        max(na.omit(POCETNOST)) == 0 ~ "NEDOSTATEČNÁ"),
                  VITALITA = case_when((max(na.omit(c(max(na.omit(POCET[YEAR == current_year])),
                                                      max(na.omit(POCET[YEAR == (current_year - 1)])),
                                                      max(na.omit(POCET[YEAR == (current_year - 2)])))))/
                                          mean(na.omit(c(max(na.omit(POCET[YEAR == (current_year - 3)])),
                                                         max(na.omit(POCET[YEAR == (current_year - 4)])),
                                                         max(na.omit(POCET[YEAR == (current_year - 5)])),
                                                         max(na.omit(POCET[YEAR == (current_year - 6)]))))))
                                       >= 0.85 ~ "POSITIVNÍ",
                                       (max(na.omit(c(max(na.omit(POCET[YEAR == current_year])),
                                                      max(na.omit(POCET[YEAR == (current_year - 1)])),
                                                      max(na.omit(POCET[YEAR == (current_year - 2)])))))/
                                          mean(na.omit(c(max(na.omit(POCET[YEAR == (current_year - 3)])),
                                                         max(na.omit(POCET[YEAR == (current_year - 4)])),
                                                         max(na.omit(POCET[YEAR == (current_year - 5)])),
                                                         max(na.omit(POCET[YEAR == (current_year - 6)]))))))
                                       < 0.85 ~ "NEGATIVNÍ",
                                       max(na.omit(c(max(na.omit(POCET[YEAR == current_year])),
                                                     max(na.omit(POCET[YEAR == (current_year - 1)])),
                                                     max(na.omit(POCET[YEAR == (current_year - 2)])))))
                                       == -Inf ~ "CHYBÍ DATA",
                                       PRESENCE == "CHYBÍ DATA" ~ "CHYBÍ DATA"),
                  HABITAT = NA,
                  ROSTLINY = case_when(max(na.omit(PLANTS)) == 1 ~ "DOSTATEČNÝ",
                                       max(na.omit(PLANTS)) == 0 ~ "NEDOSTATEČNÝ",
                                       max(na.omit(PLANTS)) == -Inf ~ "CHYBÍ DATA"),
                  LIKVIDACE = NA,
                  NEGATIV = NA,
                  MANAGEMENT = NA,
                  MONITOR = NA,
                  COLOUR = case_when(max(na.omit(PRESENT)) == 1 ~ "green",
                                     max(na.omit(PRESENT)) == -Inf ~ "grey",
                                     max(na.omit(PRESENT)) == 0 ~ "red"),
                  OVERALL = NA)
      hab_evl <- hab_evl[hab_evl$SITECODE %in% species_site$SITECODE,]
      hab_evl <- hab_evl[!duplicated(hab_evl$SITECODE),]
      return(hab_evl)
    }
    Lep.2.clear <- function(species) {
      species <- species %>%
        mutate(
          DRUH = as.factor(DRUH),
          DATE = as.Date(as.character(DATUM_OD), format = '%d.%m.%Y'),
          YEAR = substring(DATE, 1, 4),
          SITECODE = substr(EVL, 1, 9),
          NAZEV = substr(as.character(EVL), 12, nchar(as.character(EVL))),
          PRESENT = case_when(is.na(POCET) == TRUE | is.na(POCITANO) == TRUE ~ 0,
                              (is.na(POCET) == FALSE & POCET > 0) | 
                                (is.na(POCITANO) == FALSE & POCET > 0) ~ 1),
          PLANTS = case_when(DRUH == "Phengaris nausithous | Phengaris teleius" & 
                               (grepl(paste(c("krvav", "toten", "sangui"), collapse = "|"), POZNAMKA, ignore.case = TRUE) == TRUE &
                                  grepl(paste(c("jedn, vzác, ojed"), collapse = "|"), POZNAMKA, ignore.case = TRUE) == FALSE) |
                               (grepl(paste(c("krvav", "toten", "sangui"), collapse = "|"), POZN_BIO, ignore.case = TRUE) == TRUE &
                                  grepl(paste(c("jedn, vzác, ojed"), collapse = "|"), POZN_BIO, ignore.case = TRUE) == FALSE) ~ 1,
                             DRUH == "Phengaris nausithous | Phengaris teleius" & 
                               (grepl(paste(c("krvav", "toten", "sangui"), collapse = "|"), POZNAMKA, ignore.case = TRUE) == TRUE &
                                  grepl(paste(c("jedn, vzác, ojed"), collapse = "|"), POZNAMKA, ignore.case = TRUE) == TRUE) |
                               (grepl(paste(c("krvav", "toten", "sangui"), collapse = "|"), POZN_BIO, ignore.case = TRUE) == TRUE &
                                  grepl(paste(c("jedn, vzác, ojed"), collapse = "|"), POZN_BIO, ignore.case = TRUE) == TRUE) ~ 0)) 
      species <- species %>%
        filter(YEAR >= (current_year - 6))
      return(species)
    }
    
    Lep.2.site <- function(species) {
      species_site <- as.data.frame(cbind(as.vector(find.evl.SITECODE(input_species)), 
                                          as.vector(find.evl.NAZEV(input_species))))
      colnames(species_site) <- c("SITECODE", "NAZEV")
      hab_evl <- Lep.2.clear(species) %>%
        bind_rows(species_site) %>%
        group_by(SITECODE) %>%
        summarise(SITECODE = SITECODE,
                  NAZEV = NAZEV,
                  PRESENCE = case_when(max(na.omit(PRESENT)) == -Inf ~ "CHYBÍ DATA",
                                       max(na.omit(PRESENT)) >= 1 ~ "ANO",
                                       max(na.omit(PRESENT)) == 0 ~ "NE"),
                  HABITAT = NA,
                  ROSTLINY = case_when(max(na.omit(PLANTS)) == 1 ~ "DOSTATEČNÝ",
                                       max(na.omit(PLANTS)) == 0 ~ "NEDOSTATEČNÝ",
                                       max(na.omit(PLANTS)) == -Inf ~ "CHYBÍ DATA"),
                  LIKVIDACE = NA,
                  NEGATIV = NA,
                  MANAGEMENT = NA,
                  MONITOR = NA,
                  COLOUR = case_when(max(na.omit(PRESENT)) == 1 ~ "green",
                                     max(na.omit(PRESENT)) == -Inf ~ "grey",
                                     max(na.omit(PRESENT)) == 0 ~ "red"),
                  OVERALL = NA)
      hab_evl <- hab_evl[hab_evl$SITECODE %in% species_site$SITECODE,]
      hab_evl <- hab_evl[!duplicated(hab_evl$SITECODE),]
      return(hab_evl)
    }
    
    Lep.3.clear <- function(species) {
      species <- species %>%
        mutate(
          DRUH = as.factor(DRUH),
          DATE = as.Date(as.character(DATUM_OD), format = '%d.%m.%Y'),
          YEAR = substring(DATE, 1, 4),
          SITECODE = substr(EVL, 1, 9),
          NAZEV = substr(as.character(EVL), 12, nchar(as.character(EVL))),
          PRESENT = case_when(is.na(POCET) == TRUE | is.na(POCITANO) == TRUE ~ 0,
                              (is.na(POCET) == FALSE & POCET > 0) | 
                                (is.na(POCITANO) == FALSE & POCET > 0) ~ 1),
          POCET_CAT = case_when((POCET > 0 & POCET <= 10) | REL_POC == "1-10" | 
                                  REL_POC == "ojediněle" & 
                                  (POCITANO == "imaga" | POCITANO == "jedinci") ~ 1,
                                (POCET > 10 & POCET <= 35) | REL_POC == "11-100" | 
                                  REL_POC == "vzácně" & 
                                  (POCITANO == "imaga" | POCITANO == "jedinci") ~ 2,
                                (POCET > 35 & POCET < 10) | REL_POC == "11-100" | 
                                  REL_POC == "roztroušeně" & 
                                  (POCITANO == "imaga" | POCITANO == "jedinci") ~ 3,
                                (POCET > 100) | REL_POC == "hojně" & 
                                  (POCITANO == "imaga" | POCITANO == "jedinci") ~ 4),
          POCETNOST = case_when(DRUH == "Eriogaster catax" & POCET_CAT > 1 ~ 1,
                                DRUH == "Eriogaster catax" & POCET_CAT <= 1 ~ 0)) 
      species <- species %>%
        filter(YEAR >= (current_year - 6))
      return(species)
    }
    
    Lep.3.site <- function(species) {
      species_site <- as.data.frame(cbind(as.vector(find.evl.SITECODE(input_species)), 
                                          as.vector(find.evl.NAZEV(input_species))))
      colnames(species_site) <- c("SITECODE", "NAZEV")
      hab_evl <- Lep.3.clear(species) %>%
        bind_rows(species_site) %>%
        group_by(SITECODE) %>%
        summarise(SITECODE = SITECODE,
                  NAZEV = NAZEV,
                  PRESENCE = case_when(max(na.omit(PRESENT)) == -Inf ~ "CHYBÍ DATA",
                                       max(na.omit(PRESENT)) >= 1 ~ "ANO",
                                       max(na.omit(PRESENT)) == 0 ~ "NE"),
                  POCETNOST = case_when(max(na.omit(POCETNOST)) == -Inf ~ "CHYBÍ DATA",
                                        max(na.omit(POCETNOST)) == 1 ~ "DOSTATEČNÁ",
                                        max(na.omit(POCETNOST)) == 0 ~ "NEDOSTATEČNÁ"),
                  VITALITA = NA,
                  HABITAT = NA,
                  LIKVIDACE = NA,
                  NEGATIV = NA,
                  MANAGEMENT = NA,
                  MONITOR = NA,
                  COLOUR = case_when(max(na.omit(PRESENT)) == 1 ~ "green",
                                     max(na.omit(PRESENT)) == -Inf ~ "grey",
                                     max(na.omit(PRESENT)) == 0 ~ "red"),
                  OVERALL = NA)
      hab_evl <- hab_evl[hab_evl$SITECODE %in% species_site$SITECODE,]
      hab_evl <- hab_evl[!duplicated(hab_evl$SITECODE),]
      return(hab_evl)
    }
    
    # Brouci ----
    # Carabidae ----
    # Carabus hungaricus
    CarOpen.clear <- function(species) {
      species <- species %>%
        mutate(
          DRUH = as.factor(DRUH),
          DATE = as.Date(as.character(DATUM_OD), format = '%d.%m.%Y'),
          YEAR = substring(DATE, 1, 4),
          SITECODE = substr(as.character(EVL), 1, 9),
          NAZEV = substr(as.character(EVL), 12, nchar(as.character(EVL))),
          PRESENT = case_when(is.na(POCET) == FALSE & POCET > 0 & is.na(POCITANO) == TRUE ~ 0,
                              is.na(POCET) == FALSE | POCET > 0 | is.na(POCITANO) == FALSE ~ 1))
      species <- species %>%
        filter(YEAR >= (current_year - 3))
      return(species)
    }
    
    CarOpen.site <- function(species) {
      species_site <- as.data.frame(cbind(as.vector(find.evl.SITECODE(input_species)), 
                                          as.vector(find.evl.NAZEV(input_species))))
      colnames(species_site) <- c("SITECODE", "NAZEV")
      hab_evl <- CarOpen.clear(species) %>%
        bind_rows(species_site) %>%
        group_by(SITECODE) %>%
        summarise(SITECODE = SITECODE,
                  NAZEV = NAZEV,
                  PRESENCE = case_when(max(na.omit(PRESENT)) == -Inf ~ "CHYBÍ DATA",
                                       max(na.omit(PRESENT)) >= 1 ~ "ANO",
                                       max(na.omit(PRESENT)) == 0 ~ "NE"),
                  HABITAT = NA,
                  LIKVIDACE = NA,
                  MANAGEMENT = NA,
                  NEGATIV = NA,
                  OVERALL = NA,
                  COLOUR = case_when(max(na.omit(PRESENT)) == 1 ~ "green",
                                     max(na.omit(PRESENT)) == -Inf ~ "grey",
                                     max(na.omit(PRESENT)) == 0 ~ "red"),
                  VITALITA = NA)
      hab_evl <- hab_evl[hab_evl$SITECODE %in% species_site$SITECODE,]
      hab_evl <- hab_evl[!duplicated(hab_evl$SITECODE),]
      return(hab_evl)
    }
    
    CarAqua.clear <- function(species) {
      species <- species %>%
        mutate(
          DRUH = as.factor(DRUH),
          DATE = as.Date(as.character(DATUM_OD), format = '%d.%m.%Y'),
          YEAR = substring(DATE, 1, 4),
          SITECODE = substr(as.character(EVL), 1, 9),
          NAZEV = substr(as.character(EVL), 12, nchar(as.character(EVL))),
          PRESENT = case_when(is.na(POCET) == FALSE & POCET > 0 & is.na(POCITANO) == TRUE ~ 0,
                              is.na(POCET) == FALSE | POCET > 0 | is.na(POCITANO) == FALSE ~ 1))
      species <- species %>%
        filter(YEAR >= (current_year - 6))
      return(species)
    }
    
    CarAqua.site <- function(species) {
      species_site <- as.data.frame(cbind(as.vector(find.evl.SITECODE(input_species)), 
                                          as.vector(find.evl.NAZEV(input_species))))
      colnames(species_site) <- c("SITECODE", "NAZEV")
      hab_evl <- CarAqua.clear(species) %>%
        bind_rows(species_site) %>%
        group_by(SITECODE) %>%
        summarise(SITECODE = SITECODE,
                  NAZEV = NAZEV,
                  PRESENCE = case_when(max(na.omit(PRESENT)) == -Inf ~ "CHYBÍ DATA",
                                       max(na.omit(PRESENT)) >= 1 ~ "ANO",
                                       max(na.omit(PRESENT)) == 0 ~ "NE"),
                  HABITAT = NA,
                  LIKVIDACE = NA,
                  MANAGEMENT = NA,
                  NEGATIV = NA,
                  OVERALL = NA,
                  COLOUR = case_when(max(na.omit(PRESENT)) == 1 ~ "green",
                                     max(na.omit(PRESENT)) == -Inf ~ "grey",
                                     max(na.omit(PRESENT)) == 0 ~ "red"),
                  VITALITA = NA)
      hab_evl <- hab_evl[hab_evl$SITECODE %in% species_site$SITECODE,]
      hab_evl <- hab_evl[!duplicated(hab_evl$SITECODE),]
      return(hab_evl)
    }
    
    # Saproxyličtí brouci ----
    SapOpen.clear <- function(species) {
      species <- species %>%
        mutate(
          DRUH = as.factor(DRUH),
          DATE = as.Date(as.character(DATUM_OD), format = '%d.%m.%Y'),
          YEAR = substring(DATE, 1, 4),
          SITECODE = substr(as.character(EVL), 1, 9),
          NAZEV = substr(as.character(EVL), 12, nchar(as.character(EVL))),
          PRESENT = case_when(is.na(POCET) == FALSE & POCET > 0 & is.na(POCITANO) == TRUE ~ 0,
                              is.na(POCET) == FALSE | POCET > 0 | is.na(POCITANO) == FALSE ~ 1))
      species <- species %>%
        filter(YEAR >= (current_year - 6))
      return(species)
    }
    
    SapOpen.site <- function(species) {
      species_site <- as.data.frame(cbind(as.vector(find.evl.SITECODE(input_species)), 
                                          as.vector(find.evl.NAZEV(input_species))))
      colnames(species_site) <- c("SITECODE", "NAZEV")
      hab_evl <- SapOpen.clear(species) %>%
        bind_rows(species_site) %>%
        group_by(SITECODE) %>%
        summarise(SITECODE = SITECODE,
                  NAZEV = NAZEV,
                  PRESENCE = case_when(max(na.omit(PRESENT)) == -Inf ~ "CHYBÍ DATA",
                                       max(na.omit(PRESENT)) >= 1 ~ "ANO",
                                       max(na.omit(PRESENT)) == 0 ~ "NE"),
                  HABITAT = NA,
                  LIKVIDACE = NA,
                  MANAGEMENT = NA,
                  NEGATIV = NA,
                  OVERALL = NA,
                  COLOUR = case_when(max(na.omit(PRESENT)) == 1 ~ "green",
                                     max(na.omit(PRESENT)) == -Inf ~ "grey",
                                     max(na.omit(PRESENT)) == 0 ~ "red"),
                  VITALITA = NA)
      hab_evl <- hab_evl[hab_evl$SITECODE %in% species_site$SITECODE,]
      hab_evl <- hab_evl[!duplicated(hab_evl$SITECODE),]
      return(hab_evl)
    }
    
    SapFor.clear <- function(species) {
      species <- species %>%
        mutate(
          DRUH = as.factor(DRUH),
          DATE = as.Date(as.character(DATUM_OD), format = '%d.%m.%Y'),
          YEAR = substring(DATE, 1, 4),
          SITECODE = substr(EVL, 1, 9),
          NAZEV = substr(as.character(EVL), 12, nchar(as.character(EVL))),
          PRESENT = case_when(is.na(POCET) == FALSE & POCET > 0 & is.na(POCITANO) == TRUE ~ 0,
                              is.na(POCET) == FALSE | POCET > 0 | is.na(POCITANO) == FALSE ~ 1),
          POCETNOST = case_when(DRUH == "Rosalia alpina" & POCET >= 100 & SITECODE == "CZ0514243" ~ 1,
                                DRUH == "Rosalia alpina" & POCET < 100 & SITECODE == "CZ0514243" ~ 0,
                                DRUH == "Rosalia alpina" & POCET >= 10 & SITECODE != "CZ0514243" ~ 1,
                                DRUH == "Rosalia alpina" & POCET < 10 & SITECODE != "CZ0514243" ~ 0,
                                DRUH == "Rhysodes sulcatus" & POCET >= 10 ~ 1,
                                DRUH == "Rhysodes sulcatus" & POCET < 10 ~ 0)
        ) 
      species <- species %>%
        filter(YEAR >= (current_year - 6))
      return(species)
    }
    
    SapFor.site <- function(species) {
      species_site <- as.data.frame(cbind(as.vector(find.evl.SITECODE(input_species)), 
                                          as.vector(find.evl.NAZEV(input_species))))
      colnames(species_site) <- c("SITECODE", "NAZEV")
      hab_evl <- SapFor.clear(species) %>%
        bind_rows(species_site) %>%
        group_by(SITECODE) %>%
        summarise(SITECODE = SITECODE,
                  NAZEV = NAZEV,
                  PRESENCE = case_when(max(na.omit(PRESENT)) == -Inf ~ "CHYBÍ DATA",
                                       max(na.omit(PRESENT)) >= 1 ~ "ANO",
                                       max(na.omit(PRESENT)) == 0 ~ "NE"),
                  POCETNOST = case_when(max(na.omit(POCETNOST)) == -Inf ~ "CHYBÍ DATA",
                                        max(na.omit(POCETNOST)) == 1 ~ "DOSTATEČNÁ"),
                  VITALITA = NA,
                  HABITAT = NA,
                  DREVO = NA,
                  LIKVIDACE = NA,
                  MANAGEMENT = NA,
                  NEGATIV = NA,
                  OVERALL = NA,
                  COLOUR = case_when(max(na.omit(PRESENT)) == 1 ~ "green",
                                     max(na.omit(PRESENT)) == -Inf ~ "grey",
                                     max(na.omit(PRESENT)) == 0 ~ "red"),
                  VITALITA = NA)
      hab_evl <- hab_evl[hab_evl$SITECODE %in% species_site$SITECODE,]
      hab_evl <- hab_evl[!duplicated(hab_evl$SITECODE),]
      return(hab_evl)
    }
    
    # Graphoderus ----
    
    ColAqua.clear <- function(species) {
      species <- species %>%
        mutate(
          DRUH = as.factor(DRUH),
          DATE = as.Date(as.character(DATUM_OD), format = '%d.%m.%Y'),
          YEAR = substring(DATE, 1, 4),
          SITECODE = substr(as.character(EVL), 1, 9),
          NAZEV = substr(as.character(EVL), 12, nchar(as.character(EVL))),
          PRESENT = case_when(is.na(POCET) == FALSE & POCET > 0 & is.na(POCITANO) == TRUE ~ 0,
                              is.na(POCET) == FALSE | POCET > 0 | is.na(POCITANO) == FALSE ~ 1),
          POCETNOST = case_when(POCET > 10 ~ 1,
                                POCET <= 10 ~ 0))
      species <- species %>%
        filter(YEAR >= (current_year - 2))
      return(species)
    }
    
    ColAqua.site <- function(species) {
      species_site <- as.data.frame(cbind(as.vector(find.evl.SITECODE(input_species)), 
                                          as.vector(find.evl.NAZEV(input_species))))
      colnames(species_site) <- c("SITECODE", "NAZEV")
      hab_evl <- ColAqua.clear(species) %>%
        bind_rows(species_site) %>%
        group_by(SITECODE) %>%
        summarise(SITECODE = SITECODE,
                  NAZEV = NAZEV,
                  PRESENCE = case_when(max(na.omit(PRESENT)) == -Inf ~ "CHYBÍ DATA",
                                       max(na.omit(PRESENT)) >= 1 ~ "ANO",
                                       max(na.omit(PRESENT)) == 0 ~ "NE"),
                  POCETNOST = case_when(max(na.omit(POCETNOST)) == -Inf ~ "CHYBÍ DATA",
                                        max(na.omit(POCETNOST)) == 1 ~ "DOSTATEČNÁ"),
                  VITALITA = NA,
                  HABITAT = NA,
                  PRUHLEDNOST = NA,
                  ZOOPLANKTON = NA,
                  LIKVIDACE = NA,
                  MANAGEMENT = NA,
                  NEGATIV = NA,
                  OVERALL = NA,
                  COLOUR = case_when(max(na.omit(PRESENT)) == 1 ~ "green",
                                     max(na.omit(PRESENT)) == -Inf ~ "grey",
                                     max(na.omit(PRESENT)) == 0 ~ "red"))
      hab_evl <- hab_evl[hab_evl$SITECODE %in% species_site$SITECODE,]
      hab_evl <- hab_evl[!duplicated(hab_evl$SITECODE),]
      return(hab_evl)
    }
    
    # Fish ----
    
    # Ssawci ----
    Carniv.clear <- function(species) {
      species <- species %>%
        mutate(
          DRUH = as.factor(DRUH),
          DATE = as.Date(as.character(DATUM_OD), format = '%d.%m.%Y'),
          YEAR = substring(DATE, 1, 4),
          SITECODE = substr(EVL, 1, 9),
          NAZEV = substr(as.character(EVL), 12, nchar(as.character(EVL))),
          PRESENT = case_when(grepl("DNA", POZNAMKA) == TRUE |
                                grepl("C1", STRUKT_POZN) == TRUE |
                                grepl("C2", STRUKT_POZN) == TRUE ~ 1)) 
      species <- species %>%
        filter(YEAR >= (current_year - 1))
      return(species)
    }
    
    Carniv.site <- function(species) {
      species_site <- as.data.frame(cbind(as.vector(find.evl.SITECODE(input_species)), 
                                          as.vector(find.evl.NAZEV(input_species))))
      colnames(species_site) <- c("SITECODE", "NAZEV")
      hab_evl <- Carniv.clear(species) %>%
        bind_rows(species_site) %>%
        group_by(SITECODE) %>%
        summarise(SITECODE = SITECODE,
                  NAZEV = NAZEV,
                  PRESENCE = case_when(max(na.omit(PRESENT)) == -Inf ~ "CHYBÍ DATA",
                                       max(na.omit(PRESENT)) >= 1 ~ "ANO",
                                       max(na.omit(PRESENT)) == 0 ~ "NE"),
                  HABITAT = NA,
                  LIKVIDACE = NA,
                  MANAGEMENT = NA,
                  NEGATIV = NA,
                  OVERALL = NA,
                  COLOUR = case_when(max(na.omit(PRESENT)) == 1 ~ "green",
                                     max(na.omit(PRESENT)) == -Inf ~ "grey",
                                     max(na.omit(PRESENT)) == 0 ~ "red"))
      hab_evl <- hab_evl[hab_evl$SITECODE %in% species_site$SITECODE,]
      hab_evl <- hab_evl[!duplicated(hab_evl$SITECODE),]
      return(hab_evl)
    }
    # RESULT MAP ----
    # Motýle
    if (input$species == "Euplagia quadripunctaria") {
      result_evl <- Lep.1.site(species)
    } 
    if (input$species == "Phengaris nausithous") {
      result_evl <- Lep.1.site(species)
    } 
    if (input$species == "Phengaris teleius") {
      result_evl <- Lep.1.site(species)
    } 
    if (input$species == "Colias myrmidone") {
      result_evl <- Lep.2.site(species)
    } 
    if (input$species == "Lycaena dispar") {
      result_evl <- Lep.2.site(species)
    } 
    if (input$species == "Euphydryas aurinia") {
      result_evl <- Lep.1.site(species)
    } 
    if (input$species == "Euphydryas maturna") {
      result_evl <- Lep.1.site(species)
    } 
    if (input$species == "Eriogaster catax") {
      result_evl <- Lep.3.site(species)
    } 
    # Brouke
    if (input$species == "Carabus hungaricus") {
      result_evl <- CarOpen.site(species)
    } 
    if (input$species == "Carabus menetriesi pacholei") {
      result_evl <- CarAqua.site(species)
    } 
    if (input$species == "Carabus variolosus") {
      result_evl <- CarAqua.site(species)
    } 
    if (input$species == "Bolbelasmus unicornis") {
      result_evl <- SapOpen.site(species)
    } 
    if (input$species == "Lucanus cervus") {
      result_evl <- SapOpen.site(species)
    } 
    if (input$species == "Cerambyx cerdo") {
      result_evl <- SapOpen.site(species)
    } 
    if (input$species == "Osmoderma barnabita") {
      result_evl <- SapOpen.site(species)
    }
    if (input$species == "Cucujus cinnaberinus") {
      result_evl <- SapFor.site(species)
    }
    if (input$species == "Rhysodes sulcatus") {
      result_evl <- SapFor.site(species)
    }
    if (input$species == "Limoniscus violaceus") {
      result_evl <- SapFor.site(species)
    }
    if (input$species == "Rosalia alpina") {
      result_evl <- SapFor.site(species)
    }
    if (input$species == "Graphoderus bilineatus") {
      result_evl <- ColAqua.site(species)
    }
    #Ryby
    if (input$species == "Cottus gobio") {
      result_evl <- icht_site(icht_clear(species))
    }
    if (input$species == "Bombina bombina" |
        input$species == "Bombina variegata" |
        input$species == "Triturus cristatus" |
        input$species == "Triturus carnifex" |
        input$species == "Triturus dobrogicus" |
        input$species == "Lissotriton montandoni") {
      result_evl <- ampsite(amp_clear(species))
    }
    if (input$species == "Rhinolophus hipposideros" |
        input$species == "Barbastella barbastellus" |
        input$species == "Myotis bechsteini" |
        input$species == "Myotis blythii" |
        input$species == "Myotis dasycneme" |
        input$species == "Myotis emarginatus" |
        input$species == "Myotis myotis") {
      result_evl <- chirsite(clear_chir(species))
    }
    if (input$species == "Canis lupus") {
      result_evl <- Carniv.site(species)
    }
    if (input$species == "Ursus arctos") {
      result_evl <- Carniv.site(species)
    }
    if (input$species == "Lynx lynx") {
      result_evl <- Carniv.site(species)
    }
    
    target_evl <- evl[evl$SITECODE %in% find.evl.SITECODE(input$species),]
    target_evl <- merge(target_evl, result_evl, by = "SITECODE")
    
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldTopo) %>%
      addPolygons(data = czechia,
                  color = "black",
                  fill = NA,
                  weight = 1,
                  opacity = 1) %>%
      addPolygons(data = bioregs,
                  color = "black",
                  fill = NA,
                  weight = 1,
                  opacity = .5) %>%
      addPolygons(data = target_evl,
                  color = ~COLOUR,
                  fill = ~COLOUR,
                  weight = 3,
                  opacity = .97,
                  label = target_evl$NAZEV.y)
  })
  
  # Mapa nálezů ----
  output$mapa_nalezu <- renderPlot({
    req(input$file1)
    
    species <- read.csv(input$file1$datapath,
                        sep = input$sep)
    
    species <- filter(species, DRUH == input$species)
    
    mapicka <- function(species){
      species <- species[!(species$X == "<i>Skrytá lokalizace</i>"),]
      species <- species[!(species$Y == "<i>Skrytá lokalizace</i>"),]
      d <- data.frame(lon=as.numeric(sub(",", ".", species$X, fixed = TRUE)), 
                      lat=as.numeric(sub(",", ".", species$Y, fixed = TRUE)))
      coordinates(d) <- ~lon + lat
      proj4string(d) <- CRS("+init=epsg:5514")
      sp.d <- as.data.frame(spTransform(d, CRS("+init=epsg:4326")))
      names(sp.d) <- c("decimallongtitude", "decimallatitude")
      species <- cbind(species, sp.d)
      species$DATUM <- as.Date(as.character(species$DATUM_OD), format = '%Y%m%d')
      species <- (subset(species, species$DATUM > "2014-01-01"))
      return(species)
    }
    
    mapka <- function(species, mapa){
      ggplot() +
        geom_polygon(data = mapa, 
                     aes(x = long, y = lat, group = group),
                     fill = NA, colour = "black") + 
        geom_polygon(data = bioregs, 
                     aes(x = long, y = lat, group = group),
                     fill = NA, colour = "black", alpha = 0.01) +
        geom_point(data = species, aes(x = decimallongtitude, y = decimallatitude, colour = DRUH), 
                   alpha = 0.3) +
        guides(colour = guide_legend(override.aes = list(alpha = 1))) +
        coord_fixed() +
        coord_quickmap() +  
        theme_void() +
        theme(legend.position = "none")
    }
    
    species_points <- function(species){
      species_points <- data.frame(mapicka(species)$decimallongtitude, mapicka(species)$decimallatitude)
      names(species_points) <- c("long", "lat")
      coordinates(species_points) = ~long +lat
      proj4string(species_points)=proj4string(czechia)
      return(species_points)
    }
    
    species_evl <- function(species, species_points, species_sites){
      species_evl_sites <- subset(evl, evl$SITECODE %in% species_sites)
      species_evl_result <- species[!(is.na(over(species_points(mapicka(species)), 
                                                 species_evl_sites)$SITECODE)),]
      return(species_evl_result)
    }
    
    species_sites <-  function(species){
      return(sites_subjects$SITECODE[which(grepl(input$species, sites_subjects$SPECIES_NAME))])
    }
    
    result <- mapka(mapicka(species_evl(species, species_points(species), species_sites(species))), czechia)
    
    result
  })
  
  # Výběr taxonu ----
  output$group_selector = renderUI({
    selectInput(inputId = "Group",
                label = "Vyberte skupinu",
                choices = as.character(unique(taxa$Group)))
  })
  
  output$species_selector = renderUI({
    data_available = taxa[taxa$Group == input$Group, "Species"]
    selectInput(
      inputId = "species",
      label = "Vyberte druh",
      choices = unique(data_available),
      selected = unique(data_available)[1]
    )
  })
  
  output$evl_selector = renderUI({
    find.evl.SITECODE <- function(species) {
      return(subset(sites_subjects, sites_subjects$Název.latinsky == species)$Kód.lokality )
    }
    find.evl.NAZEV <- function(species) {
      return(subset(sites_subjects, sites_subjects$Název.latinsky == species)$Název.lokality)
    }
    find.evl.NUMBER <- function(species) {
      return(nrow(subset(sites_subjects, sites_subjects$Název.latinsky == species)))
    }
    evl_available = find.evl.NAZEV(input$species)
    selectInput(
      inputId = "evl_site",
      label = "Vyberte EVL",
      choices = c("", as.character(evl_available)),
      selected = NULL
    )
  })
  
  
  # Metodika ----
  output$metodika <- renderUI({
    
    req(input$species)
    
    find.czechname <- function(species) {
      return(taxa[grepl(species, taxa$Species),]$Species_CZ)
    }
    find.latinname <- function(species) {
      return(taxa[grepl(species, taxa$Species_CZ),]$Species)
    }
    
    str_head <-
      paste(h4("Metodika hodnocení stavu druhu", (find.czechname(input$species))))
    # Motýle
    str_phenau <- paste(text_phenau, text_Lep_1)
    str_phetel <- paste(text_phetel, text_Lep_1)
    str_eupqua <- paste(text_eupqua, text_Lep_1)
    str_colmyr <- paste(text_colmyr, text_Lep_2)
    str_eupaur <- paste(text_eupaur, text_Lep_1)
    str_eupmat <- paste(text_eupmat, text_Lep_1)
    str_lycdis <- paste(text_lycdis, text_Lep_1)
    # Brouke
    str_carmen <- paste(text_carmen)
    str_carvar <- paste(text_carvar)
    str_luccer <- paste(text_luccer, text_SapOpen)
    str_cercer <- paste(text_cercer, text_SapOpen)
    str_osmbar <- paste(text_SapOpen)
    str_limvio <- paste(text_limvio, text_SapFor)
    str_grabil <- paste(text_grabil)
    str_cuccin <- paste(text_cuccin, text_SapFor)
    # Korejši
    str_austor <- paste(p("Morová rána"))
    # Štírci
    str_antste <- paste(p("Prcek"))
    # Ryby
    str_cotgob <- paste(p("Jsem pod kamenem"))
    # Obojživelníci
    str_bombom <- paste(p("Neolizovat kuňky?"))
    str_bomvar <- paste(p("Olizovat kuňky?"))
    str_tricri <- paste(p("Kde jsi?"))
    str_tricar <- paste(p("Neztratil ses?"))
    str_tridob <- paste(p("Dunaj, Dunaj, Dunaj, Dunaj, aj to širé móře!"))
    str_lismon <- paste(p("Proč se nerozmnožuješ?"))
    # Ssawci
    str_canlup <- paste(text_canlup, text_Carniv)
    str_lynlyn <- paste(text_lynlyn, text_Carniv)
    
    if (input$species == "Phengaris nausithous") {
      result <- HTML(paste(str_head, str_phenau))
    }
    if (input$species == "Phengaris teleius") {
      result <- HTML(paste(str_head, str_phetel))
    }
    if (input$species == "Colias myrmidone") {
      result <- HTML(paste(str_head, str_colmyr))
    }
    if (input$species == "Lycaena dispar") {
      result <- HTML(paste(str_head, str_lycdis))
    }
    if (input$species == "Euplagia quadripunctaria") {
      result <- HTML(paste(str_head, str_eupqua))
    }
    if (input$species == "Euphydryas aurinia") {
      result <- HTML(paste(str_head, str_eupaur))
    }
    if (input$species == "Euphydryas maturna") {
      result <- HTML(paste(str_head, str_eupmat))
    }
    if (input$species == "Carabus menetriesi pacholei") {
      result <- HTML(paste(str_head, str_carmen))
    }
    if (input$species == "Carabus variolosus") {
      result <- HTML(paste(str_head, str_carvar))
    }
    if (input$species == "Lucanus cervus") {
      result <- HTML(paste(str_head, str_luccer))
    }
    if (input$species == "Cerambyx cerdo") {
      result <- HTML(paste(str_head, str_cercer))
    }
    if (input$species == "Osmoderma barnabita") {
      result <- HTML(paste(str_head, str_osmbar))
    }
    if (input$species == "Limoniscus violaceus") {
      result <- HTML(paste(str_head, str_limvio))
    }
    if (input$species == "Graphoderus bilineatus") {
      result <- HTML(paste(str_head, str_grabil))
    }
    if (input$species == "Cucujus cinnaberinus") {
      result <- HTML(paste(str_head, str_cuccin))
    }
    if (input$species == "Austropotamobius torrentium") {
      result <- HTML(paste(str_head, str_austor))
    }
    if (input$species == "Anthrenochernes stellae") {
      result <- HTML(paste(str_head, str_antste))
    }
    if (input$species == "Cottus gobio") {
      result <- HTML(paste(str_head, str_cotgob))
    }
    if (input$species == "Bombina bombina") {
      result <- HTML(paste(str_head, str_bombom))
    }
    if (input$species == "Bombina variegata") {
      result <- HTML(paste(str_head, str_bomvar))
    }
    if (input$species == "Triturus cristatus") {
      result <- HTML(paste(str_head, str_tricri))
    }
    if (input$species == "Triturus carnifex") {
      result <- HTML(paste(str_head, str_tricar))
    }
    if (input$species == "Triturus dobrogicus") {
      result <- HTML(paste(str_head, str_tridob))
    }
    if (input$species == "Lissotriton montandoni") {
      result <- HTML(paste(str_head, str_lismon))
    }
    if (input$species == "Canis lupus") {
      result <- HTML(paste(str_head, str_canlup))
    }
    if (input$species == "Lynx lynx") {
      result <- HTML(paste(str_head, str_lynlyn))
    }
    result
    
  })
  
  # Lokality ----
  output$lokality <- renderTable({
    
    req(input$species)
    
    cbind("Název EVL" = find.evl.NAZEV(input$species), 
          "SITECODE" = find.evl.SITECODE(input$species))
    
  })
  
  # Default UI ----
  output$default <- renderUI({
    
    if (input$species == "") {
      
      div(img(src = "https://raw.githubusercontent.com/jonasgaigr/N2K.CZ/main/WWW/logo.png",
              width = "30%"), style = "text-align: center;")
      
    } else {
      NULL
    }
    
  })
  
}

shinyApp(ui = ui, server = server)
