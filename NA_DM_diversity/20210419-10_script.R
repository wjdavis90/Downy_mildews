#set working directory
setwd("C://Users//wjdavis//Documents//R_working_dir//NA_DM//Specimens//20210419//run2")

#set output resolution
ppi <- 1000

#Load libraries
library(tidyverse)
library(janitor)
library(lubridate)
library(collapse)
library(RColorBrewer)
library(patchwork)
library(vegan)
library(hrbrthemes)
library(tm)
library(proustr)
library(VennDiagram)
library(maps)
library(ggspatial)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

#Load, clean, transform data--cultivated = yes/no ; native = yes/no

#Load data & reduce to relevant columns
Downy_mildews_tidy <- read.csv(file="DM_Americas_210416-1.csv", header =TRUE) %>%
clean_names() %>%
subset(, select = c(institution_code,
basis_of_record,
catalog_number,
cur_genus,
cur_epithet,
cur_scientific_name,
recorded_by,
record_number,
year,
month,
day,
cur_host,
host_family,
cultivated,
native_status,
continent,
country,
state_province,
decimal_latitude,
decimal_longitude,
geodetic_datum,
coordinate_uncertainty_in_meters,
climatic_zone)) %>%

#Code in cultivated status
#Determined by Wikipedia and/or Google search

mutate(cultivated = case_when(cur_host %in% c("Allium cepa",
"Vicia hirsuta",
"Allamanda cathartica",
"Agastache mexicana",
"Agastache nepetoides",
"Agastache scrophulariifolia",
"Ageratina altissima",
"Agrimonia eupatoria",
"Agropyron cristatum",
"Allium nigrum",
"Allium sativum",
"Allium schoenoprasum",
"Ampelopsis glandulosa",
"Anemone coronaria",
"Anemone hepatica",
"Anemone hepatica var. acuta",
"Antirrhinum majus",
"Arctotis fastuosa",
"Argyranthemum frutescens",
"Armoracia rusticana",
"Artemisia absinthium",
"Artemisia arborescens",
"Artemisia argyrophylla var. brevis",
"Artemisia douglasiana",
"Atropa belladonna",
"Aurinia saxatilis",
"Avena byzantina",
"Avena sativa",
"Barbarea verna",
"Beta vulgaris",
"Brassica juncea",
"Brassica napus",
"Brassica nigra",
"Brassica oleracea",
"Brassica oleracea var. capitata",
"Brassica rapa",
"Callistephus chinensis",
"Camelina sativa",
"Capsicum annuum",
"Cercis canadensis",
"Chenopodium album",
"Chenopodium quinoa",
"Chenopodium berlandieri",
"Chenopodium capitatum",
"Cicer arietinum",
"Clarkia amoena",
"Colocasia antiquorum",
"Coreopsis grandiflora",
"Cucumis anguria",
"Cucumis melo",
"Cucumis sativus",
"Cucurbita maxima",
"Cucurbita moschata",
"Cucurbita pepo",
"Cucurbita pepo subsp ovifera",
"Dactylis glomerata",
"Dianthus caryophyllus",
"Digitalis purpurea",
"Echinocystis lobata",
"Eupatorium purpureum",
"Helianthus annuus",
"Erysimum ◊ cheiri",
"Fagopyrum esculentum",
"Festuca arundinacea",
"Geum chiloense",
"Glycine max",
"Helichrysum bracteatum",
"Hordeum vulgare",
"Humulus lupulus",
"Humulus lupulus var. lupuloides",
"Impatiens arguta",
"Impatiens balsamina",
"Impatiens flanaganae",
"Impatiens hochstetteri",
"Impatiens walleriana",
"Lactuca sativa",
"Lathyrus odoratus",
"Lilium longiflorum",
"Lolium perenne",
"Luffa acutangula",
"Lupinus hartwegii",
"Lycopersicon esculentum",
"Hydrophyllum virginianum",
"Lagenaria siceraria",
"Lupinus bicolor",
"Lupinus densiflorus",
"Lupinus perennis",
"Lupinus polyphyllus",
"Monarda didyma",
"Oenothera biennis",
"Oenothera pallida",
"Oenothera speciosa",
"Papaver nudicaule",
"Matthiola incana",
"Medicago lupulina",
"Medicago sativa",
"Melia azedarach",
"Mesembryanthemum cordifolium",
"Momordica charantia",
"Monoculus hyoseroides",
"Moricandia arvensis",
"Mucuna pruriens var. utilis",
"Nasturtium officinale",
"Nicotiana stocktonii",
"Nicotiana sylvestris",
"Nicotiana tabacum",
"Nicotiana tomentosa",
"Ocimum basilicum",
"Papaver somniferum",
"Penstemon angustifolius",
"Phlox divaricata",
"Phlox paniculata",
"Physostegia virginiana",
"Plantago ovata",
"Plantago princeps var. princeps",
"Parthenocissus tricuspidata",
"Pastinaca sativa",
"Pennisetum glaucum",
"Pilosella aurantiaca",
"Pisum sativum",
"Plantago major",
"Plectranthus scutellarioides",
"Potentilla nepalensis",
"Vitis vulpina",
"Zea mays",
"Vitis aestivalis",
"Vitis aestivalis var. bourquiniana",
"Vitis cordifolia",
"Vitis labrusca",
"Ratibida pinnata",
"Rubus idaeus subsp strigosus",
"Rudbeckia fulgida",
"Rudbeckia hirta",
"Rudbeckia laciniata",
"Rudbeckia sp",
"Rudbeckia triloba",
"Scrophularia marylandica",
"Scrophularia lanceolata",
"Symphyotrichum lateriflorum",
"Symphyotrichum novae-angliae",
"Vitis vinifera",
"Rosa sp",
"Setaria italica",
"Sorghum bicolor",
"Sorghum halepense",
"Vitis 'Lindley' labrusca X vinefera",
"Tarenaya hassleriana",
"Raphanus raphanistrum",
"Raphanus raphanistrum subsp sativus",
"Rheum rhabarbarum",
"Ribes rubrum",
"Ribes uva-crispa",
"Rubus corchorifolius",
"Rubus villosus var. humifusus",
"Scabiosa atropurpurea",
"Scabiosa columbaria",
"Scrophularia nodosa",
"Secale cereale",
"Solanum tuberosum",
"Spilanthes ocymifolia",
"Spinacia oleracea",
"Vaccinium sp",
"Viola bicolor",
"Viola tricolor",
"Viola tricolor f. tenella",
"Vitis bicolor",
"Vitis rotundifolia",
"Vitis rufotomentosa",
"Vitis simpsonii",
"Trichosanthes colubrina",
"Trifolium incarnatum",
"Trifolium repens",
"Trigonella foenum-graecum",
"Triosteum perfoliatum",
"Triticum aestivum",
"Viburnum opulus",
"Viburnum sargentii",
"Viburnum tinus",
"Vicia faba",
"Vicia sativa",
"Vicia sativa subsp nigra",
"Vicia villosa",
"Vigna unguiculata",
"Vitis coignetiae",
"Solidago sphacelata") ~ "cultivated",
cur_host %in% c("Ambrosia artemisiifolia",
"Acalypha virginica",
"Acanthospermum australe",
"Acanthospermum humile",
"Achyronychia cooperi",
"Acmella oppositifolia",
"Acmispon americanus",
"Aconitum columbianum",
"Aconitum delphinifolium",
"Ageratina aromatica",
"Agoseris aurantiaca",
"Agoseris glauca",
"Agoseris grandiflora",
"Agoseris heterophylla",
"Agoseris parviflora",
"Agrimonia gryposepala",
"Agrimonia pubescens",
"Agrimonia rostellata",
"Agrimonia striata",
"Ambrosia psilostachya",
"Ambrosia tomentosa",
"Ambrosia trifida",
"Ampelamus laevis",
"Amsinckia douglasiana",
"Amsinckia lycopsoides",
"Amsinckia menziesii",
"Amsinckia menziesii var. intermedia",
"Androsace occidentalis",
"Anemone alpina",
"Anemone canadensis",
"Anemone caroliniana",
"Anemone cylindrica",
"Anemone deltoidea",
"Anemone drummondii",
"Anemone multifida",
"Anemone nemorosa",
"Anemone occidentalis",
"Anemone piperi",
"Anemone quinquefolia",
"Anemone virginiana",
"Angelica arguta",
"Apium annuum",
"Arabidopsis lyrata",
"Arabidopsis thaliana",
"Arabis hirsuta",
"Arabis hirsuta var. pycnocarpa",
"Argemone ochroleuca",
"Argemone polyanthemos",
"Artemisia biennis",
"Artemisia ludoviciana",
"Artemisia serrata",
"Symphyotrichum lanceolatum",
"Aster roscidus",
"Astragalus alpinus",
"Astragalus beckwithii var. purpureus",
"Astragalus canadensis",
"Astragalus cibarius",
"Astragalus gambelianus",
"Astragalus nuttallianus",
"Astragalus pectinatus",
"Astragalus pectinatus",
"Astragalus spaldingii",
"Atriplex argentea",
"Atriplex lentiformis",
"Atriplex patula",
"Atriplex prostrata subsp calotheca",
"Barbarea vulgaris",
"Bidens alba var. radiata",
"Bidens aristosa",
"Bidens bidentoides",
"Bidens bipinnata",
"Bidens cernua",
"Bidens comosa",
"Bidens connata",
"Bidens cynapiifolia",
"Bidens frondosa",
"Bidens discoidea",
"Bidens laevis",
"Bidens pilosa",
"Bidens trichosperma",
"Bidens vulgata",
"Boechera canadensis",
"Boechera dentata",
"Boechera holboellii",
"Boechera holboellii var. secunda",
"Boechera laevigata",
"Boechera sparsiflora",
"Boechera stricta",
"Brachiaria fasciculata",
"Brassica cretica",
"Brickellia diffusa",
"Bromus inermis",
"Buglossoides arvensis",
"Cakile edentula",
"Calandrinia breweri",
"Calandrinia ciliata",
"Camissonia dentata subsp littoralis",
"Capnoides sempervirens",
"Capsella bursa-pastoris",
"Cardamine angustata",
"Cardamine bulbosa",
"Cardamine concatenata",
"Cardamine cordifolia",
"Cardamine digitata",
"Cardamine diphylla",
"Cardamine glacialis",
"Cardamine heptaphylla",
"Cardamine hirsuta",
"Cardamine nuttallii",
"Cardamine occidentalis",
"Cardamine oligosperma",
"Cardamine parviflora",
"Cardamine parviflora var. arenicola",
"Cardamine pensylvanica",
"Castilleja tenuis",
"Cayaponia americana",
"Cayaponia laciniosa",
"Cayaponia racemosa",
"Cerastium crassipes",
"Celtis laevigata",
"Celtis occidentalis",
"Cerastium arvense",
"Cerastium brachypodum",
"Cerastium fontanum subsp vulgare",
"Cerastium glomeratum",
"Cerastium nutans",
"Cerastium oreophilum",
"Cerastium semidecandrum",
"Chenopodium desiccatum var. leptophylloides",
"Chenopodium incanum",
"Chenopodium fremontii",
"Chenopodium leptophyllum",
"Chenopodium simplex",
"Chenopodium hybridum",
"Chenopodium macrospermum",
"Chenopodium macrospermum subsp salsum",
"Chenopodium murale",
"Chloris virgata",
"Chorizanthe diffusa",
"Chrysosplenium iowense",
"Cichorium intybus",
"Cicuta maculata",
"Cistanthe umbellata",
"Citrullus lanatus",
"Clarkia dudleyana",
"Clarkia purpurea subsp quadrivulnera",
"Clarkia rhomboidea",
"Claytonia lanceolata",
"Claytonia linearis",
"Claytonia parviflora",
"Claytonia perfoliata",
"Claytonia virginica",
"Clibadium surinamense",
"Coccinia grandis",
"Cochlearia officinalis",
"Collinsia parviflora",
"Collinsia sparsiflora",
"Collomia grandiflora",
"Collomia heterophylla",
"Collomia linearis",
"Conioselinum chinense",
"Conioselinum pacificum",
"Convolvulus arvensis",
"Corydalis aurea",
"Corydalis aurea subsp occidentalis",
"Corydalis flavula",
"Corydalis micrantha",
"Corydalis solida",
"Crepis acuminata",
"Cryptantha affinis",
"Cryptantha ambigua",
"Cryptantha barbigera",
"Cryptantha fendleri",
"Cryptantha simulans",
"Cryptantha torreyana",
"Cryptantha watsonii",
"Cyanus segetum",
"Cyclospermum leptophyllum",
"Cyclachaena xanthiifolia",
"Cynodon dactylon",
"Delphinium depauperatum",
"Descurainia californica",
"Descurainia depressa",
"Descurainia incana",
"Descurainia incana subsp incisa",
"Descurainia incana subsp procera",
"Descurainia incisa subsp incisa",
"Descurainia longepedicellata",
"Descurainia pinnata",
"Descurainia pinnata subsp brachycarpa",
"Descurainia pinnata subsp intermedia",
"Descurainia pinnata subsp ochroleuca",
"Descurainia sophia",
"Dicentra canadensis",
"Dicentra cucullaria",
"Dicentra formosa",
"Dipsacus fullonum",
"Draba nemorosa",
"Draba cuneifolia",
"Draba reptans",
"Draba stenoloba",
"Dracocephalum parviflorum",
"Duchesnea indica",
"Dysphania ambrosioides",
"Echinospermum virginicum",
"Elephantopus carolinianus",
"Eleusine indica",
"Ellisia nyctelea",
"Elymus canadensis",
"Elymus junceus",
"Elymus repens",
"Epilobium coloratum",
"Epilobium latifolium",
"Epilobium minutum",
"Eragrostis pectinacea",
"Erechtites hieracifolia",
"Eremothera chamaenerioides",
"Eremothera minor",
"Erigeron annuus",
"Erigeron bonariensis",
"Erigeron canadensis",
"Erigeron divergens",
"Erigeron philadelphicus",
"Eriogonum deflexum",
"Eriogonum thurberi",
"Erysimum asperum",
"Erysimum capitatum",
"Eucrypta micrantha",
"Euphorbia cordifolia",
"Euphorbia glyptosperma",
"Euphorbia humistrata",
"Euphorbia hypericifolia",
"Euphorbia maculata",
"Euphorbia nutans",
"Euphorbia prostrata",
"Euphorbia serpens",
"Euphorbia serpyllifolia",
"Euphorbia serpyllifolia subsp hirtula",
"Euphorbia spathulata",
"Euphorbia stictospora",
"Eutrema edwardsii",
"Fallopia scandens",
"Filago germanica",
"Floerkea proserpinacoides",
"Fragaria virginiana",
"Galinsoga parviflora",
"Galium aparine",
"Galium asprellum",
"Galium bifolium",
"Galium boreale",
"Galium concinnum",
"Galium tinctorium",
"Galium trifidum",
"Galium trifidum subsp brevipes",
"Galium triflorum",
"Gaura mollis",
"Geranium bicknellii",
"Geranium caroliniaum",
"Geranium maculatum",
"Geranium oreganum",
"Geranium richardsonii",
"Geranium robertianum",
"Geranium texanum",
"Geranium viscosissimum",
"Geranium vulcanicola",
"Geum aleppicum",
"Geum canadense",
"Geum macrophyllum",
"Geum rivale",
"Geum triflorum",
"Geum virginianum",
"Gnaphalium pensylvanicum",
"Gnaphalium purpureum",
"Gonolobus caroliniensis",
"Gonolobus suberosus",
"Grayia spinosa",
"Grossularia menziesii",
"Guillenia lasiophylla",
"Hackelia cinerea",
"Hackelia floribunda",
"Hackelia micrantha",
"Hackelia patens",
"Hasteola suaveolens",
"Hedeoma hispida",
"Hedeoma pulegioides",
"Hedysarum alpinum",
"Hedysarum mackenzii",
"Helianthus argophyllus",
"Helianthus decapetalus",
"Helianthus divaricatus",
"Helianthus doronicoides",
"Helianthus giganteus",
"Helianthus grosseserratus",
"Helianthus hirsutus",
"Helianthus laetiflorus",
"Helianthus maximiliani",
"Helianthus nuttallii",
"Helianthus occidentalis",
"Helianthus pauciflorus",
"Helianthus pauciflorus subsp subrhomboideus",
"Helianthus petiolaris",
"Helianthus scaberrimus",
"Helianthus subtuberosus",
"Helianthus tuberosus",
"Heliocarpus americanus",
"Erigeron scaposus",
"Erodium cicutarium",
"Erysimum cheiranthoides",
"Draba verna",
"Euphorbia indica",
"Euphrasia antarctica",
"Euphrasia officinalis",
"Fallopia convolvulus",
"Fallopia dumetorum",
"Galium mollugo",
"Galium spurium",
"Geranium pusillum",
"Geranium rotundifolium",
"Geranium sibiricum",
"Hesperis matronalis",
"Hordeum murinum",
"Houstonia caerulea",
"Houstonia pusilla",
"Hydrophyllum capitatum",
"Hydrophyllum fendleri",
"Hydrophyllum fendleri var. albifrons",
"Hydrophyllum macrophyllum",
"Hydrophyllum occidentale",
"Hydrophyllum tenuipes",
"Impatiens capensis",
"Impatiens pallida",
"Isopyrum hallii",
"Krigia biflora",
"Krigia cespitosa",
"Krigia dandelion",
"Krigia virginica",
"Lactuca biennis",
"Lactuca canadensis",
"Lactuca elongata",
"Lactuca floridana",
"Lactuca graminifolia",
"Lactuca hirsuta",
"Lactuca leucophaea",
"Lactuca ludoviciana",
"Laportea canadensis",
"Lappula occidentalis",
"Lappula occidentalis var. cupulata",
"Lappula occidentalis var. occidentalis",
"Lappula virginiana",
"Lathyrus japonicus subsp maritimus",
"Lathyrus nevadensis subsp lanceolatus",
"Lathyrus venosus",
"Lipochaeta integrifolia",
"Leavenworthia exigua",
"Lepidium austrinum",
"Lepidium dictyotum",
"Lepidium lasiocarpum",
"Lepidium latipes",
"Lepidium nitidum",
"Lepidium ramosissimum",
"Lepidium thurberi",
"Lepidium virginicum",
"Lepidium virginicum subsp menziesii",
"Lesquerella purpurea",
"Lewisia cotyledon",
"Ligusticum scoticum",
"Linaria canadensis",
"Linum sulcatum",
"Lithophragma bulbiferum",
"Lithophragma parviflorum",
"Lupinus affinis",
"Lupinus argenteus var. tenellus",
"Lupinus hirsutissimus",
"Lupinus lepidus",
"Lupinus minimus",
"Lupinus odoratus",
"Lupinus sericeus",
"Lupinus subvexus",
"Hypechusa tricolor",
"Hypochaeris chillensis",
"Iberis amara",
"Iberis sempervirens",
"Lactuca saligna",
"Lactuca serriola",
"Lactuca tatarica",
"Lamium amplexicaule",
"Lampsana sp",
"Lappula patula",
"Lepidium apetalum",
"Lepidium densiflorum",
"Lepidium didymum",
"Lobularia maritima",
"Madia gracilis",
"Matelea obliqua",
"Melampyrum lineare",
"Melampyrum lineare var. americanum",
"Melothria sp",
"Mentha arvensis",
"Mentha canadensis",
"Mertensia paniculata",
"Microsteris gracilis",
"Mimulus breweri",
"Mimulus guttatus",
"Mimulus pulsiferae",
"Mimulus tricolor",
"Mirabilis alipes",
"Mirabilis laevis",
"Mirabilis nyctaginea",
"Mirabilis nyctaginea var. angustifolia",
"Monolepis nuttalliana",
"Muhlenbergia mexicana",
"Myosotis alpestris",
"Myosotis laxa",
"Myosotis macrosperma",
"Myosotis verna",
"Nabalus altissimus",
"Navarretia intertexta",
"Nemophila breviflora",
"Nemophila kirtleyi",
"Nemophila microcalyx",
"Nemophila parviflora",
"Nemophila pedunculata",
"Nemophila pedunculata var. sepulta",
"Nemophila pulchella",
"Nicotiana attenuata",
"Nicotiana bigelovii",
"Nicotiana bigelovii var. quadricoloris",
"Nicotiana repanda",
"Nicotiana trigonophylla",
"Oenothera flava",
"Oenothera humifusa",
"Oenothera laciniata",
"Oenothera primiveris",
"Oenothera villosa subsp strigosa",
"Ottleya utahensis",
"Oxytropis deflexa",
"Oxytropis nigrescens",
"Melilotus officinalis",
"Melilotus officinalis subsp alba",
"Mitracarpus hirtus",
"Momordica balsamina",
"Mukia maderaspatana",
"Myosotis arvensis",
"Myosotis discolor",
"Myosotis stricta",
"Myosotis sylvatica",
"Nicotiana benthamiana",
"Nicotiana caesia",
"Nicotiana glauca",
"Nicotiana gossei",
"Nicotiana longiflora",
"Nicotiana stocktonii",
"Oenothera mollissima",
"Parietaria debilis",
"Parietaria pensylvanica",
"Parthenocissus quinquefolia",
"Parthenocissus quinquefolia f. hirsuta",
"Phacelia congesta",
"Phacelia hastata var. leucophylla",
"Phacelia heterophylla",
"Phacelia linearis",
"Phacelia mutabilis",
"Phacelia parryi",
"Physaria gordonii",
"Physostegia parviflora",
"Plagiobothrys cognatus",
"Plagiobothrys hispidulus",
"Plagiobothrys scopulorum",
"Plagiobothrys tenellus",
"Planodes virginicum",
"Plantago aristata",
"Plantago cordata",
"Plantago erecta",
"Plantago patagonica",
"Plantago pusilla",
"Plantago rhodosperma",
"Plantago rugelii",
"Plantago spinulosa",
"Plantago virginica",
"Polygonum douglasii",
"Polygonum polygaloides subsp confertiflorum",
"Polygonum ramosissimum",
"Polygonum aviculare",
"Potentilla canadensis",
"Potentilla gracilis",
"Potentilla gracilis var. fastigiata",
"Potentilla gracilis var. flabelliformis",
"Potentilla norvegica",
"Potentilla norvegica subsp hirsuta",
"Potentilla pulcherrima",
"Potentilla rivalis",
"Pseudostellaria jamesiana",
"Pterostegia drymarioides",
"Pyrrhopappus carolinianus",
"Parthenium hysterophorus",
"Pericallis cruenta",
"Phalaris aquatica",
"Piptothrix areolaris",
"Polypsecadium magellanicum",
"Vitis riparia",
"Setaria magna",
"Sicyos angulatus",
"Vicia gigantea",
"Rorippa palustris",
"Ranunculus abortivus",
"Ranunculus acriformis",
"Ranunculus fascicularis",
"Ranunculus hispidus",
"Ranunculus pensylvanicus",
"Ranunculus recurvatus",
"Ranunculus septentrionalis",
"Ranunculus uncinatus",
"Ribes cynosbati",
"Ribes divaricatum",
"Ribes glandulosum",
"Ribes hirtellum",
"Ribes menziesii",
"Ribes oxyacanthoides",
"Ribes triste",
"Rorippa curvisiliqua",
"Rorippa islandica",
"Rubus allegheniensis",
"Rorippa teres",
"Rubus argutus",
"Rubus canadensis",
"Rubus flagellaris",
"Rubus hispidus",
"Rubus leucodermis",
"Rubus spectabilis",
"Rubus ursinus subsp macropetalus",
"Sairocarpus nuttallianus",
"Salvia carduacea",
"Salvia reflexa",
"Scrophularia leporella",
"Silene acaulis subsp subacaulescens",
"Silene antirrhina",
"Silphium integrifolium",
"Silphium laciniatum",
"Silphium perfoliatum",
"Silphium terebinthinaceum",
"Silphium trifoliatum",
"Smilax sp",
"Solidago gigantea",
"Solidago rigida",
"Solidago sempervirens",
"Solidago serotina",
"Spermacoce eryngioides",
"Sporobolus cryptandrus",
"Sporobolus neglectus",
"Stanleya pinnata",
"Stellaria longipes",
"Stellaria pubera",
"Streptanthella longirostris",
"Streptanthus carinatus subsp arizonicus",
"Symphyotrichum ascendens",
"Symphyotrichum chilense",
"Symphyotrichum ciliolatum",
"Symphyotrichum cordifolium",
"Symphyotrichum laeve",
"Symphyotrichum oblongifolium",
"Symphyotrichum praealtum",
"Symphyotrichum puniceum",
"Symphyotrichum spathulatum",
"Symphyotrichum tradescantii",
"Symphyotrichum undulatum",
"Setaria viridis",
"Ranunculus acris",
"Ranunculus bulbosus",
"Ranunculus minutiflorus",
"Ranunculus peduncularis",
"Ranunculus repens",
"Rubus parvifolius",
"Rubus laciniatus",
"Rumex acetosa",
"Rumex acetosella",
"Rumex crispus",
"Sibara pinnata",
"Silene gallica",
"Silene latifolia",
"Sinapis arvensis var. orientalis",
"Sisymbrium altissimum",
"Sisymbrium irio",
"Sisymbrium officinale",
"Sonchus arvensis",
"Sonchus asper",
"Sonchus gangliformis",
"Sonchus oleraceus",
"Spergula arvensis",
"Spergularia rubra",
"Stellaria media",
"Teucrium canadense",
"Thalictrum pubescens",
"Thysanocarpus curvipes",
"Trautvetteria caroliniensis",
"Triumfetta lappula",
"Turritis glabra",
"Urtica dioica",
"Urtica dioica subsp gracilis",
"Valeriana occidentalis",
"Valeriana sitchensis",
"Valerianella radiata",
"Verbena hastata",
"Verbesina encelioides",
"Verbesina encelioides var. exauriculata",
"Vernonia baldwinii",
"Vernonia noveboracensis",
"Veronica alpina",
"Veronica americana",
"Veronica anagallis-aquatica",
"Veronica peregrina",
"Veronica serpyllifolia",
"Veronica serpyllifolia subsp humifusa",
"Viburnum acerifolium",
"Viburnum cassinoides",
"Viburnum dentatum",
"Viburnum nudum",
"Viburnum pubescens",
"Viburnum rufotomentosum",
"Viburnum trilobum",
"Vicia americana",
"Vicia linearis",
"Vicia ludoviciana",
"Vicia sparsifolia",
"Vitis californica",
"Vitis caribaea",
"Vitis baileyana",
"Vitis cinerea",
"Vitis cinerea var. floridana",
"Vitis rupestris",
"Vitis tiliifolia",
"Vitis x doaniana",
"Whipplea modesta",
"Zinnia peruviana",
"Trifolium dubium",
"Veronica arvensis",
"Vicia cracca",
"Vicia sepium",
"Viola arvensis",
"Vitis davidii",
"Vitis piasezkii",
"Apogon humilis",
"Agrostemma githago",
"Acacia sp",
"Anemone dichotoma") ~ "wild")) %>%

#Code in native status
#Determined using http://floranorthamerica.org/ and https://plants.sc.egov.usda.gov/java/

mutate(native_status = case_when(cur_host %in% c("Acalypha virginica",
"Achyronychia cooperi",
"Acmella oppositifolia",
"Acmispon americanus",
"Acmispon americanus",
"Aconitum columbianum",
"Aconitum delphinifolium",
"Agastache mexicana",
"Agastache nepetoides",
"Agastache scrophulariifolia",
"Ageratina altissima",
"Ageratina aromatica",
"Agoseris aurantiaca",
"Agoseris glauca",
"Agoseris grandiflora",
"Agoseris heterophylla",
"Agoseris parviflora",
"Agrimonia gryposepala",
"Agrimonia pubescens",
"Agrimonia rostellata",
"Agrimonia striata",
"Allium schoenoprasum",
"Ambrosia artemisiifolia",
"Ambrosia psilostachya",
"Ambrosia tomentosa",
"Ambrosia trifida",
"Ampelamus laevis",
"Amsinckia douglasiana",
"Amsinckia lycopsoides",
"Amsinckia menziesii",
"Amsinckia menziesii var. intermedia",
"Androsace occidentalis",
"Anemone alpina",
"Anemone canadensis",
"Anemone caroliniana",
"Anemone cylindrica",
"Anemone deltoidea",
"Anemone drummondii",
"Anemone hepatica",
"Anemone hepatica var. acuta",
"Anemone multifida",
"Anemone occidentalis",
"Anemone piperi",
"Anemone quinquefolia",
"Anemone virginiana",
"Angelica arguta",
"Arabidopsis lyrata",
"Arabis hirsuta",
"Arabis hirsuta var. pycnocarpa",
"Argemone ochroleuca",
"Argemone polyanthemos",
"Artemisia biennis",
"Artemisia douglasiana",
"Artemisia ludoviciana",
"Artemisia serrata",
"Symphyotrichum lanceolatum",
"Aster roscidus",
"Astragalus alpinus",
"Astragalus beckwithii var. purpureus",
"Astragalus canadensis",
"Astragalus cibarius",
"Astragalus gambelianus",
"Astragalus nuttallianus",
"Astragalus pectinatus",
"Astragalus spaldingii",
"Atriplex argentea",
"Atriplex lentiformis",
"Bidens aristosa",
"Bidens bidentoides",
"Bidens bipinnata",
"Bidens cernua",
"Bidens comosa",
"Bidens connata",
"Bidens frondosa",
"Bidens discoidea",
"Bidens laevis",
"Bidens trichosperma",
"Bidens vulgata",
"Boechera canadensis",
"Boechera dentata",
"Boechera laevigata",
"Boechera sparsiflora",
"Boechera stricta",
"Brachiaria fasciculata",
"Brickellia diffusa",
"Cakile edentula",
"Calandrinia breweri",
"Calandrinia ciliata",
"Camissonia dentata subsp littoralis",
"Capnoides sempervirens",
"Capsicum annuum",
"Cardamine angustata",
"Cardamine bulbosa",
"Cardamine concatenata",
"Cardamine cordifolia",
"Cardamine digitata",
"Cardamine diphylla",
"Cardamine nuttallii",
"Cardamine occidentalis",
"Cardamine oligosperma",
"Cardamine parviflora",
"Cardamine parviflora var. arenicola",
"Cardamine pensylvanica",
"Castilleja tenuis",
"Cayaponia americana",
"Celtis laevigata",
"Celtis occidentalis",
"Cerastium arvense",
"Cerastium brachypodum",
"Cerastium fontanum subsp vulgare",
"Cerastium nutans",
"Cerastium oreophilum",
"Cercis canadensis",
"Chenopodium berlandieri",
"Chenopodium capitatum",
"Chenopodium desiccatum var. leptophylloides",
"Chenopodium incanum",
"Chenopodium fremontii",
"Chenopodium leptophyllum",
"Chenopodium simplex",
"Chloris virgata",
"Chorizanthe diffusa",
"Chrysosplenium iowense",
"Cicuta maculata",
"Cistanthe umbellata",
"Clarkia amoena",
"Clarkia dudleyana",
"Clarkia purpurea subsp quadrivulnera",
"Clarkia rhomboidea",
"Claytonia lanceolata",
"Claytonia linearis",
"Claytonia parviflora",
"Claytonia perfoliata",
"Claytonia virginica",
"Cochlearia officinalis",
"Collinsia parviflora",
"Collinsia sparsiflora",
"Collomia grandiflora",
"Collomia heterophylla",
"Collomia linearis",
"Conioselinum chinense",
"Conioselinum pacificum",
"Coreopsis grandiflora",
"Corydalis aurea",
"Corydalis aurea subsp occidentalis",
"Corydalis flavula",
"Corydalis micrantha",
"Crepis acuminata",
"Cryptantha affinis",
"Cryptantha ambigua",
"Cryptantha barbigera",
"Cryptantha fendleri",
"Cryptantha simulans",
"Cryptantha torreyana",
"Cryptantha watsonii",
"Cyclachaena xanthiifolia",
"Cynodon dactylon",
"Delphinium depauperatum",
"Descurainia californica",
"Descurainia depressa",
"Descurainia incana",
"Descurainia incana subsp incisa",
"Descurainia incana subsp procera",
"Descurainia incisa subsp incisa",
"Descurainia longepedicellata",
"Descurainia pinnata",
"Descurainia pinnata subsp brachycarpa",
"Descurainia pinnata subsp intermedia",
"Descurainia pinnata subsp ochroleuca",
"Dicentra canadensis",
"Dicentra cucullaria",
"Dicentra formosa",
"Echinocystis lobata",
"Draba cuneifolia",
"Draba reptans",
"Draba stenoloba",
"Dracocephalum parviflorum",
"Duchesnea indica",
"Dysphania ambrosioides",
"Echinospermum virginicum",
"Elephantopus carolinianus",
"Ellisia nyctelea",
"Elymus canadensis",
"Elymus junceus",
"Epilobium coloratum",
"Epilobium latifolium",
"Epilobium minutum",
"Eragrostis pectinacea",
"Erechtites hieracifolia",
"Eremothera chamaenerioides",
"Eremothera minor",
"Erigeron annuus",
"Erigeron canadensis",
"Erigeron divergens",
"Erigeron philadelphicus",
"Eriogonum deflexum",
"Eriogonum thurberi",
"Erysimum asperum",
"Erysimum capitatum",
"Eucrypta micrantha",
"Euphorbia cordifolia",
"Euphorbia glyptosperma",
"Euphorbia humistrata",
"Euphorbia hypericifolia",
"Euphorbia maculata",
"Euphorbia nutans",
"Euphorbia prostrata",
"Euphorbia serpens",
"Euphorbia serpyllifolia",
"Euphorbia serpyllifolia subsp hirtula",
"Euphorbia spathulata",
"Euphorbia stictospora",
"Eutrema edwardsii",
"Fallopia scandens",
"Filago germanica",
"Floerkea proserpinacoides",
"Fragaria virginiana",
"Galinsoga parviflora",
"Galium aparine",
"Galium asprellum",
"Galium bifolium",
"Galium boreale",
"Galium concinnum",
"Galium tinctorium",
"Galium trifidum",
"Galium trifidum subsp brevipes",
"Galium triflorum",
"Gaura mollis",
"Geranium bicknellii",
"Geranium caroliniaum",
"Geranium maculatum",
"Geranium oreganum",
"Geranium richardsonii",
"Geranium robertianum",
"Geranium texanum",
"Geranium viscosissimum",
"Geranium vulcanicola",
"Geum aleppicum",
"Geum canadense",
"Geum macrophyllum",
"Geum rivale",
"Geum triflorum",
"Geum virginianum",
"Gnaphalium pensylvanicum",
"Gnaphalium purpureum",
"Gonolobus caroliniensis",
"Gonolobus suberosus",
"Grayia spinosa",
"Grossularia menziesii",
"Guillenia lasiophylla",
"Hackelia cinerea",
"Hackelia floribunda",
"Hackelia micrantha",
"Hackelia patens",
"Hasteola suaveolens",
"Hedeoma hispida",
"Hedeoma pulegioides",
"Hedysarum alpinum",
"Hedysarum mackenzii",
"Helianthus argophyllus",
"Helianthus decapetalus",
"Helianthus divaricatus",
"Helianthus doronicoides",
"Helianthus giganteus",
"Helianthus grosseserratus",
"Helianthus hirsutus",
"Helianthus laetiflorus",
"Helianthus maximiliani",
"Helianthus nuttallii",
"Helianthus occidentalis",
"Helianthus pauciflorus",
"Helianthus pauciflorus subsp subrhomboideus",
"Helianthus petiolaris",
"Helianthus scaberrimus",
"Helianthus subtuberosus",
"Helianthus tuberosus",
"Heliocarpus americanus",
"Eupatorium purpureum",
"Helianthus annuus",
"Houstonia caerulea",
"Houstonia pusilla",
"Hydrophyllum capitatum",
"Hydrophyllum fendleri",
"Hydrophyllum fendleri var. albifrons",
"Hydrophyllum macrophyllum",
"Hydrophyllum occidentale",
"Hydrophyllum tenuipes",
"Impatiens capensis",
"Impatiens pallida",
"Isopyrum hallii",
"Krigia biflora",
"Krigia cespitosa",
"Krigia dandelion",
"Krigia virginica",
"Lactuca biennis",
"Lactuca canadensis",
"Lactuca elongata",
"Lactuca floridana",
"Lactuca graminifolia",
"Lactuca hirsuta",
"Lactuca leucophaea",
"Lactuca ludoviciana",
"Laportea canadensis",
"Lappula occidentalis",
"Lappula occidentalis var. cupulata",
"Lappula occidentalis var. occidentalis",
"Lappula virginiana",
"Lathyrus japonicus subsp maritimus",
"Lathyrus nevadensis subsp lanceolatus",
"Lathyrus venosus",
"Lipochaeta integrifolia",
"Leavenworthia exigua",
"Lepidium austrinum",
"Lepidium dictyotum",
"Lepidium lasiocarpum",
"Lepidium latipes",
"Lepidium nitidum",
"Lepidium ramosissimum",
"Lepidium thurberi",
"Lepidium virginicum",
"Lepidium virginicum subsp menziesii",
"Lesquerella purpurea",
"Lewisia cotyledon",
"Ligusticum scoticum",
"Linaria canadensis",
"Linum sulcatum",
"Lithophragma bulbiferum",
"Lithophragma parviflorum",
"Lupinus affinis",
"Lupinus argenteus var. tenellus",
"Lupinus hirsutissimus",
"Lupinus lepidus",
"Lupinus minimus",
"Lupinus odoratus",
"Lupinus sericeus",
"Lupinus subvexus",
"Hydrophyllum virginianum",
"Lagenaria siceraria",
"Lupinus bicolor",
"Lupinus densiflorus",
"Lupinus perennis",
"Lupinus polyphyllus",
"Madia gracilis",
"Matelea obliqua",
"Melampyrum lineare",
"Melampyrum lineare var. americanum",
"Melothria sp",
"Mentha arvensis",
"Mentha canadensis",
"Mertensia paniculata",
"Microsteris gracilis",
"Mimulus breweri",
"Mimulus guttatus",
"Mimulus pulsiferae",
"Mimulus tricolor",
"Mirabilis alipes",
"Mirabilis laevis",
"Mirabilis nyctaginea",
"Mirabilis nyctaginea var. angustifolia",
"Monolepis nuttalliana",
"Muhlenbergia mexicana",
"Myosotis alpestris",
"Myosotis laxa",
"Myosotis macrosperma",
"Myosotis verna",
"Nabalus altissimus",
"Navarretia intertexta",
"Nemophila breviflora",
"Nemophila kirtleyi",
"Nemophila microcalyx",
"Nemophila parviflora",
"Nemophila pedunculata",
"Nemophila pedunculata var. sepulta",
"Nemophila pulchella",
"Nicotiana attenuata",
"Nicotiana bigelovii",
"Nicotiana bigelovii var. quadricoloris",
"Nicotiana repanda",
"Nicotiana trigonophylla",
"Oenothera flava",
"Oenothera humifusa",
"Oenothera laciniata",
"Oenothera primiveris",
"Oenothera villosa subsp strigosa",
"Ottleya utahensis",
"Oxytropis deflexa",
"Oxytropis nigrescens",
"Monarda didyma",
"Oenothera biennis",
"Oenothera pallida",
"Oenothera speciosa",
"Papaver nudicaule",
"Parietaria debilis",
"Parietaria pensylvanica",
"Parthenocissus quinquefolia",
"Parthenocissus quinquefolia f. hirsuta",
"Phacelia congesta",
"Phacelia hastata var. leucophylla",
"Phacelia heterophylla",
"Phacelia linearis",
"Phacelia mutabilis",
"Phacelia parryi",
"Physaria gordonii",
"Physostegia parviflora",
"Plagiobothrys cognatus",
"Plagiobothrys hispidulus",
"Plagiobothrys scopulorum",
"Plagiobothrys tenellus",
"Planodes virginicum",
"Plantago aristata",
"Plantago cordata",
"Plantago erecta",
"Plantago patagonica",
"Plantago pusilla",
"Plantago rhodosperma",
"Plantago rugelii",
"Plantago spinulosa",
"Plantago virginica",
"Polygonum douglasii",
"Polygonum polygaloides subsp confertiflorum",
"Polygonum ramosissimum",
"Polygonum aviculare",
"Potentilla canadensis",
"Potentilla gracilis",
"Potentilla gracilis var. fastigiata",
"Potentilla gracilis var. flabelliformis",
"Potentilla norvegica",
"Potentilla norvegica subsp hirsuta",
"Potentilla pulcherrima",
"Potentilla rivalis",
"Pseudostellaria jamesiana",
"Pterostegia drymarioides",
"Pyrrhopappus carolinianus",
"Penstemon angustifolius",
"Phlox divaricata",
"Phlox paniculata",
"Physostegia virginiana",
"Plantago ovata",
"Plantago princeps var. princeps",
"Vitis riparia",
"Setaria magna",
"Sicyos angulatus",
"Vicia gigantea",
"Rorippa palustris",
"Ranunculus abortivus",
"Ranunculus acriformis",
"Ranunculus fascicularis",
"Ranunculus hispidus",
"Ranunculus pensylvanicus",
"Ranunculus recurvatus",
"Ranunculus septentrionalis",
"Ranunculus uncinatus",
"Ribes cynosbati",
"Ribes divaricatum",
"Ribes glandulosum",
"Ribes hirtellum",
"Ribes menziesii",
"Ribes oxyacanthoides",
"Ribes triste",
"Rorippa curvisiliqua",
"Rorippa islandica",
"Rubus allegheniensis",
"Rorippa teres",
"Rubus argutus",
"Rubus canadensis",
"Rubus flagellaris",
"Rubus hispidus",
"Rubus leucodermis",
"Rubus spectabilis",
"Rubus ursinus subsp macropetalus",
"Sairocarpus nuttallianus",
"Salvia carduacea",
"Salvia reflexa",
"Scrophularia leporella",
"Silene acaulis subsp subacaulescens",
"Silene antirrhina",
"Silphium integrifolium",
"Silphium laciniatum",
"Silphium perfoliatum",
"Silphium terebinthinaceum",
"Silphium trifoliatum",
"Smilax sp",
"Solidago gigantea",
"Solidago rigida",
"Solidago sempervirens",
"Solidago serotina",
"Solidago sphacelata",
"Spermacoce eryngioides",
"Sporobolus cryptandrus",
"Sporobolus neglectus",
"Stanleya pinnata",
"Stellaria longipes",
"Stellaria pubera",
"Streptanthella longirostris",
"Streptanthus carinatus subsp arizonicus",
"Symphyotrichum ascendens",
"Symphyotrichum chilense",
"Symphyotrichum ciliolatum",
"Symphyotrichum cordifolium",
"Symphyotrichum laeve",
"Symphyotrichum oblongifolium",
"Symphyotrichum praealtum",
"Symphyotrichum puniceum",
"Symphyotrichum spathulatum",
"Symphyotrichum tradescantii",
"Symphyotrichum undulatum",
"Vitis vulpina",
"Zea mays",
"Vitis aestivalis",
"Vitis aestivalis var. bourquiniana",
"Vitis cordifolia",
"Vitis labrusca",
"Ratibida pinnata",
"Rubus idaeus subsp strigosus",
"Rudbeckia fulgida",
"Rudbeckia hirta",
"Rudbeckia laciniata",
"Rudbeckia sp",
"Rudbeckia triloba",
"Scrophularia marylandica",
"Scrophularia lanceolata",
"Symphyotrichum lateriflorum",
"Symphyotrichum novae-angliae",
"Teucrium canadense",
"Thalictrum pubescens",
"Thysanocarpus curvipes",
"Trautvetteria caroliniensis",
"Triumfetta lappula",
"Turritis glabra",
"Urtica dioica",
"Urtica dioica subsp gracilis",
"Valeriana occidentalis",
"Valeriana sitchensis",
"Valerianella radiata",
"Verbena hastata",
"Verbesina encelioides",
"Verbesina encelioides var. exauriculata",
"Vernonia baldwinii",
"Vernonia noveboracensis",
"Veronica alpina",
"Veronica americana",
"Veronica anagallis-aquatica",
"Veronica peregrina",
"Veronica serpyllifolia",
"Veronica serpyllifolia subsp humifusa",
"Viburnum acerifolium",
"Viburnum cassinoides",
"Viburnum dentatum",
"Viburnum nudum",
"Viburnum pubescens",
"Viburnum rufotomentosum",
"Viburnum trilobum",
"Vicia americana",
"Vicia linearis",
"Vicia ludoviciana",
"Vicia sparsifolia",
"Vitis californica",
"Vitis caribaea",
"Vitis baileyana",
"Vitis cinerea",
"Vitis cinerea var. floridana",
"Vitis rupestris",
"Vitis tiliifolia",
"Vitis x doaniana",
"Whipplea modesta",
"Zinnia peruviana",
"Vaccinium sp",
"Viola bicolor",
"Viola tricolor",
"Viola tricolor f. tenella",
"Vitis bicolor",
"Vitis rotundifolia",
"Vitis rufotomentosa",
"Vitis simpsonii",
"Apogon humilis") ~ "native",
cur_host %in% c("Allium cepa",
"Acacia sp",
"Acanthospermum australe",
"Acanthospermum humile",
"Agrimonia eupatoria",
"Agropyron cristatum",
"Agrostemma githago",
"Allamanda cathartica",
"Allium nigrum",
"Allium sativum",
"Ampelopsis glandulosa",
"Anemone coronaria",
"Anemone dichotoma",
"Anemone nemorosa",
"Antirrhinum majus",
"Apium annuum",
"Arabidopsis thaliana",
"Arctotis fastuosa",
"Argyranthemum frutescens",
"Armoracia rusticana",
"Artemisia absinthium",
"Artemisia arborescens",
"Artemisia argyrophylla var. brevis",
"Astragalus pectinatus",
"Atriplex patula",
"Atriplex prostrata subsp calotheca",
"Atropa belladonna",
"Aurinia saxatilis",
"Avena byzantina",
"Avena sativa",
"Barbarea verna",
"Barbarea vulgaris",
"Beta vulgaris",
"Bidens alba var. radiata",
"Bidens cynapiifolia",
"Bidens pilosa",
"Boechera holboellii",
"Boechera holboellii var. secunda",
"Brassica cretica",
"Brassica juncea",
"Brassica napus",
"Brassica nigra",
"Brassica oleracea",
"Brassica oleracea var. capitata",
"Brassica rapa",
"Bromus inermis",
"Buglossoides arvensis",
"Callistephus chinensis",
"Camelina sativa",
"Capsella bursa-pastoris",
"Cardamine glacialis",
"Cardamine heptaphylla",
"Cardamine hirsuta",
"Cayaponia laciniosa",
"Cayaponia racemosa",
"Cerastium crassipes",
"Cerastium glomeratum",
"Cerastium semidecandrum",
"Chenopodium album",
"Chenopodium quinoa",
"Chenopodium hybridum",
"Chenopodium macrospermum",
"Chenopodium macrospermum subsp salsum",
"Chenopodium murale",
"Cicer arietinum",
"Cichorium intybus",
"Citrullus lanatus",
"Clibadium surinamense",
"Coccinia grandis",
"Colocasia antiquorum",
"Convolvulus arvensis",
"Corydalis solida",
"Cucumis anguria",
"Cucumis melo",
"Cucumis sativus",
"Cucurbita maxima",
"Cucurbita moschata",
"Cucurbita pepo",
"Cucurbita pepo subsp ovifera",
"Cyanus segetum",
"Cyclospermum leptophyllum",
"Dactylis glomerata",
"Descurainia sophia",
"Dianthus caryophyllus",
"Digitalis purpurea",
"Dipsacus fullonum",
"Draba nemorosa",
"Eleusine indica",
"Elymus repens",
"Erigeron bonariensis",
"Erigeron scaposus",
"Erodium cicutarium",
"Erysimum cheiranthoides",
"Draba verna",
"Euphorbia indica",
"Euphrasia antarctica",
"Euphrasia officinalis",
"Fallopia convolvulus",
"Fallopia dumetorum",
"Galium mollugo",
"Galium spurium",
"Geranium pusillum",
"Geranium rotundifolium",
"Geranium sibiricum",
"Hesperis matronalis",
"Hordeum murinum",
"Erysimum ◊ cheiri",
"Fagopyrum esculentum",
"Festuca arundinacea",
"Geum chiloense",
"Glycine max",
"Helichrysum bracteatum",
"Hordeum vulgare",
"Humulus lupulus",
"Humulus lupulus var. lupuloides",
"Impatiens arguta",
"Impatiens balsamina",
"Impatiens flanaganae",
"Impatiens hochstetteri",
"Impatiens walleriana",
"Lactuca sativa",
"Lathyrus odoratus",
"Lilium longiflorum",
"Lolium perenne",
"Luffa acutangula",
"Lupinus hartwegii",
"Lycopersicon esculentum",
"Hypechusa tricolor",
"Hypochaeris chillensis",
"Iberis amara",
"Iberis sempervirens",
"Lactuca saligna",
"Lactuca serriola",
"Lactuca tatarica",
"Lamium amplexicaule",
"Lampsana sp",
"Lappula patula",
"Lepidium apetalum",
"Lepidium densiflorum",
"Lepidium didymum",
"Lobularia maritima",
"Melilotus officinalis",
"Melilotus officinalis subsp alba",
"Mitracarpus hirtus",
"Momordica balsamina",
"Mukia maderaspatana",
"Myosotis arvensis",
"Myosotis discolor",
"Myosotis stricta",
"Myosotis sylvatica",
"Nicotiana benthamiana",
"Nicotiana caesia",
"Nicotiana glauca",
"Nicotiana gossei",
"Nicotiana longiflora",
"Nicotiana stocktonii",
"Oenothera mollissima",
"Matthiola incana",
"Medicago lupulina",
"Medicago sativa",
"Melia azedarach",
"Mesembryanthemum cordifolium",
"Momordica charantia",
"Monoculus hyoseroides",
"Moricandia arvensis",
"Mucuna pruriens var. utilis",
"Nasturtium officinale",
"Nicotiana stocktonii",
"Nicotiana sylvestris",
"Nicotiana tabacum",
"Nicotiana tomentosa",
"Ocimum basilicum",
"Papaver somniferum",
"Parthenium hysterophorus",
"Pericallis cruenta",
"Phalaris aquatica",
"Piptothrix areolaris",
"Polypsecadium magellanicum",
"Parthenocissus tricuspidata",
"Pastinaca sativa",
"Pennisetum glaucum",
"Pilosella aurantiaca",
"Pisum sativum",
"Plantago major",
"Plectranthus scutellarioides",
"Potentilla nepalensis",
"Setaria viridis",
"Ranunculus acris",
"Ranunculus bulbosus",
"Ranunculus minutiflorus",
"Ranunculus peduncularis",
"Ranunculus repens",
"Rubus parvifolius",
"Rubus laciniatus",
"Rumex acetosa",
"Rumex acetosella",
"Rumex crispus",
"Sibara pinnata",
"Silene gallica",
"Silene latifolia",
"Sinapis arvensis var. orientalis",
"Sisymbrium altissimum",
"Sisymbrium irio",
"Sisymbrium officinale",
"Sonchus arvensis",
"Sonchus asper",
"Sonchus gangliformis",
"Sonchus oleraceus",
"Spergula arvensis",
"Spergularia rubra",
"Stellaria media",
"Vitis vinifera",
"Rosa sp",
"Setaria italica",
"Sorghum bicolor",
"Sorghum halepense",
"Vitis 'Lindley' labrusca X vinefera",
"Tarenaya hassleriana",
"Raphanus raphanistrum",
"Raphanus raphanistrum subsp sativus",
"Rheum rhabarbarum",
"Ribes rubrum",
"Ribes uva-crispa",
"Rubus corchorifolius",
"Rubus villosus var. humifusus",
"Scabiosa atropurpurea",
"Scabiosa columbaria",
"Scrophularia nodosa",
"Secale cereale",
"Solanum tuberosum",
"Spilanthes ocymifolia",
"Spinacia oleracea",
"Trichosanthes colubrina",
"Trifolium incarnatum",
"Trifolium repens",
"Trigonella foenum-graecum",
"Triosteum perfoliatum",
"Triticum aestivum",
"Viburnum opulus",
"Viburnum sargentii",
"Viburnum tinus",
"Vicia faba",
"Vicia sativa",
"Vicia sativa subsp nigra",
"Vicia villosa",
"Vigna unguiculata",
"Vitis coignetiae",
"Viola arvensis",
"Vicia hirsuta",
"Veronica arvensis",
"Vicia cracca",
"Vicia sepium",
"Trifolium dubium",
"Vitis davidii",
"Vitis piasezkii") ~ "non-native",)) %>%

#Bin by decade

mutate(decade= case_when(year %in% c(1800:1809) ~ "1800",
year %in% c(1810:1819) ~ "1810",
year %in% c(1820:1829) ~ "1820",
year %in% c(1830:1839) ~ "1830",
year %in% c(1840:1849) ~ "1840",
year %in% c(1850:1859) ~ "1850",
year %in% c(1860:1869) ~ "1860",
year %in% c(1870:1879) ~ "1870",
year %in% c(1880:1889) ~ "1880",
year %in% c(1890:1899) ~ "1890",
year %in% c(1900:1909) ~ "1900",
year %in% c(1910:1919) ~ "1910",
year %in% c(1920:1929) ~ "1920",
year %in% c(1930:1939) ~ "1930",
year %in% c(1940:1949) ~ "1940",
year %in% c(1950:1959) ~ "1950",
year %in% c(1960:1969) ~ "1960",
year %in% c(1970:1979) ~ "1970",
year %in% c(1980:1989) ~ "1980",
year %in% c(1990:1999) ~ "1990",
year %in% c(2000:2009) ~ "2000",
year %in% c(2010:2019) ~ "2010",
year %in% c(2020:2029) ~ "2020",)) %>%

relocate(decade, .after=day)

#Combine the cultivation and native statuses into a single column
Downy_mildews_tidy$cultivated_native <-paste(Downy_mildews_tidy$cultivated,
											Downy_mildews_tidy$native_status, 
											sep="-")

#Making sure all NAs are appropriate

Downy_mildews_tidy <- type_convert(Downy_mildews_tidy, na= c("", "NA", "NA-NA"))

write.csv(Downy_mildews_tidy, file="Downy_mildews_tidy.csv")

#Creating subsets of the data by Continent
North_American_downy_mildews_tidy <- subset(Downy_mildews_tidy, continent=="North America")

write.csv(North_American_downy_mildews_tidy,
			file="North_American_downy_mildews_tidy.csv")

#Deduplication with collapse
North_American_downy_mildews_tidy_collapse <-North_American_downy_mildews_tidy %>% 
						fgroup_by(cur_scientific_name,
							recorded_by,
							year,
							month,
							day,
							cur_host,
							country,
							state_province) %>%
						collapg %>%
						relocate(institution_code, .before=cur_scientific_name) %>%
						relocate(catalog_number, .after=institution_code) %>%
						relocate(basis_of_record, .after=catalog_number) %>%
						relocate(cur_genus, .before=cur_scientific_name) %>%
						relocate(cur_epithet, .after=cur_genus) %>%
						relocate(record_number, .after=recorded_by) %>%
						relocate(decade, .after=day) %>%
						relocate(host_family, .after=cur_host) %>%
						relocate(cultivated, .after=host_family) %>%
						relocate(native_status, .after=cultivated) %>%
						relocate(cultivated_native, .after=native_status) %>%
						relocate(country, .after=continent) %>%
						relocate(state_province, .after=country)

#Removing Peronosclerospora philippinensis & sacchari because they are inoculation studies
#at Fr. Detetrick and not true diversity

North_American_downy_mildews_tidy_collapse <-subset(North_American_downy_mildews_tidy_collapse,
									!cur_scientific_name=="Peronosclerospora philippinensis" &
									!cur_scientific_name=="Peronosclerospora sacchari")

write.csv(North_American_downy_mildews_tidy_collapse,
			file="North_American_downy_mildews_tidy_collapse.csv")

#Drop missing decades
North_American_downy_mildews_tidy_decades <- drop_na(
									North_American_downy_mildews_tidy_collapse,
									decade)


#To make sure it plots correctly, turning decade into a character
North_American_downy_mildews_tidy_decades$decade <-as.character(
										North_American_downy_mildews_tidy_decades$decade)

write.csv(North_American_downy_mildews_tidy_decades,
			file="North_American_downy_mildews_tidy_decades.csv")


#Dropping missing host plant status
North_American_downy_mildews_tidy_cultivated_native <- drop_na(
										North_American_downy_mildews_tidy_collapse,
										cultivated_native)

#Creating subset by country
United_States_downy_mildew <- subset(North_American_downy_mildews_tidy_collapse, country=="United States of America")

write.csv(United_States_downy_mildew, file="United_States_downy_mildew.csv")

Canada_downy_mildew <-subset(North_American_downy_mildews_tidy_collapse, country=="Canada")

write.csv(Canada_downy_mildew, file="Canada_downy_mildew.csv")

Mexico_downy_mildew <-subset(North_American_downy_mildews_tidy_collapse, country=="Mexico")

write.csv(Mexico_downy_mildew, file="Mexico_downy_mildew.csv")

USA_downy_mildews_tidy_decades <- drop_na(United_States_downy_mildew,
											decade)

USA_downy_mildews_tidy_decades$decade <-as.character(USA_downy_mildews_tidy_decades$decade)

#Table of Collectors & Species for North America 
North_American_downy_mildews_tidy_collector <- drop_na(North_American_downy_mildews_tidy_collapse, recorded_by)

North_American_downy_mildews_tidy_collector_species_table <- table(North_American_downy_mildews_tidy_collector$recorded_by,
											North_American_downy_mildews_tidy_collector$cur_scientific_name)

write.csv(North_American_downy_mildews_tidy_collector_species_table,
			file="North_American_downy_mildews_tidy_collector_species_table.csv")

#Basic statistics
Number_collections_each_species_NA <-North_American_downy_mildews_tidy_collapse %>%
	count(cur_scientific_name) %>%
	arrange(desc(n))
write.csv(Number_collections_each_species_NA, 
			file="Number_collections_each_species_NA.csv")

Number_of_collections_each_country <-North_American_downy_mildews_tidy_collapse %>%
	count(country) %>%
	arrange(desc(n))
write.csv(Number_of_collections_each_country, file="Number_collections_each_country.csv")

Number_of_collections_each_collector_NA <-North_American_downy_mildews_tidy_collector %>%
	count(recorded_by) %>%
	arrange(desc(n))
write.csv(Number_of_collections_each_collector_NA, 
			file="Number_of_collections_each_collector_NA.csv")

Number_of_collections_each_decade_NA <-North_American_downy_mildews_tidy_decades %>%
	count(decade) %>%
	arrange(desc(n))
write.csv(Number_of_collections_each_decade_NA, 
			file="Number_of_collections_each_decade_NA.csv")

Number_of_collections_each_state <-United_States_downy_mildew %>%
	count(state_province) %>%
	arrange(desc(n))
write.csv(Number_of_collections_each_state, 
			file="Number_of_collections_each_state.csv")

Number_collections_each_species_USA <-United_States_downy_mildew %>%
	count(cur_scientific_name) %>%
	arrange(desc(n))
write.csv(Number_collections_each_species_USA, 
			file="Number_collections_each_species_USA.csv")

Number_of_collections_each_collector_USA <-United_States_downy_mildew %>%
	count(recorded_by) %>%
	arrange(desc(n))
write.csv(Number_of_collections_each_collector_USA, 
			file="Number_of_collections_each_collector_USA.csv")

Number_of_collections_host_plant_status_NA <-North_American_downy_mildews_tidy_collapse %>%
	count(cultivated_native) %>%
	arrange(desc(n))
write.csv(Number_of_collections_host_plant_status_NA, 
			file="Number_of_collections_host_plant_status_NA.csv")

Number_of_collections_host_plant_status_USA <-United_States_downy_mildew %>%
	count(cultivated_native) %>%
	arrange(desc(n))
write.csv(Number_of_collections_host_plant_status_USA, 
			file="Number_of_collections_host_plant_status_USA.csv")

Number_collections_decade_NA <-North_American_downy_mildews_tidy_decades %>%
					count(decade)

write.csv(Number_collections_decade_NA, file="Number_collections_decade_NA.csv")

Decade_species_table_NA<-table(North_American_downy_mildews_tidy_decades$decade,
							North_American_downy_mildews_tidy_decades$cur_scientific_name)

write.csv(Decade_species_table_NA, file="Decade_species_table_NA.csv")

observed_number_species_decade <-specnumber(Decade_species_table_NA)

capture.output(specnumber(Decade_species_table_NA), file="NA_observed_number_species_decade.txt")


Number_collections_per_genus_NA <-North_American_downy_mildews_tidy_collapse %>%
					count(cur_genus)
					
write.csv(Number_collections_per_genus_NA, file="Number_collections_per_genus_NA.csv")

Genus_species_table_NA<-table(North_American_downy_mildews_tidy_collapse$cur_genus,
							North_American_downy_mildews_tidy_collapse$cur_scientific_name)

write.csv(Genus_species_table_NA, file="Genus_species_table_NA.csv")

observed_number_species_genus <-specnumber(Genus_species_table_NA)

capture.output(specnumber(Genus_species_table_NA), file="NA_observed_number_species_genus.txt")


Number_collections_decade_USA <-USA_downy_mildews_tidy_decades %>%
					count(decade)

write.csv(Number_collections_decade_USA, file="Number_collections_decade_USA.csv")

Decade_species_table_USA<-table(USA_downy_mildews_tidy_decades$decade,
							USA_downy_mildews_tidy_decades$cur_scientific_name)

write.csv(Decade_species_table_USA, file="Decade_species_table_USA.csv")

observed_number_species_decade <-specnumber(Decade_species_table_USA)

capture.output(specnumber(Decade_species_table_USA), file="USA_observed_number_species_decade.txt")


Number_collections_per_genus_USA <-United_States_downy_mildew %>%
					count(cur_genus)
					
write.csv(Number_collections_per_genus_USA, file="Number_collections_per_genus_USA.csv")

Genus_species_table_USA<-table(United_States_downy_mildew$cur_genus,
							United_States_downy_mildew$cur_scientific_name)

write.csv(Genus_species_table_USA, file="Genus_species_table_USA.csv")

observed_number_species_genus <-specnumber(Genus_species_table_USA)

capture.output(specnumber(Genus_species_table_USA), file="USA_observed_number_species_genus.txt")

#Tables for curves
NA_DM_table <- table(North_American_downy_mildews_tidy_collapse$continent,
						North_American_downy_mildews_tidy_collapse$cur_scientific_name)

write.csv(NA_DM_table, file="NA_DM_table.csv")

NA_DM_collector_species_table <- table(North_American_downy_mildews_tidy_collapse$recorded_by,
									North_American_downy_mildews_tidy_collapse$cur_scientific_name)

write.csv(NA_DM_collector_species_table, 
			file="NA_DM_collector_species_table.csv")

NA_DM_table_country <- table(North_American_downy_mildews_tidy_collapse$country,
								North_American_downy_mildews_tidy_collapse$cur_scientific_name)

write.csv(NA_DM_table_country, file="NA_DM_table_country.csv")

US_DM_table <-table(United_States_downy_mildew$country,
					United_States_downy_mildew$cur_scientific_name)

write.csv(US_DM_table, file="US_DM_table.csv")

US_DM_state_species_table <- table(United_States_downy_mildew$state_province,
								United_States_downy_mildew$cur_scientific_name)

write.csv(US_DM_state_species_table, file="US_DM_state_species_table.csv")

US_DM_collector_species_table <- table(United_States_downy_mildew$recorded_by,
									United_States_downy_mildew$cur_scientific_name)

write.csv(US_DM_collector_species_table, 
			file="US_DM_collector_species_table.csv")

Canada_DM_table <-table(Canada_downy_mildew$country, Canada_downy_mildew$cur_scientific_name)
write.csv(Canada_DM_table, file="Canada_DM_table.csv")

Mexico_DM_table <-table(Mexico_downy_mildew$country, Mexico_downy_mildew$cur_scientific_name)
write.csv(Mexico_DM_table, file="Mexico_DM_table.csv")

#Observed species for Canada & Mexico
Canada_observed_number_species <-specnumber(Canada_DM_table)
capture.output(specnumber(Canada_DM_table), file= "Canada_Observed_number_species.txt")

Mexico_observed_number_species <-specnumber(Mexico_DM_table)
capture.output(specnumber(Mexico_DM_table), file= "Mexico_Observed_number_species.txt")

#Color code cheat sheet to keep the genera consistent across graphs 
#fill = modified RdBU with colors from RdYlBl & PuOr
#Basidiophora	#a50026
#Benua	#d73027
#Bremia	#f46d43
#Hyaloperonospora	#fdae61
#Paraperonospora	#fee090
#Peronosclerospora	#7f3b08
#Peronospora	#92c5de
#Plasmopara	#4393c3
#Plasmoverna	#2166ac
#Pseudoperonospora	#053061
#Sclerophthora	#542788
#Sclerospora	#2d004b


#Figure 1
#A = All N. America records binned by decade
#B = All N. America records binned by host plant status
#C = All N. America records binned by country of collection

Plot_decade <-ggplot(North_American_downy_mildews_tidy_decades, aes(x=decade, fill=cur_genus)) + 
geom_bar() + 
scale_fill_manual(name= NULL, values= c("#a50026",
							"#d73027",
							"#f46d43",
							"#fdae61",
							"#fee090",
							"#7f3b08",
							"#92c5de",
							"#4393c3",
							"#2166ac",
							"#053061",
							"#542788",
							"#2d004b")) +
scale_y_continuous(breaks=seq(250, 1500, by=250)) +
theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=10, color="black")) +
theme(axis.text.y=element_text(size=10, color="black")) +
theme(legend.position="none") +
labs(x="Decade of collection", y="Number of collections") +
theme(axis.title.x=element_text(size=10, color="black")) +
theme(axis.title.y=element_text(size=10, color="black"))

Plot_country <-ggplot(North_American_downy_mildews_tidy_collapse, aes(x=country, fill=cur_genus)) + 
geom_bar() +
scale_fill_manual(name= NULL, values= c("#a50026",
							"#d73027",
							"#f46d43",
							"#fdae61",
							"#fee090",
							"#7f3b08",
							"#92c5de",
							"#4393c3",
							"#2166ac",
							"#053061",
							"#542788",
							"#2d004b")) +
theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1,size=10, color="black")) +
theme(axis.text.y=element_text(size=10, color="black")) +
labs(x="Country", y="Number of collections") +
guides(fill=guide_legend(title="Downy mildew genera")) +
theme(axis.title.x=element_text(size=10, color="black")) +
theme(axis.title.y=element_text(size=10, color="black")) +
theme(legend.text=element_text(size=10, face="italic", color="black")) +
theme(legend.title=element_text(size=10))

Plot_native <-ggplot(North_American_downy_mildews_tidy_cultivated_native, aes(x=cultivated_native, fill=cur_genus)) + 
geom_bar() + 
scale_fill_manual(name= NULL, values= c("#a50026",
							"#d73027",
							"#f46d43",
							"#fdae61",
							"#fee090",
							"#7f3b08",
							"#92c5de",
							"#4393c3",
							"#2166ac",
							"#053061",
							"#542788",
							"#2d004b")) +
scale_x_discrete(breaks=c("cultivated-native", "cultivated-non-native", "wild-native", "wild-non-native"),
		     labels=c("Cultivated, native", "Cultivated, non-native", "Wild, native", "Wild, non-native")) +
theme(legend.position="none") +
theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=10, color="black")) +
theme(axis.text.y=element_text(size=10, color="black")) +
labs(x="Status of host plant", y="Number of collections") +
theme(axis.title.x=element_text(size=10, color="black")) +
theme(axis.title.y=element_text(size=10, color="black"))

Figure_1 <-(Plot_decade) / (Plot_native | Plot_country)

pdf("Fig_1_plot_specimens_binned_three_graphs_modified_RdBu_fill_by_genus_with_labels_legend.pdf")
print(Figure_1 +
plot_annotation(tag_levels='A',
		title="Distribution of North American downy mildew collections") +
plot_layout(guides='collect') 
)
dev.off()

tiff("Fig_1_plot_specimens_binned_three_graphs_modified_RdBu_fill_by_genus_with_labels_legend.tiff",
width= 7.4*ppi, height=9.4*ppi, res = ppi, compression= "lzw")
print(Figure_1 +
plot_annotation(tag_levels='A',
		title="Distribution of North American downy mildew collections") +
plot_layout(guides='collect') 
)
dev.off()


#Figure 2
#Venn Diagram
NA_DM_reduced_columns <-subset(North_American_downy_mildews_tidy_collapse, select = c(cur_scientific_name, country))

venn.diagram(
	x=list(
		NA_DM_reduced_columns %>% filter(country=="United States of America") %>% select(cur_scientific_name) %>% unlist(),
		NA_DM_reduced_columns %>% filter(country=="Canada") %>% select(cur_scientific_name) %>% unlist(),
		NA_DM_reduced_columns %>% filter(country=="Mexico") %>% select(cur_scientific_name) %>% unlist()
		),
		category.names= c("United States of America", "Canada", "Mexico"),
		filename= "Fig_2_venn_diagram_number_species_each_country.tiff",
		output= TRUE,
			imagetype="tiff",
			height=190,
			width=190,
			units="mm",
			resolution=1000,
			compression= "lzw",
			lwd= 1,
			col=c("#756bb1", "#386cb0", "#66a61e"),
			fill= c(alpha("#756bb1", 0.3), alpha("#386cb0", 0.3), alpha("#66a61e", 0.3)),
			cex= 2,
			cat.cex=2,
			cat.default.pos="outer",
			cat.pos= c(-30, 30, -3),
			cat.dist= c(0.03, 0.04, 0.0275),
			cat.col= "black",
			rotation=1
			)

#Figure 3
#Number of herbarium collections and community science observations
#made in each state/province of the USA
United_States_downy_mildew_state <-drop_na(United_States_downy_mildew, state_province)

Plot_specimens_binned_by_state_modified_RdBu_fill_by_genus <-ggplot(United_States_downy_mildew_state,
											aes(x=reorder(state_province,
											desc(state_province)),
											fill=cur_genus)) + 
geom_bar() + 
scale_fill_manual(name= NULL, values= c("#a50026",
							"#d73027",
							"#f46d43",
							"#fdae61",
							"#fee090",
							"#7f3b08",
							"#92c5de",
							"#4393c3",
							"#2166ac",
							"#053061",
							"#542788",
							"#2d004b")) +
scale_y_continuous(expand=expansion(mult=c(0, .1))) +
theme(axis.text.x=element_text(size=10, color="black")) +
theme(axis.text.y=element_text(size=10, color="black")) +
labs(x=NULL, y="Number of collections") +
guides(fill=guide_legend(title="Downy mildew genera")) +
theme(legend.title=element_text(size=10))+
theme(legend.text=element_text(size=10, face="italic", color="black")) +
coord_flip()

pdf("Fig_3_plot_specimens_binned_by_state_modified_RdBu_fill_by_genus.pdf")
print(Plot_specimens_binned_by_state_modified_RdBu_fill_by_genus)
dev.off()

tiff("Fig_3_plot_specimens_binned_by_state_modified_RdBu_fill_by_genus.tiff", 
width= 7.4*ppi,
height=9.4*ppi,
res = ppi, 
compression= "lzw")
print(Plot_specimens_binned_by_state_modified_RdBu_fill_by_genus)
dev.off()

#Figure 4
#Collections of the top 6 collectors by decade
#Make subsets based off collector

JJ_Davis_USA_DM <- subset(United_States_downy_mildew, recorded_by=="J J Davis")

JJ_Davis_USA_DM <-drop_na(JJ_Davis_USA_DM, decade)

JJ_Davis_USA_DM <-drop_na(JJ_Davis_USA_DM, cultivated_native)

JJ_Davis_USA_DM$decade <-as.character(JJ_Davis_USA_DM$decade)

write.csv(JJ_Davis_USA_DM, file="JJ_Davis_USA_DM.csv")

AB_Seymour_USA_DM <- subset(United_States_downy_mildew, recorded_by=="A B Seymour")

AB_Seymour_USA_DM <-drop_na(AB_Seymour_USA_DM, decade)

AB_Seymour_USA_DM <-drop_na(AB_Seymour_USA_DM, cultivated_native)

AB_Seymour_USA_DM$decade <-as.character(AB_Seymour_USA_DM$decade)

write.csv(AB_Seymour_USA_DM, file="AB_Seymour_USA_DM.csv")

HC_Greene_USA_DM <- subset(United_States_downy_mildew, recorded_by=="H C Greene")

HC_Greene_USA_DM <-drop_na(HC_Greene_USA_DM, decade)

HC_Greene_USA_DM <-drop_na(HC_Greene_USA_DM, cultivated_native)

HC_Greene_USA_DM$decade <-as.character(HC_Greene_USA_DM$decade)

write.csv(HC_Greene_USA_DM, file="HC_Greene_USA_DM.csv")

CT_Rogerson_USA_DM <- subset(United_States_downy_mildew, recorded_by=="C T Rogerson")

CT_Rogerson_USA_DM <-drop_na(CT_Rogerson_USA_DM, decade)

CT_Rogerson_USA_DM <-drop_na(CT_Rogerson_USA_DM, cultivated_native)

CT_Rogerson_USA_DM$decade <-as.character(CT_Rogerson_USA_DM$decade)

write.csv(CT_Rogerson_USA_DM, file="CT_Rogerson_USA_DM.csv")

GP_Clinton_USA_DM <- subset(United_States_downy_mildew, recorded_by=="G P Clinton")

GP_Clinton_USA_DM <-drop_na(GP_Clinton_USA_DM, decade)

GP_Clinton_USA_DM <-drop_na(GP_Clinton_USA_DM, cultivated_native)

GP_Clinton_USA_DM$decade <-as.character(GP_Clinton_USA_DM$decade)

write.csv(GP_Clinton_USA_DM, file="GP_Clinton_USA_DM.csv")

LH_Pammel_USA_DM <- subset(United_States_downy_mildew, recorded_by=="L H Pammel")

LH_Pammel_USA_DM <-drop_na(LH_Pammel_USA_DM, decade)

LH_Pammel_USA_DM <-drop_na(LH_Pammel_USA_DM, cultivated_native)

LH_Pammel_USA_DM$decade <-as.character(LH_Pammel_USA_DM$decade)

write.csv(LH_Pammel_USA_DM, file="LH_Pammel_USA_DM.csv")


JJ_Davis_decade <-ggplot(JJ_Davis_USA_DM, 
aes(x=decade, fill=cur_genus)) + 
geom_bar() +
ggtitle("J. J. Davis") + 
scale_fill_manual(name= NULL, values= c("#a50026",
							"#d73027",
							"#f46d43",
							"#fdae61",
							"#fee090",
							"#92c5de",
							"#4393c3",
							"#2166ac",
							"#053061",
							"#2d004b")) +
guides(fill=guide_legend(title="Downy mildew genera")) +
theme(legend.title=element_text(size=10)) +
theme(legend.text=element_text(size=10, face="italic", color="black")) +
theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=10, color="black")) +
theme(axis.text.y=element_text(size=10, color="black")) +
theme(axis.title.x=element_text(size=10, color="black")) +
theme(axis.title.y=element_text(size=10, color="black")) +
theme(legend.text=element_text(size=10, color="black")) +
theme(plot.title=element_text(size=10)) +
labs(x=NULL, y="Number of collections")

AB_Seymour_decade <-ggplot(AB_Seymour_USA_DM, 
aes(x=decade, fill=cur_genus)) + 
geom_bar() + 
scale_fill_manual(name= NULL, values= c("#a50026",
							"#f46d43",
							"#fdae61",
							"#92c5de",
							"#4393c3",
							"#2166ac",
							"#053061")) +
theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=10, color="black")) +
theme(axis.text.y=element_text(size=10, color="black")) +
theme(axis.title.x=element_text(size=10, color="black")) +
theme(axis.title.y=element_text(size=10, color="black")) +
theme(legend.position="none") +
labs(x=NULL, y=NULL) +
ggtitle("A. B. Seymour") +
theme(plot.title=element_text(size=10))

HC_Greene_decade <-ggplot(HC_Greene_USA_DM, 
aes(x=decade, fill=cur_genus)) + 
geom_bar() + 
scale_fill_manual(name= NULL, values= c("#a50026",
							"#f46d43",
							"#fdae61",
							"#fee090",
							"#92c5de",
							"#4393c3",
							"#2166ac",
							"#053061",
							"#2d004b")) +
theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=10, color="black")) +
theme(axis.text.y=element_text(size=10, color="black")) +
theme(legend.position="none") +
theme(axis.title.x=element_text(size=10, color="black")) +
theme(axis.title.y=element_text(size=10, color="black")) +
labs(x=NULL, y="Number of collections") +
ggtitle("H. C. Greene") +
theme(plot.title=element_text(size=10))


CT_Rogerson_decade <-ggplot(CT_Rogerson_USA_DM, 
aes(x=decade, fill=cur_genus)) + 
geom_bar() + 
scale_fill_manual(name= NULL, values= c("#a50026",
							"#f46d43",
							"#fdae61",
							"#fee090",
							"#92c5de",
							"#4393c3",
							"#053061",
							"#2d004b")) +
theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=10, color="black")) +
theme(axis.text.y=element_text(size=10, color="black")) +
theme(legend.position="none") +
theme(axis.title.x=element_text(size=10, color="black")) +
theme(axis.title.y=element_text(size=10, color="black")) +
labs(x=NULL, y=NULL) +
ggtitle("C. T. Rogerson") +
theme(plot.title=element_text(size=10))


GP_Clinton_decade <-ggplot(GP_Clinton_USA_DM, 
aes(x=decade, fill=cur_genus)) + 
geom_bar() + 
scale_fill_manual(name= NULL, values= c("#a50026",
							"#f46d43",
							"#fdae61",
							"#92c5de",
							"#4393c3",
							"#2166ac",
							"#053061",
							"#2d004b")) +
theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=10, color="black")) +
theme(axis.text.y=element_text(size=10, color="black")) +
theme(legend.position="none") +
theme(axis.title.x=element_text(size=10, color="black")) +
theme(axis.title.y=element_text(size=10, color="black")) +
labs(x="Decade of collection", y="Number of collections") +
ggtitle("G. P. Clinton") +
theme(plot.title=element_text(size=10))

LH_Pammel_decade <-ggplot(LH_Pammel_USA_DM, 
aes(x=decade, fill=cur_genus)) + 
geom_bar() + 
scale_fill_manual(name= NULL, values= c("#a50026",
							"#d73027",
							"#f46d43",
							"#fdae61",
							"#fee090",
							"#92c5de",
							"#4393c3",
							"#2166ac",
							"#053061",
							"#2d004b")) +
theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=10, color="black")) +
theme(axis.text.y=element_text(size=10, color="black")) +
theme(legend.position="none") +
theme(axis.title.x=element_text(size=10, color="black")) +
theme(axis.title.y=element_text(size=10, color="black")) +
labs(x="Decade of collection", y=NULL) +
ggtitle("L. H. Pammel") +
theme(plot.title=element_text(size=10))




Collector_decade_patchwork <- JJ_Davis_decade +
					AB_Seymour_decade +
					HC_Greene_decade +
					CT_Rogerson_decade +
					GP_Clinton_decade +
					LH_Pammel_decade +
					plot_layout(ncol=2)

pdf("Fig_4_Collector_decade_figure.pdf")
print(
Collector_decade_patchwork +
plot_annotation(tag_levels= 'A',
			title="Collections of the top six collectors by decade") +
plot_layout(guides='collect')
)
dev.off()

tiff("Fig_4_Collector_decade_figure.tiff", 
width= 7.4*ppi,
height=9.4*ppi,
res = ppi, 
compression= "lzw")
print(Collector_decade_patchwork +
plot_annotation(tag_levels= 'A',
			title="Collections of the top 6 collectors by decade") +
plot_layout(guides='collect')
)
dev.off()


#Figure 5
#Distribution maps of top 4 collected species
#Make & clean subsets of the data

United_States_downy_mildew_lat_long_clean <-drop_na(United_States_downy_mildew,
						decimal_latitude)

						
Plasmopara_viticola <-subset(United_States_downy_mildew_lat_long_clean, cur_scientific_name=="Plasmopara viticola")
Bremia_lactucae <-subset(United_States_downy_mildew_lat_long_clean, cur_scientific_name=="Bremia lactucae")
Plasmopara_halstedii <-subset(United_States_downy_mildew_lat_long_clean, cur_scientific_name=="Plasmopara halstedii")
Pseudoperonospora_cubensis <-subset(United_States_downy_mildew_lat_long_clean, cur_scientific_name=="Pseudoperonospora cubensis")
Basidiophora_entospora <-subset(United_States_downy_mildew_lat_long_clean, cur_scientific_name=="Basidiophora entospora")

write.csv(Plasmopara_viticola, file="Plasmopara_viticola.csv")
write.csv(Bremia_lactucae, file="Bremia_lactucae.csv")
write.csv(Plasmopara_halstedii, file="Plasmopara_halstedii.csv")
write.csv(Pseudoperonospora_cubensis, file="Pseudoperonospora_cubensis.csv")

#Maps

world <-ne_countries(scale="medium", returnclass="sf")

class(world)

states <-st_as_sf(map("state", plot=FALSE, fill=TRUE))

Plasmopara_viticola_map <-ggplot(data=world)+
geom_sf(fill="white", color="black") +
geom_sf(data=states, fill=NA) +
geom_point(data=Plasmopara_viticola,
		aes(x=decimal_longitude,
			y=decimal_latitude),
		size=2,
		shape=21,
		color="black",
		fill="black") +
ggtitle("Plasmopara viticola") +
theme(plot.title=element_text(face='italic')) +
labs(x=NULL, y=NULL) +
theme(axis.text.x=element_text(size=10, color="black", angle=45, vjust=1, hjust=1)) +
theme(axis.text.y=element_text(size=10, color="black")) +
coord_sf(xlim=c(-125, -65), ylim=c(25, 49))

Bremia_lactuae_map <-ggplot(data=world)+
geom_sf(fill="white", color="black") +
geom_sf(data=states, fill=NA) +
geom_point(data=Bremia_lactucae,
		aes(x=decimal_longitude,
			y=decimal_latitude),
		size=2,
		shape=21,
		color="black",
		fill="black") +
ggtitle("Bremia lactucae") +
theme(plot.title=element_text(face='italic')) +
labs(x=NULL, y=NULL) +
theme(axis.text.x=element_text(size=10, color="black", angle=45, vjust=1, hjust=1)) +
theme(axis.text.y=element_text(size=10, color="black")) +
coord_sf(xlim=c(-125, -65), ylim=c(25, 49))

Plasmopara_halstedii_map <-ggplot(data=world)+
geom_sf(fill="white", color="black") +
geom_sf(data=states, fill=NA) +
geom_point(data=Plasmopara_halstedii,
		aes(x=decimal_longitude,
			y=decimal_latitude),
		size=2,
		shape=21,
		color="black",
		fill="black") +
ggtitle("Plasmopara halstedii") +
theme(plot.title=element_text(face='italic')) +
labs(x=NULL, y=NULL) +
theme(axis.text.x=element_text(size=10, color="black", angle=45, vjust=1, hjust=1)) +
theme(axis.text.y=element_text(size=10, color="black")) +
coord_sf(xlim=c(-125, -65), ylim=c(25, 49))

Pseudoperonospora_cubensis_map <-ggplot(data=world)+
geom_sf(fill="white", color="black") +
geom_sf(data=states, fill=NA) +
geom_point(data=Pseudoperonospora_cubensis,
		aes(x=decimal_longitude,
			y=decimal_latitude),
		size=2,
		shape=21,
		color="black",
		fill="black") +
ggtitle("Pseudoperonospora cubensis") +
theme(plot.title=element_text(face='italic')) +
labs(x=NULL, y=NULL) +
theme(axis.text.x=element_text(size=10, color="black", angle=45, vjust=1, hjust=1)) +
theme(axis.text.y=element_text(size=10, color="black")) +
coord_sf(xlim=c(-125, -65), ylim=c(25, 49))




Figure_5 <-Plasmopara_viticola_map +
		Bremia_lactuae_map +
		Plasmopara_halstedii_map +
		Pseudoperonospora_cubensis_map +
		plot_layout(ncol=2)
pdf("Fig_5_Top_four_distribution_maps.pdf")
print(
Figure_5 +
plot_annotation(tag_levels="A",
			title="Distribution of the top four collected species") +
plot_layout(guides='collect')
)
dev.off()

tiff("Fig_5_Top_four_distribution_maps.tiff",
width= 7.4*ppi,
height=9.4*ppi,
res = ppi, 
compression= "lzw")
print(
Figure_5 +
plot_annotation(tag_levels="A",
			title="Distribution of the top four collected species") +
plot_layout(guides='collect')
)
dev.off()


#Supplemental figures

#Basic stats plots
Plot_number_collections_per_collector_NA <-ggplot(Number_of_collections_each_collector_NA,
													aes(x=reorder(recorded_by, -n),
													y=n,
													color="black",
													fill="black")) + 
geom_bar(stat="identity") +
theme(axis.text.x=element_blank()) +
theme(axis.text.y=element_text(size=12, color="black")) +
guides(x="none") +
theme(panel.grid.major=element_blank()) +
theme(panel.grid.minor=element_blank()) +
scale_y_continuous(expand=expansion(mult=c(0, 0.04))) +
labs(x=NULL, y=NULL)

pdf("Plot_collections_per_collector_NA.pdf")
print(Plot_number_collections_per_collector_NA)
dev.off()

tiff("Plot_collections_per_collector_NA.tiff",
width= 7.4*ppi,
height=4.7*ppi,
res = ppi,
compression= "lzw")
print(Plot_number_collections_per_collector_NA)
dev.off()

Plot_number_collections_per_collector_USA <-ggplot(Number_of_collections_each_collector_USA,
													aes(x=reorder(recorded_by, -n),
													y=n,
													color="black",
													fill="black")) + 
geom_bar(stat="identity") +
theme(axis.text.x=element_blank()) +
theme(axis.text.y=element_text(size=12, color="black")) +
guides(x="none") +
theme(panel.grid.major=element_blank()) +
theme(panel.grid.minor=element_blank()) +
scale_y_continuous(expand=expansion(mult=c(0, 0.04))) +
labs(x=NULL, y=NULL)

pdf("Sup_Fig_3_Plot_collections_per_collector_USA.pdf")
print(Plot_number_collections_per_collector_USA)
dev.off()

tiff("Sup_Fig_3_Plot_collections_per_collector_USA.tiff",
width= 7.4*ppi,
height=4.7*ppi,
res = ppi,
compression= "lzw")
print(Plot_number_collections_per_collector_USA)
dev.off()

Plot_number_collections_per_species_NA <-ggplot(Number_collections_each_species_NA,
												aes(x=reorder(cur_scientific_name, -n), y=n,
												color="black",
												fill="black")) + 
geom_bar(stat="identity") +
theme(axis.text.x=element_blank()) +
theme(axis.text.y=element_text(size=12, color="black")) +
guides(x="none") +
theme(panel.grid.major=element_blank()) +
theme(panel.grid.minor=element_blank()) +
scale_y_continuous(expand=expansion(mult=c(0, 0.06))) +
labs(x=NULL, y=NULL)

pdf("Plot_collections_per_species_NA.pdf")
print(Plot_number_collections_per_species_NA)
dev.off()

tiff("Plot_collections_per_species_NA.tiif",
width= 7.4*ppi,
height=4.7*ppi,
res = ppi,
compression= "lzw")
print(Plot_number_collections_per_species_NA)
dev.off()


Plot_number_collections_per_species_USA <-ggplot(Number_collections_each_species_USA,
												aes(x=reorder(cur_scientific_name, -n), y=n,
												color="black",
												fill="black")) +  
geom_bar(stat="identity") +
theme(axis.text.x=element_blank()) +
theme(axis.text.y=element_text(size=12)) +
guides(x="none") +
theme(panel.grid.major=element_blank()) +
theme(panel.grid.minor=element_blank()) +
scale_y_continuous(expand=expansion(mult=c(0, 0.06))) +
labs(x=NULL, y=NULL)

pdf("Plot_collections_per_species_USA.pdf")
print(Plot_number_collections_per_species_USA)
dev.off()

tiff("Plot_collections_per_species_USA.tiff",
width= 7.4*ppi,
height=4.7*ppi,
res = ppi,
compression= "lzw")
print(Plot_number_collections_per_species_USA)
dev.off()


#Species accumulation curve for North America

North_America_Observed_number_species <- specnumber(NA_DM_table)

capture.output(specnumber(NA_DM_table), file= "North_America_Observed_number_species.txt")

North_America_species_accumulation_curve_country <-specaccum(NA_DM_table_country)

capture.output(specaccum(NA_DM_table_country), file="North_America_species_accumulation_curve_country.txt")

North_America_species_accumulation_curve_collector_random <-specaccum(NA_DM_collector_species_table,
															method="random",
															permutations=1000)

capture.output(specaccum(NA_DM_collector_species_table, method="random",
														permutations=1000),
				file="North_America_species_accumulation_curve_collector_random.txt")

pdf("North_America_species_accumulation_curve_collector_plot.pdf")
plot(North_America_species_accumulation_curve_collector_random,
	ci.type="poly",
	col="#08519c",
	lwd=3,
	ci.lty=0,
	ci.col="#74a9cf")
dev.off()

tiff("North_America_species_accumulation_curve_collector_plot.tiff",
width= 7.4*ppi,
height=4.7*ppi,
res = ppi,
compression= "lzw")
plot(North_America_species_accumulation_curve_collector_random,
	ci.type="poly",
	col="#08519c",
	lwd=3,
	ci.lty=0,
	ci.col="#74a9cf")
dev.off()

#Species accumulation curve for United States of America

United_States_Observed_number_species <-specnumber(US_DM_table)

capture.output(specnumber(US_DM_table), file= "United_States_Observed_number_species.txt")

United_States_Observed_number_species_state <- specnumber(US_DM_state_species_table)

write.csv(United_States_Observed_number_species_state,
			file="United_States_Observed_number_species_state.csv")

United_States_Observed_number_species_collector <-specnumber(US_DM_collector_species_table)

write.csv(United_States_Observed_number_species_collector,
			file="United_States_Observed_number_species_collector.csv")

United_states_species_accumulation_curve_collector <-specaccum(US_DM_collector_species_table,
												method="random",
												permutations=1000)

capture.output(specaccum(US_DM_collector_species_table,
						method="random", permutations=1000),
				file="United_states_species_accumulation_curve_collector_random.txt")

pdf("Sup_Fig_1_United_states_species_accumulation_curve_collector_plot.pdf")
plot(United_states_species_accumulation_curve_collector,
	ci.type="poly",
	col="#08519c",
	lwd=3,
	ci.lty=0,
	ci.col="#74a9cf")
dev.off()

tiff("Sup_Fig_1_United_states_species_accumulation_curve_collector_plot.tiff",
width= 7.4*ppi,
height=4.7*ppi,
res = ppi,
compression= "lzw")
plot(United_states_species_accumulation_curve_collector,
	ci.type="poly",
	col="#08519c",
	lwd=3,
	ci.lty=0,
	ci.col="#74a9cf")
dev.off()


#Rarefaction curve by US State
rarecol <-brewer.pal(n=10, name="PuOr")

Number_species_by_state <-specnumber(US_DM_state_species_table)
capture.output(specnumber(US_DM_state_species_table),
	file="United_states_Observed_number_species_per_state.txt")

DM_US_state_raremax <-min(rowSums(US_DM_state_species_table))

DM_US_state_rare <-rarefy(US_DM_state_species_table, DM_US_state_raremax)

pdf("Sup_Fig_2_DM_US_state_rarecurve_plot_without_sample.pdf")
print(rarecurve(US_DM_state_species_table,
	col= rarecol,
	lwd = 2,
	cex=0.6,
	label=FALSE))
dev.off()

tiff("Sup_Fig_2_DM_US_state_rarecurve_plot_without_sample.tiff",
width= 7.4*ppi,
height=4.7*ppi,
res = ppi,
compression= "lzw")
print(rarecurve(US_DM_state_species_table,
	col= rarecol,
	lwd = 2,
	cex=0.6,
	label=FALSE))
dev.off()

United_States_downy_mildew_subset_state <-subset(United_States_downy_mildew, state_province
											%in% c("Wisconsin",
													"New York",
													"Iowa",
													"Illinois",
													"California",
													"Alabama"))

US_DM_sub_state_species_table <- table(United_States_downy_mildew_subset_state$state_province,
								United_States_downy_mildew_subset_state$cur_scientific_name)
pdf("Sup_Fig_2_DM_US_state_subset_rarecurve_plot_without_sample_with_labels.pdf")
print(rarecurve(US_DM_sub_state_species_table,
	col= "black",
	lwd = 2,
	cex=0.6,
	label=TRUE)
)
dev.off()

tiff("Sup_Fig_2_DM_US_state_subset_rarecurve_plot_without_sample_with_labels.tiff",
width= 7.4*ppi,
height=4.7*ppi,
res = ppi,
compression= "lzw")
print(rarecurve(US_DM_sub_state_species_table,
	col= "black",
	lwd = 2,
	cex=0.6,
	label=TRUE)
)
dev.off()

#Collector supplementals
#J J Davis
JJ_Davis_state <-ggplot(JJ_Davis_USA_DM,
				aes(x=reorder(state_province,
				desc(state_province)),
				fill=cur_genus)) + 
geom_bar() + 
scale_fill_manual(name= NULL, values= c("#a50026",
							"#d73027",
							"#f46d43",
							"#fdae61",
							"#fee090",
							"#92c5de",
							"#4393c3",
							"#2166ac",
							"#053061",
							"#2d004b")) +
scale_y_continuous(expand=expansion(mult=c(0, .1))) +
theme(axis.text.x=element_text(size=10, color="black")) +
theme(axis.text.y=element_text(size=10, color="black")) +
labs(x="State of collection", y="Number of collections") +
#theme(legend.position="none") +
theme(axis.title.x=element_text(size=10, color="black")) +
theme(axis.title.y=element_text(size=10, color="black")) +
theme(legend.text=element_text(size=10, color="black")) +
guides(fill=guide_legend(title="Downy mildew genera")) +
theme(legend.title=element_text(size=10))+
theme(legend.text=element_text(size=10, face="italic", color="black")) +
coord_flip()

JJ_Davis_cult_nat <-ggplot(
JJ_Davis_USA_DM,
aes(x=cultivated_native, fill=cur_genus)) + 
geom_bar() + 
scale_fill_manual(name= NULL, values= c("#a50026",
							"#d73027",
							"#f46d43",
							"#fdae61",
							"#fee090",
							"#92c5de",
							"#4393c3",
							"#2166ac",
							"#053061",
							"#2d004b")) +
theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=10, color="black")) +
theme(axis.text.y=element_text(size=10, color="black")) +
labs(x="Status of host plant", y="Number of collections") +
theme(axis.title.x=element_text(size=10, color="black")) +
theme(axis.title.y=element_text(size=10, color="black")) +
scale_x_discrete(breaks=c("cultivated-native", "cultivated-non-native", "wild-native", "wild-non-native"),
		     labels=c("Cultivated, native", "Cultivated, non-native", "Wild, native", "Wild, non-native")) +
guides(fill=guide_legend(title="Downy mildew genera")) +
theme(legend.title=element_text(size=10))+
theme(legend.text=element_text(size=10, face="italic", color="black"))

JJ_Davis_patchwork <- JJ_Davis_state / JJ_Davis_cult_nat

pdf("Sup_Fig_4_JJ_Davis_collections_with_legend.pdf")
print(JJ_Davis_patchwork +
plot_annotation(tag_levels= 'A', title="The collections of J. J. Davis") +
plot_layout(guides='collect'))
dev.off()

tiff("Sup_Fig_4_JJ_Davis_collections_with_legend.tiff",
width= 7.4*ppi,
height=4.7*ppi,
res = ppi,
compression= "lzw")
print(JJ_Davis_patchwork +
plot_annotation(tag_levels= 'A', title="The collections of J. J. Davis") +
plot_layout(guides='collect'))
dev.off()


#A B Seymour
AB_Seymour_state <-ggplot(AB_Seymour_USA_DM,
				aes(x=reorder(state_province,
				desc(state_province)),
				fill=cur_genus)) + 
geom_bar() + 
scale_fill_manual(name= NULL, values= c("#a50026",
							"#f46d43",
							"#fdae61",
							"#92c5de",
							"#4393c3",
							"#2166ac",
							"#053061")) +
#scale_y_continuous(expand=expansion(mult=c(0, .05))) +
theme(axis.text.x=element_text(size=10, color="black")) +
theme(axis.text.y=element_text(size=10, color="black")) +
labs(x="State of collection", y="Number of collections") +
theme(axis.title.x=element_text(size=10, color="black")) +
theme(axis.title.y=element_text(size=10, color="black")) +
theme(legend.position="none") +
theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=10, color="black")) +
coord_flip()

AB_Seymour_cult_nat <-ggplot(
AB_Seymour_USA_DM,
aes(x=cultivated_native, fill=cur_genus)) + 
geom_bar() + 
scale_fill_manual(name= NULL, values= c("#a50026",
							"#f46d43",
							"#fdae61",
							"#92c5de",
							"#4393c3",
							"#2166ac",
							"#053061")) +
scale_x_discrete(breaks=c("cultivated-native",
				"cultivated-non-native",
				"wild-native",
				"wild-non-native"),
		     labels=c("Cultivated, native",
				"Cultivated, non-native",
				"Wild, native",
				"Wild, non-native")) +
guides(fill=guide_legend(title="Downy mildew genera")) +
theme(legend.title=element_text(size=10))+
theme(legend.text=element_text(size=10, face="italic", color="black"))+
theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=10, color="black")) +
theme(axis.text.y=element_text(size=10, color="black")) +
theme(axis.title.x=element_text(size=10, color="black")) +
theme(axis.title.y=element_text(size=10, color="black")) +
labs(x="Status of host plant", y="Number of collections") +
theme(legend.text=element_text(size=10, face="italic", color="black"))

AB_Seymour_patchwork <- AB_Seymour_state / AB_Seymour_cult_nat

pdf("Sup_Fig_5_AB_Seymour_collections_with_legend.pdf")
print(AB_Seymour_patchwork +
plot_annotation(tag_levels= 'A', title="The collections of A. B. Seymour") +
plot_layout(guides='collect')
)
dev.off()

tiff("Sup_Fig_5_AB_Seymour_collections_with_legend.tiff",
width= 7.4*ppi,
height=4.7*ppi,
res = ppi,
compression= "lzw")
print(AB_Seymour_patchwork +
plot_annotation(tag_levels= 'A', title="The collections of A. B. Seymour") +
plot_layout(guides='collect')
)
dev.off()



#H C Greene
HC_Greene_state <-ggplot(HC_Greene_USA_DM,
				aes(x=reorder(state_province,
				desc(state_province)),
				fill=cur_genus)) + 
geom_bar() + 
scale_fill_manual(name= NULL, values= c("#a50026",
							"#f46d43",
							"#fdae61",
							"#fee090",
							"#92c5de",
							"#4393c3",
							"#2166ac",
							"#053061",
							"#2d004b")) +
scale_y_continuous(expand=expansion(mult=c(0, .05))) +
theme(axis.text.x=element_text(size=10, color="black")) +
theme(axis.text.y=element_text(size=10, color="black")) +
theme(legend.position="none") +
theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=10, color="black")) +
labs(x="State of collection", y="Number of collections") +
theme(axis.title.x=element_text(size=10, color="black")) +
theme(axis.title.y=element_text(size=10, color="black")) +
coord_flip()

HC_Greene_cult_nat <-ggplot(
HC_Greene_USA_DM,
aes(x=cultivated_native, fill=cur_genus)) + 
geom_bar() + 
scale_fill_manual(name= NULL, values= c("#a50026",
							"#f46d43",
							"#fdae61",
							"#fee090",
							"#92c5de",
							"#4393c3",
							"#2166ac",
							"#053061",
							"#2d004b")) +
scale_x_discrete(breaks=c("cultivated-native",
				"cultivated-non-native",
				"wild-native",
				"wild-non-native"),
		     labels=c("Cultivated, native",
				"Cultivated, non-native",
				"Wild, native",
				"Wild, non-native")) +
guides(fill=guide_legend(title="Downy mildew genera")) +
theme(legend.title=element_text(size=10))+
theme(legend.text=element_text(size=10, face="italic", color="black")) +
theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=10, color="black")) +
theme(axis.text.y=element_text(size=10, color="black")) +
theme(axis.title.x=element_text(size=10, color="black")) +
theme(axis.title.y=element_text(size=10, color="black")) +
labs(x="Status of host plant", y="Number of collections") +
theme(legend.text=element_text(size=10, face="italic", color="black"))

HC_Greene_patchwork <- HC_Greene_state / HC_Greene_cult_nat

pdf("Sup_Fig_6_HC_Greene_collections_with_legend.pdf")
print(HC_Greene_patchwork +
plot_annotation(tag_levels= 'A', title="The collections of H. C. Greene") +
plot_layout(guides='collect')
)
dev.off()

tiff("Sup_Fig_6_HC_Greene_collections_with_legend.tiff",
width= 7.4*ppi,
height=4.7*ppi,
res = ppi,
compression= "lzw")
print(HC_Greene_patchwork +
plot_annotation(tag_levels= 'A', title="The collections of H. C. Greene") +
plot_layout(guides='collect')
)
dev.off()

#C T Rogerson

CT_Rogerson_state <-ggplot(CT_Rogerson_USA_DM,
					aes(x=reorder(state_province,
					desc(state_province)),
					fill=cur_genus)) + 
geom_bar() + 
scale_fill_manual(name= NULL, values= c("#a50026",
							"#f46d43",
							"#fdae61",
							"#fee090",
							"#92c5de",
							"#4393c3",
							"#053061",
							"#2d004b")) +
scale_y_continuous(expand=expansion(mult=c(0, .05))) +
theme(axis.text.x=element_text(size=10, color="black")) +
theme(axis.text.y=element_text(size=10, color="black")) +
theme(legend.position="none") +
theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=10, color="black")) +
labs(x="State of collection", y="Number of collections") +
theme(axis.title.x=element_text(size=10, color="black")) +
theme(axis.title.y=element_text(size=10, color="black")) +
coord_flip()

CT_Rogerson_cult_nat <-ggplot(
CT_Rogerson_USA_DM,
aes(x=cultivated_native, fill=cur_genus)) + 
geom_bar() + 
scale_fill_manual(name= NULL, values= c("#a50026",
							"#f46d43",
							"#fdae61",
							"#fee090",
							"#92c5de",
							"#4393c3",
							"#053061",
							"#2d004b")) +
scale_x_discrete(breaks=c("cultivated-native",
				"cultivated-non-native",
				"wild-native",
				"wild-non-native"),
		     labels=c("Cultivated, native",
				"Cultivated, non-native",
				"Wild, native",
				"Wild, non-native")) +
guides(fill=guide_legend(title="Downy mildew genera")) +
theme(legend.title=element_text(size=10))+
theme(legend.text=element_text(size=10, face="italic", color="black")) +
theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=10, color="black")) +
theme(axis.text.y=element_text(size=10, color="black")) +
theme(axis.title.x=element_text(size=10, color="black")) +
theme(axis.title.y=element_text(size=10, color="black")) +
labs(x="Status of host plant", y="Number of collections") +
theme(legend.text=element_text(size=10, face="italic", color="black"))

CT_Rogerson_patchwork <- CT_Rogerson_state / CT_Rogerson_cult_nat

pdf("Sup_Fig_7_CT_Rogerson_collections_with_legend.pdf")
print(CT_Rogerson_patchwork +
plot_annotation(tag_levels= 'A', title="The collections of C. T. Rogerson") +
plot_layout(guides='collect')
)
dev.off()

tiff("Sup_Fig_7_CT_Rogerson_collections_with_legend.tiff",
width= 7.4*ppi,
height=4.7*ppi,
res = ppi,
compression= "lzw")
print(CT_Rogerson_patchwork +
plot_annotation(tag_levels= 'A', title="The collections of C. T. Rogerson") +
plot_layout(guides='collect')
)
dev.off()

#G P Clinton

GP_Clinton_state <-ggplot(GP_Clinton_USA_DM,
					aes(x=reorder(state_province,
					desc(state_province)),
					fill=cur_genus)) + 
geom_bar() + 
scale_fill_manual(name= NULL, values= c("#a50026",
							"#f46d43",
							"#fdae61",
							"#92c5de",
							"#4393c3",
							"#2166ac",
							"#053061",
							"#2d004b")) +
scale_y_continuous(expand=expansion(mult=c(0, .05))) +
theme(axis.text.x=element_text(size=10, color="black")) +
theme(axis.text.y=element_text(size=10, color="black")) +
theme(legend.position="none") +
theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=10, color="black")) +
labs(x="State of collection", y="Number of collections") +
theme(axis.title.x=element_text(size=10, color="black")) +
theme(axis.title.y=element_text(size=10, color="black")) +
coord_flip()

GP_Clinton_cult_nat <-ggplot(
GP_Clinton_USA_DM,
aes(x=cultivated_native, fill=cur_genus)) + 
geom_bar() + 
scale_fill_manual(name= NULL, values= c("#a50026",
							"#f46d43",
							"#fdae61",
							"#92c5de",
							"#4393c3",
							"#2166ac",
							"#053061",
							"#2d004b")) +
scale_x_discrete(breaks=c("cultivated-native",
				"cultivated-non-native",
				"wild-native",
				"wild-non-native"),
		     labels=c("Cultivated, native",
				"Cultivated, non-native",
				"Wild, native",
				"Wild, non-native")) +
guides(fill=guide_legend(title="Downy mildew genera")) +
theme(legend.title=element_text(size=10))+
theme(legend.text=element_text(size=10, face="italic", color="black")) +
theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=10, color="black")) +
theme(axis.text.y=element_text(size=10, color="black")) +
theme(axis.title.x=element_text(size=10, color="black")) +
theme(axis.title.y=element_text(size=10, color="black")) +
labs(x="Status of host plant", y="Number of collections") +
theme(legend.text=element_text(size=10, face="italic", color="black"))

GP_Clinton_patchwork <- GP_Clinton_state / GP_Clinton_cult_nat

pdf("Sup_Fig_8_GP_Clinton_collections_with_legend.pdf")
print(GP_Clinton_patchwork +
plot_annotation(tag_levels= 'A', title="The collections of G. P. Clinton") +
plot_layout(guides='collect')
)
dev.off()

tiff("Sup_Fig_8_GP_Clinton_collections_with_legend.tiff",
width= 7.4*ppi,
height=4.7*ppi,
res = ppi,
compression= "lzw")
print(GP_Clinton_patchwork +
plot_annotation(tag_levels= 'A', title="The collections of G. P. Clinton") +
plot_layout(guides='collect')
)
dev.off()


#L H Pammel

LH_Pammel_state <-ggplot(LH_Pammel_USA_DM,
					aes(x=reorder(state_province,
					desc(state_province)),
					fill=cur_genus)) + 
geom_bar() + 
scale_fill_manual(name= NULL, values= c("#a50026",
							"#d73027",
							"#f46d43",
							"#fdae61",
							"#fee090",
							"#92c5de",
							"#4393c3",
							"#2166ac",
							"#053061",
							"#2d004b")) +
scale_y_continuous(expand=expansion(mult=c(0, .05))) +
theme(axis.text.x=element_text(size=10, color="black")) +
theme(axis.text.y=element_text(size=10, color="black")) +
theme(legend.position="none") +
theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=10, color="black")) +
labs(x="State of collection", y="Number of collections") +
theme(axis.title.x=element_text(size=10, color="black")) +
theme(axis.title.y=element_text(size=10, color="black")) +
coord_flip()


LH_Pammel_cult_nat <-ggplot(
LH_Pammel_USA_DM,
aes(x=cultivated_native, fill=cur_genus)) + 
geom_bar() + 
scale_fill_manual(name= NULL, values= c("#a50026",
							"#d73027",
							"#f46d43",
							"#fdae61",
							"#fee090",
							"#92c5de",
							"#4393c3",
							"#2166ac",
							"#053061",
							"#2d004b")) +
scale_x_discrete(breaks=c("cultivated-native",
				"cultivated-non-native",
				"wild-native",
				"wild-non-native"),
		     labels=c("Cultivated, native",
				"Cultivated, non-native",
				"Wild, native",
				"Wild, non-native")) +
guides(fill=guide_legend(title="Downy mildew genera")) +
theme(legend.title=element_text(size=10))+
theme(legend.text=element_text(size=10, face="italic", color="black")) +
theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=10, color="black")) +
theme(axis.text.y=element_text(size=10, color="black")) +
theme(axis.title.x=element_text(size=10, color="black")) +
theme(axis.title.y=element_text(size=10, color="black")) +
labs(x="Status of host plant", y="Number of collections") +
theme(legend.text=element_text(size=10, face="italic", color="black"))

LH_Pammel_patchwork <- LH_Pammel_state / LH_Pammel_cult_nat

pdf("Sup_Fig_9_LH_Pammel_collections_with_legend.pdf")
print(LH_Pammel_patchwork +
plot_annotation(tag_levels= 'A', title="The collections of L. H. Pammel") +
plot_layout(guides='collect')
)

tiff("Sup_Fig_9_LH_Pammel_collections_with_legend.tiff",
width= 7.4*ppi,
height=4.7*ppi,
res = ppi,
compression= "lzw")
print(LH_Pammel_patchwork +
plot_annotation(tag_levels= 'A', title="The collections of L. H. Pammel") +
plot_layout(guides='collect')
)
dev.off()



#Top collected species supplementals
#Make new subsets since old subsets had records removed
#due to a lack of lat, long, which is irrelevant to
#these graphs
Plasmopara_viticola <-subset(United_States_downy_mildew, cur_scientific_name=="Plasmopara viticola")
Bremia_lactucae <-subset(United_States_downy_mildew, cur_scientific_name=="Bremia lactucae")
Plasmopara_halstedii <-subset(United_States_downy_mildew, cur_scientific_name=="Plasmopara halstedii")
Pseudoperonospora_cubensis <-subset(United_States_downy_mildew, cur_scientific_name=="Pseudoperonospora cubensis")
Basidiophora_entospora <-subset(United_States_downy_mildew, cur_scientific_name=="Basidiophora entospora")

#Plasmopara viticola
Pviticola_state <- ggplot(Plasmopara_viticola,
						aes(x=(x=reorder(state_province,
						desc(state_province))))) +
geom_bar(fill="#4393c3") +
scale_fill_manual(name=NULL, values="#4393c3") +
scale_y_continuous(expand=expansion(mult=c(0, .1))) +
theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=10, color="black")) +
theme(axis.text.y=element_text(size=10, color="black")) +
labs(x="State of collection", y="Number of collections") +
theme(axis.title.x=element_text(size=10, color="black")) +
theme(axis.title.y=element_text(size=10, color="black")) +
coord_flip()

pdf("Sup_Fig_9_Pviticola_by_state.pdf")
print(Pviticola_state)
dev.off()

tiff("Sup_Fig_9_Pviticola_by_state.tiff",
width= 7.4*ppi,
height=4.7*ppi,
res = ppi,
compression= "lzw")
print(Pviticola_state)
dev.off()

Pviticola_host <- ggplot(Plasmopara_viticola,
						aes(x=(x=reorder(cur_host,
						desc(cur_host))))) +
geom_bar(fill="#4393c3") +
scale_fill_manual(name=NULL, values="#4393c3") +
scale_y_continuous(expand=expansion(mult=c(0, .1))) +
theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=10, color="black")) +
theme(axis.text.y=element_text(size=10, face="italic", color="black")) +
labs(x="Plant host species", y="Number of collections") +
theme(axis.title.x=element_text(size=10, color="black")) +
theme(axis.title.y=element_text(size=10, color="black")) +
coord_flip()

pdf("Sup_Fig_10_Pviticola_by_host.pdf")
print(Pviticola_host)
dev.off()

tiff("Sup_Fig_10_Pviticola_by_host.tiff",
width= 7.4*ppi,
height=4.7*ppi,
res = ppi,
compression= "lzw")
print(Pviticola_host)
dev.off()

#Bremia_lac

Blactucae_state <- ggplot(Bremia_lactucae,
							aes(x=(x=reorder(state_province,
								desc(state_province))))) +
geom_bar(fill="#f46d43") +
scale_y_continuous(expand=expansion(mult=c(0, .1))) +
theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=10, color="black")) +
theme(axis.text.y=element_text(size=10, color="black")) +
labs(x="State of collection", y="Number of collections") +
theme(axis.title.x=element_text(size=10, color="black")) +
theme(axis.title.y=element_text(size=10, color="black")) +
coord_flip()

pdf("Sup_Fig_11_Blactucae_by_state.pdf")
print(Blactucae_state)
dev.off()

tiff("Sup_Fig_11_Blactucae_by_state.tiff",
width= 7.4*ppi,
height=4.7*ppi,
res = ppi,
compression= "lzw")
print(Blactucae_state)
dev.off()


Blactucae_host <- ggplot(Bremia_lactucae,
							aes(x=(x=reorder(cur_host,
								desc(cur_host))))) +
geom_bar(fill="#f46d43") +
scale_y_continuous(expand=expansion(mult=c(0, .1))) +
theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=10, color="black")) +
theme(axis.text.y=element_text(size=10, face="italic", color="black")) +
labs(x="Plant host species", y="Number of collections") +
theme(axis.title.x=element_text(size=10, color="black")) +
theme(axis.title.y=element_text(size=10, color="black")) +
coord_flip()

pdf("Sup_Fig_12_Blactucae_by_host.pdf")
print(Blactucae_host)
dev.off()

tiff("Sup_Fig_12_Blactucae_by_host.tiff",
width= 7.4*ppi,
height=4.7*ppi,
res = ppi,
compression= "lzw")
print(Blactucae_host)
dev.off()

#Plasmopara halstedii

Phalstedii_state <- ggplot(Plasmopara_halstedii,
								aes(x=(x=reorder(state_province,
									desc(state_province))))) +
geom_bar(fill="#4393c3") +
scale_y_continuous(expand=expansion(mult=c(0, .1))) +
theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=10, color="black")) +
theme(axis.text.y=element_text(size=10, color="black")) +
labs(x="State of collection", y="Number of collections") +
theme(axis.title.x=element_text(size=10, color="black")) +
theme(axis.title.y=element_text(size=10, color="black")) +
coord_flip()

pdf("Sup_Fig_13_Phalstedii_by_state.pdf")
print(Phalstedii_state)
dev.off()

tiff("Sup_Fig_13_Phalstedii_by_state.tiff",
width= 7.4*ppi,
height=4.7*ppi,
res = ppi,
compression= "lzw")
print(Phalstedii_state)
dev.off()

Phalstedii_host <- ggplot(Plasmopara_halstedii,
								aes(x=(x=reorder(cur_host,
									desc(cur_host))))) +
geom_bar(fill="#4393c3") +
scale_y_continuous(expand=expansion(mult=c(0, .1))) +
theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=10, color="black")) +
theme(axis.text.y=element_text(size=10, face="italic", color="black")) +
labs(x="Plant host species", y="Number of collections") +
theme(axis.title.x=element_text(size=10, color="black")) +
theme(axis.title.y=element_text(size=10, color="black")) +
coord_flip()

pdf("Sup_Fig_14_Phalstedii_by_host.pdf")
print(Phalstedii_host)
dev.off()

tiff("Sup_Fig_14_Phalstedii_by_host.tiff",
width= 7.4*ppi,
height=4.7*ppi,
res = ppi,
compression= "lzw")
print(Phalstedii_host)
dev.off()

#Pseudoperonospora cubensis

Pcubensis_state <- ggplot(Pseudoperonospora_cubensis,
								aes(x=(x=reorder(state_province,
									desc(state_province))))) +
geom_bar(fill="#053061") +
scale_y_continuous(expand=expansion(mult=c(0, .1))) +
theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=10, color="black")) +
theme(axis.text.y=element_text(size=10, color="black")) +
labs(x="State of collection", y="Number of collections") +
theme(axis.title.x=element_text(size=10, color="black")) +
theme(axis.title.y=element_text(size=10, color="black")) +
coord_flip()

pdf("Sup_Fig_15_Pcubensis_by_state.pdf")
print(Pcubensis_state)
dev.off()

tiff("Sup_Fig_15_Pcubensis_by_state.tiff",
width= 7.4*ppi,
height=4.7*ppi,
res = ppi,
compression= "lzw")
print(Pcubensis_state)
dev.off()

Pcubensis_host <- ggplot(Pseudoperonospora_cubensis,
								aes(x=(x=reorder(cur_host,
									desc(cur_host))))) +
geom_bar(fill="#053061") +
scale_y_continuous(expand=expansion(mult=c(0, .1))) +
theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=10, color="black")) +
theme(axis.text.y=element_text(size=10, face="italic", color="black")) +
labs(x="Plant host species", y="Number of collections") +
theme(axis.title.x=element_text(size=10, color="black")) +
theme(axis.title.y=element_text(size=10, color="black")) +
coord_flip()

pdf("Sup_Fig_16_Pcubensis_by_host.pdf")
print(Pcubensis_host)
dev.off()

tiff("Sup_Fig_16_Pcubensis_by_host.tiff",
width= 7.4*ppi,
height=4.7*ppi,
res = ppi,
compression= "lzw")
print(Pcubensis_host)
dev.off()

#Distribution map & others for Hyaloperonospora sp

United_States_downy_mildew_map <- subset(United_States_downy_mildew_lat_long_clean,
						!cur_scientific_name=="Peronospora sp" &
						!cur_scientific_name=="Plasmopara sp" &
						!cur_scientific_name=="Hyaloperonospora sp")
						

Hyaloperonospora_sp <-subset(United_States_downy_mildew_lat_long_clean,
				cur_scientific_name=="Hyaloperonospora sp")
col <-c("Hyaloperonospora sp" = "#fdae61")

Distribution_undescribed_Hyaloperonospora_species <-ggplot(data=world)+
geom_sf(fill="white", color="black") +
geom_sf(data=states, fill=NA) +
geom_point(data=United_States_downy_mildew_map,
		aes(x=decimal_longitude,
			y=decimal_latitude),
		size=2,
		shape=21,
		color="black",
		fill="black") +
geom_point(data=Hyaloperonospora_sp,
		aes(x=decimal_longitude,
			y=decimal_latitude,
			color="Hyaloperonospora sp",
			fill="Hyaloperonospora sp"),
		size=2,
		shape=21) +
labs(x=NULL, y=NULL, color=NULL, fill=NULL) +
guides(color=FALSE) +
scale_color_manual(values=col) +
scale_fill_manual(values=col) +
theme(legend.position="bottom", legend.text=element_text(size=12, color="black")) +
theme(axis.text.x=element_text(size=12, color="black")) +
theme(axis.text.y=element_text(size=12, color="black")) +
coord_sf(xlim=c(-125, -65), ylim=c(25, 49))

pdf("Distribution_undescribed_Hyaloperonospora_species.pdf")
print(Distribution_undescribed_Hyaloperonospora_species)
dev.off()

tiff("Distribution_undescribed_Hyaloperonospora_species.tiff",
width= 7.4*ppi,
height=9.4*ppi,
res = ppi, 
compression= "lzw")
print(Distribution_undescribed_Hyaloperonospora_species)
dev.off()


Hyaloperonospora_sp <-subset(United_States_downy_mildew, cur_scientific_name=="Hyaloperonospora sp")
write.csv(Hyaloperonospora_sp, file="Hyaloperonospora_sp.csv")

Plot_hyaloperonospora_by_state <- ggplot(Hyaloperonospora_sp,
							aes(x=(x=reorder(state_province,
							desc(state_province))))) +
geom_bar(fill="#fdae61") +
scale_y_continuous(expand=expansion(mult=c(0, .1))) +
#theme(axis.text.x=element_text(size=12)) +
theme(axis.text.y=element_text(size=12)) +
labs(x=NULL, y=NULL) +
coord_flip()

pdf("Plot_Hyaloperonospora_sp_binned_by_state.pdf")
print(Plot_hyaloperonospora_by_state)
dev.off()

tiff("Plot_Hyaloperonospora_sp_binned_by_state.tiff",
width= 7.4*ppi,
height=4.7*ppi,
res = ppi,
compression= "lzw")
print(Plot_hyaloperonospora_by_state)
dev.off()

Plot_Hyaloperonospora_by_cur_host <- ggplot(Hyaloperonospora_sp,
						aes(x=(x=reorder(cur_host,
						desc(cur_host))))) +
geom_bar(fill="#4393c3") +
scale_fill_manual(name=NULL, values="#4393c3") +
scale_y_continuous(expand=expansion(mult=c(0, .1))) +
theme(axis.text.x=element_text(size=12)) +
theme(axis.text.y=element_text(size=10, face="italic")) +
labs(x=NULL, y=NULL) +
coord_flip()

pdf("Plot_Hyaloperonospora_sp_by_cur_host.pdf")
print(Plot_Hyaloperonospora_by_cur_host)
dev.off()

tiff("Plot_Hyaloperonospora_sp_by_cur_host.tiff",
width= 7.4*ppi,
height=4.7*ppi,
res = ppi,
compression= "lzw")
print(Plot_Hyaloperonospora_by_cur_host)
dev.off()