library(dplyr)

source("utils.r")
source("labels.r")

# Load the data
# MAKE SURE TO CREATE A COPY OF THE FILES FIRST
typhoid <- "Typhoid Fever (2011 - 2018).xlsx"
abd <- "Acute Bloody Diarrhea (2011 - 2018).xlsx"
geodata <- read.csv("geodata/baguio_city_geodata.csv")


#### Preprocessing ####
typhoid11 <- import_year(typhoid, "2011")
typhoid12 <- import_year(typhoid, "2012")
typhoid13 <- import_year(typhoid, "2013")
typhoid14 <- import_year(typhoid, "2014")
typhoid15 <- import_year(typhoid, "2015")
typhoid16 <- import_year(typhoid, "2016")
typhoid17 <- import_year(typhoid, "2017")
typhoid18 <- import_year(typhoid, "2018")

abd11 <- import_year(abd, "2011")
abd12 <- import_year(abd, "2012")
abd13 <- import_year(abd, "2013")
abd14 <- import_year(abd, "2014")
abd15 <- import_year(abd, "2015")
abd16 <- import_year(abd, "2016")
abd17 <- import_year(abd, "2017")
abd18 <- import_year(abd, "2018")

## FIXME: Convert numeric into Date
typhoid11[, 3] <- as.Date(typhoid11[, 3], origin = "1899-12-30")
typhoid11[, 4] <- as.Date(typhoid11[, 4], origin = "1899-12-30")

cleaned_typhoid11 <- rename_barangays(typhoid11$Barangay, patterns, replacements)

df_cleaned_typhoid11 <- typhoid11[
    cleaned_typhoid11 %in% toupper(geodata$ADM4_EN),
]

df_cleaned_typhoid11 <- df_cleaned_typhoid11[!is.na(df_cleaned_typhoid11$Barangay),]

not_typhoid11 <- typhoid11[
    !(cleaned_typhoid11 %in% toupper(geodata$ADM4_EN)),
]

View(not_typhoid11)
View(df_cleaned_typhoid11)


#### Typhoid 2011 ####
column_names <- c("MorbidityWeek", short_brgy_names)
init_values <- numeric(130)

typhoid11_df_cases <- count_cases(column_names, init_values, short_brgy_names, df_cleaned_typhoid11)
View(typhoid11_df_cases)

B=typhoid11$Barangay
SS=typhoid11$CASECLASS
MW=typhoid11$MorbidityWeek

Imelda = B=="IMELDA VILLAGE"
BakakengCentral = B=="BAKAKENG CENTRAL"
ABCR = B=="A. BONIFACIO-CAGUIOA-RIMANDO (ABCR)"
Ambiong = B=="AMBIONG"
BakakengNorth = B=="BAKAKENG NORTH"
Asin = B=="ASIN"
Balsigan = B=="BALSIGAN"
Bayan = B=="BAYAN PARK WEST (BAYAN PARK)"
BGH=B=="BGH COMPOUND"
Brookside = B=="BROOKSIDE"
Camdas = B=="CAMDAS SUBDIVISION"
c7=B=="CAMP 7"
c8=B=="CAMP 8"
ca=B=="CAMP ALLEN"
cf=B=="CAMPO FILIPINO"
ccv=B=="COUNTRY CLUB VILLAGE"
DPS=B=="DPS AREA"
enghill=B=="ENGINEER'S HILL"
fvill=B=="FAIRVIEW VILLAGE"
GLuna=B=="GENERAL LUNA, UPPER"
Gib=B=="GIBRALTAR"
GuisadC=B=="GUISAD CENTRAL"
GuisadS=B=="GUISAD SORONG"
HHL=B=="HAPPY HOMES (HAPPY HOMES-LUCBAN)"
kias=B=="KIAS"
LLoak=B=="LIWANAG-LOAKAN"
LoakP=B=="LOAKAN PROPER"
LOPJ=B=="LOPEZ JAENA"
lucnab=B=="LUCNAB"
MR=B=="MANUEL A. ROXAS"
MCutOff=B=="MILITARY CUT-OFF"
MRR=B=="MRR-QUEEN OF PEACE"
PadreZ=B=="PADRE ZAMORA"
Pinget=B=="PINGET"
PPP=B=="PINSAO PILOT PROJECT"
PisaoP=B=="PINSAO PROPER"
Poliwes=B=="POLIWES"
QHP=B=="QUEZON HILL PROPER"
QHL=B=="QUIRINO HILL, LOWER"
QHM=B=="QUIRINO HILL, MIDDLE"
salud=B=="SALUD MITRA"
sanvic=B=="SAN VICENTE"
SanCamp=B=="SANITARY CAMP, NORTH"
STE=B=="SANTA ESCOLASTICA"
STP=B=="SANTO TOMAS PROPER"&B=="STO. TOMAS PROPER"
STsa=B=="SANTO TOMAS SCHOOL AREA"
tranc=B=="TRANCOVILLE"
birac=B=="BIRAC"
AL=B=="APUGAN-LOAKAN"
AZKCO=B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")
ATAB=B==toupper("Alfonso Tabora")
AB=B==toupper("Andres Bonifacio (Lower Bokawkan)")
ATOK=B==toupper("Atok Trail")
auroraP=B==toupper("Aurora Hill Proper (Malvar-Sgt. Floresca)")
auroraN=B==toupper("Aurora Hill, North Central")
auroraS=B==toupper("Aurora Hill, South Central")
BL=B==toupper("Bagong Lipunan (Market Area)")
Bal=B==toupper("Bal-Marcoville (Marcoville)")
BPE=B==toupper("Bayan Park East")
BPV=B==toupper("Bayan Park Village")
Brookspoint=B==toupper("Brookspoint")
CabinetHill=B==toupper("Cabinet Hill-Teacher's Camp")
CityCC=B==toupper("City Camp Central")
CityCP=B==toupper("City Camp Proper")
DagisanL=B==toupper("Dagsian, Lower")
Cres=B==toupper("Cresencia Village")
DagisanU=B==toupper("Dagsian, Upper")
Dizon=B==toupper("Dizon Subdivision")
Domin=B==toupper("Dominican Hill-Mirador")
Dontogan=B=="DONTOGAN"
DPS=B=="DPS AREA"
Ferdinand=B==toupper("Ferdinand (Happy Homes-Campo Sioco)")
GabSi=B=="GABRIELLA SILANG"
GenE=B==toupper("General Emilio F. Aguinaldo (Quirino-Magsaysay, Lower)")
GenLL=B==toupper("General Luna, Lower")
GenLU=B==toupper("General Luna, Upper")
Greenwater=B==toupper("Greenwater Village")
HHol=B==toupper("Happy Hollow")
HHom=B==toupper("Happy Homes (Happy Homes-Lucban)")
HCC=B==toupper("Harrison-Claudio Carantes")
Hillside=B==toupper("Hillside")
HGhostE=B==toupper("Holy Ghost Extension")
HGhostP=B==toupper("Holy Ghost Proper")
Honeymoon=B==toupper("Honeymoon (Honeymoon-Holy Ghost)")
IMarcos=B==toupper("Imelda R. Marcos (La Salle)")
Imelda=B==toupper("Imelda Village")
Kabayanihan=B==toupper("Kabayanihan")
Kagitingan=B==toupper("Kagitingan")
KayangHill=B==toupper("Kayang-Hilltop")
KayangE=B==toupper("Kayang Extension")
LBK=B==toupper("Legarda-Burnham-Kisad")
LourdesE=B==toupper("Lourdes Subdivision Extension")
LourdesL=B==toupper("Lourdes Subdivision, Lower")
LourdesP=B==toupper("Lourdes Subdivision, Proper")
Lualhati=B==toupper("Lualhati")
MagsaysayPR=B==toupper("Magsaysay Private Road")
MagsaysayL=B==toupper("Magsaysay, Lower")
MagsaysayU=B==toupper("Magsaysay, Upper")
MalcolmSquarePerfectoJoseAbadSantos=B==toupper("Malcolm Square-Perfecto (Jose Abad Santos)")
MarketSubdivisionUpper=B==toupper("Market Subdivision, Upper")
MiddleQuezonHillSubdivsion=B==toupper("Middle Quezon Hill Subdivsiion(Quezon Hill Middle)")
MinesView=B==toupper("Mines View Park")
ModernSiteE=B==toupper("Modern Site, East")
ModernSiteW=B==toupper("Modern Site, West")
NewLuc=B==toupper("New Lucban")
Outlook=B==toupper("Outlook Drive")
Pacdal=B==toupper("Pacdal")
PadreB=B==toupper("Padre Burgos")
PadreZ=B==toupper("Padre Zamora")
PU=B==toupper("Palma-Urbano (Cariño-Palma)")
PhilAm=B==toupper("Phil-Am")
pucsusan=B==toupper("Pucsusan")
QHillU=B==toupper("Quezon Hill, Upper")
UpperQM=B==toupper("Quirino-Magsaysay, Upper (Upper QM)")
QHillE=B==toupper("Quirino Hill, East")
QHillW=B==toupper("Quirino Hill, West")
RizalMonumentArea=B==toupper("Rizal Monument Area")
RockQuarryLower=B==toupper("Rock Quarry, Lower")
RockQuarryMiddle=B==toupper("Rock Quarry, Middle")
RockQuarryUpper=B==toupper("Rock Quarry, Upper")
SanAntonioVillage=B==toupper("San Antonio Village")
SanLuisVillage=B==toupper("San Luis Village")
SanRoqueVillage=B==toupper("San Roque Village")
SanitaryCampSouth=B==toupper("Sanitary Camp, South")
SantoR=B==toupper("Santo Rosario")
ScoutB=B==toupper("Scout Barrio")
Session=B==toupper("Session Road Area")
Slaughter=B==toupper("Slaughter House Area (Santo Niño Slaughter)")
SLU=B==toupper("SLU-SVP Housing Village")
SouthDrive=B==toupper("South Drive")
TeoAl=B==toupper("Teodora Alonzo")
tranco=B==toupper("Trancoville")
victoria=B==toupper("Victoria Village")
STJos=B=="ST JOSEPH VILLAGE"

tdf11 <- data.frame(MorbidityWeek=1,
                  ABonifacioCaguioaRimandoABCR=0,
                  AbanaoZanduetaKayongChugumOtekAZKCO=0,
                  AlfonsoTabora=0,
                  Ambiong=0,
                  AndresBonifacioLowerBokawkan=0,
                  ApuganLoakan=0,
                  AsinRoad=0,
                  AtokTrail=0,
                  AuroraHillProperMalvarSgtFloresca=0,
                  AuroraHillNorthCentral=0,
                  AuroraHillSouthCentral=0,
                  BagongLipunanMarketArea=0,
                  BakakengCentral=0,
                  BakakengNorth=1,
                  BalMarcovilleMarcoville=0,
                  Balsigan=0,
                  BayanParkEast=0,
                  BayanParkVillage=0,
                  BayanParkWestBayanPark=0,
                  BGHCompound=0,
                  Brookside=0,
                  Brookspoint=0,
                  CabinetHillTeachersCamp=0,
                  CamdasSubdivision=0,
                  Camp7=0,
                  Camp8=0,
                  CampAllen=0,
                  CampoFilipino=0,
                  CityCampCentral=0,
                  CityCampProper=0,
                  CountryClubVillage=0,
                  CresenciaVillage=0,
                  DagisanLower=0,
                  DagisanUpper=0,
                  DizonSubdivision=0,
                  DominicanHillMirador=0,
                  Dontogan=0,
                  DPSArea=0,
                  EngineersHill=0,
                  FairviewVillage=0,
                  FerdinandHappyHomesCampoSioco=0,
                  FortdelPilar=0,
                  GabrielaSilang=0,
                  GeneralEmilioFAguinaldoQuirinoMagsaysayLower=0,
                  GeneralLunaLower=0,
                  GeneralLunaUpper=0,
                  Gibraltar=0,
                  GreenwaterVillage=0,
                  GuisadCentral=0,
                  GuisadSorong=0,
                  HappyHollow=0,
                  HappyHomesHappyHomesLucban=0,
                  HarrisonClaudioCarantes=0,
                  Hillside=0,
                  HolyGhostExtension=0,
                  HolyGhostProper=0,
                  HoneymoonHoneymoonHolyGhost=0,
                  ImeldaRMarcosLaSalle=0,
                  ImeldaVillage=0,
                  Irisan=0,
                  Kabayanihan=0,
                  Kagitingan=0,
                  KayangHilltop=0,
                  KayangExtension=0,
                  Kias=0,
                  LegardaBurnhamKisad=0,
                  LiwanagLoakan=0,
                  LoakanProper=0,
                  LopezJaena=0,
                  LourdesSubdivisionExtension=0,
                  LourdesSubdivisionLower=0,
                  LourdesSubdivisionProper=0,
                  Lualhati=0,
                  Lucnab=0,
                  MagsaysayPrivateRoad=0,
                  MagsaysayLower=0,
                  MagsaysayUpper=0,
                  MalcolmSquarePerfectoJoseAbadSantos=0,
                  ManuelARoxas=0,
                  MarketSubdivisionUpper=0,
                  MiddleQuezonHillSubdivsion=0,
                  MilitaryCutoff=0,
                  MinesViewPark=0,
                  ModernSiteEast=0,
                  ModernSiteWest=0,
                  MRRQueenofPeace=0,
                  NewLucban=0,
                  OutlookDrive=0,
                  Pacdal=0,
                  PadreBurgos=0,
                  PadreZamora=0,
                  PalmaUrbanoCarinoPalma=0,
                  PhilAm=0,
                  Pinget=0,
                  PinsaoPilotProject=0,
                  PinsaoProper=0,
                  Poliwes=0,
                  Pucsusan=0,
                  QuezonHillProper=0,
                  QuezonHillUpper=0,
                  QuirinoMagsaysayUpperUpperQM=0,
                  QuirinoHillEast=0,
                  QuirinoHillLower=0,
                  QuirinoHillMiddle=0,
                  QuirinoHillWest=0,
                  RizalMonumentArea=0,
                  RockQuarryLower=0,
                  RockQuarryMiddle=0,
                  RockQuarryUpper=0,
                  SaludMitra=0,
                  SanAntoniVillage=0,
                  SanLuisVillage=0,
                  SanRoqueVillage=0,
                  SanVicente=0,
                  SanitaryCampNorth=0,
                  SanitaryCampSouth=0,
                  SantaEscolastica=0,
                  SantoRosario=0,
                  SantoTomasProper=0,
                  SantoTomasSchoolArea=0,
                  ScoutBarrio=0,
                  SessionRoadArea=0,
                  SlaughterHouseAreaSantoNiñoSlaughter=0,
                  SLUSVPHousingVillage=0,
                  SouthDrive=0,
                  TeodoraAlonzo=0,
                  Trancoville=0,
                  VictoriaVillage=0,
                  SaintJosephVillage=0
)

for (n in 2:52) {
  tdf11[nrow(tdf11)+1,]=c(n,
                      count(typhoid11[ABCR&typhoid11$CASECLASS=="Probable"&typhoid11$MorbidityWeek==n,])+
                        count(typhoid11[ABCR&typhoid11$CASECLASS=="PROBABLE"&typhoid11$MorbidityWeek==n,])+
                        count(typhoid11[ABCR&typhoid11$CASECLASS=="CONFIRMED"&typhoid11$MorbidityWeek==n,]),
                      count(typhoid11[B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")&SS=="Probable"&MW==n,])+
                        count(typhoid11[B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[B==toupper("Alfonso Tabora")&SS=="Probable"&MW==n,])+
                        count(typhoid11[B==toupper("Alfonso Tabora")&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[B==toupper("Alfonso Tabora")&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[B=="AMBIONG"&SS=="Probable"&MW==n,])+
                        count(typhoid11[B=="AMBIONG"&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[B=="AMBIONG"&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[AB&SS=="Probable"&MW==n,])+
                        count(typhoid11[AB&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[AB&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[AL&SS=="Probable"&MW==n,])+
                        count(typhoid11[AL&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[AL&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[Asin&SS=="Probable"&MW==n,])+
                        count(typhoid11[Asin&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[Asin&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[ATOK&SS=="Probable"&MW==n,])+
                        count(typhoid11[ATOK&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[ATOK&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[auroraP&SS=="Probable"&MW==n,])+
                        count(typhoid11[auroraP&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[auroraP&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[auroraN&SS=="Probable"&MW==n,])+
                        count(typhoid11[auroraN&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[auroraN&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[auroraS&SS=="Probable"&MW==n,])+
                        count(typhoid11[auroraS&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[auroraS&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[BL&SS=="Probable"&MW==n,])+
                        count(typhoid11[BL&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[BL&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[BakakengCentral&SS=="Probable"&MW==n,])+
                        count(typhoid11[BakakengCentral&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[BakakengCentral&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[BakakengNorth&SS=="Probable"&MW==n,])+
                        count(typhoid11[BakakengNorth&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[BakakengNorth&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[Bal&SS=="Probable"&MW==n,])+
                        count(typhoid11[Bal&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[Bal&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[Balsigan&SS=="Probable"&MW==n,])+
                        count(typhoid11[Balsigan&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[Balsigan&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[BPE&SS=="Probable"&MW==n,])+
                        count(typhoid11[BPE&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[BPE&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[BPV&SS=="Probable"&MW==n,])+
                        count(typhoid11[BPV&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[BPV&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[Bayan&SS=="Probable"&MW==n,])+
                        count(typhoid11[Bayan&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[Bayan&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[BGH&SS=="Probable"&MW==n,])+
                        count(typhoid11[BGH&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[BGH&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[Brookside&SS=="Probable"&MW==n,])+
                        count(typhoid11[Brookside&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[Brookside&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[Brookspoint&SS=="Probable"&MW==n,])+
                        count(typhoid11[Brookspoint&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[Brookspoint&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[CabinetHill&SS=="Probable"&MW==n,])+
                        count(typhoid11[CabinetHill&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[CabinetHill&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[Camdas&SS=="Probable"&MW==n,])+
                        count(typhoid11[Camdas&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[Camdas&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[c7&SS=="Probable"&MW==n,])+
                        count(typhoid11[c7&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[c7&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[c8&SS=="Probable"&MW==n,])+
                        count(typhoid11[c8&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[c8&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[ca&SS=="Probable"&MW==n,])+
                        count(typhoid11[ca&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[ca&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[cf&SS=="Probable"&MW==n,])+
                        count(typhoid11[cf&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[cf&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[CityCC&SS=="Probable"&MW==n,])+
                        count(typhoid11[CityCC&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[CityCC&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[CityCP&SS=="Probable"&MW==n,])+
                        count(typhoid11[CityCP&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[CityCP&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[ccv&SS=="Probable"&MW==n,])+
                        count(typhoid11[ccv&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[ccv&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[Cres&SS=="Probable"&MW==n,])+
                        count(typhoid11[Cres&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[Cres&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[DagisanL&SS=="Probable"&MW==n,])+
                        count(typhoid11[DagisanL&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[DagisanL&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[DagisanU&SS=="Probable"&MW==n,])+
                        count(typhoid11[DagisanU&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[DagisanU&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[Dizon&SS=="Probable"&MW==n,])+
                        count(typhoid11[Dizon&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[Dizon&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[Domin&SS=="Probable"&MW==n,])+
                        count(typhoid11[Domin&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[Domin&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[Dontogan&SS=="Probable"&MW==n,])+
                        count(typhoid11[Dontogan&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[Dontogan&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[DPS&SS=="Probable"&MW==n,])+
                        count(typhoid11[DPS&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[DPS&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[enghill&SS=="Probable"&MW==n,])+
                        count(typhoid11[enghill&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[enghill&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[fvill&SS=="Probable"&MW==n,])+
                        count(typhoid11[fvill&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[fvill&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[Ferdinand&SS=="Probable"&MW==n,])+
                        count(typhoid11[Ferdinand&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[Ferdinand&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[B=="FORT DEL PILAR"&SS=="Probable"&MW==n,])+
                        count(typhoid11[B=="FORT DEL PILAR"&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[B=="FORT DEL PILAR"&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[GabSi&SS=="Probable"&MW==n,])+
                        count(typhoid11[GabSi&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[GabSi&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[GenE&SS=="Probable"&MW==n,])+
                        count(typhoid11[GenE&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[GenE&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[GenLL&SS=="Probable"&MW==n,])+
                        count(typhoid11[GenLL&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[GenLL&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[GenLU&SS=="Probable"&MW==n,])+
                        count(typhoid11[GenLU&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[GenLU&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[Gib&SS=="Probable"&MW==n,])+
                        count(typhoid11[Gib&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[Gib&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[Greenwater&SS=="Probable"&MW==n,])+
                        count(typhoid11[Greenwater&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[Greenwater&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[GuisadC&SS=="Probable"&MW==n,])+
                        count(typhoid11[GuisadC&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[GuisadC&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[GuisadS&SS=="Probable"&MW==n,])+
                        count(typhoid11[GuisadS&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[GuisadS&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[HHol&SS=="Probable"&MW==n,])+
                        count(typhoid11[HHol&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[HHol&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[HHom&SS=="Probable"&MW==n,])+
                        count(typhoid11[HHom&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[HHom&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[HCC&SS=="Probable"&MW==n,])+
                        count(typhoid11[HCC&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[HCC&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[Hillside&SS=="Probable"&MW==n,])+
                        count(typhoid11[Hillside&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[Hillside&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[HGhostE&SS=="Probable"&MW==n,])+
                        count(typhoid11[HGhostE&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[HGhostE&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[HGhostP&SS=="Probable"&MW==n,])+
                        count(typhoid11[HGhostP&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[HGhostP&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[Honeymoon&SS=="Probable"&MW==n,])+
                        count(typhoid11[Honeymoon&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[Honeymoon&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[IMarcos&SS=="Probable"&MW==n,])+
                        count(typhoid11[IMarcos&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[IMarcos&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[Imelda&SS=="Probable"&MW==n,])+
                        count(typhoid11[Imelda&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[Imelda&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[B=="IRISAN"&SS=="Probable"&MW==n,])+
                        count(typhoid11[B=="IRISAN"&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[B=="IRISAN"&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[Kabayanihan&SS=="Probable"&MW==n,])+
                        count(typhoid11[Kabayanihan&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[Kabayanihan&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[Kagitingan&SS=="Probable"&MW==n,])+
                        count(typhoid11[Kagitingan&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[Kagitingan&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[KayangHill&SS=="Probable"&MW==n,])+
                        count(typhoid11[KayangHill&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[KayangHill&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[KayangE&SS=="Probable"&MW==n,])+
                        count(typhoid11[KayangE&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[KayangE&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[kias&SS=="Probable"&MW==n,])+
                        count(typhoid11[kias&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[kias&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[LBK&SS=="Probable"&MW==n,])+
                        count(typhoid11[LBK&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[LBK&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[LLoak&SS=="Probable"&MW==n,])+
                        count(typhoid11[LLoak&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[LLoak&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[LoakP&SS=="Probable"&MW==n,])+
                        count(typhoid11[LoakP&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[LoakP&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[LOPJ&SS=="Probable"&MW==n,])+
                        count(typhoid11[LOPJ&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[LOPJ&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[LourdesE&SS=="Probable"&MW==n,])+
                        count(typhoid11[LourdesE&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[LourdesE&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[LourdesL&SS=="Probable"&MW==n,])+
                        count(typhoid11[LourdesL&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[LourdesL&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[LourdesP&SS=="Probable"&MW==n,])+
                        count(typhoid11[LourdesP&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[LourdesP&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[Lualhati&SS=="Probable"&MW==n,])+
                        count(typhoid11[Lualhati&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[Lualhati&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[lucnab&SS=="Probable"&MW==n,])+
                        count(typhoid11[lucnab&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[lucnab&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[MagsaysayPR&SS=="Probable"&MW==n,])+
                        count(typhoid11[MagsaysayPR&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[MagsaysayPR&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[MagsaysayL&SS=="Probable"&MW==n,])+
                        count(typhoid11[MagsaysayL&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[MagsaysayL&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[MagsaysayU&SS=="Probable"&MW==n,])+
                        count(typhoid11[MagsaysayU&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[MagsaysayU&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[MalcolmSquarePerfectoJoseAbadSantos&SS=="Probable"&MW==n,])+
                        count(typhoid11[MalcolmSquarePerfectoJoseAbadSantos&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[MalcolmSquarePerfectoJoseAbadSantos&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[MR&SS=="Probable"&MW==n,])+
                        count(typhoid11[MR&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[MR&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[MarketSubdivisionUpper&SS=="Probable"&MW==n,])+
                        count(typhoid11[MarketSubdivisionUpper&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[MarketSubdivisionUpper&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[MiddleQuezonHillSubdivsion&SS=="Probable"&MW==n,])+
                        count(typhoid11[MiddleQuezonHillSubdivsion&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[MiddleQuezonHillSubdivsion&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[MCutOff&SS=="Probable"&MW==n,])+
                        count(typhoid11[MCutOff&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[MCutOff&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[MinesView&SS=="Probable"&MW==n,])+
                        count(typhoid11[MinesView&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[MinesView&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[ModernSiteE&SS=="Probable"&MW==n,])+
                        count(typhoid11[ModernSiteE&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[ModernSiteE&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[ModernSiteW&SS=="Probable"&MW==n,])+
                        count(typhoid11[ModernSiteW&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[ModernSiteW&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[MRR&SS=="Probable"&MW==n,])+
                        count(typhoid11[MRR&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[MRR&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[NewLuc&SS=="Probable"&MW==n,])+
                        count(typhoid11[NewLuc&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[NewLuc&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[Outlook&SS=="Probable"&MW==n,])+
                        count(typhoid11[Outlook&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[Outlook&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[Pacdal&SS=="Probable"&MW==n,])+
                        count(typhoid11[Pacdal&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[Pacdal&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[PadreB&SS=="Probable"&MW==n,])+
                        count(typhoid11[PadreB&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[PadreB&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[PadreZ&SS=="Probable"&MW==n,])+
                        count(typhoid11[PadreZ&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[PadreZ&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[PU&SS=="Probable"&MW==n,])+
                        count(typhoid11[PU&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[PU&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[PhilAm&SS=="Probable"&MW==n,])+
                        count(typhoid11[PhilAm&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[PhilAm&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[Pinget&SS=="Probable"&MW==n,])+
                        count(typhoid11[Pinget&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[Pinget&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[PPP&SS=="Probable"&MW==n,])+
                        count(typhoid11[PPP&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[PPP&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[PisaoP&SS=="Probable"&MW==n,])+
                        count(typhoid11[PisaoP&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[PisaoP&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[Poliwes&SS=="Probable"&MW==n,])+
                        count(typhoid11[Poliwes&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[Poliwes&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[pucsusan&SS=="Probable"&MW==n,])+
                        count(typhoid11[pucsusan&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[pucsusan&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[QHP&SS=="Probable"&MW==n,])+
                        count(typhoid11[QHP&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[QHP&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[QHillU&SS=="Probable"&MW==n,])+
                        count(typhoid11[QHillU&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[QHillU&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[UpperQM&SS=="Probable"&MW==n,])+
                        count(typhoid11[UpperQM&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[UpperQM&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[QHillE&SS=="Probable"&MW==n,])+
                        count(typhoid11[QHillE&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[QHillE&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[QHL&SS=="Probable"&MW==n,])+
                        count(typhoid11[QHL&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[QHL&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[QHM&SS=="Probable"&MW==n,])+
                        count(typhoid11[QHM&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[QHM&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[QHillW&SS=="Probable"&MW==n,])+
                        count(typhoid11[QHillW&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[QHillW&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[RizalMonumentArea&SS=="Probable"&MW==n,])+
                        count(typhoid11[RizalMonumentArea&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[RizalMonumentArea&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[RockQuarryLower&SS=="Probable"&MW==n,])+
                        count(typhoid11[RockQuarryLower&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[RockQuarryLower&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[RockQuarryMiddle&SS=="Probable"&MW==n,])+
                        count(typhoid11[RockQuarryMiddle&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[RockQuarryMiddle&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[RockQuarryUpper&SS=="Probable"&MW==n,])+
                        count(typhoid11[RockQuarryUpper&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[RockQuarryUpper&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[salud&SS=="Probable"&MW==n,])+
                        count(typhoid11[salud&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[salud&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[SanAntonioVillage&SS=="Probable"&MW==n,])+
                        count(typhoid11[SanAntonioVillage&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[SanAntonioVillage&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[SanLuisVillage&SS=="Probable"&MW==n,])+
                        count(typhoid11[SanLuisVillage&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[SanLuisVillage&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[SanRoqueVillage&SS=="Probable"&MW==n,])+
                        count(typhoid11[SanRoqueVillage&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[SanRoqueVillage&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[sanvic&SS=="Probable"&MW==n,])+
                        count(typhoid11[sanvic&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[sanvic&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[SanCamp&SS=="Probable"&MW==n,])+
                        count(typhoid11[SanCamp&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[SanCamp&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[SanitaryCampSouth&SS=="Probable"&MW==n,])+
                        count(typhoid11[SanitaryCampSouth&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[SanitaryCampSouth&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[STE&SS=="Probable"&MW==n,])+
                        count(typhoid11[STE&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[STE&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[SantoR&SS=="Probable"&MW==n,])+
                        count(typhoid11[SantoR&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[SantoR&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[STP&SS=="Probable"&MW==n,])+
                        count(typhoid11[STP&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[STP&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[STsa&SS=="Probable"&MW==n,])+
                        count(typhoid11[STsa&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[STsa&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[ScoutB&SS=="Probable"&MW==n,])+
                        count(typhoid11[ScoutB&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[ScoutB&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[Session&SS=="Probable"&MW==n,])+
                        count(typhoid11[Session&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[Session&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[Slaughter&SS=="Probable"&MW==n,])+
                        count(typhoid11[Slaughter&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[Slaughter&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[SLU&SS=="Probable"&MW==n,])+
                        count(typhoid11[SLU&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[SLU&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[SouthDrive&SS=="Probable"&MW==n,])+
                        count(typhoid11[SouthDrive&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[SouthDrive&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[TeoAl&SS=="Probable"&MW==n,])+
                        count(typhoid11[TeoAl&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[TeoAl&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[tranco&SS=="Probable"&MW==n,])+
                        count(typhoid11[tranco&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[tranco&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[victoria&SS=="Probable"&MW==n,])+
                        count(typhoid11[victoria&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[victoria&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid11[STJos&SS=="Probable"&MW==n,])+
                        count(typhoid11[STJos&SS=="PROBABLE"&MW==n,])+
                        count(typhoid11[STJos&SS=="CONFIRMED"&MW==n,])
  )
}

B=typhoid12$Barangay
SS=typhoid12$CASECLASS
MW=typhoid12$MorbidityWeek

Imelda = B=="IMELDA VILLAGE"
BakakengCentral = B=="BAKAKENG CENTRAL"
ABCR = B=="A. BONIFACIO-CAGUIOA-RIMANDO (ABCR)"
Ambiong = B=="AMBIONG"
BakakengNorth = B=="BAKAKENG NORTH"
Asin = B=="ASIN"
Balsigan = B=="BALSIGAN"
Bayan = B=="BAYAN PARK WEST (BAYAN PARK)"
BGH=B=="BGH COMPOUND"
Brookside = B=="BROOKSIDE"
Camdas = B=="CAMDAS SUBDIVISION"
c7=B=="CAMP 7"
c8=B=="CAMP 8"
ca=B=="CAMP ALLEN"
cf=B=="CAMPO FILIPINO"
ccv=B=="COUNTRY CLUB VILLAGE"
DPS=B=="DPS AREA"
enghill=B=="ENGINEER'S HILL"
fvill=B=="FAIRVIEW VILLAGE"
GLuna=B=="GENERAL LUNA, UPPER"
Gib=B=="GIBRALTAR"
GuisadC=B=="GUISAD CENTRAL"
GuisadS=B=="GUISAD SORONG"
HHL=B=="HAPPY HOMES (HAPPY HOMES-LUCBAN)"
kias=B=="KIAS"
LLoak=B=="LIWANAG-LOAKAN"
LoakP=B=="LOAKAN PROPER"
LOPJ=B=="LOPEZ JAENA"
lucnab=B=="LUCNAB"
MR=B=="MANUEL A. ROXAS"
MCutOff=B=="MILITARY CUT-OFF"
MRR=B=="MRR-QUEEN OF PEACE"
PadreZ=B=="PADRE ZAMORA"
Pinget=B=="PINGET"
PPP=B=="PINSAO PILOT PROJECT"
PisaoP=B=="PINSAO PROPER"
Poliwes=B=="POLIWES"
QHP=B=="QUEZON HILL PROPER"
QHL=B=="QUIRINO HILL, LOWER"
QHM=B=="QUIRINO HILL, MIDDLE"
salud=B=="SALUD MITRA"
sanvic=B=="SAN VICENTE"
SanCamp=B=="SANITARY CAMP, NORTH"
STE=B=="SANTA ESCOLASTICA"
STP=B=="SANTO TOMAS PROPER"&B=="STO. TOMAS PROPER"
STsa=B=="SANTO TOMAS SCHOOL AREA"
tranc=B=="TRANCOVILLE"
birac=B=="BIRAC"
AL=B=="APUGAN-LOAKAN"
AZKCO=B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")
ATAB=B==toupper("Alfonso Tabora")
AB=B==toupper("Andres Bonifacio (Lower Bokawkan)")
ATOK=B==toupper("Atok Trail")
auroraP=B==toupper("Aurora Hill Proper (Malvar-Sgt. Floresca)")
auroraN=B==toupper("Aurora Hill, North Central")
auroraS=B==toupper("Aurora Hill, South Central")
BL=B==toupper("Bagong Lipunan (Market Area)")
Bal=B==toupper("Bal-Marcoville (Marcoville)")
BPE=B==toupper("Bayan Park East")
BPV=B==toupper("Bayan Park Village")
Brookspoint=B==toupper("Brookspoint")
CabinetHill=B==toupper("Cabinet Hill-Teacher's Camp")
CityCC=B==toupper("City Camp Central")
CityCP=B==toupper("City Camp Proper")
DagisanL=B==toupper("Dagsian, Lower")
Cres=B==toupper("Cresencia Village")
DagisanU=B==toupper("Dagsian, Upper")
Dizon=B==toupper("Dizon Subdivision")
Domin=B==toupper("Dominican Hill-Mirador")
Dontogan=B=="DONTOGAN"
DPS=B=="DPS AREA"
Ferdinand=B==toupper("Ferdinand (Happy Homes-Campo Sioco)")
GabSi=B=="GABRIELLA SILANG"
GenE=B==toupper("General Emilio F. Aguinaldo (Quirino-Magsaysay, Lower)")
GenLL=B==toupper("General Luna, Lower")
GenLU=B==toupper("General Luna, Upper")
Greenwater=B==toupper("Greenwater Village")
HHol=B==toupper("Happy Hollow")
HHom=B==toupper("Happy Homes (Happy Homes-Lucban)")
HCC=B==toupper("Harrison-Claudio Carantes")
Hillside=B==toupper("Hillside")
HGhostE=B==toupper("Holy Ghost Extension")
HGhostP=B==toupper("Holy Ghost Proper")
Honeymoon=B==toupper("Honeymoon (Honeymoon-Holy Ghost)")
IMarcos=B==toupper("Imelda R. Marcos (La Salle)")
Imelda=B==toupper("Imelda Village")
Kabayanihan=B==toupper("Kabayanihan")
Kagitingan=B==toupper("Kagitingan")
KayangHill=B==toupper("Kayang-Hilltop")
KayangE=B==toupper("Kayang Extension")
LBK=B==toupper("Legarda-Burnham-Kisad")
LourdesE=B==toupper("Lourdes Subdivision Extension")
LourdesL=B==toupper("Lourdes Subdivision, Lower")
LourdesP=B==toupper("Lourdes Subdivision, Proper")
Lualhati=B==toupper("Lualhati")
MagsaysayPR=B==toupper("Magsaysay Private Road")
MagsaysayL=B==toupper("Magsaysay, Lower")
MagsaysayU=B==toupper("Magsaysay, Upper")
MalcolmSquarePerfectoJoseAbadSantos=B==toupper("Malcolm Square-Perfecto (Jose Abad Santos)")
MarketSubdivisionUpper=B==toupper("Market Subdivision, Upper")
MiddleQuezonHillSubdivsion=B==toupper("Middle Quezon Hill Subdivsiion(Quezon Hill Middle)")
MinesView=B==toupper("Mines View Park")
ModernSiteE=B==toupper("Modern Site, East")
ModernSiteW=B==toupper("Modern Site, West")
NewLuc=B==toupper("New Lucban")
Outlook=B==toupper("Outlook Drive")
Pacdal=B==toupper("Pacdal")
PadreB=B==toupper("Padre Burgos")
PadreZ=B==toupper("Padre Zamora")
PU=B==toupper("Palma-Urbano (Cariño-Palma)")
PhilAm=B==toupper("Phil-Am")
pucsusan=B==toupper("Pucsusan")
QHillU=B==toupper("Quezon Hill, Upper")
UpperQM=B==toupper("Quirino-Magsaysay, Upper (Upper QM)")
QHillE=B==toupper("Quirino Hill, East")
QHillW=B==toupper("Quirino Hill, West")
RizalMonumentArea=B==toupper("Rizal Monument Area")
RockQuarryLower=B==toupper("Rock Quarry, Lower")
RockQuarryMiddle=B==toupper("Rock Quarry, Middle")
RockQuarryUpper=B==toupper("Rock Quarry, Upper")
SanAntonioVillage=B==toupper("San Antonio Village")
SanLuisVillage=B==toupper("San Luis Village")
SanRoqueVillage=B==toupper("San Roque Village")
SanitaryCampSouth=B==toupper("Sanitary Camp, South")
SantoR=B==toupper("Santo Rosario")
ScoutB=B==toupper("Scout Barrio")
Session=B==toupper("Session Road Area")
Slaughter=B==toupper("Slaughter House Area (Santo Niño Slaughter)")
SLU=B==toupper("SLU-SVP Housing Village")
SouthDrive=B==toupper("South Drive")
TeoAl=B==toupper("Teodora Alonzo")
tranco=B==toupper("Trancoville")
victoria=B==toupper("Victoria Village")
STJos=B=="ST JOSEPH VILLAGE"

tdf12 <- data.frame(MorbidityWeek=1,
                   ABonifacioCaguioaRimandoABCR=0,
                   AbanaoZanduetaKayongChugumOtekAZKCO=0,
                   AlfonsoTabora=0,
                   Ambiong=0,
                   AndresBonifacioLowerBokawkan=0,
                   ApuganLoakan=0,
                   AsinRoad=0,
                   AtokTrail=0,
                   AuroraHillProperMalvarSgtFloresca=0,
                   AuroraHillNorthCentral=0,
                   AuroraHillSouthCentral=0,
                   BagongLipunanMarketArea=0,
                   BakakengCentral=0,
                   BakakengNorth=1,
                   BalMarcovilleMarcoville=0,
                   Balsigan=0,
                   BayanParkEast=0,
                   BayanParkVillage=0,
                   BayanParkWestBayanPark=0,
                   BGHCompound=0,
                   Brookside=0,
                   Brookspoint=0,
                   CabinetHillTeachersCamp=0,
                   CamdasSubdivision=0,
                   Camp7=0,
                   Camp8=0,
                   CampAllen=0,
                   CampoFilipino=0,
                   CityCampCentral=0,
                   CityCampProper=0,
                   CountryClubVillage=0,
                   CresenciaVillage=0,
                   DagisanLower=0,
                   DagisanUpper=0,
                   DizonSubdivision=0,
                   DominicanHillMirador=0,
                   Dontogan=0,
                   DPSArea=0,
                   EngineersHill=0,
                   FairviewVillage=0,
                   FerdinandHappyHomesCampoSioco=0,
                   FortdelPilar=0,
                   GabrielaSilang=0,
                   GeneralEmilioFAguinaldoQuirinoMagsaysayLower=0,
                   GeneralLunaLower=0,
                   GeneralLunaUpper=0,
                   Gibraltar=0,
                   GreenwaterVillage=0,
                   GuisadCentral=0,
                   GuisadSorong=0,
                   HappyHollow=0,
                   HappyHomesHappyHomesLucban=0,
                   HarrisonClaudioCarantes=0,
                   Hillside=0,
                   HolyGhostExtension=0,
                   HolyGhostProper=0,
                   HoneymoonHoneymoonHolyGhost=0,
                   ImeldaRMarcosLaSalle=0,
                   ImeldaVillage=0,
                   Irisan=0,
                   Kabayanihan=0,
                   Kagitingan=0,
                   KayangHilltop=0,
                   KayangExtension=0,
                   Kias=0,
                   LegardaBurnhamKisad=0,
                   LiwanagLoakan=0,
                   LoakanProper=0,
                   LopezJaena=0,
                   LourdesSubdivisionExtension=0,
                   LourdesSubdivisionLower=0,
                   LourdesSubdivisionProper=0,
                   Lualhati=0,
                   Lucnab=0,
                   MagsaysayPrivateRoad=0,
                   MagsaysayLower=0,
                   MagsaysayUpper=0,
                   MalcolmSquarePerfectoJoseAbadSantos=0,
                   ManuelARoxas=0,
                   MarketSubdivisionUpper=0,
                   MiddleQuezonHillSubdivsion=0,
                   MilitaryCutoff=0,
                   MinesViewPark=0,
                   ModernSiteEast=0,
                   ModernSiteWest=0,
                   MRRQueenofPeace=0,
                   NewLucban=0,
                   OutlookDrive=0,
                   Pacdal=0,
                   PadreBurgos=0,
                   PadreZamora=0,
                   PalmaUrbanoCarinoPalma=0,
                   PhilAm=0,
                   Pinget=0,
                   PinsaoPilotProject=0,
                   PinsaoProper=0,
                   Poliwes=0,
                   Pucsusan=0,
                   QuezonHillProper=0,
                   QuezonHillUpper=0,
                   QuirinoMagsaysayUpperUpperQM=0,
                   QuirinoHillEast=0,
                   QuirinoHillLower=0,
                   QuirinoHillMiddle=0,
                   QuirinoHillWest=0,
                   RizalMonumentArea=0,
                   RockQuarryLower=0,
                   RockQuarryMiddle=0,
                   RockQuarryUpper=0,
                   SaludMitra=0,
                   SanAntoniVillage=0,
                   SanLuisVillage=0,
                   SanRoqueVillage=0,
                   SanVicente=0,
                   SanitaryCampNorth=0,
                   SanitaryCampSouth=0,
                   SantaEscolastica=0,
                   SantoRosario=0,
                   SantoTomasProper=0,
                   SantoTomasSchoolArea=0,
                   ScoutBarrio=0,
                   SessionRoadArea=0,
                   SlaughterHouseAreaSantoNiñoSlaughter=0,
                   SLUSVPHousingVillage=0,
                   SouthDrive=0,
                   TeodoraAlonzo=0,
                   Trancoville=0,
                   VictoriaVillage=0,
                   SaintJosephVillage=0
)

for (n in 2:52) {
  tdf12[nrow(tdf12)+1,]=c(n,
                      count(typhoid12[ABCR&typhoid12$CASECLASS=="Probable"&typhoid12$MorbidityWeek==n,])+
                        count(typhoid12[ABCR&typhoid12$CASECLASS=="PROBABLE"&typhoid12$MorbidityWeek==n,])+
                        count(typhoid12[ABCR&typhoid12$CASECLASS=="CONFIRMED"&typhoid12$MorbidityWeek==n,]),
                      count(typhoid12[B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")&SS=="Probable"&MW==n,])+
                        count(typhoid12[B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[B==toupper("Alfonso Tabora")&SS=="Probable"&MW==n,])+
                        count(typhoid12[B==toupper("Alfonso Tabora")&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[B==toupper("Alfonso Tabora")&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[B=="AMBIONG"&SS=="Probable"&MW==n,])+
                        count(typhoid12[B=="AMBIONG"&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[B=="AMBIONG"&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[AB&SS=="Probable"&MW==n,])+
                        count(typhoid12[AB&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[AB&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[AL&SS=="Probable"&MW==n,])+
                        count(typhoid12[AL&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[AL&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[Asin&SS=="Probable"&MW==n,])+
                        count(typhoid12[Asin&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[Asin&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[ATOK&SS=="Probable"&MW==n,])+
                        count(typhoid12[ATOK&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[ATOK&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[auroraP&SS=="Probable"&MW==n,])+
                        count(typhoid12[auroraP&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[auroraP&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[auroraN&SS=="Probable"&MW==n,])+
                        count(typhoid12[auroraN&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[auroraN&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[auroraS&SS=="Probable"&MW==n,])+
                        count(typhoid12[auroraS&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[auroraS&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[BL&SS=="Probable"&MW==n,])+
                        count(typhoid12[BL&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[BL&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[BakakengCentral&SS=="Probable"&MW==n,])+
                        count(typhoid12[BakakengCentral&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[BakakengCentral&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[BakakengNorth&SS=="Probable"&MW==n,])+
                        count(typhoid12[BakakengNorth&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[BakakengNorth&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[Bal&SS=="Probable"&MW==n,])+
                        count(typhoid12[Bal&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[Bal&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[Balsigan&SS=="Probable"&MW==n,])+
                        count(typhoid12[Balsigan&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[Balsigan&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[BPE&SS=="Probable"&MW==n,])+
                        count(typhoid12[BPE&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[BPE&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[BPV&SS=="Probable"&MW==n,])+
                        count(typhoid12[BPV&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[BPV&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[Bayan&SS=="Probable"&MW==n,])+
                        count(typhoid12[Bayan&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[Bayan&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[BGH&SS=="Probable"&MW==n,])+
                        count(typhoid12[BGH&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[BGH&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[Brookside&SS=="Probable"&MW==n,])+
                        count(typhoid12[Brookside&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[Brookside&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[Brookspoint&SS=="Probable"&MW==n,])+
                        count(typhoid12[Brookspoint&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[Brookspoint&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[CabinetHill&SS=="Probable"&MW==n,])+
                        count(typhoid12[CabinetHill&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[CabinetHill&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[Camdas&SS=="Probable"&MW==n,])+
                        count(typhoid12[Camdas&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[Camdas&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[c7&SS=="Probable"&MW==n,])+
                        count(typhoid12[c7&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[c7&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[c8&SS=="Probable"&MW==n,])+
                        count(typhoid12[c8&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[c8&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[ca&SS=="Probable"&MW==n,])+
                        count(typhoid12[ca&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[ca&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[cf&SS=="Probable"&MW==n,])+
                        count(typhoid12[cf&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[cf&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[CityCC&SS=="Probable"&MW==n,])+
                        count(typhoid12[CityCC&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[CityCC&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[CityCP&SS=="Probable"&MW==n,])+
                        count(typhoid12[CityCP&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[CityCP&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[ccv&SS=="Probable"&MW==n,])+
                        count(typhoid12[ccv&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[ccv&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[Cres&SS=="Probable"&MW==n,])+
                        count(typhoid12[Cres&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[Cres&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[DagisanL&SS=="Probable"&MW==n,])+
                        count(typhoid12[DagisanL&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[DagisanL&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[DagisanU&SS=="Probable"&MW==n,])+
                        count(typhoid12[DagisanU&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[DagisanU&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[Dizon&SS=="Probable"&MW==n,])+
                        count(typhoid12[Dizon&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[Dizon&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[Domin&SS=="Probable"&MW==n,])+
                        count(typhoid12[Domin&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[Domin&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[Dontogan&SS=="Probable"&MW==n,])+
                        count(typhoid12[Dontogan&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[Dontogan&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[DPS&SS=="Probable"&MW==n,])+
                        count(typhoid12[DPS&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[DPS&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[enghill&SS=="Probable"&MW==n,])+
                        count(typhoid12[enghill&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[enghill&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[fvill&SS=="Probable"&MW==n,])+
                        count(typhoid12[fvill&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[fvill&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[Ferdinand&SS=="Probable"&MW==n,])+
                        count(typhoid12[Ferdinand&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[Ferdinand&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[B=="FORT DEL PILAR"&SS=="Probable"&MW==n,])+
                        count(typhoid12[B=="FORT DEL PILAR"&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[B=="FORT DEL PILAR"&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[GabSi&SS=="Probable"&MW==n,])+
                        count(typhoid12[GabSi&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[GabSi&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[GenE&SS=="Probable"&MW==n,])+
                        count(typhoid12[GenE&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[GenE&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[GenLL&SS=="Probable"&MW==n,])+
                        count(typhoid12[GenLL&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[GenLL&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[GenLU&SS=="Probable"&MW==n,])+
                        count(typhoid12[GenLU&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[GenLU&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[Gib&SS=="Probable"&MW==n,])+
                        count(typhoid12[Gib&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[Gib&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[Greenwater&SS=="Probable"&MW==n,])+
                        count(typhoid12[Greenwater&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[Greenwater&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[GuisadC&SS=="Probable"&MW==n,])+
                        count(typhoid12[GuisadC&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[GuisadC&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[GuisadS&SS=="Probable"&MW==n,])+
                        count(typhoid12[GuisadS&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[GuisadS&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[HHol&SS=="Probable"&MW==n,])+
                        count(typhoid12[HHol&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[HHol&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[HHom&SS=="Probable"&MW==n,])+
                        count(typhoid12[HHom&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[HHom&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[HCC&SS=="Probable"&MW==n,])+
                        count(typhoid12[HCC&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[HCC&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[Hillside&SS=="Probable"&MW==n,])+
                        count(typhoid12[Hillside&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[Hillside&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[HGhostE&SS=="Probable"&MW==n,])+
                        count(typhoid12[HGhostE&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[HGhostE&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[HGhostP&SS=="Probable"&MW==n,])+
                        count(typhoid12[HGhostP&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[HGhostP&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[Honeymoon&SS=="Probable"&MW==n,])+
                        count(typhoid12[Honeymoon&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[Honeymoon&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[IMarcos&SS=="Probable"&MW==n,])+
                        count(typhoid12[IMarcos&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[IMarcos&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[Imelda&SS=="Probable"&MW==n,])+
                        count(typhoid12[Imelda&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[Imelda&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[B=="IRISAN"&SS=="Probable"&MW==n,])+
                        count(typhoid12[B=="IRISAN"&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[B=="IRISAN"&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[Kabayanihan&SS=="Probable"&MW==n,])+
                        count(typhoid12[Kabayanihan&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[Kabayanihan&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[Kagitingan&SS=="Probable"&MW==n,])+
                        count(typhoid12[Kagitingan&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[Kagitingan&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[KayangHill&SS=="Probable"&MW==n,])+
                        count(typhoid12[KayangHill&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[KayangHill&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[KayangE&SS=="Probable"&MW==n,])+
                        count(typhoid12[KayangE&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[KayangE&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[kias&SS=="Probable"&MW==n,])+
                        count(typhoid12[kias&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[kias&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[LBK&SS=="Probable"&MW==n,])+
                        count(typhoid12[LBK&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[LBK&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[LLoak&SS=="Probable"&MW==n,])+
                        count(typhoid12[LLoak&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[LLoak&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[LoakP&SS=="Probable"&MW==n,])+
                        count(typhoid12[LoakP&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[LoakP&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[LOPJ&SS=="Probable"&MW==n,])+
                        count(typhoid12[LOPJ&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[LOPJ&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[LourdesE&SS=="Probable"&MW==n,])+
                        count(typhoid12[LourdesE&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[LourdesE&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[LourdesL&SS=="Probable"&MW==n,])+
                        count(typhoid12[LourdesL&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[LourdesL&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[LourdesP&SS=="Probable"&MW==n,])+
                        count(typhoid12[LourdesP&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[LourdesP&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[Lualhati&SS=="Probable"&MW==n,])+
                        count(typhoid12[Lualhati&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[Lualhati&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[lucnab&SS=="Probable"&MW==n,])+
                        count(typhoid12[lucnab&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[lucnab&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[MagsaysayPR&SS=="Probable"&MW==n,])+
                        count(typhoid12[MagsaysayPR&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[MagsaysayPR&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[MagsaysayL&SS=="Probable"&MW==n,])+
                        count(typhoid12[MagsaysayL&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[MagsaysayL&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[MagsaysayU&SS=="Probable"&MW==n,])+
                        count(typhoid12[MagsaysayU&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[MagsaysayU&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[MalcolmSquarePerfectoJoseAbadSantos&SS=="Probable"&MW==n,])+
                        count(typhoid12[MalcolmSquarePerfectoJoseAbadSantos&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[MalcolmSquarePerfectoJoseAbadSantos&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[MR&SS=="Probable"&MW==n,])+
                        count(typhoid12[MR&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[MR&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[MarketSubdivisionUpper&SS=="Probable"&MW==n,])+
                        count(typhoid12[MarketSubdivisionUpper&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[MarketSubdivisionUpper&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[MiddleQuezonHillSubdivsion&SS=="Probable"&MW==n,])+
                        count(typhoid12[MiddleQuezonHillSubdivsion&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[MiddleQuezonHillSubdivsion&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[MCutOff&SS=="Probable"&MW==n,])+
                        count(typhoid12[MCutOff&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[MCutOff&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[MinesView&SS=="Probable"&MW==n,])+
                        count(typhoid12[MinesView&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[MinesView&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[ModernSiteE&SS=="Probable"&MW==n,])+
                        count(typhoid12[ModernSiteE&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[ModernSiteE&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[ModernSiteW&SS=="Probable"&MW==n,])+
                        count(typhoid12[ModernSiteW&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[ModernSiteW&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[MRR&SS=="Probable"&MW==n,])+
                        count(typhoid12[MRR&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[MRR&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[NewLuc&SS=="Probable"&MW==n,])+
                        count(typhoid12[NewLuc&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[NewLuc&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[Outlook&SS=="Probable"&MW==n,])+
                        count(typhoid12[Outlook&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[Outlook&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[Pacdal&SS=="Probable"&MW==n,])+
                        count(typhoid12[Pacdal&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[Pacdal&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[PadreB&SS=="Probable"&MW==n,])+
                        count(typhoid12[PadreB&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[PadreB&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[PadreZ&SS=="Probable"&MW==n,])+
                        count(typhoid12[PadreZ&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[PadreZ&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[PU&SS=="Probable"&MW==n,])+
                        count(typhoid12[PU&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[PU&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[PhilAm&SS=="Probable"&MW==n,])+
                        count(typhoid12[PhilAm&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[PhilAm&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[Pinget&SS=="Probable"&MW==n,])+
                        count(typhoid12[Pinget&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[Pinget&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[PPP&SS=="Probable"&MW==n,])+
                        count(typhoid12[PPP&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[PPP&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[PisaoP&SS=="Probable"&MW==n,])+
                        count(typhoid12[PisaoP&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[PisaoP&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[Poliwes&SS=="Probable"&MW==n,])+
                        count(typhoid12[Poliwes&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[Poliwes&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[pucsusan&SS=="Probable"&MW==n,])+
                        count(typhoid12[pucsusan&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[pucsusan&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[QHP&SS=="Probable"&MW==n,])+
                        count(typhoid12[QHP&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[QHP&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[QHillU&SS=="Probable"&MW==n,])+
                        count(typhoid12[QHillU&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[QHillU&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[UpperQM&SS=="Probable"&MW==n,])+
                        count(typhoid12[UpperQM&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[UpperQM&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[QHillE&SS=="Probable"&MW==n,])+
                        count(typhoid12[QHillE&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[QHillE&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[QHL&SS=="Probable"&MW==n,])+
                        count(typhoid12[QHL&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[QHL&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[QHM&SS=="Probable"&MW==n,])+
                        count(typhoid12[QHM&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[QHM&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[QHillW&SS=="Probable"&MW==n,])+
                        count(typhoid12[QHillW&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[QHillW&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[RizalMonumentArea&SS=="Probable"&MW==n,])+
                        count(typhoid12[RizalMonumentArea&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[RizalMonumentArea&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[RockQuarryLower&SS=="Probable"&MW==n,])+
                        count(typhoid12[RockQuarryLower&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[RockQuarryLower&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[RockQuarryMiddle&SS=="Probable"&MW==n,])+
                        count(typhoid12[RockQuarryMiddle&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[RockQuarryMiddle&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[RockQuarryUpper&SS=="Probable"&MW==n,])+
                        count(typhoid12[RockQuarryUpper&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[RockQuarryUpper&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[salud&SS=="Probable"&MW==n,])+
                        count(typhoid12[salud&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[salud&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[SanAntonioVillage&SS=="Probable"&MW==n,])+
                        count(typhoid12[SanAntonioVillage&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[SanAntonioVillage&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[SanLuisVillage&SS=="Probable"&MW==n,])+
                        count(typhoid12[SanLuisVillage&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[SanLuisVillage&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[SanRoqueVillage&SS=="Probable"&MW==n,])+
                        count(typhoid12[SanRoqueVillage&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[SanRoqueVillage&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[sanvic&SS=="Probable"&MW==n,])+
                        count(typhoid12[sanvic&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[sanvic&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[SanCamp&SS=="Probable"&MW==n,])+
                        count(typhoid12[SanCamp&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[SanCamp&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[SanitaryCampSouth&SS=="Probable"&MW==n,])+
                        count(typhoid12[SanitaryCampSouth&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[SanitaryCampSouth&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[STE&SS=="Probable"&MW==n,])+
                        count(typhoid12[STE&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[STE&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[SantoR&SS=="Probable"&MW==n,])+
                        count(typhoid12[SantoR&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[SantoR&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[STP&SS=="Probable"&MW==n,])+
                        count(typhoid12[STP&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[STP&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[STsa&SS=="Probable"&MW==n,])+
                        count(typhoid12[STsa&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[STsa&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[ScoutB&SS=="Probable"&MW==n,])+
                        count(typhoid12[ScoutB&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[ScoutB&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[Session&SS=="Probable"&MW==n,])+
                        count(typhoid12[Session&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[Session&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[Slaughter&SS=="Probable"&MW==n,])+
                        count(typhoid12[Slaughter&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[Slaughter&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[SLU&SS=="Probable"&MW==n,])+
                        count(typhoid12[SLU&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[SLU&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[SouthDrive&SS=="Probable"&MW==n,])+
                        count(typhoid12[SouthDrive&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[SouthDrive&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[TeoAl&SS=="Probable"&MW==n,])+
                        count(typhoid12[TeoAl&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[TeoAl&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[tranco&SS=="Probable"&MW==n,])+
                        count(typhoid12[tranco&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[tranco&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[victoria&SS=="Probable"&MW==n,])+
                        count(typhoid12[victoria&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[victoria&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid12[STJos&SS=="Probable"&MW==n,])+
                        count(typhoid12[STJos&SS=="PROBABLE"&MW==n,])+
                        count(typhoid12[STJos&SS=="CONFIRMED"&MW==n,])
  )
}

B=typhoid13$Barangay
SS=typhoid13$CASECLASS
MW=typhoid13$MorbidityWeek

Imelda = B=="IMELDA VILLAGE"
BakakengCentral = B=="BAKAKENG CENTRAL"
ABCR = B=="A. BONIFACIO-CAGUIOA-RIMANDO (ABCR)"
Ambiong = B=="AMBIONG"
BakakengNorth = B=="BAKAKENG NORTH"
Asin = B=="ASIN"
Balsigan = B=="BALSIGAN"
Bayan = B=="BAYAN PARK WEST (BAYAN PARK)"
BGH=B=="BGH COMPOUND"
Brookside = B=="BROOKSIDE"
Camdas = B=="CAMDAS SUBDIVISION"
c7=B=="CAMP 7"
c8=B=="CAMP 8"
ca=B=="CAMP ALLEN"
cf=B=="CAMPO FILIPINO"
ccv=B=="COUNTRY CLUB VILLAGE"
DPS=B=="DPS AREA"
enghill=B=="ENGINEER'S HILL"
fvill=B=="FAIRVIEW VILLAGE"
GLuna=B=="GENERAL LUNA, UPPER"
Gib=B=="GIBRALTAR"
GuisadC=B=="GUISAD CENTRAL"
GuisadS=B=="GUISAD SORONG"
HHL=B=="HAPPY HOMES (HAPPY HOMES-LUCBAN)"
kias=B=="KIAS"
LLoak=B=="LIWANAG-LOAKAN"
LoakP=B=="LOAKAN PROPER"
LOPJ=B=="LOPEZ JAENA"
lucnab=B=="LUCNAB"
MR=B=="MANUEL A. ROXAS"
MCutOff=B=="MILITARY CUT-OFF"
MRR=B=="MRR-QUEEN OF PEACE"
PadreZ=B=="PADRE ZAMORA"
Pinget=B=="PINGET"
PPP=B=="PINSAO PILOT PROJECT"
PisaoP=B=="PINSAO PROPER"
Poliwes=B=="POLIWES"
QHP=B=="QUEZON HILL PROPER"
QHL=B=="QUIRINO HILL, LOWER"
QHM=B=="QUIRINO HILL, MIDDLE"
salud=B=="SALUD MITRA"
sanvic=B=="SAN VICENTE"
SanCamp=B=="SANITARY CAMP, NORTH"
STE=B=="SANTA ESCOLASTICA"
STP=B=="SANTO TOMAS PROPER"&B=="STO. TOMAS PROPER"
STsa=B=="SANTO TOMAS SCHOOL AREA"
tranc=B=="TRANCOVILLE"
birac=B=="BIRAC"
AL=B=="APUGAN-LOAKAN"
AZKCO=B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")
ATAB=B==toupper("Alfonso Tabora")
AB=B==toupper("Andres Bonifacio (Lower Bokawkan)")
ATOK=B==toupper("Atok Trail")
auroraP=B==toupper("Aurora Hill Proper (Malvar-Sgt. Floresca)")
auroraN=B==toupper("Aurora Hill, North Central")
auroraS=B==toupper("Aurora Hill, South Central")
BL=B==toupper("Bagong Lipunan (Market Area)")
Bal=B==toupper("Bal-Marcoville (Marcoville)")
BPE=B==toupper("Bayan Park East")
BPV=B==toupper("Bayan Park Village")
Brookspoint=B==toupper("Brookspoint")
CabinetHill=B==toupper("Cabinet Hill-Teacher's Camp")
CityCC=B==toupper("City Camp Central")
CityCP=B==toupper("City Camp Proper")
DagisanL=B==toupper("Dagsian, Lower")
Cres=B==toupper("Cresencia Village")
DagisanU=B==toupper("Dagsian, Upper")
Dizon=B==toupper("Dizon Subdivision")
Domin=B==toupper("Dominican Hill-Mirador")
Dontogan=B=="DONTOGAN"
DPS=B=="DPS AREA"
Ferdinand=B==toupper("Ferdinand (Happy Homes-Campo Sioco)")
GabSi=B=="GABRIELLA SILANG"
GenE=B==toupper("General Emilio F. Aguinaldo (Quirino-Magsaysay, Lower)")
GenLL=B==toupper("General Luna, Lower")
GenLU=B==toupper("General Luna, Upper")
Greenwater=B==toupper("Greenwater Village")
HHol=B==toupper("Happy Hollow")
HHom=B==toupper("Happy Homes (Happy Homes-Lucban)")
HCC=B==toupper("Harrison-Claudio Carantes")
Hillside=B==toupper("Hillside")
HGhostE=B==toupper("Holy Ghost Extension")
HGhostP=B==toupper("Holy Ghost Proper")
Honeymoon=B==toupper("Honeymoon (Honeymoon-Holy Ghost)")
IMarcos=B==toupper("Imelda R. Marcos (La Salle)")
Imelda=B==toupper("Imelda Village")
Kabayanihan=B==toupper("Kabayanihan")
Kagitingan=B==toupper("Kagitingan")
KayangHill=B==toupper("Kayang-Hilltop")
KayangE=B==toupper("Kayang Extension")
LBK=B==toupper("Legarda-Burnham-Kisad")
LourdesE=B==toupper("Lourdes Subdivision Extension")
LourdesL=B==toupper("Lourdes Subdivision, Lower")
LourdesP=B==toupper("Lourdes Subdivision, Proper")
Lualhati=B==toupper("Lualhati")
MagsaysayPR=B==toupper("Magsaysay Private Road")
MagsaysayL=B==toupper("Magsaysay, Lower")
MagsaysayU=B==toupper("Magsaysay, Upper")
MalcolmSquarePerfectoJoseAbadSantos=B==toupper("Malcolm Square-Perfecto (Jose Abad Santos)")
MarketSubdivisionUpper=B==toupper("Market Subdivision, Upper")
MiddleQuezonHillSubdivsion=B==toupper("Middle Quezon Hill Subdivsiion(Quezon Hill Middle)")
MinesView=B==toupper("Mines View Park")
ModernSiteE=B==toupper("Modern Site, East")
ModernSiteW=B==toupper("Modern Site, West")
NewLuc=B==toupper("New Lucban")
Outlook=B==toupper("Outlook Drive")
Pacdal=B==toupper("Pacdal")
PadreB=B==toupper("Padre Burgos")
PadreZ=B==toupper("Padre Zamora")
PU=B==toupper("Palma-Urbano (Cariño-Palma)")
PhilAm=B==toupper("Phil-Am")
pucsusan=B==toupper("Pucsusan")
QHillU=B==toupper("Quezon Hill, Upper")
UpperQM=B==toupper("Quirino-Magsaysay, Upper (Upper QM)")
QHillE=B==toupper("Quirino Hill, East")
QHillW=B==toupper("Quirino Hill, West")
RizalMonumentArea=B==toupper("Rizal Monument Area")
RockQuarryLower=B==toupper("Rock Quarry, Lower")
RockQuarryMiddle=B==toupper("Rock Quarry, Middle")
RockQuarryUpper=B==toupper("Rock Quarry, Upper")
SanAntonioVillage=B==toupper("San Antonio Village")
SanLuisVillage=B==toupper("San Luis Village")
SanRoqueVillage=B==toupper("San Roque Village")
SanitaryCampSouth=B==toupper("Sanitary Camp, South")
SantoR=B==toupper("Santo Rosario")
ScoutB=B==toupper("Scout Barrio")
Session=B==toupper("Session Road Area")
Slaughter=B==toupper("Slaughter House Area (Santo Niño Slaughter)")
SLU=B==toupper("SLU-SVP Housing Village")
SouthDrive=B==toupper("South Drive")
TeoAl=B==toupper("Teodora Alonzo")
tranco=B==toupper("Trancoville")
victoria=B==toupper("Victoria Village")
STJos=B=="ST JOSEPH VILLAGE"

tdf13 <- data.frame(MorbidityWeek=1,
                   ABonifacioCaguioaRimandoABCR=0,
                   AbanaoZanduetaKayongChugumOtekAZKCO=0,
                   AlfonsoTabora=0,
                   Ambiong=0,
                   AndresBonifacioLowerBokawkan=0,
                   ApuganLoakan=0,
                   AsinRoad=0,
                   AtokTrail=0,
                   AuroraHillProperMalvarSgtFloresca=0,
                   AuroraHillNorthCentral=0,
                   AuroraHillSouthCentral=0,
                   BagongLipunanMarketArea=0,
                   BakakengCentral=0,
                   BakakengNorth=1,
                   BalMarcovilleMarcoville=0,
                   Balsigan=0,
                   BayanParkEast=0,
                   BayanParkVillage=0,
                   BayanParkWestBayanPark=0,
                   BGHCompound=0,
                   Brookside=0,
                   Brookspoint=0,
                   CabinetHillTeachersCamp=0,
                   CamdasSubdivision=0,
                   Camp7=0,
                   Camp8=0,
                   CampAllen=0,
                   CampoFilipino=0,
                   CityCampCentral=0,
                   CityCampProper=0,
                   CountryClubVillage=0,
                   CresenciaVillage=0,
                   DagisanLower=0,
                   DagisanUpper=0,
                   DizonSubdivision=0,
                   DominicanHillMirador=0,
                   Dontogan=0,
                   DPSArea=0,
                   EngineersHill=0,
                   FairviewVillage=0,
                   FerdinandHappyHomesCampoSioco=0,
                   FortdelPilar=0,
                   GabrielaSilang=0,
                   GeneralEmilioFAguinaldoQuirinoMagsaysayLower=0,
                   GeneralLunaLower=0,
                   GeneralLunaUpper=0,
                   Gibraltar=0,
                   GreenwaterVillage=0,
                   GuisadCentral=0,
                   GuisadSorong=0,
                   HappyHollow=0,
                   HappyHomesHappyHomesLucban=0,
                   HarrisonClaudioCarantes=0,
                   Hillside=0,
                   HolyGhostExtension=0,
                   HolyGhostProper=0,
                   HoneymoonHoneymoonHolyGhost=0,
                   ImeldaRMarcosLaSalle=0,
                   ImeldaVillage=0,
                   Irisan=0,
                   Kabayanihan=0,
                   Kagitingan=0,
                   KayangHilltop=0,
                   KayangExtension=0,
                   Kias=0,
                   LegardaBurnhamKisad=0,
                   LiwanagLoakan=0,
                   LoakanProper=0,
                   LopezJaena=0,
                   LourdesSubdivisionExtension=0,
                   LourdesSubdivisionLower=0,
                   LourdesSubdivisionProper=0,
                   Lualhati=0,
                   Lucnab=0,
                   MagsaysayPrivateRoad=0,
                   MagsaysayLower=0,
                   MagsaysayUpper=0,
                   MalcolmSquarePerfectoJoseAbadSantos=0,
                   ManuelARoxas=0,
                   MarketSubdivisionUpper=0,
                   MiddleQuezonHillSubdivsion=0,
                   MilitaryCutoff=0,
                   MinesViewPark=0,
                   ModernSiteEast=0,
                   ModernSiteWest=0,
                   MRRQueenofPeace=0,
                   NewLucban=0,
                   OutlookDrive=0,
                   Pacdal=0,
                   PadreBurgos=0,
                   PadreZamora=0,
                   PalmaUrbanoCarinoPalma=0,
                   PhilAm=0,
                   Pinget=0,
                   PinsaoPilotProject=0,
                   PinsaoProper=0,
                   Poliwes=0,
                   Pucsusan=0,
                   QuezonHillProper=0,
                   QuezonHillUpper=0,
                   QuirinoMagsaysayUpperUpperQM=0,
                   QuirinoHillEast=0,
                   QuirinoHillLower=0,
                   QuirinoHillMiddle=0,
                   QuirinoHillWest=0,
                   RizalMonumentArea=0,
                   RockQuarryLower=0,
                   RockQuarryMiddle=0,
                   RockQuarryUpper=0,
                   SaludMitra=0,
                   SanAntoniVillage=0,
                   SanLuisVillage=0,
                   SanRoqueVillage=0,
                   SanVicente=0,
                   SanitaryCampNorth=0,
                   SanitaryCampSouth=0,
                   SantaEscolastica=0,
                   SantoRosario=0,
                   SantoTomasProper=0,
                   SantoTomasSchoolArea=0,
                   ScoutBarrio=0,
                   SessionRoadArea=0,
                   SlaughterHouseAreaSantoNiñoSlaughter=0,
                   SLUSVPHousingVillage=0,
                   SouthDrive=0,
                   TeodoraAlonzo=0,
                   Trancoville=0,
                   VictoriaVillage=0,
                   SaintJosephVillage=0
)

for (n in 2:52) {
  tdf13[nrow(tdf13)+1,]=c(n,
                      count(typhoid13[ABCR&typhoid13$CASECLASS=="Probable"&typhoid13$MorbidityWeek==n,])+
                        count(typhoid13[ABCR&typhoid13$CASECLASS=="PROBABLE"&typhoid13$MorbidityWeek==n,])+
                        count(typhoid13[ABCR&typhoid13$CASECLASS=="CONFIRMED"&typhoid13$MorbidityWeek==n,]),
                      count(typhoid13[B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")&SS=="Probable"&MW==n,])+
                        count(typhoid13[B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[B==toupper("Alfonso Tabora")&SS=="Probable"&MW==n,])+
                        count(typhoid13[B==toupper("Alfonso Tabora")&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[B==toupper("Alfonso Tabora")&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[B=="AMBIONG"&SS=="Probable"&MW==n,])+
                        count(typhoid13[B=="AMBIONG"&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[B=="AMBIONG"&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[AB&SS=="Probable"&MW==n,])+
                        count(typhoid13[AB&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[AB&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[AL&SS=="Probable"&MW==n,])+
                        count(typhoid13[AL&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[AL&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[Asin&SS=="Probable"&MW==n,])+
                        count(typhoid13[Asin&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[Asin&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[ATOK&SS=="Probable"&MW==n,])+
                        count(typhoid13[ATOK&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[ATOK&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[auroraP&SS=="Probable"&MW==n,])+
                        count(typhoid13[auroraP&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[auroraP&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[auroraN&SS=="Probable"&MW==n,])+
                        count(typhoid13[auroraN&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[auroraN&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[auroraS&SS=="Probable"&MW==n,])+
                        count(typhoid13[auroraS&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[auroraS&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[BL&SS=="Probable"&MW==n,])+
                        count(typhoid13[BL&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[BL&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[BakakengCentral&SS=="Probable"&MW==n,])+
                        count(typhoid13[BakakengCentral&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[BakakengCentral&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[BakakengNorth&SS=="Probable"&MW==n,])+
                        count(typhoid13[BakakengNorth&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[BakakengNorth&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[Bal&SS=="Probable"&MW==n,])+
                        count(typhoid13[Bal&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[Bal&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[Balsigan&SS=="Probable"&MW==n,])+
                        count(typhoid13[Balsigan&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[Balsigan&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[BPE&SS=="Probable"&MW==n,])+
                        count(typhoid13[BPE&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[BPE&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[BPV&SS=="Probable"&MW==n,])+
                        count(typhoid13[BPV&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[BPV&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[Bayan&SS=="Probable"&MW==n,])+
                        count(typhoid13[Bayan&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[Bayan&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[BGH&SS=="Probable"&MW==n,])+
                        count(typhoid13[BGH&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[BGH&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[Brookside&SS=="Probable"&MW==n,])+
                        count(typhoid13[Brookside&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[Brookside&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[Brookspoint&SS=="Probable"&MW==n,])+
                        count(typhoid13[Brookspoint&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[Brookspoint&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[CabinetHill&SS=="Probable"&MW==n,])+
                        count(typhoid13[CabinetHill&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[CabinetHill&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[Camdas&SS=="Probable"&MW==n,])+
                        count(typhoid13[Camdas&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[Camdas&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[c7&SS=="Probable"&MW==n,])+
                        count(typhoid13[c7&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[c7&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[c8&SS=="Probable"&MW==n,])+
                        count(typhoid13[c8&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[c8&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[ca&SS=="Probable"&MW==n,])+
                        count(typhoid13[ca&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[ca&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[cf&SS=="Probable"&MW==n,])+
                        count(typhoid13[cf&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[cf&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[CityCC&SS=="Probable"&MW==n,])+
                        count(typhoid13[CityCC&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[CityCC&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[CityCP&SS=="Probable"&MW==n,])+
                        count(typhoid13[CityCP&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[CityCP&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[ccv&SS=="Probable"&MW==n,])+
                        count(typhoid13[ccv&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[ccv&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[Cres&SS=="Probable"&MW==n,])+
                        count(typhoid13[Cres&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[Cres&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[DagisanL&SS=="Probable"&MW==n,])+
                        count(typhoid13[DagisanL&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[DagisanL&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[DagisanU&SS=="Probable"&MW==n,])+
                        count(typhoid13[DagisanU&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[DagisanU&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[Dizon&SS=="Probable"&MW==n,])+
                        count(typhoid13[Dizon&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[Dizon&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[Domin&SS=="Probable"&MW==n,])+
                        count(typhoid13[Domin&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[Domin&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[Dontogan&SS=="Probable"&MW==n,])+
                        count(typhoid13[Dontogan&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[Dontogan&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[DPS&SS=="Probable"&MW==n,])+
                        count(typhoid13[DPS&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[DPS&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[enghill&SS=="Probable"&MW==n,])+
                        count(typhoid13[enghill&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[enghill&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[fvill&SS=="Probable"&MW==n,])+
                        count(typhoid13[fvill&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[fvill&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[Ferdinand&SS=="Probable"&MW==n,])+
                        count(typhoid13[Ferdinand&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[Ferdinand&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[B=="FORT DEL PILAR"&SS=="Probable"&MW==n,])+
                        count(typhoid13[B=="FORT DEL PILAR"&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[B=="FORT DEL PILAR"&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[GabSi&SS=="Probable"&MW==n,])+
                        count(typhoid13[GabSi&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[GabSi&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[GenE&SS=="Probable"&MW==n,])+
                        count(typhoid13[GenE&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[GenE&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[GenLL&SS=="Probable"&MW==n,])+
                        count(typhoid13[GenLL&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[GenLL&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[GenLU&SS=="Probable"&MW==n,])+
                        count(typhoid13[GenLU&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[GenLU&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[Gib&SS=="Probable"&MW==n,])+
                        count(typhoid13[Gib&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[Gib&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[Greenwater&SS=="Probable"&MW==n,])+
                        count(typhoid13[Greenwater&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[Greenwater&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[GuisadC&SS=="Probable"&MW==n,])+
                        count(typhoid13[GuisadC&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[GuisadC&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[GuisadS&SS=="Probable"&MW==n,])+
                        count(typhoid13[GuisadS&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[GuisadS&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[HHol&SS=="Probable"&MW==n,])+
                        count(typhoid13[HHol&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[HHol&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[HHom&SS=="Probable"&MW==n,])+
                        count(typhoid13[HHom&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[HHom&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[HCC&SS=="Probable"&MW==n,])+
                        count(typhoid13[HCC&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[HCC&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[Hillside&SS=="Probable"&MW==n,])+
                        count(typhoid13[Hillside&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[Hillside&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[HGhostE&SS=="Probable"&MW==n,])+
                        count(typhoid13[HGhostE&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[HGhostE&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[HGhostP&SS=="Probable"&MW==n,])+
                        count(typhoid13[HGhostP&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[HGhostP&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[Honeymoon&SS=="Probable"&MW==n,])+
                        count(typhoid13[Honeymoon&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[Honeymoon&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[IMarcos&SS=="Probable"&MW==n,])+
                        count(typhoid13[IMarcos&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[IMarcos&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[Imelda&SS=="Probable"&MW==n,])+
                        count(typhoid13[Imelda&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[Imelda&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[B=="IRISAN"&SS=="Probable"&MW==n,])+
                        count(typhoid13[B=="IRISAN"&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[B=="IRISAN"&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[Kabayanihan&SS=="Probable"&MW==n,])+
                        count(typhoid13[Kabayanihan&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[Kabayanihan&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[Kagitingan&SS=="Probable"&MW==n,])+
                        count(typhoid13[Kagitingan&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[Kagitingan&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[KayangHill&SS=="Probable"&MW==n,])+
                        count(typhoid13[KayangHill&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[KayangHill&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[KayangE&SS=="Probable"&MW==n,])+
                        count(typhoid13[KayangE&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[KayangE&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[kias&SS=="Probable"&MW==n,])+
                        count(typhoid13[kias&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[kias&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[LBK&SS=="Probable"&MW==n,])+
                        count(typhoid13[LBK&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[LBK&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[LLoak&SS=="Probable"&MW==n,])+
                        count(typhoid13[LLoak&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[LLoak&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[LoakP&SS=="Probable"&MW==n,])+
                        count(typhoid13[LoakP&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[LoakP&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[LOPJ&SS=="Probable"&MW==n,])+
                        count(typhoid13[LOPJ&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[LOPJ&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[LourdesE&SS=="Probable"&MW==n,])+
                        count(typhoid13[LourdesE&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[LourdesE&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[LourdesL&SS=="Probable"&MW==n,])+
                        count(typhoid13[LourdesL&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[LourdesL&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[LourdesP&SS=="Probable"&MW==n,])+
                        count(typhoid13[LourdesP&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[LourdesP&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[Lualhati&SS=="Probable"&MW==n,])+
                        count(typhoid13[Lualhati&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[Lualhati&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[lucnab&SS=="Probable"&MW==n,])+
                        count(typhoid13[lucnab&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[lucnab&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[MagsaysayPR&SS=="Probable"&MW==n,])+
                        count(typhoid13[MagsaysayPR&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[MagsaysayPR&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[MagsaysayL&SS=="Probable"&MW==n,])+
                        count(typhoid13[MagsaysayL&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[MagsaysayL&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[MagsaysayU&SS=="Probable"&MW==n,])+
                        count(typhoid13[MagsaysayU&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[MagsaysayU&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[MalcolmSquarePerfectoJoseAbadSantos&SS=="Probable"&MW==n,])+
                        count(typhoid13[MalcolmSquarePerfectoJoseAbadSantos&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[MalcolmSquarePerfectoJoseAbadSantos&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[MR&SS=="Probable"&MW==n,])+
                        count(typhoid13[MR&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[MR&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[MarketSubdivisionUpper&SS=="Probable"&MW==n,])+
                        count(typhoid13[MarketSubdivisionUpper&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[MarketSubdivisionUpper&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[MiddleQuezonHillSubdivsion&SS=="Probable"&MW==n,])+
                        count(typhoid13[MiddleQuezonHillSubdivsion&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[MiddleQuezonHillSubdivsion&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[MCutOff&SS=="Probable"&MW==n,])+
                        count(typhoid13[MCutOff&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[MCutOff&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[MinesView&SS=="Probable"&MW==n,])+
                        count(typhoid13[MinesView&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[MinesView&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[ModernSiteE&SS=="Probable"&MW==n,])+
                        count(typhoid13[ModernSiteE&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[ModernSiteE&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[ModernSiteW&SS=="Probable"&MW==n,])+
                        count(typhoid13[ModernSiteW&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[ModernSiteW&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[MRR&SS=="Probable"&MW==n,])+
                        count(typhoid13[MRR&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[MRR&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[NewLuc&SS=="Probable"&MW==n,])+
                        count(typhoid13[NewLuc&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[NewLuc&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[Outlook&SS=="Probable"&MW==n,])+
                        count(typhoid13[Outlook&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[Outlook&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[Pacdal&SS=="Probable"&MW==n,])+
                        count(typhoid13[Pacdal&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[Pacdal&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[PadreB&SS=="Probable"&MW==n,])+
                        count(typhoid13[PadreB&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[PadreB&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[PadreZ&SS=="Probable"&MW==n,])+
                        count(typhoid13[PadreZ&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[PadreZ&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[PU&SS=="Probable"&MW==n,])+
                        count(typhoid13[PU&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[PU&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[PhilAm&SS=="Probable"&MW==n,])+
                        count(typhoid13[PhilAm&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[PhilAm&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[Pinget&SS=="Probable"&MW==n,])+
                        count(typhoid13[Pinget&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[Pinget&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[PPP&SS=="Probable"&MW==n,])+
                        count(typhoid13[PPP&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[PPP&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[PisaoP&SS=="Probable"&MW==n,])+
                        count(typhoid13[PisaoP&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[PisaoP&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[Poliwes&SS=="Probable"&MW==n,])+
                        count(typhoid13[Poliwes&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[Poliwes&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[pucsusan&SS=="Probable"&MW==n,])+
                        count(typhoid13[pucsusan&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[pucsusan&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[QHP&SS=="Probable"&MW==n,])+
                        count(typhoid13[QHP&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[QHP&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[QHillU&SS=="Probable"&MW==n,])+
                        count(typhoid13[QHillU&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[QHillU&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[UpperQM&SS=="Probable"&MW==n,])+
                        count(typhoid13[UpperQM&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[UpperQM&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[QHillE&SS=="Probable"&MW==n,])+
                        count(typhoid13[QHillE&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[QHillE&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[QHL&SS=="Probable"&MW==n,])+
                        count(typhoid13[QHL&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[QHL&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[QHM&SS=="Probable"&MW==n,])+
                        count(typhoid13[QHM&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[QHM&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[QHillW&SS=="Probable"&MW==n,])+
                        count(typhoid13[QHillW&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[QHillW&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[RizalMonumentArea&SS=="Probable"&MW==n,])+
                        count(typhoid13[RizalMonumentArea&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[RizalMonumentArea&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[RockQuarryLower&SS=="Probable"&MW==n,])+
                        count(typhoid13[RockQuarryLower&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[RockQuarryLower&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[RockQuarryMiddle&SS=="Probable"&MW==n,])+
                        count(typhoid13[RockQuarryMiddle&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[RockQuarryMiddle&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[RockQuarryUpper&SS=="Probable"&MW==n,])+
                        count(typhoid13[RockQuarryUpper&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[RockQuarryUpper&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[salud&SS=="Probable"&MW==n,])+
                        count(typhoid13[salud&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[salud&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[SanAntonioVillage&SS=="Probable"&MW==n,])+
                        count(typhoid13[SanAntonioVillage&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[SanAntonioVillage&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[SanLuisVillage&SS=="Probable"&MW==n,])+
                        count(typhoid13[SanLuisVillage&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[SanLuisVillage&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[SanRoqueVillage&SS=="Probable"&MW==n,])+
                        count(typhoid13[SanRoqueVillage&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[SanRoqueVillage&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[sanvic&SS=="Probable"&MW==n,])+
                        count(typhoid13[sanvic&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[sanvic&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[SanCamp&SS=="Probable"&MW==n,])+
                        count(typhoid13[SanCamp&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[SanCamp&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[SanitaryCampSouth&SS=="Probable"&MW==n,])+
                        count(typhoid13[SanitaryCampSouth&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[SanitaryCampSouth&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[STE&SS=="Probable"&MW==n,])+
                        count(typhoid13[STE&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[STE&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[SantoR&SS=="Probable"&MW==n,])+
                        count(typhoid13[SantoR&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[SantoR&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[STP&SS=="Probable"&MW==n,])+
                        count(typhoid13[STP&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[STP&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[STsa&SS=="Probable"&MW==n,])+
                        count(typhoid13[STsa&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[STsa&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[ScoutB&SS=="Probable"&MW==n,])+
                        count(typhoid13[ScoutB&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[ScoutB&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[Session&SS=="Probable"&MW==n,])+
                        count(typhoid13[Session&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[Session&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[Slaughter&SS=="Probable"&MW==n,])+
                        count(typhoid13[Slaughter&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[Slaughter&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[SLU&SS=="Probable"&MW==n,])+
                        count(typhoid13[SLU&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[SLU&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[SouthDrive&SS=="Probable"&MW==n,])+
                        count(typhoid13[SouthDrive&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[SouthDrive&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[TeoAl&SS=="Probable"&MW==n,])+
                        count(typhoid13[TeoAl&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[TeoAl&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[tranco&SS=="Probable"&MW==n,])+
                        count(typhoid13[tranco&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[tranco&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[victoria&SS=="Probable"&MW==n,])+
                        count(typhoid13[victoria&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[victoria&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid13[STJos&SS=="Probable"&MW==n,])+
                        count(typhoid13[STJos&SS=="PROBABLE"&MW==n,])+
                        count(typhoid13[STJos&SS=="CONFIRMED"&MW==n,])
  )
}

B=typhoid14$Barangay
SS=typhoid14$CASECLASS
MW=typhoid14$MorbidityWeek

Imelda = B=="IMELDA VILLAGE"
BakakengCentral = B=="BAKAKENG CENTRAL"
ABCR = B=="A. BONIFACIO-CAGUIOA-RIMANDO (ABCR)"
Ambiong = B=="AMBIONG"
BakakengNorth = B=="BAKAKENG NORTH"
Asin = B=="ASIN"
Balsigan = B=="BALSIGAN"
Bayan = B=="BAYAN PARK WEST (BAYAN PARK)"
BGH=B=="BGH COMPOUND"
Brookside = B=="BROOKSIDE"
Camdas = B=="CAMDAS SUBDIVISION"
c7=B=="CAMP 7"
c8=B=="CAMP 8"
ca=B=="CAMP ALLEN"
cf=B=="CAMPO FILIPINO"
ccv=B=="COUNTRY CLUB VILLAGE"
DPS=B=="DPS AREA"
enghill=B=="ENGINEER'S HILL"
fvill=B=="FAIRVIEW VILLAGE"
GLuna=B=="GENERAL LUNA, UPPER"
Gib=B=="GIBRALTAR"
GuisadC=B=="GUISAD CENTRAL"
GuisadS=B=="GUISAD SORONG"
HHL=B=="HAPPY HOMES (HAPPY HOMES-LUCBAN)"
kias=B=="KIAS"
LLoak=B=="LIWANAG-LOAKAN"
LoakP=B=="LOAKAN PROPER"
LOPJ=B=="LOPEZ JAENA"
lucnab=B=="LUCNAB"
MR=B=="MANUEL A. ROXAS"
MCutOff=B=="MILITARY CUT-OFF"
MRR=B=="MRR-QUEEN OF PEACE"
PadreZ=B=="PADRE ZAMORA"
Pinget=B=="PINGET"
PPP=B=="PINSAO PILOT PROJECT"
PisaoP=B=="PINSAO PROPER"
Poliwes=B=="POLIWES"
QHP=B=="QUEZON HILL PROPER"
QHL=B=="QUIRINO HILL, LOWER"
QHM=B=="QUIRINO HILL, MIDDLE"
salud=B=="SALUD MITRA"
sanvic=B=="SAN VICENTE"
SanCamp=B=="SANITARY CAMP, NORTH"
STE=B=="SANTA ESCOLASTICA"
STP=B=="SANTO TOMAS PROPER"&B=="STO. TOMAS PROPER"
STsa=B=="SANTO TOMAS SCHOOL AREA"
tranc=B=="TRANCOVILLE"
birac=B=="BIRAC"
AL=B=="APUGAN-LOAKAN"
AZKCO=B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")
ATAB=B==toupper("Alfonso Tabora")
AB=B==toupper("Andres Bonifacio (Lower Bokawkan)")
ATOK=B==toupper("Atok Trail")
auroraP=B==toupper("Aurora Hill Proper (Malvar-Sgt. Floresca)")
auroraN=B==toupper("Aurora Hill, North Central")
auroraS=B==toupper("Aurora Hill, South Central")
BL=B==toupper("Bagong Lipunan (Market Area)")
Bal=B==toupper("Bal-Marcoville (Marcoville)")
BPE=B==toupper("Bayan Park East")
BPV=B==toupper("Bayan Park Village")
Brookspoint=B==toupper("Brookspoint")
CabinetHill=B==toupper("Cabinet Hill-Teacher's Camp")
CityCC=B==toupper("City Camp Central")
CityCP=B==toupper("City Camp Proper")
DagisanL=B==toupper("Dagsian, Lower")
Cres=B==toupper("Cresencia Village")
DagisanU=B==toupper("Dagsian, Upper")
Dizon=B==toupper("Dizon Subdivision")
Domin=B==toupper("Dominican Hill-Mirador")
Dontogan=B=="DONTOGAN"
DPS=B=="DPS AREA"
Ferdinand=B==toupper("Ferdinand (Happy Homes-Campo Sioco)")
GabSi=B=="GABRIELLA SILANG"
GenE=B==toupper("General Emilio F. Aguinaldo (Quirino-Magsaysay, Lower)")
GenLL=B==toupper("General Luna, Lower")
GenLU=B==toupper("General Luna, Upper")
Greenwater=B==toupper("Greenwater Village")
HHol=B==toupper("Happy Hollow")
HHom=B==toupper("Happy Homes (Happy Homes-Lucban)")
HCC=B==toupper("Harrison-Claudio Carantes")
Hillside=B==toupper("Hillside")
HGhostE=B==toupper("Holy Ghost Extension")
HGhostP=B==toupper("Holy Ghost Proper")
Honeymoon=B==toupper("Honeymoon (Honeymoon-Holy Ghost)")
IMarcos=B==toupper("Imelda R. Marcos (La Salle)")
Imelda=B==toupper("Imelda Village")
Kabayanihan=B==toupper("Kabayanihan")
Kagitingan=B==toupper("Kagitingan")
KayangHill=B==toupper("Kayang-Hilltop")
KayangE=B==toupper("Kayang Extension")
LBK=B==toupper("Legarda-Burnham-Kisad")
LourdesE=B==toupper("Lourdes Subdivision Extension")
LourdesL=B==toupper("Lourdes Subdivision, Lower")
LourdesP=B==toupper("Lourdes Subdivision, Proper")
Lualhati=B==toupper("Lualhati")
MagsaysayPR=B==toupper("Magsaysay Private Road")
MagsaysayL=B==toupper("Magsaysay, Lower")
MagsaysayU=B==toupper("Magsaysay, Upper")
MalcolmSquarePerfectoJoseAbadSantos=B==toupper("Malcolm Square-Perfecto (Jose Abad Santos)")
MarketSubdivisionUpper=B==toupper("Market Subdivision, Upper")
MiddleQuezonHillSubdivsion=B==toupper("Middle Quezon Hill Subdivsiion(Quezon Hill Middle)")
MinesView=B==toupper("Mines View Park")
ModernSiteE=B==toupper("Modern Site, East")
ModernSiteW=B==toupper("Modern Site, West")
NewLuc=B==toupper("New Lucban")
Outlook=B==toupper("Outlook Drive")
Pacdal=B==toupper("Pacdal")
PadreB=B==toupper("Padre Burgos")
PadreZ=B==toupper("Padre Zamora")
PU=B==toupper("Palma-Urbano (Cariño-Palma)")
PhilAm=B==toupper("Phil-Am")
pucsusan=B==toupper("Pucsusan")
QHillU=B==toupper("Quezon Hill, Upper")
UpperQM=B==toupper("Quirino-Magsaysay, Upper (Upper QM)")
QHillE=B==toupper("Quirino Hill, East")
QHillW=B==toupper("Quirino Hill, West")
RizalMonumentArea=B==toupper("Rizal Monument Area")
RockQuarryLower=B==toupper("Rock Quarry, Lower")
RockQuarryMiddle=B==toupper("Rock Quarry, Middle")
RockQuarryUpper=B==toupper("Rock Quarry, Upper")
SanAntonioVillage=B==toupper("San Antonio Village")
SanLuisVillage=B==toupper("San Luis Village")
SanRoqueVillage=B==toupper("San Roque Village")
SanitaryCampSouth=B==toupper("Sanitary Camp, South")
SantoR=B==toupper("Santo Rosario")
ScoutB=B==toupper("Scout Barrio")
Session=B==toupper("Session Road Area")
Slaughter=B==toupper("Slaughter House Area (Santo Niño Slaughter)")
SLU=B==toupper("SLU-SVP Housing Village")
SouthDrive=B==toupper("South Drive")
TeoAl=B==toupper("Teodora Alonzo")
tranco=B==toupper("Trancoville")
victoria=B==toupper("Victoria Village")
STJos=B=="ST JOSEPH VILLAGE"

tdf14 <- data.frame(MorbidityWeek=1,
                   ABonifacioCaguioaRimandoABCR=0,
                   AbanaoZanduetaKayongChugumOtekAZKCO=0,
                   AlfonsoTabora=0,
                   Ambiong=0,
                   AndresBonifacioLowerBokawkan=0,
                   ApuganLoakan=0,
                   AsinRoad=0,
                   AtokTrail=0,
                   AuroraHillProperMalvarSgtFloresca=0,
                   AuroraHillNorthCentral=0,
                   AuroraHillSouthCentral=0,
                   BagongLipunanMarketArea=0,
                   BakakengCentral=0,
                   BakakengNorth=1,
                   BalMarcovilleMarcoville=0,
                   Balsigan=0,
                   BayanParkEast=0,
                   BayanParkVillage=0,
                   BayanParkWestBayanPark=0,
                   BGHCompound=0,
                   Brookside=0,
                   Brookspoint=0,
                   CabinetHillTeachersCamp=0,
                   CamdasSubdivision=0,
                   Camp7=0,
                   Camp8=0,
                   CampAllen=0,
                   CampoFilipino=0,
                   CityCampCentral=0,
                   CityCampProper=0,
                   CountryClubVillage=0,
                   CresenciaVillage=0,
                   DagisanLower=0,
                   DagisanUpper=0,
                   DizonSubdivision=0,
                   DominicanHillMirador=0,
                   Dontogan=0,
                   DPSArea=0,
                   EngineersHill=0,
                   FairviewVillage=0,
                   FerdinandHappyHomesCampoSioco=0,
                   FortdelPilar=0,
                   GabrielaSilang=0,
                   GeneralEmilioFAguinaldoQuirinoMagsaysayLower=0,
                   GeneralLunaLower=0,
                   GeneralLunaUpper=0,
                   Gibraltar=0,
                   GreenwaterVillage=0,
                   GuisadCentral=0,
                   GuisadSorong=0,
                   HappyHollow=0,
                   HappyHomesHappyHomesLucban=0,
                   HarrisonClaudioCarantes=0,
                   Hillside=0,
                   HolyGhostExtension=0,
                   HolyGhostProper=0,
                   HoneymoonHoneymoonHolyGhost=0,
                   ImeldaRMarcosLaSalle=0,
                   ImeldaVillage=0,
                   Irisan=0,
                   Kabayanihan=0,
                   Kagitingan=0,
                   KayangHilltop=0,
                   KayangExtension=0,
                   Kias=0,
                   LegardaBurnhamKisad=0,
                   LiwanagLoakan=0,
                   LoakanProper=0,
                   LopezJaena=0,
                   LourdesSubdivisionExtension=0,
                   LourdesSubdivisionLower=0,
                   LourdesSubdivisionProper=0,
                   Lualhati=0,
                   Lucnab=0,
                   MagsaysayPrivateRoad=0,
                   MagsaysayLower=0,
                   MagsaysayUpper=0,
                   MalcolmSquarePerfectoJoseAbadSantos=0,
                   ManuelARoxas=0,
                   MarketSubdivisionUpper=0,
                   MiddleQuezonHillSubdivsion=0,
                   MilitaryCutoff=0,
                   MinesViewPark=0,
                   ModernSiteEast=0,
                   ModernSiteWest=0,
                   MRRQueenofPeace=0,
                   NewLucban=0,
                   OutlookDrive=0,
                   Pacdal=0,
                   PadreBurgos=0,
                   PadreZamora=0,
                   PalmaUrbanoCarinoPalma=0,
                   PhilAm=0,
                   Pinget=0,
                   PinsaoPilotProject=0,
                   PinsaoProper=0,
                   Poliwes=0,
                   Pucsusan=0,
                   QuezonHillProper=0,
                   QuezonHillUpper=0,
                   QuirinoMagsaysayUpperUpperQM=0,
                   QuirinoHillEast=0,
                   QuirinoHillLower=0,
                   QuirinoHillMiddle=0,
                   QuirinoHillWest=0,
                   RizalMonumentArea=0,
                   RockQuarryLower=0,
                   RockQuarryMiddle=0,
                   RockQuarryUpper=0,
                   SaludMitra=0,
                   SanAntoniVillage=0,
                   SanLuisVillage=0,
                   SanRoqueVillage=0,
                   SanVicente=0,
                   SanitaryCampNorth=0,
                   SanitaryCampSouth=0,
                   SantaEscolastica=0,
                   SantoRosario=0,
                   SantoTomasProper=0,
                   SantoTomasSchoolArea=0,
                   ScoutBarrio=0,
                   SessionRoadArea=0,
                   SlaughterHouseAreaSantoNiñoSlaughter=0,
                   SLUSVPHousingVillage=0,
                   SouthDrive=0,
                   TeodoraAlonzo=0,
                   Trancoville=0,
                   VictoriaVillage=0,
                   SaintJosephVillage=0
)

for (n in 2:52) {
  tdf14[nrow(tdf14)+1,]=c(n,
                      count(typhoid14[ABCR&typhoid14$CASECLASS=="Probable"&typhoid14$MorbidityWeek==n,])+
                        count(typhoid14[ABCR&typhoid14$CASECLASS=="PROBABLE"&typhoid14$MorbidityWeek==n,])+
                        count(typhoid14[ABCR&typhoid14$CASECLASS=="CONFIRMED"&typhoid14$MorbidityWeek==n,]),
                      count(typhoid14[B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")&SS=="Probable"&MW==n,])+
                        count(typhoid14[B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[B==toupper("Alfonso Tabora")&SS=="Probable"&MW==n,])+
                        count(typhoid14[B==toupper("Alfonso Tabora")&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[B==toupper("Alfonso Tabora")&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[B=="AMBIONG"&SS=="Probable"&MW==n,])+
                        count(typhoid14[B=="AMBIONG"&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[B=="AMBIONG"&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[AB&SS=="Probable"&MW==n,])+
                        count(typhoid14[AB&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[AB&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[AL&SS=="Probable"&MW==n,])+
                        count(typhoid14[AL&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[AL&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[Asin&SS=="Probable"&MW==n,])+
                        count(typhoid14[Asin&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[Asin&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[ATOK&SS=="Probable"&MW==n,])+
                        count(typhoid14[ATOK&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[ATOK&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[auroraP&SS=="Probable"&MW==n,])+
                        count(typhoid14[auroraP&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[auroraP&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[auroraN&SS=="Probable"&MW==n,])+
                        count(typhoid14[auroraN&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[auroraN&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[auroraS&SS=="Probable"&MW==n,])+
                        count(typhoid14[auroraS&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[auroraS&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[BL&SS=="Probable"&MW==n,])+
                        count(typhoid14[BL&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[BL&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[BakakengCentral&SS=="Probable"&MW==n,])+
                        count(typhoid14[BakakengCentral&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[BakakengCentral&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[BakakengNorth&SS=="Probable"&MW==n,])+
                        count(typhoid14[BakakengNorth&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[BakakengNorth&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[Bal&SS=="Probable"&MW==n,])+
                        count(typhoid14[Bal&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[Bal&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[Balsigan&SS=="Probable"&MW==n,])+
                        count(typhoid14[Balsigan&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[Balsigan&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[BPE&SS=="Probable"&MW==n,])+
                        count(typhoid14[BPE&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[BPE&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[BPV&SS=="Probable"&MW==n,])+
                        count(typhoid14[BPV&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[BPV&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[Bayan&SS=="Probable"&MW==n,])+
                        count(typhoid14[Bayan&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[Bayan&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[BGH&SS=="Probable"&MW==n,])+
                        count(typhoid14[BGH&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[BGH&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[Brookside&SS=="Probable"&MW==n,])+
                        count(typhoid14[Brookside&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[Brookside&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[Brookspoint&SS=="Probable"&MW==n,])+
                        count(typhoid14[Brookspoint&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[Brookspoint&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[CabinetHill&SS=="Probable"&MW==n,])+
                        count(typhoid14[CabinetHill&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[CabinetHill&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[Camdas&SS=="Probable"&MW==n,])+
                        count(typhoid14[Camdas&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[Camdas&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[c7&SS=="Probable"&MW==n,])+
                        count(typhoid14[c7&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[c7&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[c8&SS=="Probable"&MW==n,])+
                        count(typhoid14[c8&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[c8&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[ca&SS=="Probable"&MW==n,])+
                        count(typhoid14[ca&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[ca&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[cf&SS=="Probable"&MW==n,])+
                        count(typhoid14[cf&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[cf&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[CityCC&SS=="Probable"&MW==n,])+
                        count(typhoid14[CityCC&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[CityCC&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[CityCP&SS=="Probable"&MW==n,])+
                        count(typhoid14[CityCP&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[CityCP&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[ccv&SS=="Probable"&MW==n,])+
                        count(typhoid14[ccv&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[ccv&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[Cres&SS=="Probable"&MW==n,])+
                        count(typhoid14[Cres&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[Cres&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[DagisanL&SS=="Probable"&MW==n,])+
                        count(typhoid14[DagisanL&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[DagisanL&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[DagisanU&SS=="Probable"&MW==n,])+
                        count(typhoid14[DagisanU&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[DagisanU&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[Dizon&SS=="Probable"&MW==n,])+
                        count(typhoid14[Dizon&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[Dizon&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[Domin&SS=="Probable"&MW==n,])+
                        count(typhoid14[Domin&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[Domin&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[Dontogan&SS=="Probable"&MW==n,])+
                        count(typhoid14[Dontogan&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[Dontogan&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[DPS&SS=="Probable"&MW==n,])+
                        count(typhoid14[DPS&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[DPS&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[enghill&SS=="Probable"&MW==n,])+
                        count(typhoid14[enghill&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[enghill&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[fvill&SS=="Probable"&MW==n,])+
                        count(typhoid14[fvill&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[fvill&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[Ferdinand&SS=="Probable"&MW==n,])+
                        count(typhoid14[Ferdinand&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[Ferdinand&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[B=="FORT DEL PILAR"&SS=="Probable"&MW==n,])+
                        count(typhoid14[B=="FORT DEL PILAR"&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[B=="FORT DEL PILAR"&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[GabSi&SS=="Probable"&MW==n,])+
                        count(typhoid14[GabSi&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[GabSi&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[GenE&SS=="Probable"&MW==n,])+
                        count(typhoid14[GenE&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[GenE&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[GenLL&SS=="Probable"&MW==n,])+
                        count(typhoid14[GenLL&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[GenLL&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[GenLU&SS=="Probable"&MW==n,])+
                        count(typhoid14[GenLU&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[GenLU&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[Gib&SS=="Probable"&MW==n,])+
                        count(typhoid14[Gib&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[Gib&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[Greenwater&SS=="Probable"&MW==n,])+
                        count(typhoid14[Greenwater&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[Greenwater&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[GuisadC&SS=="Probable"&MW==n,])+
                        count(typhoid14[GuisadC&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[GuisadC&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[GuisadS&SS=="Probable"&MW==n,])+
                        count(typhoid14[GuisadS&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[GuisadS&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[HHol&SS=="Probable"&MW==n,])+
                        count(typhoid14[HHol&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[HHol&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[HHom&SS=="Probable"&MW==n,])+
                        count(typhoid14[HHom&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[HHom&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[HCC&SS=="Probable"&MW==n,])+
                        count(typhoid14[HCC&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[HCC&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[Hillside&SS=="Probable"&MW==n,])+
                        count(typhoid14[Hillside&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[Hillside&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[HGhostE&SS=="Probable"&MW==n,])+
                        count(typhoid14[HGhostE&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[HGhostE&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[HGhostP&SS=="Probable"&MW==n,])+
                        count(typhoid14[HGhostP&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[HGhostP&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[Honeymoon&SS=="Probable"&MW==n,])+
                        count(typhoid14[Honeymoon&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[Honeymoon&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[IMarcos&SS=="Probable"&MW==n,])+
                        count(typhoid14[IMarcos&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[IMarcos&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[Imelda&SS=="Probable"&MW==n,])+
                        count(typhoid14[Imelda&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[Imelda&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[B=="IRISAN"&SS=="Probable"&MW==n,])+
                        count(typhoid14[B=="IRISAN"&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[B=="IRISAN"&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[Kabayanihan&SS=="Probable"&MW==n,])+
                        count(typhoid14[Kabayanihan&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[Kabayanihan&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[Kagitingan&SS=="Probable"&MW==n,])+
                        count(typhoid14[Kagitingan&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[Kagitingan&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[KayangHill&SS=="Probable"&MW==n,])+
                        count(typhoid14[KayangHill&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[KayangHill&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[KayangE&SS=="Probable"&MW==n,])+
                        count(typhoid14[KayangE&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[KayangE&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[kias&SS=="Probable"&MW==n,])+
                        count(typhoid14[kias&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[kias&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[LBK&SS=="Probable"&MW==n,])+
                        count(typhoid14[LBK&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[LBK&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[LLoak&SS=="Probable"&MW==n,])+
                        count(typhoid14[LLoak&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[LLoak&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[LoakP&SS=="Probable"&MW==n,])+
                        count(typhoid14[LoakP&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[LoakP&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[LOPJ&SS=="Probable"&MW==n,])+
                        count(typhoid14[LOPJ&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[LOPJ&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[LourdesE&SS=="Probable"&MW==n,])+
                        count(typhoid14[LourdesE&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[LourdesE&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[LourdesL&SS=="Probable"&MW==n,])+
                        count(typhoid14[LourdesL&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[LourdesL&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[LourdesP&SS=="Probable"&MW==n,])+
                        count(typhoid14[LourdesP&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[LourdesP&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[Lualhati&SS=="Probable"&MW==n,])+
                        count(typhoid14[Lualhati&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[Lualhati&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[lucnab&SS=="Probable"&MW==n,])+
                        count(typhoid14[lucnab&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[lucnab&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[MagsaysayPR&SS=="Probable"&MW==n,])+
                        count(typhoid14[MagsaysayPR&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[MagsaysayPR&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[MagsaysayL&SS=="Probable"&MW==n,])+
                        count(typhoid14[MagsaysayL&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[MagsaysayL&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[MagsaysayU&SS=="Probable"&MW==n,])+
                        count(typhoid14[MagsaysayU&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[MagsaysayU&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[MalcolmSquarePerfectoJoseAbadSantos&SS=="Probable"&MW==n,])+
                        count(typhoid14[MalcolmSquarePerfectoJoseAbadSantos&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[MalcolmSquarePerfectoJoseAbadSantos&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[MR&SS=="Probable"&MW==n,])+
                        count(typhoid14[MR&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[MR&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[MarketSubdivisionUpper&SS=="Probable"&MW==n,])+
                        count(typhoid14[MarketSubdivisionUpper&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[MarketSubdivisionUpper&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[MiddleQuezonHillSubdivsion&SS=="Probable"&MW==n,])+
                        count(typhoid14[MiddleQuezonHillSubdivsion&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[MiddleQuezonHillSubdivsion&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[MCutOff&SS=="Probable"&MW==n,])+
                        count(typhoid14[MCutOff&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[MCutOff&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[MinesView&SS=="Probable"&MW==n,])+
                        count(typhoid14[MinesView&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[MinesView&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[ModernSiteE&SS=="Probable"&MW==n,])+
                        count(typhoid14[ModernSiteE&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[ModernSiteE&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[ModernSiteW&SS=="Probable"&MW==n,])+
                        count(typhoid14[ModernSiteW&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[ModernSiteW&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[MRR&SS=="Probable"&MW==n,])+
                        count(typhoid14[MRR&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[MRR&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[NewLuc&SS=="Probable"&MW==n,])+
                        count(typhoid14[NewLuc&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[NewLuc&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[Outlook&SS=="Probable"&MW==n,])+
                        count(typhoid14[Outlook&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[Outlook&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[Pacdal&SS=="Probable"&MW==n,])+
                        count(typhoid14[Pacdal&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[Pacdal&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[PadreB&SS=="Probable"&MW==n,])+
                        count(typhoid14[PadreB&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[PadreB&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[PadreZ&SS=="Probable"&MW==n,])+
                        count(typhoid14[PadreZ&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[PadreZ&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[PU&SS=="Probable"&MW==n,])+
                        count(typhoid14[PU&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[PU&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[PhilAm&SS=="Probable"&MW==n,])+
                        count(typhoid14[PhilAm&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[PhilAm&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[Pinget&SS=="Probable"&MW==n,])+
                        count(typhoid14[Pinget&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[Pinget&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[PPP&SS=="Probable"&MW==n,])+
                        count(typhoid14[PPP&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[PPP&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[PisaoP&SS=="Probable"&MW==n,])+
                        count(typhoid14[PisaoP&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[PisaoP&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[Poliwes&SS=="Probable"&MW==n,])+
                        count(typhoid14[Poliwes&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[Poliwes&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[pucsusan&SS=="Probable"&MW==n,])+
                        count(typhoid14[pucsusan&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[pucsusan&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[QHP&SS=="Probable"&MW==n,])+
                        count(typhoid14[QHP&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[QHP&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[QHillU&SS=="Probable"&MW==n,])+
                        count(typhoid14[QHillU&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[QHillU&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[UpperQM&SS=="Probable"&MW==n,])+
                        count(typhoid14[UpperQM&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[UpperQM&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[QHillE&SS=="Probable"&MW==n,])+
                        count(typhoid14[QHillE&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[QHillE&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[QHL&SS=="Probable"&MW==n,])+
                        count(typhoid14[QHL&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[QHL&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[QHM&SS=="Probable"&MW==n,])+
                        count(typhoid14[QHM&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[QHM&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[QHillW&SS=="Probable"&MW==n,])+
                        count(typhoid14[QHillW&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[QHillW&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[RizalMonumentArea&SS=="Probable"&MW==n,])+
                        count(typhoid14[RizalMonumentArea&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[RizalMonumentArea&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[RockQuarryLower&SS=="Probable"&MW==n,])+
                        count(typhoid14[RockQuarryLower&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[RockQuarryLower&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[RockQuarryMiddle&SS=="Probable"&MW==n,])+
                        count(typhoid14[RockQuarryMiddle&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[RockQuarryMiddle&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[RockQuarryUpper&SS=="Probable"&MW==n,])+
                        count(typhoid14[RockQuarryUpper&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[RockQuarryUpper&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[salud&SS=="Probable"&MW==n,])+
                        count(typhoid14[salud&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[salud&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[SanAntonioVillage&SS=="Probable"&MW==n,])+
                        count(typhoid14[SanAntonioVillage&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[SanAntonioVillage&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[SanLuisVillage&SS=="Probable"&MW==n,])+
                        count(typhoid14[SanLuisVillage&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[SanLuisVillage&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[SanRoqueVillage&SS=="Probable"&MW==n,])+
                        count(typhoid14[SanRoqueVillage&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[SanRoqueVillage&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[sanvic&SS=="Probable"&MW==n,])+
                        count(typhoid14[sanvic&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[sanvic&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[SanCamp&SS=="Probable"&MW==n,])+
                        count(typhoid14[SanCamp&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[SanCamp&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[SanitaryCampSouth&SS=="Probable"&MW==n,])+
                        count(typhoid14[SanitaryCampSouth&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[SanitaryCampSouth&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[STE&SS=="Probable"&MW==n,])+
                        count(typhoid14[STE&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[STE&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[SantoR&SS=="Probable"&MW==n,])+
                        count(typhoid14[SantoR&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[SantoR&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[STP&SS=="Probable"&MW==n,])+
                        count(typhoid14[STP&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[STP&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[STsa&SS=="Probable"&MW==n,])+
                        count(typhoid14[STsa&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[STsa&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[ScoutB&SS=="Probable"&MW==n,])+
                        count(typhoid14[ScoutB&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[ScoutB&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[Session&SS=="Probable"&MW==n,])+
                        count(typhoid14[Session&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[Session&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[Slaughter&SS=="Probable"&MW==n,])+
                        count(typhoid14[Slaughter&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[Slaughter&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[SLU&SS=="Probable"&MW==n,])+
                        count(typhoid14[SLU&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[SLU&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[SouthDrive&SS=="Probable"&MW==n,])+
                        count(typhoid14[SouthDrive&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[SouthDrive&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[TeoAl&SS=="Probable"&MW==n,])+
                        count(typhoid14[TeoAl&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[TeoAl&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[tranco&SS=="Probable"&MW==n,])+
                        count(typhoid14[tranco&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[tranco&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[victoria&SS=="Probable"&MW==n,])+
                        count(typhoid14[victoria&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[victoria&SS=="CONFIRMED"&MW==n,]),
                      count(typhoid14[STJos&SS=="Probable"&MW==n,])+
                        count(typhoid14[STJos&SS=="PROBABLE"&MW==n,])+
                        count(typhoid14[STJos&SS=="CONFIRMED"&MW==n,])
  )
}

B=typhoid15$Barangay
SS=typhoid15$CASECLASS
MW=typhoid15$MorbidityWeek

Imelda = B=="IMELDA VILLAGE"
BakakengCentral = B=="BAKAKENG CENTRAL"
ABCR = B=="A. BONIFACIO-CAGUIOA-RIMANDO (ABCR)"
Ambiong = B=="AMBIONG"
BakakengNorth = B=="BAKAKENG NORTH"
Asin = B=="ASIN"
Balsigan = B=="BALSIGAN"
Bayan = B=="BAYAN PARK WEST (BAYAN PARK)"
BGH=B=="BGH COMPOUND"
Brookside = B=="BROOKSIDE"
Camdas = B=="CAMDAS SUBDIVISION"
c7=B=="CAMP 7"
c8=B=="CAMP 8"
ca=B=="CAMP ALLEN"
cf=B=="CAMPO FILIPINO"
ccv=B=="COUNTRY CLUB VILLAGE"
DPS=B=="DPS AREA"
enghill=B=="ENGINEER'S HILL"
fvill=B=="FAIRVIEW VILLAGE"
GLuna=B=="GENERAL LUNA, UPPER"
Gib=B=="GIBRALTAR"
GuisadC=B=="GUISAD CENTRAL"
GuisadS=B=="GUISAD SORONG"
HHL=B=="HAPPY HOMES (HAPPY HOMES-LUCBAN)"
kias=B=="KIAS"
LLoak=B=="LIWANAG-LOAKAN"
LoakP=B=="LOAKAN PROPER"
LOPJ=B=="LOPEZ JAENA"
lucnab=B=="LUCNAB"
MR=B=="MANUEL A. ROXAS"
MCutOff=B=="MILITARY CUT-OFF"
MRR=B=="MRR-QUEEN OF PEACE"
PadreZ=B=="PADRE ZAMORA"
Pinget=B=="PINGET"
PPP=B=="PINSAO PILOT PROJECT"
PisaoP=B=="PINSAO PROPER"
Poliwes=B=="POLIWES"
QHP=B=="QUEZON HILL PROPER"
QHL=B=="QUIRINO HILL, LOWER"
QHM=B=="QUIRINO HILL, MIDDLE"
salud=B=="SALUD MITRA"
sanvic=B=="SAN VICENTE"
SanCamp=B=="SANITARY CAMP, NORTH"
STE=B=="SANTA ESCOLASTICA"
STP=B=="SANTO TOMAS PROPER"&B=="STO. TOMAS PROPER"
STsa=B=="SANTO TOMAS SCHOOL AREA"
tranc=B=="TRANCOVILLE"
birac=B=="BIRAC"
AL=B=="APUGAN-LOAKAN"
AZKCO=B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")
ATAB=B==toupper("Alfonso Tabora")
AB=B==toupper("Andres Bonifacio (Lower Bokawkan)")
ATOK=B==toupper("Atok Trail")
auroraP=B==toupper("Aurora Hill Proper (Malvar-Sgt. Floresca)")
auroraN=B==toupper("Aurora Hill, North Central")
auroraS=B==toupper("Aurora Hill, South Central")
BL=B==toupper("Bagong Lipunan (Market Area)")
Bal=B==toupper("Bal-Marcoville (Marcoville)")
BPE=B==toupper("Bayan Park East")
BPV=B==toupper("Bayan Park Village")
Brookspoint=B==toupper("Brookspoint")
CabinetHill=B==toupper("Cabinet Hill-Teacher's Camp")
CityCC=B==toupper("City Camp Central")
CityCP=B==toupper("City Camp Proper")
DagisanL=B==toupper("Dagsian, Lower")
Cres=B==toupper("Cresencia Village")
DagisanU=B==toupper("Dagsian, Upper")
Dizon=B==toupper("Dizon Subdivision")
Domin=B==toupper("Dominican Hill-Mirador")
Dontogan=B=="DONTOGAN"
DPS=B=="DPS AREA"
Ferdinand=B==toupper("Ferdinand (Happy Homes-Campo Sioco)")
GabSi=B=="GABRIELLA SILANG"
GenE=B==toupper("General Emilio F. Aguinaldo (Quirino-Magsaysay, Lower)")
GenLL=B==toupper("General Luna, Lower")
GenLU=B==toupper("General Luna, Upper")
Greenwater=B==toupper("Greenwater Village")
HHol=B==toupper("Happy Hollow")
HHom=B==toupper("Happy Homes (Happy Homes-Lucban)")
HCC=B==toupper("Harrison-Claudio Carantes")
Hillside=B==toupper("Hillside")
HGhostE=B==toupper("Holy Ghost Extension")
HGhostP=B==toupper("Holy Ghost Proper")
Honeymoon=B==toupper("Honeymoon (Honeymoon-Holy Ghost)")
IMarcos=B==toupper("Imelda R. Marcos (La Salle)")
Imelda=B==toupper("Imelda Village")
Kabayanihan=B==toupper("Kabayanihan")
Kagitingan=B==toupper("Kagitingan")
KayangHill=B==toupper("Kayang-Hilltop")
KayangE=B==toupper("Kayang Extension")
LBK=B==toupper("Legarda-Burnham-Kisad")
LourdesE=B==toupper("Lourdes Subdivision Extension")
LourdesL=B==toupper("Lourdes Subdivision, Lower")
LourdesP=B==toupper("Lourdes Subdivision, Proper")
Lualhati=B==toupper("Lualhati")
MagsaysayPR=B==toupper("Magsaysay Private Road")
MagsaysayL=B==toupper("Magsaysay, Lower")
MagsaysayU=B==toupper("Magsaysay, Upper")
MalcolmSquarePerfectoJoseAbadSantos=B==toupper("Malcolm Square-Perfecto (Jose Abad Santos)")
MarketSubdivisionUpper=B==toupper("Market Subdivision, Upper")
MiddleQuezonHillSubdivsion=B==toupper("Middle Quezon Hill Subdivsiion(Quezon Hill Middle)")
MinesView=B==toupper("Mines View Park")
ModernSiteE=B==toupper("Modern Site, East")
ModernSiteW=B==toupper("Modern Site, West")
NewLuc=B==toupper("New Lucban")
Outlook=B==toupper("Outlook Drive")
Pacdal=B==toupper("Pacdal")
PadreB=B==toupper("Padre Burgos")
PadreZ=B==toupper("Padre Zamora")
PU=B==toupper("Palma-Urbano (Cariño-Palma)")
PhilAm=B==toupper("Phil-Am")
pucsusan=B==toupper("Pucsusan")
QHillU=B==toupper("Quezon Hill, Upper")
UpperQM=B==toupper("Quirino-Magsaysay, Upper (Upper QM)")
QHillE=B==toupper("Quirino Hill, East")
QHillW=B==toupper("Quirino Hill, West")
RizalMonumentArea=B==toupper("Rizal Monument Area")
RockQuarryLower=B==toupper("Rock Quarry, Lower")
RockQuarryMiddle=B==toupper("Rock Quarry, Middle")
RockQuarryUpper=B==toupper("Rock Quarry, Upper")
SanAntonioVillage=B==toupper("San Antonio Village")
SanLuisVillage=B==toupper("San Luis Village")
SanRoqueVillage=B==toupper("San Roque Village")
SanitaryCampSouth=B==toupper("Sanitary Camp, South")
SantoR=B==toupper("Santo Rosario")
ScoutB=B==toupper("Scout Barrio")
Session=B==toupper("Session Road Area")
Slaughter=B==toupper("Slaughter House Area (Santo Niño Slaughter)")
SLU=B==toupper("SLU-SVP Housing Village")
SouthDrive=B==toupper("South Drive")
TeoAl=B==toupper("Teodora Alonzo")
tranco=B==toupper("Trancoville")
victoria=B==toupper("Victoria Village")
STJos=B=="ST JOSEPH VILLAGE"

tdf15 <- data.frame(MorbidityWeek=1,
                   ABonifacioCaguioaRimandoABCR=0,
                   AbanaoZanduetaKayongChugumOtekAZKCO=0,
                   AlfonsoTabora=0,
                   Ambiong=0,
                   AndresBonifacioLowerBokawkan=0,
                   ApuganLoakan=0,
                   AsinRoad=0,
                   AtokTrail=0,
                   AuroraHillProperMalvarSgtFloresca=0,
                   AuroraHillNorthCentral=0,
                   AuroraHillSouthCentral=0,
                   BagongLipunanMarketArea=0,
                   BakakengCentral=0,
                   BakakengNorth=1,
                   BalMarcovilleMarcoville=0,
                   Balsigan=0,
                   BayanParkEast=0,
                   BayanParkVillage=0,
                   BayanParkWestBayanPark=0,
                   BGHCompound=0,
                   Brookside=0,
                   Brookspoint=0,
                   CabinetHillTeachersCamp=0,
                   CamdasSubdivision=0,
                   Camp7=0,
                   Camp8=0,
                   CampAllen=0,
                   CampoFilipino=0,
                   CityCampCentral=0,
                   CityCampProper=0,
                   CountryClubVillage=0,
                   CresenciaVillage=0,
                   DagisanLower=0,
                   DagisanUpper=0,
                   DizonSubdivision=0,
                   DominicanHillMirador=0,
                   Dontogan=0,
                   DPSArea=0,
                   EngineersHill=0,
                   FairviewVillage=0,
                   FerdinandHappyHomesCampoSioco=0,
                   FortdelPilar=0,
                   GabrielaSilang=0,
                   GeneralEmilioFAguinaldoQuirinoMagsaysayLower=0,
                   GeneralLunaLower=0,
                   GeneralLunaUpper=0,
                   Gibraltar=0,
                   GreenwaterVillage=0,
                   GuisadCentral=0,
                   GuisadSorong=0,
                   HappyHollow=0,
                   HappyHomesHappyHomesLucban=0,
                   HarrisonClaudioCarantes=0,
                   Hillside=0,
                   HolyGhostExtension=0,
                   HolyGhostProper=0,
                   HoneymoonHoneymoonHolyGhost=0,
                   ImeldaRMarcosLaSalle=0,
                   ImeldaVillage=0,
                   Irisan=0,
                   Kabayanihan=0,
                   Kagitingan=0,
                   KayangHilltop=0,
                   KayangExtension=0,
                   Kias=0,
                   LegardaBurnhamKisad=0,
                   LiwanagLoakan=0,
                   LoakanProper=0,
                   LopezJaena=0,
                   LourdesSubdivisionExtension=0,
                   LourdesSubdivisionLower=0,
                   LourdesSubdivisionProper=0,
                   Lualhati=0,
                   Lucnab=0,
                   MagsaysayPrivateRoad=0,
                   MagsaysayLower=0,
                   MagsaysayUpper=0,
                   MalcolmSquarePerfectoJoseAbadSantos=0,
                   ManuelARoxas=0,
                   MarketSubdivisionUpper=0,
                   MiddleQuezonHillSubdivsion=0,
                   MilitaryCutoff=0,
                   MinesViewPark=0,
                   ModernSiteEast=0,
                   ModernSiteWest=0,
                   MRRQueenofPeace=0,
                   NewLucban=0,
                   OutlookDrive=0,
                   Pacdal=0,
                   PadreBurgos=0,
                   PadreZamora=0,
                   PalmaUrbanoCarinoPalma=0,
                   PhilAm=0,
                   Pinget=0,
                   PinsaoPilotProject=0,
                   PinsaoProper=0,
                   Poliwes=0,
                   Pucsusan=0,
                   QuezonHillProper=0,
                   QuezonHillUpper=0,
                   QuirinoMagsaysayUpperUpperQM=0,
                   QuirinoHillEast=0,
                   QuirinoHillLower=0,
                   QuirinoHillMiddle=0,
                   QuirinoHillWest=0,
                   RizalMonumentArea=0,
                   RockQuarryLower=0,
                   RockQuarryMiddle=0,
                   RockQuarryUpper=0,
                   SaludMitra=0,
                   SanAntoniVillage=0,
                   SanLuisVillage=0,
                   SanRoqueVillage=0,
                   SanVicente=0,
                   SanitaryCampNorth=0,
                   SanitaryCampSouth=0,
                   SantaEscolastica=0,
                   SantoRosario=0,
                   SantoTomasProper=0,
                   SantoTomasSchoolArea=0,
                   ScoutBarrio=0,
                   SessionRoadArea=0,
                   SlaughterHouseAreaSantoNiñoSlaughter=0,
                   SLUSVPHousingVillage=0,
                   SouthDrive=0,
                   TeodoraAlonzo=0,
                   Trancoville=0,
                   VictoriaVillage=0,
                   SaintJosephVillage=0
)

for (n in 2:52) {
  tdf15[nrow(tdf15)+1,]=c(n,
                        count(typhoid15[ABCR&typhoid15$CASECLASS=="Probable"&typhoid15$MorbidityWeek==n,])+
                          count(typhoid15[ABCR&typhoid15$CASECLASS=="PROBABLE"&typhoid15$MorbidityWeek==n,])+
                          count(typhoid15[ABCR&typhoid15$CASECLASS=="CONFIRMED"&typhoid15$MorbidityWeek==n,]),
                        count(typhoid15[B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")&SS=="Probable"&MW==n,])+
                          count(typhoid15[B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[B==toupper("Alfonso Tabora")&SS=="Probable"&MW==n,])+
                          count(typhoid15[B==toupper("Alfonso Tabora")&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[B==toupper("Alfonso Tabora")&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[B=="AMBIONG"&SS=="Probable"&MW==n,])+
                          count(typhoid15[B=="AMBIONG"&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[B=="AMBIONG"&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[AB&SS=="Probable"&MW==n,])+
                          count(typhoid15[AB&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[AB&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[AL&SS=="Probable"&MW==n,])+
                          count(typhoid15[AL&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[AL&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[Asin&SS=="Probable"&MW==n,])+
                          count(typhoid15[Asin&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[Asin&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[ATOK&SS=="Probable"&MW==n,])+
                          count(typhoid15[ATOK&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[ATOK&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[auroraP&SS=="Probable"&MW==n,])+
                          count(typhoid15[auroraP&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[auroraP&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[auroraN&SS=="Probable"&MW==n,])+
                          count(typhoid15[auroraN&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[auroraN&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[auroraS&SS=="Probable"&MW==n,])+
                          count(typhoid15[auroraS&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[auroraS&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[BL&SS=="Probable"&MW==n,])+
                          count(typhoid15[BL&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[BL&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[BakakengCentral&SS=="Probable"&MW==n,])+
                          count(typhoid15[BakakengCentral&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[BakakengCentral&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[BakakengNorth&SS=="Probable"&MW==n,])+
                          count(typhoid15[BakakengNorth&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[BakakengNorth&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[Bal&SS=="Probable"&MW==n,])+
                          count(typhoid15[Bal&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[Bal&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[Balsigan&SS=="Probable"&MW==n,])+
                          count(typhoid15[Balsigan&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[Balsigan&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[BPE&SS=="Probable"&MW==n,])+
                          count(typhoid15[BPE&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[BPE&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[BPV&SS=="Probable"&MW==n,])+
                          count(typhoid15[BPV&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[BPV&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[Bayan&SS=="Probable"&MW==n,])+
                          count(typhoid15[Bayan&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[Bayan&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[BGH&SS=="Probable"&MW==n,])+
                          count(typhoid15[BGH&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[BGH&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[Brookside&SS=="Probable"&MW==n,])+
                          count(typhoid15[Brookside&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[Brookside&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[Brookspoint&SS=="Probable"&MW==n,])+
                          count(typhoid15[Brookspoint&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[Brookspoint&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[CabinetHill&SS=="Probable"&MW==n,])+
                          count(typhoid15[CabinetHill&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[CabinetHill&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[Camdas&SS=="Probable"&MW==n,])+
                          count(typhoid15[Camdas&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[Camdas&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[c7&SS=="Probable"&MW==n,])+
                          count(typhoid15[c7&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[c7&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[c8&SS=="Probable"&MW==n,])+
                          count(typhoid15[c8&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[c8&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[ca&SS=="Probable"&MW==n,])+
                          count(typhoid15[ca&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[ca&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[cf&SS=="Probable"&MW==n,])+
                          count(typhoid15[cf&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[cf&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[CityCC&SS=="Probable"&MW==n,])+
                          count(typhoid15[CityCC&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[CityCC&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[CityCP&SS=="Probable"&MW==n,])+
                          count(typhoid15[CityCP&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[CityCP&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[ccv&SS=="Probable"&MW==n,])+
                          count(typhoid15[ccv&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[ccv&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[Cres&SS=="Probable"&MW==n,])+
                          count(typhoid15[Cres&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[Cres&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[DagisanL&SS=="Probable"&MW==n,])+
                          count(typhoid15[DagisanL&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[DagisanL&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[DagisanU&SS=="Probable"&MW==n,])+
                          count(typhoid15[DagisanU&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[DagisanU&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[Dizon&SS=="Probable"&MW==n,])+
                          count(typhoid15[Dizon&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[Dizon&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[Domin&SS=="Probable"&MW==n,])+
                          count(typhoid15[Domin&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[Domin&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[Dontogan&SS=="Probable"&MW==n,])+
                          count(typhoid15[Dontogan&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[Dontogan&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[DPS&SS=="Probable"&MW==n,])+
                          count(typhoid15[DPS&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[DPS&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[enghill&SS=="Probable"&MW==n,])+
                          count(typhoid15[enghill&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[enghill&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[fvill&SS=="Probable"&MW==n,])+
                          count(typhoid15[fvill&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[fvill&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[Ferdinand&SS=="Probable"&MW==n,])+
                          count(typhoid15[Ferdinand&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[Ferdinand&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[B=="FORT DEL PILAR"&SS=="Probable"&MW==n,])+
                          count(typhoid15[B=="FORT DEL PILAR"&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[B=="FORT DEL PILAR"&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[GabSi&SS=="Probable"&MW==n,])+
                          count(typhoid15[GabSi&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[GabSi&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[GenE&SS=="Probable"&MW==n,])+
                          count(typhoid15[GenE&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[GenE&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[GenLL&SS=="Probable"&MW==n,])+
                          count(typhoid15[GenLL&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[GenLL&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[GenLU&SS=="Probable"&MW==n,])+
                          count(typhoid15[GenLU&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[GenLU&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[Gib&SS=="Probable"&MW==n,])+
                          count(typhoid15[Gib&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[Gib&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[Greenwater&SS=="Probable"&MW==n,])+
                          count(typhoid15[Greenwater&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[Greenwater&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[GuisadC&SS=="Probable"&MW==n,])+
                          count(typhoid15[GuisadC&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[GuisadC&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[GuisadS&SS=="Probable"&MW==n,])+
                          count(typhoid15[GuisadS&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[GuisadS&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[HHol&SS=="Probable"&MW==n,])+
                          count(typhoid15[HHol&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[HHol&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[HHom&SS=="Probable"&MW==n,])+
                          count(typhoid15[HHom&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[HHom&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[HCC&SS=="Probable"&MW==n,])+
                          count(typhoid15[HCC&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[HCC&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[Hillside&SS=="Probable"&MW==n,])+
                          count(typhoid15[Hillside&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[Hillside&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[HGhostE&SS=="Probable"&MW==n,])+
                          count(typhoid15[HGhostE&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[HGhostE&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[HGhostP&SS=="Probable"&MW==n,])+
                          count(typhoid15[HGhostP&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[HGhostP&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[Honeymoon&SS=="Probable"&MW==n,])+
                          count(typhoid15[Honeymoon&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[Honeymoon&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[IMarcos&SS=="Probable"&MW==n,])+
                          count(typhoid15[IMarcos&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[IMarcos&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[Imelda&SS=="Probable"&MW==n,])+
                          count(typhoid15[Imelda&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[Imelda&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[B=="IRISAN"&SS=="Probable"&MW==n,])+
                          count(typhoid15[B=="IRISAN"&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[B=="IRISAN"&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[Kabayanihan&SS=="Probable"&MW==n,])+
                          count(typhoid15[Kabayanihan&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[Kabayanihan&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[Kagitingan&SS=="Probable"&MW==n,])+
                          count(typhoid15[Kagitingan&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[Kagitingan&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[KayangHill&SS=="Probable"&MW==n,])+
                          count(typhoid15[KayangHill&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[KayangHill&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[KayangE&SS=="Probable"&MW==n,])+
                          count(typhoid15[KayangE&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[KayangE&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[kias&SS=="Probable"&MW==n,])+
                          count(typhoid15[kias&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[kias&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[LBK&SS=="Probable"&MW==n,])+
                          count(typhoid15[LBK&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[LBK&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[LLoak&SS=="Probable"&MW==n,])+
                          count(typhoid15[LLoak&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[LLoak&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[LoakP&SS=="Probable"&MW==n,])+
                          count(typhoid15[LoakP&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[LoakP&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[LOPJ&SS=="Probable"&MW==n,])+
                          count(typhoid15[LOPJ&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[LOPJ&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[LourdesE&SS=="Probable"&MW==n,])+
                          count(typhoid15[LourdesE&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[LourdesE&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[LourdesL&SS=="Probable"&MW==n,])+
                          count(typhoid15[LourdesL&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[LourdesL&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[LourdesP&SS=="Probable"&MW==n,])+
                          count(typhoid15[LourdesP&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[LourdesP&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[Lualhati&SS=="Probable"&MW==n,])+
                          count(typhoid15[Lualhati&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[Lualhati&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[lucnab&SS=="Probable"&MW==n,])+
                          count(typhoid15[lucnab&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[lucnab&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[MagsaysayPR&SS=="Probable"&MW==n,])+
                          count(typhoid15[MagsaysayPR&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[MagsaysayPR&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[MagsaysayL&SS=="Probable"&MW==n,])+
                          count(typhoid15[MagsaysayL&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[MagsaysayL&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[MagsaysayU&SS=="Probable"&MW==n,])+
                          count(typhoid15[MagsaysayU&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[MagsaysayU&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[MalcolmSquarePerfectoJoseAbadSantos&SS=="Probable"&MW==n,])+
                          count(typhoid15[MalcolmSquarePerfectoJoseAbadSantos&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[MalcolmSquarePerfectoJoseAbadSantos&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[MR&SS=="Probable"&MW==n,])+
                          count(typhoid15[MR&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[MR&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[MarketSubdivisionUpper&SS=="Probable"&MW==n,])+
                          count(typhoid15[MarketSubdivisionUpper&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[MarketSubdivisionUpper&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[MiddleQuezonHillSubdivsion&SS=="Probable"&MW==n,])+
                          count(typhoid15[MiddleQuezonHillSubdivsion&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[MiddleQuezonHillSubdivsion&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[MCutOff&SS=="Probable"&MW==n,])+
                          count(typhoid15[MCutOff&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[MCutOff&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[MinesView&SS=="Probable"&MW==n,])+
                          count(typhoid15[MinesView&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[MinesView&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[ModernSiteE&SS=="Probable"&MW==n,])+
                          count(typhoid15[ModernSiteE&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[ModernSiteE&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[ModernSiteW&SS=="Probable"&MW==n,])+
                          count(typhoid15[ModernSiteW&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[ModernSiteW&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[MRR&SS=="Probable"&MW==n,])+
                          count(typhoid15[MRR&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[MRR&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[NewLuc&SS=="Probable"&MW==n,])+
                          count(typhoid15[NewLuc&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[NewLuc&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[Outlook&SS=="Probable"&MW==n,])+
                          count(typhoid15[Outlook&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[Outlook&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[Pacdal&SS=="Probable"&MW==n,])+
                          count(typhoid15[Pacdal&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[Pacdal&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[PadreB&SS=="Probable"&MW==n,])+
                          count(typhoid15[PadreB&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[PadreB&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[PadreZ&SS=="Probable"&MW==n,])+
                          count(typhoid15[PadreZ&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[PadreZ&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[PU&SS=="Probable"&MW==n,])+
                          count(typhoid15[PU&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[PU&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[PhilAm&SS=="Probable"&MW==n,])+
                          count(typhoid15[PhilAm&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[PhilAm&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[Pinget&SS=="Probable"&MW==n,])+
                          count(typhoid15[Pinget&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[Pinget&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[PPP&SS=="Probable"&MW==n,])+
                          count(typhoid15[PPP&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[PPP&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[PisaoP&SS=="Probable"&MW==n,])+
                          count(typhoid15[PisaoP&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[PisaoP&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[Poliwes&SS=="Probable"&MW==n,])+
                          count(typhoid15[Poliwes&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[Poliwes&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[pucsusan&SS=="Probable"&MW==n,])+
                          count(typhoid15[pucsusan&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[pucsusan&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[QHP&SS=="Probable"&MW==n,])+
                          count(typhoid15[QHP&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[QHP&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[QHillU&SS=="Probable"&MW==n,])+
                          count(typhoid15[QHillU&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[QHillU&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[UpperQM&SS=="Probable"&MW==n,])+
                          count(typhoid15[UpperQM&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[UpperQM&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[QHillE&SS=="Probable"&MW==n,])+
                          count(typhoid15[QHillE&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[QHillE&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[QHL&SS=="Probable"&MW==n,])+
                          count(typhoid15[QHL&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[QHL&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[QHM&SS=="Probable"&MW==n,])+
                          count(typhoid15[QHM&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[QHM&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[QHillW&SS=="Probable"&MW==n,])+
                          count(typhoid15[QHillW&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[QHillW&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[RizalMonumentArea&SS=="Probable"&MW==n,])+
                          count(typhoid15[RizalMonumentArea&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[RizalMonumentArea&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[RockQuarryLower&SS=="Probable"&MW==n,])+
                          count(typhoid15[RockQuarryLower&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[RockQuarryLower&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[RockQuarryMiddle&SS=="Probable"&MW==n,])+
                          count(typhoid15[RockQuarryMiddle&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[RockQuarryMiddle&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[RockQuarryUpper&SS=="Probable"&MW==n,])+
                          count(typhoid15[RockQuarryUpper&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[RockQuarryUpper&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[salud&SS=="Probable"&MW==n,])+
                          count(typhoid15[salud&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[salud&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[SanAntonioVillage&SS=="Probable"&MW==n,])+
                          count(typhoid15[SanAntonioVillage&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[SanAntonioVillage&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[SanLuisVillage&SS=="Probable"&MW==n,])+
                          count(typhoid15[SanLuisVillage&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[SanLuisVillage&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[SanRoqueVillage&SS=="Probable"&MW==n,])+
                          count(typhoid15[SanRoqueVillage&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[SanRoqueVillage&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[sanvic&SS=="Probable"&MW==n,])+
                          count(typhoid15[sanvic&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[sanvic&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[SanCamp&SS=="Probable"&MW==n,])+
                          count(typhoid15[SanCamp&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[SanCamp&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[SanitaryCampSouth&SS=="Probable"&MW==n,])+
                          count(typhoid15[SanitaryCampSouth&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[SanitaryCampSouth&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[STE&SS=="Probable"&MW==n,])+
                          count(typhoid15[STE&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[STE&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[SantoR&SS=="Probable"&MW==n,])+
                          count(typhoid15[SantoR&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[SantoR&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[STP&SS=="Probable"&MW==n,])+
                          count(typhoid15[STP&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[STP&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[STsa&SS=="Probable"&MW==n,])+
                          count(typhoid15[STsa&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[STsa&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[ScoutB&SS=="Probable"&MW==n,])+
                          count(typhoid15[ScoutB&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[ScoutB&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[Session&SS=="Probable"&MW==n,])+
                          count(typhoid15[Session&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[Session&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[Slaughter&SS=="Probable"&MW==n,])+
                          count(typhoid15[Slaughter&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[Slaughter&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[SLU&SS=="Probable"&MW==n,])+
                          count(typhoid15[SLU&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[SLU&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[SouthDrive&SS=="Probable"&MW==n,])+
                          count(typhoid15[SouthDrive&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[SouthDrive&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[TeoAl&SS=="Probable"&MW==n,])+
                          count(typhoid15[TeoAl&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[TeoAl&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[tranco&SS=="Probable"&MW==n,])+
                          count(typhoid15[tranco&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[tranco&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[victoria&SS=="Probable"&MW==n,])+
                          count(typhoid15[victoria&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[victoria&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid15[STJos&SS=="Probable"&MW==n,])+
                          count(typhoid15[STJos&SS=="PROBABLE"&MW==n,])+
                          count(typhoid15[STJos&SS=="CONFIRMED"&MW==n,])
  )
}

B=typhoid16$Barangay
SS=typhoid16$CASECLASS
MW=typhoid16$MorbidityWeek

Imelda = B=="IMELDA VILLAGE"
BakakengCentral = B=="BAKAKENG CENTRAL"
ABCR = B=="A. BONIFACIO-CAGUIOA-RIMANDO (ABCR)"
Ambiong = B=="AMBIONG"
BakakengNorth = B=="BAKAKENG NORTH"
Asin = B=="ASIN"
Balsigan = B=="BALSIGAN"
Bayan = B=="BAYAN PARK WEST (BAYAN PARK)"
BGH=B=="BGH COMPOUND"
Brookside = B=="BROOKSIDE"
Camdas = B=="CAMDAS SUBDIVISION"
c7=B=="CAMP 7"
c8=B=="CAMP 8"
ca=B=="CAMP ALLEN"
cf=B=="CAMPO FILIPINO"
ccv=B=="COUNTRY CLUB VILLAGE"
DPS=B=="DPS AREA"
enghill=B=="ENGINEER'S HILL"
fvill=B=="FAIRVIEW VILLAGE"
GLuna=B=="GENERAL LUNA, UPPER"
Gib=B=="GIBRALTAR"
GuisadC=B=="GUISAD CENTRAL"
GuisadS=B=="GUISAD SORONG"
HHL=B=="HAPPY HOMES (HAPPY HOMES-LUCBAN)"
kias=B=="KIAS"
LLoak=B=="LIWANAG-LOAKAN"
LoakP=B=="LOAKAN PROPER"
LOPJ=B=="LOPEZ JAENA"
lucnab=B=="LUCNAB"
MR=B=="MANUEL A. ROXAS"
MCutOff=B=="MILITARY CUT-OFF"
MRR=B=="MRR-QUEEN OF PEACE"
PadreZ=B=="PADRE ZAMORA"
Pinget=B=="PINGET"
PPP=B=="PINSAO PILOT PROJECT"
PisaoP=B=="PINSAO PROPER"
Poliwes=B=="POLIWES"
QHP=B=="QUEZON HILL PROPER"
QHL=B=="QUIRINO HILL, LOWER"
QHM=B=="QUIRINO HILL, MIDDLE"
salud=B=="SALUD MITRA"
sanvic=B=="SAN VICENTE"
SanCamp=B=="SANITARY CAMP, NORTH"
STE=B=="SANTA ESCOLASTICA"
STP=B=="SANTO TOMAS PROPER"&B=="STO. TOMAS PROPER"
STsa=B=="SANTO TOMAS SCHOOL AREA"
tranc=B=="TRANCOVILLE"
birac=B=="BIRAC"
AL=B=="APUGAN-LOAKAN"
AZKCO=B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")
ATAB=B==toupper("Alfonso Tabora")
AB=B==toupper("Andres Bonifacio (Lower Bokawkan)")
ATOK=B==toupper("Atok Trail")
auroraP=B==toupper("Aurora Hill Proper (Malvar-Sgt. Floresca)")
auroraN=B==toupper("Aurora Hill, North Central")
auroraS=B==toupper("Aurora Hill, South Central")
BL=B==toupper("Bagong Lipunan (Market Area)")
Bal=B==toupper("Bal-Marcoville (Marcoville)")
BPE=B==toupper("Bayan Park East")
BPV=B==toupper("Bayan Park Village")
Brookspoint=B==toupper("Brookspoint")
CabinetHill=B==toupper("Cabinet Hill-Teacher's Camp")
CityCC=B==toupper("City Camp Central")
CityCP=B==toupper("City Camp Proper")
DagisanL=B==toupper("Dagsian, Lower")
Cres=B==toupper("Cresencia Village")
DagisanU=B==toupper("Dagsian, Upper")
Dizon=B==toupper("Dizon Subdivision")
Domin=B==toupper("Dominican Hill-Mirador")
Dontogan=B=="DONTOGAN"
DPS=B=="DPS AREA"
Ferdinand=B==toupper("Ferdinand (Happy Homes-Campo Sioco)")
GabSi=B=="GABRIELLA SILANG"
GenE=B==toupper("General Emilio F. Aguinaldo (Quirino-Magsaysay, Lower)")
GenLL=B==toupper("General Luna, Lower")
GenLU=B==toupper("General Luna, Upper")
Greenwater=B==toupper("Greenwater Village")
HHol=B==toupper("Happy Hollow")
HHom=B==toupper("Happy Homes (Happy Homes-Lucban)")
HCC=B==toupper("Harrison-Claudio Carantes")
Hillside=B==toupper("Hillside")
HGhostE=B==toupper("Holy Ghost Extension")
HGhostP=B==toupper("Holy Ghost Proper")
Honeymoon=B==toupper("Honeymoon (Honeymoon-Holy Ghost)")
IMarcos=B==toupper("Imelda R. Marcos (La Salle)")
Imelda=B==toupper("Imelda Village")
Kabayanihan=B==toupper("Kabayanihan")
Kagitingan=B==toupper("Kagitingan")
KayangHill=B==toupper("Kayang-Hilltop")
KayangE=B==toupper("Kayang Extension")
LBK=B==toupper("Legarda-Burnham-Kisad")
LourdesE=B==toupper("Lourdes Subdivision Extension")
LourdesL=B==toupper("Lourdes Subdivision, Lower")
LourdesP=B==toupper("Lourdes Subdivision, Proper")
Lualhati=B==toupper("Lualhati")
MagsaysayPR=B==toupper("Magsaysay Private Road")
MagsaysayL=B==toupper("Magsaysay, Lower")
MagsaysayU=B==toupper("Magsaysay, Upper")
MalcolmSquarePerfectoJoseAbadSantos=B==toupper("Malcolm Square-Perfecto (Jose Abad Santos)")
MarketSubdivisionUpper=B==toupper("Market Subdivision, Upper")
MiddleQuezonHillSubdivsion=B==toupper("Middle Quezon Hill Subdivsiion(Quezon Hill Middle)")
MinesView=B==toupper("Mines View Park")
ModernSiteE=B==toupper("Modern Site, East")
ModernSiteW=B==toupper("Modern Site, West")
NewLuc=B==toupper("New Lucban")
Outlook=B==toupper("Outlook Drive")
Pacdal=B==toupper("Pacdal")
PadreB=B==toupper("Padre Burgos")
PadreZ=B==toupper("Padre Zamora")
PU=B==toupper("Palma-Urbano (Cariño-Palma)")
PhilAm=B==toupper("Phil-Am")
pucsusan=B==toupper("Pucsusan")
QHillU=B==toupper("Quezon Hill, Upper")
UpperQM=B==toupper("Quirino-Magsaysay, Upper (Upper QM)")
QHillE=B==toupper("Quirino Hill, East")
QHillW=B==toupper("Quirino Hill, West")
RizalMonumentArea=B==toupper("Rizal Monument Area")
RockQuarryLower=B==toupper("Rock Quarry, Lower")
RockQuarryMiddle=B==toupper("Rock Quarry, Middle")
RockQuarryUpper=B==toupper("Rock Quarry, Upper")
SanAntonioVillage=B==toupper("San Antonio Village")
SanLuisVillage=B==toupper("San Luis Village")
SanRoqueVillage=B==toupper("San Roque Village")
SanitaryCampSouth=B==toupper("Sanitary Camp, South")
SantoR=B==toupper("Santo Rosario")
ScoutB=B==toupper("Scout Barrio")
Session=B==toupper("Session Road Area")
Slaughter=B==toupper("Slaughter House Area (Santo Niño Slaughter)")
SLU=B==toupper("SLU-SVP Housing Village")
SouthDrive=B==toupper("South Drive")
TeoAl=B==toupper("Teodora Alonzo")
tranco=B==toupper("Trancoville")
victoria=B==toupper("Victoria Village")
STJos=B=="ST JOSEPH VILLAGE"

tdf16 <- data.frame(MorbidityWeek=1,
                   ABonifacioCaguioaRimandoABCR=0,
                   AbanaoZanduetaKayongChugumOtekAZKCO=0,
                   AlfonsoTabora=0,
                   Ambiong=0,
                   AndresBonifacioLowerBokawkan=0,
                   ApuganLoakan=0,
                   AsinRoad=0,
                   AtokTrail=0,
                   AuroraHillProperMalvarSgtFloresca=0,
                   AuroraHillNorthCentral=0,
                   AuroraHillSouthCentral=0,
                   BagongLipunanMarketArea=0,
                   BakakengCentral=0,
                   BakakengNorth=1,
                   BalMarcovilleMarcoville=0,
                   Balsigan=0,
                   BayanParkEast=0,
                   BayanParkVillage=0,
                   BayanParkWestBayanPark=0,
                   BGHCompound=0,
                   Brookside=0,
                   Brookspoint=0,
                   CabinetHillTeachersCamp=0,
                   CamdasSubdivision=0,
                   Camp7=0,
                   Camp8=0,
                   CampAllen=0,
                   CampoFilipino=0,
                   CityCampCentral=0,
                   CityCampProper=0,
                   CountryClubVillage=0,
                   CresenciaVillage=0,
                   DagisanLower=0,
                   DagisanUpper=0,
                   DizonSubdivision=0,
                   DominicanHillMirador=0,
                   Dontogan=0,
                   DPSArea=0,
                   EngineersHill=0,
                   FairviewVillage=0,
                   FerdinandHappyHomesCampoSioco=0,
                   FortdelPilar=0,
                   GabrielaSilang=0,
                   GeneralEmilioFAguinaldoQuirinoMagsaysayLower=0,
                   GeneralLunaLower=0,
                   GeneralLunaUpper=0,
                   Gibraltar=0,
                   GreenwaterVillage=0,
                   GuisadCentral=0,
                   GuisadSorong=0,
                   HappyHollow=0,
                   HappyHomesHappyHomesLucban=0,
                   HarrisonClaudioCarantes=0,
                   Hillside=0,
                   HolyGhostExtension=0,
                   HolyGhostProper=0,
                   HoneymoonHoneymoonHolyGhost=0,
                   ImeldaRMarcosLaSalle=0,
                   ImeldaVillage=0,
                   Irisan=0,
                   Kabayanihan=0,
                   Kagitingan=0,
                   KayangHilltop=0,
                   KayangExtension=0,
                   Kias=0,
                   LegardaBurnhamKisad=0,
                   LiwanagLoakan=0,
                   LoakanProper=0,
                   LopezJaena=0,
                   LourdesSubdivisionExtension=0,
                   LourdesSubdivisionLower=0,
                   LourdesSubdivisionProper=0,
                   Lualhati=0,
                   Lucnab=0,
                   MagsaysayPrivateRoad=0,
                   MagsaysayLower=0,
                   MagsaysayUpper=0,
                   MalcolmSquarePerfectoJoseAbadSantos=0,
                   ManuelARoxas=0,
                   MarketSubdivisionUpper=0,
                   MiddleQuezonHillSubdivsion=0,
                   MilitaryCutoff=0,
                   MinesViewPark=0,
                   ModernSiteEast=0,
                   ModernSiteWest=0,
                   MRRQueenofPeace=0,
                   NewLucban=0,
                   OutlookDrive=0,
                   Pacdal=0,
                   PadreBurgos=0,
                   PadreZamora=0,
                   PalmaUrbanoCarinoPalma=0,
                   PhilAm=0,
                   Pinget=0,
                   PinsaoPilotProject=0,
                   PinsaoProper=0,
                   Poliwes=0,
                   Pucsusan=0,
                   QuezonHillProper=0,
                   QuezonHillUpper=0,
                   QuirinoMagsaysayUpperUpperQM=0,
                   QuirinoHillEast=0,
                   QuirinoHillLower=0,
                   QuirinoHillMiddle=0,
                   QuirinoHillWest=0,
                   RizalMonumentArea=0,
                   RockQuarryLower=0,
                   RockQuarryMiddle=0,
                   RockQuarryUpper=0,
                   SaludMitra=0,
                   SanAntoniVillage=0,
                   SanLuisVillage=0,
                   SanRoqueVillage=0,
                   SanVicente=0,
                   SanitaryCampNorth=0,
                   SanitaryCampSouth=0,
                   SantaEscolastica=0,
                   SantoRosario=0,
                   SantoTomasProper=0,
                   SantoTomasSchoolArea=0,
                   ScoutBarrio=0,
                   SessionRoadArea=0,
                   SlaughterHouseAreaSantoNiñoSlaughter=0,
                   SLUSVPHousingVillage=0,
                   SouthDrive=0,
                   TeodoraAlonzo=0,
                   Trancoville=0,
                   VictoriaVillage=0,
                   SaintJosephVillage=0
)

for (n in 2:52) {
  tdf16[nrow(tdf16)+1,]=c(n,
                        count(typhoid16[ABCR&typhoid16$CASECLASS=="Probable"&typhoid16$MorbidityWeek==n,])+
                          count(typhoid16[ABCR&typhoid16$CASECLASS=="PROBABLE"&typhoid16$MorbidityWeek==n,])+
                          count(typhoid16[ABCR&typhoid16$CASECLASS=="CONFIRMED"&typhoid16$MorbidityWeek==n,]),
                        count(typhoid16[B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")&SS=="Probable"&MW==n,])+
                          count(typhoid16[B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[B==toupper("Alfonso Tabora")&SS=="Probable"&MW==n,])+
                          count(typhoid16[B==toupper("Alfonso Tabora")&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[B==toupper("Alfonso Tabora")&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[B=="AMBIONG"&SS=="Probable"&MW==n,])+
                          count(typhoid16[B=="AMBIONG"&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[B=="AMBIONG"&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[AB&SS=="Probable"&MW==n,])+
                          count(typhoid16[AB&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[AB&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[AL&SS=="Probable"&MW==n,])+
                          count(typhoid16[AL&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[AL&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[Asin&SS=="Probable"&MW==n,])+
                          count(typhoid16[Asin&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[Asin&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[ATOK&SS=="Probable"&MW==n,])+
                          count(typhoid16[ATOK&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[ATOK&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[auroraP&SS=="Probable"&MW==n,])+
                          count(typhoid16[auroraP&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[auroraP&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[auroraN&SS=="Probable"&MW==n,])+
                          count(typhoid16[auroraN&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[auroraN&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[auroraS&SS=="Probable"&MW==n,])+
                          count(typhoid16[auroraS&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[auroraS&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[BL&SS=="Probable"&MW==n,])+
                          count(typhoid16[BL&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[BL&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[BakakengCentral&SS=="Probable"&MW==n,])+
                          count(typhoid16[BakakengCentral&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[BakakengCentral&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[BakakengNorth&SS=="Probable"&MW==n,])+
                          count(typhoid16[BakakengNorth&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[BakakengNorth&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[Bal&SS=="Probable"&MW==n,])+
                          count(typhoid16[Bal&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[Bal&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[Balsigan&SS=="Probable"&MW==n,])+
                          count(typhoid16[Balsigan&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[Balsigan&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[BPE&SS=="Probable"&MW==n,])+
                          count(typhoid16[BPE&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[BPE&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[BPV&SS=="Probable"&MW==n,])+
                          count(typhoid16[BPV&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[BPV&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[Bayan&SS=="Probable"&MW==n,])+
                          count(typhoid16[Bayan&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[Bayan&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[BGH&SS=="Probable"&MW==n,])+
                          count(typhoid16[BGH&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[BGH&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[Brookside&SS=="Probable"&MW==n,])+
                          count(typhoid16[Brookside&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[Brookside&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[Brookspoint&SS=="Probable"&MW==n,])+
                          count(typhoid16[Brookspoint&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[Brookspoint&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[CabinetHill&SS=="Probable"&MW==n,])+
                          count(typhoid16[CabinetHill&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[CabinetHill&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[Camdas&SS=="Probable"&MW==n,])+
                          count(typhoid16[Camdas&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[Camdas&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[c7&SS=="Probable"&MW==n,])+
                          count(typhoid16[c7&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[c7&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[c8&SS=="Probable"&MW==n,])+
                          count(typhoid16[c8&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[c8&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[ca&SS=="Probable"&MW==n,])+
                          count(typhoid16[ca&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[ca&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[cf&SS=="Probable"&MW==n,])+
                          count(typhoid16[cf&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[cf&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[CityCC&SS=="Probable"&MW==n,])+
                          count(typhoid16[CityCC&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[CityCC&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[CityCP&SS=="Probable"&MW==n,])+
                          count(typhoid16[CityCP&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[CityCP&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[ccv&SS=="Probable"&MW==n,])+
                          count(typhoid16[ccv&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[ccv&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[Cres&SS=="Probable"&MW==n,])+
                          count(typhoid16[Cres&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[Cres&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[DagisanL&SS=="Probable"&MW==n,])+
                          count(typhoid16[DagisanL&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[DagisanL&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[DagisanU&SS=="Probable"&MW==n,])+
                          count(typhoid16[DagisanU&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[DagisanU&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[Dizon&SS=="Probable"&MW==n,])+
                          count(typhoid16[Dizon&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[Dizon&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[Domin&SS=="Probable"&MW==n,])+
                          count(typhoid16[Domin&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[Domin&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[Dontogan&SS=="Probable"&MW==n,])+
                          count(typhoid16[Dontogan&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[Dontogan&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[DPS&SS=="Probable"&MW==n,])+
                          count(typhoid16[DPS&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[DPS&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[enghill&SS=="Probable"&MW==n,])+
                          count(typhoid16[enghill&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[enghill&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[fvill&SS=="Probable"&MW==n,])+
                          count(typhoid16[fvill&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[fvill&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[Ferdinand&SS=="Probable"&MW==n,])+
                          count(typhoid16[Ferdinand&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[Ferdinand&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[B=="FORT DEL PILAR"&SS=="Probable"&MW==n,])+
                          count(typhoid16[B=="FORT DEL PILAR"&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[B=="FORT DEL PILAR"&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[GabSi&SS=="Probable"&MW==n,])+
                          count(typhoid16[GabSi&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[GabSi&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[GenE&SS=="Probable"&MW==n,])+
                          count(typhoid16[GenE&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[GenE&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[GenLL&SS=="Probable"&MW==n,])+
                          count(typhoid16[GenLL&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[GenLL&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[GenLU&SS=="Probable"&MW==n,])+
                          count(typhoid16[GenLU&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[GenLU&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[Gib&SS=="Probable"&MW==n,])+
                          count(typhoid16[Gib&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[Gib&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[Greenwater&SS=="Probable"&MW==n,])+
                          count(typhoid16[Greenwater&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[Greenwater&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[GuisadC&SS=="Probable"&MW==n,])+
                          count(typhoid16[GuisadC&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[GuisadC&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[GuisadS&SS=="Probable"&MW==n,])+
                          count(typhoid16[GuisadS&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[GuisadS&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[HHol&SS=="Probable"&MW==n,])+
                          count(typhoid16[HHol&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[HHol&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[HHom&SS=="Probable"&MW==n,])+
                          count(typhoid16[HHom&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[HHom&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[HCC&SS=="Probable"&MW==n,])+
                          count(typhoid16[HCC&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[HCC&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[Hillside&SS=="Probable"&MW==n,])+
                          count(typhoid16[Hillside&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[Hillside&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[HGhostE&SS=="Probable"&MW==n,])+
                          count(typhoid16[HGhostE&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[HGhostE&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[HGhostP&SS=="Probable"&MW==n,])+
                          count(typhoid16[HGhostP&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[HGhostP&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[Honeymoon&SS=="Probable"&MW==n,])+
                          count(typhoid16[Honeymoon&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[Honeymoon&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[IMarcos&SS=="Probable"&MW==n,])+
                          count(typhoid16[IMarcos&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[IMarcos&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[Imelda&SS=="Probable"&MW==n,])+
                          count(typhoid16[Imelda&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[Imelda&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[B=="IRISAN"&SS=="Probable"&MW==n,])+
                          count(typhoid16[B=="IRISAN"&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[B=="IRISAN"&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[Kabayanihan&SS=="Probable"&MW==n,])+
                          count(typhoid16[Kabayanihan&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[Kabayanihan&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[Kagitingan&SS=="Probable"&MW==n,])+
                          count(typhoid16[Kagitingan&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[Kagitingan&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[KayangHill&SS=="Probable"&MW==n,])+
                          count(typhoid16[KayangHill&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[KayangHill&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[KayangE&SS=="Probable"&MW==n,])+
                          count(typhoid16[KayangE&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[KayangE&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[kias&SS=="Probable"&MW==n,])+
                          count(typhoid16[kias&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[kias&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[LBK&SS=="Probable"&MW==n,])+
                          count(typhoid16[LBK&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[LBK&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[LLoak&SS=="Probable"&MW==n,])+
                          count(typhoid16[LLoak&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[LLoak&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[LoakP&SS=="Probable"&MW==n,])+
                          count(typhoid16[LoakP&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[LoakP&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[LOPJ&SS=="Probable"&MW==n,])+
                          count(typhoid16[LOPJ&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[LOPJ&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[LourdesE&SS=="Probable"&MW==n,])+
                          count(typhoid16[LourdesE&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[LourdesE&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[LourdesL&SS=="Probable"&MW==n,])+
                          count(typhoid16[LourdesL&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[LourdesL&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[LourdesP&SS=="Probable"&MW==n,])+
                          count(typhoid16[LourdesP&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[LourdesP&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[Lualhati&SS=="Probable"&MW==n,])+
                          count(typhoid16[Lualhati&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[Lualhati&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[lucnab&SS=="Probable"&MW==n,])+
                          count(typhoid16[lucnab&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[lucnab&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[MagsaysayPR&SS=="Probable"&MW==n,])+
                          count(typhoid16[MagsaysayPR&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[MagsaysayPR&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[MagsaysayL&SS=="Probable"&MW==n,])+
                          count(typhoid16[MagsaysayL&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[MagsaysayL&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[MagsaysayU&SS=="Probable"&MW==n,])+
                          count(typhoid16[MagsaysayU&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[MagsaysayU&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[MalcolmSquarePerfectoJoseAbadSantos&SS=="Probable"&MW==n,])+
                          count(typhoid16[MalcolmSquarePerfectoJoseAbadSantos&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[MalcolmSquarePerfectoJoseAbadSantos&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[MR&SS=="Probable"&MW==n,])+
                          count(typhoid16[MR&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[MR&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[MarketSubdivisionUpper&SS=="Probable"&MW==n,])+
                          count(typhoid16[MarketSubdivisionUpper&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[MarketSubdivisionUpper&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[MiddleQuezonHillSubdivsion&SS=="Probable"&MW==n,])+
                          count(typhoid16[MiddleQuezonHillSubdivsion&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[MiddleQuezonHillSubdivsion&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[MCutOff&SS=="Probable"&MW==n,])+
                          count(typhoid16[MCutOff&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[MCutOff&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[MinesView&SS=="Probable"&MW==n,])+
                          count(typhoid16[MinesView&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[MinesView&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[ModernSiteE&SS=="Probable"&MW==n,])+
                          count(typhoid16[ModernSiteE&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[ModernSiteE&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[ModernSiteW&SS=="Probable"&MW==n,])+
                          count(typhoid16[ModernSiteW&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[ModernSiteW&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[MRR&SS=="Probable"&MW==n,])+
                          count(typhoid16[MRR&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[MRR&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[NewLuc&SS=="Probable"&MW==n,])+
                          count(typhoid16[NewLuc&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[NewLuc&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[Outlook&SS=="Probable"&MW==n,])+
                          count(typhoid16[Outlook&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[Outlook&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[Pacdal&SS=="Probable"&MW==n,])+
                          count(typhoid16[Pacdal&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[Pacdal&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[PadreB&SS=="Probable"&MW==n,])+
                          count(typhoid16[PadreB&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[PadreB&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[PadreZ&SS=="Probable"&MW==n,])+
                          count(typhoid16[PadreZ&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[PadreZ&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[PU&SS=="Probable"&MW==n,])+
                          count(typhoid16[PU&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[PU&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[PhilAm&SS=="Probable"&MW==n,])+
                          count(typhoid16[PhilAm&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[PhilAm&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[Pinget&SS=="Probable"&MW==n,])+
                          count(typhoid16[Pinget&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[Pinget&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[PPP&SS=="Probable"&MW==n,])+
                          count(typhoid16[PPP&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[PPP&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[PisaoP&SS=="Probable"&MW==n,])+
                          count(typhoid16[PisaoP&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[PisaoP&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[Poliwes&SS=="Probable"&MW==n,])+
                          count(typhoid16[Poliwes&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[Poliwes&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[pucsusan&SS=="Probable"&MW==n,])+
                          count(typhoid16[pucsusan&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[pucsusan&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[QHP&SS=="Probable"&MW==n,])+
                          count(typhoid16[QHP&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[QHP&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[QHillU&SS=="Probable"&MW==n,])+
                          count(typhoid16[QHillU&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[QHillU&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[UpperQM&SS=="Probable"&MW==n,])+
                          count(typhoid16[UpperQM&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[UpperQM&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[QHillE&SS=="Probable"&MW==n,])+
                          count(typhoid16[QHillE&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[QHillE&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[QHL&SS=="Probable"&MW==n,])+
                          count(typhoid16[QHL&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[QHL&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[QHM&SS=="Probable"&MW==n,])+
                          count(typhoid16[QHM&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[QHM&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[QHillW&SS=="Probable"&MW==n,])+
                          count(typhoid16[QHillW&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[QHillW&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[RizalMonumentArea&SS=="Probable"&MW==n,])+
                          count(typhoid16[RizalMonumentArea&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[RizalMonumentArea&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[RockQuarryLower&SS=="Probable"&MW==n,])+
                          count(typhoid16[RockQuarryLower&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[RockQuarryLower&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[RockQuarryMiddle&SS=="Probable"&MW==n,])+
                          count(typhoid16[RockQuarryMiddle&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[RockQuarryMiddle&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[RockQuarryUpper&SS=="Probable"&MW==n,])+
                          count(typhoid16[RockQuarryUpper&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[RockQuarryUpper&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[salud&SS=="Probable"&MW==n,])+
                          count(typhoid16[salud&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[salud&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[SanAntonioVillage&SS=="Probable"&MW==n,])+
                          count(typhoid16[SanAntonioVillage&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[SanAntonioVillage&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[SanLuisVillage&SS=="Probable"&MW==n,])+
                          count(typhoid16[SanLuisVillage&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[SanLuisVillage&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[SanRoqueVillage&SS=="Probable"&MW==n,])+
                          count(typhoid16[SanRoqueVillage&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[SanRoqueVillage&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[sanvic&SS=="Probable"&MW==n,])+
                          count(typhoid16[sanvic&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[sanvic&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[SanCamp&SS=="Probable"&MW==n,])+
                          count(typhoid16[SanCamp&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[SanCamp&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[SanitaryCampSouth&SS=="Probable"&MW==n,])+
                          count(typhoid16[SanitaryCampSouth&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[SanitaryCampSouth&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[STE&SS=="Probable"&MW==n,])+
                          count(typhoid16[STE&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[STE&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[SantoR&SS=="Probable"&MW==n,])+
                          count(typhoid16[SantoR&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[SantoR&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[STP&SS=="Probable"&MW==n,])+
                          count(typhoid16[STP&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[STP&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[STsa&SS=="Probable"&MW==n,])+
                          count(typhoid16[STsa&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[STsa&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[ScoutB&SS=="Probable"&MW==n,])+
                          count(typhoid16[ScoutB&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[ScoutB&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[Session&SS=="Probable"&MW==n,])+
                          count(typhoid16[Session&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[Session&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[Slaughter&SS=="Probable"&MW==n,])+
                          count(typhoid16[Slaughter&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[Slaughter&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[SLU&SS=="Probable"&MW==n,])+
                          count(typhoid16[SLU&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[SLU&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[SouthDrive&SS=="Probable"&MW==n,])+
                          count(typhoid16[SouthDrive&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[SouthDrive&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[TeoAl&SS=="Probable"&MW==n,])+
                          count(typhoid16[TeoAl&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[TeoAl&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[tranco&SS=="Probable"&MW==n,])+
                          count(typhoid16[tranco&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[tranco&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[victoria&SS=="Probable"&MW==n,])+
                          count(typhoid16[victoria&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[victoria&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid16[STJos&SS=="Probable"&MW==n,])+
                          count(typhoid16[STJos&SS=="PROBABLE"&MW==n,])+
                          count(typhoid16[STJos&SS=="CONFIRMED"&MW==n,])
  )
}

B=typhoid17$Barangay
SS=typhoid17$CASECLASS
MW=typhoid17$MorbidityWeek

Imelda = B=="IMELDA VILLAGE"
BakakengCentral = B=="BAKAKENG CENTRAL"
ABCR = B=="A. BONIFACIO-CAGUIOA-RIMANDO (ABCR)"
Ambiong = B=="AMBIONG"
BakakengNorth = B=="BAKAKENG NORTH"
Asin = B=="ASIN"
Balsigan = B=="BALSIGAN"
Bayan = B=="BAYAN PARK WEST (BAYAN PARK)"
BGH=B=="BGH COMPOUND"
Brookside = B=="BROOKSIDE"
Camdas = B=="CAMDAS SUBDIVISION"
c7=B=="CAMP 7"
c8=B=="CAMP 8"
ca=B=="CAMP ALLEN"
cf=B=="CAMPO FILIPINO"
ccv=B=="COUNTRY CLUB VILLAGE"
DPS=B=="DPS AREA"
enghill=B=="ENGINEER'S HILL"
fvill=B=="FAIRVIEW VILLAGE"
GLuna=B=="GENERAL LUNA, UPPER"
Gib=B=="GIBRALTAR"
GuisadC=B=="GUISAD CENTRAL"
GuisadS=B=="GUISAD SORONG"
HHL=B=="HAPPY HOMES (HAPPY HOMES-LUCBAN)"
kias=B=="KIAS"
LLoak=B=="LIWANAG-LOAKAN"
LoakP=B=="LOAKAN PROPER"
LOPJ=B=="LOPEZ JAENA"
lucnab=B=="LUCNAB"
MR=B=="MANUEL A. ROXAS"
MCutOff=B=="MILITARY CUT-OFF"
MRR=B=="MRR-QUEEN OF PEACE"
PadreZ=B=="PADRE ZAMORA"
Pinget=B=="PINGET"
PPP=B=="PINSAO PILOT PROJECT"
PisaoP=B=="PINSAO PROPER"
Poliwes=B=="POLIWES"
QHP=B=="QUEZON HILL PROPER"
QHL=B=="QUIRINO HILL, LOWER"
QHM=B=="QUIRINO HILL, MIDDLE"
salud=B=="SALUD MITRA"
sanvic=B=="SAN VICENTE"
SanCamp=B=="SANITARY CAMP, NORTH"
STE=B=="SANTA ESCOLASTICA"
STP=B=="SANTO TOMAS PROPER"&B=="STO. TOMAS PROPER"
STsa=B=="SANTO TOMAS SCHOOL AREA"
tranc=B=="TRANCOVILLE"
birac=B=="BIRAC"
AL=B=="APUGAN-LOAKAN"
AZKCO=B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")
ATAB=B==toupper("Alfonso Tabora")
AB=B==toupper("Andres Bonifacio (Lower Bokawkan)")
ATOK=B==toupper("Atok Trail")
auroraP=B==toupper("Aurora Hill Proper (Malvar-Sgt. Floresca)")
auroraN=B==toupper("Aurora Hill, North Central")
auroraS=B==toupper("Aurora Hill, South Central")
BL=B==toupper("Bagong Lipunan (Market Area)")
Bal=B==toupper("Bal-Marcoville (Marcoville)")
BPE=B==toupper("Bayan Park East")
BPV=B==toupper("Bayan Park Village")
Brookspoint=B==toupper("Brookspoint")
CabinetHill=B==toupper("Cabinet Hill-Teacher's Camp")
CityCC=B==toupper("City Camp Central")
CityCP=B==toupper("City Camp Proper")
DagisanL=B==toupper("Dagsian, Lower")
Cres=B==toupper("Cresencia Village")
DagisanU=B==toupper("Dagsian, Upper")
Dizon=B==toupper("Dizon Subdivision")
Domin=B==toupper("Dominican Hill-Mirador")
Dontogan=B=="DONTOGAN"
DPS=B=="DPS AREA"
Ferdinand=B==toupper("Ferdinand (Happy Homes-Campo Sioco)")
GabSi=B=="GABRIELLA SILANG"
GenE=B==toupper("General Emilio F. Aguinaldo (Quirino-Magsaysay, Lower)")
GenLL=B==toupper("General Luna, Lower")
GenLU=B==toupper("General Luna, Upper")
Greenwater=B==toupper("Greenwater Village")
HHol=B==toupper("Happy Hollow")
HHom=B==toupper("Happy Homes (Happy Homes-Lucban)")
HCC=B==toupper("Harrison-Claudio Carantes")
Hillside=B==toupper("Hillside")
HGhostE=B==toupper("Holy Ghost Extension")
HGhostP=B==toupper("Holy Ghost Proper")
Honeymoon=B==toupper("Honeymoon (Honeymoon-Holy Ghost)")
IMarcos=B==toupper("Imelda R. Marcos (La Salle)")
Imelda=B==toupper("Imelda Village")
Kabayanihan=B==toupper("Kabayanihan")
Kagitingan=B==toupper("Kagitingan")
KayangHill=B==toupper("Kayang-Hilltop")
KayangE=B==toupper("Kayang Extension")
LBK=B==toupper("Legarda-Burnham-Kisad")
LourdesE=B==toupper("Lourdes Subdivision Extension")
LourdesL=B==toupper("Lourdes Subdivision, Lower")
LourdesP=B==toupper("Lourdes Subdivision, Proper")
Lualhati=B==toupper("Lualhati")
MagsaysayPR=B==toupper("Magsaysay Private Road")
MagsaysayL=B==toupper("Magsaysay, Lower")
MagsaysayU=B==toupper("Magsaysay, Upper")
MalcolmSquarePerfectoJoseAbadSantos=B==toupper("Malcolm Square-Perfecto (Jose Abad Santos)")
MarketSubdivisionUpper=B==toupper("Market Subdivision, Upper")
MiddleQuezonHillSubdivsion=B==toupper("Middle Quezon Hill Subdivsiion(Quezon Hill Middle)")
MinesView=B==toupper("Mines View Park")
ModernSiteE=B==toupper("Modern Site, East")
ModernSiteW=B==toupper("Modern Site, West")
NewLuc=B==toupper("New Lucban")
Outlook=B==toupper("Outlook Drive")
Pacdal=B==toupper("Pacdal")
PadreB=B==toupper("Padre Burgos")
PadreZ=B==toupper("Padre Zamora")
PU=B==toupper("Palma-Urbano (Cariño-Palma)")
PhilAm=B==toupper("Phil-Am")
pucsusan=B==toupper("Pucsusan")
QHillU=B==toupper("Quezon Hill, Upper")
UpperQM=B==toupper("Quirino-Magsaysay, Upper (Upper QM)")
QHillE=B==toupper("Quirino Hill, East")
QHillW=B==toupper("Quirino Hill, West")
RizalMonumentArea=B==toupper("Rizal Monument Area")
RockQuarryLower=B==toupper("Rock Quarry, Lower")
RockQuarryMiddle=B==toupper("Rock Quarry, Middle")
RockQuarryUpper=B==toupper("Rock Quarry, Upper")
SanAntonioVillage=B==toupper("San Antonio Village")
SanLuisVillage=B==toupper("San Luis Village")
SanRoqueVillage=B==toupper("San Roque Village")
SanitaryCampSouth=B==toupper("Sanitary Camp, South")
SantoR=B==toupper("Santo Rosario")
ScoutB=B==toupper("Scout Barrio")
Session=B==toupper("Session Road Area")
Slaughter=B==toupper("Slaughter House Area (Santo Niño Slaughter)")
SLU=B==toupper("SLU-SVP Housing Village")
SouthDrive=B==toupper("South Drive")
TeoAl=B==toupper("Teodora Alonzo")
tranco=B==toupper("Trancoville")
victoria=B==toupper("Victoria Village")
STJos=B=="ST JOSEPH VILLAGE"

tdf17 <- data.frame(MorbidityWeek=1,
                   ABonifacioCaguioaRimandoABCR=0,
                   AbanaoZanduetaKayongChugumOtekAZKCO=0,
                   AlfonsoTabora=0,
                   Ambiong=0,
                   AndresBonifacioLowerBokawkan=0,
                   ApuganLoakan=0,
                   AsinRoad=0,
                   AtokTrail=0,
                   AuroraHillProperMalvarSgtFloresca=0,
                   AuroraHillNorthCentral=0,
                   AuroraHillSouthCentral=0,
                   BagongLipunanMarketArea=0,
                   BakakengCentral=0,
                   BakakengNorth=1,
                   BalMarcovilleMarcoville=0,
                   Balsigan=0,
                   BayanParkEast=0,
                   BayanParkVillage=0,
                   BayanParkWestBayanPark=0,
                   BGHCompound=0,
                   Brookside=0,
                   Brookspoint=0,
                   CabinetHillTeachersCamp=0,
                   CamdasSubdivision=0,
                   Camp7=0,
                   Camp8=0,
                   CampAllen=0,
                   CampoFilipino=0,
                   CityCampCentral=0,
                   CityCampProper=0,
                   CountryClubVillage=0,
                   CresenciaVillage=0,
                   DagisanLower=0,
                   DagisanUpper=0,
                   DizonSubdivision=0,
                   DominicanHillMirador=0,
                   Dontogan=0,
                   DPSArea=0,
                   EngineersHill=0,
                   FairviewVillage=0,
                   FerdinandHappyHomesCampoSioco=0,
                   FortdelPilar=0,
                   GabrielaSilang=0,
                   GeneralEmilioFAguinaldoQuirinoMagsaysayLower=0,
                   GeneralLunaLower=0,
                   GeneralLunaUpper=0,
                   Gibraltar=0,
                   GreenwaterVillage=0,
                   GuisadCentral=0,
                   GuisadSorong=0,
                   HappyHollow=0,
                   HappyHomesHappyHomesLucban=0,
                   HarrisonClaudioCarantes=0,
                   Hillside=0,
                   HolyGhostExtension=0,
                   HolyGhostProper=0,
                   HoneymoonHoneymoonHolyGhost=0,
                   ImeldaRMarcosLaSalle=0,
                   ImeldaVillage=0,
                   Irisan=0,
                   Kabayanihan=0,
                   Kagitingan=0,
                   KayangHilltop=0,
                   KayangExtension=0,
                   Kias=0,
                   LegardaBurnhamKisad=0,
                   LiwanagLoakan=0,
                   LoakanProper=0,
                   LopezJaena=0,
                   LourdesSubdivisionExtension=0,
                   LourdesSubdivisionLower=0,
                   LourdesSubdivisionProper=0,
                   Lualhati=0,
                   Lucnab=0,
                   MagsaysayPrivateRoad=0,
                   MagsaysayLower=0,
                   MagsaysayUpper=0,
                   MalcolmSquarePerfectoJoseAbadSantos=0,
                   ManuelARoxas=0,
                   MarketSubdivisionUpper=0,
                   MiddleQuezonHillSubdivsion=0,
                   MilitaryCutoff=0,
                   MinesViewPark=0,
                   ModernSiteEast=0,
                   ModernSiteWest=0,
                   MRRQueenofPeace=0,
                   NewLucban=0,
                   OutlookDrive=0,
                   Pacdal=0,
                   PadreBurgos=0,
                   PadreZamora=0,
                   PalmaUrbanoCarinoPalma=0,
                   PhilAm=0,
                   Pinget=0,
                   PinsaoPilotProject=0,
                   PinsaoProper=0,
                   Poliwes=0,
                   Pucsusan=0,
                   QuezonHillProper=0,
                   QuezonHillUpper=0,
                   QuirinoMagsaysayUpperUpperQM=0,
                   QuirinoHillEast=0,
                   QuirinoHillLower=0,
                   QuirinoHillMiddle=0,
                   QuirinoHillWest=0,
                   RizalMonumentArea=0,
                   RockQuarryLower=0,
                   RockQuarryMiddle=0,
                   RockQuarryUpper=0,
                   SaludMitra=0,
                   SanAntoniVillage=0,
                   SanLuisVillage=0,
                   SanRoqueVillage=0,
                   SanVicente=0,
                   SanitaryCampNorth=0,
                   SanitaryCampSouth=0,
                   SantaEscolastica=0,
                   SantoRosario=0,
                   SantoTomasProper=0,
                   SantoTomasSchoolArea=0,
                   ScoutBarrio=0,
                   SessionRoadArea=0,
                   SlaughterHouseAreaSantoNiñoSlaughter=0,
                   SLUSVPHousingVillage=0,
                   SouthDrive=0,
                   TeodoraAlonzo=0,
                   Trancoville=0,
                   VictoriaVillage=0,
                   SaintJosephVillage=0
)

for (n in 2:52) {
  tdf17[nrow(tdf17)+1,]=c(n,
                        count(typhoid17[ABCR&typhoid17$CASECLASS=="Probable"&typhoid17$MorbidityWeek==n,])+
                          count(typhoid17[ABCR&typhoid17$CASECLASS=="PROBABLE"&typhoid17$MorbidityWeek==n,])+
                          count(typhoid17[ABCR&typhoid17$CASECLASS=="CONFIRMED"&typhoid17$MorbidityWeek==n,]),
                        count(typhoid17[B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")&SS=="Probable"&MW==n,])+
                          count(typhoid17[B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[B==toupper("Alfonso Tabora")&SS=="Probable"&MW==n,])+
                          count(typhoid17[B==toupper("Alfonso Tabora")&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[B==toupper("Alfonso Tabora")&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[B=="AMBIONG"&SS=="Probable"&MW==n,])+
                          count(typhoid17[B=="AMBIONG"&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[B=="AMBIONG"&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[AB&SS=="Probable"&MW==n,])+
                          count(typhoid17[AB&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[AB&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[AL&SS=="Probable"&MW==n,])+
                          count(typhoid17[AL&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[AL&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[Asin&SS=="Probable"&MW==n,])+
                          count(typhoid17[Asin&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[Asin&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[ATOK&SS=="Probable"&MW==n,])+
                          count(typhoid17[ATOK&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[ATOK&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[auroraP&SS=="Probable"&MW==n,])+
                          count(typhoid17[auroraP&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[auroraP&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[auroraN&SS=="Probable"&MW==n,])+
                          count(typhoid17[auroraN&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[auroraN&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[auroraS&SS=="Probable"&MW==n,])+
                          count(typhoid17[auroraS&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[auroraS&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[BL&SS=="Probable"&MW==n,])+
                          count(typhoid17[BL&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[BL&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[BakakengCentral&SS=="Probable"&MW==n,])+
                          count(typhoid17[BakakengCentral&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[BakakengCentral&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[BakakengNorth&SS=="Probable"&MW==n,])+
                          count(typhoid17[BakakengNorth&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[BakakengNorth&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[Bal&SS=="Probable"&MW==n,])+
                          count(typhoid17[Bal&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[Bal&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[Balsigan&SS=="Probable"&MW==n,])+
                          count(typhoid17[Balsigan&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[Balsigan&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[BPE&SS=="Probable"&MW==n,])+
                          count(typhoid17[BPE&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[BPE&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[BPV&SS=="Probable"&MW==n,])+
                          count(typhoid17[BPV&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[BPV&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[Bayan&SS=="Probable"&MW==n,])+
                          count(typhoid17[Bayan&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[Bayan&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[BGH&SS=="Probable"&MW==n,])+
                          count(typhoid17[BGH&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[BGH&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[Brookside&SS=="Probable"&MW==n,])+
                          count(typhoid17[Brookside&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[Brookside&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[Brookspoint&SS=="Probable"&MW==n,])+
                          count(typhoid17[Brookspoint&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[Brookspoint&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[CabinetHill&SS=="Probable"&MW==n,])+
                          count(typhoid17[CabinetHill&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[CabinetHill&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[Camdas&SS=="Probable"&MW==n,])+
                          count(typhoid17[Camdas&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[Camdas&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[c7&SS=="Probable"&MW==n,])+
                          count(typhoid17[c7&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[c7&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[c8&SS=="Probable"&MW==n,])+
                          count(typhoid17[c8&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[c8&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[ca&SS=="Probable"&MW==n,])+
                          count(typhoid17[ca&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[ca&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[cf&SS=="Probable"&MW==n,])+
                          count(typhoid17[cf&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[cf&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[CityCC&SS=="Probable"&MW==n,])+
                          count(typhoid17[CityCC&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[CityCC&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[CityCP&SS=="Probable"&MW==n,])+
                          count(typhoid17[CityCP&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[CityCP&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[ccv&SS=="Probable"&MW==n,])+
                          count(typhoid17[ccv&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[ccv&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[Cres&SS=="Probable"&MW==n,])+
                          count(typhoid17[Cres&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[Cres&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[DagisanL&SS=="Probable"&MW==n,])+
                          count(typhoid17[DagisanL&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[DagisanL&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[DagisanU&SS=="Probable"&MW==n,])+
                          count(typhoid17[DagisanU&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[DagisanU&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[Dizon&SS=="Probable"&MW==n,])+
                          count(typhoid17[Dizon&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[Dizon&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[Domin&SS=="Probable"&MW==n,])+
                          count(typhoid17[Domin&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[Domin&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[Dontogan&SS=="Probable"&MW==n,])+
                          count(typhoid17[Dontogan&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[Dontogan&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[DPS&SS=="Probable"&MW==n,])+
                          count(typhoid17[DPS&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[DPS&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[enghill&SS=="Probable"&MW==n,])+
                          count(typhoid17[enghill&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[enghill&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[fvill&SS=="Probable"&MW==n,])+
                          count(typhoid17[fvill&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[fvill&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[Ferdinand&SS=="Probable"&MW==n,])+
                          count(typhoid17[Ferdinand&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[Ferdinand&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[B=="FORT DEL PILAR"&SS=="Probable"&MW==n,])+
                          count(typhoid17[B=="FORT DEL PILAR"&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[B=="FORT DEL PILAR"&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[GabSi&SS=="Probable"&MW==n,])+
                          count(typhoid17[GabSi&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[GabSi&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[GenE&SS=="Probable"&MW==n,])+
                          count(typhoid17[GenE&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[GenE&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[GenLL&SS=="Probable"&MW==n,])+
                          count(typhoid17[GenLL&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[GenLL&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[GenLU&SS=="Probable"&MW==n,])+
                          count(typhoid17[GenLU&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[GenLU&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[Gib&SS=="Probable"&MW==n,])+
                          count(typhoid17[Gib&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[Gib&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[Greenwater&SS=="Probable"&MW==n,])+
                          count(typhoid17[Greenwater&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[Greenwater&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[GuisadC&SS=="Probable"&MW==n,])+
                          count(typhoid17[GuisadC&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[GuisadC&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[GuisadS&SS=="Probable"&MW==n,])+
                          count(typhoid17[GuisadS&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[GuisadS&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[HHol&SS=="Probable"&MW==n,])+
                          count(typhoid17[HHol&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[HHol&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[HHom&SS=="Probable"&MW==n,])+
                          count(typhoid17[HHom&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[HHom&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[HCC&SS=="Probable"&MW==n,])+
                          count(typhoid17[HCC&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[HCC&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[Hillside&SS=="Probable"&MW==n,])+
                          count(typhoid17[Hillside&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[Hillside&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[HGhostE&SS=="Probable"&MW==n,])+
                          count(typhoid17[HGhostE&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[HGhostE&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[HGhostP&SS=="Probable"&MW==n,])+
                          count(typhoid17[HGhostP&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[HGhostP&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[Honeymoon&SS=="Probable"&MW==n,])+
                          count(typhoid17[Honeymoon&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[Honeymoon&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[IMarcos&SS=="Probable"&MW==n,])+
                          count(typhoid17[IMarcos&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[IMarcos&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[Imelda&SS=="Probable"&MW==n,])+
                          count(typhoid17[Imelda&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[Imelda&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[B=="IRISAN"&SS=="Probable"&MW==n,])+
                          count(typhoid17[B=="IRISAN"&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[B=="IRISAN"&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[Kabayanihan&SS=="Probable"&MW==n,])+
                          count(typhoid17[Kabayanihan&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[Kabayanihan&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[Kagitingan&SS=="Probable"&MW==n,])+
                          count(typhoid17[Kagitingan&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[Kagitingan&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[KayangHill&SS=="Probable"&MW==n,])+
                          count(typhoid17[KayangHill&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[KayangHill&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[KayangE&SS=="Probable"&MW==n,])+
                          count(typhoid17[KayangE&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[KayangE&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[kias&SS=="Probable"&MW==n,])+
                          count(typhoid17[kias&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[kias&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[LBK&SS=="Probable"&MW==n,])+
                          count(typhoid17[LBK&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[LBK&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[LLoak&SS=="Probable"&MW==n,])+
                          count(typhoid17[LLoak&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[LLoak&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[LoakP&SS=="Probable"&MW==n,])+
                          count(typhoid17[LoakP&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[LoakP&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[LOPJ&SS=="Probable"&MW==n,])+
                          count(typhoid17[LOPJ&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[LOPJ&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[LourdesE&SS=="Probable"&MW==n,])+
                          count(typhoid17[LourdesE&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[LourdesE&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[LourdesL&SS=="Probable"&MW==n,])+
                          count(typhoid17[LourdesL&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[LourdesL&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[LourdesP&SS=="Probable"&MW==n,])+
                          count(typhoid17[LourdesP&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[LourdesP&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[Lualhati&SS=="Probable"&MW==n,])+
                          count(typhoid17[Lualhati&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[Lualhati&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[lucnab&SS=="Probable"&MW==n,])+
                          count(typhoid17[lucnab&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[lucnab&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[MagsaysayPR&SS=="Probable"&MW==n,])+
                          count(typhoid17[MagsaysayPR&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[MagsaysayPR&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[MagsaysayL&SS=="Probable"&MW==n,])+
                          count(typhoid17[MagsaysayL&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[MagsaysayL&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[MagsaysayU&SS=="Probable"&MW==n,])+
                          count(typhoid17[MagsaysayU&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[MagsaysayU&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[MalcolmSquarePerfectoJoseAbadSantos&SS=="Probable"&MW==n,])+
                          count(typhoid17[MalcolmSquarePerfectoJoseAbadSantos&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[MalcolmSquarePerfectoJoseAbadSantos&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[MR&SS=="Probable"&MW==n,])+
                          count(typhoid17[MR&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[MR&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[MarketSubdivisionUpper&SS=="Probable"&MW==n,])+
                          count(typhoid17[MarketSubdivisionUpper&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[MarketSubdivisionUpper&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[MiddleQuezonHillSubdivsion&SS=="Probable"&MW==n,])+
                          count(typhoid17[MiddleQuezonHillSubdivsion&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[MiddleQuezonHillSubdivsion&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[MCutOff&SS=="Probable"&MW==n,])+
                          count(typhoid17[MCutOff&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[MCutOff&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[MinesView&SS=="Probable"&MW==n,])+
                          count(typhoid17[MinesView&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[MinesView&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[ModernSiteE&SS=="Probable"&MW==n,])+
                          count(typhoid17[ModernSiteE&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[ModernSiteE&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[ModernSiteW&SS=="Probable"&MW==n,])+
                          count(typhoid17[ModernSiteW&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[ModernSiteW&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[MRR&SS=="Probable"&MW==n,])+
                          count(typhoid17[MRR&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[MRR&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[NewLuc&SS=="Probable"&MW==n,])+
                          count(typhoid17[NewLuc&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[NewLuc&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[Outlook&SS=="Probable"&MW==n,])+
                          count(typhoid17[Outlook&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[Outlook&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[Pacdal&SS=="Probable"&MW==n,])+
                          count(typhoid17[Pacdal&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[Pacdal&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[PadreB&SS=="Probable"&MW==n,])+
                          count(typhoid17[PadreB&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[PadreB&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[PadreZ&SS=="Probable"&MW==n,])+
                          count(typhoid17[PadreZ&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[PadreZ&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[PU&SS=="Probable"&MW==n,])+
                          count(typhoid17[PU&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[PU&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[PhilAm&SS=="Probable"&MW==n,])+
                          count(typhoid17[PhilAm&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[PhilAm&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[Pinget&SS=="Probable"&MW==n,])+
                          count(typhoid17[Pinget&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[Pinget&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[PPP&SS=="Probable"&MW==n,])+
                          count(typhoid17[PPP&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[PPP&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[PisaoP&SS=="Probable"&MW==n,])+
                          count(typhoid17[PisaoP&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[PisaoP&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[Poliwes&SS=="Probable"&MW==n,])+
                          count(typhoid17[Poliwes&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[Poliwes&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[pucsusan&SS=="Probable"&MW==n,])+
                          count(typhoid17[pucsusan&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[pucsusan&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[QHP&SS=="Probable"&MW==n,])+
                          count(typhoid17[QHP&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[QHP&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[QHillU&SS=="Probable"&MW==n,])+
                          count(typhoid17[QHillU&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[QHillU&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[UpperQM&SS=="Probable"&MW==n,])+
                          count(typhoid17[UpperQM&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[UpperQM&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[QHillE&SS=="Probable"&MW==n,])+
                          count(typhoid17[QHillE&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[QHillE&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[QHL&SS=="Probable"&MW==n,])+
                          count(typhoid17[QHL&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[QHL&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[QHM&SS=="Probable"&MW==n,])+
                          count(typhoid17[QHM&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[QHM&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[QHillW&SS=="Probable"&MW==n,])+
                          count(typhoid17[QHillW&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[QHillW&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[RizalMonumentArea&SS=="Probable"&MW==n,])+
                          count(typhoid17[RizalMonumentArea&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[RizalMonumentArea&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[RockQuarryLower&SS=="Probable"&MW==n,])+
                          count(typhoid17[RockQuarryLower&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[RockQuarryLower&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[RockQuarryMiddle&SS=="Probable"&MW==n,])+
                          count(typhoid17[RockQuarryMiddle&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[RockQuarryMiddle&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[RockQuarryUpper&SS=="Probable"&MW==n,])+
                          count(typhoid17[RockQuarryUpper&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[RockQuarryUpper&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[salud&SS=="Probable"&MW==n,])+
                          count(typhoid17[salud&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[salud&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[SanAntonioVillage&SS=="Probable"&MW==n,])+
                          count(typhoid17[SanAntonioVillage&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[SanAntonioVillage&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[SanLuisVillage&SS=="Probable"&MW==n,])+
                          count(typhoid17[SanLuisVillage&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[SanLuisVillage&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[SanRoqueVillage&SS=="Probable"&MW==n,])+
                          count(typhoid17[SanRoqueVillage&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[SanRoqueVillage&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[sanvic&SS=="Probable"&MW==n,])+
                          count(typhoid17[sanvic&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[sanvic&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[SanCamp&SS=="Probable"&MW==n,])+
                          count(typhoid17[SanCamp&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[SanCamp&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[SanitaryCampSouth&SS=="Probable"&MW==n,])+
                          count(typhoid17[SanitaryCampSouth&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[SanitaryCampSouth&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[STE&SS=="Probable"&MW==n,])+
                          count(typhoid17[STE&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[STE&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[SantoR&SS=="Probable"&MW==n,])+
                          count(typhoid17[SantoR&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[SantoR&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[STP&SS=="Probable"&MW==n,])+
                          count(typhoid17[STP&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[STP&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[STsa&SS=="Probable"&MW==n,])+
                          count(typhoid17[STsa&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[STsa&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[ScoutB&SS=="Probable"&MW==n,])+
                          count(typhoid17[ScoutB&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[ScoutB&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[Session&SS=="Probable"&MW==n,])+
                          count(typhoid17[Session&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[Session&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[Slaughter&SS=="Probable"&MW==n,])+
                          count(typhoid17[Slaughter&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[Slaughter&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[SLU&SS=="Probable"&MW==n,])+
                          count(typhoid17[SLU&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[SLU&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[SouthDrive&SS=="Probable"&MW==n,])+
                          count(typhoid17[SouthDrive&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[SouthDrive&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[TeoAl&SS=="Probable"&MW==n,])+
                          count(typhoid17[TeoAl&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[TeoAl&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[tranco&SS=="Probable"&MW==n,])+
                          count(typhoid17[tranco&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[tranco&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[victoria&SS=="Probable"&MW==n,])+
                          count(typhoid17[victoria&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[victoria&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid17[STJos&SS=="Probable"&MW==n,])+
                          count(typhoid17[STJos&SS=="PROBABLE"&MW==n,])+
                          count(typhoid17[STJos&SS=="CONFIRMED"&MW==n,])
  )
}

B=typhoid18$Barangay
SS=typhoid18$CASECLASS
MW=typhoid18$MorbidityWeek

Imelda = B=="IMELDA VILLAGE"
BakakengCentral = B=="BAKAKENG CENTRAL"
ABCR = B=="A. BONIFACIO-CAGUIOA-RIMANDO (ABCR)"
Ambiong = B=="AMBIONG"
BakakengNorth = B=="BAKAKENG NORTH"
Asin = B=="ASIN"
Balsigan = B=="BALSIGAN"
Bayan = B=="BAYAN PARK WEST (BAYAN PARK)"
BGH=B=="BGH COMPOUND"
Brookside = B=="BROOKSIDE"
Camdas = B=="CAMDAS SUBDIVISION"
c7=B=="CAMP 7"
c8=B=="CAMP 8"
ca=B=="CAMP ALLEN"
cf=B=="CAMPO FILIPINO"
ccv=B=="COUNTRY CLUB VILLAGE"
DPS=B=="DPS AREA"
enghill=B=="ENGINEER'S HILL"
fvill=B=="FAIRVIEW VILLAGE"
GLuna=B=="GENERAL LUNA, UPPER"
Gib=B=="GIBRALTAR"
GuisadC=B=="GUISAD CENTRAL"
GuisadS=B=="GUISAD SORONG"
HHL=B=="HAPPY HOMES (HAPPY HOMES-LUCBAN)"
kias=B=="KIAS"
LLoak=B=="LIWANAG-LOAKAN"
LoakP=B=="LOAKAN PROPER"
LOPJ=B=="LOPEZ JAENA"
lucnab=B=="LUCNAB"
MR=B=="MANUEL A. ROXAS"
MCutOff=B=="MILITARY CUT-OFF"
MRR=B=="MRR-QUEEN OF PEACE"
PadreZ=B=="PADRE ZAMORA"
Pinget=B=="PINGET"
PPP=B=="PINSAO PILOT PROJECT"
PisaoP=B=="PINSAO PROPER"
Poliwes=B=="POLIWES"
QHP=B=="QUEZON HILL PROPER"
QHL=B=="QUIRINO HILL, LOWER"
QHM=B=="QUIRINO HILL, MIDDLE"
salud=B=="SALUD MITRA"
sanvic=B=="SAN VICENTE"
SanCamp=B=="SANITARY CAMP, NORTH"
STE=B=="SANTA ESCOLASTICA"
STP=B=="SANTO TOMAS PROPER"&B=="STO. TOMAS PROPER"
STsa=B=="SANTO TOMAS SCHOOL AREA"
tranc=B=="TRANCOVILLE"
birac=B=="BIRAC"
AL=B=="APUGAN-LOAKAN"
AZKCO=B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")
ATAB=B==toupper("Alfonso Tabora")
AB=B==toupper("Andres Bonifacio (Lower Bokawkan)")
ATOK=B==toupper("Atok Trail")
auroraP=B==toupper("Aurora Hill Proper (Malvar-Sgt. Floresca)")
auroraN=B==toupper("Aurora Hill, North Central")
auroraS=B==toupper("Aurora Hill, South Central")
BL=B==toupper("Bagong Lipunan (Market Area)")
Bal=B==toupper("Bal-Marcoville (Marcoville)")
BPE=B==toupper("Bayan Park East")
BPV=B==toupper("Bayan Park Village")
Brookspoint=B==toupper("Brookspoint")
CabinetHill=B==toupper("Cabinet Hill-Teacher's Camp")
CityCC=B==toupper("City Camp Central")
CityCP=B==toupper("City Camp Proper")
DagisanL=B==toupper("Dagsian, Lower")
Cres=B==toupper("Cresencia Village")
DagisanU=B==toupper("Dagsian, Upper")
Dizon=B==toupper("Dizon Subdivision")
Domin=B==toupper("Dominican Hill-Mirador")
Dontogan=B=="DONTOGAN"
DPS=B=="DPS AREA"
Ferdinand=B==toupper("Ferdinand (Happy Homes-Campo Sioco)")
GabSi=B=="GABRIELLA SILANG"
GenE=B==toupper("General Emilio F. Aguinaldo (Quirino-Magsaysay, Lower)")
GenLL=B==toupper("General Luna, Lower")
GenLU=B==toupper("General Luna, Upper")
Greenwater=B==toupper("Greenwater Village")
HHol=B==toupper("Happy Hollow")
HHom=B==toupper("Happy Homes (Happy Homes-Lucban)")
HCC=B==toupper("Harrison-Claudio Carantes")
Hillside=B==toupper("Hillside")
HGhostE=B==toupper("Holy Ghost Extension")
HGhostP=B==toupper("Holy Ghost Proper")
Honeymoon=B==toupper("Honeymoon (Honeymoon-Holy Ghost)")
IMarcos=B==toupper("Imelda R. Marcos (La Salle)")
Imelda=B==toupper("Imelda Village")
Kabayanihan=B==toupper("Kabayanihan")
Kagitingan=B==toupper("Kagitingan")
KayangHill=B==toupper("Kayang-Hilltop")
KayangE=B==toupper("Kayang Extension")
LBK=B==toupper("Legarda-Burnham-Kisad")
LourdesE=B==toupper("Lourdes Subdivision Extension")
LourdesL=B==toupper("Lourdes Subdivision, Lower")
LourdesP=B==toupper("Lourdes Subdivision, Proper")
Lualhati=B==toupper("Lualhati")
MagsaysayPR=B==toupper("Magsaysay Private Road")
MagsaysayL=B==toupper("Magsaysay, Lower")
MagsaysayU=B==toupper("Magsaysay, Upper")
MalcolmSquarePerfectoJoseAbadSantos=B==toupper("Malcolm Square-Perfecto (Jose Abad Santos)")
MarketSubdivisionUpper=B==toupper("Market Subdivision, Upper")
MiddleQuezonHillSubdivsion=B==toupper("Middle Quezon Hill Subdivsiion(Quezon Hill Middle)")
MinesView=B==toupper("Mines View Park")
ModernSiteE=B==toupper("Modern Site, East")
ModernSiteW=B==toupper("Modern Site, West")
NewLuc=B==toupper("New Lucban")
Outlook=B==toupper("Outlook Drive")
Pacdal=B==toupper("Pacdal")
PadreB=B==toupper("Padre Burgos")
PadreZ=B==toupper("Padre Zamora")
PU=B==toupper("Palma-Urbano (Cariño-Palma)")
PhilAm=B==toupper("Phil-Am")
pucsusan=B==toupper("Pucsusan")
QHillU=B==toupper("Quezon Hill, Upper")
UpperQM=B==toupper("Quirino-Magsaysay, Upper (Upper QM)")
QHillE=B==toupper("Quirino Hill, East")
QHillW=B==toupper("Quirino Hill, West")
RizalMonumentArea=B==toupper("Rizal Monument Area")
RockQuarryLower=B==toupper("Rock Quarry, Lower")
RockQuarryMiddle=B==toupper("Rock Quarry, Middle")
RockQuarryUpper=B==toupper("Rock Quarry, Upper")
SanAntonioVillage=B==toupper("San Antonio Village")
SanLuisVillage=B==toupper("San Luis Village")
SanRoqueVillage=B==toupper("San Roque Village")
SanitaryCampSouth=B==toupper("Sanitary Camp, South")
SantoR=B==toupper("Santo Rosario")
ScoutB=B==toupper("Scout Barrio")
Session=B==toupper("Session Road Area")
Slaughter=B==toupper("Slaughter House Area (Santo Niño Slaughter)")
SLU=B==toupper("SLU-SVP Housing Village")
SouthDrive=B==toupper("South Drive")
TeoAl=B==toupper("Teodora Alonzo")
tranco=B==toupper("Trancoville")
victoria=B==toupper("Victoria Village")
STJos=B=="ST JOSEPH VILLAGE"

tdf18 <- data.frame(MorbidityWeek=1,
                   ABonifacioCaguioaRimandoABCR=0,
                   AbanaoZanduetaKayongChugumOtekAZKCO=0,
                   AlfonsoTabora=0,
                   Ambiong=0,
                   AndresBonifacioLowerBokawkan=0,
                   ApuganLoakan=0,
                   AsinRoad=0,
                   AtokTrail=0,
                   AuroraHillProperMalvarSgtFloresca=0,
                   AuroraHillNorthCentral=0,
                   AuroraHillSouthCentral=0,
                   BagongLipunanMarketArea=0,
                   BakakengCentral=0,
                   BakakengNorth=1,
                   BalMarcovilleMarcoville=0,
                   Balsigan=0,
                   BayanParkEast=0,
                   BayanParkVillage=0,
                   BayanParkWestBayanPark=0,
                   BGHCompound=0,
                   Brookside=0,
                   Brookspoint=0,
                   CabinetHillTeachersCamp=0,
                   CamdasSubdivision=0,
                   Camp7=0,
                   Camp8=0,
                   CampAllen=0,
                   CampoFilipino=0,
                   CityCampCentral=0,
                   CityCampProper=0,
                   CountryClubVillage=0,
                   CresenciaVillage=0,
                   DagisanLower=0,
                   DagisanUpper=0,
                   DizonSubdivision=0,
                   DominicanHillMirador=0,
                   Dontogan=0,
                   DPSArea=0,
                   EngineersHill=0,
                   FairviewVillage=0,
                   FerdinandHappyHomesCampoSioco=0,
                   FortdelPilar=0,
                   GabrielaSilang=0,
                   GeneralEmilioFAguinaldoQuirinoMagsaysayLower=0,
                   GeneralLunaLower=0,
                   GeneralLunaUpper=0,
                   Gibraltar=0,
                   GreenwaterVillage=0,
                   GuisadCentral=0,
                   GuisadSorong=0,
                   HappyHollow=0,
                   HappyHomesHappyHomesLucban=0,
                   HarrisonClaudioCarantes=0,
                   Hillside=0,
                   HolyGhostExtension=0,
                   HolyGhostProper=0,
                   HoneymoonHoneymoonHolyGhost=0,
                   ImeldaRMarcosLaSalle=0,
                   ImeldaVillage=0,
                   Irisan=0,
                   Kabayanihan=0,
                   Kagitingan=0,
                   KayangHilltop=0,
                   KayangExtension=0,
                   Kias=0,
                   LegardaBurnhamKisad=0,
                   LiwanagLoakan=0,
                   LoakanProper=0,
                   LopezJaena=0,
                   LourdesSubdivisionExtension=0,
                   LourdesSubdivisionLower=0,
                   LourdesSubdivisionProper=0,
                   Lualhati=0,
                   Lucnab=0,
                   MagsaysayPrivateRoad=0,
                   MagsaysayLower=0,
                   MagsaysayUpper=0,
                   MalcolmSquarePerfectoJoseAbadSantos=0,
                   ManuelARoxas=0,
                   MarketSubdivisionUpper=0,
                   MiddleQuezonHillSubdivsion=0,
                   MilitaryCutoff=0,
                   MinesViewPark=0,
                   ModernSiteEast=0,
                   ModernSiteWest=0,
                   MRRQueenofPeace=0,
                   NewLucban=0,
                   OutlookDrive=0,
                   Pacdal=0,
                   PadreBurgos=0,
                   PadreZamora=0,
                   PalmaUrbanoCarinoPalma=0,
                   PhilAm=0,
                   Pinget=0,
                   PinsaoPilotProject=0,
                   PinsaoProper=0,
                   Poliwes=0,
                   Pucsusan=0,
                   QuezonHillProper=0,
                   QuezonHillUpper=0,
                   QuirinoMagsaysayUpperUpperQM=0,
                   QuirinoHillEast=0,
                   QuirinoHillLower=0,
                   QuirinoHillMiddle=0,
                   QuirinoHillWest=0,
                   RizalMonumentArea=0,
                   RockQuarryLower=0,
                   RockQuarryMiddle=0,
                   RockQuarryUpper=0,
                   SaludMitra=0,
                   SanAntoniVillage=0,
                   SanLuisVillage=0,
                   SanRoqueVillage=0,
                   SanVicente=0,
                   SanitaryCampNorth=0,
                   SanitaryCampSouth=0,
                   SantaEscolastica=0,
                   SantoRosario=0,
                   SantoTomasProper=0,
                   SantoTomasSchoolArea=0,
                   ScoutBarrio=0,
                   SessionRoadArea=0,
                   SlaughterHouseAreaSantoNiñoSlaughter=0,
                   SLUSVPHousingVillage=0,
                   SouthDrive=0,
                   TeodoraAlonzo=0,
                   Trancoville=0,
                   VictoriaVillage=0,
                   SaintJosephVillage=0
)

for (n in 2:52) {
  tdf18[nrow(tdf18)+1,]=c(n,
                        count(typhoid18[ABCR&SS=="Probable"&MW==n,])+
                          count(typhoid18[ABCR&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[ABCR&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")&SS=="Probable"&MW==n,])+
                          count(typhoid18[B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[B==toupper("Alfonso Tabora")&SS=="Probable"&MW==n,])+
                          count(typhoid18[B==toupper("Alfonso Tabora")&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[B==toupper("Alfonso Tabora")&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[B=="AMBIONG"&SS=="Probable"&MW==n,])+
                          count(typhoid18[B=="AMBIONG"&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[B=="AMBIONG"&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[AB&SS=="Probable"&MW==n,])+
                          count(typhoid18[AB&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[AB&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[AL&SS=="Probable"&MW==n,])+
                          count(typhoid18[AL&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[AL&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[Asin&SS=="Probable"&MW==n,])+
                          count(typhoid18[Asin&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[Asin&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[ATOK&SS=="Probable"&MW==n,])+
                          count(typhoid18[ATOK&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[ATOK&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[auroraP&SS=="Probable"&MW==n,])+
                          count(typhoid18[auroraP&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[auroraP&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[auroraN&SS=="Probable"&MW==n,])+
                          count(typhoid18[auroraN&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[auroraN&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[auroraS&SS=="Probable"&MW==n,])+
                          count(typhoid18[auroraS&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[auroraS&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[BL&SS=="Probable"&MW==n,])+
                          count(typhoid18[BL&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[BL&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[BakakengCentral&SS=="Probable"&MW==n,])+
                          count(typhoid18[BakakengCentral&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[BakakengCentral&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[BakakengNorth&SS=="Probable"&MW==n,])+
                          count(typhoid18[BakakengNorth&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[BakakengNorth&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[Bal&SS=="Probable"&MW==n,])+
                          count(typhoid18[Bal&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[Bal&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[Balsigan&SS=="Probable"&MW==n,])+
                          count(typhoid18[Balsigan&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[Balsigan&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[BPE&SS=="Probable"&MW==n,])+
                          count(typhoid18[BPE&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[BPE&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[BPV&SS=="Probable"&MW==n,])+
                          count(typhoid18[BPV&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[BPV&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[Bayan&SS=="Probable"&MW==n,])+
                          count(typhoid18[Bayan&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[Bayan&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[BGH&SS=="Probable"&MW==n,])+
                          count(typhoid18[BGH&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[BGH&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[Brookside&SS=="Probable"&MW==n,])+
                          count(typhoid18[Brookside&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[Brookside&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[Brookspoint&SS=="Probable"&MW==n,])+
                          count(typhoid18[Brookspoint&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[Brookspoint&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[CabinetHill&SS=="Probable"&MW==n,])+
                          count(typhoid18[CabinetHill&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[CabinetHill&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[Camdas&SS=="Probable"&MW==n,])+
                          count(typhoid18[Camdas&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[Camdas&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[c7&SS=="Probable"&MW==n,])+
                          count(typhoid18[c7&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[c7&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[c8&SS=="Probable"&MW==n,])+
                          count(typhoid18[c8&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[c8&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[ca&SS=="Probable"&MW==n,])+
                          count(typhoid18[ca&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[ca&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[cf&SS=="Probable"&MW==n,])+
                          count(typhoid18[cf&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[cf&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[CityCC&SS=="Probable"&MW==n,])+
                          count(typhoid18[CityCC&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[CityCC&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[CityCP&SS=="Probable"&MW==n,])+
                          count(typhoid18[CityCP&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[CityCP&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[ccv&SS=="Probable"&MW==n,])+
                          count(typhoid18[ccv&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[ccv&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[Cres&SS=="Probable"&MW==n,])+
                          count(typhoid18[Cres&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[Cres&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[DagisanL&SS=="Probable"&MW==n,])+
                          count(typhoid18[DagisanL&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[DagisanL&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[DagisanU&SS=="Probable"&MW==n,])+
                          count(typhoid18[DagisanU&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[DagisanU&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[Dizon&SS=="Probable"&MW==n,])+
                          count(typhoid18[Dizon&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[Dizon&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[Domin&SS=="Probable"&MW==n,])+
                          count(typhoid18[Domin&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[Domin&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[Dontogan&SS=="Probable"&MW==n,])+
                          count(typhoid18[Dontogan&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[Dontogan&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[DPS&SS=="Probable"&MW==n,])+
                          count(typhoid18[DPS&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[DPS&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[enghill&SS=="Probable"&MW==n,])+
                          count(typhoid18[enghill&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[enghill&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[fvill&SS=="Probable"&MW==n,])+
                          count(typhoid18[fvill&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[fvill&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[Ferdinand&SS=="Probable"&MW==n,])+
                          count(typhoid18[Ferdinand&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[Ferdinand&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[B=="FORT DEL PILAR"&SS=="Probable"&MW==n,])+
                          count(typhoid18[B=="FORT DEL PILAR"&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[B=="FORT DEL PILAR"&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[GabSi&SS=="Probable"&MW==n,])+
                          count(typhoid18[GabSi&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[GabSi&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[GenE&SS=="Probable"&MW==n,])+
                          count(typhoid18[GenE&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[GenE&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[GenLL&SS=="Probable"&MW==n,])+
                          count(typhoid18[GenLL&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[GenLL&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[GenLU&SS=="Probable"&MW==n,])+
                          count(typhoid18[GenLU&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[GenLU&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[Gib&SS=="Probable"&MW==n,])+
                          count(typhoid18[Gib&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[Gib&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[Greenwater&SS=="Probable"&MW==n,])+
                          count(typhoid18[Greenwater&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[Greenwater&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[GuisadC&SS=="Probable"&MW==n,])+
                          count(typhoid18[GuisadC&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[GuisadC&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[GuisadS&SS=="Probable"&MW==n,])+
                          count(typhoid18[GuisadS&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[GuisadS&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[HHol&SS=="Probable"&MW==n,])+
                          count(typhoid18[HHol&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[HHol&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[HHom&SS=="Probable"&MW==n,])+
                          count(typhoid18[HHom&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[HHom&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[HCC&SS=="Probable"&MW==n,])+
                          count(typhoid18[HCC&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[HCC&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[Hillside&SS=="Probable"&MW==n,])+
                          count(typhoid18[Hillside&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[Hillside&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[HGhostE&SS=="Probable"&MW==n,])+
                          count(typhoid18[HGhostE&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[HGhostE&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[HGhostP&SS=="Probable"&MW==n,])+
                          count(typhoid18[HGhostP&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[HGhostP&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[Honeymoon&SS=="Probable"&MW==n,])+
                          count(typhoid18[Honeymoon&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[Honeymoon&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[IMarcos&SS=="Probable"&MW==n,])+
                          count(typhoid18[IMarcos&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[IMarcos&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[Imelda&SS=="Probable"&MW==n,])+
                          count(typhoid18[Imelda&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[Imelda&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[B=="IRISAN"&SS=="Probable"&MW==n,])+
                          count(typhoid18[B=="IRISAN"&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[B=="IRISAN"&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[Kabayanihan&SS=="Probable"&MW==n,])+
                          count(typhoid18[Kabayanihan&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[Kabayanihan&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[Kagitingan&SS=="Probable"&MW==n,])+
                          count(typhoid18[Kagitingan&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[Kagitingan&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[KayangHill&SS=="Probable"&MW==n,])+
                          count(typhoid18[KayangHill&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[KayangHill&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[KayangE&SS=="Probable"&MW==n,])+
                          count(typhoid18[KayangE&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[KayangE&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[kias&SS=="Probable"&MW==n,])+
                          count(typhoid18[kias&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[kias&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[LBK&SS=="Probable"&MW==n,])+
                          count(typhoid18[LBK&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[LBK&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[LLoak&SS=="Probable"&MW==n,])+
                          count(typhoid18[LLoak&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[LLoak&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[LoakP&SS=="Probable"&MW==n,])+
                          count(typhoid18[LoakP&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[LoakP&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[LOPJ&SS=="Probable"&MW==n,])+
                          count(typhoid18[LOPJ&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[LOPJ&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[LourdesE&SS=="Probable"&MW==n,])+
                          count(typhoid18[LourdesE&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[LourdesE&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[LourdesL&SS=="Probable"&MW==n,])+
                          count(typhoid18[LourdesL&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[LourdesL&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[LourdesP&SS=="Probable"&MW==n,])+
                          count(typhoid18[LourdesP&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[LourdesP&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[Lualhati&SS=="Probable"&MW==n,])+
                          count(typhoid18[Lualhati&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[Lualhati&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[lucnab&SS=="Probable"&MW==n,])+
                          count(typhoid18[lucnab&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[lucnab&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[MagsaysayPR&SS=="Probable"&MW==n,])+
                          count(typhoid18[MagsaysayPR&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[MagsaysayPR&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[MagsaysayL&SS=="Probable"&MW==n,])+
                          count(typhoid18[MagsaysayL&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[MagsaysayL&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[MagsaysayU&SS=="Probable"&MW==n,])+
                          count(typhoid18[MagsaysayU&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[MagsaysayU&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[MalcolmSquarePerfectoJoseAbadSantos&SS=="Probable"&MW==n,])+
                          count(typhoid18[MalcolmSquarePerfectoJoseAbadSantos&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[MalcolmSquarePerfectoJoseAbadSantos&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[MR&SS=="Probable"&MW==n,])+
                          count(typhoid18[MR&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[MR&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[MarketSubdivisionUpper&SS=="Probable"&MW==n,])+
                          count(typhoid18[MarketSubdivisionUpper&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[MarketSubdivisionUpper&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[MiddleQuezonHillSubdivsion&SS=="Probable"&MW==n,])+
                          count(typhoid18[MiddleQuezonHillSubdivsion&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[MiddleQuezonHillSubdivsion&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[MCutOff&SS=="Probable"&MW==n,])+
                          count(typhoid18[MCutOff&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[MCutOff&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[MinesView&SS=="Probable"&MW==n,])+
                          count(typhoid18[MinesView&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[MinesView&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[ModernSiteE&SS=="Probable"&MW==n,])+
                          count(typhoid18[ModernSiteE&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[ModernSiteE&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[ModernSiteW&SS=="Probable"&MW==n,])+
                          count(typhoid18[ModernSiteW&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[ModernSiteW&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[MRR&SS=="Probable"&MW==n,])+
                          count(typhoid18[MRR&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[MRR&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[NewLuc&SS=="Probable"&MW==n,])+
                          count(typhoid18[NewLuc&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[NewLuc&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[Outlook&SS=="Probable"&MW==n,])+
                          count(typhoid18[Outlook&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[Outlook&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[Pacdal&SS=="Probable"&MW==n,])+
                          count(typhoid18[Pacdal&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[Pacdal&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[PadreB&SS=="Probable"&MW==n,])+
                          count(typhoid18[PadreB&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[PadreB&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[PadreZ&SS=="Probable"&MW==n,])+
                          count(typhoid18[PadreZ&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[PadreZ&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[PU&SS=="Probable"&MW==n,])+
                          count(typhoid18[PU&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[PU&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[PhilAm&SS=="Probable"&MW==n,])+
                          count(typhoid18[PhilAm&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[PhilAm&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[Pinget&SS=="Probable"&MW==n,])+
                          count(typhoid18[Pinget&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[Pinget&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[PPP&SS=="Probable"&MW==n,])+
                          count(typhoid18[PPP&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[PPP&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[PisaoP&SS=="Probable"&MW==n,])+
                          count(typhoid18[PisaoP&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[PisaoP&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[Poliwes&SS=="Probable"&MW==n,])+
                          count(typhoid18[Poliwes&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[Poliwes&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[pucsusan&SS=="Probable"&MW==n,])+
                          count(typhoid18[pucsusan&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[pucsusan&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[QHP&SS=="Probable"&MW==n,])+
                          count(typhoid18[QHP&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[QHP&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[QHillU&SS=="Probable"&MW==n,])+
                          count(typhoid18[QHillU&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[QHillU&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[UpperQM&SS=="Probable"&MW==n,])+
                          count(typhoid18[UpperQM&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[UpperQM&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[QHillE&SS=="Probable"&MW==n,])+
                          count(typhoid18[QHillE&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[QHillE&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[QHL&SS=="Probable"&MW==n,])+
                          count(typhoid18[QHL&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[QHL&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[QHM&SS=="Probable"&MW==n,])+
                          count(typhoid18[QHM&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[QHM&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[QHillW&SS=="Probable"&MW==n,])+
                          count(typhoid18[QHillW&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[QHillW&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[RizalMonumentArea&SS=="Probable"&MW==n,])+
                          count(typhoid18[RizalMonumentArea&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[RizalMonumentArea&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[RockQuarryLower&SS=="Probable"&MW==n,])+
                          count(typhoid18[RockQuarryLower&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[RockQuarryLower&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[RockQuarryMiddle&SS=="Probable"&MW==n,])+
                          count(typhoid18[RockQuarryMiddle&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[RockQuarryMiddle&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[RockQuarryUpper&SS=="Probable"&MW==n,])+
                          count(typhoid18[RockQuarryUpper&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[RockQuarryUpper&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[salud&SS=="Probable"&MW==n,])+
                          count(typhoid18[salud&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[salud&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[SanAntonioVillage&SS=="Probable"&MW==n,])+
                          count(typhoid18[SanAntonioVillage&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[SanAntonioVillage&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[SanLuisVillage&SS=="Probable"&MW==n,])+
                          count(typhoid18[SanLuisVillage&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[SanLuisVillage&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[SanRoqueVillage&SS=="Probable"&MW==n,])+
                          count(typhoid18[SanRoqueVillage&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[SanRoqueVillage&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[sanvic&SS=="Probable"&MW==n,])+
                          count(typhoid18[sanvic&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[sanvic&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[SanCamp&SS=="Probable"&MW==n,])+
                          count(typhoid18[SanCamp&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[SanCamp&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[SanitaryCampSouth&SS=="Probable"&MW==n,])+
                          count(typhoid18[SanitaryCampSouth&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[SanitaryCampSouth&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[STE&SS=="Probable"&MW==n,])+
                          count(typhoid18[STE&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[STE&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[SantoR&SS=="Probable"&MW==n,])+
                          count(typhoid18[SantoR&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[SantoR&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[STP&SS=="Probable"&MW==n,])+
                          count(typhoid18[STP&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[STP&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[STsa&SS=="Probable"&MW==n,])+
                          count(typhoid18[STsa&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[STsa&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[ScoutB&SS=="Probable"&MW==n,])+
                          count(typhoid18[ScoutB&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[ScoutB&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[Session&SS=="Probable"&MW==n,])+
                          count(typhoid18[Session&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[Session&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[Slaughter&SS=="Probable"&MW==n,])+
                          count(typhoid18[Slaughter&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[Slaughter&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[SLU&SS=="Probable"&MW==n,])+
                          count(typhoid18[SLU&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[SLU&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[SouthDrive&SS=="Probable"&MW==n,])+
                          count(typhoid18[SouthDrive&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[SouthDrive&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[TeoAl&SS=="Probable"&MW==n,])+
                          count(typhoid18[TeoAl&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[TeoAl&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[tranco&SS=="Probable"&MW==n,])+
                          count(typhoid18[tranco&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[tranco&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[victoria&SS=="Probable"&MW==n,])+
                          count(typhoid18[victoria&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[victoria&SS=="CONFIRMED"&MW==n,]),
                        count(typhoid18[STJos&SS=="Probable"&MW==n,])+
                          count(typhoid18[STJos&SS=="PROBABLE"&MW==n,])+
                          count(typhoid18[STJos&SS=="CONFIRMED"&MW==n,])
  )
}

B=abd11$Barangay
SC=abd11$StoolCulture
MW=abd11$MorbidityWeek

Imelda = B=="IMELDA VILLAGE"
BakakengCentral = B=="BAKAKENG CENTRAL"
ABCR = B=="A. BONIFACIO-CAGUIOA-RIMANDO (ABCR)"
Ambiong = B=="AMBIONG"
BakakengNorth = B=="BAKAKENG NORTH"
Asin = B=="ASIN"
Balsigan = B=="BALSIGAN"
Bayan = B=="BAYAN PARK WEST (BAYAN PARK)"
BGH=B=="BGH COMPOUND"
Brookside = B=="BROOKSIDE"
Camdas = B=="CAMDAS SUBDIVISION"
c7=B=="CAMP 7"
c8=B=="CAMP 8"
ca=B=="CAMP ALLEN"
cf=B=="CAMPO FILIPINO"
ccv=B=="COUNTRY CLUB VILLAGE"
DPS=B=="DPS AREA"
enghill=B=="ENGINEER'S HILL"
fvill=B=="FAIRVIEW VILLAGE"
GLuna=B=="GENERAL LUNA, UPPER"
Gib=B=="GIBRALTAR"
GuisadC=B=="GUISAD CENTRAL"
GuisadS=B=="GUISAD SORONG"
HHL=B=="HAPPY HOMES (HAPPY HOMES-LUCBAN)"
kias=B=="KIAS"
LLoak=B=="LIWANAG-LOAKAN"
LoakP=B=="LOAKAN PROPER"
LOPJ=B=="LOPEZ JAENA"
lucnab=B=="LUCNAB"
MR=B=="MANUEL A. ROXAS"
MCutOff=B=="MILITARY CUT-OFF"
MRR=B=="MRR-QUEEN OF PEACE"
PadreZ=B=="PADRE ZAMORA"
Pinget=B=="PINGET"
PPP=B=="PINSAO PILOT PROJECT"
PisaoP=B=="PINSAO PROPER"
Poliwes=B=="POLIWES"
QHP=B=="QUEZON HILL PROPER"
QHL=B=="QUIRINO HILL, LOWER"
QHM=B=="QUIRINO HILL, MIDDLE"
salud=B=="SALUD MITRA"
sanvic=B=="SAN VICENTE"
SanCamp=B=="SANITARY CAMP, NORTH"
STE=B=="SANTA ESCOLASTICA"
STP=B=="SANTO TOMAS PROPER"&B=="STO. TOMAS PROPER"
STsa=B=="SANTO TOMAS SCHOOL AREA"
tranc=B=="TRANCOVILLE"
birac=B=="BIRAC"
AL=B=="APUGAN-LOAKAN"
AZKCO=B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")
ATAB=B==toupper("Alfonso Tabora")
AB=B==toupper("Andres Bonifacio (Lower Bokawkan)")
ATOK=B==toupper("Atok Trail")
auroraP=B==toupper("Aurora Hill Proper (Malvar-Sgt. Floresca)")
auroraN=B==toupper("Aurora Hill, North Central")
auroraS=B==toupper("Aurora Hill, South Central")
BL=B==toupper("Bagong Lipunan (Market Area)")
Bal=B==toupper("Bal-Marcoville (Marcoville)")
BPE=B==toupper("Bayan Park East")
BPV=B==toupper("Bayan Park Village")
Brookspoint=B==toupper("Brookspoint")
CabinetHill=B==toupper("Cabinet Hill-Teacher's Camp")
CityCC=B==toupper("City Camp Central")
CityCP=B==toupper("City Camp Proper")
DagisanL=B==toupper("Dagsian, Lower")
Cres=B==toupper("Cresencia Village")
DagisanU=B==toupper("Dagsian, Upper")
Dizon=B==toupper("Dizon Subdivision")
Domin=B==toupper("Dominican Hill-Mirador")
Dontogan=B=="DONTOGAN"
DPS=B=="DPS AREA"
Ferdinand=B==toupper("Ferdinand (Happy Homes-Campo Sioco)")
GabSi=B=="GABRIELLA SILANG"
GenE=B==toupper("General Emilio F. Aguinaldo (Quirino-Magsaysay, Lower)")
GenLL=B==toupper("General Luna, Lower")
GenLU=B==toupper("General Luna, Upper")
Greenwater=B==toupper("Greenwater Village")
HHol=B==toupper("Happy Hollow")
HHom=B==toupper("Happy Homes (Happy Homes-Lucban)")
HCC=B==toupper("Harrison-Claudio Carantes")
Hillside=B==toupper("Hillside")
HGhostE=B==toupper("Holy Ghost Extension")
HGhostP=B==toupper("Holy Ghost Proper")
Honeymoon=B==toupper("Honeymoon (Honeymoon-Holy Ghost)")
IMarcos=B==toupper("Imelda R. Marcos (La Salle)")
Imelda=B==toupper("Imelda Village")
Kabayanihan=B==toupper("Kabayanihan")
Kagitingan=B==toupper("Kagitingan")
KayangHill=B==toupper("Kayang-Hilltop")
KayangE=B==toupper("Kayang Extension")
LBK=B==toupper("Legarda-Burnham-Kisad")
LourdesE=B==toupper("Lourdes Subdivision Extension")
LourdesL=B==toupper("Lourdes Subdivision, Lower")
LourdesP=B==toupper("Lourdes Subdivision, Proper")
Lualhati=B==toupper("Lualhati")
MagsaysayPR=B==toupper("Magsaysay Private Road")
MagsaysayL=B==toupper("Magsaysay, Lower")
MagsaysayU=B==toupper("Magsaysay, Upper")
MalcolmSquarePerfectoJoseAbadSantos=B==toupper("Malcolm Square-Perfecto (Jose Abad Santos)")
MarketSubdivisionUpper=B==toupper("Market Subdivision, Upper")
MiddleQuezonHillSubdivsion=B==toupper("Middle Quezon Hill Subdivsiion(Quezon Hill Middle)")
MinesView=B==toupper("Mines View Park")
ModernSiteE=B==toupper("Modern Site, East")
ModernSiteW=B==toupper("Modern Site, West")
NewLuc=B==toupper("New Lucban")
Outlook=B==toupper("Outlook Drive")
Pacdal=B==toupper("Pacdal")
PadreB=B==toupper("Padre Burgos")
PadreZ=B==toupper("Padre Zamora")
PU=B==toupper("Palma-Urbano (Cariño-Palma)")
PhilAm=B==toupper("Phil-Am")
pucsusan=B==toupper("Pucsusan")
QHillU=B==toupper("Quezon Hill, Upper")
UpperQM=B==toupper("Quirino-Magsaysay, Upper (Upper QM)")
QHillE=B==toupper("Quirino Hill, East")
QHillW=B==toupper("Quirino Hill, West")
RizalMonumentArea=B==toupper("Rizal Monument Area")
RockQuarryLower=B==toupper("Rock Quarry, Lower")
RockQuarryMiddle=B==toupper("Rock Quarry, Middle")
RockQuarryUpper=B==toupper("Rock Quarry, Upper")
SanAntonioVillage=B==toupper("San Antonio Village")
SanLuisVillage=B==toupper("San Luis Village")
SanRoqueVillage=B==toupper("San Roque Village")
SanitaryCampSouth=B==toupper("Sanitary Camp, South")
SantoR=B==toupper("Santo Rosario")
ScoutB=B==toupper("Scout Barrio")
Session=B==toupper("Session Road Area")
Slaughter=B==toupper("Slaughter House Area (Santo Niño Slaughter)")
SLU=B==toupper("SLU-SVP Housing Village")
SouthDrive=B==toupper("South Drive")
TeoAl=B==toupper("Teodora Alonzo")
tranco=B==toupper("Trancoville")
victoria=B==toupper("Victoria Village")
STJos=B=="ST JOSEPH VILLAGE"

adf11 <- data.frame(MorbidityWeek=1,
                    ABonifacioCaguioaRimandoABCR=0,
                    AbanaoZanduetaKayongChugumOtekAZKCO=0,
                    AlfonsoTabora=0,
                    Ambiong=0,
                    AndresBonifacioLowerBokawkan=0,
                    ApuganLoakan=0,
                    AsinRoad=0,
                    AtokTrail=0,
                    AuroraHillProperMalvarSgtFloresca=0,
                    AuroraHillNorthCentral=0,
                    AuroraHillSouthCentral=0,
                    BagongLipunanMarketArea=0,
                    BakakengCentral=0,
                    BakakengNorth=1,
                    BalMarcovilleMarcoville=0,
                    Balsigan=0,
                    BayanParkEast=0,
                    BayanParkVillage=0,
                    BayanParkWestBayanPark=0,
                    BGHCompound=0,
                    Brookside=0,
                    Brookspoint=0,
                    CabinetHillTeachersCamp=0,
                    CamdasSubdivision=0,
                    Camp7=0,
                    Camp8=0,
                    CampAllen=0,
                    CampoFilipino=0,
                    CityCampCentral=0,
                    CityCampProper=0,
                    CountryClubVillage=0,
                    CresenciaVillage=0,
                    DagisanLower=0,
                    DagisanUpper=0,
                    DizonSubdivision=0,
                    DominicanHillMirador=0,
                    Dontogan=0,
                    DPSArea=0,
                    EngineersHill=0,
                    FairviewVillage=0,
                    FerdinandHappyHomesCampoSioco=0,
                    FortdelPilar=0,
                    GabrielaSilang=0,
                    GeneralEmilioFAguinaldoQuirinoMagsaysayLower=0,
                    GeneralLunaLower=0,
                    GeneralLunaUpper=0,
                    Gibraltar=0,
                    GreenwaterVillage=0,
                    GuisadCentral=0,
                    GuisadSorong=0,
                    HappyHollow=0,
                    HappyHomesHappyHomesLucban=0,
                    HarrisonClaudioCarantes=0,
                    Hillside=0,
                    HolyGhostExtension=0,
                    HolyGhostProper=0,
                    HoneymoonHoneymoonHolyGhost=0,
                    ImeldaRMarcosLaSalle=0,
                    ImeldaVillage=0,
                    Irisan=0,
                    Kabayanihan=0,
                    Kagitingan=0,
                    KayangHilltop=0,
                    KayangExtension=0,
                    Kias=0,
                    LegardaBurnhamKisad=0,
                    LiwanagLoakan=0,
                    LoakanProper=0,
                    LopezJaena=0,
                    LourdesSubdivisionExtension=0,
                    LourdesSubdivisionLower=0,
                    LourdesSubdivisionProper=0,
                    Lualhati=0,
                    Lucnab=0,
                    MagsaysayPrivateRoad=0,
                    MagsaysayLower=0,
                    MagsaysayUpper=0,
                    MalcolmSquarePerfectoJoseAbadSantos=0,
                    ManuelARoxas=0,
                    MarketSubdivisionUpper=0,
                    MiddleQuezonHillSubdivsion=0,
                    MilitaryCutoff=0,
                    MinesViewPark=0,
                    ModernSiteEast=0,
                    ModernSiteWest=0,
                    MRRQueenofPeace=0,
                    NewLucban=0,
                    OutlookDrive=0,
                    Pacdal=0,
                    PadreBurgos=0,
                    PadreZamora=0,
                    PalmaUrbanoCarinoPalma=0,
                    PhilAm=0,
                    Pinget=0,
                    PinsaoPilotProject=0,
                    PinsaoProper=0,
                    Poliwes=0,
                    Pucsusan=0,
                    QuezonHillProper=0,
                    QuezonHillUpper=0,
                    QuirinoMagsaysayUpperUpperQM=0,
                    QuirinoHillEast=0,
                    QuirinoHillLower=0,
                    QuirinoHillMiddle=0,
                    QuirinoHillWest=0,
                    RizalMonumentArea=0,
                    RockQuarryLower=0,
                    RockQuarryMiddle=0,
                    RockQuarryUpper=0,
                    SaludMitra=0,
                    SanAntoniVillage=0,
                    SanLuisVillage=0,
                    SanRoqueVillage=0,
                    SanVicente=0,
                    SanitaryCampNorth=0,
                    SanitaryCampSouth=0,
                    SantaEscolastica=0,
                    SantoRosario=0,
                    SantoTomasProper=0,
                    SantoTomasSchoolArea=0,
                    ScoutBarrio=0,
                    SessionRoadArea=0,
                    SlaughterHouseAreaSantoNiñoSlaughter=0,
                    SLUSVPHousingVillage=0,
                    SouthDrive=0,
                    TeodoraAlonzo=0,
                    Trancoville=0,
                    VictoriaVillage=0,
                    SaintJosephVillage=0
)

for (n in 2:52) {
  adf11[nrow(adf11)+1,]=c(n,
                          count(abd11[ABCR&SC=="Positive"&MW==n,])+
                            count(abd11[ABCR&SC=="POSITIVE"&MW==n,]),
                          count(abd11[B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")&SC=="Positive"&MW==n,])+
                            count(abd11[B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")&SC=="POSITIVE"&MW==n,]),
                          count(abd11[B==toupper("Alfonso Tabora")&SC=="Positive"&MW==n,])+
                            count(abd11[B==toupper("Alfonso Tabora")&SC=="POSITIVE"&MW==n,]),
                          count(abd11[B=="AMBIONG"&SC=="Positive"&MW==n,])+
                            count(abd11[B=="AMBIONG"&SC=="POSITIVE"&MW==n,]),
                          count(abd11[AB&SC=="Positive"&MW==n,])+
                            count(abd11[AB&SC=="POSITIVE"&MW==n,]),
                          count(abd11[AL&SC=="Positive"&MW==n,])+
                            count(abd11[AL&SC=="POSITIVE"&MW==n,]),
                          count(abd11[Asin&SC=="Positive"&MW==n,])+
                            count(abd11[Asin&SC=="POSITIVE"&MW==n,]),
                          count(abd11[ATOK&SC=="Positive"&MW==n,])+
                            count(abd11[ATOK&SC=="POSITIVE"&MW==n,]),
                          count(abd11[auroraP&SC=="Positive"&MW==n,])+
                            count(abd11[auroraP&SC=="POSITIVE"&MW==n,]),
                          count(abd11[auroraN&SC=="Positive"&MW==n,])+
                            count(abd11[auroraN&SC=="POSITIVE"&MW==n,]),
                          count(abd11[auroraS&SC=="POSITIVE"&MW==n,]),
                          count(abd11[BL&SC=="Positive"&MW==n,])+
                            count(abd11[BL&SC=="POSITIVE"&MW==n,]),
                          count(abd11[BakakengCentral&SC=="Positive"&MW==n,])+
                            count(abd11[BakakengCentral&SC=="POSITIVE"&MW==n,]),
                          count(abd11[BakakengNorth&SC=="Positive"&MW==n,])+
                            count(abd11[BakakengNorth&SC=="POSITIVE"&MW==n,]),
                          count(abd11[Bal&SC=="Positive"&MW==n,])+
                            count(abd11[Bal&SC=="POSITIVE"&MW==n,]),
                          count(abd11[Balsigan&SC=="Positive"&MW==n,])+
                            count(abd11[Balsigan&SC=="POSITIVE"&MW==n,]),
                          count(abd11[BPE&SC=="Positive"&MW==n,])+
                            count(abd11[BPE&SC=="POSITIVE"&MW==n,]),
                          count(abd11[BPV&SC=="Positive"&MW==n,])+
                            count(abd11[BPV&SC=="POSITIVE"&MW==n,]),
                          count(abd11[Bayan&SC=="Positive"&MW==n,])+
                            count(abd11[Bayan&SC=="POSITIVE"&MW==n,]),
                          count(abd11[BGH&SC=="Positive"&MW==n,])+
                            count(abd11[BGH&SC=="POSITIVE"&MW==n,]),
                          count(abd11[Brookside&SC=="Positive"&MW==n,])+
                            count(abd11[Brookside&SC=="POSITIVE"&MW==n,]),
                          count(abd11[Brookspoint&SC=="Positive"&MW==n,])+
                            count(abd11[Brookspoint&SC=="POSITIVE"&MW==n,]),
                          count(abd11[CabinetHill&SC=="Positive"&MW==n,])+
                            count(abd11[CabinetHill&SC=="POSITIVE"&MW==n,]),
                          count(abd11[Camdas&SC=="Positive"&MW==n,])+
                            count(abd11[Camdas&SC=="POSITIVE"&MW==n,]),
                          count(abd11[c7&SC=="Positive"&MW==n,])+
                            count(abd11[c7&SC=="POSITIVE"&MW==n,]),
                          count(abd11[c8&SC=="Positive"&MW==n,])+
                            count(abd11[c8&SC=="POSITIVE"&MW==n,]),
                          count(abd11[ca&SC=="Positive"&MW==n,])+
                            count(abd11[ca&SC=="POSITIVE"&MW==n,]),
                          count(abd11[cf&SC=="Positive"&MW==n,])+
                            count(abd11[cf&SC=="POSITIVE"&MW==n,]),
                          count(abd11[CityCC&SC=="Positive"&MW==n,])+
                            count(abd11[CityCC&SC=="POSITIVE"&MW==n,]),
                          count(abd11[CityCP&SC=="Positive"&MW==n,])+
                            count(abd11[CityCP&SC=="POSITIVE"&MW==n,]),
                          count(abd11[ccv&SC=="Positive"&MW==n,])+
                            count(abd11[ccv&SC=="POSITIVE"&MW==n,]),
                          count(abd11[Cres&SC=="Positive"&MW==n,])+
                            count(abd11[Cres&SC=="POSITIVE"&MW==n,]),
                          count(abd11[DagisanL&SC=="Positive"&MW==n,])+
                            count(abd11[DagisanL&SC=="POSITIVE"&MW==n,]),
                          count(abd11[DagisanU&SC=="Positive"&MW==n,])+
                            count(abd11[DagisanU&SC=="POSITIVE"&MW==n,]),
                          count(abd11[Dizon&SC=="Positive"&MW==n,])+
                            count(abd11[Dizon&SC=="POSITIVE"&MW==n,]),
                          count(abd11[Domin&SC=="Positive"&MW==n,])+
                            count(abd11[Domin&SC=="POSITIVE"&MW==n,]),
                          count(abd11[Dontogan&SC=="Positive"&MW==n,])+
                            count(abd11[Dontogan&SC=="POSITIVE"&MW==n,]),
                          count(abd11[DPS&SC=="Positive"&MW==n,])+
                            count(abd11[DPS&SC=="POSITIVE"&MW==n,]),
                          count(abd11[enghill&SC=="Positive"&MW==n,])+
                            count(abd11[enghill&SC=="POSITIVE"&MW==n,]),
                          count(abd11[fvill&SC=="Positive"&MW==n,])+
                            count(abd11[fvill&SC=="POSITIVE"&MW==n,]),
                          count(abd11[Ferdinand&SC=="Positive"&MW==n,])+
                            count(abd11[Ferdinand&SC=="POSITIVE"&MW==n,]),
                          count(abd11[B=="FORT DEL PILAR"&SC=="Positive"&MW==n,])+
                            count(abd11[B=="FORT DEL PILAR"&SC=="POSITIVE"&MW==n,]),
                          count(abd11[GabSi&SC=="Positive"&MW==n,])+
                            count(abd11[GabSi&SC=="POSITIVE"&MW==n,]),
                          count(abd11[GenE&SC=="Positive"&MW==n,])+
                            count(abd11[GenE&SC=="POSITIVE"&MW==n,]),
                          count(abd11[GenLL&SC=="Positive"&MW==n,])+
                            count(abd11[GenLL&SC=="POSITIVE"&MW==n,]),
                          count(abd11[GenLU&SC=="Positive"&MW==n,])+
                            count(abd11[GenLU&SC=="POSITIVE"&MW==n,]),
                          count(abd11[Gib&SC=="Positive"&MW==n,])+
                            count(abd11[Gib&SC=="POSITIVE"&MW==n,]),
                          count(abd11[Greenwater&SC=="Positive"&MW==n,])+
                            count(abd11[Greenwater&SC=="POSITIVE"&MW==n,]),
                          count(abd11[GuisadC&SC=="Positive"&MW==n,])+
                            count(abd11[GuisadC&SC=="POSITIVE"&MW==n,]),
                          count(abd11[GuisadS&SC=="Positive"&MW==n,])+
                            count(abd11[GuisadS&SC=="POSITIVE"&MW==n,]),
                          count(abd11[HHol&SC=="Positive"&MW==n,])+
                            count(abd11[HHol&SC=="POSITIVE"&MW==n,]),
                          count(abd11[HHom&SC=="Positive"&MW==n,])+
                            count(abd11[HHom&SC=="POSITIVE"&MW==n,]),
                          count(abd11[HCC&SC=="Positive"&MW==n,])+
                            count(abd11[HCC&SC=="POSITIVE"&MW==n,]),
                          count(abd11[Hillside&SC=="Positive"&MW==n,])+
                            count(abd11[Hillside&SC=="POSITIVE"&MW==n,]),
                          count(abd11[HGhostE&SC=="Positive"&MW==n,])+
                            count(abd11[HGhostE&SC=="POSITIVE"&MW==n,]),
                          count(abd11[HGhostP&SC=="Positive"&MW==n,])+
                            count(abd11[HGhostP&SC=="POSITIVE"&MW==n,]),
                          count(abd11[Honeymoon&SC=="Positive"&MW==n,])+
                            count(abd11[Honeymoon&SC=="POSITIVE"&MW==n,]),
                          count(abd11[IMarcos&SC=="Positive"&MW==n,])+
                            count(abd11[IMarcos&SC=="POSITIVE"&MW==n,]),
                          count(abd11[Imelda&SC=="Positive"&MW==n,])+
                            count(abd11[Imelda&SC=="POSITIVE"&MW==n,]),
                          count(abd11[B=="IRISAN"&SC=="Positive"&MW==n,])+
                            count(abd11[B=="IRISAN"&SC=="POSITIVE"&MW==n,]),
                          count(abd11[Kabayanihan&SC=="Positive"&MW==n,])+
                            count(abd11[Kabayanihan&SC=="POSITIVE"&MW==n,]),
                          count(abd11[Kagitingan&SC=="Positive"&MW==n,])+
                            count(abd11[Kagitingan&SC=="POSITIVE"&MW==n,]),
                          count(abd11[KayangHill&SC=="Positive"&MW==n,])+
                            count(abd11[KayangHill&SC=="POSITIVE"&MW==n,]),
                          count(abd11[KayangE&SC=="Positive"&MW==n,])+
                            count(abd11[KayangE&SC=="POSITIVE"&MW==n,]),
                          count(abd11[kias&SC=="Positive"&MW==n,])+
                            count(abd11[kias&SC=="POSITIVE"&MW==n,]),
                          count(abd11[LBK&SC=="Positive"&MW==n,])+
                            count(abd11[LBK&SC=="POSITIVE"&MW==n,]),
                          count(abd11[LLoak&SC=="Positive"&MW==n,])+
                            count(abd11[LLoak&SC=="POSITIVE"&MW==n,]),
                          count(abd11[LoakP&SC=="Positive"&MW==n,])+
                            count(abd11[LoakP&SC=="POSITIVE"&MW==n,]),
                          count(abd11[LOPJ&SC=="Positive"&MW==n,])+
                            count(abd11[LOPJ&SC=="POSITIVE"&MW==n,]),
                          count(abd11[LourdesE&SC=="Positive"&MW==n,])+
                            count(abd11[LourdesE&SC=="POSITIVE"&MW==n,]),
                          count(abd11[LourdesL&SC=="Positive"&MW==n,])+
                            count(abd11[LourdesL&SC=="POSITIVE"&MW==n,]),
                          count(abd11[LourdesP&SC=="Positive"&MW==n,])+
                            count(abd11[LourdesP&SC=="POSITIVE"&MW==n,]),
                          count(abd11[Lualhati&SC=="Positive"&MW==n,])+
                            count(abd11[Lualhati&SC=="POSITIVE"&MW==n,]),
                          count(abd11[lucnab&SC=="Positive"&MW==n,])+
                            count(abd11[lucnab&SC=="POSITIVE"&MW==n,]),
                          count(abd11[MagsaysayPR&SC=="Positive"&MW==n,])+
                            count(abd11[MagsaysayPR&SC=="POSITIVE"&MW==n,]),
                          count(abd11[MagsaysayL&SC=="Positive"&MW==n,])+
                            count(abd11[MagsaysayL&SC=="POSITIVE"&MW==n,]),
                          count(abd11[MagsaysayU&SC=="Positive"&MW==n,])+
                            count(abd11[MagsaysayU&SC=="POSITIVE"&MW==n,]),
                          count(abd11[MalcolmSquarePerfectoJoseAbadSantos&SC=="Positive"&MW==n,])+
                            count(abd11[MalcolmSquarePerfectoJoseAbadSantos&SC=="POSITIVE"&MW==n,]),
                          count(abd11[MR&SC=="Positive"&MW==n,])+
                            count(abd11[MR&SC=="POSITIVE"&MW==n,]),
                          count(abd11[MarketSubdivisionUpper&SC=="Positive"&MW==n,])+
                            count(abd11[MarketSubdivisionUpper&SC=="POSITIVE"&MW==n,]),
                          count(abd11[MiddleQuezonHillSubdivsion&SC=="Positive"&MW==n,])+
                            count(abd11[MiddleQuezonHillSubdivsion&SC=="POSITIVE"&MW==n,]),
                          count(abd11[MCutOff&SC=="Positive"&MW==n,])+
                            count(abd11[MCutOff&SC=="POSITIVE"&MW==n,]),
                          count(abd11[MinesView&SC=="Positive"&MW==n,])+
                            count(abd11[MinesView&SC=="POSITIVE"&MW==n,]),
                          count(abd11[ModernSiteE&SC=="Positive"&MW==n,])+
                            count(abd11[ModernSiteE&SC=="POSITIVE"&MW==n,]),
                          count(abd11[ModernSiteW&SC=="Positive"&MW==n,])+
                            count(abd11[ModernSiteW&SC=="POSITIVE"&MW==n,]),
                          count(abd11[MRR&SC=="Positive"&MW==n,])+
                            count(abd11[MRR&SC=="POSITIVE"&MW==n,]),
                          count(abd11[NewLuc&SC=="Positive"&MW==n,])+
                            count(abd11[NewLuc&SC=="POSITIVE"&MW==n,]),
                          count(abd11[Outlook&SC=="Positive"&MW==n,])+
                            count(abd11[Outlook&SC=="POSITIVE"&MW==n,]),
                          count(abd11[Pacdal&SC=="Positive"&MW==n,])+
                            count(abd11[Pacdal&SC=="POSITIVE"&MW==n,]),
                          count(abd11[PadreB&SC=="Positive"&MW==n,])+
                            count(abd11[PadreB&SC=="POSITIVE"&MW==n,]),
                          count(abd11[PadreZ&SC=="Positive"&MW==n,])+
                            count(abd11[PadreZ&SC=="POSITIVE"&MW==n,]),
                          count(abd11[PU&SC=="Positive"&MW==n,])+
                            count(abd11[PU&SC=="POSITIVE"&MW==n,]),
                          count(abd11[PhilAm&SC=="Positive"&MW==n,])+
                            count(abd11[PhilAm&SC=="POSITIVE"&MW==n,]),
                          count(abd11[Pinget&SC=="Positive"&MW==n,])+
                            count(abd11[Pinget&SC=="POSITIVE"&MW==n,]),
                          count(abd11[PPP&SC=="Positive"&MW==n,])+
                            count(abd11[PPP&SC=="POSITIVE"&MW==n,]),
                          count(abd11[PisaoP&SC=="Positive"&MW==n,])+
                            count(abd11[PisaoP&SC=="POSITIVE"&MW==n,]),
                          count(abd11[Poliwes&SC=="Positive"&MW==n,])+
                            count(abd11[Poliwes&SC=="POSITIVE"&MW==n,]),
                          count(abd11[pucsusan&SC=="Positive"&MW==n,])+
                            count(abd11[pucsusan&SC=="POSITIVE"&MW==n,]),
                          count(abd11[QHP&SC=="Positive"&MW==n,])+
                            count(abd11[QHP&SC=="POSITIVE"&MW==n,]),
                          count(abd11[QHillU&SC=="Positive"&MW==n,])+
                            count(abd11[QHillU&SC=="POSITIVE"&MW==n,]),
                          count(abd11[UpperQM&SC=="Positive"&MW==n,])+
                            count(abd11[UpperQM&SC=="POSITIVE"&MW==n,]),
                          count(abd11[QHillE&SC=="Positive"&MW==n,])+
                            count(abd11[QHillE&SC=="POSITIVE"&MW==n,]),
                          count(abd11[QHL&SC=="Positive"&MW==n,])+
                            count(abd11[QHL&SC=="POSITIVE"&MW==n,]),
                          count(abd11[QHM&SC=="Positive"&MW==n,])+
                            count(abd11[QHM&SC=="POSITIVE"&MW==n,]),
                          count(abd11[QHillW&SC=="Positive"&MW==n,])+
                            count(abd11[QHillW&SC=="POSITIVE"&MW==n,]),
                          count(abd11[RizalMonumentArea&SC=="Positive"&MW==n,])+
                            count(abd11[RizalMonumentArea&SC=="POSITIVE"&MW==n,]),
                          count(abd11[RockQuarryLower&SC=="Positive"&MW==n,])+
                            count(abd11[RockQuarryLower&SC=="POSITIVE"&MW==n,]),
                          count(abd11[RockQuarryMiddle&SC=="Positive"&MW==n,])+
                            count(abd11[RockQuarryMiddle&SC=="POSITIVE"&MW==n,]),
                          count(abd11[RockQuarryUpper&SC=="Positive"&MW==n,])+
                            count(abd11[RockQuarryUpper&SC=="POSITIVE"&MW==n,]),
                          count(abd11[salud&SC=="Positive"&MW==n,])+
                            count(abd11[salud&SC=="POSITIVE"&MW==n,]),
                          count(abd11[SanAntonioVillage&SC=="Positive"&MW==n,])+
                            count(abd11[SanAntonioVillage&SC=="POSITIVE"&MW==n,]),
                          count(abd11[SanLuisVillage&SC=="Positive"&MW==n,])+
                            count(abd11[SanLuisVillage&SC=="POSITIVE"&MW==n,]),
                          count(abd11[SanRoqueVillage&SC=="Positive"&MW==n,])+
                            count(abd11[SanRoqueVillage&SC=="POSITIVE"&MW==n,]),
                          count(abd11[sanvic&SC=="Positive"&MW==n,])+
                            count(abd11[sanvic&SC=="POSITIVE"&MW==n,]),
                          count(abd11[SanCamp&SC=="Positive"&MW==n,])+
                            count(abd11[SanCamp&SC=="POSITIVE"&MW==n,]),
                          count(abd11[SanitaryCampSouth&SC=="Positive"&MW==n,])+
                            count(abd11[SanitaryCampSouth&SC=="POSITIVE"&MW==n,]),
                          count(abd11[STE&SC=="Positive"&MW==n,])+
                            count(abd11[STE&SC=="POSITIVE"&MW==n,]),
                          count(abd11[SantoR&SC=="Positive"&MW==n,])+
                            count(abd11[SantoR&SC=="POSITIVE"&MW==n,]),
                          count(abd11[STP&SC=="Positive"&MW==n,])+
                            count(abd11[STP&SC=="POSITIVE"&MW==n,]),
                          count(abd11[STsa&SC=="Positive"&MW==n,])+
                            count(abd11[STsa&SC=="POSITIVE"&MW==n,]),
                          count(abd11[ScoutB&SC=="Positive"&MW==n,])+
                            count(abd11[ScoutB&SC=="POSITIVE"&MW==n,]),
                          count(abd11[Session&SC=="Positive"&MW==n,])+
                            count(abd11[Session&SC=="POSITIVE"&MW==n,]),
                          count(abd11[Slaughter&SC=="Positive"&MW==n,])+
                            count(abd11[Slaughter&SC=="POSITIVE"&MW==n,]),
                          count(abd11[SLU&SC=="Positive"&MW==n,])+
                            count(abd11[SLU&SC=="POSITIVE"&MW==n,]),
                          count(abd11[SouthDrive&SC=="Positive"&MW==n,])+
                            count(abd11[SouthDrive&SC=="POSITIVE"&MW==n,]),
                          count(abd11[TeoAl&SC=="Positive"&MW==n,])+
                            count(abd11[TeoAl&SC=="POSITIVE"&MW==n,]),
                          count(abd11[tranco&SC=="Positive"&MW==n,])+
                            count(abd11[tranco&SC=="POSITIVE"&MW==n,]),
                          count(abd11[victoria&SC=="Positive"&MW==n,])+
                            count(abd11[victoria&SC=="POSITIVE"&MW==n,]),
                          count(abd11[STJos&SC=="Positive"&MW==n,])+
                            count(abd11[STJos&SC=="POSITIVE"&MW==n,])
  )
}

B=abd12$Barangay
SC=abd12$StoolCulture
MW=abd12$MorbidityWeek

Imelda = B=="IMELDA VILLAGE"
BakakengCentral = B=="BAKAKENG CENTRAL"
ABCR = B=="A. BONIFACIO-CAGUIOA-RIMANDO (ABCR)"
Ambiong = B=="AMBIONG"
BakakengNorth = B=="BAKAKENG NORTH"
Asin = B=="ASIN"
Balsigan = B=="BALSIGAN"
Bayan = B=="BAYAN PARK WEST (BAYAN PARK)"
BGH=B=="BGH COMPOUND"
Brookside = B=="BROOKSIDE"
Camdas = B=="CAMDAS SUBDIVISION"
c7=B=="CAMP 7"
c8=B=="CAMP 8"
ca=B=="CAMP ALLEN"
cf=B=="CAMPO FILIPINO"
ccv=B=="COUNTRY CLUB VILLAGE"
DPS=B=="DPS AREA"
enghill=B=="ENGINEER'S HILL"
fvill=B=="FAIRVIEW VILLAGE"
GLuna=B=="GENERAL LUNA, UPPER"
Gib=B=="GIBRALTAR"
GuisadC=B=="GUISAD CENTRAL"
GuisadS=B=="GUISAD SORONG"
HHL=B=="HAPPY HOMES (HAPPY HOMES-LUCBAN)"
kias=B=="KIAS"
LLoak=B=="LIWANAG-LOAKAN"
LoakP=B=="LOAKAN PROPER"
LOPJ=B=="LOPEZ JAENA"
lucnab=B=="LUCNAB"
MR=B=="MANUEL A. ROXAS"
MCutOff=B=="MILITARY CUT-OFF"
MRR=B=="MRR-QUEEN OF PEACE"
PadreZ=B=="PADRE ZAMORA"
Pinget=B=="PINGET"
PPP=B=="PINSAO PILOT PROJECT"
PisaoP=B=="PINSAO PROPER"
Poliwes=B=="POLIWES"
QHP=B=="QUEZON HILL PROPER"
QHL=B=="QUIRINO HILL, LOWER"
QHM=B=="QUIRINO HILL, MIDDLE"
salud=B=="SALUD MITRA"
sanvic=B=="SAN VICENTE"
SanCamp=B=="SANITARY CAMP, NORTH"
STE=B=="SANTA ESCOLASTICA"
STP=B=="SANTO TOMAS PROPER"&B=="STO. TOMAS PROPER"
STsa=B=="SANTO TOMAS SCHOOL AREA"
tranc=B=="TRANCOVILLE"
birac=B=="BIRAC"
AL=B=="APUGAN-LOAKAN"
AZKCO=B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")
ATAB=B==toupper("Alfonso Tabora")
AB=B==toupper("Andres Bonifacio (Lower Bokawkan)")
ATOK=B==toupper("Atok Trail")
auroraP=B==toupper("Aurora Hill Proper (Malvar-Sgt. Floresca)")
auroraN=B==toupper("Aurora Hill, North Central")
auroraS=B==toupper("Aurora Hill, South Central")
BL=B==toupper("Bagong Lipunan (Market Area)")
Bal=B==toupper("Bal-Marcoville (Marcoville)")
BPE=B==toupper("Bayan Park East")
BPV=B==toupper("Bayan Park Village")
Brookspoint=B==toupper("Brookspoint")
CabinetHill=B==toupper("Cabinet Hill-Teacher's Camp")
CityCC=B==toupper("City Camp Central")
CityCP=B==toupper("City Camp Proper")
DagisanL=B==toupper("Dagsian, Lower")
Cres=B==toupper("Cresencia Village")
DagisanU=B==toupper("Dagsian, Upper")
Dizon=B==toupper("Dizon Subdivision")
Domin=B==toupper("Dominican Hill-Mirador")
Dontogan=B=="DONTOGAN"
DPS=B=="DPS AREA"
Ferdinand=B==toupper("Ferdinand (Happy Homes-Campo Sioco)")
GabSi=B=="GABRIELLA SILANG"
GenE=B==toupper("General Emilio F. Aguinaldo (Quirino-Magsaysay, Lower)")
GenLL=B==toupper("General Luna, Lower")
GenLU=B==toupper("General Luna, Upper")
Greenwater=B==toupper("Greenwater Village")
HHol=B==toupper("Happy Hollow")
HHom=B==toupper("Happy Homes (Happy Homes-Lucban)")
HCC=B==toupper("Harrison-Claudio Carantes")
Hillside=B==toupper("Hillside")
HGhostE=B==toupper("Holy Ghost Extension")
HGhostP=B==toupper("Holy Ghost Proper")
Honeymoon=B==toupper("Honeymoon (Honeymoon-Holy Ghost)")
IMarcos=B==toupper("Imelda R. Marcos (La Salle)")
Imelda=B==toupper("Imelda Village")
Kabayanihan=B==toupper("Kabayanihan")
Kagitingan=B==toupper("Kagitingan")
KayangHill=B==toupper("Kayang-Hilltop")
KayangE=B==toupper("Kayang Extension")
LBK=B==toupper("Legarda-Burnham-Kisad")
LourdesE=B==toupper("Lourdes Subdivision Extension")
LourdesL=B==toupper("Lourdes Subdivision, Lower")
LourdesP=B==toupper("Lourdes Subdivision, Proper")
Lualhati=B==toupper("Lualhati")
MagsaysayPR=B==toupper("Magsaysay Private Road")
MagsaysayL=B==toupper("Magsaysay, Lower")
MagsaysayU=B==toupper("Magsaysay, Upper")
MalcolmSquarePerfectoJoseAbadSantos=B==toupper("Malcolm Square-Perfecto (Jose Abad Santos)")
MarketSubdivisionUpper=B==toupper("Market Subdivision, Upper")
MiddleQuezonHillSubdivsion=B==toupper("Middle Quezon Hill Subdivsiion(Quezon Hill Middle)")
MinesView=B==toupper("Mines View Park")
ModernSiteE=B==toupper("Modern Site, East")
ModernSiteW=B==toupper("Modern Site, West")
NewLuc=B==toupper("New Lucban")
Outlook=B==toupper("Outlook Drive")
Pacdal=B==toupper("Pacdal")
PadreB=B==toupper("Padre Burgos")
PadreZ=B==toupper("Padre Zamora")
PU=B==toupper("Palma-Urbano (Cariño-Palma)")
PhilAm=B==toupper("Phil-Am")
pucsusan=B==toupper("Pucsusan")
QHillU=B==toupper("Quezon Hill, Upper")
UpperQM=B==toupper("Quirino-Magsaysay, Upper (Upper QM)")
QHillE=B==toupper("Quirino Hill, East")
QHillW=B==toupper("Quirino Hill, West")
RizalMonumentArea=B==toupper("Rizal Monument Area")
RockQuarryLower=B==toupper("Rock Quarry, Lower")
RockQuarryMiddle=B==toupper("Rock Quarry, Middle")
RockQuarryUpper=B==toupper("Rock Quarry, Upper")
SanAntonioVillage=B==toupper("San Antonio Village")
SanLuisVillage=B==toupper("San Luis Village")
SanRoqueVillage=B==toupper("San Roque Village")
SanitaryCampSouth=B==toupper("Sanitary Camp, South")
SantoR=B==toupper("Santo Rosario")
ScoutB=B==toupper("Scout Barrio")
Session=B==toupper("Session Road Area")
Slaughter=B==toupper("Slaughter House Area (Santo Niño Slaughter)")
SLU=B==toupper("SLU-SVP Housing Village")
SouthDrive=B==toupper("South Drive")
TeoAl=B==toupper("Teodora Alonzo")
tranco=B==toupper("Trancoville")
victoria=B==toupper("Victoria Village")
STJos=B=="ST JOSEPH VILLAGE"

adf12 <- data.frame(MorbidityWeek=1,
                    ABonifacioCaguioaRimandoABCR=0,
                    AbanaoZanduetaKayongChugumOtekAZKCO=0,
                    AlfonsoTabora=0,
                    Ambiong=0,
                    AndresBonifacioLowerBokawkan=0,
                    ApuganLoakan=0,
                    AsinRoad=0,
                    AtokTrail=0,
                    AuroraHillProperMalvarSgtFloresca=0,
                    AuroraHillNorthCentral=0,
                    AuroraHillSouthCentral=0,
                    BagongLipunanMarketArea=0,
                    BakakengCentral=0,
                    BakakengNorth=1,
                    BalMarcovilleMarcoville=0,
                    Balsigan=0,
                    BayanParkEast=0,
                    BayanParkVillage=0,
                    BayanParkWestBayanPark=0,
                    BGHCompound=0,
                    Brookside=0,
                    Brookspoint=0,
                    CabinetHillTeachersCamp=0,
                    CamdasSubdivision=0,
                    Camp7=0,
                    Camp8=0,
                    CampAllen=0,
                    CampoFilipino=0,
                    CityCampCentral=0,
                    CityCampProper=0,
                    CountryClubVillage=0,
                    CresenciaVillage=0,
                    DagisanLower=0,
                    DagisanUpper=0,
                    DizonSubdivision=0,
                    DominicanHillMirador=0,
                    Dontogan=0,
                    DPSArea=0,
                    EngineersHill=0,
                    FairviewVillage=0,
                    FerdinandHappyHomesCampoSioco=0,
                    FortdelPilar=0,
                    GabrielaSilang=0,
                    GeneralEmilioFAguinaldoQuirinoMagsaysayLower=0,
                    GeneralLunaLower=0,
                    GeneralLunaUpper=0,
                    Gibraltar=0,
                    GreenwaterVillage=0,
                    GuisadCentral=0,
                    GuisadSorong=0,
                    HappyHollow=0,
                    HappyHomesHappyHomesLucban=0,
                    HarrisonClaudioCarantes=0,
                    Hillside=0,
                    HolyGhostExtension=0,
                    HolyGhostProper=0,
                    HoneymoonHoneymoonHolyGhost=0,
                    ImeldaRMarcosLaSalle=0,
                    ImeldaVillage=0,
                    Irisan=0,
                    Kabayanihan=0,
                    Kagitingan=0,
                    KayangHilltop=0,
                    KayangExtension=0,
                    Kias=0,
                    LegardaBurnhamKisad=0,
                    LiwanagLoakan=0,
                    LoakanProper=0,
                    LopezJaena=0,
                    LourdesSubdivisionExtension=0,
                    LourdesSubdivisionLower=0,
                    LourdesSubdivisionProper=0,
                    Lualhati=0,
                    Lucnab=0,
                    MagsaysayPrivateRoad=0,
                    MagsaysayLower=0,
                    MagsaysayUpper=0,
                    MalcolmSquarePerfectoJoseAbadSantos=0,
                    ManuelARoxas=0,
                    MarketSubdivisionUpper=0,
                    MiddleQuezonHillSubdivsion=0,
                    MilitaryCutoff=0,
                    MinesViewPark=0,
                    ModernSiteEast=0,
                    ModernSiteWest=0,
                    MRRQueenofPeace=0,
                    NewLucban=0,
                    OutlookDrive=0,
                    Pacdal=0,
                    PadreBurgos=0,
                    PadreZamora=0,
                    PalmaUrbanoCarinoPalma=0,
                    PhilAm=0,
                    Pinget=0,
                    PinsaoPilotProject=0,
                    PinsaoProper=0,
                    Poliwes=0,
                    Pucsusan=0,
                    QuezonHillProper=0,
                    QuezonHillUpper=0,
                    QuirinoMagsaysayUpperUpperQM=0,
                    QuirinoHillEast=0,
                    QuirinoHillLower=0,
                    QuirinoHillMiddle=0,
                    QuirinoHillWest=0,
                    RizalMonumentArea=0,
                    RockQuarryLower=0,
                    RockQuarryMiddle=0,
                    RockQuarryUpper=0,
                    SaludMitra=0,
                    SanAntoniVillage=0,
                    SanLuisVillage=0,
                    SanRoqueVillage=0,
                    SanVicente=0,
                    SanitaryCampNorth=0,
                    SanitaryCampSouth=0,
                    SantaEscolastica=0,
                    SantoRosario=0,
                    SantoTomasProper=0,
                    SantoTomasSchoolArea=0,
                    ScoutBarrio=0,
                    SessionRoadArea=0,
                    SlaughterHouseAreaSantoNiñoSlaughter=0,
                    SLUSVPHousingVillage=0,
                    SouthDrive=0,
                    TeodoraAlonzo=0,
                    Trancoville=0,
                    VictoriaVillage=0,
                    SaintJosephVillage=0
)

for (n in 2:52) {
  adf12[nrow(adf12)+1,]=c(n,
                          count(abd12[ABCR&SC=="Positive"&MW==n,])+
                            count(abd12[ABCR&SC=="POSITIVE"&MW==n,]),
                          count(abd12[B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")&SC=="Positive"&MW==n,])+
                            count(abd12[B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")&SC=="POSITIVE"&MW==n,]),
                          count(abd12[B==toupper("Alfonso Tabora")&SC=="Positive"&MW==n,])+
                            count(abd12[B==toupper("Alfonso Tabora")&SC=="POSITIVE"&MW==n,]),
                          count(abd12[B=="AMBIONG"&SC=="Positive"&MW==n,])+
                            count(abd12[B=="AMBIONG"&SC=="POSITIVE"&MW==n,]),
                          count(abd12[AB&SC=="Positive"&MW==n,])+
                            count(abd12[AB&SC=="POSITIVE"&MW==n,]),
                          count(abd12[AL&SC=="Positive"&MW==n,])+
                            count(abd12[AL&SC=="POSITIVE"&MW==n,]),
                          count(abd12[Asin&SC=="Positive"&MW==n,])+
                            count(abd12[Asin&SC=="POSITIVE"&MW==n,]),
                          count(abd12[ATOK&SC=="Positive"&MW==n,])+
                            count(abd12[ATOK&SC=="POSITIVE"&MW==n,]),
                          count(abd12[auroraP&SC=="Positive"&MW==n,])+
                            count(abd12[auroraP&SC=="POSITIVE"&MW==n,]),
                          count(abd12[auroraN&SC=="Positive"&MW==n,])+
                            count(abd12[auroraN&SC=="POSITIVE"&MW==n,]),
                          count(abd12[auroraS&SC=="POSITIVE"&MW==n,]),
                          count(abd12[BL&SC=="Positive"&MW==n,])+
                            count(abd12[BL&SC=="POSITIVE"&MW==n,]),
                          count(abd12[BakakengCentral&SC=="Positive"&MW==n,])+
                            count(abd12[BakakengCentral&SC=="POSITIVE"&MW==n,]),
                          count(abd12[BakakengNorth&SC=="Positive"&MW==n,])+
                            count(abd12[BakakengNorth&SC=="POSITIVE"&MW==n,]),
                          count(abd12[Bal&SC=="Positive"&MW==n,])+
                            count(abd12[Bal&SC=="POSITIVE"&MW==n,]),
                          count(abd12[Balsigan&SC=="Positive"&MW==n,])+
                            count(abd12[Balsigan&SC=="POSITIVE"&MW==n,]),
                          count(abd12[BPE&SC=="Positive"&MW==n,])+
                            count(abd12[BPE&SC=="POSITIVE"&MW==n,]),
                          count(abd12[BPV&SC=="Positive"&MW==n,])+
                            count(abd12[BPV&SC=="POSITIVE"&MW==n,]),
                          count(abd12[Bayan&SC=="Positive"&MW==n,])+
                            count(abd12[Bayan&SC=="POSITIVE"&MW==n,]),
                          count(abd12[BGH&SC=="Positive"&MW==n,])+
                            count(abd12[BGH&SC=="POSITIVE"&MW==n,]),
                          count(abd12[Brookside&SC=="Positive"&MW==n,])+
                            count(abd12[Brookside&SC=="POSITIVE"&MW==n,]),
                          count(abd12[Brookspoint&SC=="Positive"&MW==n,])+
                            count(abd12[Brookspoint&SC=="POSITIVE"&MW==n,]),
                          count(abd12[CabinetHill&SC=="Positive"&MW==n,])+
                            count(abd12[CabinetHill&SC=="POSITIVE"&MW==n,]),
                          count(abd12[Camdas&SC=="Positive"&MW==n,])+
                            count(abd12[Camdas&SC=="POSITIVE"&MW==n,]),
                          count(abd12[c7&SC=="Positive"&MW==n,])+
                            count(abd12[c7&SC=="POSITIVE"&MW==n,]),
                          count(abd12[c8&SC=="Positive"&MW==n,])+
                            count(abd12[c8&SC=="POSITIVE"&MW==n,]),
                          count(abd12[ca&SC=="Positive"&MW==n,])+
                            count(abd12[ca&SC=="POSITIVE"&MW==n,]),
                          count(abd12[cf&SC=="Positive"&MW==n,])+
                            count(abd12[cf&SC=="POSITIVE"&MW==n,]),
                          count(abd12[CityCC&SC=="Positive"&MW==n,])+
                            count(abd12[CityCC&SC=="POSITIVE"&MW==n,]),
                          count(abd12[CityCP&SC=="Positive"&MW==n,])+
                            count(abd12[CityCP&SC=="POSITIVE"&MW==n,]),
                          count(abd12[ccv&SC=="Positive"&MW==n,])+
                            count(abd12[ccv&SC=="POSITIVE"&MW==n,]),
                          count(abd12[Cres&SC=="Positive"&MW==n,])+
                            count(abd12[Cres&SC=="POSITIVE"&MW==n,]),
                          count(abd12[DagisanL&SC=="Positive"&MW==n,])+
                            count(abd12[DagisanL&SC=="POSITIVE"&MW==n,]),
                          count(abd12[DagisanU&SC=="Positive"&MW==n,])+
                            count(abd12[DagisanU&SC=="POSITIVE"&MW==n,]),
                          count(abd12[Dizon&SC=="Positive"&MW==n,])+
                            count(abd12[Dizon&SC=="POSITIVE"&MW==n,]),
                          count(abd12[Domin&SC=="Positive"&MW==n,])+
                            count(abd12[Domin&SC=="POSITIVE"&MW==n,]),
                          count(abd12[Dontogan&SC=="Positive"&MW==n,])+
                            count(abd12[Dontogan&SC=="POSITIVE"&MW==n,]),
                          count(abd12[DPS&SC=="Positive"&MW==n,])+
                            count(abd12[DPS&SC=="POSITIVE"&MW==n,]),
                          count(abd12[enghill&SC=="Positive"&MW==n,])+
                            count(abd12[enghill&SC=="POSITIVE"&MW==n,]),
                          count(abd12[fvill&SC=="Positive"&MW==n,])+
                            count(abd12[fvill&SC=="POSITIVE"&MW==n,]),
                          count(abd12[Ferdinand&SC=="Positive"&MW==n,])+
                            count(abd12[Ferdinand&SC=="POSITIVE"&MW==n,]),
                          count(abd12[B=="FORT DEL PILAR"&SC=="Positive"&MW==n,])+
                            count(abd12[B=="FORT DEL PILAR"&SC=="POSITIVE"&MW==n,]),
                          count(abd12[GabSi&SC=="Positive"&MW==n,])+
                            count(abd12[GabSi&SC=="POSITIVE"&MW==n,]),
                          count(abd12[GenE&SC=="Positive"&MW==n,])+
                            count(abd12[GenE&SC=="POSITIVE"&MW==n,]),
                          count(abd12[GenLL&SC=="Positive"&MW==n,])+
                            count(abd12[GenLL&SC=="POSITIVE"&MW==n,]),
                          count(abd12[GenLU&SC=="Positive"&MW==n,])+
                            count(abd12[GenLU&SC=="POSITIVE"&MW==n,]),
                          count(abd12[Gib&SC=="Positive"&MW==n,])+
                            count(abd12[Gib&SC=="POSITIVE"&MW==n,]),
                          count(abd12[Greenwater&SC=="Positive"&MW==n,])+
                            count(abd12[Greenwater&SC=="POSITIVE"&MW==n,]),
                          count(abd12[GuisadC&SC=="Positive"&MW==n,])+
                            count(abd12[GuisadC&SC=="POSITIVE"&MW==n,]),
                          count(abd12[GuisadS&SC=="Positive"&MW==n,])+
                            count(abd12[GuisadS&SC=="POSITIVE"&MW==n,]),
                          count(abd12[HHol&SC=="Positive"&MW==n,])+
                            count(abd12[HHol&SC=="POSITIVE"&MW==n,]),
                          count(abd12[HHom&SC=="Positive"&MW==n,])+
                            count(abd12[HHom&SC=="POSITIVE"&MW==n,]),
                          count(abd12[HCC&SC=="Positive"&MW==n,])+
                            count(abd12[HCC&SC=="POSITIVE"&MW==n,]),
                          count(abd12[Hillside&SC=="Positive"&MW==n,])+
                            count(abd12[Hillside&SC=="POSITIVE"&MW==n,]),
                          count(abd12[HGhostE&SC=="Positive"&MW==n,])+
                            count(abd12[HGhostE&SC=="POSITIVE"&MW==n,]),
                          count(abd12[HGhostP&SC=="Positive"&MW==n,])+
                            count(abd12[HGhostP&SC=="POSITIVE"&MW==n,]),
                          count(abd12[Honeymoon&SC=="Positive"&MW==n,])+
                            count(abd12[Honeymoon&SC=="POSITIVE"&MW==n,]),
                          count(abd12[IMarcos&SC=="Positive"&MW==n,])+
                            count(abd12[IMarcos&SC=="POSITIVE"&MW==n,]),
                          count(abd12[Imelda&SC=="Positive"&MW==n,])+
                            count(abd12[Imelda&SC=="POSITIVE"&MW==n,]),
                          count(abd12[B=="IRISAN"&SC=="Positive"&MW==n,])+
                            count(abd12[B=="IRISAN"&SC=="POSITIVE"&MW==n,]),
                          count(abd12[Kabayanihan&SC=="Positive"&MW==n,])+
                            count(abd12[Kabayanihan&SC=="POSITIVE"&MW==n,]),
                          count(abd12[Kagitingan&SC=="Positive"&MW==n,])+
                            count(abd12[Kagitingan&SC=="POSITIVE"&MW==n,]),
                          count(abd12[KayangHill&SC=="Positive"&MW==n,])+
                            count(abd12[KayangHill&SC=="POSITIVE"&MW==n,]),
                          count(abd12[KayangE&SC=="Positive"&MW==n,])+
                            count(abd12[KayangE&SC=="POSITIVE"&MW==n,]),
                          count(abd12[kias&SC=="Positive"&MW==n,])+
                            count(abd12[kias&SC=="POSITIVE"&MW==n,]),
                          count(abd12[LBK&SC=="Positive"&MW==n,])+
                            count(abd12[LBK&SC=="POSITIVE"&MW==n,]),
                          count(abd12[LLoak&SC=="Positive"&MW==n,])+
                            count(abd12[LLoak&SC=="POSITIVE"&MW==n,]),
                          count(abd12[LoakP&SC=="Positive"&MW==n,])+
                            count(abd12[LoakP&SC=="POSITIVE"&MW==n,]),
                          count(abd12[LOPJ&SC=="Positive"&MW==n,])+
                            count(abd12[LOPJ&SC=="POSITIVE"&MW==n,]),
                          count(abd12[LourdesE&SC=="Positive"&MW==n,])+
                            count(abd12[LourdesE&SC=="POSITIVE"&MW==n,]),
                          count(abd12[LourdesL&SC=="Positive"&MW==n,])+
                            count(abd12[LourdesL&SC=="POSITIVE"&MW==n,]),
                          count(abd12[LourdesP&SC=="Positive"&MW==n,])+
                            count(abd12[LourdesP&SC=="POSITIVE"&MW==n,]),
                          count(abd12[Lualhati&SC=="Positive"&MW==n,])+
                            count(abd12[Lualhati&SC=="POSITIVE"&MW==n,]),
                          count(abd12[lucnab&SC=="Positive"&MW==n,])+
                            count(abd12[lucnab&SC=="POSITIVE"&MW==n,]),
                          count(abd12[MagsaysayPR&SC=="Positive"&MW==n,])+
                            count(abd12[MagsaysayPR&SC=="POSITIVE"&MW==n,]),
                          count(abd12[MagsaysayL&SC=="Positive"&MW==n,])+
                            count(abd12[MagsaysayL&SC=="POSITIVE"&MW==n,]),
                          count(abd12[MagsaysayU&SC=="Positive"&MW==n,])+
                            count(abd12[MagsaysayU&SC=="POSITIVE"&MW==n,]),
                          count(abd12[MalcolmSquarePerfectoJoseAbadSantos&SC=="Positive"&MW==n,])+
                            count(abd12[MalcolmSquarePerfectoJoseAbadSantos&SC=="POSITIVE"&MW==n,]),
                          count(abd12[MR&SC=="Positive"&MW==n,])+
                            count(abd12[MR&SC=="POSITIVE"&MW==n,]),
                          count(abd12[MarketSubdivisionUpper&SC=="Positive"&MW==n,])+
                            count(abd12[MarketSubdivisionUpper&SC=="POSITIVE"&MW==n,]),
                          count(abd12[MiddleQuezonHillSubdivsion&SC=="Positive"&MW==n,])+
                            count(abd12[MiddleQuezonHillSubdivsion&SC=="POSITIVE"&MW==n,]),
                          count(abd12[MCutOff&SC=="Positive"&MW==n,])+
                            count(abd12[MCutOff&SC=="POSITIVE"&MW==n,]),
                          count(abd12[MinesView&SC=="Positive"&MW==n,])+
                            count(abd12[MinesView&SC=="POSITIVE"&MW==n,]),
                          count(abd12[ModernSiteE&SC=="Positive"&MW==n,])+
                            count(abd12[ModernSiteE&SC=="POSITIVE"&MW==n,]),
                          count(abd12[ModernSiteW&SC=="Positive"&MW==n,])+
                            count(abd12[ModernSiteW&SC=="POSITIVE"&MW==n,]),
                          count(abd12[MRR&SC=="Positive"&MW==n,])+
                            count(abd12[MRR&SC=="POSITIVE"&MW==n,]),
                          count(abd12[NewLuc&SC=="Positive"&MW==n,])+
                            count(abd12[NewLuc&SC=="POSITIVE"&MW==n,]),
                          count(abd12[Outlook&SC=="Positive"&MW==n,])+
                            count(abd12[Outlook&SC=="POSITIVE"&MW==n,]),
                          count(abd12[Pacdal&SC=="Positive"&MW==n,])+
                            count(abd12[Pacdal&SC=="POSITIVE"&MW==n,]),
                          count(abd12[PadreB&SC=="Positive"&MW==n,])+
                            count(abd12[PadreB&SC=="POSITIVE"&MW==n,]),
                          count(abd12[PadreZ&SC=="Positive"&MW==n,])+
                            count(abd12[PadreZ&SC=="POSITIVE"&MW==n,]),
                          count(abd12[PU&SC=="Positive"&MW==n,])+
                            count(abd12[PU&SC=="POSITIVE"&MW==n,]),
                          count(abd12[PhilAm&SC=="Positive"&MW==n,])+
                            count(abd12[PhilAm&SC=="POSITIVE"&MW==n,]),
                          count(abd12[Pinget&SC=="Positive"&MW==n,])+
                            count(abd12[Pinget&SC=="POSITIVE"&MW==n,]),
                          count(abd12[PPP&SC=="Positive"&MW==n,])+
                            count(abd12[PPP&SC=="POSITIVE"&MW==n,]),
                          count(abd12[PisaoP&SC=="Positive"&MW==n,])+
                            count(abd12[PisaoP&SC=="POSITIVE"&MW==n,]),
                          count(abd12[Poliwes&SC=="Positive"&MW==n,])+
                            count(abd12[Poliwes&SC=="POSITIVE"&MW==n,]),
                          count(abd12[pucsusan&SC=="Positive"&MW==n,])+
                            count(abd12[pucsusan&SC=="POSITIVE"&MW==n,]),
                          count(abd12[QHP&SC=="Positive"&MW==n,])+
                            count(abd12[QHP&SC=="POSITIVE"&MW==n,]),
                          count(abd12[QHillU&SC=="Positive"&MW==n,])+
                            count(abd12[QHillU&SC=="POSITIVE"&MW==n,]),
                          count(abd12[UpperQM&SC=="Positive"&MW==n,])+
                            count(abd12[UpperQM&SC=="POSITIVE"&MW==n,]),
                          count(abd12[QHillE&SC=="Positive"&MW==n,])+
                            count(abd12[QHillE&SC=="POSITIVE"&MW==n,]),
                          count(abd12[QHL&SC=="Positive"&MW==n,])+
                            count(abd12[QHL&SC=="POSITIVE"&MW==n,]),
                          count(abd12[QHM&SC=="Positive"&MW==n,])+
                            count(abd12[QHM&SC=="POSITIVE"&MW==n,]),
                          count(abd12[QHillW&SC=="Positive"&MW==n,])+
                            count(abd12[QHillW&SC=="POSITIVE"&MW==n,]),
                          count(abd12[RizalMonumentArea&SC=="Positive"&MW==n,])+
                            count(abd12[RizalMonumentArea&SC=="POSITIVE"&MW==n,]),
                          count(abd12[RockQuarryLower&SC=="Positive"&MW==n,])+
                            count(abd12[RockQuarryLower&SC=="POSITIVE"&MW==n,]),
                          count(abd12[RockQuarryMiddle&SC=="Positive"&MW==n,])+
                            count(abd12[RockQuarryMiddle&SC=="POSITIVE"&MW==n,]),
                          count(abd12[RockQuarryUpper&SC=="Positive"&MW==n,])+
                            count(abd12[RockQuarryUpper&SC=="POSITIVE"&MW==n,]),
                          count(abd12[salud&SC=="Positive"&MW==n,])+
                            count(abd12[salud&SC=="POSITIVE"&MW==n,]),
                          count(abd12[SanAntonioVillage&SC=="Positive"&MW==n,])+
                            count(abd12[SanAntonioVillage&SC=="POSITIVE"&MW==n,]),
                          count(abd12[SanLuisVillage&SC=="Positive"&MW==n,])+
                            count(abd12[SanLuisVillage&SC=="POSITIVE"&MW==n,]),
                          count(abd12[SanRoqueVillage&SC=="Positive"&MW==n,])+
                            count(abd12[SanRoqueVillage&SC=="POSITIVE"&MW==n,]),
                          count(abd12[sanvic&SC=="Positive"&MW==n,])+
                            count(abd12[sanvic&SC=="POSITIVE"&MW==n,]),
                          count(abd12[SanCamp&SC=="Positive"&MW==n,])+
                            count(abd12[SanCamp&SC=="POSITIVE"&MW==n,]),
                          count(abd12[SanitaryCampSouth&SC=="Positive"&MW==n,])+
                            count(abd12[SanitaryCampSouth&SC=="POSITIVE"&MW==n,]),
                          count(abd12[STE&SC=="Positive"&MW==n,])+
                            count(abd12[STE&SC=="POSITIVE"&MW==n,]),
                          count(abd12[SantoR&SC=="Positive"&MW==n,])+
                            count(abd12[SantoR&SC=="POSITIVE"&MW==n,]),
                          count(abd12[STP&SC=="Positive"&MW==n,])+
                            count(abd12[STP&SC=="POSITIVE"&MW==n,]),
                          count(abd12[STsa&SC=="Positive"&MW==n,])+
                            count(abd12[STsa&SC=="POSITIVE"&MW==n,]),
                          count(abd12[ScoutB&SC=="Positive"&MW==n,])+
                            count(abd12[ScoutB&SC=="POSITIVE"&MW==n,]),
                          count(abd12[Session&SC=="Positive"&MW==n,])+
                            count(abd12[Session&SC=="POSITIVE"&MW==n,]),
                          count(abd12[Slaughter&SC=="Positive"&MW==n,])+
                            count(abd12[Slaughter&SC=="POSITIVE"&MW==n,]),
                          count(abd12[SLU&SC=="Positive"&MW==n,])+
                            count(abd12[SLU&SC=="POSITIVE"&MW==n,]),
                          count(abd12[SouthDrive&SC=="Positive"&MW==n,])+
                            count(abd12[SouthDrive&SC=="POSITIVE"&MW==n,]),
                          count(abd12[TeoAl&SC=="Positive"&MW==n,])+
                            count(abd12[TeoAl&SC=="POSITIVE"&MW==n,]),
                          count(abd12[tranco&SC=="Positive"&MW==n,])+
                            count(abd12[tranco&SC=="POSITIVE"&MW==n,]),
                          count(abd12[victoria&SC=="Positive"&MW==n,])+
                            count(abd12[victoria&SC=="POSITIVE"&MW==n,]),
                          count(abd12[STJos&SC=="Positive"&MW==n,])+
                            count(abd12[STJos&SC=="POSITIVE"&MW==n,])
  )
}

B=abd13$Barangay
SC=abd13$StoolCulture
MW=abd13$MorbidityWeek

Imelda = B=="IMELDA VILLAGE"
BakakengCentral = B=="BAKAKENG CENTRAL"
ABCR = B=="A. BONIFACIO-CAGUIOA-RIMANDO (ABCR)"
Ambiong = B=="AMBIONG"
BakakengNorth = B=="BAKAKENG NORTH"
Asin = B=="ASIN"
Balsigan = B=="BALSIGAN"
Bayan = B=="BAYAN PARK WEST (BAYAN PARK)"
BGH=B=="BGH COMPOUND"
Brookside = B=="BROOKSIDE"
Camdas = B=="CAMDAS SUBDIVISION"
c7=B=="CAMP 7"
c8=B=="CAMP 8"
ca=B=="CAMP ALLEN"
cf=B=="CAMPO FILIPINO"
ccv=B=="COUNTRY CLUB VILLAGE"
DPS=B=="DPS AREA"
enghill=B=="ENGINEER'S HILL"
fvill=B=="FAIRVIEW VILLAGE"
GLuna=B=="GENERAL LUNA, UPPER"
Gib=B=="GIBRALTAR"
GuisadC=B=="GUISAD CENTRAL"
GuisadS=B=="GUISAD SORONG"
HHL=B=="HAPPY HOMES (HAPPY HOMES-LUCBAN)"
kias=B=="KIAS"
LLoak=B=="LIWANAG-LOAKAN"
LoakP=B=="LOAKAN PROPER"
LOPJ=B=="LOPEZ JAENA"
lucnab=B=="LUCNAB"
MR=B=="MANUEL A. ROXAS"
MCutOff=B=="MILITARY CUT-OFF"
MRR=B=="MRR-QUEEN OF PEACE"
PadreZ=B=="PADRE ZAMORA"
Pinget=B=="PINGET"
PPP=B=="PINSAO PILOT PROJECT"
PisaoP=B=="PINSAO PROPER"
Poliwes=B=="POLIWES"
QHP=B=="QUEZON HILL PROPER"
QHL=B=="QUIRINO HILL, LOWER"
QHM=B=="QUIRINO HILL, MIDDLE"
salud=B=="SALUD MITRA"
sanvic=B=="SAN VICENTE"
SanCamp=B=="SANITARY CAMP, NORTH"
STE=B=="SANTA ESCOLASTICA"
STP=B=="SANTO TOMAS PROPER"&B=="STO. TOMAS PROPER"
STsa=B=="SANTO TOMAS SCHOOL AREA"
tranc=B=="TRANCOVILLE"
birac=B=="BIRAC"
AL=B=="APUGAN-LOAKAN"
AZKCO=B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")
ATAB=B==toupper("Alfonso Tabora")
AB=B==toupper("Andres Bonifacio (Lower Bokawkan)")
ATOK=B==toupper("Atok Trail")
auroraP=B==toupper("Aurora Hill Proper (Malvar-Sgt. Floresca)")
auroraN=B==toupper("Aurora Hill, North Central")
auroraS=B==toupper("Aurora Hill, South Central")
BL=B==toupper("Bagong Lipunan (Market Area)")
Bal=B==toupper("Bal-Marcoville (Marcoville)")
BPE=B==toupper("Bayan Park East")
BPV=B==toupper("Bayan Park Village")
Brookspoint=B==toupper("Brookspoint")
CabinetHill=B==toupper("Cabinet Hill-Teacher's Camp")
CityCC=B==toupper("City Camp Central")
CityCP=B==toupper("City Camp Proper")
DagisanL=B==toupper("Dagsian, Lower")
Cres=B==toupper("Cresencia Village")
DagisanU=B==toupper("Dagsian, Upper")
Dizon=B==toupper("Dizon Subdivision")
Domin=B==toupper("Dominican Hill-Mirador")
Dontogan=B=="DONTOGAN"
DPS=B=="DPS AREA"
Ferdinand=B==toupper("Ferdinand (Happy Homes-Campo Sioco)")
GabSi=B=="GABRIELLA SILANG"
GenE=B==toupper("General Emilio F. Aguinaldo (Quirino-Magsaysay, Lower)")
GenLL=B==toupper("General Luna, Lower")
GenLU=B==toupper("General Luna, Upper")
Greenwater=B==toupper("Greenwater Village")
HHol=B==toupper("Happy Hollow")
HHom=B==toupper("Happy Homes (Happy Homes-Lucban)")
HCC=B==toupper("Harrison-Claudio Carantes")
Hillside=B==toupper("Hillside")
HGhostE=B==toupper("Holy Ghost Extension")
HGhostP=B==toupper("Holy Ghost Proper")
Honeymoon=B==toupper("Honeymoon (Honeymoon-Holy Ghost)")
IMarcos=B==toupper("Imelda R. Marcos (La Salle)")
Imelda=B==toupper("Imelda Village")
Kabayanihan=B==toupper("Kabayanihan")
Kagitingan=B==toupper("Kagitingan")
KayangHill=B==toupper("Kayang-Hilltop")
KayangE=B==toupper("Kayang Extension")
LBK=B==toupper("Legarda-Burnham-Kisad")
LourdesE=B==toupper("Lourdes Subdivision Extension")
LourdesL=B==toupper("Lourdes Subdivision, Lower")
LourdesP=B==toupper("Lourdes Subdivision, Proper")
Lualhati=B==toupper("Lualhati")
MagsaysayPR=B==toupper("Magsaysay Private Road")
MagsaysayL=B==toupper("Magsaysay, Lower")
MagsaysayU=B==toupper("Magsaysay, Upper")
MalcolmSquarePerfectoJoseAbadSantos=B==toupper("Malcolm Square-Perfecto (Jose Abad Santos)")
MarketSubdivisionUpper=B==toupper("Market Subdivision, Upper")
MiddleQuezonHillSubdivsion=B==toupper("Middle Quezon Hill Subdivsiion(Quezon Hill Middle)")
MinesView=B==toupper("Mines View Park")
ModernSiteE=B==toupper("Modern Site, East")
ModernSiteW=B==toupper("Modern Site, West")
NewLuc=B==toupper("New Lucban")
Outlook=B==toupper("Outlook Drive")
Pacdal=B==toupper("Pacdal")
PadreB=B==toupper("Padre Burgos")
PadreZ=B==toupper("Padre Zamora")
PU=B==toupper("Palma-Urbano (Cariño-Palma)")
PhilAm=B==toupper("Phil-Am")
pucsusan=B==toupper("Pucsusan")
QHillU=B==toupper("Quezon Hill, Upper")
UpperQM=B==toupper("Quirino-Magsaysay, Upper (Upper QM)")
QHillE=B==toupper("Quirino Hill, East")
QHillW=B==toupper("Quirino Hill, West")
RizalMonumentArea=B==toupper("Rizal Monument Area")
RockQuarryLower=B==toupper("Rock Quarry, Lower")
RockQuarryMiddle=B==toupper("Rock Quarry, Middle")
RockQuarryUpper=B==toupper("Rock Quarry, Upper")
SanAntonioVillage=B==toupper("San Antonio Village")
SanLuisVillage=B==toupper("San Luis Village")
SanRoqueVillage=B==toupper("San Roque Village")
SanitaryCampSouth=B==toupper("Sanitary Camp, South")
SantoR=B==toupper("Santo Rosario")
ScoutB=B==toupper("Scout Barrio")
Session=B==toupper("Session Road Area")
Slaughter=B==toupper("Slaughter House Area (Santo Niño Slaughter)")
SLU=B==toupper("SLU-SVP Housing Village")
SouthDrive=B==toupper("South Drive")
TeoAl=B==toupper("Teodora Alonzo")
tranco=B==toupper("Trancoville")
victoria=B==toupper("Victoria Village")
STJos=B=="ST JOSEPH VILLAGE"

adf13 <- data.frame(MorbidityWeek=1,
                  ABonifacioCaguioaRimandoABCR=0,
                  AbanaoZanduetaKayongChugumOtekAZKCO=0,
                  AlfonsoTabora=0,
                  Ambiong=0,
                  AndresBonifacioLowerBokawkan=0,
                  ApuganLoakan=0,
                  AsinRoad=0,
                  AtokTrail=0,
                  AuroraHillProperMalvarSgtFloresca=0,
                  AuroraHillNorthCentral=0,
                  AuroraHillSouthCentral=0,
                  BagongLipunanMarketArea=0,
                  BakakengCentral=0,
                  BakakengNorth=1,
                  BalMarcovilleMarcoville=0,
                  Balsigan=0,
                  BayanParkEast=0,
                  BayanParkVillage=0,
                  BayanParkWestBayanPark=0,
                  BGHCompound=0,
                  Brookside=0,
                  Brookspoint=0,
                  CabinetHillTeachersCamp=0,
                  CamdasSubdivision=0,
                  Camp7=0,
                  Camp8=0,
                  CampAllen=0,
                  CampoFilipino=0,
                  CityCampCentral=0,
                  CityCampProper=0,
                  CountryClubVillage=0,
                  CresenciaVillage=0,
                  DagisanLower=0,
                  DagisanUpper=0,
                  DizonSubdivision=0,
                  DominicanHillMirador=0,
                  Dontogan=0,
                  DPSArea=0,
                  EngineersHill=0,
                  FairviewVillage=0,
                  FerdinandHappyHomesCampoSioco=0,
                  FortdelPilar=0,
                  GabrielaSilang=0,
                  GeneralEmilioFAguinaldoQuirinoMagsaysayLower=0,
                  GeneralLunaLower=0,
                  GeneralLunaUpper=0,
                  Gibraltar=0,
                  GreenwaterVillage=0,
                  GuisadCentral=0,
                  GuisadSorong=0,
                  HappyHollow=0,
                  HappyHomesHappyHomesLucban=0,
                  HarrisonClaudioCarantes=0,
                  Hillside=0,
                  HolyGhostExtension=0,
                  HolyGhostProper=0,
                  HoneymoonHoneymoonHolyGhost=0,
                  ImeldaRMarcosLaSalle=0,
                  ImeldaVillage=0,
                  Irisan=0,
                  Kabayanihan=0,
                  Kagitingan=0,
                  KayangHilltop=0,
                  KayangExtension=0,
                  Kias=0,
                  LegardaBurnhamKisad=0,
                  LiwanagLoakan=0,
                  LoakanProper=0,
                  LopezJaena=0,
                  LourdesSubdivisionExtension=0,
                  LourdesSubdivisionLower=0,
                  LourdesSubdivisionProper=0,
                  Lualhati=0,
                  Lucnab=0,
                  MagsaysayPrivateRoad=0,
                  MagsaysayLower=0,
                  MagsaysayUpper=0,
                  MalcolmSquarePerfectoJoseAbadSantos=0,
                  ManuelARoxas=0,
                  MarketSubdivisionUpper=0,
                  MiddleQuezonHillSubdivsion=0,
                  MilitaryCutoff=0,
                  MinesViewPark=0,
                  ModernSiteEast=0,
                  ModernSiteWest=0,
                  MRRQueenofPeace=0,
                  NewLucban=0,
                  OutlookDrive=0,
                  Pacdal=0,
                  PadreBurgos=0,
                  PadreZamora=0,
                  PalmaUrbanoCarinoPalma=0,
                  PhilAm=0,
                  Pinget=0,
                  PinsaoPilotProject=0,
                  PinsaoProper=0,
                  Poliwes=0,
                  Pucsusan=0,
                  QuezonHillProper=0,
                  QuezonHillUpper=0,
                  QuirinoMagsaysayUpperUpperQM=0,
                  QuirinoHillEast=0,
                  QuirinoHillLower=0,
                  QuirinoHillMiddle=0,
                  QuirinoHillWest=0,
                  RizalMonumentArea=0,
                  RockQuarryLower=0,
                  RockQuarryMiddle=0,
                  RockQuarryUpper=0,
                  SaludMitra=0,
                  SanAntoniVillage=0,
                  SanLuisVillage=0,
                  SanRoqueVillage=0,
                  SanVicente=0,
                  SanitaryCampNorth=0,
                  SanitaryCampSouth=0,
                  SantaEscolastica=0,
                  SantoRosario=0,
                  SantoTomasProper=0,
                  SantoTomasSchoolArea=0,
                  ScoutBarrio=0,
                  SessionRoadArea=0,
                  SlaughterHouseAreaSantoNiñoSlaughter=0,
                  SLUSVPHousingVillage=0,
                  SouthDrive=0,
                  TeodoraAlonzo=0,
                  Trancoville=0,
                  VictoriaVillage=0,
                  SaintJosephVillage=0
)

for (n in 2:52) {
  adf13[nrow(adf13)+1,]=c(n,
                         count(abd13[ABCR&SC=="Positive"&MW==n,])+
                           count(abd13[ABCR&SC=="POSITIVE"&MW==n,]),
                      count(abd13[B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")&SC=="Positive"&MW==n,])+
                        count(abd13[B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")&SC=="POSITIVE"&MW==n,]),
                      count(abd13[B==toupper("Alfonso Tabora")&SC=="Positive"&MW==n,])+
                        count(abd13[B==toupper("Alfonso Tabora")&SC=="POSITIVE"&MW==n,]),
                      count(abd13[B=="AMBIONG"&SC=="Positive"&MW==n,])+
                        count(abd13[B=="AMBIONG"&SC=="POSITIVE"&MW==n,]),
                      count(abd13[AB&SC=="Positive"&MW==n,])+
                        count(abd13[AB&SC=="POSITIVE"&MW==n,]),
                      count(abd13[AL&SC=="Positive"&MW==n,])+
                        count(abd13[AL&SC=="POSITIVE"&MW==n,]),
                        count(abd13[Asin&SC=="Positive"&MW==n,])+
                        count(abd13[Asin&SC=="POSITIVE"&MW==n,]),
                      count(abd13[ATOK&SC=="Positive"&MW==n,])+
                        count(abd13[ATOK&SC=="POSITIVE"&MW==n,]),
                      count(abd13[auroraP&SC=="Positive"&MW==n,])+
                        count(abd13[auroraP&SC=="POSITIVE"&MW==n,]),
                      count(abd13[auroraN&SC=="Positive"&MW==n,])+
                        count(abd13[auroraN&SC=="POSITIVE"&MW==n,]),
                        count(abd13[auroraS&SC=="POSITIVE"&MW==n,]),
                      count(abd13[BL&SC=="Positive"&MW==n,])+
                        count(abd13[BL&SC=="POSITIVE"&MW==n,]),
                      count(abd13[BakakengCentral&SC=="Positive"&MW==n,])+
                        count(abd13[BakakengCentral&SC=="POSITIVE"&MW==n,]),
                      count(abd13[BakakengNorth&SC=="Positive"&MW==n,])+
                        count(abd13[BakakengNorth&SC=="POSITIVE"&MW==n,]),
                      count(abd13[Bal&SC=="Positive"&MW==n,])+
                        count(abd13[Bal&SC=="POSITIVE"&MW==n,]),
                      count(abd13[Balsigan&SC=="Positive"&MW==n,])+
                        count(abd13[Balsigan&SC=="POSITIVE"&MW==n,]),
                      count(abd13[BPE&SC=="Positive"&MW==n,])+
                        count(abd13[BPE&SC=="POSITIVE"&MW==n,]),
                      count(abd13[BPV&SC=="Positive"&MW==n,])+
                        count(abd13[BPV&SC=="POSITIVE"&MW==n,]),
                      count(abd13[Bayan&SC=="Positive"&MW==n,])+
                        count(abd13[Bayan&SC=="POSITIVE"&MW==n,]),
                      count(abd13[BGH&SC=="Positive"&MW==n,])+
                        count(abd13[BGH&SC=="POSITIVE"&MW==n,]),
                      count(abd13[Brookside&SC=="Positive"&MW==n,])+
                        count(abd13[Brookside&SC=="POSITIVE"&MW==n,]),
                      count(abd13[Brookspoint&SC=="Positive"&MW==n,])+
                        count(abd13[Brookspoint&SC=="POSITIVE"&MW==n,]),
                      count(abd13[CabinetHill&SC=="Positive"&MW==n,])+
                        count(abd13[CabinetHill&SC=="POSITIVE"&MW==n,]),
                      count(abd13[Camdas&SC=="Positive"&MW==n,])+
                        count(abd13[Camdas&SC=="POSITIVE"&MW==n,]),
                      count(abd13[c7&SC=="Positive"&MW==n,])+
                        count(abd13[c7&SC=="POSITIVE"&MW==n,]),
                      count(abd13[c8&SC=="Positive"&MW==n,])+
                        count(abd13[c8&SC=="POSITIVE"&MW==n,]),
                      count(abd13[ca&SC=="Positive"&MW==n,])+
                        count(abd13[ca&SC=="POSITIVE"&MW==n,]),
                      count(abd13[cf&SC=="Positive"&MW==n,])+
                        count(abd13[cf&SC=="POSITIVE"&MW==n,]),
                      count(abd13[CityCC&SC=="Positive"&MW==n,])+
                        count(abd13[CityCC&SC=="POSITIVE"&MW==n,]),
                      count(abd13[CityCP&SC=="Positive"&MW==n,])+
                        count(abd13[CityCP&SC=="POSITIVE"&MW==n,]),
                      count(abd13[ccv&SC=="Positive"&MW==n,])+
                        count(abd13[ccv&SC=="POSITIVE"&MW==n,]),
                      count(abd13[Cres&SC=="Positive"&MW==n,])+
                        count(abd13[Cres&SC=="POSITIVE"&MW==n,]),
                      count(abd13[DagisanL&SC=="Positive"&MW==n,])+
                        count(abd13[DagisanL&SC=="POSITIVE"&MW==n,]),
                      count(abd13[DagisanU&SC=="Positive"&MW==n,])+
                        count(abd13[DagisanU&SC=="POSITIVE"&MW==n,]),
                      count(abd13[Dizon&SC=="Positive"&MW==n,])+
                        count(abd13[Dizon&SC=="POSITIVE"&MW==n,]),
                      count(abd13[Domin&SC=="Positive"&MW==n,])+
                        count(abd13[Domin&SC=="POSITIVE"&MW==n,]),
                      count(abd13[Dontogan&SC=="Positive"&MW==n,])+
                        count(abd13[Dontogan&SC=="POSITIVE"&MW==n,]),
                      count(abd13[DPS&SC=="Positive"&MW==n,])+
                        count(abd13[DPS&SC=="POSITIVE"&MW==n,]),
                      count(abd13[enghill&SC=="Positive"&MW==n,])+
                        count(abd13[enghill&SC=="POSITIVE"&MW==n,]),
                      count(abd13[fvill&SC=="Positive"&MW==n,])+
                        count(abd13[fvill&SC=="POSITIVE"&MW==n,]),
                      count(abd13[Ferdinand&SC=="Positive"&MW==n,])+
                        count(abd13[Ferdinand&SC=="POSITIVE"&MW==n,]),
                      count(abd13[B=="FORT DEL PILAR"&SC=="Positive"&MW==n,])+
                        count(abd13[B=="FORT DEL PILAR"&SC=="POSITIVE"&MW==n,]),
                      count(abd13[GabSi&SC=="Positive"&MW==n,])+
                        count(abd13[GabSi&SC=="POSITIVE"&MW==n,]),
                      count(abd13[GenE&SC=="Positive"&MW==n,])+
                        count(abd13[GenE&SC=="POSITIVE"&MW==n,]),
                      count(abd13[GenLL&SC=="Positive"&MW==n,])+
                        count(abd13[GenLL&SC=="POSITIVE"&MW==n,]),
                      count(abd13[GenLU&SC=="Positive"&MW==n,])+
                        count(abd13[GenLU&SC=="POSITIVE"&MW==n,]),
                      count(abd13[Gib&SC=="Positive"&MW==n,])+
                        count(abd13[Gib&SC=="POSITIVE"&MW==n,]),
                      count(abd13[Greenwater&SC=="Positive"&MW==n,])+
                        count(abd13[Greenwater&SC=="POSITIVE"&MW==n,]),
                      count(abd13[GuisadC&SC=="Positive"&MW==n,])+
                        count(abd13[GuisadC&SC=="POSITIVE"&MW==n,]),
                      count(abd13[GuisadS&SC=="Positive"&MW==n,])+
                        count(abd13[GuisadS&SC=="POSITIVE"&MW==n,]),
                      count(abd13[HHol&SC=="Positive"&MW==n,])+
                        count(abd13[HHol&SC=="POSITIVE"&MW==n,]),
                      count(abd13[HHom&SC=="Positive"&MW==n,])+
                        count(abd13[HHom&SC=="POSITIVE"&MW==n,]),
                      count(abd13[HCC&SC=="Positive"&MW==n,])+
                        count(abd13[HCC&SC=="POSITIVE"&MW==n,]),
                      count(abd13[Hillside&SC=="Positive"&MW==n,])+
                        count(abd13[Hillside&SC=="POSITIVE"&MW==n,]),
                      count(abd13[HGhostE&SC=="Positive"&MW==n,])+
                        count(abd13[HGhostE&SC=="POSITIVE"&MW==n,]),
                      count(abd13[HGhostP&SC=="Positive"&MW==n,])+
                        count(abd13[HGhostP&SC=="POSITIVE"&MW==n,]),
                      count(abd13[Honeymoon&SC=="Positive"&MW==n,])+
                        count(abd13[Honeymoon&SC=="POSITIVE"&MW==n,]),
                      count(abd13[IMarcos&SC=="Positive"&MW==n,])+
                        count(abd13[IMarcos&SC=="POSITIVE"&MW==n,]),
                      count(abd13[Imelda&SC=="Positive"&MW==n,])+
                        count(abd13[Imelda&SC=="POSITIVE"&MW==n,]),
                      count(abd13[B=="IRISAN"&SC=="Positive"&MW==n,])+
                        count(abd13[B=="IRISAN"&SC=="POSITIVE"&MW==n,]),
                      count(abd13[Kabayanihan&SC=="Positive"&MW==n,])+
                        count(abd13[Kabayanihan&SC=="POSITIVE"&MW==n,]),
                      count(abd13[Kagitingan&SC=="Positive"&MW==n,])+
                        count(abd13[Kagitingan&SC=="POSITIVE"&MW==n,]),
                      count(abd13[KayangHill&SC=="Positive"&MW==n,])+
                        count(abd13[KayangHill&SC=="POSITIVE"&MW==n,]),
                      count(abd13[KayangE&SC=="Positive"&MW==n,])+
                        count(abd13[KayangE&SC=="POSITIVE"&MW==n,]),
                      count(abd13[kias&SC=="Positive"&MW==n,])+
                        count(abd13[kias&SC=="POSITIVE"&MW==n,]),
                      count(abd13[LBK&SC=="Positive"&MW==n,])+
                        count(abd13[LBK&SC=="POSITIVE"&MW==n,]),
                      count(abd13[LLoak&SC=="Positive"&MW==n,])+
                        count(abd13[LLoak&SC=="POSITIVE"&MW==n,]),
                      count(abd13[LoakP&SC=="Positive"&MW==n,])+
                        count(abd13[LoakP&SC=="POSITIVE"&MW==n,]),
                      count(abd13[LOPJ&SC=="Positive"&MW==n,])+
                        count(abd13[LOPJ&SC=="POSITIVE"&MW==n,]),
                      count(abd13[LourdesE&SC=="Positive"&MW==n,])+
                        count(abd13[LourdesE&SC=="POSITIVE"&MW==n,]),
                      count(abd13[LourdesL&SC=="Positive"&MW==n,])+
                        count(abd13[LourdesL&SC=="POSITIVE"&MW==n,]),
                      count(abd13[LourdesP&SC=="Positive"&MW==n,])+
                        count(abd13[LourdesP&SC=="POSITIVE"&MW==n,]),
                      count(abd13[Lualhati&SC=="Positive"&MW==n,])+
                        count(abd13[Lualhati&SC=="POSITIVE"&MW==n,]),
                      count(abd13[lucnab&SC=="Positive"&MW==n,])+
                        count(abd13[lucnab&SC=="POSITIVE"&MW==n,]),
                      count(abd13[MagsaysayPR&SC=="Positive"&MW==n,])+
                        count(abd13[MagsaysayPR&SC=="POSITIVE"&MW==n,]),
                      count(abd13[MagsaysayL&SC=="Positive"&MW==n,])+
                        count(abd13[MagsaysayL&SC=="POSITIVE"&MW==n,]),
                      count(abd13[MagsaysayU&SC=="Positive"&MW==n,])+
                        count(abd13[MagsaysayU&SC=="POSITIVE"&MW==n,]),
                      count(abd13[MalcolmSquarePerfectoJoseAbadSantos&SC=="Positive"&MW==n,])+
                        count(abd13[MalcolmSquarePerfectoJoseAbadSantos&SC=="POSITIVE"&MW==n,]),
                      count(abd13[MR&SC=="Positive"&MW==n,])+
                        count(abd13[MR&SC=="POSITIVE"&MW==n,]),
                      count(abd13[MarketSubdivisionUpper&SC=="Positive"&MW==n,])+
                        count(abd13[MarketSubdivisionUpper&SC=="POSITIVE"&MW==n,]),
                      count(abd13[MiddleQuezonHillSubdivsion&SC=="Positive"&MW==n,])+
                        count(abd13[MiddleQuezonHillSubdivsion&SC=="POSITIVE"&MW==n,]),
                      count(abd13[MCutOff&SC=="Positive"&MW==n,])+
                        count(abd13[MCutOff&SC=="POSITIVE"&MW==n,]),
                      count(abd13[MinesView&SC=="Positive"&MW==n,])+
                        count(abd13[MinesView&SC=="POSITIVE"&MW==n,]),
                      count(abd13[ModernSiteE&SC=="Positive"&MW==n,])+
                        count(abd13[ModernSiteE&SC=="POSITIVE"&MW==n,]),
                      count(abd13[ModernSiteW&SC=="Positive"&MW==n,])+
                        count(abd13[ModernSiteW&SC=="POSITIVE"&MW==n,]),
                      count(abd13[MRR&SC=="Positive"&MW==n,])+
                        count(abd13[MRR&SC=="POSITIVE"&MW==n,]),
                      count(abd13[NewLuc&SC=="Positive"&MW==n,])+
                        count(abd13[NewLuc&SC=="POSITIVE"&MW==n,]),
                      count(abd13[Outlook&SC=="Positive"&MW==n,])+
                        count(abd13[Outlook&SC=="POSITIVE"&MW==n,]),
                      count(abd13[Pacdal&SC=="Positive"&MW==n,])+
                        count(abd13[Pacdal&SC=="POSITIVE"&MW==n,]),
                      count(abd13[PadreB&SC=="Positive"&MW==n,])+
                        count(abd13[PadreB&SC=="POSITIVE"&MW==n,]),
                      count(abd13[PadreZ&SC=="Positive"&MW==n,])+
                        count(abd13[PadreZ&SC=="POSITIVE"&MW==n,]),
                      count(abd13[PU&SC=="Positive"&MW==n,])+
                        count(abd13[PU&SC=="POSITIVE"&MW==n,]),
                      count(abd13[PhilAm&SC=="Positive"&MW==n,])+
                        count(abd13[PhilAm&SC=="POSITIVE"&MW==n,]),
                      count(abd13[Pinget&SC=="Positive"&MW==n,])+
                        count(abd13[Pinget&SC=="POSITIVE"&MW==n,]),
                      count(abd13[PPP&SC=="Positive"&MW==n,])+
                        count(abd13[PPP&SC=="POSITIVE"&MW==n,]),
                      count(abd13[PisaoP&SC=="Positive"&MW==n,])+
                        count(abd13[PisaoP&SC=="POSITIVE"&MW==n,]),
                      count(abd13[Poliwes&SC=="Positive"&MW==n,])+
                        count(abd13[Poliwes&SC=="POSITIVE"&MW==n,]),
                      count(abd13[pucsusan&SC=="Positive"&MW==n,])+
                        count(abd13[pucsusan&SC=="POSITIVE"&MW==n,]),
                      count(abd13[QHP&SC=="Positive"&MW==n,])+
                        count(abd13[QHP&SC=="POSITIVE"&MW==n,]),
                      count(abd13[QHillU&SC=="Positive"&MW==n,])+
                        count(abd13[QHillU&SC=="POSITIVE"&MW==n,]),
                      count(abd13[UpperQM&SC=="Positive"&MW==n,])+
                        count(abd13[UpperQM&SC=="POSITIVE"&MW==n,]),
                      count(abd13[QHillE&SC=="Positive"&MW==n,])+
                        count(abd13[QHillE&SC=="POSITIVE"&MW==n,]),
                      count(abd13[QHL&SC=="Positive"&MW==n,])+
                        count(abd13[QHL&SC=="POSITIVE"&MW==n,]),
                      count(abd13[QHM&SC=="Positive"&MW==n,])+
                        count(abd13[QHM&SC=="POSITIVE"&MW==n,]),
                      count(abd13[QHillW&SC=="Positive"&MW==n,])+
                        count(abd13[QHillW&SC=="POSITIVE"&MW==n,]),
                      count(abd13[RizalMonumentArea&SC=="Positive"&MW==n,])+
                        count(abd13[RizalMonumentArea&SC=="POSITIVE"&MW==n,]),
                      count(abd13[RockQuarryLower&SC=="Positive"&MW==n,])+
                        count(abd13[RockQuarryLower&SC=="POSITIVE"&MW==n,]),
                      count(abd13[RockQuarryMiddle&SC=="Positive"&MW==n,])+
                        count(abd13[RockQuarryMiddle&SC=="POSITIVE"&MW==n,]),
                      count(abd13[RockQuarryUpper&SC=="Positive"&MW==n,])+
                        count(abd13[RockQuarryUpper&SC=="POSITIVE"&MW==n,]),
                      count(abd13[salud&SC=="Positive"&MW==n,])+
                        count(abd13[salud&SC=="POSITIVE"&MW==n,]),
                      count(abd13[SanAntonioVillage&SC=="Positive"&MW==n,])+
                        count(abd13[SanAntonioVillage&SC=="POSITIVE"&MW==n,]),
                      count(abd13[SanLuisVillage&SC=="Positive"&MW==n,])+
                        count(abd13[SanLuisVillage&SC=="POSITIVE"&MW==n,]),
                      count(abd13[SanRoqueVillage&SC=="Positive"&MW==n,])+
                        count(abd13[SanRoqueVillage&SC=="POSITIVE"&MW==n,]),
                      count(abd13[sanvic&SC=="Positive"&MW==n,])+
                        count(abd13[sanvic&SC=="POSITIVE"&MW==n,]),
                      count(abd13[SanCamp&SC=="Positive"&MW==n,])+
                        count(abd13[SanCamp&SC=="POSITIVE"&MW==n,]),
                      count(abd13[SanitaryCampSouth&SC=="Positive"&MW==n,])+
                        count(abd13[SanitaryCampSouth&SC=="POSITIVE"&MW==n,]),
                      count(abd13[STE&SC=="Positive"&MW==n,])+
                        count(abd13[STE&SC=="POSITIVE"&MW==n,]),
                      count(abd13[SantoR&SC=="Positive"&MW==n,])+
                        count(abd13[SantoR&SC=="POSITIVE"&MW==n,]),
                      count(abd13[STP&SC=="Positive"&MW==n,])+
                        count(abd13[STP&SC=="POSITIVE"&MW==n,]),
                      count(abd13[STsa&SC=="Positive"&MW==n,])+
                        count(abd13[STsa&SC=="POSITIVE"&MW==n,]),
                      count(abd13[ScoutB&SC=="Positive"&MW==n,])+
                        count(abd13[ScoutB&SC=="POSITIVE"&MW==n,]),
                      count(abd13[Session&SC=="Positive"&MW==n,])+
                        count(abd13[Session&SC=="POSITIVE"&MW==n,]),
                      count(abd13[Slaughter&SC=="Positive"&MW==n,])+
                        count(abd13[Slaughter&SC=="POSITIVE"&MW==n,]),
                      count(abd13[SLU&SC=="Positive"&MW==n,])+
                        count(abd13[SLU&SC=="POSITIVE"&MW==n,]),
                      count(abd13[SouthDrive&SC=="Positive"&MW==n,])+
                        count(abd13[SouthDrive&SC=="POSITIVE"&MW==n,]),
                      count(abd13[TeoAl&SC=="Positive"&MW==n,])+
                        count(abd13[TeoAl&SC=="POSITIVE"&MW==n,]),
                      count(abd13[tranco&SC=="Positive"&MW==n,])+
                        count(abd13[tranco&SC=="POSITIVE"&MW==n,]),
                      count(abd13[victoria&SC=="Positive"&MW==n,])+
                        count(abd13[victoria&SC=="POSITIVE"&MW==n,]),
                      count(abd13[STJos&SC=="Positive"&MW==n,])+
                        count(abd13[STJos&SC=="POSITIVE"&MW==n,])
  )
}

B=abd14$Barangay
SC=abd14$StoolCulture
MW=abd14$MorbidityWeek

Imelda = B=="IMELDA VILLAGE"
BakakengCentral = B=="BAKAKENG CENTRAL"
ABCR = B=="A. BONIFACIO-CAGUIOA-RIMANDO (ABCR)"
Ambiong = B=="AMBIONG"
BakakengNorth = B=="BAKAKENG NORTH"
Asin = B=="ASIN"
Balsigan = B=="BALSIGAN"
Bayan = B=="BAYAN PARK WEST (BAYAN PARK)"
BGH=B=="BGH COMPOUND"
Brookside = B=="BROOKSIDE"
Camdas = B=="CAMDAS SUBDIVISION"
c7=B=="CAMP 7"
c8=B=="CAMP 8"
ca=B=="CAMP ALLEN"
cf=B=="CAMPO FILIPINO"
ccv=B=="COUNTRY CLUB VILLAGE"
DPS=B=="DPS AREA"
enghill=B=="ENGINEER'S HILL"
fvill=B=="FAIRVIEW VILLAGE"
GLuna=B=="GENERAL LUNA, UPPER"
Gib=B=="GIBRALTAR"
GuisadC=B=="GUISAD CENTRAL"
GuisadS=B=="GUISAD SORONG"
HHL=B=="HAPPY HOMES (HAPPY HOMES-LUCBAN)"
kias=B=="KIAS"
LLoak=B=="LIWANAG-LOAKAN"
LoakP=B=="LOAKAN PROPER"
LOPJ=B=="LOPEZ JAENA"
lucnab=B=="LUCNAB"
MR=B=="MANUEL A. ROXAS"
MCutOff=B=="MILITARY CUT-OFF"
MRR=B=="MRR-QUEEN OF PEACE"
PadreZ=B=="PADRE ZAMORA"
Pinget=B=="PINGET"
PPP=B=="PINSAO PILOT PROJECT"
PisaoP=B=="PINSAO PROPER"
Poliwes=B=="POLIWES"
QHP=B=="QUEZON HILL PROPER"
QHL=B=="QUIRINO HILL, LOWER"
QHM=B=="QUIRINO HILL, MIDDLE"
salud=B=="SALUD MITRA"
sanvic=B=="SAN VICENTE"
SanCamp=B=="SANITARY CAMP, NORTH"
STE=B=="SANTA ESCOLASTICA"
STP=B=="SANTO TOMAS PROPER"&B=="STO. TOMAS PROPER"
STsa=B=="SANTO TOMAS SCHOOL AREA"
tranc=B=="TRANCOVILLE"
birac=B=="BIRAC"
AL=B=="APUGAN-LOAKAN"
AZKCO=B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")
ATAB=B==toupper("Alfonso Tabora")
AB=B==toupper("Andres Bonifacio (Lower Bokawkan)")
ATOK=B==toupper("Atok Trail")
auroraP=B==toupper("Aurora Hill Proper (Malvar-Sgt. Floresca)")
auroraN=B==toupper("Aurora Hill, North Central")
auroraS=B==toupper("Aurora Hill, South Central")
BL=B==toupper("Bagong Lipunan (Market Area)")
Bal=B==toupper("Bal-Marcoville (Marcoville)")
BPE=B==toupper("Bayan Park East")
BPV=B==toupper("Bayan Park Village")
Brookspoint=B==toupper("Brookspoint")
CabinetHill=B==toupper("Cabinet Hill-Teacher's Camp")
CityCC=B==toupper("City Camp Central")
CityCP=B==toupper("City Camp Proper")
DagisanL=B==toupper("Dagsian, Lower")
Cres=B==toupper("Cresencia Village")
DagisanU=B==toupper("Dagsian, Upper")
Dizon=B==toupper("Dizon Subdivision")
Domin=B==toupper("Dominican Hill-Mirador")
Dontogan=B=="DONTOGAN"
DPS=B=="DPS AREA"
Ferdinand=B==toupper("Ferdinand (Happy Homes-Campo Sioco)")
GabSi=B=="GABRIELLA SILANG"
GenE=B==toupper("General Emilio F. Aguinaldo (Quirino-Magsaysay, Lower)")
GenLL=B==toupper("General Luna, Lower")
GenLU=B==toupper("General Luna, Upper")
Greenwater=B==toupper("Greenwater Village")
HHol=B==toupper("Happy Hollow")
HHom=B==toupper("Happy Homes (Happy Homes-Lucban)")
HCC=B==toupper("Harrison-Claudio Carantes")
Hillside=B==toupper("Hillside")
HGhostE=B==toupper("Holy Ghost Extension")
HGhostP=B==toupper("Holy Ghost Proper")
Honeymoon=B==toupper("Honeymoon (Honeymoon-Holy Ghost)")
IMarcos=B==toupper("Imelda R. Marcos (La Salle)")
Imelda=B==toupper("Imelda Village")
Kabayanihan=B==toupper("Kabayanihan")
Kagitingan=B==toupper("Kagitingan")
KayangHill=B==toupper("Kayang-Hilltop")
KayangE=B==toupper("Kayang Extension")
LBK=B==toupper("Legarda-Burnham-Kisad")
LourdesE=B==toupper("Lourdes Subdivision Extension")
LourdesL=B==toupper("Lourdes Subdivision, Lower")
LourdesP=B==toupper("Lourdes Subdivision, Proper")
Lualhati=B==toupper("Lualhati")
MagsaysayPR=B==toupper("Magsaysay Private Road")
MagsaysayL=B==toupper("Magsaysay, Lower")
MagsaysayU=B==toupper("Magsaysay, Upper")
MalcolmSquarePerfectoJoseAbadSantos=B==toupper("Malcolm Square-Perfecto (Jose Abad Santos)")
MarketSubdivisionUpper=B==toupper("Market Subdivision, Upper")
MiddleQuezonHillSubdivsion=B==toupper("Middle Quezon Hill Subdivsiion(Quezon Hill Middle)")
MinesView=B==toupper("Mines View Park")
ModernSiteE=B==toupper("Modern Site, East")
ModernSiteW=B==toupper("Modern Site, West")
NewLuc=B==toupper("New Lucban")
Outlook=B==toupper("Outlook Drive")
Pacdal=B==toupper("Pacdal")
PadreB=B==toupper("Padre Burgos")
PadreZ=B==toupper("Padre Zamora")
PU=B==toupper("Palma-Urbano (Cariño-Palma)")
PhilAm=B==toupper("Phil-Am")
pucsusan=B==toupper("Pucsusan")
QHillU=B==toupper("Quezon Hill, Upper")
UpperQM=B==toupper("Quirino-Magsaysay, Upper (Upper QM)")
QHillE=B==toupper("Quirino Hill, East")
QHillW=B==toupper("Quirino Hill, West")
RizalMonumentArea=B==toupper("Rizal Monument Area")
RockQuarryLower=B==toupper("Rock Quarry, Lower")
RockQuarryMiddle=B==toupper("Rock Quarry, Middle")
RockQuarryUpper=B==toupper("Rock Quarry, Upper")
SanAntonioVillage=B==toupper("San Antonio Village")
SanLuisVillage=B==toupper("San Luis Village")
SanRoqueVillage=B==toupper("San Roque Village")
SanitaryCampSouth=B==toupper("Sanitary Camp, South")
SantoR=B==toupper("Santo Rosario")
ScoutB=B==toupper("Scout Barrio")
Session=B==toupper("Session Road Area")
Slaughter=B==toupper("Slaughter House Area (Santo Niño Slaughter)")
SLU=B==toupper("SLU-SVP Housing Village")
SouthDrive=B==toupper("South Drive")
TeoAl=B==toupper("Teodora Alonzo")
tranco=B==toupper("Trancoville")
victoria=B==toupper("Victoria Village")
STJos=B=="ST JOSEPH VILLAGE"

adf14 <- data.frame(MorbidityWeek=1,
                    ABonifacioCaguioaRimandoABCR=0,
                    AbanaoZanduetaKayongChugumOtekAZKCO=0,
                    AlfonsoTabora=0,
                    Ambiong=0,
                    AndresBonifacioLowerBokawkan=0,
                    ApuganLoakan=0,
                    AsinRoad=0,
                    AtokTrail=0,
                    AuroraHillProperMalvarSgtFloresca=0,
                    AuroraHillNorthCentral=0,
                    AuroraHillSouthCentral=0,
                    BagongLipunanMarketArea=0,
                    BakakengCentral=0,
                    BakakengNorth=1,
                    BalMarcovilleMarcoville=0,
                    Balsigan=0,
                    BayanParkEast=0,
                    BayanParkVillage=0,
                    BayanParkWestBayanPark=0,
                    BGHCompound=0,
                    Brookside=0,
                    Brookspoint=0,
                    CabinetHillTeachersCamp=0,
                    CamdasSubdivision=0,
                    Camp7=0,
                    Camp8=0,
                    CampAllen=0,
                    CampoFilipino=0,
                    CityCampCentral=0,
                    CityCampProper=0,
                    CountryClubVillage=0,
                    CresenciaVillage=0,
                    DagisanLower=0,
                    DagisanUpper=0,
                    DizonSubdivision=0,
                    DominicanHillMirador=0,
                    Dontogan=0,
                    DPSArea=0,
                    EngineersHill=0,
                    FairviewVillage=0,
                    FerdinandHappyHomesCampoSioco=0,
                    FortdelPilar=0,
                    GabrielaSilang=0,
                    GeneralEmilioFAguinaldoQuirinoMagsaysayLower=0,
                    GeneralLunaLower=0,
                    GeneralLunaUpper=0,
                    Gibraltar=0,
                    GreenwaterVillage=0,
                    GuisadCentral=0,
                    GuisadSorong=0,
                    HappyHollow=0,
                    HappyHomesHappyHomesLucban=0,
                    HarrisonClaudioCarantes=0,
                    Hillside=0,
                    HolyGhostExtension=0,
                    HolyGhostProper=0,
                    HoneymoonHoneymoonHolyGhost=0,
                    ImeldaRMarcosLaSalle=0,
                    ImeldaVillage=0,
                    Irisan=0,
                    Kabayanihan=0,
                    Kagitingan=0,
                    KayangHilltop=0,
                    KayangExtension=0,
                    Kias=0,
                    LegardaBurnhamKisad=0,
                    LiwanagLoakan=0,
                    LoakanProper=0,
                    LopezJaena=0,
                    LourdesSubdivisionExtension=0,
                    LourdesSubdivisionLower=0,
                    LourdesSubdivisionProper=0,
                    Lualhati=0,
                    Lucnab=0,
                    MagsaysayPrivateRoad=0,
                    MagsaysayLower=0,
                    MagsaysayUpper=0,
                    MalcolmSquarePerfectoJoseAbadSantos=0,
                    ManuelARoxas=0,
                    MarketSubdivisionUpper=0,
                    MiddleQuezonHillSubdivsion=0,
                    MilitaryCutoff=0,
                    MinesViewPark=0,
                    ModernSiteEast=0,
                    ModernSiteWest=0,
                    MRRQueenofPeace=0,
                    NewLucban=0,
                    OutlookDrive=0,
                    Pacdal=0,
                    PadreBurgos=0,
                    PadreZamora=0,
                    PalmaUrbanoCarinoPalma=0,
                    PhilAm=0,
                    Pinget=0,
                    PinsaoPilotProject=0,
                    PinsaoProper=0,
                    Poliwes=0,
                    Pucsusan=0,
                    QuezonHillProper=0,
                    QuezonHillUpper=0,
                    QuirinoMagsaysayUpperUpperQM=0,
                    QuirinoHillEast=0,
                    QuirinoHillLower=0,
                    QuirinoHillMiddle=0,
                    QuirinoHillWest=0,
                    RizalMonumentArea=0,
                    RockQuarryLower=0,
                    RockQuarryMiddle=0,
                    RockQuarryUpper=0,
                    SaludMitra=0,
                    SanAntoniVillage=0,
                    SanLuisVillage=0,
                    SanRoqueVillage=0,
                    SanVicente=0,
                    SanitaryCampNorth=0,
                    SanitaryCampSouth=0,
                    SantaEscolastica=0,
                    SantoRosario=0,
                    SantoTomasProper=0,
                    SantoTomasSchoolArea=0,
                    ScoutBarrio=0,
                    SessionRoadArea=0,
                    SlaughterHouseAreaSantoNiñoSlaughter=0,
                    SLUSVPHousingVillage=0,
                    SouthDrive=0,
                    TeodoraAlonzo=0,
                    Trancoville=0,
                    VictoriaVillage=0,
                    SaintJosephVillage=0
)

for (n in 2:52) {
  adf14[nrow(adf14)+1,]=c(n,
                          count(abd14[ABCR&SC=="Positive"&MW==n,])+
                            count(abd14[ABCR&SC=="POSITIVE"&MW==n,]),
                          count(abd14[B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")&SC=="Positive"&MW==n,])+
                            count(abd14[B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")&SC=="POSITIVE"&MW==n,]),
                          count(abd14[B==toupper("Alfonso Tabora")&SC=="Positive"&MW==n,])+
                            count(abd14[B==toupper("Alfonso Tabora")&SC=="POSITIVE"&MW==n,]),
                          count(abd14[B=="AMBIONG"&SC=="Positive"&MW==n,])+
                            count(abd14[B=="AMBIONG"&SC=="POSITIVE"&MW==n,]),
                          count(abd14[AB&SC=="Positive"&MW==n,])+
                            count(abd14[AB&SC=="POSITIVE"&MW==n,]),
                          count(abd14[AL&SC=="Positive"&MW==n,])+
                            count(abd14[AL&SC=="POSITIVE"&MW==n,]),
                          count(abd14[Asin&SC=="Positive"&MW==n,])+
                            count(abd14[Asin&SC=="POSITIVE"&MW==n,]),
                          count(abd14[ATOK&SC=="Positive"&MW==n,])+
                            count(abd14[ATOK&SC=="POSITIVE"&MW==n,]),
                          count(abd14[auroraP&SC=="Positive"&MW==n,])+
                            count(abd14[auroraP&SC=="POSITIVE"&MW==n,]),
                          count(abd14[auroraN&SC=="Positive"&MW==n,])+
                            count(abd14[auroraN&SC=="POSITIVE"&MW==n,]),
                          count(abd14[auroraS&SC=="POSITIVE"&MW==n,]),
                          count(abd14[BL&SC=="Positive"&MW==n,])+
                            count(abd14[BL&SC=="POSITIVE"&MW==n,]),
                          count(abd14[BakakengCentral&SC=="Positive"&MW==n,])+
                            count(abd14[BakakengCentral&SC=="POSITIVE"&MW==n,]),
                          count(abd14[BakakengNorth&SC=="Positive"&MW==n,])+
                            count(abd14[BakakengNorth&SC=="POSITIVE"&MW==n,]),
                          count(abd14[Bal&SC=="Positive"&MW==n,])+
                            count(abd14[Bal&SC=="POSITIVE"&MW==n,]),
                          count(abd14[Balsigan&SC=="Positive"&MW==n,])+
                            count(abd14[Balsigan&SC=="POSITIVE"&MW==n,]),
                          count(abd14[BPE&SC=="Positive"&MW==n,])+
                            count(abd14[BPE&SC=="POSITIVE"&MW==n,]),
                          count(abd14[BPV&SC=="Positive"&MW==n,])+
                            count(abd14[BPV&SC=="POSITIVE"&MW==n,]),
                          count(abd14[Bayan&SC=="Positive"&MW==n,])+
                            count(abd14[Bayan&SC=="POSITIVE"&MW==n,]),
                          count(abd14[BGH&SC=="Positive"&MW==n,])+
                            count(abd14[BGH&SC=="POSITIVE"&MW==n,]),
                          count(abd14[Brookside&SC=="Positive"&MW==n,])+
                            count(abd14[Brookside&SC=="POSITIVE"&MW==n,]),
                          count(abd14[Brookspoint&SC=="Positive"&MW==n,])+
                            count(abd14[Brookspoint&SC=="POSITIVE"&MW==n,]),
                          count(abd14[CabinetHill&SC=="Positive"&MW==n,])+
                            count(abd14[CabinetHill&SC=="POSITIVE"&MW==n,]),
                          count(abd14[Camdas&SC=="Positive"&MW==n,])+
                            count(abd14[Camdas&SC=="POSITIVE"&MW==n,]),
                          count(abd14[c7&SC=="Positive"&MW==n,])+
                            count(abd14[c7&SC=="POSITIVE"&MW==n,]),
                          count(abd14[c8&SC=="Positive"&MW==n,])+
                            count(abd14[c8&SC=="POSITIVE"&MW==n,]),
                          count(abd14[ca&SC=="Positive"&MW==n,])+
                            count(abd14[ca&SC=="POSITIVE"&MW==n,]),
                          count(abd14[cf&SC=="Positive"&MW==n,])+
                            count(abd14[cf&SC=="POSITIVE"&MW==n,]),
                          count(abd14[CityCC&SC=="Positive"&MW==n,])+
                            count(abd14[CityCC&SC=="POSITIVE"&MW==n,]),
                          count(abd14[CityCP&SC=="Positive"&MW==n,])+
                            count(abd14[CityCP&SC=="POSITIVE"&MW==n,]),
                          count(abd14[ccv&SC=="Positive"&MW==n,])+
                            count(abd14[ccv&SC=="POSITIVE"&MW==n,]),
                          count(abd14[Cres&SC=="Positive"&MW==n,])+
                            count(abd14[Cres&SC=="POSITIVE"&MW==n,]),
                          count(abd14[DagisanL&SC=="Positive"&MW==n,])+
                            count(abd14[DagisanL&SC=="POSITIVE"&MW==n,]),
                          count(abd14[DagisanU&SC=="Positive"&MW==n,])+
                            count(abd14[DagisanU&SC=="POSITIVE"&MW==n,]),
                          count(abd14[Dizon&SC=="Positive"&MW==n,])+
                            count(abd14[Dizon&SC=="POSITIVE"&MW==n,]),
                          count(abd14[Domin&SC=="Positive"&MW==n,])+
                            count(abd14[Domin&SC=="POSITIVE"&MW==n,]),
                          count(abd14[Dontogan&SC=="Positive"&MW==n,])+
                            count(abd14[Dontogan&SC=="POSITIVE"&MW==n,]),
                          count(abd14[DPS&SC=="Positive"&MW==n,])+
                            count(abd14[DPS&SC=="POSITIVE"&MW==n,]),
                          count(abd14[enghill&SC=="Positive"&MW==n,])+
                            count(abd14[enghill&SC=="POSITIVE"&MW==n,]),
                          count(abd14[fvill&SC=="Positive"&MW==n,])+
                            count(abd14[fvill&SC=="POSITIVE"&MW==n,]),
                          count(abd14[Ferdinand&SC=="Positive"&MW==n,])+
                            count(abd14[Ferdinand&SC=="POSITIVE"&MW==n,]),
                          count(abd14[B=="FORT DEL PILAR"&SC=="Positive"&MW==n,])+
                            count(abd14[B=="FORT DEL PILAR"&SC=="POSITIVE"&MW==n,]),
                          count(abd14[GabSi&SC=="Positive"&MW==n,])+
                            count(abd14[GabSi&SC=="POSITIVE"&MW==n,]),
                          count(abd14[GenE&SC=="Positive"&MW==n,])+
                            count(abd14[GenE&SC=="POSITIVE"&MW==n,]),
                          count(abd14[GenLL&SC=="Positive"&MW==n,])+
                            count(abd14[GenLL&SC=="POSITIVE"&MW==n,]),
                          count(abd14[GenLU&SC=="Positive"&MW==n,])+
                            count(abd14[GenLU&SC=="POSITIVE"&MW==n,]),
                          count(abd14[Gib&SC=="Positive"&MW==n,])+
                            count(abd14[Gib&SC=="POSITIVE"&MW==n,]),
                          count(abd14[Greenwater&SC=="Positive"&MW==n,])+
                            count(abd14[Greenwater&SC=="POSITIVE"&MW==n,]),
                          count(abd14[GuisadC&SC=="Positive"&MW==n,])+
                            count(abd14[GuisadC&SC=="POSITIVE"&MW==n,]),
                          count(abd14[GuisadS&SC=="Positive"&MW==n,])+
                            count(abd14[GuisadS&SC=="POSITIVE"&MW==n,]),
                          count(abd14[HHol&SC=="Positive"&MW==n,])+
                            count(abd14[HHol&SC=="POSITIVE"&MW==n,]),
                          count(abd14[HHom&SC=="Positive"&MW==n,])+
                            count(abd14[HHom&SC=="POSITIVE"&MW==n,]),
                          count(abd14[HCC&SC=="Positive"&MW==n,])+
                            count(abd14[HCC&SC=="POSITIVE"&MW==n,]),
                          count(abd14[Hillside&SC=="Positive"&MW==n,])+
                            count(abd14[Hillside&SC=="POSITIVE"&MW==n,]),
                          count(abd14[HGhostE&SC=="Positive"&MW==n,])+
                            count(abd14[HGhostE&SC=="POSITIVE"&MW==n,]),
                          count(abd14[HGhostP&SC=="Positive"&MW==n,])+
                            count(abd14[HGhostP&SC=="POSITIVE"&MW==n,]),
                          count(abd14[Honeymoon&SC=="Positive"&MW==n,])+
                            count(abd14[Honeymoon&SC=="POSITIVE"&MW==n,]),
                          count(abd14[IMarcos&SC=="Positive"&MW==n,])+
                            count(abd14[IMarcos&SC=="POSITIVE"&MW==n,]),
                          count(abd14[Imelda&SC=="Positive"&MW==n,])+
                            count(abd14[Imelda&SC=="POSITIVE"&MW==n,]),
                          count(abd14[B=="IRISAN"&SC=="Positive"&MW==n,])+
                            count(abd14[B=="IRISAN"&SC=="POSITIVE"&MW==n,]),
                          count(abd14[Kabayanihan&SC=="Positive"&MW==n,])+
                            count(abd14[Kabayanihan&SC=="POSITIVE"&MW==n,]),
                          count(abd14[Kagitingan&SC=="Positive"&MW==n,])+
                            count(abd14[Kagitingan&SC=="POSITIVE"&MW==n,]),
                          count(abd14[KayangHill&SC=="Positive"&MW==n,])+
                            count(abd14[KayangHill&SC=="POSITIVE"&MW==n,]),
                          count(abd14[KayangE&SC=="Positive"&MW==n,])+
                            count(abd14[KayangE&SC=="POSITIVE"&MW==n,]),
                          count(abd14[kias&SC=="Positive"&MW==n,])+
                            count(abd14[kias&SC=="POSITIVE"&MW==n,]),
                          count(abd14[LBK&SC=="Positive"&MW==n,])+
                            count(abd14[LBK&SC=="POSITIVE"&MW==n,]),
                          count(abd14[LLoak&SC=="Positive"&MW==n,])+
                            count(abd14[LLoak&SC=="POSITIVE"&MW==n,]),
                          count(abd14[LoakP&SC=="Positive"&MW==n,])+
                            count(abd14[LoakP&SC=="POSITIVE"&MW==n,]),
                          count(abd14[LOPJ&SC=="Positive"&MW==n,])+
                            count(abd14[LOPJ&SC=="POSITIVE"&MW==n,]),
                          count(abd14[LourdesE&SC=="Positive"&MW==n,])+
                            count(abd14[LourdesE&SC=="POSITIVE"&MW==n,]),
                          count(abd14[LourdesL&SC=="Positive"&MW==n,])+
                            count(abd14[LourdesL&SC=="POSITIVE"&MW==n,]),
                          count(abd14[LourdesP&SC=="Positive"&MW==n,])+
                            count(abd14[LourdesP&SC=="POSITIVE"&MW==n,]),
                          count(abd14[Lualhati&SC=="Positive"&MW==n,])+
                            count(abd14[Lualhati&SC=="POSITIVE"&MW==n,]),
                          count(abd14[lucnab&SC=="Positive"&MW==n,])+
                            count(abd14[lucnab&SC=="POSITIVE"&MW==n,]),
                          count(abd14[MagsaysayPR&SC=="Positive"&MW==n,])+
                            count(abd14[MagsaysayPR&SC=="POSITIVE"&MW==n,]),
                          count(abd14[MagsaysayL&SC=="Positive"&MW==n,])+
                            count(abd14[MagsaysayL&SC=="POSITIVE"&MW==n,]),
                          count(abd14[MagsaysayU&SC=="Positive"&MW==n,])+
                            count(abd14[MagsaysayU&SC=="POSITIVE"&MW==n,]),
                          count(abd14[MalcolmSquarePerfectoJoseAbadSantos&SC=="Positive"&MW==n,])+
                            count(abd14[MalcolmSquarePerfectoJoseAbadSantos&SC=="POSITIVE"&MW==n,]),
                          count(abd14[MR&SC=="Positive"&MW==n,])+
                            count(abd14[MR&SC=="POSITIVE"&MW==n,]),
                          count(abd14[MarketSubdivisionUpper&SC=="Positive"&MW==n,])+
                            count(abd14[MarketSubdivisionUpper&SC=="POSITIVE"&MW==n,]),
                          count(abd14[MiddleQuezonHillSubdivsion&SC=="Positive"&MW==n,])+
                            count(abd14[MiddleQuezonHillSubdivsion&SC=="POSITIVE"&MW==n,]),
                          count(abd14[MCutOff&SC=="Positive"&MW==n,])+
                            count(abd14[MCutOff&SC=="POSITIVE"&MW==n,]),
                          count(abd14[MinesView&SC=="Positive"&MW==n,])+
                            count(abd14[MinesView&SC=="POSITIVE"&MW==n,]),
                          count(abd14[ModernSiteE&SC=="Positive"&MW==n,])+
                            count(abd14[ModernSiteE&SC=="POSITIVE"&MW==n,]),
                          count(abd14[ModernSiteW&SC=="Positive"&MW==n,])+
                            count(abd14[ModernSiteW&SC=="POSITIVE"&MW==n,]),
                          count(abd14[MRR&SC=="Positive"&MW==n,])+
                            count(abd14[MRR&SC=="POSITIVE"&MW==n,]),
                          count(abd14[NewLuc&SC=="Positive"&MW==n,])+
                            count(abd14[NewLuc&SC=="POSITIVE"&MW==n,]),
                          count(abd14[Outlook&SC=="Positive"&MW==n,])+
                            count(abd14[Outlook&SC=="POSITIVE"&MW==n,]),
                          count(abd14[Pacdal&SC=="Positive"&MW==n,])+
                            count(abd14[Pacdal&SC=="POSITIVE"&MW==n,]),
                          count(abd14[PadreB&SC=="Positive"&MW==n,])+
                            count(abd14[PadreB&SC=="POSITIVE"&MW==n,]),
                          count(abd14[PadreZ&SC=="Positive"&MW==n,])+
                            count(abd14[PadreZ&SC=="POSITIVE"&MW==n,]),
                          count(abd14[PU&SC=="Positive"&MW==n,])+
                            count(abd14[PU&SC=="POSITIVE"&MW==n,]),
                          count(abd14[PhilAm&SC=="Positive"&MW==n,])+
                            count(abd14[PhilAm&SC=="POSITIVE"&MW==n,]),
                          count(abd14[Pinget&SC=="Positive"&MW==n,])+
                            count(abd14[Pinget&SC=="POSITIVE"&MW==n,]),
                          count(abd14[PPP&SC=="Positive"&MW==n,])+
                            count(abd14[PPP&SC=="POSITIVE"&MW==n,]),
                          count(abd14[PisaoP&SC=="Positive"&MW==n,])+
                            count(abd14[PisaoP&SC=="POSITIVE"&MW==n,]),
                          count(abd14[Poliwes&SC=="Positive"&MW==n,])+
                            count(abd14[Poliwes&SC=="POSITIVE"&MW==n,]),
                          count(abd14[pucsusan&SC=="Positive"&MW==n,])+
                            count(abd14[pucsusan&SC=="POSITIVE"&MW==n,]),
                          count(abd14[QHP&SC=="Positive"&MW==n,])+
                            count(abd14[QHP&SC=="POSITIVE"&MW==n,]),
                          count(abd14[QHillU&SC=="Positive"&MW==n,])+
                            count(abd14[QHillU&SC=="POSITIVE"&MW==n,]),
                          count(abd14[UpperQM&SC=="Positive"&MW==n,])+
                            count(abd14[UpperQM&SC=="POSITIVE"&MW==n,]),
                          count(abd14[QHillE&SC=="Positive"&MW==n,])+
                            count(abd14[QHillE&SC=="POSITIVE"&MW==n,]),
                          count(abd14[QHL&SC=="Positive"&MW==n,])+
                            count(abd14[QHL&SC=="POSITIVE"&MW==n,]),
                          count(abd14[QHM&SC=="Positive"&MW==n,])+
                            count(abd14[QHM&SC=="POSITIVE"&MW==n,]),
                          count(abd14[QHillW&SC=="Positive"&MW==n,])+
                            count(abd14[QHillW&SC=="POSITIVE"&MW==n,]),
                          count(abd14[RizalMonumentArea&SC=="Positive"&MW==n,])+
                            count(abd14[RizalMonumentArea&SC=="POSITIVE"&MW==n,]),
                          count(abd14[RockQuarryLower&SC=="Positive"&MW==n,])+
                            count(abd14[RockQuarryLower&SC=="POSITIVE"&MW==n,]),
                          count(abd14[RockQuarryMiddle&SC=="Positive"&MW==n,])+
                            count(abd14[RockQuarryMiddle&SC=="POSITIVE"&MW==n,]),
                          count(abd14[RockQuarryUpper&SC=="Positive"&MW==n,])+
                            count(abd14[RockQuarryUpper&SC=="POSITIVE"&MW==n,]),
                          count(abd14[salud&SC=="Positive"&MW==n,])+
                            count(abd14[salud&SC=="POSITIVE"&MW==n,]),
                          count(abd14[SanAntonioVillage&SC=="Positive"&MW==n,])+
                            count(abd14[SanAntonioVillage&SC=="POSITIVE"&MW==n,]),
                          count(abd14[SanLuisVillage&SC=="Positive"&MW==n,])+
                            count(abd14[SanLuisVillage&SC=="POSITIVE"&MW==n,]),
                          count(abd14[SanRoqueVillage&SC=="Positive"&MW==n,])+
                            count(abd14[SanRoqueVillage&SC=="POSITIVE"&MW==n,]),
                          count(abd14[sanvic&SC=="Positive"&MW==n,])+
                            count(abd14[sanvic&SC=="POSITIVE"&MW==n,]),
                          count(abd14[SanCamp&SC=="Positive"&MW==n,])+
                            count(abd14[SanCamp&SC=="POSITIVE"&MW==n,]),
                          count(abd14[SanitaryCampSouth&SC=="Positive"&MW==n,])+
                            count(abd14[SanitaryCampSouth&SC=="POSITIVE"&MW==n,]),
                          count(abd14[STE&SC=="Positive"&MW==n,])+
                            count(abd14[STE&SC=="POSITIVE"&MW==n,]),
                          count(abd14[SantoR&SC=="Positive"&MW==n,])+
                            count(abd14[SantoR&SC=="POSITIVE"&MW==n,]),
                          count(abd14[STP&SC=="Positive"&MW==n,])+
                            count(abd14[STP&SC=="POSITIVE"&MW==n,]),
                          count(abd14[STsa&SC=="Positive"&MW==n,])+
                            count(abd14[STsa&SC=="POSITIVE"&MW==n,]),
                          count(abd14[ScoutB&SC=="Positive"&MW==n,])+
                            count(abd14[ScoutB&SC=="POSITIVE"&MW==n,]),
                          count(abd14[Session&SC=="Positive"&MW==n,])+
                            count(abd14[Session&SC=="POSITIVE"&MW==n,]),
                          count(abd14[Slaughter&SC=="Positive"&MW==n,])+
                            count(abd14[Slaughter&SC=="POSITIVE"&MW==n,]),
                          count(abd14[SLU&SC=="Positive"&MW==n,])+
                            count(abd14[SLU&SC=="POSITIVE"&MW==n,]),
                          count(abd14[SouthDrive&SC=="Positive"&MW==n,])+
                            count(abd14[SouthDrive&SC=="POSITIVE"&MW==n,]),
                          count(abd14[TeoAl&SC=="Positive"&MW==n,])+
                            count(abd14[TeoAl&SC=="POSITIVE"&MW==n,]),
                          count(abd14[tranco&SC=="Positive"&MW==n,])+
                            count(abd14[tranco&SC=="POSITIVE"&MW==n,]),
                          count(abd14[victoria&SC=="Positive"&MW==n,])+
                            count(abd14[victoria&SC=="POSITIVE"&MW==n,]),
                          count(abd14[STJos&SC=="Positive"&MW==n,])+
                            count(abd14[STJos&SC=="POSITIVE"&MW==n,])
  )
}

B=abd15$Barangay
SC=abd15$StoolCulture
MW=abd15$MorbidityWeek

Imelda = B=="IMELDA VILLAGE"
BakakengCentral = B=="BAKAKENG CENTRAL"
ABCR = B=="A. BONIFACIO-CAGUIOA-RIMANDO (ABCR)"
Ambiong = B=="AMBIONG"
BakakengNorth = B=="BAKAKENG NORTH"
Asin = B=="ASIN"
Balsigan = B=="BALSIGAN"
Bayan = B=="BAYAN PARK WEST (BAYAN PARK)"
BGH=B=="BGH COMPOUND"
Brookside = B=="BROOKSIDE"
Camdas = B=="CAMDAS SUBDIVISION"
c7=B=="CAMP 7"
c8=B=="CAMP 8"
ca=B=="CAMP ALLEN"
cf=B=="CAMPO FILIPINO"
ccv=B=="COUNTRY CLUB VILLAGE"
DPS=B=="DPS AREA"
enghill=B=="ENGINEER'S HILL"
fvill=B=="FAIRVIEW VILLAGE"
GLuna=B=="GENERAL LUNA, UPPER"
Gib=B=="GIBRALTAR"
GuisadC=B=="GUISAD CENTRAL"
GuisadS=B=="GUISAD SORONG"
HHL=B=="HAPPY HOMES (HAPPY HOMES-LUCBAN)"
kias=B=="KIAS"
LLoak=B=="LIWANAG-LOAKAN"
LoakP=B=="LOAKAN PROPER"
LOPJ=B=="LOPEZ JAENA"
lucnab=B=="LUCNAB"
MR=B=="MANUEL A. ROXAS"
MCutOff=B=="MILITARY CUT-OFF"
MRR=B=="MRR-QUEEN OF PEACE"
PadreZ=B=="PADRE ZAMORA"
Pinget=B=="PINGET"
PPP=B=="PINSAO PILOT PROJECT"
PisaoP=B=="PINSAO PROPER"
Poliwes=B=="POLIWES"
QHP=B=="QUEZON HILL PROPER"
QHL=B=="QUIRINO HILL, LOWER"
QHM=B=="QUIRINO HILL, MIDDLE"
salud=B=="SALUD MITRA"
sanvic=B=="SAN VICENTE"
SanCamp=B=="SANITARY CAMP, NORTH"
STE=B=="SANTA ESCOLASTICA"
STP=B=="SANTO TOMAS PROPER"&B=="STO. TOMAS PROPER"
STsa=B=="SANTO TOMAS SCHOOL AREA"
tranc=B=="TRANCOVILLE"
birac=B=="BIRAC"
AL=B=="APUGAN-LOAKAN"
AZKCO=B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")
ATAB=B==toupper("Alfonso Tabora")
AB=B==toupper("Andres Bonifacio (Lower Bokawkan)")
ATOK=B==toupper("Atok Trail")
auroraP=B==toupper("Aurora Hill Proper (Malvar-Sgt. Floresca)")
auroraN=B==toupper("Aurora Hill, North Central")
auroraS=B==toupper("Aurora Hill, South Central")
BL=B==toupper("Bagong Lipunan (Market Area)")
Bal=B==toupper("Bal-Marcoville (Marcoville)")
BPE=B==toupper("Bayan Park East")
BPV=B==toupper("Bayan Park Village")
Brookspoint=B==toupper("Brookspoint")
CabinetHill=B==toupper("Cabinet Hill-Teacher's Camp")
CityCC=B==toupper("City Camp Central")
CityCP=B==toupper("City Camp Proper")
DagisanL=B==toupper("Dagsian, Lower")
Cres=B==toupper("Cresencia Village")
DagisanU=B==toupper("Dagsian, Upper")
Dizon=B==toupper("Dizon Subdivision")
Domin=B==toupper("Dominican Hill-Mirador")
Dontogan=B=="DONTOGAN"
DPS=B=="DPS AREA"
Ferdinand=B==toupper("Ferdinand (Happy Homes-Campo Sioco)")
GabSi=B=="GABRIELLA SILANG"
GenE=B==toupper("General Emilio F. Aguinaldo (Quirino-Magsaysay, Lower)")
GenLL=B==toupper("General Luna, Lower")
GenLU=B==toupper("General Luna, Upper")
Greenwater=B==toupper("Greenwater Village")
HHol=B==toupper("Happy Hollow")
HHom=B==toupper("Happy Homes (Happy Homes-Lucban)")
HCC=B==toupper("Harrison-Claudio Carantes")
Hillside=B==toupper("Hillside")
HGhostE=B==toupper("Holy Ghost Extension")
HGhostP=B==toupper("Holy Ghost Proper")
Honeymoon=B==toupper("Honeymoon (Honeymoon-Holy Ghost)")
IMarcos=B==toupper("Imelda R. Marcos (La Salle)")
Imelda=B==toupper("Imelda Village")
Kabayanihan=B==toupper("Kabayanihan")
Kagitingan=B==toupper("Kagitingan")
KayangHill=B==toupper("Kayang-Hilltop")
KayangE=B==toupper("Kayang Extension")
LBK=B==toupper("Legarda-Burnham-Kisad")
LourdesE=B==toupper("Lourdes Subdivision Extension")
LourdesL=B==toupper("Lourdes Subdivision, Lower")
LourdesP=B==toupper("Lourdes Subdivision, Proper")
Lualhati=B==toupper("Lualhati")
MagsaysayPR=B==toupper("Magsaysay Private Road")
MagsaysayL=B==toupper("Magsaysay, Lower")
MagsaysayU=B==toupper("Magsaysay, Upper")
MalcolmSquarePerfectoJoseAbadSantos=B==toupper("Malcolm Square-Perfecto (Jose Abad Santos)")
MarketSubdivisionUpper=B==toupper("Market Subdivision, Upper")
MiddleQuezonHillSubdivsion=B==toupper("Middle Quezon Hill Subdivsiion(Quezon Hill Middle)")
MinesView=B==toupper("Mines View Park")
ModernSiteE=B==toupper("Modern Site, East")
ModernSiteW=B==toupper("Modern Site, West")
NewLuc=B==toupper("New Lucban")
Outlook=B==toupper("Outlook Drive")
Pacdal=B==toupper("Pacdal")
PadreB=B==toupper("Padre Burgos")
PadreZ=B==toupper("Padre Zamora")
PU=B==toupper("Palma-Urbano (Cariño-Palma)")
PhilAm=B==toupper("Phil-Am")
pucsusan=B==toupper("Pucsusan")
QHillU=B==toupper("Quezon Hill, Upper")
UpperQM=B==toupper("Quirino-Magsaysay, Upper (Upper QM)")
QHillE=B==toupper("Quirino Hill, East")
QHillW=B==toupper("Quirino Hill, West")
RizalMonumentArea=B==toupper("Rizal Monument Area")
RockQuarryLower=B==toupper("Rock Quarry, Lower")
RockQuarryMiddle=B==toupper("Rock Quarry, Middle")
RockQuarryUpper=B==toupper("Rock Quarry, Upper")
SanAntonioVillage=B==toupper("San Antonio Village")
SanLuisVillage=B==toupper("San Luis Village")
SanRoqueVillage=B==toupper("San Roque Village")
SanitaryCampSouth=B==toupper("Sanitary Camp, South")
SantoR=B==toupper("Santo Rosario")
ScoutB=B==toupper("Scout Barrio")
Session=B==toupper("Session Road Area")
Slaughter=B==toupper("Slaughter House Area (Santo Niño Slaughter)")
SLU=B==toupper("SLU-SVP Housing Village")
SouthDrive=B==toupper("South Drive")
TeoAl=B==toupper("Teodora Alonzo")
tranco=B==toupper("Trancoville")
victoria=B==toupper("Victoria Village")
STJos=B=="ST JOSEPH VILLAGE"

adf15 <- data.frame(MorbidityWeek=1,
                    ABonifacioCaguioaRimandoABCR=0,
                    AbanaoZanduetaKayongChugumOtekAZKCO=0,
                    AlfonsoTabora=0,
                    Ambiong=0,
                    AndresBonifacioLowerBokawkan=0,
                    ApuganLoakan=0,
                    AsinRoad=0,
                    AtokTrail=0,
                    AuroraHillProperMalvarSgtFloresca=0,
                    AuroraHillNorthCentral=0,
                    AuroraHillSouthCentral=0,
                    BagongLipunanMarketArea=0,
                    BakakengCentral=0,
                    BakakengNorth=1,
                    BalMarcovilleMarcoville=0,
                    Balsigan=0,
                    BayanParkEast=0,
                    BayanParkVillage=0,
                    BayanParkWestBayanPark=0,
                    BGHCompound=0,
                    Brookside=0,
                    Brookspoint=0,
                    CabinetHillTeachersCamp=0,
                    CamdasSubdivision=0,
                    Camp7=0,
                    Camp8=0,
                    CampAllen=0,
                    CampoFilipino=0,
                    CityCampCentral=0,
                    CityCampProper=0,
                    CountryClubVillage=0,
                    CresenciaVillage=0,
                    DagisanLower=0,
                    DagisanUpper=0,
                    DizonSubdivision=0,
                    DominicanHillMirador=0,
                    Dontogan=0,
                    DPSArea=0,
                    EngineersHill=0,
                    FairviewVillage=0,
                    FerdinandHappyHomesCampoSioco=0,
                    FortdelPilar=0,
                    GabrielaSilang=0,
                    GeneralEmilioFAguinaldoQuirinoMagsaysayLower=0,
                    GeneralLunaLower=0,
                    GeneralLunaUpper=0,
                    Gibraltar=0,
                    GreenwaterVillage=0,
                    GuisadCentral=0,
                    GuisadSorong=0,
                    HappyHollow=0,
                    HappyHomesHappyHomesLucban=0,
                    HarrisonClaudioCarantes=0,
                    Hillside=0,
                    HolyGhostExtension=0,
                    HolyGhostProper=0,
                    HoneymoonHoneymoonHolyGhost=0,
                    ImeldaRMarcosLaSalle=0,
                    ImeldaVillage=0,
                    Irisan=0,
                    Kabayanihan=0,
                    Kagitingan=0,
                    KayangHilltop=0,
                    KayangExtension=0,
                    Kias=0,
                    LegardaBurnhamKisad=0,
                    LiwanagLoakan=0,
                    LoakanProper=0,
                    LopezJaena=0,
                    LourdesSubdivisionExtension=0,
                    LourdesSubdivisionLower=0,
                    LourdesSubdivisionProper=0,
                    Lualhati=0,
                    Lucnab=0,
                    MagsaysayPrivateRoad=0,
                    MagsaysayLower=0,
                    MagsaysayUpper=0,
                    MalcolmSquarePerfectoJoseAbadSantos=0,
                    ManuelARoxas=0,
                    MarketSubdivisionUpper=0,
                    MiddleQuezonHillSubdivsion=0,
                    MilitaryCutoff=0,
                    MinesViewPark=0,
                    ModernSiteEast=0,
                    ModernSiteWest=0,
                    MRRQueenofPeace=0,
                    NewLucban=0,
                    OutlookDrive=0,
                    Pacdal=0,
                    PadreBurgos=0,
                    PadreZamora=0,
                    PalmaUrbanoCarinoPalma=0,
                    PhilAm=0,
                    Pinget=0,
                    PinsaoPilotProject=0,
                    PinsaoProper=0,
                    Poliwes=0,
                    Pucsusan=0,
                    QuezonHillProper=0,
                    QuezonHillUpper=0,
                    QuirinoMagsaysayUpperUpperQM=0,
                    QuirinoHillEast=0,
                    QuirinoHillLower=0,
                    QuirinoHillMiddle=0,
                    QuirinoHillWest=0,
                    RizalMonumentArea=0,
                    RockQuarryLower=0,
                    RockQuarryMiddle=0,
                    RockQuarryUpper=0,
                    SaludMitra=0,
                    SanAntoniVillage=0,
                    SanLuisVillage=0,
                    SanRoqueVillage=0,
                    SanVicente=0,
                    SanitaryCampNorth=0,
                    SanitaryCampSouth=0,
                    SantaEscolastica=0,
                    SantoRosario=0,
                    SantoTomasProper=0,
                    SantoTomasSchoolArea=0,
                    ScoutBarrio=0,
                    SessionRoadArea=0,
                    SlaughterHouseAreaSantoNiñoSlaughter=0,
                    SLUSVPHousingVillage=0,
                    SouthDrive=0,
                    TeodoraAlonzo=0,
                    Trancoville=0,
                    VictoriaVillage=0,
                    SaintJosephVillage=0
)

for (n in 2:52) {
  adf15[nrow(adf15)+1,]=c(n,
                          count(abd15[ABCR&SC=="Positive"&MW==n,])+
                            count(abd15[ABCR&SC=="POSITIVE"&MW==n,]),
                          count(abd15[B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")&SC=="Positive"&MW==n,])+
                            count(abd15[B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")&SC=="POSITIVE"&MW==n,]),
                          count(abd15[B==toupper("Alfonso Tabora")&SC=="Positive"&MW==n,])+
                            count(abd15[B==toupper("Alfonso Tabora")&SC=="POSITIVE"&MW==n,]),
                          count(abd15[B=="AMBIONG"&SC=="Positive"&MW==n,])+
                            count(abd15[B=="AMBIONG"&SC=="POSITIVE"&MW==n,]),
                          count(abd15[AB&SC=="Positive"&MW==n,])+
                            count(abd15[AB&SC=="POSITIVE"&MW==n,]),
                          count(abd15[AL&SC=="Positive"&MW==n,])+
                            count(abd15[AL&SC=="POSITIVE"&MW==n,]),
                          count(abd15[Asin&SC=="Positive"&MW==n,])+
                            count(abd15[Asin&SC=="POSITIVE"&MW==n,]),
                          count(abd15[ATOK&SC=="Positive"&MW==n,])+
                            count(abd15[ATOK&SC=="POSITIVE"&MW==n,]),
                          count(abd15[auroraP&SC=="Positive"&MW==n,])+
                            count(abd15[auroraP&SC=="POSITIVE"&MW==n,]),
                          count(abd15[auroraN&SC=="Positive"&MW==n,])+
                            count(abd15[auroraN&SC=="POSITIVE"&MW==n,]),
                          count(abd15[auroraS&SC=="POSITIVE"&MW==n,]),
                          count(abd15[BL&SC=="Positive"&MW==n,])+
                            count(abd15[BL&SC=="POSITIVE"&MW==n,]),
                          count(abd15[BakakengCentral&SC=="Positive"&MW==n,])+
                            count(abd15[BakakengCentral&SC=="POSITIVE"&MW==n,]),
                          count(abd15[BakakengNorth&SC=="Positive"&MW==n,])+
                            count(abd15[BakakengNorth&SC=="POSITIVE"&MW==n,]),
                          count(abd15[Bal&SC=="Positive"&MW==n,])+
                            count(abd15[Bal&SC=="POSITIVE"&MW==n,]),
                          count(abd15[Balsigan&SC=="Positive"&MW==n,])+
                            count(abd15[Balsigan&SC=="POSITIVE"&MW==n,]),
                          count(abd15[BPE&SC=="Positive"&MW==n,])+
                            count(abd15[BPE&SC=="POSITIVE"&MW==n,]),
                          count(abd15[BPV&SC=="Positive"&MW==n,])+
                            count(abd15[BPV&SC=="POSITIVE"&MW==n,]),
                          count(abd15[Bayan&SC=="Positive"&MW==n,])+
                            count(abd15[Bayan&SC=="POSITIVE"&MW==n,]),
                          count(abd15[BGH&SC=="Positive"&MW==n,])+
                            count(abd15[BGH&SC=="POSITIVE"&MW==n,]),
                          count(abd15[Brookside&SC=="Positive"&MW==n,])+
                            count(abd15[Brookside&SC=="POSITIVE"&MW==n,]),
                          count(abd15[Brookspoint&SC=="Positive"&MW==n,])+
                            count(abd15[Brookspoint&SC=="POSITIVE"&MW==n,]),
                          count(abd15[CabinetHill&SC=="Positive"&MW==n,])+
                            count(abd15[CabinetHill&SC=="POSITIVE"&MW==n,]),
                          count(abd15[Camdas&SC=="Positive"&MW==n,])+
                            count(abd15[Camdas&SC=="POSITIVE"&MW==n,]),
                          count(abd15[c7&SC=="Positive"&MW==n,])+
                            count(abd15[c7&SC=="POSITIVE"&MW==n,]),
                          count(abd15[c8&SC=="Positive"&MW==n,])+
                            count(abd15[c8&SC=="POSITIVE"&MW==n,]),
                          count(abd15[ca&SC=="Positive"&MW==n,])+
                            count(abd15[ca&SC=="POSITIVE"&MW==n,]),
                          count(abd15[cf&SC=="Positive"&MW==n,])+
                            count(abd15[cf&SC=="POSITIVE"&MW==n,]),
                          count(abd15[CityCC&SC=="Positive"&MW==n,])+
                            count(abd15[CityCC&SC=="POSITIVE"&MW==n,]),
                          count(abd15[CityCP&SC=="Positive"&MW==n,])+
                            count(abd15[CityCP&SC=="POSITIVE"&MW==n,]),
                          count(abd15[ccv&SC=="Positive"&MW==n,])+
                            count(abd15[ccv&SC=="POSITIVE"&MW==n,]),
                          count(abd15[Cres&SC=="Positive"&MW==n,])+
                            count(abd15[Cres&SC=="POSITIVE"&MW==n,]),
                          count(abd15[DagisanL&SC=="Positive"&MW==n,])+
                            count(abd15[DagisanL&SC=="POSITIVE"&MW==n,]),
                          count(abd15[DagisanU&SC=="Positive"&MW==n,])+
                            count(abd15[DagisanU&SC=="POSITIVE"&MW==n,]),
                          count(abd15[Dizon&SC=="Positive"&MW==n,])+
                            count(abd15[Dizon&SC=="POSITIVE"&MW==n,]),
                          count(abd15[Domin&SC=="Positive"&MW==n,])+
                            count(abd15[Domin&SC=="POSITIVE"&MW==n,]),
                          count(abd15[Dontogan&SC=="Positive"&MW==n,])+
                            count(abd15[Dontogan&SC=="POSITIVE"&MW==n,]),
                          count(abd15[DPS&SC=="Positive"&MW==n,])+
                            count(abd15[DPS&SC=="POSITIVE"&MW==n,]),
                          count(abd15[enghill&SC=="Positive"&MW==n,])+
                            count(abd15[enghill&SC=="POSITIVE"&MW==n,]),
                          count(abd15[fvill&SC=="Positive"&MW==n,])+
                            count(abd15[fvill&SC=="POSITIVE"&MW==n,]),
                          count(abd15[Ferdinand&SC=="Positive"&MW==n,])+
                            count(abd15[Ferdinand&SC=="POSITIVE"&MW==n,]),
                          count(abd15[B=="FORT DEL PILAR"&SC=="Positive"&MW==n,])+
                            count(abd15[B=="FORT DEL PILAR"&SC=="POSITIVE"&MW==n,]),
                          count(abd15[GabSi&SC=="Positive"&MW==n,])+
                            count(abd15[GabSi&SC=="POSITIVE"&MW==n,]),
                          count(abd15[GenE&SC=="Positive"&MW==n,])+
                            count(abd15[GenE&SC=="POSITIVE"&MW==n,]),
                          count(abd15[GenLL&SC=="Positive"&MW==n,])+
                            count(abd15[GenLL&SC=="POSITIVE"&MW==n,]),
                          count(abd15[GenLU&SC=="Positive"&MW==n,])+
                            count(abd15[GenLU&SC=="POSITIVE"&MW==n,]),
                          count(abd15[Gib&SC=="Positive"&MW==n,])+
                            count(abd15[Gib&SC=="POSITIVE"&MW==n,]),
                          count(abd15[Greenwater&SC=="Positive"&MW==n,])+
                            count(abd15[Greenwater&SC=="POSITIVE"&MW==n,]),
                          count(abd15[GuisadC&SC=="Positive"&MW==n,])+
                            count(abd15[GuisadC&SC=="POSITIVE"&MW==n,]),
                          count(abd15[GuisadS&SC=="Positive"&MW==n,])+
                            count(abd15[GuisadS&SC=="POSITIVE"&MW==n,]),
                          count(abd15[HHol&SC=="Positive"&MW==n,])+
                            count(abd15[HHol&SC=="POSITIVE"&MW==n,]),
                          count(abd15[HHom&SC=="Positive"&MW==n,])+
                            count(abd15[HHom&SC=="POSITIVE"&MW==n,]),
                          count(abd15[HCC&SC=="Positive"&MW==n,])+
                            count(abd15[HCC&SC=="POSITIVE"&MW==n,]),
                          count(abd15[Hillside&SC=="Positive"&MW==n,])+
                            count(abd15[Hillside&SC=="POSITIVE"&MW==n,]),
                          count(abd15[HGhostE&SC=="Positive"&MW==n,])+
                            count(abd15[HGhostE&SC=="POSITIVE"&MW==n,]),
                          count(abd15[HGhostP&SC=="Positive"&MW==n,])+
                            count(abd15[HGhostP&SC=="POSITIVE"&MW==n,]),
                          count(abd15[Honeymoon&SC=="Positive"&MW==n,])+
                            count(abd15[Honeymoon&SC=="POSITIVE"&MW==n,]),
                          count(abd15[IMarcos&SC=="Positive"&MW==n,])+
                            count(abd15[IMarcos&SC=="POSITIVE"&MW==n,]),
                          count(abd15[Imelda&SC=="Positive"&MW==n,])+
                            count(abd15[Imelda&SC=="POSITIVE"&MW==n,]),
                          count(abd15[B=="IRISAN"&SC=="Positive"&MW==n,])+
                            count(abd15[B=="IRISAN"&SC=="POSITIVE"&MW==n,]),
                          count(abd15[Kabayanihan&SC=="Positive"&MW==n,])+
                            count(abd15[Kabayanihan&SC=="POSITIVE"&MW==n,]),
                          count(abd15[Kagitingan&SC=="Positive"&MW==n,])+
                            count(abd15[Kagitingan&SC=="POSITIVE"&MW==n,]),
                          count(abd15[KayangHill&SC=="Positive"&MW==n,])+
                            count(abd15[KayangHill&SC=="POSITIVE"&MW==n,]),
                          count(abd15[KayangE&SC=="Positive"&MW==n,])+
                            count(abd15[KayangE&SC=="POSITIVE"&MW==n,]),
                          count(abd15[kias&SC=="Positive"&MW==n,])+
                            count(abd15[kias&SC=="POSITIVE"&MW==n,]),
                          count(abd15[LBK&SC=="Positive"&MW==n,])+
                            count(abd15[LBK&SC=="POSITIVE"&MW==n,]),
                          count(abd15[LLoak&SC=="Positive"&MW==n,])+
                            count(abd15[LLoak&SC=="POSITIVE"&MW==n,]),
                          count(abd15[LoakP&SC=="Positive"&MW==n,])+
                            count(abd15[LoakP&SC=="POSITIVE"&MW==n,]),
                          count(abd15[LOPJ&SC=="Positive"&MW==n,])+
                            count(abd15[LOPJ&SC=="POSITIVE"&MW==n,]),
                          count(abd15[LourdesE&SC=="Positive"&MW==n,])+
                            count(abd15[LourdesE&SC=="POSITIVE"&MW==n,]),
                          count(abd15[LourdesL&SC=="Positive"&MW==n,])+
                            count(abd15[LourdesL&SC=="POSITIVE"&MW==n,]),
                          count(abd15[LourdesP&SC=="Positive"&MW==n,])+
                            count(abd15[LourdesP&SC=="POSITIVE"&MW==n,]),
                          count(abd15[Lualhati&SC=="Positive"&MW==n,])+
                            count(abd15[Lualhati&SC=="POSITIVE"&MW==n,]),
                          count(abd15[lucnab&SC=="Positive"&MW==n,])+
                            count(abd15[lucnab&SC=="POSITIVE"&MW==n,]),
                          count(abd15[MagsaysayPR&SC=="Positive"&MW==n,])+
                            count(abd15[MagsaysayPR&SC=="POSITIVE"&MW==n,]),
                          count(abd15[MagsaysayL&SC=="Positive"&MW==n,])+
                            count(abd15[MagsaysayL&SC=="POSITIVE"&MW==n,]),
                          count(abd15[MagsaysayU&SC=="Positive"&MW==n,])+
                            count(abd15[MagsaysayU&SC=="POSITIVE"&MW==n,]),
                          count(abd15[MalcolmSquarePerfectoJoseAbadSantos&SC=="Positive"&MW==n,])+
                            count(abd15[MalcolmSquarePerfectoJoseAbadSantos&SC=="POSITIVE"&MW==n,]),
                          count(abd15[MR&SC=="Positive"&MW==n,])+
                            count(abd15[MR&SC=="POSITIVE"&MW==n,]),
                          count(abd15[MarketSubdivisionUpper&SC=="Positive"&MW==n,])+
                            count(abd15[MarketSubdivisionUpper&SC=="POSITIVE"&MW==n,]),
                          count(abd15[MiddleQuezonHillSubdivsion&SC=="Positive"&MW==n,])+
                            count(abd15[MiddleQuezonHillSubdivsion&SC=="POSITIVE"&MW==n,]),
                          count(abd15[MCutOff&SC=="Positive"&MW==n,])+
                            count(abd15[MCutOff&SC=="POSITIVE"&MW==n,]),
                          count(abd15[MinesView&SC=="Positive"&MW==n,])+
                            count(abd15[MinesView&SC=="POSITIVE"&MW==n,]),
                          count(abd15[ModernSiteE&SC=="Positive"&MW==n,])+
                            count(abd15[ModernSiteE&SC=="POSITIVE"&MW==n,]),
                          count(abd15[ModernSiteW&SC=="Positive"&MW==n,])+
                            count(abd15[ModernSiteW&SC=="POSITIVE"&MW==n,]),
                          count(abd15[MRR&SC=="Positive"&MW==n,])+
                            count(abd15[MRR&SC=="POSITIVE"&MW==n,]),
                          count(abd15[NewLuc&SC=="Positive"&MW==n,])+
                            count(abd15[NewLuc&SC=="POSITIVE"&MW==n,]),
                          count(abd15[Outlook&SC=="Positive"&MW==n,])+
                            count(abd15[Outlook&SC=="POSITIVE"&MW==n,]),
                          count(abd15[Pacdal&SC=="Positive"&MW==n,])+
                            count(abd15[Pacdal&SC=="POSITIVE"&MW==n,]),
                          count(abd15[PadreB&SC=="Positive"&MW==n,])+
                            count(abd15[PadreB&SC=="POSITIVE"&MW==n,]),
                          count(abd15[PadreZ&SC=="Positive"&MW==n,])+
                            count(abd15[PadreZ&SC=="POSITIVE"&MW==n,]),
                          count(abd15[PU&SC=="Positive"&MW==n,])+
                            count(abd15[PU&SC=="POSITIVE"&MW==n,]),
                          count(abd15[PhilAm&SC=="Positive"&MW==n,])+
                            count(abd15[PhilAm&SC=="POSITIVE"&MW==n,]),
                          count(abd15[Pinget&SC=="Positive"&MW==n,])+
                            count(abd15[Pinget&SC=="POSITIVE"&MW==n,]),
                          count(abd15[PPP&SC=="Positive"&MW==n,])+
                            count(abd15[PPP&SC=="POSITIVE"&MW==n,]),
                          count(abd15[PisaoP&SC=="Positive"&MW==n,])+
                            count(abd15[PisaoP&SC=="POSITIVE"&MW==n,]),
                          count(abd15[Poliwes&SC=="Positive"&MW==n,])+
                            count(abd15[Poliwes&SC=="POSITIVE"&MW==n,]),
                          count(abd15[pucsusan&SC=="Positive"&MW==n,])+
                            count(abd15[pucsusan&SC=="POSITIVE"&MW==n,]),
                          count(abd15[QHP&SC=="Positive"&MW==n,])+
                            count(abd15[QHP&SC=="POSITIVE"&MW==n,]),
                          count(abd15[QHillU&SC=="Positive"&MW==n,])+
                            count(abd15[QHillU&SC=="POSITIVE"&MW==n,]),
                          count(abd15[UpperQM&SC=="Positive"&MW==n,])+
                            count(abd15[UpperQM&SC=="POSITIVE"&MW==n,]),
                          count(abd15[QHillE&SC=="Positive"&MW==n,])+
                            count(abd15[QHillE&SC=="POSITIVE"&MW==n,]),
                          count(abd15[QHL&SC=="Positive"&MW==n,])+
                            count(abd15[QHL&SC=="POSITIVE"&MW==n,]),
                          count(abd15[QHM&SC=="Positive"&MW==n,])+
                            count(abd15[QHM&SC=="POSITIVE"&MW==n,]),
                          count(abd15[QHillW&SC=="Positive"&MW==n,])+
                            count(abd15[QHillW&SC=="POSITIVE"&MW==n,]),
                          count(abd15[RizalMonumentArea&SC=="Positive"&MW==n,])+
                            count(abd15[RizalMonumentArea&SC=="POSITIVE"&MW==n,]),
                          count(abd15[RockQuarryLower&SC=="Positive"&MW==n,])+
                            count(abd15[RockQuarryLower&SC=="POSITIVE"&MW==n,]),
                          count(abd15[RockQuarryMiddle&SC=="Positive"&MW==n,])+
                            count(abd15[RockQuarryMiddle&SC=="POSITIVE"&MW==n,]),
                          count(abd15[RockQuarryUpper&SC=="Positive"&MW==n,])+
                            count(abd15[RockQuarryUpper&SC=="POSITIVE"&MW==n,]),
                          count(abd15[salud&SC=="Positive"&MW==n,])+
                            count(abd15[salud&SC=="POSITIVE"&MW==n,]),
                          count(abd15[SanAntonioVillage&SC=="Positive"&MW==n,])+
                            count(abd15[SanAntonioVillage&SC=="POSITIVE"&MW==n,]),
                          count(abd15[SanLuisVillage&SC=="Positive"&MW==n,])+
                            count(abd15[SanLuisVillage&SC=="POSITIVE"&MW==n,]),
                          count(abd15[SanRoqueVillage&SC=="Positive"&MW==n,])+
                            count(abd15[SanRoqueVillage&SC=="POSITIVE"&MW==n,]),
                          count(abd15[sanvic&SC=="Positive"&MW==n,])+
                            count(abd15[sanvic&SC=="POSITIVE"&MW==n,]),
                          count(abd15[SanCamp&SC=="Positive"&MW==n,])+
                            count(abd15[SanCamp&SC=="POSITIVE"&MW==n,]),
                          count(abd15[SanitaryCampSouth&SC=="Positive"&MW==n,])+
                            count(abd15[SanitaryCampSouth&SC=="POSITIVE"&MW==n,]),
                          count(abd15[STE&SC=="Positive"&MW==n,])+
                            count(abd15[STE&SC=="POSITIVE"&MW==n,]),
                          count(abd15[SantoR&SC=="Positive"&MW==n,])+
                            count(abd15[SantoR&SC=="POSITIVE"&MW==n,]),
                          count(abd15[STP&SC=="Positive"&MW==n,])+
                            count(abd15[STP&SC=="POSITIVE"&MW==n,]),
                          count(abd15[STsa&SC=="Positive"&MW==n,])+
                            count(abd15[STsa&SC=="POSITIVE"&MW==n,]),
                          count(abd15[ScoutB&SC=="Positive"&MW==n,])+
                            count(abd15[ScoutB&SC=="POSITIVE"&MW==n,]),
                          count(abd15[Session&SC=="Positive"&MW==n,])+
                            count(abd15[Session&SC=="POSITIVE"&MW==n,]),
                          count(abd15[Slaughter&SC=="Positive"&MW==n,])+
                            count(abd15[Slaughter&SC=="POSITIVE"&MW==n,]),
                          count(abd15[SLU&SC=="Positive"&MW==n,])+
                            count(abd15[SLU&SC=="POSITIVE"&MW==n,]),
                          count(abd15[SouthDrive&SC=="Positive"&MW==n,])+
                            count(abd15[SouthDrive&SC=="POSITIVE"&MW==n,]),
                          count(abd15[TeoAl&SC=="Positive"&MW==n,])+
                            count(abd15[TeoAl&SC=="POSITIVE"&MW==n,]),
                          count(abd15[tranco&SC=="Positive"&MW==n,])+
                            count(abd15[tranco&SC=="POSITIVE"&MW==n,]),
                          count(abd15[victoria&SC=="Positive"&MW==n,])+
                            count(abd15[victoria&SC=="POSITIVE"&MW==n,]),
                          count(abd15[STJos&SC=="Positive"&MW==n,])+
                            count(abd15[STJos&SC=="POSITIVE"&MW==n,])
  )
}

B=abd16$Barangay
SC=abd16$StoolCulture
MW=abd16$MorbidityWeek

Imelda = B=="IMELDA VILLAGE"
BakakengCentral = B=="BAKAKENG CENTRAL"
ABCR = B=="A. BONIFACIO-CAGUIOA-RIMANDO (ABCR)"
Ambiong = B=="AMBIONG"
BakakengNorth = B=="BAKAKENG NORTH"
Asin = B=="ASIN"
Balsigan = B=="BALSIGAN"
Bayan = B=="BAYAN PARK WEST (BAYAN PARK)"
BGH=B=="BGH COMPOUND"
Brookside = B=="BROOKSIDE"
Camdas = B=="CAMDAS SUBDIVISION"
c7=B=="CAMP 7"
c8=B=="CAMP 8"
ca=B=="CAMP ALLEN"
cf=B=="CAMPO FILIPINO"
ccv=B=="COUNTRY CLUB VILLAGE"
DPS=B=="DPS AREA"
enghill=B=="ENGINEER'S HILL"
fvill=B=="FAIRVIEW VILLAGE"
GLuna=B=="GENERAL LUNA, UPPER"
Gib=B=="GIBRALTAR"
GuisadC=B=="GUISAD CENTRAL"
GuisadS=B=="GUISAD SORONG"
HHL=B=="HAPPY HOMES (HAPPY HOMES-LUCBAN)"
kias=B=="KIAS"
LLoak=B=="LIWANAG-LOAKAN"
LoakP=B=="LOAKAN PROPER"
LOPJ=B=="LOPEZ JAENA"
lucnab=B=="LUCNAB"
MR=B=="MANUEL A. ROXAS"
MCutOff=B=="MILITARY CUT-OFF"
MRR=B=="MRR-QUEEN OF PEACE"
PadreZ=B=="PADRE ZAMORA"
Pinget=B=="PINGET"
PPP=B=="PINSAO PILOT PROJECT"
PisaoP=B=="PINSAO PROPER"
Poliwes=B=="POLIWES"
QHP=B=="QUEZON HILL PROPER"
QHL=B=="QUIRINO HILL, LOWER"
QHM=B=="QUIRINO HILL, MIDDLE"
salud=B=="SALUD MITRA"
sanvic=B=="SAN VICENTE"
SanCamp=B=="SANITARY CAMP, NORTH"
STE=B=="SANTA ESCOLASTICA"
STP=B=="SANTO TOMAS PROPER"&B=="STO. TOMAS PROPER"
STsa=B=="SANTO TOMAS SCHOOL AREA"
tranc=B=="TRANCOVILLE"
birac=B=="BIRAC"
AL=B=="APUGAN-LOAKAN"
AZKCO=B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")
ATAB=B==toupper("Alfonso Tabora")
AB=B==toupper("Andres Bonifacio (Lower Bokawkan)")
ATOK=B==toupper("Atok Trail")
auroraP=B==toupper("Aurora Hill Proper (Malvar-Sgt. Floresca)")
auroraN=B==toupper("Aurora Hill, North Central")
auroraS=B==toupper("Aurora Hill, South Central")
BL=B==toupper("Bagong Lipunan (Market Area)")
Bal=B==toupper("Bal-Marcoville (Marcoville)")
BPE=B==toupper("Bayan Park East")
BPV=B==toupper("Bayan Park Village")
Brookspoint=B==toupper("Brookspoint")
CabinetHill=B==toupper("Cabinet Hill-Teacher's Camp")
CityCC=B==toupper("City Camp Central")
CityCP=B==toupper("City Camp Proper")
DagisanL=B==toupper("Dagsian, Lower")
Cres=B==toupper("Cresencia Village")
DagisanU=B==toupper("Dagsian, Upper")
Dizon=B==toupper("Dizon Subdivision")
Domin=B==toupper("Dominican Hill-Mirador")
Dontogan=B=="DONTOGAN"
DPS=B=="DPS AREA"
Ferdinand=B==toupper("Ferdinand (Happy Homes-Campo Sioco)")
GabSi=B=="GABRIELLA SILANG"
GenE=B==toupper("General Emilio F. Aguinaldo (Quirino-Magsaysay, Lower)")
GenLL=B==toupper("General Luna, Lower")
GenLU=B==toupper("General Luna, Upper")
Greenwater=B==toupper("Greenwater Village")
HHol=B==toupper("Happy Hollow")
HHom=B==toupper("Happy Homes (Happy Homes-Lucban)")
HCC=B==toupper("Harrison-Claudio Carantes")
Hillside=B==toupper("Hillside")
HGhostE=B==toupper("Holy Ghost Extension")
HGhostP=B==toupper("Holy Ghost Proper")
Honeymoon=B==toupper("Honeymoon (Honeymoon-Holy Ghost)")
IMarcos=B==toupper("Imelda R. Marcos (La Salle)")
Imelda=B==toupper("Imelda Village")
Kabayanihan=B==toupper("Kabayanihan")
Kagitingan=B==toupper("Kagitingan")
KayangHill=B==toupper("Kayang-Hilltop")
KayangE=B==toupper("Kayang Extension")
LBK=B==toupper("Legarda-Burnham-Kisad")
LourdesE=B==toupper("Lourdes Subdivision Extension")
LourdesL=B==toupper("Lourdes Subdivision, Lower")
LourdesP=B==toupper("Lourdes Subdivision, Proper")
Lualhati=B==toupper("Lualhati")
MagsaysayPR=B==toupper("Magsaysay Private Road")
MagsaysayL=B==toupper("Magsaysay, Lower")
MagsaysayU=B==toupper("Magsaysay, Upper")
MalcolmSquarePerfectoJoseAbadSantos=B==toupper("Malcolm Square-Perfecto (Jose Abad Santos)")
MarketSubdivisionUpper=B==toupper("Market Subdivision, Upper")
MiddleQuezonHillSubdivsion=B==toupper("Middle Quezon Hill Subdivsiion(Quezon Hill Middle)")
MinesView=B==toupper("Mines View Park")
ModernSiteE=B==toupper("Modern Site, East")
ModernSiteW=B==toupper("Modern Site, West")
NewLuc=B==toupper("New Lucban")
Outlook=B==toupper("Outlook Drive")
Pacdal=B==toupper("Pacdal")
PadreB=B==toupper("Padre Burgos")
PadreZ=B==toupper("Padre Zamora")
PU=B==toupper("Palma-Urbano (Cariño-Palma)")
PhilAm=B==toupper("Phil-Am")
pucsusan=B==toupper("Pucsusan")
QHillU=B==toupper("Quezon Hill, Upper")
UpperQM=B==toupper("Quirino-Magsaysay, Upper (Upper QM)")
QHillE=B==toupper("Quirino Hill, East")
QHillW=B==toupper("Quirino Hill, West")
RizalMonumentArea=B==toupper("Rizal Monument Area")
RockQuarryLower=B==toupper("Rock Quarry, Lower")
RockQuarryMiddle=B==toupper("Rock Quarry, Middle")
RockQuarryUpper=B==toupper("Rock Quarry, Upper")
SanAntonioVillage=B==toupper("San Antonio Village")
SanLuisVillage=B==toupper("San Luis Village")
SanRoqueVillage=B==toupper("San Roque Village")
SanitaryCampSouth=B==toupper("Sanitary Camp, South")
SantoR=B==toupper("Santo Rosario")
ScoutB=B==toupper("Scout Barrio")
Session=B==toupper("Session Road Area")
Slaughter=B==toupper("Slaughter House Area (Santo Niño Slaughter)")
SLU=B==toupper("SLU-SVP Housing Village")
SouthDrive=B==toupper("South Drive")
TeoAl=B==toupper("Teodora Alonzo")
tranco=B==toupper("Trancoville")
victoria=B==toupper("Victoria Village")
STJos=B=="ST JOSEPH VILLAGE"

adf16 <- data.frame(MorbidityWeek=1,
                    ABonifacioCaguioaRimandoABCR=0,
                    AbanaoZanduetaKayongChugumOtekAZKCO=0,
                    AlfonsoTabora=0,
                    Ambiong=0,
                    AndresBonifacioLowerBokawkan=0,
                    ApuganLoakan=0,
                    AsinRoad=0,
                    AtokTrail=0,
                    AuroraHillProperMalvarSgtFloresca=0,
                    AuroraHillNorthCentral=0,
                    AuroraHillSouthCentral=0,
                    BagongLipunanMarketArea=0,
                    BakakengCentral=0,
                    BakakengNorth=1,
                    BalMarcovilleMarcoville=0,
                    Balsigan=0,
                    BayanParkEast=0,
                    BayanParkVillage=0,
                    BayanParkWestBayanPark=0,
                    BGHCompound=0,
                    Brookside=0,
                    Brookspoint=0,
                    CabinetHillTeachersCamp=0,
                    CamdasSubdivision=0,
                    Camp7=0,
                    Camp8=0,
                    CampAllen=0,
                    CampoFilipino=0,
                    CityCampCentral=0,
                    CityCampProper=0,
                    CountryClubVillage=0,
                    CresenciaVillage=0,
                    DagisanLower=0,
                    DagisanUpper=0,
                    DizonSubdivision=0,
                    DominicanHillMirador=0,
                    Dontogan=0,
                    DPSArea=0,
                    EngineersHill=0,
                    FairviewVillage=0,
                    FerdinandHappyHomesCampoSioco=0,
                    FortdelPilar=0,
                    GabrielaSilang=0,
                    GeneralEmilioFAguinaldoQuirinoMagsaysayLower=0,
                    GeneralLunaLower=0,
                    GeneralLunaUpper=0,
                    Gibraltar=0,
                    GreenwaterVillage=0,
                    GuisadCentral=0,
                    GuisadSorong=0,
                    HappyHollow=0,
                    HappyHomesHappyHomesLucban=0,
                    HarrisonClaudioCarantes=0,
                    Hillside=0,
                    HolyGhostExtension=0,
                    HolyGhostProper=0,
                    HoneymoonHoneymoonHolyGhost=0,
                    ImeldaRMarcosLaSalle=0,
                    ImeldaVillage=0,
                    Irisan=0,
                    Kabayanihan=0,
                    Kagitingan=0,
                    KayangHilltop=0,
                    KayangExtension=0,
                    Kias=0,
                    LegardaBurnhamKisad=0,
                    LiwanagLoakan=0,
                    LoakanProper=0,
                    LopezJaena=0,
                    LourdesSubdivisionExtension=0,
                    LourdesSubdivisionLower=0,
                    LourdesSubdivisionProper=0,
                    Lualhati=0,
                    Lucnab=0,
                    MagsaysayPrivateRoad=0,
                    MagsaysayLower=0,
                    MagsaysayUpper=0,
                    MalcolmSquarePerfectoJoseAbadSantos=0,
                    ManuelARoxas=0,
                    MarketSubdivisionUpper=0,
                    MiddleQuezonHillSubdivsion=0,
                    MilitaryCutoff=0,
                    MinesViewPark=0,
                    ModernSiteEast=0,
                    ModernSiteWest=0,
                    MRRQueenofPeace=0,
                    NewLucban=0,
                    OutlookDrive=0,
                    Pacdal=0,
                    PadreBurgos=0,
                    PadreZamora=0,
                    PalmaUrbanoCarinoPalma=0,
                    PhilAm=0,
                    Pinget=0,
                    PinsaoPilotProject=0,
                    PinsaoProper=0,
                    Poliwes=0,
                    Pucsusan=0,
                    QuezonHillProper=0,
                    QuezonHillUpper=0,
                    QuirinoMagsaysayUpperUpperQM=0,
                    QuirinoHillEast=0,
                    QuirinoHillLower=0,
                    QuirinoHillMiddle=0,
                    QuirinoHillWest=0,
                    RizalMonumentArea=0,
                    RockQuarryLower=0,
                    RockQuarryMiddle=0,
                    RockQuarryUpper=0,
                    SaludMitra=0,
                    SanAntoniVillage=0,
                    SanLuisVillage=0,
                    SanRoqueVillage=0,
                    SanVicente=0,
                    SanitaryCampNorth=0,
                    SanitaryCampSouth=0,
                    SantaEscolastica=0,
                    SantoRosario=0,
                    SantoTomasProper=0,
                    SantoTomasSchoolArea=0,
                    ScoutBarrio=0,
                    SessionRoadArea=0,
                    SlaughterHouseAreaSantoNiñoSlaughter=0,
                    SLUSVPHousingVillage=0,
                    SouthDrive=0,
                    TeodoraAlonzo=0,
                    Trancoville=0,
                    VictoriaVillage=0,
                    SaintJosephVillage=0
)

for (n in 2:52) {
  adf16[nrow(adf16)+1,]=c(n,
                          count(abd16[ABCR&SC=="Positive"&MW==n,])+
                            count(abd16[ABCR&SC=="POSITIVE"&MW==n,]),
                          count(abd16[B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")&SC=="Positive"&MW==n,])+
                            count(abd16[B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")&SC=="POSITIVE"&MW==n,]),
                          count(abd16[B==toupper("Alfonso Tabora")&SC=="Positive"&MW==n,])+
                            count(abd16[B==toupper("Alfonso Tabora")&SC=="POSITIVE"&MW==n,]),
                          count(abd16[B=="AMBIONG"&SC=="Positive"&MW==n,])+
                            count(abd16[B=="AMBIONG"&SC=="POSITIVE"&MW==n,]),
                          count(abd16[AB&SC=="Positive"&MW==n,])+
                            count(abd16[AB&SC=="POSITIVE"&MW==n,]),
                          count(abd16[AL&SC=="Positive"&MW==n,])+
                            count(abd16[AL&SC=="POSITIVE"&MW==n,]),
                          count(abd16[Asin&SC=="Positive"&MW==n,])+
                            count(abd16[Asin&SC=="POSITIVE"&MW==n,]),
                          count(abd16[ATOK&SC=="Positive"&MW==n,])+
                            count(abd16[ATOK&SC=="POSITIVE"&MW==n,]),
                          count(abd16[auroraP&SC=="Positive"&MW==n,])+
                            count(abd16[auroraP&SC=="POSITIVE"&MW==n,]),
                          count(abd16[auroraN&SC=="Positive"&MW==n,])+
                            count(abd16[auroraN&SC=="POSITIVE"&MW==n,]),
                          count(abd16[auroraS&SC=="POSITIVE"&MW==n,]),
                          count(abd16[BL&SC=="Positive"&MW==n,])+
                            count(abd16[BL&SC=="POSITIVE"&MW==n,]),
                          count(abd16[BakakengCentral&SC=="Positive"&MW==n,])+
                            count(abd16[BakakengCentral&SC=="POSITIVE"&MW==n,]),
                          count(abd16[BakakengNorth&SC=="Positive"&MW==n,])+
                            count(abd16[BakakengNorth&SC=="POSITIVE"&MW==n,]),
                          count(abd16[Bal&SC=="Positive"&MW==n,])+
                            count(abd16[Bal&SC=="POSITIVE"&MW==n,]),
                          count(abd16[Balsigan&SC=="Positive"&MW==n,])+
                            count(abd16[Balsigan&SC=="POSITIVE"&MW==n,]),
                          count(abd16[BPE&SC=="Positive"&MW==n,])+
                            count(abd16[BPE&SC=="POSITIVE"&MW==n,]),
                          count(abd16[BPV&SC=="Positive"&MW==n,])+
                            count(abd16[BPV&SC=="POSITIVE"&MW==n,]),
                          count(abd16[Bayan&SC=="Positive"&MW==n,])+
                            count(abd16[Bayan&SC=="POSITIVE"&MW==n,]),
                          count(abd16[BGH&SC=="Positive"&MW==n,])+
                            count(abd16[BGH&SC=="POSITIVE"&MW==n,]),
                          count(abd16[Brookside&SC=="Positive"&MW==n,])+
                            count(abd16[Brookside&SC=="POSITIVE"&MW==n,]),
                          count(abd16[Brookspoint&SC=="Positive"&MW==n,])+
                            count(abd16[Brookspoint&SC=="POSITIVE"&MW==n,]),
                          count(abd16[CabinetHill&SC=="Positive"&MW==n,])+
                            count(abd16[CabinetHill&SC=="POSITIVE"&MW==n,]),
                          count(abd16[Camdas&SC=="Positive"&MW==n,])+
                            count(abd16[Camdas&SC=="POSITIVE"&MW==n,]),
                          count(abd16[c7&SC=="Positive"&MW==n,])+
                            count(abd16[c7&SC=="POSITIVE"&MW==n,]),
                          count(abd16[c8&SC=="Positive"&MW==n,])+
                            count(abd16[c8&SC=="POSITIVE"&MW==n,]),
                          count(abd16[ca&SC=="Positive"&MW==n,])+
                            count(abd16[ca&SC=="POSITIVE"&MW==n,]),
                          count(abd16[cf&SC=="Positive"&MW==n,])+
                            count(abd16[cf&SC=="POSITIVE"&MW==n,]),
                          count(abd16[CityCC&SC=="Positive"&MW==n,])+
                            count(abd16[CityCC&SC=="POSITIVE"&MW==n,]),
                          count(abd16[CityCP&SC=="Positive"&MW==n,])+
                            count(abd16[CityCP&SC=="POSITIVE"&MW==n,]),
                          count(abd16[ccv&SC=="Positive"&MW==n,])+
                            count(abd16[ccv&SC=="POSITIVE"&MW==n,]),
                          count(abd16[Cres&SC=="Positive"&MW==n,])+
                            count(abd16[Cres&SC=="POSITIVE"&MW==n,]),
                          count(abd16[DagisanL&SC=="Positive"&MW==n,])+
                            count(abd16[DagisanL&SC=="POSITIVE"&MW==n,]),
                          count(abd16[DagisanU&SC=="Positive"&MW==n,])+
                            count(abd16[DagisanU&SC=="POSITIVE"&MW==n,]),
                          count(abd16[Dizon&SC=="Positive"&MW==n,])+
                            count(abd16[Dizon&SC=="POSITIVE"&MW==n,]),
                          count(abd16[Domin&SC=="Positive"&MW==n,])+
                            count(abd16[Domin&SC=="POSITIVE"&MW==n,]),
                          count(abd16[Dontogan&SC=="Positive"&MW==n,])+
                            count(abd16[Dontogan&SC=="POSITIVE"&MW==n,]),
                          count(abd16[DPS&SC=="Positive"&MW==n,])+
                            count(abd16[DPS&SC=="POSITIVE"&MW==n,]),
                          count(abd16[enghill&SC=="Positive"&MW==n,])+
                            count(abd16[enghill&SC=="POSITIVE"&MW==n,]),
                          count(abd16[fvill&SC=="Positive"&MW==n,])+
                            count(abd16[fvill&SC=="POSITIVE"&MW==n,]),
                          count(abd16[Ferdinand&SC=="Positive"&MW==n,])+
                            count(abd16[Ferdinand&SC=="POSITIVE"&MW==n,]),
                          count(abd16[B=="FORT DEL PILAR"&SC=="Positive"&MW==n,])+
                            count(abd16[B=="FORT DEL PILAR"&SC=="POSITIVE"&MW==n,]),
                          count(abd16[GabSi&SC=="Positive"&MW==n,])+
                            count(abd16[GabSi&SC=="POSITIVE"&MW==n,]),
                          count(abd16[GenE&SC=="Positive"&MW==n,])+
                            count(abd16[GenE&SC=="POSITIVE"&MW==n,]),
                          count(abd16[GenLL&SC=="Positive"&MW==n,])+
                            count(abd16[GenLL&SC=="POSITIVE"&MW==n,]),
                          count(abd16[GenLU&SC=="Positive"&MW==n,])+
                            count(abd16[GenLU&SC=="POSITIVE"&MW==n,]),
                          count(abd16[Gib&SC=="Positive"&MW==n,])+
                            count(abd16[Gib&SC=="POSITIVE"&MW==n,]),
                          count(abd16[Greenwater&SC=="Positive"&MW==n,])+
                            count(abd16[Greenwater&SC=="POSITIVE"&MW==n,]),
                          count(abd16[GuisadC&SC=="Positive"&MW==n,])+
                            count(abd16[GuisadC&SC=="POSITIVE"&MW==n,]),
                          count(abd16[GuisadS&SC=="Positive"&MW==n,])+
                            count(abd16[GuisadS&SC=="POSITIVE"&MW==n,]),
                          count(abd16[HHol&SC=="Positive"&MW==n,])+
                            count(abd16[HHol&SC=="POSITIVE"&MW==n,]),
                          count(abd16[HHom&SC=="Positive"&MW==n,])+
                            count(abd16[HHom&SC=="POSITIVE"&MW==n,]),
                          count(abd16[HCC&SC=="Positive"&MW==n,])+
                            count(abd16[HCC&SC=="POSITIVE"&MW==n,]),
                          count(abd16[Hillside&SC=="Positive"&MW==n,])+
                            count(abd16[Hillside&SC=="POSITIVE"&MW==n,]),
                          count(abd16[HGhostE&SC=="Positive"&MW==n,])+
                            count(abd16[HGhostE&SC=="POSITIVE"&MW==n,]),
                          count(abd16[HGhostP&SC=="Positive"&MW==n,])+
                            count(abd16[HGhostP&SC=="POSITIVE"&MW==n,]),
                          count(abd16[Honeymoon&SC=="Positive"&MW==n,])+
                            count(abd16[Honeymoon&SC=="POSITIVE"&MW==n,]),
                          count(abd16[IMarcos&SC=="Positive"&MW==n,])+
                            count(abd16[IMarcos&SC=="POSITIVE"&MW==n,]),
                          count(abd16[Imelda&SC=="Positive"&MW==n,])+
                            count(abd16[Imelda&SC=="POSITIVE"&MW==n,]),
                          count(abd16[B=="IRISAN"&SC=="Positive"&MW==n,])+
                            count(abd16[B=="IRISAN"&SC=="POSITIVE"&MW==n,]),
                          count(abd16[Kabayanihan&SC=="Positive"&MW==n,])+
                            count(abd16[Kabayanihan&SC=="POSITIVE"&MW==n,]),
                          count(abd16[Kagitingan&SC=="Positive"&MW==n,])+
                            count(abd16[Kagitingan&SC=="POSITIVE"&MW==n,]),
                          count(abd16[KayangHill&SC=="Positive"&MW==n,])+
                            count(abd16[KayangHill&SC=="POSITIVE"&MW==n,]),
                          count(abd16[KayangE&SC=="Positive"&MW==n,])+
                            count(abd16[KayangE&SC=="POSITIVE"&MW==n,]),
                          count(abd16[kias&SC=="Positive"&MW==n,])+
                            count(abd16[kias&SC=="POSITIVE"&MW==n,]),
                          count(abd16[LBK&SC=="Positive"&MW==n,])+
                            count(abd16[LBK&SC=="POSITIVE"&MW==n,]),
                          count(abd16[LLoak&SC=="Positive"&MW==n,])+
                            count(abd16[LLoak&SC=="POSITIVE"&MW==n,]),
                          count(abd16[LoakP&SC=="Positive"&MW==n,])+
                            count(abd16[LoakP&SC=="POSITIVE"&MW==n,]),
                          count(abd16[LOPJ&SC=="Positive"&MW==n,])+
                            count(abd16[LOPJ&SC=="POSITIVE"&MW==n,]),
                          count(abd16[LourdesE&SC=="Positive"&MW==n,])+
                            count(abd16[LourdesE&SC=="POSITIVE"&MW==n,]),
                          count(abd16[LourdesL&SC=="Positive"&MW==n,])+
                            count(abd16[LourdesL&SC=="POSITIVE"&MW==n,]),
                          count(abd16[LourdesP&SC=="Positive"&MW==n,])+
                            count(abd16[LourdesP&SC=="POSITIVE"&MW==n,]),
                          count(abd16[Lualhati&SC=="Positive"&MW==n,])+
                            count(abd16[Lualhati&SC=="POSITIVE"&MW==n,]),
                          count(abd16[lucnab&SC=="Positive"&MW==n,])+
                            count(abd16[lucnab&SC=="POSITIVE"&MW==n,]),
                          count(abd16[MagsaysayPR&SC=="Positive"&MW==n,])+
                            count(abd16[MagsaysayPR&SC=="POSITIVE"&MW==n,]),
                          count(abd16[MagsaysayL&SC=="Positive"&MW==n,])+
                            count(abd16[MagsaysayL&SC=="POSITIVE"&MW==n,]),
                          count(abd16[MagsaysayU&SC=="Positive"&MW==n,])+
                            count(abd16[MagsaysayU&SC=="POSITIVE"&MW==n,]),
                          count(abd16[MalcolmSquarePerfectoJoseAbadSantos&SC=="Positive"&MW==n,])+
                            count(abd16[MalcolmSquarePerfectoJoseAbadSantos&SC=="POSITIVE"&MW==n,]),
                          count(abd16[MR&SC=="Positive"&MW==n,])+
                            count(abd16[MR&SC=="POSITIVE"&MW==n,]),
                          count(abd16[MarketSubdivisionUpper&SC=="Positive"&MW==n,])+
                            count(abd16[MarketSubdivisionUpper&SC=="POSITIVE"&MW==n,]),
                          count(abd16[MiddleQuezonHillSubdivsion&SC=="Positive"&MW==n,])+
                            count(abd16[MiddleQuezonHillSubdivsion&SC=="POSITIVE"&MW==n,]),
                          count(abd16[MCutOff&SC=="Positive"&MW==n,])+
                            count(abd16[MCutOff&SC=="POSITIVE"&MW==n,]),
                          count(abd16[MinesView&SC=="Positive"&MW==n,])+
                            count(abd16[MinesView&SC=="POSITIVE"&MW==n,]),
                          count(abd16[ModernSiteE&SC=="Positive"&MW==n,])+
                            count(abd16[ModernSiteE&SC=="POSITIVE"&MW==n,]),
                          count(abd16[ModernSiteW&SC=="Positive"&MW==n,])+
                            count(abd16[ModernSiteW&SC=="POSITIVE"&MW==n,]),
                          count(abd16[MRR&SC=="Positive"&MW==n,])+
                            count(abd16[MRR&SC=="POSITIVE"&MW==n,]),
                          count(abd16[NewLuc&SC=="Positive"&MW==n,])+
                            count(abd16[NewLuc&SC=="POSITIVE"&MW==n,]),
                          count(abd16[Outlook&SC=="Positive"&MW==n,])+
                            count(abd16[Outlook&SC=="POSITIVE"&MW==n,]),
                          count(abd16[Pacdal&SC=="Positive"&MW==n,])+
                            count(abd16[Pacdal&SC=="POSITIVE"&MW==n,]),
                          count(abd16[PadreB&SC=="Positive"&MW==n,])+
                            count(abd16[PadreB&SC=="POSITIVE"&MW==n,]),
                          count(abd16[PadreZ&SC=="Positive"&MW==n,])+
                            count(abd16[PadreZ&SC=="POSITIVE"&MW==n,]),
                          count(abd16[PU&SC=="Positive"&MW==n,])+
                            count(abd16[PU&SC=="POSITIVE"&MW==n,]),
                          count(abd16[PhilAm&SC=="Positive"&MW==n,])+
                            count(abd16[PhilAm&SC=="POSITIVE"&MW==n,]),
                          count(abd16[Pinget&SC=="Positive"&MW==n,])+
                            count(abd16[Pinget&SC=="POSITIVE"&MW==n,]),
                          count(abd16[PPP&SC=="Positive"&MW==n,])+
                            count(abd16[PPP&SC=="POSITIVE"&MW==n,]),
                          count(abd16[PisaoP&SC=="Positive"&MW==n,])+
                            count(abd16[PisaoP&SC=="POSITIVE"&MW==n,]),
                          count(abd16[Poliwes&SC=="Positive"&MW==n,])+
                            count(abd16[Poliwes&SC=="POSITIVE"&MW==n,]),
                          count(abd16[pucsusan&SC=="Positive"&MW==n,])+
                            count(abd16[pucsusan&SC=="POSITIVE"&MW==n,]),
                          count(abd16[QHP&SC=="Positive"&MW==n,])+
                            count(abd16[QHP&SC=="POSITIVE"&MW==n,]),
                          count(abd16[QHillU&SC=="Positive"&MW==n,])+
                            count(abd16[QHillU&SC=="POSITIVE"&MW==n,]),
                          count(abd16[UpperQM&SC=="Positive"&MW==n,])+
                            count(abd16[UpperQM&SC=="POSITIVE"&MW==n,]),
                          count(abd16[QHillE&SC=="Positive"&MW==n,])+
                            count(abd16[QHillE&SC=="POSITIVE"&MW==n,]),
                          count(abd16[QHL&SC=="Positive"&MW==n,])+
                            count(abd16[QHL&SC=="POSITIVE"&MW==n,]),
                          count(abd16[QHM&SC=="Positive"&MW==n,])+
                            count(abd16[QHM&SC=="POSITIVE"&MW==n,]),
                          count(abd16[QHillW&SC=="Positive"&MW==n,])+
                            count(abd16[QHillW&SC=="POSITIVE"&MW==n,]),
                          count(abd16[RizalMonumentArea&SC=="Positive"&MW==n,])+
                            count(abd16[RizalMonumentArea&SC=="POSITIVE"&MW==n,]),
                          count(abd16[RockQuarryLower&SC=="Positive"&MW==n,])+
                            count(abd16[RockQuarryLower&SC=="POSITIVE"&MW==n,]),
                          count(abd16[RockQuarryMiddle&SC=="Positive"&MW==n,])+
                            count(abd16[RockQuarryMiddle&SC=="POSITIVE"&MW==n,]),
                          count(abd16[RockQuarryUpper&SC=="Positive"&MW==n,])+
                            count(abd16[RockQuarryUpper&SC=="POSITIVE"&MW==n,]),
                          count(abd16[salud&SC=="Positive"&MW==n,])+
                            count(abd16[salud&SC=="POSITIVE"&MW==n,]),
                          count(abd16[SanAntonioVillage&SC=="Positive"&MW==n,])+
                            count(abd16[SanAntonioVillage&SC=="POSITIVE"&MW==n,]),
                          count(abd16[SanLuisVillage&SC=="Positive"&MW==n,])+
                            count(abd16[SanLuisVillage&SC=="POSITIVE"&MW==n,]),
                          count(abd16[SanRoqueVillage&SC=="Positive"&MW==n,])+
                            count(abd16[SanRoqueVillage&SC=="POSITIVE"&MW==n,]),
                          count(abd16[sanvic&SC=="Positive"&MW==n,])+
                            count(abd16[sanvic&SC=="POSITIVE"&MW==n,]),
                          count(abd16[SanCamp&SC=="Positive"&MW==n,])+
                            count(abd16[SanCamp&SC=="POSITIVE"&MW==n,]),
                          count(abd16[SanitaryCampSouth&SC=="Positive"&MW==n,])+
                            count(abd16[SanitaryCampSouth&SC=="POSITIVE"&MW==n,]),
                          count(abd16[STE&SC=="Positive"&MW==n,])+
                            count(abd16[STE&SC=="POSITIVE"&MW==n,]),
                          count(abd16[SantoR&SC=="Positive"&MW==n,])+
                            count(abd16[SantoR&SC=="POSITIVE"&MW==n,]),
                          count(abd16[STP&SC=="Positive"&MW==n,])+
                            count(abd16[STP&SC=="POSITIVE"&MW==n,]),
                          count(abd16[STsa&SC=="Positive"&MW==n,])+
                            count(abd16[STsa&SC=="POSITIVE"&MW==n,]),
                          count(abd16[ScoutB&SC=="Positive"&MW==n,])+
                            count(abd16[ScoutB&SC=="POSITIVE"&MW==n,]),
                          count(abd16[Session&SC=="Positive"&MW==n,])+
                            count(abd16[Session&SC=="POSITIVE"&MW==n,]),
                          count(abd16[Slaughter&SC=="Positive"&MW==n,])+
                            count(abd16[Slaughter&SC=="POSITIVE"&MW==n,]),
                          count(abd16[SLU&SC=="Positive"&MW==n,])+
                            count(abd16[SLU&SC=="POSITIVE"&MW==n,]),
                          count(abd16[SouthDrive&SC=="Positive"&MW==n,])+
                            count(abd16[SouthDrive&SC=="POSITIVE"&MW==n,]),
                          count(abd16[TeoAl&SC=="Positive"&MW==n,])+
                            count(abd16[TeoAl&SC=="POSITIVE"&MW==n,]),
                          count(abd16[tranco&SC=="Positive"&MW==n,])+
                            count(abd16[tranco&SC=="POSITIVE"&MW==n,]),
                          count(abd16[victoria&SC=="Positive"&MW==n,])+
                            count(abd16[victoria&SC=="POSITIVE"&MW==n,]),
                          count(abd16[STJos&SC=="Positive"&MW==n,])+
                            count(abd16[STJos&SC=="POSITIVE"&MW==n,])
  )
}

B=abd17$Barangay
SC=abd17$StoolCulture
MW=abd17$MorbidityWeek

Imelda = B=="IMELDA VILLAGE"
BakakengCentral = B=="BAKAKENG CENTRAL"
ABCR = B=="A. BONIFACIO-CAGUIOA-RIMANDO (ABCR)"
Ambiong = B=="AMBIONG"
BakakengNorth = B=="BAKAKENG NORTH"
Asin = B=="ASIN"
Balsigan = B=="BALSIGAN"
Bayan = B=="BAYAN PARK WEST (BAYAN PARK)"
BGH=B=="BGH COMPOUND"
Brookside = B=="BROOKSIDE"
Camdas = B=="CAMDAS SUBDIVISION"
c7=B=="CAMP 7"
c8=B=="CAMP 8"
ca=B=="CAMP ALLEN"
cf=B=="CAMPO FILIPINO"
ccv=B=="COUNTRY CLUB VILLAGE"
DPS=B=="DPS AREA"
enghill=B=="ENGINEER'S HILL"
fvill=B=="FAIRVIEW VILLAGE"
GLuna=B=="GENERAL LUNA, UPPER"
Gib=B=="GIBRALTAR"
GuisadC=B=="GUISAD CENTRAL"
GuisadS=B=="GUISAD SORONG"
HHL=B=="HAPPY HOMES (HAPPY HOMES-LUCBAN)"
kias=B=="KIAS"
LLoak=B=="LIWANAG-LOAKAN"
LoakP=B=="LOAKAN PROPER"
LOPJ=B=="LOPEZ JAENA"
lucnab=B=="LUCNAB"
MR=B=="MANUEL A. ROXAS"
MCutOff=B=="MILITARY CUT-OFF"
MRR=B=="MRR-QUEEN OF PEACE"
PadreZ=B=="PADRE ZAMORA"
Pinget=B=="PINGET"
PPP=B=="PINSAO PILOT PROJECT"
PisaoP=B=="PINSAO PROPER"
Poliwes=B=="POLIWES"
QHP=B=="QUEZON HILL PROPER"
QHL=B=="QUIRINO HILL, LOWER"
QHM=B=="QUIRINO HILL, MIDDLE"
salud=B=="SALUD MITRA"
sanvic=B=="SAN VICENTE"
SanCamp=B=="SANITARY CAMP, NORTH"
STE=B=="SANTA ESCOLASTICA"
STP=B=="SANTO TOMAS PROPER"&B=="STO. TOMAS PROPER"
STsa=B=="SANTO TOMAS SCHOOL AREA"
tranc=B=="TRANCOVILLE"
birac=B=="BIRAC"
AL=B=="APUGAN-LOAKAN"
AZKCO=B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")
ATAB=B==toupper("Alfonso Tabora")
AB=B==toupper("Andres Bonifacio (Lower Bokawkan)")
ATOK=B==toupper("Atok Trail")
auroraP=B==toupper("Aurora Hill Proper (Malvar-Sgt. Floresca)")
auroraN=B==toupper("Aurora Hill, North Central")
auroraS=B==toupper("Aurora Hill, South Central")
BL=B==toupper("Bagong Lipunan (Market Area)")
Bal=B==toupper("Bal-Marcoville (Marcoville)")
BPE=B==toupper("Bayan Park East")
BPV=B==toupper("Bayan Park Village")
Brookspoint=B==toupper("Brookspoint")
CabinetHill=B==toupper("Cabinet Hill-Teacher's Camp")
CityCC=B==toupper("City Camp Central")
CityCP=B==toupper("City Camp Proper")
DagisanL=B==toupper("Dagsian, Lower")
Cres=B==toupper("Cresencia Village")
DagisanU=B==toupper("Dagsian, Upper")
Dizon=B==toupper("Dizon Subdivision")
Domin=B==toupper("Dominican Hill-Mirador")
Dontogan=B=="DONTOGAN"
DPS=B=="DPS AREA"
Ferdinand=B==toupper("Ferdinand (Happy Homes-Campo Sioco)")
GabSi=B=="GABRIELLA SILANG"
GenE=B==toupper("General Emilio F. Aguinaldo (Quirino-Magsaysay, Lower)")
GenLL=B==toupper("General Luna, Lower")
GenLU=B==toupper("General Luna, Upper")
Greenwater=B==toupper("Greenwater Village")
HHol=B==toupper("Happy Hollow")
HHom=B==toupper("Happy Homes (Happy Homes-Lucban)")
HCC=B==toupper("Harrison-Claudio Carantes")
Hillside=B==toupper("Hillside")
HGhostE=B==toupper("Holy Ghost Extension")
HGhostP=B==toupper("Holy Ghost Proper")
Honeymoon=B==toupper("Honeymoon (Honeymoon-Holy Ghost)")
IMarcos=B==toupper("Imelda R. Marcos (La Salle)")
Imelda=B==toupper("Imelda Village")
Kabayanihan=B==toupper("Kabayanihan")
Kagitingan=B==toupper("Kagitingan")
KayangHill=B==toupper("Kayang-Hilltop")
KayangE=B==toupper("Kayang Extension")
LBK=B==toupper("Legarda-Burnham-Kisad")
LourdesE=B==toupper("Lourdes Subdivision Extension")
LourdesL=B==toupper("Lourdes Subdivision, Lower")
LourdesP=B==toupper("Lourdes Subdivision, Proper")
Lualhati=B==toupper("Lualhati")
MagsaysayPR=B==toupper("Magsaysay Private Road")
MagsaysayL=B==toupper("Magsaysay, Lower")
MagsaysayU=B==toupper("Magsaysay, Upper")
MalcolmSquarePerfectoJoseAbadSantos=B==toupper("Malcolm Square-Perfecto (Jose Abad Santos)")
MarketSubdivisionUpper=B==toupper("Market Subdivision, Upper")
MiddleQuezonHillSubdivsion=B==toupper("Middle Quezon Hill Subdivsiion(Quezon Hill Middle)")
MinesView=B==toupper("Mines View Park")
ModernSiteE=B==toupper("Modern Site, East")
ModernSiteW=B==toupper("Modern Site, West")
NewLuc=B==toupper("New Lucban")
Outlook=B==toupper("Outlook Drive")
Pacdal=B==toupper("Pacdal")
PadreB=B==toupper("Padre Burgos")
PadreZ=B==toupper("Padre Zamora")
PU=B==toupper("Palma-Urbano (Cariño-Palma)")
PhilAm=B==toupper("Phil-Am")
pucsusan=B==toupper("Pucsusan")
QHillU=B==toupper("Quezon Hill, Upper")
UpperQM=B==toupper("Quirino-Magsaysay, Upper (Upper QM)")
QHillE=B==toupper("Quirino Hill, East")
QHillW=B==toupper("Quirino Hill, West")
RizalMonumentArea=B==toupper("Rizal Monument Area")
RockQuarryLower=B==toupper("Rock Quarry, Lower")
RockQuarryMiddle=B==toupper("Rock Quarry, Middle")
RockQuarryUpper=B==toupper("Rock Quarry, Upper")
SanAntonioVillage=B==toupper("San Antonio Village")
SanLuisVillage=B==toupper("San Luis Village")
SanRoqueVillage=B==toupper("San Roque Village")
SanitaryCampSouth=B==toupper("Sanitary Camp, South")
SantoR=B==toupper("Santo Rosario")
ScoutB=B==toupper("Scout Barrio")
Session=B==toupper("Session Road Area")
Slaughter=B==toupper("Slaughter House Area (Santo Niño Slaughter)")
SLU=B==toupper("SLU-SVP Housing Village")
SouthDrive=B==toupper("South Drive")
TeoAl=B==toupper("Teodora Alonzo")
tranco=B==toupper("Trancoville")
victoria=B==toupper("Victoria Village")
STJos=B=="ST JOSEPH VILLAGE"

adf17 <- data.frame(MorbidityWeek=1,
                    ABonifacioCaguioaRimandoABCR=0,
                    AbanaoZanduetaKayongChugumOtekAZKCO=0,
                    AlfonsoTabora=0,
                    Ambiong=0,
                    AndresBonifacioLowerBokawkan=0,
                    ApuganLoakan=0,
                    AsinRoad=0,
                    AtokTrail=0,
                    AuroraHillProperMalvarSgtFloresca=0,
                    AuroraHillNorthCentral=0,
                    AuroraHillSouthCentral=0,
                    BagongLipunanMarketArea=0,
                    BakakengCentral=0,
                    BakakengNorth=1,
                    BalMarcovilleMarcoville=0,
                    Balsigan=0,
                    BayanParkEast=0,
                    BayanParkVillage=0,
                    BayanParkWestBayanPark=0,
                    BGHCompound=0,
                    Brookside=0,
                    Brookspoint=0,
                    CabinetHillTeachersCamp=0,
                    CamdasSubdivision=0,
                    Camp7=0,
                    Camp8=0,
                    CampAllen=0,
                    CampoFilipino=0,
                    CityCampCentral=0,
                    CityCampProper=0,
                    CountryClubVillage=0,
                    CresenciaVillage=0,
                    DagisanLower=0,
                    DagisanUpper=0,
                    DizonSubdivision=0,
                    DominicanHillMirador=0,
                    Dontogan=0,
                    DPSArea=0,
                    EngineersHill=0,
                    FairviewVillage=0,
                    FerdinandHappyHomesCampoSioco=0,
                    FortdelPilar=0,
                    GabrielaSilang=0,
                    GeneralEmilioFAguinaldoQuirinoMagsaysayLower=0,
                    GeneralLunaLower=0,
                    GeneralLunaUpper=0,
                    Gibraltar=0,
                    GreenwaterVillage=0,
                    GuisadCentral=0,
                    GuisadSorong=0,
                    HappyHollow=0,
                    HappyHomesHappyHomesLucban=0,
                    HarrisonClaudioCarantes=0,
                    Hillside=0,
                    HolyGhostExtension=0,
                    HolyGhostProper=0,
                    HoneymoonHoneymoonHolyGhost=0,
                    ImeldaRMarcosLaSalle=0,
                    ImeldaVillage=0,
                    Irisan=0,
                    Kabayanihan=0,
                    Kagitingan=0,
                    KayangHilltop=0,
                    KayangExtension=0,
                    Kias=0,
                    LegardaBurnhamKisad=0,
                    LiwanagLoakan=0,
                    LoakanProper=0,
                    LopezJaena=0,
                    LourdesSubdivisionExtension=0,
                    LourdesSubdivisionLower=0,
                    LourdesSubdivisionProper=0,
                    Lualhati=0,
                    Lucnab=0,
                    MagsaysayPrivateRoad=0,
                    MagsaysayLower=0,
                    MagsaysayUpper=0,
                    MalcolmSquarePerfectoJoseAbadSantos=0,
                    ManuelARoxas=0,
                    MarketSubdivisionUpper=0,
                    MiddleQuezonHillSubdivsion=0,
                    MilitaryCutoff=0,
                    MinesViewPark=0,
                    ModernSiteEast=0,
                    ModernSiteWest=0,
                    MRRQueenofPeace=0,
                    NewLucban=0,
                    OutlookDrive=0,
                    Pacdal=0,
                    PadreBurgos=0,
                    PadreZamora=0,
                    PalmaUrbanoCarinoPalma=0,
                    PhilAm=0,
                    Pinget=0,
                    PinsaoPilotProject=0,
                    PinsaoProper=0,
                    Poliwes=0,
                    Pucsusan=0,
                    QuezonHillProper=0,
                    QuezonHillUpper=0,
                    QuirinoMagsaysayUpperUpperQM=0,
                    QuirinoHillEast=0,
                    QuirinoHillLower=0,
                    QuirinoHillMiddle=0,
                    QuirinoHillWest=0,
                    RizalMonumentArea=0,
                    RockQuarryLower=0,
                    RockQuarryMiddle=0,
                    RockQuarryUpper=0,
                    SaludMitra=0,
                    SanAntoniVillage=0,
                    SanLuisVillage=0,
                    SanRoqueVillage=0,
                    SanVicente=0,
                    SanitaryCampNorth=0,
                    SanitaryCampSouth=0,
                    SantaEscolastica=0,
                    SantoRosario=0,
                    SantoTomasProper=0,
                    SantoTomasSchoolArea=0,
                    ScoutBarrio=0,
                    SessionRoadArea=0,
                    SlaughterHouseAreaSantoNiñoSlaughter=0,
                    SLUSVPHousingVillage=0,
                    SouthDrive=0,
                    TeodoraAlonzo=0,
                    Trancoville=0,
                    VictoriaVillage=0,
                    SaintJosephVillage=0
)

for (n in 2:52) {
  adf17[nrow(adf17)+1,]=c(n,
                          count(abd17[ABCR&SC=="Positive"&MW==n,])+
                            count(abd17[ABCR&SC=="POSITIVE"&MW==n,]),
                          count(abd17[B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")&SC=="Positive"&MW==n,])+
                            count(abd17[B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")&SC=="POSITIVE"&MW==n,]),
                          count(abd17[B==toupper("Alfonso Tabora")&SC=="Positive"&MW==n,])+
                            count(abd17[B==toupper("Alfonso Tabora")&SC=="POSITIVE"&MW==n,]),
                          count(abd17[B=="AMBIONG"&SC=="Positive"&MW==n,])+
                            count(abd17[B=="AMBIONG"&SC=="POSITIVE"&MW==n,]),
                          count(abd17[AB&SC=="Positive"&MW==n,])+
                            count(abd17[AB&SC=="POSITIVE"&MW==n,]),
                          count(abd17[AL&SC=="Positive"&MW==n,])+
                            count(abd17[AL&SC=="POSITIVE"&MW==n,]),
                          count(abd17[Asin&SC=="Positive"&MW==n,])+
                            count(abd17[Asin&SC=="POSITIVE"&MW==n,]),
                          count(abd17[ATOK&SC=="Positive"&MW==n,])+
                            count(abd17[ATOK&SC=="POSITIVE"&MW==n,]),
                          count(abd17[auroraP&SC=="Positive"&MW==n,])+
                            count(abd17[auroraP&SC=="POSITIVE"&MW==n,]),
                          count(abd17[auroraN&SC=="Positive"&MW==n,])+
                            count(abd17[auroraN&SC=="POSITIVE"&MW==n,]),
                          count(abd17[auroraS&SC=="POSITIVE"&MW==n,]),
                          count(abd17[BL&SC=="Positive"&MW==n,])+
                            count(abd17[BL&SC=="POSITIVE"&MW==n,]),
                          count(abd17[BakakengCentral&SC=="Positive"&MW==n,])+
                            count(abd17[BakakengCentral&SC=="POSITIVE"&MW==n,]),
                          count(abd17[BakakengNorth&SC=="Positive"&MW==n,])+
                            count(abd17[BakakengNorth&SC=="POSITIVE"&MW==n,]),
                          count(abd17[Bal&SC=="Positive"&MW==n,])+
                            count(abd17[Bal&SC=="POSITIVE"&MW==n,]),
                          count(abd17[Balsigan&SC=="Positive"&MW==n,])+
                            count(abd17[Balsigan&SC=="POSITIVE"&MW==n,]),
                          count(abd17[BPE&SC=="Positive"&MW==n,])+
                            count(abd17[BPE&SC=="POSITIVE"&MW==n,]),
                          count(abd17[BPV&SC=="Positive"&MW==n,])+
                            count(abd17[BPV&SC=="POSITIVE"&MW==n,]),
                          count(abd17[Bayan&SC=="Positive"&MW==n,])+
                            count(abd17[Bayan&SC=="POSITIVE"&MW==n,]),
                          count(abd17[BGH&SC=="Positive"&MW==n,])+
                            count(abd17[BGH&SC=="POSITIVE"&MW==n,]),
                          count(abd17[Brookside&SC=="Positive"&MW==n,])+
                            count(abd17[Brookside&SC=="POSITIVE"&MW==n,]),
                          count(abd17[Brookspoint&SC=="Positive"&MW==n,])+
                            count(abd17[Brookspoint&SC=="POSITIVE"&MW==n,]),
                          count(abd17[CabinetHill&SC=="Positive"&MW==n,])+
                            count(abd17[CabinetHill&SC=="POSITIVE"&MW==n,]),
                          count(abd17[Camdas&SC=="Positive"&MW==n,])+
                            count(abd17[Camdas&SC=="POSITIVE"&MW==n,]),
                          count(abd17[c7&SC=="Positive"&MW==n,])+
                            count(abd17[c7&SC=="POSITIVE"&MW==n,]),
                          count(abd17[c8&SC=="Positive"&MW==n,])+
                            count(abd17[c8&SC=="POSITIVE"&MW==n,]),
                          count(abd17[ca&SC=="Positive"&MW==n,])+
                            count(abd17[ca&SC=="POSITIVE"&MW==n,]),
                          count(abd17[cf&SC=="Positive"&MW==n,])+
                            count(abd17[cf&SC=="POSITIVE"&MW==n,]),
                          count(abd17[CityCC&SC=="Positive"&MW==n,])+
                            count(abd17[CityCC&SC=="POSITIVE"&MW==n,]),
                          count(abd17[CityCP&SC=="Positive"&MW==n,])+
                            count(abd17[CityCP&SC=="POSITIVE"&MW==n,]),
                          count(abd17[ccv&SC=="Positive"&MW==n,])+
                            count(abd17[ccv&SC=="POSITIVE"&MW==n,]),
                          count(abd17[Cres&SC=="Positive"&MW==n,])+
                            count(abd17[Cres&SC=="POSITIVE"&MW==n,]),
                          count(abd17[DagisanL&SC=="Positive"&MW==n,])+
                            count(abd17[DagisanL&SC=="POSITIVE"&MW==n,]),
                          count(abd17[DagisanU&SC=="Positive"&MW==n,])+
                            count(abd17[DagisanU&SC=="POSITIVE"&MW==n,]),
                          count(abd17[Dizon&SC=="Positive"&MW==n,])+
                            count(abd17[Dizon&SC=="POSITIVE"&MW==n,]),
                          count(abd17[Domin&SC=="Positive"&MW==n,])+
                            count(abd17[Domin&SC=="POSITIVE"&MW==n,]),
                          count(abd17[Dontogan&SC=="Positive"&MW==n,])+
                            count(abd17[Dontogan&SC=="POSITIVE"&MW==n,]),
                          count(abd17[DPS&SC=="Positive"&MW==n,])+
                            count(abd17[DPS&SC=="POSITIVE"&MW==n,]),
                          count(abd17[enghill&SC=="Positive"&MW==n,])+
                            count(abd17[enghill&SC=="POSITIVE"&MW==n,]),
                          count(abd17[fvill&SC=="Positive"&MW==n,])+
                            count(abd17[fvill&SC=="POSITIVE"&MW==n,]),
                          count(abd17[Ferdinand&SC=="Positive"&MW==n,])+
                            count(abd17[Ferdinand&SC=="POSITIVE"&MW==n,]),
                          count(abd17[B=="FORT DEL PILAR"&SC=="Positive"&MW==n,])+
                            count(abd17[B=="FORT DEL PILAR"&SC=="POSITIVE"&MW==n,]),
                          count(abd17[GabSi&SC=="Positive"&MW==n,])+
                            count(abd17[GabSi&SC=="POSITIVE"&MW==n,]),
                          count(abd17[GenE&SC=="Positive"&MW==n,])+
                            count(abd17[GenE&SC=="POSITIVE"&MW==n,]),
                          count(abd17[GenLL&SC=="Positive"&MW==n,])+
                            count(abd17[GenLL&SC=="POSITIVE"&MW==n,]),
                          count(abd17[GenLU&SC=="Positive"&MW==n,])+
                            count(abd17[GenLU&SC=="POSITIVE"&MW==n,]),
                          count(abd17[Gib&SC=="Positive"&MW==n,])+
                            count(abd17[Gib&SC=="POSITIVE"&MW==n,]),
                          count(abd17[Greenwater&SC=="Positive"&MW==n,])+
                            count(abd17[Greenwater&SC=="POSITIVE"&MW==n,]),
                          count(abd17[GuisadC&SC=="Positive"&MW==n,])+
                            count(abd17[GuisadC&SC=="POSITIVE"&MW==n,]),
                          count(abd17[GuisadS&SC=="Positive"&MW==n,])+
                            count(abd17[GuisadS&SC=="POSITIVE"&MW==n,]),
                          count(abd17[HHol&SC=="Positive"&MW==n,])+
                            count(abd17[HHol&SC=="POSITIVE"&MW==n,]),
                          count(abd17[HHom&SC=="Positive"&MW==n,])+
                            count(abd17[HHom&SC=="POSITIVE"&MW==n,]),
                          count(abd17[HCC&SC=="Positive"&MW==n,])+
                            count(abd17[HCC&SC=="POSITIVE"&MW==n,]),
                          count(abd17[Hillside&SC=="Positive"&MW==n,])+
                            count(abd17[Hillside&SC=="POSITIVE"&MW==n,]),
                          count(abd17[HGhostE&SC=="Positive"&MW==n,])+
                            count(abd17[HGhostE&SC=="POSITIVE"&MW==n,]),
                          count(abd17[HGhostP&SC=="Positive"&MW==n,])+
                            count(abd17[HGhostP&SC=="POSITIVE"&MW==n,]),
                          count(abd17[Honeymoon&SC=="Positive"&MW==n,])+
                            count(abd17[Honeymoon&SC=="POSITIVE"&MW==n,]),
                          count(abd17[IMarcos&SC=="Positive"&MW==n,])+
                            count(abd17[IMarcos&SC=="POSITIVE"&MW==n,]),
                          count(abd17[Imelda&SC=="Positive"&MW==n,])+
                            count(abd17[Imelda&SC=="POSITIVE"&MW==n,]),
                          count(abd17[B=="IRISAN"&SC=="Positive"&MW==n,])+
                            count(abd17[B=="IRISAN"&SC=="POSITIVE"&MW==n,]),
                          count(abd17[Kabayanihan&SC=="Positive"&MW==n,])+
                            count(abd17[Kabayanihan&SC=="POSITIVE"&MW==n,]),
                          count(abd17[Kagitingan&SC=="Positive"&MW==n,])+
                            count(abd17[Kagitingan&SC=="POSITIVE"&MW==n,]),
                          count(abd17[KayangHill&SC=="Positive"&MW==n,])+
                            count(abd17[KayangHill&SC=="POSITIVE"&MW==n,]),
                          count(abd17[KayangE&SC=="Positive"&MW==n,])+
                            count(abd17[KayangE&SC=="POSITIVE"&MW==n,]),
                          count(abd17[kias&SC=="Positive"&MW==n,])+
                            count(abd17[kias&SC=="POSITIVE"&MW==n,]),
                          count(abd17[LBK&SC=="Positive"&MW==n,])+
                            count(abd17[LBK&SC=="POSITIVE"&MW==n,]),
                          count(abd17[LLoak&SC=="Positive"&MW==n,])+
                            count(abd17[LLoak&SC=="POSITIVE"&MW==n,]),
                          count(abd17[LoakP&SC=="Positive"&MW==n,])+
                            count(abd17[LoakP&SC=="POSITIVE"&MW==n,]),
                          count(abd17[LOPJ&SC=="Positive"&MW==n,])+
                            count(abd17[LOPJ&SC=="POSITIVE"&MW==n,]),
                          count(abd17[LourdesE&SC=="Positive"&MW==n,])+
                            count(abd17[LourdesE&SC=="POSITIVE"&MW==n,]),
                          count(abd17[LourdesL&SC=="Positive"&MW==n,])+
                            count(abd17[LourdesL&SC=="POSITIVE"&MW==n,]),
                          count(abd17[LourdesP&SC=="Positive"&MW==n,])+
                            count(abd17[LourdesP&SC=="POSITIVE"&MW==n,]),
                          count(abd17[Lualhati&SC=="Positive"&MW==n,])+
                            count(abd17[Lualhati&SC=="POSITIVE"&MW==n,]),
                          count(abd17[lucnab&SC=="Positive"&MW==n,])+
                            count(abd17[lucnab&SC=="POSITIVE"&MW==n,]),
                          count(abd17[MagsaysayPR&SC=="Positive"&MW==n,])+
                            count(abd17[MagsaysayPR&SC=="POSITIVE"&MW==n,]),
                          count(abd17[MagsaysayL&SC=="Positive"&MW==n,])+
                            count(abd17[MagsaysayL&SC=="POSITIVE"&MW==n,]),
                          count(abd17[MagsaysayU&SC=="Positive"&MW==n,])+
                            count(abd17[MagsaysayU&SC=="POSITIVE"&MW==n,]),
                          count(abd17[MalcolmSquarePerfectoJoseAbadSantos&SC=="Positive"&MW==n,])+
                            count(abd17[MalcolmSquarePerfectoJoseAbadSantos&SC=="POSITIVE"&MW==n,]),
                          count(abd17[MR&SC=="Positive"&MW==n,])+
                            count(abd17[MR&SC=="POSITIVE"&MW==n,]),
                          count(abd17[MarketSubdivisionUpper&SC=="Positive"&MW==n,])+
                            count(abd17[MarketSubdivisionUpper&SC=="POSITIVE"&MW==n,]),
                          count(abd17[MiddleQuezonHillSubdivsion&SC=="Positive"&MW==n,])+
                            count(abd17[MiddleQuezonHillSubdivsion&SC=="POSITIVE"&MW==n,]),
                          count(abd17[MCutOff&SC=="Positive"&MW==n,])+
                            count(abd17[MCutOff&SC=="POSITIVE"&MW==n,]),
                          count(abd17[MinesView&SC=="Positive"&MW==n,])+
                            count(abd17[MinesView&SC=="POSITIVE"&MW==n,]),
                          count(abd17[ModernSiteE&SC=="Positive"&MW==n,])+
                            count(abd17[ModernSiteE&SC=="POSITIVE"&MW==n,]),
                          count(abd17[ModernSiteW&SC=="Positive"&MW==n,])+
                            count(abd17[ModernSiteW&SC=="POSITIVE"&MW==n,]),
                          count(abd17[MRR&SC=="Positive"&MW==n,])+
                            count(abd17[MRR&SC=="POSITIVE"&MW==n,]),
                          count(abd17[NewLuc&SC=="Positive"&MW==n,])+
                            count(abd17[NewLuc&SC=="POSITIVE"&MW==n,]),
                          count(abd17[Outlook&SC=="Positive"&MW==n,])+
                            count(abd17[Outlook&SC=="POSITIVE"&MW==n,]),
                          count(abd17[Pacdal&SC=="Positive"&MW==n,])+
                            count(abd17[Pacdal&SC=="POSITIVE"&MW==n,]),
                          count(abd17[PadreB&SC=="Positive"&MW==n,])+
                            count(abd17[PadreB&SC=="POSITIVE"&MW==n,]),
                          count(abd17[PadreZ&SC=="Positive"&MW==n,])+
                            count(abd17[PadreZ&SC=="POSITIVE"&MW==n,]),
                          count(abd17[PU&SC=="Positive"&MW==n,])+
                            count(abd17[PU&SC=="POSITIVE"&MW==n,]),
                          count(abd17[PhilAm&SC=="Positive"&MW==n,])+
                            count(abd17[PhilAm&SC=="POSITIVE"&MW==n,]),
                          count(abd17[Pinget&SC=="Positive"&MW==n,])+
                            count(abd17[Pinget&SC=="POSITIVE"&MW==n,]),
                          count(abd17[PPP&SC=="Positive"&MW==n,])+
                            count(abd17[PPP&SC=="POSITIVE"&MW==n,]),
                          count(abd17[PisaoP&SC=="Positive"&MW==n,])+
                            count(abd17[PisaoP&SC=="POSITIVE"&MW==n,]),
                          count(abd17[Poliwes&SC=="Positive"&MW==n,])+
                            count(abd17[Poliwes&SC=="POSITIVE"&MW==n,]),
                          count(abd17[pucsusan&SC=="Positive"&MW==n,])+
                            count(abd17[pucsusan&SC=="POSITIVE"&MW==n,]),
                          count(abd17[QHP&SC=="Positive"&MW==n,])+
                            count(abd17[QHP&SC=="POSITIVE"&MW==n,]),
                          count(abd17[QHillU&SC=="Positive"&MW==n,])+
                            count(abd17[QHillU&SC=="POSITIVE"&MW==n,]),
                          count(abd17[UpperQM&SC=="Positive"&MW==n,])+
                            count(abd17[UpperQM&SC=="POSITIVE"&MW==n,]),
                          count(abd17[QHillE&SC=="Positive"&MW==n,])+
                            count(abd17[QHillE&SC=="POSITIVE"&MW==n,]),
                          count(abd17[QHL&SC=="Positive"&MW==n,])+
                            count(abd17[QHL&SC=="POSITIVE"&MW==n,]),
                          count(abd17[QHM&SC=="Positive"&MW==n,])+
                            count(abd17[QHM&SC=="POSITIVE"&MW==n,]),
                          count(abd17[QHillW&SC=="Positive"&MW==n,])+
                            count(abd17[QHillW&SC=="POSITIVE"&MW==n,]),
                          count(abd17[RizalMonumentArea&SC=="Positive"&MW==n,])+
                            count(abd17[RizalMonumentArea&SC=="POSITIVE"&MW==n,]),
                          count(abd17[RockQuarryLower&SC=="Positive"&MW==n,])+
                            count(abd17[RockQuarryLower&SC=="POSITIVE"&MW==n,]),
                          count(abd17[RockQuarryMiddle&SC=="Positive"&MW==n,])+
                            count(abd17[RockQuarryMiddle&SC=="POSITIVE"&MW==n,]),
                          count(abd17[RockQuarryUpper&SC=="Positive"&MW==n,])+
                            count(abd17[RockQuarryUpper&SC=="POSITIVE"&MW==n,]),
                          count(abd17[salud&SC=="Positive"&MW==n,])+
                            count(abd17[salud&SC=="POSITIVE"&MW==n,]),
                          count(abd17[SanAntonioVillage&SC=="Positive"&MW==n,])+
                            count(abd17[SanAntonioVillage&SC=="POSITIVE"&MW==n,]),
                          count(abd17[SanLuisVillage&SC=="Positive"&MW==n,])+
                            count(abd17[SanLuisVillage&SC=="POSITIVE"&MW==n,]),
                          count(abd17[SanRoqueVillage&SC=="Positive"&MW==n,])+
                            count(abd17[SanRoqueVillage&SC=="POSITIVE"&MW==n,]),
                          count(abd17[sanvic&SC=="Positive"&MW==n,])+
                            count(abd17[sanvic&SC=="POSITIVE"&MW==n,]),
                          count(abd17[SanCamp&SC=="Positive"&MW==n,])+
                            count(abd17[SanCamp&SC=="POSITIVE"&MW==n,]),
                          count(abd17[SanitaryCampSouth&SC=="Positive"&MW==n,])+
                            count(abd17[SanitaryCampSouth&SC=="POSITIVE"&MW==n,]),
                          count(abd17[STE&SC=="Positive"&MW==n,])+
                            count(abd17[STE&SC=="POSITIVE"&MW==n,]),
                          count(abd17[SantoR&SC=="Positive"&MW==n,])+
                            count(abd17[SantoR&SC=="POSITIVE"&MW==n,]),
                          count(abd17[STP&SC=="Positive"&MW==n,])+
                            count(abd17[STP&SC=="POSITIVE"&MW==n,]),
                          count(abd17[STsa&SC=="Positive"&MW==n,])+
                            count(abd17[STsa&SC=="POSITIVE"&MW==n,]),
                          count(abd17[ScoutB&SC=="Positive"&MW==n,])+
                            count(abd17[ScoutB&SC=="POSITIVE"&MW==n,]),
                          count(abd17[Session&SC=="Positive"&MW==n,])+
                            count(abd17[Session&SC=="POSITIVE"&MW==n,]),
                          count(abd17[Slaughter&SC=="Positive"&MW==n,])+
                            count(abd17[Slaughter&SC=="POSITIVE"&MW==n,]),
                          count(abd17[SLU&SC=="Positive"&MW==n,])+
                            count(abd17[SLU&SC=="POSITIVE"&MW==n,]),
                          count(abd17[SouthDrive&SC=="Positive"&MW==n,])+
                            count(abd17[SouthDrive&SC=="POSITIVE"&MW==n,]),
                          count(abd17[TeoAl&SC=="Positive"&MW==n,])+
                            count(abd17[TeoAl&SC=="POSITIVE"&MW==n,]),
                          count(abd17[tranco&SC=="Positive"&MW==n,])+
                            count(abd17[tranco&SC=="POSITIVE"&MW==n,]),
                          count(abd17[victoria&SC=="Positive"&MW==n,])+
                            count(abd17[victoria&SC=="POSITIVE"&MW==n,]),
                          count(abd17[STJos&SC=="Positive"&MW==n,])+
                            count(abd17[STJos&SC=="POSITIVE"&MW==n,])
  )
}

B=abd18$Barangay
SC=abd18$StoolCulture
MW=abd18$MorbidityWeek

Imelda = B=="IMELDA VILLAGE"
BakakengCentral = B=="BAKAKENG CENTRAL"
ABCR = B=="A. BONIFACIO-CAGUIOA-RIMANDO (ABCR)"
Ambiong = B=="AMBIONG"
BakakengNorth = B=="BAKAKENG NORTH"
Asin = B=="ASIN"
Balsigan = B=="BALSIGAN"
Bayan = B=="BAYAN PARK WEST (BAYAN PARK)"
BGH=B=="BGH COMPOUND"
Brookside = B=="BROOKSIDE"
Camdas = B=="CAMDAS SUBDIVISION"
c7=B=="CAMP 7"
c8=B=="CAMP 8"
ca=B=="CAMP ALLEN"
cf=B=="CAMPO FILIPINO"
ccv=B=="COUNTRY CLUB VILLAGE"
DPS=B=="DPS AREA"
enghill=B=="ENGINEER'S HILL"
fvill=B=="FAIRVIEW VILLAGE"
GLuna=B=="GENERAL LUNA, UPPER"
Gib=B=="GIBRALTAR"
GuisadC=B=="GUISAD CENTRAL"
GuisadS=B=="GUISAD SORONG"
HHL=B=="HAPPY HOMES (HAPPY HOMES-LUCBAN)"
kias=B=="KIAS"
LLoak=B=="LIWANAG-LOAKAN"
LoakP=B=="LOAKAN PROPER"
LOPJ=B=="LOPEZ JAENA"
lucnab=B=="LUCNAB"
MR=B=="MANUEL A. ROXAS"
MCutOff=B=="MILITARY CUT-OFF"
MRR=B=="MRR-QUEEN OF PEACE"
PadreZ=B=="PADRE ZAMORA"
Pinget=B=="PINGET"
PPP=B=="PINSAO PILOT PROJECT"
PisaoP=B=="PINSAO PROPER"
Poliwes=B=="POLIWES"
QHP=B=="QUEZON HILL PROPER"
QHL=B=="QUIRINO HILL, LOWER"
QHM=B=="QUIRINO HILL, MIDDLE"
salud=B=="SALUD MITRA"
sanvic=B=="SAN VICENTE"
SanCamp=B=="SANITARY CAMP, NORTH"
STE=B=="SANTA ESCOLASTICA"
STP=B=="SANTO TOMAS PROPER"&B=="STO. TOMAS PROPER"
STsa=B=="SANTO TOMAS SCHOOL AREA"
tranc=B=="TRANCOVILLE"
birac=B=="BIRAC"
AL=B=="APUGAN-LOAKAN"
AZKCO=B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")
ATAB=B==toupper("Alfonso Tabora")
AB=B==toupper("Andres Bonifacio (Lower Bokawkan)")
ATOK=B==toupper("Atok Trail")
auroraP=B==toupper("Aurora Hill Proper (Malvar-Sgt. Floresca)")
auroraN=B==toupper("Aurora Hill, North Central")
auroraS=B==toupper("Aurora Hill, South Central")
BL=B==toupper("Bagong Lipunan (Market Area)")
Bal=B==toupper("Bal-Marcoville (Marcoville)")
BPE=B==toupper("Bayan Park East")
BPV=B==toupper("Bayan Park Village")
Brookspoint=B==toupper("Brookspoint")
CabinetHill=B==toupper("Cabinet Hill-Teacher's Camp")
CityCC=B==toupper("City Camp Central")
CityCP=B==toupper("City Camp Proper")
DagisanL=B==toupper("Dagsian, Lower")
Cres=B==toupper("Cresencia Village")
DagisanU=B==toupper("Dagsian, Upper")
Dizon=B==toupper("Dizon Subdivision")
Domin=B==toupper("Dominican Hill-Mirador")
Dontogan=B=="DONTOGAN"
DPS=B=="DPS AREA"
Ferdinand=B==toupper("Ferdinand (Happy Homes-Campo Sioco)")
GabSi=B=="GABRIELLA SILANG"
GenE=B==toupper("General Emilio F. Aguinaldo (Quirino-Magsaysay, Lower)")
GenLL=B==toupper("General Luna, Lower")
GenLU=B==toupper("General Luna, Upper")
Greenwater=B==toupper("Greenwater Village")
HHol=B==toupper("Happy Hollow")
HHom=B==toupper("Happy Homes (Happy Homes-Lucban)")
HCC=B==toupper("Harrison-Claudio Carantes")
Hillside=B==toupper("Hillside")
HGhostE=B==toupper("Holy Ghost Extension")
HGhostP=B==toupper("Holy Ghost Proper")
Honeymoon=B==toupper("Honeymoon (Honeymoon-Holy Ghost)")
IMarcos=B==toupper("Imelda R. Marcos (La Salle)")
Imelda=B==toupper("Imelda Village")
Kabayanihan=B==toupper("Kabayanihan")
Kagitingan=B==toupper("Kagitingan")
KayangHill=B==toupper("Kayang-Hilltop")
KayangE=B==toupper("Kayang Extension")
LBK=B==toupper("Legarda-Burnham-Kisad")
LourdesE=B==toupper("Lourdes Subdivision Extension")
LourdesL=B==toupper("Lourdes Subdivision, Lower")
LourdesP=B==toupper("Lourdes Subdivision, Proper")
Lualhati=B==toupper("Lualhati")
MagsaysayPR=B==toupper("Magsaysay Private Road")
MagsaysayL=B==toupper("Magsaysay, Lower")
MagsaysayU=B==toupper("Magsaysay, Upper")
MalcolmSquarePerfectoJoseAbadSantos=B==toupper("Malcolm Square-Perfecto (Jose Abad Santos)")
MarketSubdivisionUpper=B==toupper("Market Subdivision, Upper")
MiddleQuezonHillSubdivsion=B==toupper("Middle Quezon Hill Subdivsiion(Quezon Hill Middle)")
MinesView=B==toupper("Mines View Park")
ModernSiteE=B==toupper("Modern Site, East")
ModernSiteW=B==toupper("Modern Site, West")
NewLuc=B==toupper("New Lucban")
Outlook=B==toupper("Outlook Drive")
Pacdal=B==toupper("Pacdal")
PadreB=B==toupper("Padre Burgos")
PadreZ=B==toupper("Padre Zamora")
PU=B==toupper("Palma-Urbano (Cariño-Palma)")
PhilAm=B==toupper("Phil-Am")
pucsusan=B==toupper("Pucsusan")
QHillU=B==toupper("Quezon Hill, Upper")
UpperQM=B==toupper("Quirino-Magsaysay, Upper (Upper QM)")
QHillE=B==toupper("Quirino Hill, East")
QHillW=B==toupper("Quirino Hill, West")
RizalMonumentArea=B==toupper("Rizal Monument Area")
RockQuarryLower=B==toupper("Rock Quarry, Lower")
RockQuarryMiddle=B==toupper("Rock Quarry, Middle")
RockQuarryUpper=B==toupper("Rock Quarry, Upper")
SanAntonioVillage=B==toupper("San Antonio Village")
SanLuisVillage=B==toupper("San Luis Village")
SanRoqueVillage=B==toupper("San Roque Village")
SanitaryCampSouth=B==toupper("Sanitary Camp, South")
SantoR=B==toupper("Santo Rosario")
ScoutB=B==toupper("Scout Barrio")
Session=B==toupper("Session Road Area")
Slaughter=B==toupper("Slaughter House Area (Santo Niño Slaughter)")
SLU=B==toupper("SLU-SVP Housing Village")
SouthDrive=B==toupper("South Drive")
TeoAl=B==toupper("Teodora Alonzo")
tranco=B==toupper("Trancoville")
victoria=B==toupper("Victoria Village")
STJos=B=="ST JOSEPH VILLAGE"

adf18 <- data.frame(MorbidityWeek=1,
                    ABonifacioCaguioaRimandoABCR=0,
                    AbanaoZanduetaKayongChugumOtekAZKCO=0,
                    AlfonsoTabora=0,
                    Ambiong=0,
                    AndresBonifacioLowerBokawkan=0,
                    ApuganLoakan=0,
                    AsinRoad=0,
                    AtokTrail=0,
                    AuroraHillProperMalvarSgtFloresca=0,
                    AuroraHillNorthCentral=0,
                    AuroraHillSouthCentral=0,
                    BagongLipunanMarketArea=0,
                    BakakengCentral=0,
                    BakakengNorth=1,
                    BalMarcovilleMarcoville=0,
                    Balsigan=0,
                    BayanParkEast=0,
                    BayanParkVillage=0,
                    BayanParkWestBayanPark=0,
                    BGHCompound=0,
                    Brookside=0,
                    Brookspoint=0,
                    CabinetHillTeachersCamp=0,
                    CamdasSubdivision=0,
                    Camp7=0,
                    Camp8=0,
                    CampAllen=0,
                    CampoFilipino=0,
                    CityCampCentral=0,
                    CityCampProper=0,
                    CountryClubVillage=0,
                    CresenciaVillage=0,
                    DagisanLower=0,
                    DagisanUpper=0,
                    DizonSubdivision=0,
                    DominicanHillMirador=0,
                    Dontogan=0,
                    DPSArea=0,
                    EngineersHill=0,
                    FairviewVillage=0,
                    FerdinandHappyHomesCampoSioco=0,
                    FortdelPilar=0,
                    GabrielaSilang=0,
                    GeneralEmilioFAguinaldoQuirinoMagsaysayLower=0,
                    GeneralLunaLower=0,
                    GeneralLunaUpper=0,
                    Gibraltar=0,
                    GreenwaterVillage=0,
                    GuisadCentral=0,
                    GuisadSorong=0,
                    HappyHollow=0,
                    HappyHomesHappyHomesLucban=0,
                    HarrisonClaudioCarantes=0,
                    Hillside=0,
                    HolyGhostExtension=0,
                    HolyGhostProper=0,
                    HoneymoonHoneymoonHolyGhost=0,
                    ImeldaRMarcosLaSalle=0,
                    ImeldaVillage=0,
                    Irisan=0,
                    Kabayanihan=0,
                    Kagitingan=0,
                    KayangHilltop=0,
                    KayangExtension=0,
                    Kias=0,
                    LegardaBurnhamKisad=0,
                    LiwanagLoakan=0,
                    LoakanProper=0,
                    LopezJaena=0,
                    LourdesSubdivisionExtension=0,
                    LourdesSubdivisionLower=0,
                    LourdesSubdivisionProper=0,
                    Lualhati=0,
                    Lucnab=0,
                    MagsaysayPrivateRoad=0,
                    MagsaysayLower=0,
                    MagsaysayUpper=0,
                    MalcolmSquarePerfectoJoseAbadSantos=0,
                    ManuelARoxas=0,
                    MarketSubdivisionUpper=0,
                    MiddleQuezonHillSubdivsion=0,
                    MilitaryCutoff=0,
                    MinesViewPark=0,
                    ModernSiteEast=0,
                    ModernSiteWest=0,
                    MRRQueenofPeace=0,
                    NewLucban=0,
                    OutlookDrive=0,
                    Pacdal=0,
                    PadreBurgos=0,
                    PadreZamora=0,
                    PalmaUrbanoCarinoPalma=0,
                    PhilAm=0,
                    Pinget=0,
                    PinsaoPilotProject=0,
                    PinsaoProper=0,
                    Poliwes=0,
                    Pucsusan=0,
                    QuezonHillProper=0,
                    QuezonHillUpper=0,
                    QuirinoMagsaysayUpperUpperQM=0,
                    QuirinoHillEast=0,
                    QuirinoHillLower=0,
                    QuirinoHillMiddle=0,
                    QuirinoHillWest=0,
                    RizalMonumentArea=0,
                    RockQuarryLower=0,
                    RockQuarryMiddle=0,
                    RockQuarryUpper=0,
                    SaludMitra=0,
                    SanAntoniVillage=0,
                    SanLuisVillage=0,
                    SanRoqueVillage=0,
                    SanVicente=0,
                    SanitaryCampNorth=0,
                    SanitaryCampSouth=0,
                    SantaEscolastica=0,
                    SantoRosario=0,
                    SantoTomasProper=0,
                    SantoTomasSchoolArea=0,
                    ScoutBarrio=0,
                    SessionRoadArea=0,
                    SlaughterHouseAreaSantoNiñoSlaughter=0,
                    SLUSVPHousingVillage=0,
                    SouthDrive=0,
                    TeodoraAlonzo=0,
                    Trancoville=0,
                    VictoriaVillage=0,
                    SaintJosephVillage=0
)

for (n in 2:52) {
  adf18[nrow(adf18)+1,]=c(n,
                          count(abd18[ABCR&SC=="Positive"&MW==n,])+
                            count(abd18[ABCR&SC=="POSITIVE"&MW==n,]),
                          count(abd18[B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")&SC=="Positive"&MW==n,])+
                            count(abd18[B==toupper("Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)")&SC=="POSITIVE"&MW==n,]),
                          count(abd18[B==toupper("Alfonso Tabora")&SC=="Positive"&MW==n,])+
                            count(abd18[B==toupper("Alfonso Tabora")&SC=="POSITIVE"&MW==n,]),
                          count(abd18[B=="AMBIONG"&SC=="Positive"&MW==n,])+
                            count(abd18[B=="AMBIONG"&SC=="POSITIVE"&MW==n,]),
                          count(abd18[AB&SC=="Positive"&MW==n,])+
                            count(abd18[AB&SC=="POSITIVE"&MW==n,]),
                          count(abd18[AL&SC=="Positive"&MW==n,])+
                            count(abd18[AL&SC=="POSITIVE"&MW==n,]),
                          count(abd18[Asin&SC=="Positive"&MW==n,])+
                            count(abd18[Asin&SC=="POSITIVE"&MW==n,]),
                          count(abd18[ATOK&SC=="Positive"&MW==n,])+
                            count(abd18[ATOK&SC=="POSITIVE"&MW==n,]),
                          count(abd18[auroraP&SC=="Positive"&MW==n,])+
                            count(abd18[auroraP&SC=="POSITIVE"&MW==n,]),
                          count(abd18[auroraN&SC=="Positive"&MW==n,])+
                            count(abd18[auroraN&SC=="POSITIVE"&MW==n,]),
                          count(abd18[auroraS&SC=="POSITIVE"&MW==n,]),
                          count(abd18[BL&SC=="Positive"&MW==n,])+
                            count(abd18[BL&SC=="POSITIVE"&MW==n,]),
                          count(abd18[BakakengCentral&SC=="Positive"&MW==n,])+
                            count(abd18[BakakengCentral&SC=="POSITIVE"&MW==n,]),
                          count(abd18[BakakengNorth&SC=="Positive"&MW==n,])+
                            count(abd18[BakakengNorth&SC=="POSITIVE"&MW==n,]),
                          count(abd18[Bal&SC=="Positive"&MW==n,])+
                            count(abd18[Bal&SC=="POSITIVE"&MW==n,]),
                          count(abd18[Balsigan&SC=="Positive"&MW==n,])+
                            count(abd18[Balsigan&SC=="POSITIVE"&MW==n,]),
                          count(abd18[BPE&SC=="Positive"&MW==n,])+
                            count(abd18[BPE&SC=="POSITIVE"&MW==n,]),
                          count(abd18[BPV&SC=="Positive"&MW==n,])+
                            count(abd18[BPV&SC=="POSITIVE"&MW==n,]),
                          count(abd18[Bayan&SC=="Positive"&MW==n,])+
                            count(abd18[Bayan&SC=="POSITIVE"&MW==n,]),
                          count(abd18[BGH&SC=="Positive"&MW==n,])+
                            count(abd18[BGH&SC=="POSITIVE"&MW==n,]),
                          count(abd18[Brookside&SC=="Positive"&MW==n,])+
                            count(abd18[Brookside&SC=="POSITIVE"&MW==n,]),
                          count(abd18[Brookspoint&SC=="Positive"&MW==n,])+
                            count(abd18[Brookspoint&SC=="POSITIVE"&MW==n,]),
                          count(abd18[CabinetHill&SC=="Positive"&MW==n,])+
                            count(abd18[CabinetHill&SC=="POSITIVE"&MW==n,]),
                          count(abd18[Camdas&SC=="Positive"&MW==n,])+
                            count(abd18[Camdas&SC=="POSITIVE"&MW==n,]),
                          count(abd18[c7&SC=="Positive"&MW==n,])+
                            count(abd18[c7&SC=="POSITIVE"&MW==n,]),
                          count(abd18[c8&SC=="Positive"&MW==n,])+
                            count(abd18[c8&SC=="POSITIVE"&MW==n,]),
                          count(abd18[ca&SC=="Positive"&MW==n,])+
                            count(abd18[ca&SC=="POSITIVE"&MW==n,]),
                          count(abd18[cf&SC=="Positive"&MW==n,])+
                            count(abd18[cf&SC=="POSITIVE"&MW==n,]),
                          count(abd18[CityCC&SC=="Positive"&MW==n,])+
                            count(abd18[CityCC&SC=="POSITIVE"&MW==n,]),
                          count(abd18[CityCP&SC=="Positive"&MW==n,])+
                            count(abd18[CityCP&SC=="POSITIVE"&MW==n,]),
                          count(abd18[ccv&SC=="Positive"&MW==n,])+
                            count(abd18[ccv&SC=="POSITIVE"&MW==n,]),
                          count(abd18[Cres&SC=="Positive"&MW==n,])+
                            count(abd18[Cres&SC=="POSITIVE"&MW==n,]),
                          count(abd18[DagisanL&SC=="Positive"&MW==n,])+
                            count(abd18[DagisanL&SC=="POSITIVE"&MW==n,]),
                          count(abd18[DagisanU&SC=="Positive"&MW==n,])+
                            count(abd18[DagisanU&SC=="POSITIVE"&MW==n,]),
                          count(abd18[Dizon&SC=="Positive"&MW==n,])+
                            count(abd18[Dizon&SC=="POSITIVE"&MW==n,]),
                          count(abd18[Domin&SC=="Positive"&MW==n,])+
                            count(abd18[Domin&SC=="POSITIVE"&MW==n,]),
                          count(abd18[Dontogan&SC=="Positive"&MW==n,])+
                            count(abd18[Dontogan&SC=="POSITIVE"&MW==n,]),
                          count(abd18[DPS&SC=="Positive"&MW==n,])+
                            count(abd18[DPS&SC=="POSITIVE"&MW==n,]),
                          count(abd18[enghill&SC=="Positive"&MW==n,])+
                            count(abd18[enghill&SC=="POSITIVE"&MW==n,]),
                          count(abd18[fvill&SC=="Positive"&MW==n,])+
                            count(abd18[fvill&SC=="POSITIVE"&MW==n,]),
                          count(abd18[Ferdinand&SC=="Positive"&MW==n,])+
                            count(abd18[Ferdinand&SC=="POSITIVE"&MW==n,]),
                          count(abd18[B=="FORT DEL PILAR"&SC=="Positive"&MW==n,])+
                            count(abd18[B=="FORT DEL PILAR"&SC=="POSITIVE"&MW==n,]),
                          count(abd18[GabSi&SC=="Positive"&MW==n,])+
                            count(abd18[GabSi&SC=="POSITIVE"&MW==n,]),
                          count(abd18[GenE&SC=="Positive"&MW==n,])+
                            count(abd18[GenE&SC=="POSITIVE"&MW==n,]),
                          count(abd18[GenLL&SC=="Positive"&MW==n,])+
                            count(abd18[GenLL&SC=="POSITIVE"&MW==n,]),
                          count(abd18[GenLU&SC=="Positive"&MW==n,])+
                            count(abd18[GenLU&SC=="POSITIVE"&MW==n,]),
                          count(abd18[Gib&SC=="Positive"&MW==n,])+
                            count(abd18[Gib&SC=="POSITIVE"&MW==n,]),
                          count(abd18[Greenwater&SC=="Positive"&MW==n,])+
                            count(abd18[Greenwater&SC=="POSITIVE"&MW==n,]),
                          count(abd18[GuisadC&SC=="Positive"&MW==n,])+
                            count(abd18[GuisadC&SC=="POSITIVE"&MW==n,]),
                          count(abd18[GuisadS&SC=="Positive"&MW==n,])+
                            count(abd18[GuisadS&SC=="POSITIVE"&MW==n,]),
                          count(abd18[HHol&SC=="Positive"&MW==n,])+
                            count(abd18[HHol&SC=="POSITIVE"&MW==n,]),
                          count(abd18[HHom&SC=="Positive"&MW==n,])+
                            count(abd18[HHom&SC=="POSITIVE"&MW==n,]),
                          count(abd18[HCC&SC=="Positive"&MW==n,])+
                            count(abd18[HCC&SC=="POSITIVE"&MW==n,]),
                          count(abd18[Hillside&SC=="Positive"&MW==n,])+
                            count(abd18[Hillside&SC=="POSITIVE"&MW==n,]),
                          count(abd18[HGhostE&SC=="Positive"&MW==n,])+
                            count(abd18[HGhostE&SC=="POSITIVE"&MW==n,]),
                          count(abd18[HGhostP&SC=="Positive"&MW==n,])+
                            count(abd18[HGhostP&SC=="POSITIVE"&MW==n,]),
                          count(abd18[Honeymoon&SC=="Positive"&MW==n,])+
                            count(abd18[Honeymoon&SC=="POSITIVE"&MW==n,]),
                          count(abd18[IMarcos&SC=="Positive"&MW==n,])+
                            count(abd18[IMarcos&SC=="POSITIVE"&MW==n,]),
                          count(abd18[Imelda&SC=="Positive"&MW==n,])+
                            count(abd18[Imelda&SC=="POSITIVE"&MW==n,]),
                          count(abd18[B=="IRISAN"&SC=="Positive"&MW==n,])+
                            count(abd18[B=="IRISAN"&SC=="POSITIVE"&MW==n,]),
                          count(abd18[Kabayanihan&SC=="Positive"&MW==n,])+
                            count(abd18[Kabayanihan&SC=="POSITIVE"&MW==n,]),
                          count(abd18[Kagitingan&SC=="Positive"&MW==n,])+
                            count(abd18[Kagitingan&SC=="POSITIVE"&MW==n,]),
                          count(abd18[KayangHill&SC=="Positive"&MW==n,])+
                            count(abd18[KayangHill&SC=="POSITIVE"&MW==n,]),
                          count(abd18[KayangE&SC=="Positive"&MW==n,])+
                            count(abd18[KayangE&SC=="POSITIVE"&MW==n,]),
                          count(abd18[kias&SC=="Positive"&MW==n,])+
                            count(abd18[kias&SC=="POSITIVE"&MW==n,]),
                          count(abd18[LBK&SC=="Positive"&MW==n,])+
                            count(abd18[LBK&SC=="POSITIVE"&MW==n,]),
                          count(abd18[LLoak&SC=="Positive"&MW==n,])+
                            count(abd18[LLoak&SC=="POSITIVE"&MW==n,]),
                          count(abd18[LoakP&SC=="Positive"&MW==n,])+
                            count(abd18[LoakP&SC=="POSITIVE"&MW==n,]),
                          count(abd18[LOPJ&SC=="Positive"&MW==n,])+
                            count(abd18[LOPJ&SC=="POSITIVE"&MW==n,]),
                          count(abd18[LourdesE&SC=="Positive"&MW==n,])+
                            count(abd18[LourdesE&SC=="POSITIVE"&MW==n,]),
                          count(abd18[LourdesL&SC=="Positive"&MW==n,])+
                            count(abd18[LourdesL&SC=="POSITIVE"&MW==n,]),
                          count(abd18[LourdesP&SC=="Positive"&MW==n,])+
                            count(abd18[LourdesP&SC=="POSITIVE"&MW==n,]),
                          count(abd18[Lualhati&SC=="Positive"&MW==n,])+
                            count(abd18[Lualhati&SC=="POSITIVE"&MW==n,]),
                          count(abd18[lucnab&SC=="Positive"&MW==n,])+
                            count(abd18[lucnab&SC=="POSITIVE"&MW==n,]),
                          count(abd18[MagsaysayPR&SC=="Positive"&MW==n,])+
                            count(abd18[MagsaysayPR&SC=="POSITIVE"&MW==n,]),
                          count(abd18[MagsaysayL&SC=="Positive"&MW==n,])+
                            count(abd18[MagsaysayL&SC=="POSITIVE"&MW==n,]),
                          count(abd18[MagsaysayU&SC=="Positive"&MW==n,])+
                            count(abd18[MagsaysayU&SC=="POSITIVE"&MW==n,]),
                          count(abd18[MalcolmSquarePerfectoJoseAbadSantos&SC=="Positive"&MW==n,])+
                            count(abd18[MalcolmSquarePerfectoJoseAbadSantos&SC=="POSITIVE"&MW==n,]),
                          count(abd18[MR&SC=="Positive"&MW==n,])+
                            count(abd18[MR&SC=="POSITIVE"&MW==n,]),
                          count(abd18[MarketSubdivisionUpper&SC=="Positive"&MW==n,])+
                            count(abd18[MarketSubdivisionUpper&SC=="POSITIVE"&MW==n,]),
                          count(abd18[MiddleQuezonHillSubdivsion&SC=="Positive"&MW==n,])+
                            count(abd18[MiddleQuezonHillSubdivsion&SC=="POSITIVE"&MW==n,]),
                          count(abd18[MCutOff&SC=="Positive"&MW==n,])+
                            count(abd18[MCutOff&SC=="POSITIVE"&MW==n,]),
                          count(abd18[MinesView&SC=="Positive"&MW==n,])+
                            count(abd18[MinesView&SC=="POSITIVE"&MW==n,]),
                          count(abd18[ModernSiteE&SC=="Positive"&MW==n,])+
                            count(abd18[ModernSiteE&SC=="POSITIVE"&MW==n,]),
                          count(abd18[ModernSiteW&SC=="Positive"&MW==n,])+
                            count(abd18[ModernSiteW&SC=="POSITIVE"&MW==n,]),
                          count(abd18[MRR&SC=="Positive"&MW==n,])+
                            count(abd18[MRR&SC=="POSITIVE"&MW==n,]),
                          count(abd18[NewLuc&SC=="Positive"&MW==n,])+
                            count(abd18[NewLuc&SC=="POSITIVE"&MW==n,]),
                          count(abd18[Outlook&SC=="Positive"&MW==n,])+
                            count(abd18[Outlook&SC=="POSITIVE"&MW==n,]),
                          count(abd18[Pacdal&SC=="Positive"&MW==n,])+
                            count(abd18[Pacdal&SC=="POSITIVE"&MW==n,]),
                          count(abd18[PadreB&SC=="Positive"&MW==n,])+
                            count(abd18[PadreB&SC=="POSITIVE"&MW==n,]),
                          count(abd18[PadreZ&SC=="Positive"&MW==n,])+
                            count(abd18[PadreZ&SC=="POSITIVE"&MW==n,]),
                          count(abd18[PU&SC=="Positive"&MW==n,])+
                            count(abd18[PU&SC=="POSITIVE"&MW==n,]),
                          count(abd18[PhilAm&SC=="Positive"&MW==n,])+
                            count(abd18[PhilAm&SC=="POSITIVE"&MW==n,]),
                          count(abd18[Pinget&SC=="Positive"&MW==n,])+
                            count(abd18[Pinget&SC=="POSITIVE"&MW==n,]),
                          count(abd18[PPP&SC=="Positive"&MW==n,])+
                            count(abd18[PPP&SC=="POSITIVE"&MW==n,]),
                          count(abd18[PisaoP&SC=="Positive"&MW==n,])+
                            count(abd18[PisaoP&SC=="POSITIVE"&MW==n,]),
                          count(abd18[Poliwes&SC=="Positive"&MW==n,])+
                            count(abd18[Poliwes&SC=="POSITIVE"&MW==n,]),
                          count(abd18[pucsusan&SC=="Positive"&MW==n,])+
                            count(abd18[pucsusan&SC=="POSITIVE"&MW==n,]),
                          count(abd18[QHP&SC=="Positive"&MW==n,])+
                            count(abd18[QHP&SC=="POSITIVE"&MW==n,]),
                          count(abd18[QHillU&SC=="Positive"&MW==n,])+
                            count(abd18[QHillU&SC=="POSITIVE"&MW==n,]),
                          count(abd18[UpperQM&SC=="Positive"&MW==n,])+
                            count(abd18[UpperQM&SC=="POSITIVE"&MW==n,]),
                          count(abd18[QHillE&SC=="Positive"&MW==n,])+
                            count(abd18[QHillE&SC=="POSITIVE"&MW==n,]),
                          count(abd18[QHL&SC=="Positive"&MW==n,])+
                            count(abd18[QHL&SC=="POSITIVE"&MW==n,]),
                          count(abd18[QHM&SC=="Positive"&MW==n,])+
                            count(abd18[QHM&SC=="POSITIVE"&MW==n,]),
                          count(abd18[QHillW&SC=="Positive"&MW==n,])+
                            count(abd18[QHillW&SC=="POSITIVE"&MW==n,]),
                          count(abd18[RizalMonumentArea&SC=="Positive"&MW==n,])+
                            count(abd18[RizalMonumentArea&SC=="POSITIVE"&MW==n,]),
                          count(abd18[RockQuarryLower&SC=="Positive"&MW==n,])+
                            count(abd18[RockQuarryLower&SC=="POSITIVE"&MW==n,]),
                          count(abd18[RockQuarryMiddle&SC=="Positive"&MW==n,])+
                            count(abd18[RockQuarryMiddle&SC=="POSITIVE"&MW==n,]),
                          count(abd18[RockQuarryUpper&SC=="Positive"&MW==n,])+
                            count(abd18[RockQuarryUpper&SC=="POSITIVE"&MW==n,]),
                          count(abd18[salud&SC=="Positive"&MW==n,])+
                            count(abd18[salud&SC=="POSITIVE"&MW==n,]),
                          count(abd18[SanAntonioVillage&SC=="Positive"&MW==n,])+
                            count(abd18[SanAntonioVillage&SC=="POSITIVE"&MW==n,]),
                          count(abd18[SanLuisVillage&SC=="Positive"&MW==n,])+
                            count(abd18[SanLuisVillage&SC=="POSITIVE"&MW==n,]),
                          count(abd18[SanRoqueVillage&SC=="Positive"&MW==n,])+
                            count(abd18[SanRoqueVillage&SC=="POSITIVE"&MW==n,]),
                          count(abd18[sanvic&SC=="Positive"&MW==n,])+
                            count(abd18[sanvic&SC=="POSITIVE"&MW==n,]),
                          count(abd18[SanCamp&SC=="Positive"&MW==n,])+
                            count(abd18[SanCamp&SC=="POSITIVE"&MW==n,]),
                          count(abd18[SanitaryCampSouth&SC=="Positive"&MW==n,])+
                            count(abd18[SanitaryCampSouth&SC=="POSITIVE"&MW==n,]),
                          count(abd18[STE&SC=="Positive"&MW==n,])+
                            count(abd18[STE&SC=="POSITIVE"&MW==n,]),
                          count(abd18[SantoR&SC=="Positive"&MW==n,])+
                            count(abd18[SantoR&SC=="POSITIVE"&MW==n,]),
                          count(abd18[STP&SC=="Positive"&MW==n,])+
                            count(abd18[STP&SC=="POSITIVE"&MW==n,]),
                          count(abd18[STsa&SC=="Positive"&MW==n,])+
                            count(abd18[STsa&SC=="POSITIVE"&MW==n,]),
                          count(abd18[ScoutB&SC=="Positive"&MW==n,])+
                            count(abd18[ScoutB&SC=="POSITIVE"&MW==n,]),
                          count(abd18[Session&SC=="Positive"&MW==n,])+
                            count(abd18[Session&SC=="POSITIVE"&MW==n,]),
                          count(abd18[Slaughter&SC=="Positive"&MW==n,])+
                            count(abd18[Slaughter&SC=="POSITIVE"&MW==n,]),
                          count(abd18[SLU&SC=="Positive"&MW==n,])+
                            count(abd18[SLU&SC=="POSITIVE"&MW==n,]),
                          count(abd18[SouthDrive&SC=="Positive"&MW==n,])+
                            count(abd18[SouthDrive&SC=="POSITIVE"&MW==n,]),
                          count(abd18[TeoAl&SC=="Positive"&MW==n,])+
                            count(abd18[TeoAl&SC=="POSITIVE"&MW==n,]),
                          count(abd18[tranco&SC=="Positive"&MW==n,])+
                            count(abd18[tranco&SC=="POSITIVE"&MW==n,]),
                          count(abd18[victoria&SC=="Positive"&MW==n,])+
                            count(abd18[victoria&SC=="POSITIVE"&MW==n,]),
                          count(abd18[STJos&SC=="Positive"&MW==n,])+
                            count(abd18[STJos&SC=="POSITIVE"&MW==n,])
  )
}

View(tdf11)
View(tdf12)
View(tdf13)
View(tdf14)
View(tdf15)
View(tdf16)
View(tdf17)
View(tdf18)

View(adf11)
View(adf12)
View(adf13)
View(adf14)
View(adf15)
View(adf16)
View(adf17)
View(adf18)
