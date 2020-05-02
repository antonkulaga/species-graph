object TestTables extends scala.App  with TestExpressions {

}


trait TestExpressions {

  val row = Vector(
    ("Homo_sapiens", Vector("ens:ENSG00000005187", "ens:ENSG00000011295", "ens:ENSG00000101049")),
    (
      "Coturnix_japonica",
      Vector("ens:ENSCJPG00005018771", "ens:ENSCJPG00005014453", "ens:ENSCJPG00005020840")
    ),
    (
      "Cyanistes_caeruleus",
      Vector("ens:ENSCCEG00000021414", "ens:ENSCCEG00000011183", "ens:ENSCCEG00000006147")
    ),
    (
      "Gallus_gallus",
      Vector("ens:ENSGALG00000002200", "ens:ENSGALG00000004416", "ens:ENSGALG00000003537")
    ),
    (
      "Meleagris_gallopavo",
      Vector("ens:ENSMGAG00000010342", "ens:ENSMGAG00000006277", "ens:ENSMGAG00000003077")
    ),
    (
      "Parus_major",
      Vector("ens:ENSPMJG00000017134", "ens:ENSPMJG00000019681", "ens:ENSPMJG00000002883")
    ),
    ("Strigops_habroptila", Vector("null", "ens:ENSSHBG00005015765", "ens:ENSSHBG00005006563")),
    (
      "Taeniopygia_guttata",
      Vector("ens:ENSTGUG00000019715", "ens:ENSTGUG00000006675", "ens:ENSTGUG00000004480")
    ),
    (
      "Zonotrichia_albicollis",
      Vector("ens:ENSZALG00000000577", "ens:ENSZALG00000014443", "ens:ENSZALG00000013458")
    ),
    (
      "Ailuropoda_melanoleuca",
      Vector("ens:ENSAMEG00000006222", "ens:ENSAMEG00000004576", "ens:ENSAMEG00000003739")
    ),
    (
      "Bos_grunniens",
      Vector("ens:ENSBGRG00000026556", "ens:ENSBGRG00000010803", "ens:ENSBGRG00000012136")
    ),
    ("Bos_taurus", Vector("ens:ENSBTAG00000006447", "ens:ENSBTAG00000013270", "null")),
    (
      "Callithrix_jacchus",
      Vector("ens:ENSCJAG00000035643", "ens:ENSCJAG00000012193", "ens:ENSCJAG00000017869")
    ),
    (
      "Capra_hircus",
      Vector("ens:ENSCHIG00000024519", "ens:ENSCHIG00000022610", "ens:ENSCHIG00000022211")
    ),
    ("Cavia_aperea", Vector("null", "null", "ens:ENSCAPG00000006884")),
    (
      "Cavia_porcellus",
      Vector("ens:ENSCPOG00000008928", "ens:ENSCPOG00000000846", "ens:ENSCPOG00000020415")
    ),
    ("Equus_caballus", Vector("ens:ENSECAG00000006669", "ens:ENSECAG00000008225", "null")),
    ("Erinaceus_europaeus", Vector("ens:ENSEEUG00000006889", "ens:ENSEEUG00000002787", "null")),
    (
      "Felis_catus",
      Vector("ens:ENSFCAG00000015159", "ens:ENSFCAG00000030206", "ens:ENSFCAG00000001877")
    ),
    (
      "Heterocephalus_glaber",
      Vector("ens:ENSHGLG00100012363", "ens:ENSHGLG00100003199", "ens:ENSHGLG00100008029")
    ),
    (
      "Macaca_fascicularis",
      Vector("ens:ENSMFAG00000029841", "ens:ENSMFAG00000036185", "ens:ENSMFAG00000032486")
    ),
    (
      "Macaca_mulatta",
      Vector("ens:ENSMMUG00000016745", "ens:ENSMMUG00000008949", "ens:ENSMMUG00000007925")
    ),
    (
      "Macaca_nemestrina",
      Vector("ens:ENSMNEG00000031986", "ens:ENSMNEG00000030246", "ens:ENSMNEG00000035575")
    ),
    (
      "Meriones_unguiculatus",
      Vector("ens:ENSMUGG00000007207", "ens:ENSMUGG00000017244", "ens:ENSMUGG00000021047")
    ),
    ("Mesocricetus_auratus", Vector("ens:ENSMAUG00000014046", "ens:ENSMAUG00000020275", "null")),
    (
      "Microcebus_murinus",
      Vector("ens:ENSMICG00000030875", "ens:ENSMICG00000007108", "ens:ENSMICG00000015157")
    ),
    ("Monodelphis_domestica", Vector("ens:ENSMODG00000007016", "null", "null")),
    (
      "Mus_caroli",
      Vector("ens:MGP_CAROLIEiJ_G0030383", "ens:MGP_CAROLIEiJ_G0016490", "ens:MGP_CAROLIEiJ_G0024608")
    ),
    (
      "Mus_musculus",
      Vector("ens:ENSMUSG00000030935", "ens:ENSMUSG00000042298", "ens:ENSMUSG00000017868")
    ),
    (
      "Mus_spicilegus",
      Vector("ens:ENSMSIG00000001752", "ens:ENSMSIG00000018495", "ens:ENSMSIG00000028516")
    ),
    ("Ornithorhynchus_anatinus", Vector("null", "ens:ENSOANG00000012683", "null")),
    ("Oryctolagus_cuniculus", Vector("ens:ENSOCUG00000009900", "ens:ENSOCUG00000010451", "null")),
    (
      "Otolemur_garnettii",
      Vector("ens:ENSOGAG00000011552", "ens:ENSOGAG00000009113", "ens:ENSOGAG00000007695")
    ),
    (
      "Ovis_aries",
      Vector("ens:ENSOARG00000013163", "ens:ENSOARG00000017392", "ens:ENSOARG00000003232")
    ),
    (
      "Pan_paniscus",
      Vector("ens:ENSPPAG00000037343", "ens:ENSPPAG00000021467", "ens:ENSPPAG00000031264")
    ),
    (
      "Pan_troglodytes",
      Vector("ens:ENSPTRG00000007849", "ens:ENSPTRG00000008807", "ens:ENSPTRG00000013508")
    ),
    ("Phascolarctos_cinereus", Vector("ens:ENSPCIG00000029240", "ens:ENSPCIG00000006942", "null")),
    (
      "Rattus_norvegicus",
      Vector("ens:ENSRNOG00000032246", "ens:ENSRNOG00000002977", "ens:ENSRNOG00000033573")
    ),
    (
      "Rhinolophus_ferrumequinum",
      Vector("ens:ENSRFEG00010020456", "ens:ENSRFEG00010010569", "ens:ENSRFEG00010006923")
    ),
    (
      "Rhinopithecus_bieti",
      Vector("ens:ENSRBIG00000043460", "ens:ENSRBIG00000036704", "ens:ENSRBIG00000041182")
    ),
    ("Sarcophilus_harrisii", Vector("ens:ENSSHAG00000004164", "null", "null")),
    (
      "Suricata_suricatta",
      Vector("ens:ENSSSUG00005009674", "ens:ENSSSUG00005021271", "ens:ENSSSUG00005019897")
    ),
    (
      "Sus_scrofa",
      Vector("ens:ENSSSCG00000007857", "ens:ENSSSCG00000023747", "ens:ENSSSCG00000024325")
    ),
    (
      "Tupaia_belangeri",
      Vector("ens:ENSTBEG00000006365", "ens:ENSTBEG00000014142", "ens:ENSTBEG00000014653")
    ),
    (
      "Tursiops_truncatus",
      Vector("ens:ENSTTRG00000016436", "ens:ENSTTRG00000002021", "ens:ENSTTRG00000004500")
    ),
    ("Ursus_americanus", Vector("null", "ens:ENSUAMG00000019684", "ens:ENSUAMG00000026358")),
    (
      "Anolis_carolinensis",
      Vector("ens:ENSACAG00000009637", "ens:ENSACAG00000014927", "ens:ENSACAG00000002261")
    ),
    (
      "Gopherus_agassizii",
      Vector("ens:ENSGAGG00000008232", "ens:ENSGAGG00000025578", "ens:ENSGAGG00000000577")
    ),
    (
      "Pogona_vitticeps",
      Vector("ens:ENSPVIG00000008636", "ens:ENSPVIG00000008524", "ens:ENSPVIG00000022306")
    ),
    ("Callorhinchus_milii", Vector("null", "null", "null")),
    (
      "Latimeria_chalumnae",
      Vector("ens:ENSLACG00000004631", "ens:ENSLACG00000008943", "ens:ENSLACG00000001395")
    ),
    ("Betta_splendens", Vector("null", "ens:ENSBSLG00000022343", "ens:ENSBSLG00000003364")),
    (
      "Clupea_harengus",
      Vector("ens:ENSCHAG00000014503", "ens:ENSCHAG00000017081", "ens:ENSCHAG00000027097")
    ),
    (
      "Electrophorus_electricus",
      Vector("ens:ENSEEEG00000013718", "ens:ENSEEEG00000014865", "ens:ENSEEEG00000008927")
    ),
    (
      "Esox_lucius",
      Vector("ens:ENSELUG00000000399", "ens:ENSELUG00000016887", "ens:ENSELUG00000013281")
    ),
    ("Neogobius_melanostomus", Vector("null", "null", "ens:ENSNMLG00000020683")),
    ("Scophthalmus_maximus", Vector("null", "null", "ens:ENSSMAG00000014701"))
  )
}
