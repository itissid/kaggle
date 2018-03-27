# For 2016-2017 data joins
non_join_features.default = c(
    "id_parcel", "fips", "latitude",
    "longitude", "pooltypeid2", "pooltypeid7",
    "rawcensustractandblock", "tax_building", "tax_total",
    "tax_year","tax_land","tax_property",
    "tax_delinquency", "tax_delinquency_year", "censustractandblock",
    "census", "tract_number", "tract_block")


# For 2016-2017 data joins
join_features.default=c(
    "build_year",
    "area_basement",
    "area_patio",
    "area_shed",
    "area_pool",
    "area_lot",
    "area_garage",
    "area_firstfloor_finished",
    "area_total_calc",
    "area_base",
    "area_live_finished",
    "area_liveperi_finished",
    "area_total_finished",
    "area_unknown",
    "num_unit",
    "num_story",
    "num_room",
    "num_bathroom",
    "num_bedroom",
    "num_bathroom_calc",
    "num_bath",
    "num_75_bath",
    "num_fireplace",
    "num_pool",
    "num_garage",
    "region_county",
    "region_city",
    "region_zip",
    "region_neighbor",
    "deck",
    "zoning_property",
    "zoning_landuse",
    "zoning_landuse_county",
    "flag_fireplace",
    "flag_tub",
    "flag_tub_extra",
    "quality",
    "framing",
    "material",
    "story",
    "heating",
    "aircon",
    "architectural_style"
    )

mapping_dates.default = list("2016-10-01"= "201610", "2016-11-01" ="201611", "2016-12-01"="201612")
second_round_mapping_dates.default = list("2016-10-01"= "201710", "2016-11-01" ="201711", "2016-12-01"="201712")

###########################
### Linear Regression related ##########
##########################


features.excluded.regression.default = c(
    "tax_year",
    "tax_delinquency",
    "area_total_calc", # causes full rank issues in LM
    "num_bathroom_calc", # caused rank issues in Lm
    "id_parcel",
    "pooltypeid2", # Redundant to pooltype10
    "pooltypeid7", # redundant to pooltype10
    "censustractandblock", # redundant
    "rawcensustractandblock", # broken into block and tract
    "fips", # redundant
    "census" # redundant
)

# Quite a few features here are dropped because of the large # of categories
# Ideally we should do multilevel regression for some of them
features.categorical.regression.default = c(
    "region_city",
    "region_county",
    "region_neighbor",
    "region_zip",
    "zoning_landuse",
    "zoning_property",
    "zoning_landuse_county",
    "quality",
    "framing",
    "architectural_style",
    "num_unit",
    "build_year",
    "date",
    "tax_delinquency_year",
    "tract_number",
    "tract_block"
)

# TODO: Maybe we can use certain permutations of this to see what improves prediction
features.treated.vtreat.regression.default = c(
    "region_city",
    "region_county",
    "region_neighbor",
    "region_zip",
    "zoning_landuse",
    "zoning_property",
    "zoning_landuse_county",
    "quality",
    "framing",
    "architectural_style",
    "num_unit",
    "build_year",
    "date",
    "tax_delinquency_year",
    "tract_number",
    "tract_block"
)

features.logtransformed.regression.default = c(
    "area_lot", "area_total_calc", "tax_total", "tax_land", "tax_property", "tax_building" ,
    "area_total_finished", "area_live_finished", "area_firstfloor_finished", "area_garage",
    "area_shed", "area_patio", "area_basement", "area_base", "area_unknown", "area_pool",
    "area_liveperi_finished")

###########################################
## XGBoost related data definitions ########
##########################################

features.excluded.xg.default = c(
    "id_parcel",
    "pooltypeid2", # Redundant to pooltype10
    "pooltypeid7", # redundant to pooltype10
    "censustractandblock", # redundant
    "rawcensustractandblock", # broken into block and tract
    "fips", # redundant
    "census", # redundant
    "area_total_calc", # 100% redundant to area_live_finished
    "tax_year" # This has only one value for the training set.
    #"aircon",
    #"heating",
    ##"zoning_landuse",
    ##"zoning_property",
    ##"zoning_landuse_county",
    ##"area_liveperi_finished",
    #"area_patio",
    #"area_pool",
    #"area_shed",
    #"area_garage",
    #"area_total_finished",
    #"area_unknown", 
    #"area_base",
    #"area_firstfloor_finished",
    #"deck",

    #"date",
    ##"tract_number",
    ##"tract_block",
    #"quality",
    #"framing",
    #"architectural_style",
    #"region_neighbor"
)

features.logtransformed.xg.default = c(
    "tax_building",
    "tax_land",
    "tax_property",
    "tax_total",
    "area_live_finished",
    "area_lot"
)

features.categorical.xg.default = c(
    # NOTE: see the note on why some largely missing categorical variables are included:
    "region_city",
    "region_county",
    "region_zip",
    "build_year",
    "zoning_landuse_county",
    "zoning_landuse",
    "tax_year",
    "tract_block",
    "tract_number",

    # The following properties are largely missing, but I added them because xgboost treats missing data as information
    # Consider removing one or more of these if they aren't in the feature importance matrix
    "zoning_property",
    "zoning_landuse_county",
    "aircon",
    "heating",
    "deck",
    "quality",
    "framing",
    "architectural_style",
    "flag_tub",
    "flag_tub_extra",
    "flag_fireplace",
    "material", 
    "region_neighbor",
    "tax_delinquency",
    "tax_delinquency_year"

)

features.treated.vtreat.xg.default = c(
    "region_city",
    "region_county",
    "region_neighbor",
    "region_zip",
    "zoning_landuse",
    "zoning_property",
    "zoning_landuse_county",
    "build_year",
    "tax_delinquency_year",
    "tract_number",
    "tract_block"
)
