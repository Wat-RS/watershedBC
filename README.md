# watershedBC

**Watershed reports for anywhere in British Columbia, on demand.**

watershedBC is an open-source R Shiny app. Click any point on the map (or search a named stream), pick a watershed, and get a report: discharge estimates, land-cover, forest disturbance, roads, water licences, climate, snow, landslide susceptibility and satellite imagery, all summarised for that watershed and downloadable as a PDF.

> ⚠️ **watershedBC is a prototype.** Results are indicative only, have not been validated, and must not be used for regulatory, engineering or water-management decisions. Always go back to the source data.

- **App:** <!-- TODO: current production URL (e.g. https://watershedbc.ca) -->
- **Issues & feedback:** https://github.com/Wat-RS/watershedBC/issues
- **Contact:** Alex Bevington, alex.bevington@unbc.ca

---

## Contents

- [What the tool does](#what-the-tool-does)
- [Where the data comes from](#where-the-data-comes-from)
- [Live vs. pre-processed layers](#live-vs-pre-processed-layers)
- [Partners](#partners)
- [Contributing](#contributing)
- [Running the app locally](#running-the-app-locally)
- [Repository layout](#repository-layout)
- [License](#license)

---

## What the tool does

### 1. Pick a watershed

Choose a watershed source, then click the map or search by name:

| Source | What it is |
|---|---|
| **Freshwater Atlas Named Watersheds** | ~11,500 named drainage polygons from the BC Freshwater Atlas (FWA). Best starting point. |
| **Freshwater Atlas by Stream Order** | ~1.35 million FWA watershed units rolled up by stream order, so every stream segment has a polygon for its whole upstream area. |
| **Custom Basin at Point of Interest** | ~946,000 basins delineated every ~1 km along BC waterways from the Copernicus GLO-30 DEM (WhiteboxTools). For points not on a named stream or gauge. |
| **Water Survey of Canada Basins** | ~2,900 drainage basins above WSC hydrometric stations in BC and Yukon. Best for comparing against observed flow. |
| **Coastal Watersheds with Typing** | Detailed Coastal Watershed Basin Model (DCWBM) basins with hydrologic regime typing (rain, hybrid, snow, glacial) from 1991–2020 normals. BC Ministry of Forests. |
| **Coastal Important Fisheries Watersheds** | The DCWBM subset identified as Important Fisheries Watersheds (IFW). |

### 2. Choose report modules

| Module | What you get |
|---|---|
| **Streamflow and Freshwater** | Mean annual and daily discharge estimated by scaling a similar WSC gauge (random-forest matching; *not validated*); wetlands, lakes and glaciers from the FWA; glacier change 1985 → 2021; dams. |
| **Forest Disturbance** | Cutblocks and wildfire area by year, roads by type, and a *simplified* ECA (40-year linear recovery; **this is not a formal ECA**). |
| **CEF Human Disturbance – 2021** | Human disturbance footprint from the provincial Cumulative Effects Framework. |
| **Stream Profile** | Longitudinal profiles of the named streams in the watershed. |
| **Water Allocations** | Water rights licences and water approval points. |
| **ClimateBC** | Mean annual precipitation, temperature and climatic moisture deficit (1991–2020) plus CMIP6 projections. |
| **Snow Remote Sensing** | Snow-cover timing from a multi-sensor (MODIS / VIIRS / Landsat) composite, 2001–2023. |
| **Landslide Zonation** | Landslide susceptibility from slope (GLO-30) and precipitation (ClimateBC). |
| **Satellite Imagery** | Cloud-free Landsat 1985–1990, Landsat 2020–2023 and Sentinel-2 2023 mosaics. |

### 3. Explore the map and download the report

The map also has reference layers you can switch on (lidar hillshades, BC streams/roads/place names, WSC stations, the provincial landslide inventory), a measure tool, and PDF export of the current view. Each report can be downloaded as a PDF.

---

## Where the data comes from

| Theme | Source |
|---|---|
| Watersheds, streams, lakes, wetlands, glaciers | [BC Freshwater Atlas](https://catalogue.data.gov.bc.ca/dataset/freshwater-atlas-named-watersheds) – BC Data Catalogue |
| Gauged basins and flow records | Water Survey of Canada – [HYDAT](https://www.canada.ca/en/environment-climate-change/services/water-overview/quantity/monitoring/survey/data-products-services/national-archive-hydat.html) and [hydrometric basin polygons](https://collaboration.cmc.ec.gc.ca/cmc/hydrometrics/www/HydrometricNetworkBasinPolygons/) |
| Coastal watersheds and typing (DCWBM, IFW) | BC Ministry of Forests |
| Cutblocks | [Harvested Areas of BC (Consolidated Cutblocks)](https://catalogue.data.gov.bc.ca/dataset/harvested-areas-of-bc-consolidated-cutblocks-) |
| Wildfire | [BC Wildfire Service fire perimeters – historical and current](https://catalogue.data.gov.bc.ca/dataset/fire-perimeters-historical) |
| Roads | Digital Road Atlas (DRA) – BC Data Catalogue |
| Human disturbance | [CEF Disturbance 2021](https://catalogue.data.gov.bc.ca/dataset/ce-disturbance-2021/) |
| Water licences and approvals | [Water Rights Licences](https://catalogue.data.gov.bc.ca/dataset/water-rights-licences-public), [Water Approval Points](https://catalogue.data.gov.bc.ca/dataset/water-approval-points) |
| Glacier change | BC glacier extents 1985 and 2021 – BC Data Catalogue |
| Climate | [ClimateBC](https://climatebc.ca/) v7.3 (Wang et al. 2016); CMIP6 13-GCM ensemble |
| Elevation | Copernicus GLO-30 DEM |
| Lidar | Lidar DTM hillshades by acquisition year, plus a province-wide mosaic <!-- TODO: confirm source (LidarBC?) --> |
| Snow | MODIS Terra/Aqua, VIIRS, Landsat composite |
| Imagery | USGS Landsat Collection 2 L2; ESA Copernicus Sentinel-2 L2A; Hansen Global Forest Change Landsat tiles |
| Landslide inventory | Landslides v14 (Brideau et al. 2026) |

---

## Live vs. pre-processed layers

watershedBC pulls data from three kinds of place. This matters for keeping the data current, and for contributing (see [Contributing](#contributing)).

### 🟢 Live: fetched at request time from public services

These are always as current as the provider. **No database needed.**

| Layer | Service |
|---|---|
| Glacier extents 1985 and 2021 | BC Data Catalogue (`bcdata`) |
| FWA stream network (Stream Profile) | BC Data Catalogue (`bcdata`) |
| Water rights licences, water approval points | BC Data Catalogue (`bcdata`) |
| BC streams, roads and place names (map overlays) | DataBC WMS – `openmaps.gov.bc.ca` |
| BC basemap | `maps.gov.bc.ca` tiles |
| World imagery / topo basemaps | Esri tiles |
| Landsat 2000 / 2021 basemaps | Hansen Global Forest Change tiles |
| Lidar hillshades | WMTS – `geoserver.watershedbc.ca` |

### 🔵 Cloud-optimized rasters: public, read in place

Rasters are stored as Cloud-Optimized GeoTIFFs in a public S3 bucket (`bcbasin.s3.ca-central-1.amazonaws.com`) and read over HTTP with GDAL `/vsicurl/`. **No database needed**, but they are static snapshots.

GLO-30 DEM and slope · ClimateBC 1991–2020 normals · snow-cover timing · Landsat 1985–1990 and 2020–2023 mosaics · Sentinel-2 2023 mosaic

### 🟠 Pre-processed: PostGIS database

Large vector layers are loaded into a PostGIS database so they can be clipped to a watershed in seconds. They are refreshed periodically with the scripts in `functions_prep/`. **These need database access.**

| Table(s) | Contents | Refreshed by |
|---|---|---|
| `fwa_named`, `fwa_named_names` | FWA named watersheds and search list | `getData_FWA_named_list.R` |
| `fwa_rollup` | FWA watersheds rolled up by stream order | `getData_fwa_rollup.R`, `getData_fwa_rollup_merge.R` |
| `basinsv4` | Custom basins at ~1 km pour points | <!-- TODO --> |
| `wsc_stationloc`, `wsc_drainagebasin_clean_3005` | WSC stations and gauged basins | `getData_WSC_Basins.R` |
| `wsc_monthly_with_attri_dams_*`, `daily_wsc_clean_*`, `wsc_pp_fasstr_calc_daily_stats` | Flow records and attributes for the discharge model | <!-- TODO --> |
| `dcwbm_bsn_typed_1991_2020_100m`, `dcwbm_bsn_typed_ifw_1991_2020_100m` | Coastal typed watersheds and IFW | `getGBR_typedWS.R`, `getData_iFWA.R` |
| `fwa_lakes`, `fwa_wetlands`, `fwa_glaciers` | FWA waterbodies | `getData_BCGW.R` |
| `cutblocks`, `fire` | Consolidated cutblocks, wildfire perimeters | `getData_BCGW.R`, `fire_cb.R` |
| `dra` | Digital Road Atlas | `getData_DRA.R` |
| `bc_cef_2021` | CEF human disturbance | `getData_CEF.R` |
| `dams` | Dams | <!-- TODO --> |
| `usage` | Anonymous app usage log (sessions, reports run) | the app itself |

---

## Partners

watershedBC began in the BC Government's GitHub organization ([bcgov/watershedBC](https://github.com/bcgov/watershedBC)) and is now developed by the [Watershed Remote Sensing Lab](https://watershedlab.ca) (Wat-RS) at the University of Northern British Columbia.

The project is built in collaboration with the Province of British Columbia. <!-- TODO: name the ministries/branches and the nature of the collaboration, e.g. BC Ministry of Forests (coastal watershed typing, DCWBM/IFW), Ministry of Water, Land and Resource Stewardship, GeoBC / LidarBC, BC Wildfire Service. --> Most of the underlying data is published openly by the Province through the [BC Data Catalogue](https://catalogue.data.gov.bc.ca/).

watershedBC is not an official Government of British Columbia product.

---

## Contributing

Contributions are welcome: bug reports, ideas, code, documentation and data.

### The problem: access to the database

The app connects to a private PostGIS database **when it starts**: it loads the watershed search list, WSC stations and discharge-model tables before the UI appears. That means a fresh clone **won't start without database credentials**, even if you only want to change a button colour. The database is tens of gigabytes of provincial-scale vector data, so it can't simply be shipped with the repo.

Until that's solved, here is how to help.

### Ways to contribute without database access

1. **Open an issue.** Bugs, confusing results, missing datasets, report wording, ideas. This is the most useful thing most people can do: https://github.com/Wat-RS/watershedBC/issues
2. **Work on anything that only uses live or COG data** (the 🟢 and 🔵 layers above). Prototype a new module or plot in a standalone R script for a single watershed polygon (e.g. one downloaded with `bcdata`), then open a PR or issue with the script. We'll wire it into the app.
3. **Front-end, map and report work.** CSS (`www/css/`), JavaScript (`www/js/`), map controls (`functions_app/app_5_leaflet.R`) and the PDF report template (`functions_report/report.Rmd`) can all be developed and reviewed from code. Describe how you tested it in the PR.
4. **Data preparation scripts.** Improvements to `functions_prep/` (new sources, faster processing, updated vintages) only need public data to develop.
5. **Documentation.** This README, the in-app help text (`functions_app/app_7_tooltips.R`), and method descriptions.

### Getting database access

If you're working on something that needs the full app (e.g. changing how a PostGIS layer is summarised), contact alex.bevington@unbc.ca. Read-only credentials can be provided to collaborators.

### Roadmap: making contribution easier

We'd like anyone to be able to run the app. Planned steps, and help is very welcome:

- [ ] **Demo mode:** if no database credentials are set, start the app with a small bundled sample (one region, e.g. a single FWA named watershed group) stored as a GeoPackage, instead of failing.
- [ ] **Sample database:** a Docker `postgis` image or `pg_dump` of a small region, with a one-line setup.
- [ ] **Documented schema:** columns, CRS and source for every table, generated from the prep scripts.
- [ ] **Reproducible build:** run `functions_prep/` end-to-end to rebuild the full database from public sources.
- [ ] **`renv` lockfile** for pinned R package versions.

### Pull requests

1. Fork the repo and create a branch from `main`.
2. Keep PRs focused: one feature or fix per PR.
3. Don't commit data files (`*.sqlite`, `*.gpkg`, `data/`), logs or credentials. `.Renviron` must never be committed.
4. Describe what you changed and how you tested it.

---

## Running the app locally

Requires R (developed on R 4.6) and the packages listed in `functions_app/app_1_libs.R`.

1. Clone the repo:
   ```bash
   git clone https://github.com/Wat-RS/watershedBC.git
   ```
2. Create a `.Renviron` file in the project root with database credentials (see [Getting database access](#getting-database-access)):
   ```
   aw_dbname=...
   aw_host=...
   aw_port=...
   aw_user=...
   aw_password=...
   ```
3. Open `watershedBC.Rproj` in RStudio and run `app.R`, or:
   ```r
   shiny::runApp()
   ```

---

## Repository layout

```
app.R                     Main Shiny app (UI + server)
functions_app/            App components
  app_1_libs.R            Packages
  app_2_db.R              Database connection pool + PostGIS clip helpers
  app_3_load_dat.R        Data loaded at startup (search list, WSC stations)
  app_4_discharge_*.R     Discharge estimation
  app_5_leaflet.R         Base map, layers, lidar control
  app_7_tooltips.R        Help text and disclaimer
  app_8_report.R          Report generation
functions_report/         PDF report template (report.Rmd)
functions_prep/           Scripts that build the PostGIS tables from public data
functions_monitoring/     Usage statistics (Usage tab)
www/                      CSS, JavaScript and images
```

---

## License

Code is released under the [Apache License 2.0](LICENSE). Data remain under their original licenses, mostly the [Open Government Licence – British Columbia](https://www2.gov.bc.ca/gov/content/data/open-data/open-government-licence-bc) and the [Open Government Licence – Canada](https://open.canada.ca/en/open-government-licence-canada).
