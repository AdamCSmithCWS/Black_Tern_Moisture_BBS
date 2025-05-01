


# extracting the bbsBayes maps and calculating centroids ------------------


library(sf)
locat = system.file("maps",
            package="bbsBayes")
map.file = "BBS_CWS_strata"

bbs_strata_map = sf::read_sf(dsn = locat,
                         layer = map.file)

bbs_strata_proj = st_transform(bbs_strata_map,crs = 102003) #USA Contiguous albers equal area projection stanadard from ESRI - https://mgimond.github.io/Spatial/coordinate-systems-in-r.html

strat_centroids = st_centroid(bbs_strata_proj)



# extracting the bbsBayes area weights ------------------------------------

library(sf)
area_weights = read.csv(system.file("area-weight/stratcan.csv",
                    package="bbsBayes"),stringsAsFactors = F)




# reading in the annual indices of abundance ----------------------------------


### then these area_weights, plus the annual indices and the centroidsare all you need to calculate the geometric centoird of the species distribution in each year

# population centroid = area and abundance weighted mean coordinate
# 
# e.g., for each year, the mean x-coordinate = 
#   (area[1:nstrata] * index[1:nstrata] * coordinate[1:nstrata])/sum(area[1:nstrata] * index[1:nstrata])
# 



