#python3 -m venv /path/to/new/virtual/environment

# import psycopg2
import math
import json
import django.contrib.gis.gdal as gdal

from plotter.models import Study




def rotate(x, y, ref_pt_x, ref_pt_y, rot_rad):
    '''Rotating a point around an arbitrary point,
    the arbitrary point here being the lower left corner of the rep'''
    newX = ref_pt_x + (x-ref_pt_x)*math.cos(rot_rad)-(y-ref_pt_x)*math.sin(rot_rad)
    newY = ref_pt_y + (x-ref_pt_x)*math.sin(rot_rad)+(y-ref_pt_x)*math.cos(rot_rad)

    return newX, newY

def create_plot(llx, lly, study):
    '''Function to create individual plot
    returns the text WKT of the polygon
    as well as the lower righthand point, which is the new
    lower left of the next plot
    '''
    ## Building a single plot
    ulx = llx 
    uly = lly + study.plot_sz_long

    urx = llx + study.plot_sz_wide
    ury = lly + study.plot_sz_long

    lrx = llx + study.plot_sz_wide
    lry = lly
    
    pts_poly = [
        (llx, lly),
        (ulx, uly),
        (urx, ury),
        (lrx, lry),
        (llx, lly),
    ]

    return pts_poly

def transform_coords(x, y, to_lat_long = False):
    '''Takes a lat and long and creates a(n ogr) point, and then
    transforms to wtm.

    returns tuple of the transformed point
    '''

    # y = 43.1510673232675
    # x = -89.4187545776367

    ## "WGS84"
    wgs84 = gdal.SpatialReference(4326)
    wtm = gdal.SpatialReference(3395)


    t_form = None
    in_srs = None
    if to_lat_long:
        t_form = gdal.CoordTransform(wtm, wgs84)
        in_srs = 4326
    else:
        t_form = gdal.CoordTransform(wgs84, wtm)
        in_srs = 3395


    pt = gdal.OGRGeometry( 'POINT ({x} {y})'.format(x=x, y=y), srs=in_srs )

    pt.transform(t_form)

    return pt.coords



def transform_geojson_to_wgs84(geojson_dict):
    '''transform a geojson obj of plots to wgs84 to be sent back 
    to leaflet'''



def find_bearing(x, y, center_x, center_y):
    '''center_x and _y are the pivot point
    ref: https://stackoverflow.com/questions/5058617/bearing-between-two-points
    Something to look into: does this need to be in planar units?
    '''
    angle = math.degrees(math.atan2(y - center_y, x - center_x))
    angle_radians = angle * (math.pi / 180)
    bearing1 = (angle + 360) % 360
    bearing2 = (90 - angle) % 360
    return angle_radians






# conn = psycopg2.connect(database='', password='')
# cur = conn.cursor()

def create_rep(ref_pt_x, ref_pt_y, rot_rad, study):
    '''putting it together to create the rep of plots'''
    pts_polys = []

    for plt_y in range(study.plot_cnt_long):
        print("Working on row {}...".format(plt_y))

        ll_x = ref_pt_x
        # for other rows of plots
        # and for alleys
        ll_y = ref_pt_y +\
            (plt_y * study.plot_sz_long) +\
            (plt_y * study.alley_dist_long) 


        for plt_x in range(study.plot_cnt_wide):
            
            pts_poly = create_plot(ll_x, ll_y, study)
            pts_polys.append(pts_poly)
            # For between plot alleys
            ll_x = pts_poly[3][0] + study.alley_dist_wide
            ll_y = pts_poly[3][1]

    ## For transforming the points of each plot to correct for 
    ##  off North angle of the plot orientation
    dict_polys = {
         "type": "FeatureCollection",
        "features": [
        ]
    }   



    for i, poly in enumerate(pts_polys):
        tmplt_poly = {
             "type": "Feature",
             "geometry": {
               "type": "Polygon",
               "coordinates": [
                 
                 ]
             },
             "properties": {
               "plot_num": 0
               }
             }

        llx, lly = poly[0]
        ulx, uly = poly[1]
        urx, ury = poly[2]
        lrx, lry = poly[3]
        ## Transforming once all calculated
        tllx, tlly = rotate(llx, lly, ref_pt_x, ref_pt_y, rot_rad)
        tulx, tuly = rotate(ulx, uly, ref_pt_x, ref_pt_y, rot_rad)
        turx, tury = rotate(urx, ury, ref_pt_x, ref_pt_y, rot_rad)
        tlrx, tlry = rotate(lrx, lry, ref_pt_x, ref_pt_y, rot_rad)

        ## CoordTransforming back to WGS84
        # transform_coords(x, y, to_lat_long = False)
        tllx, tlly = transform_coords(llx, lly, True)
        tulx, tuly = transform_coords(ulx, uly, True)
        turx, tury = transform_coords(urx, ury, True)
        tlrx, tlry = transform_coords(lrx, lry, True)
        

        coords = [

            [tllx, tlly],
            [tulx, tuly],
            [turx, tury],
            [tlrx, tlry],
            [tllx, tlly],

        ]

        tmplt_poly["geometry"]["coordinates"] = [ coords ]
        tmplt_poly["properties"]["plot_num"] = i + 1

        dict_polys["features"].append(tmplt_poly)

    return dict_polys



study = Study.objects.get(pk=1)

ll_ref_pt_lat = 43.294281854583
ll_ref_pt_lng = -89.3825554847717

ul_ref_pt_lat = 43.2972803939921
ul_ref_pt_lng = -89.3825554847717

rot_rad = find_bearing(
    ul_ref_pt_lng,
    ul_ref_pt_lat,
    ll_ref_pt_lng,
    ll_ref_pt_lat
)

ref_pt_x, ref_pt_y = transform_coords(ll_ref_pt_lng, ll_ref_pt_lat)

test_wicst = create_rep(ref_pt_x, ref_pt_y, rot_rad, study)


json.dump(test_wicst, open("test_wicst.geojson", 'w'), indent=2)


# wkt_poly = """
#     POLYGON( ({llx} {lly}, {ulx} {uly}, {urx} {ury}, {lrx} {lry}, {llx} {lly}))
# """.format(llx = tllx, lly = tlly, ulx = tulx, uly = tuly, lrx = tlrx, lry = tlry, urx = turx, ury = tury)
# s_tmplt_poly = """
# create table if not exists test_poly (id serial, name varchar, geom geometry(Polygon, 3395));
# INSERT INTO test_poly
# SELECT {id}, '{rot} degrees', ST_GeomFromText('{geom}', 3395);

# """.format(id = i, rot = info['rot'], geom = wkt_poly)


# cur.execute(s_tmplt_poly)
    
# conn.commit()
# conn.close()


# info = {

#     ## The below are given within the Study

#     ## Feet or metric
#     "metric" : True,

#     ## Size of plots
#     "plot_sz_wide" : 10,
#     "plot_sz_long" : 10,

#     ## Six plots by six plots
#     "plot_cnt_wide" : 6,
#     "plot_cnt_long" : 6,

#     ## Alley distances
#     "alley_dist_wide" : 10,
#     "alley_dist_long" : 0,

#     ## Location of lower left coordinate 
#     ##  This is our reference coordinate
#     # "ref_pt_x" : 100,
#     # "ref_pt_y" : 100,

#     "ll_ref_pt_lat" : 43.1510673232675,
#     "ll_ref_pt_lng" : -89.4187545776367,

#     "ul_ref_pt_lat" : 43.1663989929558,
#     "ul_ref_pt_lng" : -89.4170379638672,
#     ## world transverse mercator
#     "srid" : 3395,

#     ## Rotation angle (calculated by clicks)
#     "rot" : 10,
#     "rot_rad" : 10 * (math.pi / 180),

# }
