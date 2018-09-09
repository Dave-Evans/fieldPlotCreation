#python3 -m venv /path/to/new/virtual/environment

import psycopg2
import math


def rotate(x, y, ref_pt_x, ref_pt_y, rot_rad):
    '''Rotating a point around an arbitrary point,
    the arbitrary point here being the lower left corner of the rep'''
    newX = ref_pt_x + (x-ref_pt_x)*math.cos(rot_rad)-(y-ref_pt_x)*math.sin(rot_rad)
    newY = ref_pt_y + (x-ref_pt_x)*math.sin(rot_rad)+(y-ref_pt_x)*math.cos(rot_rad)

    return newX, newY




def create_plot(llx, lly, info):
    '''Function to create individual plot
    returns the text WKT of the polygon
    as well as the lower righthand point, which is the new
    lower left of the next plot
    '''
    ## Building a single plot
    ulx = llx 
    uly = lly + info['plot_sz_long']

    urx = llx + info['plot_sz_wide']
    ury = lly + info['plot_sz_long']

    lrx = llx + info['plot_sz_wide']
    lry = lly
    
    pts_poly = [
        (llx, lly),
        (ulx, uly),
        (urx, ury),
        (lrx, lry),
        (llx, lly),
    ]

    return pts_poly

info = {

    ## The below are givens:
    ## world transverse mercator
    "srid" : 3395,

    ## Rotation angle (calculated by clicks)
    "rot" : 10,
    "rot_rad" : 10 * (math.pi / 180),

    ## Feet or metric
    "metric" : True,

    ## Size of plots
    "plot_sz_wide" : 10,
    "plot_sz_long" : 10,

    ## Six plots by six plots
    "plot_cnt_wide" : 6,
    "plot_cnt_long" : 6,

    ## Alley distances
    "alley_dist_wide" : 10,
    "alley_dist_long" : 0,

    ## Location of lower left coordinate 
    ##  This is our reference coordinate
    "ref_pt_x" : 100,
    "ref_pt_y" : 100
}



conn = psycopg2.connect(database='wieff', password='jackson')
cur = conn.cursor()
pts_polys = []

for plt_y in range(info['plot_cnt_long']):
    print("Working on row {}...".format(plt_y))

    ll_x = info['ref_pt_x']
    # for other rows of plots
    # and for alleys
    ll_y = info['ref_pt_y'] +\
        (plt_y * info['plot_sz_long']) +\
        (plt_y * info['alley_dist_long']) 


    for plt_x in range(info['plot_cnt_wide']):
        
        pts_poly = create_plot(ll_x, ll_y, info)
        pts_polys.append(pts_poly)
        # For between plot alleys
        ll_x = pts_poly[3][0] + info['alley_dist_wide']
        ll_y = pts_poly[3][1]

## For transforming the points of each plot to correct for 
##  off North angle of the plot orientation
for i, poly in enumerate(pts_polys):
    
    llx, lly = poly[0]
    ulx, uly = poly[1]
    urx, ury = poly[2]
    lrx, lry = poly[3]
    ## Transforming once all calculated
    tllx, tlly = rotate(llx, lly, info['ref_pt_x'], info['ref_pt_y'], info['rot_rad'])
    tulx, tuly = rotate(ulx, uly, info['ref_pt_x'], info['ref_pt_y'], info['rot_rad'])
    turx, tury = rotate(urx, ury, info['ref_pt_x'], info['ref_pt_y'], info['rot_rad'])
    tlrx, tlry = rotate(lrx, lry, info['ref_pt_x'], info['ref_pt_y'], info['rot_rad'])
    wkt_poly = """
        ST_GeomFromText('POLYGON( ({llx} {lly}, {ulx} {uly}, {urx} {ury}, {lrx} {lry}, {llx} {lly}))', 3395);
    """.format(llx = tllx, lly = tlly, ulx = tulx, uly = tuly, lrx = tlrx, lry = tlry, urx = turx, ury = tury)

    s_tmplt_poly = """
    create table if not exists test_poly (id serial, name varchar, geom geometry(Polygon, 3395));
    INSERT INTO test_poly
    SELECT {id}, '{rot} degrees', {geom}

    """.format(id = i, rot = info['rot'], geom = wkt_poly)


    cur.execute(s_tmplt_poly)
    
conn.commit()
conn.close()

