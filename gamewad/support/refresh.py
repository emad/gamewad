
import os
import simplejson

def main(fn):
    f = open(fn)
    data = simplejson.load(f)
    games = data['games']
    for g in games:
        zfn = "../priv/www/static/games/" + os.path.basename(os.path.splitext(g['zip_url'])[0])
        if not os.path.exists(zfn):
            print "curl -O -C -", g['zip_url']
            print "unzip ", os.path.basename(g['zip_url'])
