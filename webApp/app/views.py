import pandas as pd
import pymysql as mdb
import json

from a_Model import ModelIt
from flask import render_template, request 
from app import app

VioCode = {0:"Pass",
           1:"Food Protection Management",
           2:"Food",
           3:"Time and Temperature Ctrl",
           4:"FoodContamination",
           5:"Personnel",
           6:"Equipment and Utensils",
           7:"Water",
           8:"Sewage",
           9:"Plumbing",
           10:"Toilet and Handwashing",
           11:"Refuse Disposal",
           12:"Insect and Animal Ctrl",
           13:"Physical Facilities",
           14:"Other Operations",
           15:"Susceptible Populations",
           16:"Others"}

colCode = {0:"#ccff99",
           1:"#00FFFF",
           2:"#0000FF",
           3:"#C0C0C0",
           4:"#FF0000",
           5:"#808080",
           6:"#008000",
           7:"#00FF00",
           8:"#800000",
           9:"#000080",
           10:"#808000",
           11:"#800080",
           12:"#FF00FF",
           13:"#000000",
           14:"#008080",
           15:"#FFFF00",
           16:"#FF9966"}

@app.route('/')
@app.route('/index')
#def index():
#    return render_template("index.html", title = 'Home')

# render_template looks for the html file in its default folder templates
#  See documentation from http://flask.pocoo.org/docs/0.10/quickstart/

@app.route('/input')
def safefood_input():
    zipcode = request.args.get('ID')
    db= mdb.connect(user="root", host="localhost", passwd = "0520", db="citydb", charset='utf8')
    
    with db:
        cur = db.cursor()
        #just select the city from the world_innodb that the user inputs
        cur.execute("SELECT * FROM smote order by VioLevel_pred desc;")
        query_results = cur.fetchall()
    
    restaurants = []; restaurants_txt = []; restaurants_col = []
    restKeys = ['Index','Name','VioLevel','VioType','VioLevel_pred','VioType_pred','Rating',
                'PropValue','Address','Zip','locLong','locLati','Category']
    CategList = ['arts','food','nightlife','hotelstravel','restaurants','eventservices']
    numResult = len(query_results)
        
    
    restaurants_df = pd.DataFrame(columns = restKeys, index = range(numResult))
    restaurants_txtdf = pd.DataFrame(columns = restKeys, index = range(numResult))
    restaurants_coldf = pd.DataFrame(columns = restKeys, index = range(numResult))

    for restInd in range(len(query_results)):
        # restKeys are same as column names of MySQL table, except the coded categories in MySQL are removed, and 'Category' is added
        result = list(query_results[restInd])
        categ = ''
        for i in range(len(result)-len(restKeys)+1):         
            if result[i+len(restKeys)-1] == 1: categ += CategList[i]+', '
    
        restValues = [result[j] for j in range(len(restKeys)-1)]

        restValues.append(categ)
        restValues_txt = list(restValues)
        restValues_col = list(restValues)
        
        restValues_col[3]  = colCode[int(float(restValues_col[3]))]
        restValues_col[5]  = colCode[int(float(restValues_col[5]))]

        restInfo = dict(zip(restKeys, restValues))
        restInfo_txt = dict(zip(restKeys, restValues_txt))
        restInfo_col = dict(zip(restKeys, restValues_col))

        restaurants.append(restInfo) # list of dictionaries, readable by html
        restaurants_txt.append(restInfo_txt) # list of dictionaries, readable by html
        restaurants_col.append(restInfo_col) # list of dictionaries, readable by html

        restaurants_df.iloc[restInd,:] = restValues
        restaurants_txtdf.iloc[restInd,:] = restValues_txt
        restaurants_coldf.iloc[restInd,:] = restValues_col

    map_center = [restaurants_df.locLati.astype(float).mean(),restaurants_df.locLong.astype(float).mean()]
    restaurants_json = json.loads(restaurants_df.to_json())
    restaurants_txtjson = json.loads(restaurants_txtdf.to_json())
    restaurants_coljson = json.loads(restaurants_coldf.to_json())
    
    return render_template("input.html", VioCode=VioCode, colCode=colCode, map_center = map_center, 
                           restaurants = restaurants, restaurants_json = restaurants_json,    
                           restaurants_txt = restaurants_txt, restaurants_txtjson = restaurants_txtjson, 
                           restaurants_coljson = restaurants_coljson)

@app.route('/output')
def safefood_output():
    zipcode = request.args.get('ID')
    db= mdb.connect(user="root", host="localhost", passwd = "0520", db="citydb", charset='utf8')
    
    with db:
        cur = db.cursor()
        cur.execute("SELECT * FROM smote WHERE Zip='%s' order by VioLevel_pred desc;" % zipcode)
        query_results = cur.fetchall()
    
    restaurants = []; restaurants_txt = []; restaurants_col = []
    restKeys = ['Index','Name','VioLevel','VioType','VioLevel_pred','VioType_pred','Rating',
                'PropValue','Address','Zip','locLong','locLati','Category']
    CategList = ['arts','food','nightlife','hotelstravel','restaurants','eventservices']
    numResult = len(query_results)
    print numResult, 'restaurants found'
    
    
    restaurants_df = pd.DataFrame(columns = restKeys, index = range(numResult))
    restaurants_txtdf = pd.DataFrame(columns = restKeys, index = range(numResult))
    restaurants_coldf = pd.DataFrame(columns = restKeys, index = range(numResult))

    for restInd in range(len(query_results)):
        # restKeys are same as column names of MySQL table, except the coded categories in MySQL are removed, and 'Category' is added
        result = list(query_results[restInd])
        categ = ''
        for i in range(len(result)-len(restKeys)+1):         
            if result[i+len(restKeys)-1] == 1: categ += CategList[i]+', '

        restValues = [result[j] for j in range(len(restKeys)-1)]
        restValues.append(categ)

        restValues_txt = list(restValues)
        restValues_col = list(restValues)
        
        restValues_txt[3]  = VioCode[int(float(restValues_txt[3]))]
        restValues_txt[5]  = VioCode[int(float(restValues_txt[5]))]
        
        restValues_col[3]  = colCode[int(float(restValues_col[3]))]
        restValues_col[5]  = colCode[int(float(restValues_col[5]))]
        
        restInfo = dict(zip(restKeys, restValues))
        restInfo_txt = dict(zip(restKeys, restValues_txt))
        restInfo_col = dict(zip(restKeys, restValues_col))

        restaurants.append(restInfo) # list of dictionaries, readable by html
        restaurants_txt.append(restInfo_txt) # list of dictionaries, readable by html
        restaurants_col.append(restInfo_col)

        restaurants_df.iloc[restInd,:] = restValues
        restaurants_txtdf.iloc[restInd,:] = restValues_txt
        restaurants_coldf.iloc[restInd,:] = restValues_col
    
    map_center = [restaurants_df.locLati.astype(float).mean(),restaurants_df.locLong.astype(float).mean()]
    
    restaurants_json = json.loads(restaurants_df.to_json())
    restaurants_txtjson = json.loads(restaurants_txtdf.to_json())
    restaurants_coljson = json.loads(restaurants_coldf.to_json())

    return render_template("output.html", numResult = numResult, VioCode = VioCode, colCode=colCode, 
                           map_center = map_center, restaurants = restaurants, restaurants_json = restaurants_json, 
                           restaurants_txt = restaurants_txt, restaurants_txtjson = restaurants_txtjson, 
                           restaurants_coljson = restaurants_coljson)    


