import pandas as pd
import pymysql as mdb
import json

from a_Model import ModelIt
from flask import render_template, request 
from app import app

VioCode = {0:'Pass',
           1:'Food Protection Management',
           2:'Food',
           3:'Time & Temperature Ctrl',
           4:'FoodContamination',
           5:'Personnel',
           6:'Equipment and Utensils',
           7:'Water',
           8:'Sewage',
           9:'Plumbing',
           10:'Toilet/Handwashing',
           11:'Refuse Disposal',
           12:'Insect/Animal Ctrl',
           13:'Physical Facilities',
           14:'Other Operations',
           15:'Susceptible Populations',
           16:'Others'}

@app.route('/')
@app.route('/index')
def index():
    return render_template("index.html", title = 'Home')

# render_template looks for the html file in its default folder templates
#  See documentation from http://flask.pocoo.org/docs/0.10/quickstart/

@app.route('/input')
def safefood_input():
    zipcode = request.args.get('ID')
    db= mdb.connect(user="root", host="localhost", passwd = "0520", db="citydb", charset='utf8')
    
    with db:
        cur = db.cursor()
        #just select the city from the world_innodb that the user inputs
        cur.execute("SELECT * FROM singleInsp order by VioLevel_pred desc;")
        query_results = cur.fetchall()
    
    restaurants = []; restaurants_txt = []
    restKeys = ['Index','Name','VioLevel','VioType','VioLevel_pred','VioType_pred','Rating',
                'PropValue','Zip','locLong','locLati','Category']
    CategList = ['arts','food','nightlife','hotelstravel','restaurants','eventservices']
    numResult = len(query_results)
        
    
    restaurants_df = pd.DataFrame(columns = restKeys, index = range(numResult))
    for restInd in range(len(query_results)):
        # restKeys are same as column names of MySQL table, except the coded categories in MySQL are removed, and 'Category' is added
        result = list(query_results[restInd])
        categ = ''
        for i in range(len(result)-len(restKeys)+1):         
            if result[i+len(restKeys)-1] == 1: categ += CategList[i]+', '
    
        restValues = [result[j] for j in range(len(restKeys)-1)]

        restValues.append(categ)
        restValues_txt = list(restValues)
        restValues_txt[3]  = VioCode[int(float(restValues_txt[3]))]
        restValues_txt[5]  = VioCode[int(float(restValues_txt[5]))]
        restInfo = dict(zip(restKeys, restValues))
        restInfo_txt = dict(zip(restKeys, restValues_txt))
        restaurants.append(restInfo) # list of dictionaries, readable by html
        restaurants_txt.append(restInfo_txt) # list of dictionaries, readable by html
        restaurants_df.iloc[restInd,:] = restValues
    map_center = [restaurants_df.locLati.astype(float).mean(),restaurants_df.locLong.astype(float).mean()]
    restaurants_json = json.loads(restaurants_df.to_json())
    
    return render_template("input.html", map_center = map_center, restaurants = restaurants, restaurants_json = restaurants_json, restaurants_txt = restaurants_txt)

@app.route('/output')
def safefood_output():
    zipcode = request.args.get('ID')
    db= mdb.connect(user="root", host="localhost", passwd = "0520", db="citydb", charset='utf8')
    
    with db:
        cur = db.cursor()
        #just select the city from the world_innodb that the user inputs
        cur.execute("SELECT * FROM singleInsp WHERE Zip='%s' order by VioLevel_pred desc;" % zipcode)
        query_results = cur.fetchall()
    
    restaurants = []; restaurants_txt = []
    restKeys = ['Index','Name','VioLevel','VioType','VioLevel_pred','VioType_pred','Rating',
                'PropValue','Zip','locLong','locLati','Category']
    CategList = ['arts','food','nightlife','hotelstravel','restaurants','eventservices']
    numResult = len(query_results)
    print numResult, 'restaurants found'
    
    
    restaurants_df = pd.DataFrame(columns = restKeys, index = range(numResult))
    for restInd in range(len(query_results)):
        # restKeys are same as column names of MySQL table, except the coded categories in MySQL are removed, and 'Category' is added
        result = list(query_results[restInd])
        #result[3]  = VioCode[int(result[3])]
        #result[5]  = VioCode[int(result[5])]	 
        categ = ''
        for i in range(len(result)-len(restKeys)+1):         
            if result[i+len(restKeys)-1] == 1: categ += CategList[i]+', '
        restValues = [result[j] for j in range(len(restKeys)-1)]
        restValues.append(categ)
        restValues_txt = list(restValues)
        restValues_txt[3]  = VioCode[int(float(restValues_txt[3]))]
        restValues_txt[5]  = VioCode[int(float(restValues_txt[5]))]
        restInfo = dict(zip(restKeys, restValues))
        restInfo_txt = dict(zip(restKeys, restValues_txt))
        restaurants.append(restInfo) # list of dictionaries, readable by html
        restaurants_txt.append(restInfo_txt) # list of dictionaries, readable by html
        restaurants_df.iloc[restInd,:] = restValues
    map_center = [restaurants_df.locLati.astype(float).mean(),restaurants_df.locLong.astype(float).mean()]
    restaurants_json = json.loads(restaurants_df.to_json())
    #print restaurants_json
    #print restaurants_df.head()
    #call a function from a_Model package. note we are only pulling one result in the query
    #pop_input = cities[0]['population']
    #the_result = ModelIt(city, pop_input)
    return render_template("output.html", numResult = numResult, map_center = map_center, restaurants = restaurants, restaurants_json = restaurants_json, restaurants_txt = restaurants_txt)    


