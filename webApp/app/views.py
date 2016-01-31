from a_Model import ModelIt
from flask import render_template, request 
from app import app
import pymysql as mdb

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

@app.route('/db')
def cities_page():
    db= mdb.connect(user="root", host="localhost", passwd = "0520", db="world_innodb", charset='utf8')
    with db: 
        cur = db.cursor()
        cur.execute("SELECT Name FROM City LIMIT 15;")
        query_results = cur.fetchall()
    cities = ""
    for result in query_results:
        cities += result[0]
        cities += "<br>"
    return cities

@app.route("/db_fancy")
def cities_page_fancy():
    with db:
        cur = db.cursor()
        cur.execute("SELECT Name, CountryCode, Population FROM City ORDER BY Population LIMIT 15;")
        query_results = cur.fetchall()
    cities = []
    for result in query_results:
        cities.append(dict(name=result[0], country=result[1], population=result[2]))
    return render_template('cities.html', cities=cities) 

@app.route('/input')
def cities_input():
    return render_template("input.html")

#@app.route('/output')
#def cities_output():
#    return render_template("output.html")

@app.route('/output')
def cities_output():
    # pull 'ID' from input field and store it
    city = request.args.get('ID')
    db= mdb.connect(user="root", host="localhost", passwd = "0520", db="world_innodb", charset='utf8')
    with db:
        cur = db.cursor()
        #just select the city from the world_innodb that the user inputs
        cur.execute("SELECT Name, CountryCode,  Population FROM City WHERE Name='%s';" % city)
        query_results = cur.fetchall()
    cities = []
    for result in query_results:
        cities.append(dict(name=result[0], country=result[1], population=result[2]))
    #call a function from a_Model package. note we are only pulling one result in the query
    pop_input = cities[0]['population']
    the_result = ModelIt(city, pop_input)
    return render_template("output.html", cities = cities, the_result = the_result)

@app.route('/safefoodmap')
def safefoodmap():
    zipcode = request.args.get('ID')
    db= mdb.connect(user="root", host="localhost", passwd = "0520", db="citydb", charset='utf8')
    
    with db:
        cur = db.cursor()
        #just select the city from the world_innodb that the user inputs
        cur.execute("SELECT * FROM singleInsp WHERE Zip='%s' order by VioLevel_pred desc;" % zipcode)
        query_results = cur.fetchall()
    
    restaurants = []
    restKeys = ['Index','Name','VioLevel','VioType','VioLevel_pred','VioType_pred','Rating',
                'PropValue','Zip','locLong','locLati','Category']
    CategList = ['arts','food','nightlife','hotelstravel','restaurants','eventservices']
    numResult = len(query_results)
    print numResult, 'restaurants found'
    
    for result in query_results: # each result represents a restaurant
        # restKeys are same as column names of MySQL table, except the coded categories in MySQL are removed, and 'Category' is added
        result = list(result)
        result[3]  = VioCode[int(result[3])]
        result[5]  = VioCode[int(result[5])]	 
        categ = ''
        for i in range(len(result)-len(restKeys)+1):         
            if result[i+len(restKeys)-1] == 1: categ += CategList[i]+', '
        restValues = [str(result[i]) for i in range(len(restKeys)-1)] 
        restValues.append(categ)
        restInfo = dict(zip(restKeys, restValues))
        restaurants.append(restInfo)
    #print restaurants
    #call a function from a_Model package. note we are only pulling one result in the query
    #pop_input = cities[0]['population']
    #the_result = ModelIt(city, pop_input)
    return render_template("output.html", numResult = numResult, restaurants = restaurants)

@app.route('/mapexample')
def show_map():
    return render_template("mapexample.html")

@app.route('/momap')
def show_momap():
    return render_template("momap.html")
    


