var map = L.map('map').setView([42.3598, -71.0851], 13);

var layer = L.tileLayer('https://api.tiles.mapbox.com/v4/{id}/{z}/{x}/{y}.png?access_token={accessToken}', 
                        {attribution: 'Map data &copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors, \
                                       <a href="http://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>, \
                                       Imagery © <a href="http://mapbox.com">Mapbox</a>',
                         maxZoom: 18,
                         id: 'shimolu523.p1g0bd7h',
                         accessToken: 'pk.eyJ1Ijoic2hpbW9sdTUyMyIsImEiOiJjaWswbjk3cjkzYWR5dm9raTgxaXhrejNmIn0.Z9EirFx5JhpxrXCAI65AJQ'
                        });

layer.addTo(map);

var restaurants_json = restaurants_json; //json file passed from view.py

//var locLong = restaurants_json['locLong'][0];
//var locLati = restaurants_json['locLati'][0];

console.log(restaurants_json);

//var marker = L.marker([locLong, locLati]);
//marker.addTo(map);


