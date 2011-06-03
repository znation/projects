var http = require("http");
var querystring = require("querystring");

var symbols = [
    "YHOO"
];

var startYear = 2009, endYear = 2011;

var queryString = {
    'diagnostics': 'true',
    'env': "store://datatables.org/alltableswithkeys",
    'format': 'json'
};

var basePath = '/v1/public/yql?';

var options = {
  host: 'query.yahooapis.com',
  port: 80
};

for (var i=0; i<symbols.length; i++)
{
    var symbol = symbols[i];
    for (var year=endYear-1; year>=startYear; year--)
    {
        var startDate = String(year) + "-01-01";
        var endDate = String(year+1) + "-01-01";
        queryString.q = 'select * from yahoo.finance.historicaldata where symbol = "' + symbol + '" and startDate = "' + startDate + '" and endDate = "' + endDate + '"';
        options.path = basePath + querystring.stringify(queryString);
        http.get(options, function(res) {
            var data = "";
            res.on('data', function (chunk) {
                data += chunk;
            });
            res.on('end', function() {
                var parsed = JSON.parse(data);
                var quotes = parsed["query"]["results"]["quote"];
                printCSV(quotes);
            });
        }).on('error', function(e) {
          console.log("Got error: " + e.message);
        });
    }
}

function printCSV(quotes)
{
    for (var i=0; i<quotes.length; i++)
    {
        var quote = quotes[i];
        console.log(quote["date"]);
    }
}

