var http = require("http");
var querystring = require("querystring");

// The NASDAQ-100
var symbols = [
    "ATVI",
    "ADBE",
    "AKAM",
    "ALXN",
    "ALTR",
    "AMZN",
    "AMGN",
    "APOL",
    "AAPL",
    "AMAT",
    "ADSK",
    "ADP",
    "BIDU",
    "BBBY",
    "BIIB",
    "BMC",
    "BRCM",
    "CHRW",
    "CA",
    "CELG",
    "CEPH",
    "CERN",
    "CHKP",
    "CSCO",
    "CTXS",
    "CTSH",
    "CMCSA",
    "COST",
    "CTRP",
    "DELL",
    "XRAY",
    "DTV",
    "DLTR",
    "EBAY",
    "ERTS",
    "EXPE",
    "EXPD",
    "ESRX",
    "FFIV",
    "FAST",
    "FSLR",
    "FISV",
    "FLEX",
    "FLIR",
    "GRMN",
    "GILD",
    "GOOG",
    "GMCR",
    "HSIC",
    "ILMN",
    "INFY",
    "INTC",
    "INTU",
    "ISRG",
    "JOYG",
    "KLAC",
    "LRCX",
    "LINTA",
    "LIFE",
    "LLTC",
    "MRVL",
    "MAT",
    "MXIM",
    "MCHP",
    "MU",
    "MSFT",
    "MYL",
    "NTAP",
    "NFLX",
    "NWSA",
    "NIHD",
    "NVDA",
    "ORLY",
    "ORCL",
    "PCAR",
    "PAYX",
    "PCLN",
    "QGEN",
    "QCOM",
    "RIMM",
    "ROST",
    "SNDK",
    "STX",
    "SHLD",
    "SIAL",
    "SPLS",
    "SBUX",
    "SRCL",
    "SYMC",
    "TEVA",
    "URBN",
    "VRSN",
    "VRTX",
    "VMED",
    "VOD",
    "WCRX",
    "WFM",
    "WYNN",
    "XLNX",
    "YHOO"
];

var startYear = 2000, endYear = 2011;

var queryString = {
    
};

var basePath = '/v1/public/yql?';


for (var i=0; i<symbols.length; i++)
{
    var symbol = symbols[i];
    for (var year=endYear-1; year>=startYear; year--)
    {
        var startDate = String(year) + "-01-01";
        var endDate = String(year+1) + "-01-01";
        var queryString = {
            'diagnostics': 'true',
            'env': "store://datatables.org/alltableswithkeys",
            'format': 'json',
            'q': 'select * from yahoo.finance.historicaldata where symbol = "' + symbol + '" and startDate = "' + startDate + '" and endDate = "' + endDate + '"'
        }
        var options = {
            host: 'query.yahooapis.com',
            port: 80,
            path: basePath + querystring.stringify(queryString)
        };
        http.get(options, function(res) {
            var data = "";
            res.on('data', function (chunk) {
                data += chunk;
            });
            res.on('end', function() {
                try
                {
                    var parsed = JSON.parse(data);
                }
                catch (e)
                {
                    console.error(data);
                    console.trace();
                    process.exit(1);
                }
                if (parsed == null ||
                    parsed["query"] == null ||
                    parsed["query"]["results"] == null ||
                    parsed["query"]["results"]["quote"] == null)
                {
                    console.warn("URL is: http://" + options.host + options.path);
                    console.warn(JSON.stringify(parsed));
                    process.exit(1);
                }
                var quotes = parsed["query"]["results"]["quote"];
                printCSV(symbol, quotes);
            });
        }).on('error', function(e) {
          console.warn("Got error: " + e.message);
          process.exit(1);
        });
    }
}

function printCSV(symbol, quotes)
{
    for (var i=0; i<quotes.length; i++)
    {
        var quote = quotes[i];
        
        var dateStr = quote["date"];
        var datePieces = dateStr.split("-");
        var year = datePieces[0];
        var month = datePieces[1];
        var day = datePieces[2];
        var date = String(month) + "/" + String(day) + "/" + String(year);
        
        console.log(symbol +
            "\t" + date +
            "\t" + quote["Open"] +
            "\t" + quote["High"] +
            "\t" + quote["Low"] +
            "\t" + quote["Close"] +
            "\t" + quote["Volume"] +
            "\t" + quote["Adj_Close"]);
    }
}

