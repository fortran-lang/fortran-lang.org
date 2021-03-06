fortranLang = {

baseurl: '/pr/220',

findGetParameter:
    (function findGetParameter(parameterName) {
        // Return a GET HTTP parameter
        var result = null,
            tmp = [];
        location.search
            .substr(1)
            .split("&")
            .forEach(function (item) {
            tmp = item.split("=");
            if (tmp[0] === parameterName) result = decodeURIComponent(tmp[1]);
            });
        return result;
    }),


loadJSON:
    (function loadJSON(url, onLoad) {
        // Load JSON data from url
    //
        var json = null;
        $.ajax({
            'global': false,
            'url': url,
            'dataType': "json",
            'success': onLoad,
            'error': function(jzXHR,status) {
                console.log('Failed to load json file at '+url);
                console.log(' Text status: '+status);
            }
        });
    })

};