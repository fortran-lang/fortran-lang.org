---
layout: null
---


fortranLang = {

baseurl: '{{site.baseurl}}',

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
    (function loadJSON(url) {
        // Load JSON data from url
    //
        var json = null;
        $.ajax({
            'async': false,
            'global': false,
            'url': url,
            'dataType': "json",
            'success': function (data) {
                json = data;
            }
        });
        return json;
    })

};