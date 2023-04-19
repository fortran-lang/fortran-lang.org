fortranLang = {

baseurl: '/fortran-lang.org/pr/421',

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
