function findGetParameter(parameterName) {
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
}

function getSubSentences(sentence) {
    // Return all permutations of contiguous sub sentences from a sentence
    var words = sentence.split(" ");

    var subs = [];

    var N = words.length;

    var i;
    for (i = 1; i <= N; i++){ // Loop over possible sentence lengths

        var j;
        for (j=0; j<=(N-i); j++){ // Loop over sentence locations

            sub_i = words.slice(j,j+i);
            subs.push(sub_i.join(" "));

        }

    }

    return subs;

}

function searchProjects(queryString,projects) {
    // Basic sub-string matching within project fields
    //
    //  Results ranked by size of matched sub-string and field weight
    //
    var subs = getSubSentences(queryString);

    // Sort by subsentence length descending
    subs = subs.sort(function(a,b){return b.split(" ").length - a.split(" ").length})

    var fields = ['name','description','tags','license','github','url'];
    var fieldWeights = [10.0,1.0,1.0,0.1,1.0,0.1];

    // Loop over projects JSON
    var i;
    for (i = 0; i<projects.length; i++){

        var j;

        projects[i].score = 0;

        var subMatch = [];
        for (j=0; j<subs.length; j++){
            subMatch[j] = false;
        }        

        for (j=0; j<fields.length; j++){

            var k;
            for (k=0; k<subs.length; k++){

                if (subMatch[k]){
                    // Don't match a sub-sentence more than once per project
                    continue;
                }

                fieldData = projects[i][fields[j]].toLowerCase();
                querySubString = subs[k].toLowerCase();
                queryStringLength = querySubString.split(" ").length;

                if (fieldData.includes(querySubString)){

                    subMatch[k] = true;
                    projects[i].score += fieldWeights[j]*queryStringLength;
                    break;

                }

            }

        }

    }

    return projects.sort(function(a,b){return b.score - a.score});

}

function resultsToHTML(results){
    // Return HTML representation of search results
    //

    var out = "";
    var i;
    for (i=0;i<results.length;i++){
    
        if (results[i].score > 0){
    
            project = results[i];
    
            if (results[i].github != ""){
                out += '<h3><a href="https://github.com/'+project.github+'" target="_blank">';
                out += '<i class="devicon-github-plain colored"></i> '+project.name+'</a></h3>';
            } else {
                out += '<h3><a href="'+project.url+'" target="_blank">';
                if (project.url.includes('gitlab.com')) {
                    out += '<i class="devicon-gitlab-plain colored"></i> ';
                } else if (project.url.includes('bitbucket.org')){
                    out += '<i class="devicon-bitbucket-plain colored"></i> ';
                }
                out += project.name+'</a></h3>';
            }
            
            out += '<p> '+project.description;

            var cats = project.categories.split(" ");
            out += ' ('
            var j;
            for (j=0;j<cats.length;j++){
                out += '<a href="/pr/96/packages/'+cats[j]+'">'+cats[j]+'</a>';
                if (j<cats.length-1){
                    out += ', ';
                }
            }
            out += ') </p>'
            
            

            out += '<p class="light small"><b>Tags: </b>'+project.tags+'</p>';
    
        }
    
    }
    
    return out;

}

// Perform search here onload
var queryString = findGetParameter('query').replace(/\+/g," ").replace(/"/g,'');
document.getElementById('search-query').value = queryString;

results = searchProjects(queryString,projects);
resultsHTML = resultsToHTML(results);
document.getElementById('search-results').innerHTML = resultsHTML;