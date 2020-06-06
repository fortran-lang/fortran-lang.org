// Populate element with id 'package-topics' with list
//  of top tags in package index
//
(function() {

    if (!!document.getElementById('package-topics')){

        var nTag = 50;

        var tags = new Map();

        // Load projects from json
        var projects = fortranLang.loadJSON(fortranLang.baseurl+'/packages/package_index.json').projects;

        var project_tags = [];
        for (var i = 0; i<projects.length; i++){

            project_tags = projects[i].tags.split(" ");

            for (var j=0; j<project_tags.length; j++ ){

                if (!tags.has(project_tags[j])){
                    tags.set(project_tags[j],1);
                } else {
                    tags.set(project_tags[j],
                        tags.get(project_tags[j])+1);
                }

            }

        }

        var tagSort = [...tags.entries()].sort( (a,b) => b[1] - a[1] );

        tagCloud = '&bull;';
        for (var i=0; i<nTag; i++){

            if (tagSort[i][0] === ''){
                continue;
            }

            tagCloud += Mustache.render(` <a href="{{baseurl}}/packages/search?query={{tag}}">{{tag}}</a> &bull;`,
                {baseurl: fortranLang.baseurl, tag:tagSort[i][0]}
            );

        }

        document.getElementById('package-topics').innerHTML = tagCloud;

    }

})();