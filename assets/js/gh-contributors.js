// 
if (!!document.getElementById("gh-contributors")){

    var repoFilter = "";

    repos = ['fortran-lang-fortran-lang.org','fortran-lang-fpm','fortran-lang-stdlib','j3-fortran-fortran_proposals'];

    repoStats = repos.map( r => loadJSON('/community/github_stats_data/data-'+r+'.json') );

    userComments = mapUserComments(repoStats);

    dateBounds = getDateBounds(userComments);
    
    var tmin0 = dateBounds[0];
    var tmax0 = dateBounds[1];

    var tmin = tmin0;
    var tmax = tmax0;

    $(function() {
        $( "#slider-range" ).slider({
            range: true,
            min: new Date(dateBounds[0]).getTime() / 1000,
            max: new Date(dateBounds[1]).getTime() / 1000,
            step: 86400,
            values: [ new Date(dateBounds[0]).getTime() / 1000, new Date(dateBounds[1]).getTime() / 1000 ],
            slide: function( event, ui ) {
                $( "#amount" ).val( (new Date(ui.values[ 0 ] *1000).toDateString() ) + " - " + (new Date(ui.values[ 1 ] *1000)).toDateString() );
            },
            stop: function( event, ui) {
                tmin = ui.values[ 0 ] *1000;
                tmax = ui.values[ 1 ] *1000;
                generateContributorStats();
            }
        });
        $( "#amount" ).val( (new Date($( "#slider-range" ).slider( "values", 0 )*1000).toDateString()) +
        " - " + (new Date($( "#slider-range" ).slider( "values", 1 )*1000)).toDateString());
    });


    generateContributorStats(); //,repo='fortran-lang',startDate=0,endDate=Infinity)

    console.log("Done.")

}

function repoSelector(selector){

    repoFilter = selector.value;
    generateContributorStats(true);

}

function generateContributorStats(resetRange = false){

    if (resetRange){

        var filteredComments = filterComments(userComments,repoFilter);

        dateBounds = getDateBounds(filteredComments);

        tmin = dateBounds[0];
        tmax = dateBounds[1];  

        $("#slider-range").slider("values",0,dateBounds[0]/1000);
        $("#slider-range").slider("values",1,dateBounds[1]/1000);

    } else {

        var filteredComments = filterComments(userComments,repoFilter,tmin,tmax);

    }

    var userNames = [...filteredComments.keys()];
    
    var userSort = userNames.sort((u1,u2) => filteredComments.get(u2).length - filteredComments.get(u1).length);

    document.getElementById("gh-contributors").innerHTML = '';
    for (var i = 0; i < userSort.length; i++){ //userNames.length; i++){

        addContributorChart(userSort[i],filteredComments.get(userSort[i]).length,$('#userPlots')[0].checked);
    }

    var oneday = Date.UTC(96, 1, 2) - Date.UTC(96, 1, 1);
    var bucketSize = 7*oneday;

    var allDates = [].concat(...userNames.map(u=>filteredComments.get(u)));
    plotData = getUserPlotData(allDates,tmin0,tmax0,bucketSize)
    plotContributorChart('all',plotData,true);

    if ($('#userPlots')[0].checked){

        for (var i=0; i < userSort.length; i++){

            plotData = getUserPlotData(filteredComments.get(userSort[i]),tmin,tmax,bucketSize)
            plotContributorChart(userSort[i],plotData);

        }

    }

}

function getDateBounds(userCommentData){

    var userNames = [...userCommentData.keys()];

    var allDates = [].concat(...userNames.map(u=>userCommentData.get(u))).map(c => c.date.valueOf());

    var tmin = Math.min(...allDates);
    var tmax = Math.max(...allDates);

    return [tmin, tmax];

}

function filterComments(userCommentData,repo='fortran-lang',startDate=0,endDate=Infinity){

    userNames = [...userCommentData.keys()];
    
    userCommentData2 = new Map(userCommentData);
    for (var i=0; i< userNames.length; i++){

        if (userNames[i].includes('[bot]')){
            userCommentData2.delete(userNames[i]);
            continue;
        }

        userData = userCommentData.get(userNames[i]);

        userData = userData.filter(c => c.repo.includes(repo) && 
                                        c.date.valueOf() >= startDate &&
                                        c.date.valueOf() <= endDate);
        
        if (userData.length > 0){
            userCommentData2.set(userNames[i],userData);
        } else {
            userCommentData2.delete(userNames[i]);
        }

    }

    return userCommentData2;
}

function getUserPlotData(userData,tmin,tmax,bucketSize){


    var start = Math.min( ...userData.map(c => c.date.valueOf()) );
    var end = Math.max( ...userData.map(c => c.date.valueOf()) );

    var bi = 0;
    var plotData = [];
    plotData.push({x: tmin-bucketSize, y: 0});
    plotData.push({x: start-bucketSize, y: 0});

    for (var d = start; d < end+bucketSize; d=d+bucketSize){

        count = userData.map(c => c.date>=d && c.date < (d+bucketSize)).filter(Boolean).length;

        plotData.push({x: new Date(d), y: count})

        bi = bi + 1;

    }
    plotData.push({x: end+bucketSize, y: 0});
    plotData.push({x: tmax+bucketSize, y: 0});

    return plotData;
}



function mapUserComments(repoList){

    var userCommentData = new Map();

    for (var k = 0; k < repoList.length; k++){
        
        let repo = repoList[k];

        for (var i = 0; i < repo.issues.length; i++) {

            if (!userCommentData.has(repo.issues[i].user)){
                userCommentData.set(repo.issues[i].user,[]);
            }

            userData = userCommentData.get(repo.issues[i].user);
            userData.push({repo: repo.name, date: new Date(repo.issues[i].date)});

            let comments = repo.issues[i].comments;

            for (var j = 0; j < comments.length; j++) {

                if (!userCommentData.has(comments[j].user)){
                    userCommentData.set(comments[j].user,[]);
                }

                userData = userCommentData.get(comments[j].user);
                userData.push({repo: repo.name, date: new Date(comments[j].date)});
                
            }

        }

    }

    userNames = [...userCommentData.keys()];

    for (var i = 0; i < userNames.length; i++){
        userData = userCommentData.get(userNames[i]);
        userData.sort((a,b) => a.date - b.date);
    }

    return userCommentData;

}


// Add a new contributor box
//  to #gh-contributors element
//
function addContributorChart(userName, nComment, plot=true){

    // document.getElementById("gh-contributors").innerHTML += 
    //      '<div class="col-flex"> <h3>'+
    //      '<a href="https://github.com/"'+userName+'>' userName+'</h3>'+
    //      '<canvas id="chart-'+userName+'" width="250" height="200"></canvas>'+
    //      '</div>';

    document.getElementById("gh-contributors").innerHTML += 
        Mustache.render(`
        <div class="col-flex contributor"> 
        <img src="https://github.com/{{userName}}.png?size=40" width="40" style="border-radius: 7px;">
        <div style="display: inline-block; vertical-align: middle; margin-left: 10px">
        <a class="hidenewwindow" href="https://github.com/{{userName}}" target="_blank" rel="noopener" style="font-size:22px;">
        {{userName}} </a> </br>
        <i><b> {{nComment}} comments </b></i>
        </div>
        {{#plot}}
        <div style="position: relative;">
        <canvas id="chart-{{userName}}"></canvas>
        </div>
        {{/plot}}
        </div>
        `,{userName: userName, nComment:nComment, plot: plot});

}

// Populate contributor chart 
//  with (x,y) plot data
//
function plotContributorChart(userName,plotData_i,responsive=false){

    ctx_i= document.getElementById('chart-'+userName).getContext('2d');

    charts_i = new Chart(ctx_i, {
        type: 'scatter',
        data: {
            datasets: [{
                label: 'Scatter Dataset',
                data: plotData_i
            }]
        },
        options: plotOptions(responsive)
    });

}


// Configuration for chart.js
//  https://www.chartjs.org/docs/latest/
//
function plotOptions(responsive=false) {
    return {
        responsive: true,
        aspectRatio: 2,
        legend: {
            display: false
        },
        elements: {
            point: {
                radius: 0
            },
            line: {
                backgroundColor: 'rgba(115, 79, 150, 0.5)',
                cubicInterpolationMode: 'monotone'
            }
        },
        scales: {
            xAxes: [{
                type: 'time',
                time: {
                    unit: 'month'
                },
                position: 'bottom'
            }],
            yAxes: [{
                ticks:{
                    // max: 100
                    maxTicksLimit: 5
                }
            }]
        },
        datasets: {
            scatter: {
                showLine: true
            }
        }
    }
}


function loadJSON(url) {
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
}